//! Kafka Pipeline Example for copybook-rs
//!
//! This example demonstrates:
//! - Reading COBOL copybook files
//! - Decoding binary mainframe data
//! - Producing decoded records to Kafka topics
//! - Error handling and retry logic
//!
//! # Prerequisites
//!
//! 1. A running Kafka broker
//! 2. Create a topic: `kafka-topics --create --topic copybook-data --bootstrap-server localhost:9092`
//!
//! # Configuration
//!
//! Set environment variables:
//! - `KAFKA_BROKERS`: Comma-separated list of Kafka brokers (default: "localhost:9092")
//! - `KAFKA_TOPIC`: Topic name (default: "copybook-data")
//! - `COPYBOOK_PATH`: Path to COBOL copybook file
//! - `DATA_PATH`: Path to binary data file
//!
//! # Running the example
//!
//! ```bash
//! cargo run --example kafka_pipeline
//! ```

use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, UnmappablePolicy, ZonedEncodingFormat};
use copybook_core::parse_copybook;
use rdkafka::config::ClientConfig;
use rdkafka::producer::{BaseRecord, Producer, ThreadedProducer};
use std::env;
use std::fs;
use std::path::Path;
use std::time::Duration;
use thiserror::Error;
use tokio::time::sleep;
use tracing::{error, info, warn};

/// Maximum number of retry attempts for Kafka operations
const MAX_RETRIES: u32 = 3;

/// Delay between retry attempts
const RETRY_DELAY_MS: u64 = 1000;

/// Errors that can occur in the Kafka pipeline
#[derive(Error, Debug)]
enum PipelineError {
    #[error("Copybook parse error: {0}")]
    CopybookParse(String),

    #[error("Data read error: {0}")]
    DataRead(String),

    #[error("Decode error: {0}")]
    Decode(String),

    #[error("Kafka error: {0}")]
    Kafka(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Kafka producer configuration
struct KafkaConfig {
    brokers: String,
    topic: String,
}

impl KafkaConfig {
    /// Load configuration from environment variables
    fn from_env() -> Self {
        Self {
            brokers: env::var("KAFKA_BROKERS").unwrap_or_else(|_| "localhost:9092".to_string()),
            topic: env::var("KAFKA_TOPIC").unwrap_or_else(|_| "copybook-data".to_string()),
        }
    }
}

/// Kafka pipeline for processing copybook data
struct KafkaPipeline {
    producer: ThreadedProducer<rdkafka::producer::DefaultProducerContext>,
    config: KafkaConfig,
}

impl KafkaPipeline {
    /// Create a new Kafka pipeline
    fn new(config: KafkaConfig) -> Result<Self, PipelineError> {
        let producer: ThreadedProducer<rdkafka::producer::DefaultProducerContext> =
            ClientConfig::new()
                .set("bootstrap.servers", &config.brokers)
                .set("message.timeout.ms", "5000")
                .set("request.timeout.ms", "5000")
                .set("delivery.timeout.ms", "10000")
                .set("acks", "1")
                .create()
                .map_err(|e| PipelineError::Kafka(format!("Failed to create producer: {}", e)))?;

        info!("Kafka producer created for brokers: {}", config.brokers);

        Ok(Self { producer, config })
    }

    /// Send a message to Kafka with retry logic
    fn send_message(&self, key: Option<&[u8]>, payload: &[u8]) -> Result<(), PipelineError> {
        let mut attempt = 0;

        loop {
            attempt += 1;

            let record = BaseRecord::<[u8], _>::to(&self.config.topic)
                .key(key.unwrap_or(b""))
                .payload(payload);

            match self.producer.send(record) {
                Ok(_) => {
                    info!("Message sent successfully to topic: {}", self.config.topic);
                    return Ok(());
                }
                Err((e, _)) => {
                    if attempt >= MAX_RETRIES {
                        return Err(PipelineError::Kafka(format!(
                            "Failed to send message after {} attempts: {}",
                            MAX_RETRIES, e
                        )));
                    }

                    warn!(
                        "Failed to send message (attempt {}/{}): {}, retrying in {}ms...",
                        attempt, MAX_RETRIES, e, RETRY_DELAY_MS
                    );

                    std::thread::sleep(Duration::from_millis(RETRY_DELAY_MS));
                }
            }
        }
    }

    /// Flush any pending messages
    fn flush(&self, timeout_ms: u64) {
        info!("Flushing producer messages...");
        if let Err(e) = self.producer.flush(Duration::from_millis(timeout_ms)) {
            error!("Failed to flush producer: {}", e);
        }
    }
}

/// Process a copybook file and send decoded records to Kafka
async fn process_copybook_to_kafka(
    copybook_path: &Path,
    data_path: &Path,
    pipeline: &KafkaPipeline,
) -> Result<usize, PipelineError> {
    // Read copybook
    info!("Reading copybook from: {}", copybook_path.display());
    let copybook_content = fs::read_to_string(copybook_path)
        .map_err(|e| PipelineError::DataRead(format!("Failed to read copybook: {}", e)))?;

    // Parse copybook
    info!("Parsing copybook schema...");
    let schema = parse_copybook(&copybook_content)
        .map_err(|e| PipelineError::CopybookParse(format!("Failed to parse copybook: {}", e)))?;

    info!("Schema loaded: {} fields, LRECL: {:?}",
          schema.fields.len(), schema.lrecl_fixed);

    // Configure decode options
    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037, // EBCDIC
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: true, // Include metadata for audit trail
        emit_raw: copybook_codec::RawMode::Off,
        strict_mode: false,
        max_errors: Some(100),
        on_decode_unmappable: UnmappablePolicy::Replace,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Read binary data
    info!("Reading binary data from: {}", data_path.display());
    let data = fs::read(data_path)
        .map_err(|e| PipelineError::DataRead(format!("Failed to read data file: {}", e)))?;

    let record_length = schema.lrecl_fixed.unwrap_or(data.len() as u32) as usize;
    let total_records = data.len() / record_length;

    info!("Processing {} records ({} bytes each)...", total_records, record_length);

    let mut processed = 0;
    let mut errors = 0;

    // Process each record
    for i in 0..total_records {
        let start = i * record_length;
        let end = (start + record_length).min(data.len());
        let record_data = &data[start..end];

        // Decode record
        match copybook_codec::decode_record(&schema, record_data, &options) {
            Ok(json_value) => {
                // Serialize to JSON
                match serde_json::to_vec(&json_value) {
                    Ok(json_bytes) => {
                        // Send to Kafka
                        let key = format!("record-{}", i);
                        match pipeline.send_message(Some(key.as_bytes()), &json_bytes) {
                            Ok(_) => processed += 1,
                            Err(e) => {
                                error!("Failed to send record {}: {}", i, e);
                                errors += 1;
                            }
                        }
                    }
                    Err(e) => {
                        error!("Failed to serialize record {}: {}", i, e);
                        errors += 1;
                    }
                }
            }
            Err(e) => {
                warn!("Failed to decode record {}: {}", i, e);
                errors += 1;
            }
        }

        // Progress update every 100 records
        if (i + 1) % 100 == 0 {
            info!("Processed {}/{} records ({} errors)", i + 1, total_records, errors);
        }

        // Small delay to avoid overwhelming Kafka
        if (i + 1) % 50 == 0 {
            sleep(Duration::from_millis(10)).await;
        }
    }

    info!("Processing complete: {} records processed, {} errors", processed, errors);

    Ok(processed)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .with_target(false)
        .init();

    info!("Starting Kafka Pipeline Example");

    // Load configuration
    let kafka_config = KafkaConfig::from_env();
    info!("Kafka configuration: brokers={}, topic={}", kafka_config.brokers, kafka_config.topic);

    // Get file paths from environment or use defaults
    let copybook_path_str = env::var("COPYBOOK_PATH").unwrap_or_else(|_| "test-data/simple.cpy".to_string());
    let data_path_str = env::var("DATA_PATH").unwrap_or_else(|_| "test-data/simple.bin".to_string());
    let copybook_path = Path::new(&copybook_path_str);
    let data_path = Path::new(&data_path_str);

    // Check if files exist
    if !copybook_path.exists() {
        error!("Copybook file not found: {}", copybook_path.display());
        error!("Please set COPYBOOK_PATH environment variable");
        return Err("Copybook file not found".into());
    }

    if !data_path.exists() {
        error!("Data file not found: {}", data_path.display());
        error!("Please set DATA_PATH environment variable");
        return Err("Data file not found".into());
    }

    // Create Kafka pipeline
    let pipeline = KafkaPipeline::new(kafka_config)?;

    // Process copybook data
    match process_copybook_to_kafka(copybook_path, data_path, &pipeline).await {
        Ok(count) => {
            info!("Successfully processed {} records", count);
        }
        Err(e) => {
            error!("Pipeline error: {}", e);
            return Err(e.into());
        }
    }

    // Flush pending messages
    pipeline.flush(5000);

    info!("Pipeline completed successfully");

    Ok(())
}
