// SPDX-License-Identifier: AGPL-3.0-or-later
//! Kafka producer example for copybook-rs.
//!
//! Reads fixed-length copybook data, decodes it with copybook-rs, and publishes
//! JSON records to Kafka.

use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;
use rdkafka::config::ClientConfig;
use rdkafka::producer::{BaseRecord, Producer, ThreadedProducer};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use thiserror::Error;
use tracing::{error, info, warn};

const MAX_RETRIES: u32 = 3;
const RETRY_DELAY_MS: u64 = 1_000;
const DEFAULT_BROKERS: &str = "localhost:9092";
const DEFAULT_TOPIC: &str = "copybook-data";
const DEFAULT_CODEPAGE: Codepage = Codepage::ASCII;
const DEFAULT_COPYBOOK_PATH: &str = "sample_data/schema.cpy";
const DEFAULT_DATA_PATH: &str = "sample_data/data.bin";

#[derive(Error, Debug)]
enum ProducerError {
    #[error("Copybook parse error: {0}")]
    CopybookParse(String),
    #[error("Data read error: {0}")]
    DataRead(String),
    #[error("Kafka error: {0}")]
    Kafka(String),
    #[error("Config error: {0}")]
    Config(String),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

struct ProducerConfig {
    brokers: String,
    topic: String,
    copybook_path: PathBuf,
    data_path: PathBuf,
    codepage: Codepage,
}

impl ProducerConfig {
    fn from_env() -> Result<Self, ProducerError> {
        let brokers = env::var("KAFKA_BROKERS").unwrap_or_else(|_| DEFAULT_BROKERS.to_string());
        let topic = env::var("KAFKA_TOPIC").unwrap_or_else(|_| DEFAULT_TOPIC.to_string());
        let copybook_path = Path::new(&env::var("COPYBOOK_PATH").unwrap_or_else(|_| {
            manifest_relative_path(DEFAULT_COPYBOOK_PATH)
                .to_string_lossy()
                .into_owned()
        }))
        .to_path_buf();
        let data_path = Path::new(&env::var("DATA_PATH").unwrap_or_else(|_| {
            manifest_relative_path(DEFAULT_DATA_PATH)
                .to_string_lossy()
                .into_owned()
        }))
        .to_path_buf();
        let codepage = parse_codepage(
            env::var("CODEPAGE")
                .unwrap_or_else(|_| DEFAULT_CODEPAGE.as_str().to_owned())
                .as_str(),
        )?;

        Ok(Self {
            brokers,
            topic,
            copybook_path,
            data_path,
            codepage,
        })
    }
}

fn manifest_relative_path(file: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(file)
        .to_path_buf()
}

fn parse_codepage(value: &str) -> Result<Codepage, ProducerError> {
    match value.to_ascii_lowercase().as_str() {
        "ascii" => Ok(Codepage::ASCII),
        "cp037" => Ok(Codepage::CP037),
        "cp273" => Ok(Codepage::CP273),
        "cp500" => Ok(Codepage::CP500),
        "cp1047" => Ok(Codepage::CP1047),
        "cp1140" => Ok(Codepage::CP1140),
        _ => Err(ProducerError::Config(format!(
            "Unsupported CODEPAGE '{value}'. Use one of ascii, cp037, cp273, cp500, cp1047, cp1140."
        ))),
    }
}

struct KafkaPipeline {
    producer: ThreadedProducer<rdkafka::producer::DefaultProducerContext>,
    config: ProducerConfig,
}

impl KafkaPipeline {
    fn new(config: ProducerConfig) -> Result<Self, ProducerError> {
        let producer = ClientConfig::new()
            .set("bootstrap.servers", &config.brokers)
            .set("message.timeout.ms", "5000")
            .set("request.timeout.ms", "5000")
            .set("acks", "1")
            .create::<ThreadedProducer<_>>()
            .map_err(|error| ProducerError::Kafka(format!("Failed to create producer: {error}")))?;

        info!("Kafka producer created for brokers: {}", config.brokers);
        Ok(Self { producer, config })
    }

    fn send_message(&self, key: Option<&[u8]>, payload: &[u8]) -> Result<(), ProducerError> {
        let mut attempt = 0;
        let mut delay_ms = RETRY_DELAY_MS;

        loop {
            attempt += 1;
            let record = BaseRecord::<[u8], _>::to(&self.config.topic)
                .key(key.unwrap_or_default())
                .payload(payload);

            match self.producer.send(record) {
                Ok(()) => {
                    return Ok(());
                }
                Err((error, _)) => {
                    if attempt >= MAX_RETRIES {
                        return Err(ProducerError::Kafka(format!(
                            "Failed to send message after {MAX_RETRIES} attempts: {error}"
                        )));
                    }

                    warn!(
                        "Failed to send message (attempt {attempt}/{MAX_RETRIES}): {error}, retrying in {delay_ms}ms..."
                    );
                    std::thread::sleep(Duration::from_millis(delay_ms));
                    delay_ms = delay_ms.saturating_mul(2);
                }
            }
        }
    }

    fn flush(&self, timeout_ms: u64) {
        if let Err(error) = self.producer.flush(Duration::from_millis(timeout_ms)) {
            error!("Failed to flush producer: {error}");
        }
    }
}

fn decode_options(codepage: Codepage) -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::Fixed,
        codepage,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: Some(100),
        on_decode_unmappable: UnmappablePolicy::Replace,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    }
}

fn process_copybook_to_kafka(
    copybook_path: &Path,
    data_path: &Path,
    codepage: Codepage,
    pipeline: &KafkaPipeline,
) -> Result<(usize, usize), ProducerError> {
    info!("Reading copybook from: {}", copybook_path.display());
    let copybook_content = fs::read_to_string(copybook_path)
        .map_err(|error| ProducerError::CopybookParse(format!("Failed to read copybook: {error}")))?;

    info!("Parsing copybook schema...");
    let schema = parse_copybook(&copybook_content)
        .map_err(|error| ProducerError::CopybookParse(format!("Failed to parse copybook: {error}")))?;

    let options = decode_options(codepage);
    info!(
        "Schema loaded: {} fields, LRECL: {:?}",
        schema.fields.len(),
        schema.lrecl_fixed
    );

    let record_length = schema.lrecl_fixed.ok_or_else(|| {
        ProducerError::CopybookParse("Copybook does not define fixed LRECL".to_string())
    })?;
    let record_length_usize = usize::try_from(record_length)
        .map_err(|error| ProducerError::CopybookParse(format!("LRECL conversion failed: {error}")))?;
    let data = fs::read(data_path)
        .map_err(|error| ProducerError::DataRead(format!("Failed to read data file: {error}")))?;

    if !data.len().is_multiple_of(record_length_usize) {
        warn!(
            "Data length {} is not a multiple of record length {}, trailing bytes will be ignored",
            data.len(),
            record_length_usize
        );
    }

    let total_records = data.len() / record_length_usize;
    info!(
        "Processing {total_records} records ({record_length_usize} bytes each)..."
    );

    let mut processed = 0usize;
    let mut errors = 0usize;

    for record_index in 0..total_records {
        let start = record_index * record_length_usize;
        let end = start + record_length_usize;
        let record = &data[start..end];

        match copybook_codec::decode_record(&schema, record, &options) {
            Ok(json_value) => {
                match serde_json::to_vec(&json_value) {
                    Ok(payload) => {
                        let key = json_value
                            .get("ORDER_ID")
                            .and_then(|v| v.as_str())
                            .unwrap_or_else(|| "record")
                            .to_string();

                        if pipeline.send_message(Some(key.as_bytes()), &payload).is_ok() {
                            processed += 1;
                        } else {
                            errors += 1;
                            warn!("Failed to send record {record_index} to topic");
                        }
                    }
                    Err(error) => {
                        errors += 1;
                        warn!("Failed to serialize record {record_index}: {error}");
                    }
                }
            }
            Err(error) => {
                errors += 1;
                warn!("Failed to decode record {record_index}: {error}");
            }
        }

        if (record_index + 1) % 100 == 0 {
            info!("Processed {}/{} records ({} errors)", record_index + 1, total_records, errors);
        }
    }

    Ok((processed, errors))
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .with_target(false)
        .init();

    info!("Starting Kafka producer example");

    let config = ProducerConfig::from_env()?;
    info!(
        "Kafka configuration: brokers={}, topic={}",
        config.brokers, config.topic
    );
    info!("Using codepage: {}", config.codepage);

    if !config.copybook_path.exists() {
        return Err(Box::new(ProducerError::DataRead(format!(
            "Copybook file not found: {}",
            config.copybook_path.display()
        ))));
    }

    if !config.data_path.exists() {
        return Err(Box::new(ProducerError::DataRead(format!(
            "Data file not found: {}",
            config.data_path.display()
        ))));
    }

    let pipeline = KafkaPipeline::new(config)?;

    let (processed, errors) = process_copybook_to_kafka(
        &pipeline.config.copybook_path,
        &pipeline.config.data_path,
        pipeline.config.codepage,
        &pipeline,
    )?;

    pipeline.flush(5_000);
    info!(
        "Processing complete: {} records processed, {} errors",
        processed, errors
    );

    Ok(())
}
