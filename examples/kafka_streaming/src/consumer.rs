// SPDX-License-Identifier: AGPL-3.0-or-later
//! Kafka consumer example for copybook-rs.
//!
//! Reads JSON messages produced by the Kafka producer example.

use rdkafka::consumer::{CommitMode, Consumer, StreamConsumer};
use rdkafka::message::Message;
use rdkafka::config::ClientConfig;
use serde_json::Value;
use std::env;
use std::str::from_utf8;
use thiserror::Error;
use futures::StreamExt;
use tokio::time::{sleep, Duration};
use tracing::{info, warn};

const DEFAULT_BROKERS: &str = "localhost:9092";
const DEFAULT_TOPIC: &str = "copybook-data";
const DEFAULT_GROUP_ID: &str = "copybook-streaming-consumer";

#[derive(Error, Debug)]
enum ConsumerError {
    #[error("Kafka error: {0}")]
    Kafka(String),
}

struct ConsumerConfig {
    brokers: String,
    topic: String,
    group_id: String,
    max_messages: Option<usize>,
    poll_interval_ms: u64,
}

impl ConsumerConfig {
    fn from_env() -> Self {
        let max_messages = env::var("MAX_MESSAGES").ok().and_then(|value| value.parse().ok());
        let poll_interval_ms = env::var("POLL_INTERVAL_MS")
            .ok()
            .and_then(|value| value.parse().ok())
            .unwrap_or(250);

        Self {
            brokers: env::var("KAFKA_BROKERS").unwrap_or_else(|_| DEFAULT_BROKERS.to_string()),
            topic: env::var("KAFKA_TOPIC").unwrap_or_else(|_| DEFAULT_TOPIC.to_string()),
            group_id: env::var("KAFKA_GROUP_ID")
                .unwrap_or_else(|_| DEFAULT_GROUP_ID.to_string()),
            max_messages,
            poll_interval_ms,
        }
    }
}

fn initialize_consumer(config: &ConsumerConfig) -> Result<StreamConsumer, ConsumerError> {
    let consumer: StreamConsumer = ClientConfig::new()
        .set("bootstrap.servers", &config.brokers)
        .set("group.id", &config.group_id)
        .set("enable.auto.commit", "false")
        .set("auto.offset.reset", "earliest")
        .set("session.timeout.ms", "6000")
        .create()
        .map_err(|error| ConsumerError::Kafka(format!("Failed to create consumer: {error}")))?;

    consumer
        .subscribe(&[&config.topic])
        .map_err(|error| ConsumerError::Kafka(format!("Failed to subscribe to {}: {error}", config.topic)))?;

    Ok(consumer)
}

fn message_key(message: &impl Message) -> String {
    match message.key() {
        Some(bytes) => from_utf8(bytes).map_or_else(
            |_| format!("0x{}", bytes.iter().map(|byte| format!("{byte:02X}")).collect::<String>()),
            |value| value.to_string(),
        ),
        None => "<none>".to_string(),
    }
}

fn pretty_payload(value: &[u8]) -> String {
    serde_json::from_slice::<Value>(value)
        .map_or_else(
            |_| format!("0x{}", value.iter().map(|byte| format!("{byte:02X}")).collect::<String>()),
            |json| serde_json::to_string_pretty(&json).unwrap_or_else(|_| String::from_utf8_lossy(value).to_string()),
        )
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .with_target(false)
        .init();

    info!("Starting Kafka consumer example");

    let config = ConsumerConfig::from_env();
    let consumer = initialize_consumer(&config)?;
    let mut stream = consumer.stream();

    let mut count = 0usize;
    loop {
        match stream.next().await {
            Some(Ok(message)) => {
                let key = message_key(&message);
                let payload = message
                    .payload()
                    .map(pretty_payload)
                    .unwrap_or_else(|| "<empty payload>".to_string());
                let offset = message.offset();
                let partition = message.partition();
                let topic = message.topic();

                info!("Consumed record on {topic}[{partition}]@{offset} key={key}");
                println!("{payload}");
                info!("--------------------------------");

                if let Err(error) = consumer.commit_message(&message, CommitMode::Async) {
                    warn!("Failed to commit offset: {error}");
                }

                count += 1;
                if let Some(max_messages) = config.max_messages {
                    if count >= max_messages {
                        info!("Reached message limit ({max_messages}); exiting.");
                        break;
                    }
                }
            }
            Some(Err(error)) => {
                warn!("Consumer stream error: {error}");
                sleep(Duration::from_millis(config.poll_interval_ms)).await;
            }
            None => {
                sleep(Duration::from_millis(config.poll_interval_ms)).await;
            }
        }
    }

    info!("Consumed {} messages", count);
    Ok(())
}
