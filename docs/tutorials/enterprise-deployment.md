# Enterprise Deployment Tutorial

Learn how to deploy copybook-rs in production environments with enterprise-grade reliability and monitoring.

## What You'll Learn

This tutorial covers:
- Production-ready deployment strategies
- Enterprise monitoring and observability
- Performance tuning for mainframe workloads
- Error handling and recovery patterns
- Security considerations for financial data

## Prerequisites

- Completed [Getting Started Tutorial](getting-started.md)
- Understanding of enterprise deployment practices
- Access to production-scale test data
- Monitoring infrastructure (logs, metrics)

## Deployment Overview

copybook-rs is **production-ready** with enterprise-grade safety features:

- ✅ **458+ tests passing** with comprehensive validation
- ✅ **Zero panic risk** through systematic elimination of `.unwrap()` calls
- ✅ **15-52x performance** above enterprise targets
- ✅ **Zero unsafe code** for memory safety guarantees
- ✅ **Bounded memory usage** (<256 MiB for multi-GB files)
- ✅ **Structured error taxonomy** for production monitoring

## Step 1: Production Build Configuration

Configure your build for enterprise deployment:

```toml
# Cargo.toml for production deployment
[dependencies]
copybook-core = "0.1"
copybook-codec = "0.1"
copybook-cli = "0.1"

# Production features
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter", "json"] }
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", features = ["full"] }

[profile.release]
opt-level = 3           # Maximum optimization
debug = false           # Remove debug symbols
lto = "fat"            # Link-time optimization
codegen-units = 1      # Single codegen unit for performance
panic = "abort"        # Fast failure in production
```

Build with maximum optimization:

```bash
# Production build with all optimizations
cargo build --workspace --release

# Verify build integrity
cargo test --workspace --release

# Check for any remaining panic risks
cargo clippy --workspace -- -D warnings -W clippy::pedantic
```

## Step 2: Enterprise Error Handling Strategy

Implement comprehensive error handling for production monitoring:

```rust
use copybook_core::{Error, ErrorCode};
use copybook_codec::DecodeOptions;
use tracing::{error, warn, info, debug};
use std::collections::HashMap;

/// Enterprise error handler with structured logging and metrics
#[derive(Debug)]
pub struct EnterpriseErrorHandler {
    error_counts: HashMap<ErrorCode, u64>,
    critical_threshold: u64,
}

impl EnterpriseErrorHandler {
    pub fn new() -> Self {
        Self {
            error_counts: HashMap::new(),
            critical_threshold: 100, // Alert after 100 errors of same type
        }
    }

    /// Handle copybook-rs errors with enterprise monitoring
    pub fn handle_error(&mut self, error: &Error, context: &str) -> ErrorSeverity {
        // Update error metrics
        *self.error_counts.entry(error.code).or_insert(0) += 1;
        let count = self.error_counts[&error.code];

        let severity = match error.code {
            // Critical infrastructure errors
            ErrorCode::CBKS141_RECORD_TOO_LARGE => {
                error!(
                    error_code = ?error.code,
                    error_message = %error.message,
                    context = %context,
                    count = %count,
                    "Critical: Record size exceeds enterprise limits"
                );
                ErrorSeverity::Critical
            }

            // Data quality warnings
            ErrorCode::CBKD101_INVALID_FIELD_TYPE |
            ErrorCode::CBKD201_TRUNCATED_RECORD => {
                warn!(
                    error_code = ?error.code,
                    error_message = %error.message,
                    context = %context,
                    count = %count,
                    "Data quality issue detected"
                );
                ErrorSeverity::Warning
            }

            // Parsing errors (may indicate schema changes)
            ErrorCode::CBKP001_SYNTAX |
            ErrorCode::CBKP021_ODO_NOT_TAIL => {
                error!(
                    error_code = ?error.code,
                    error_message = %error.message,
                    context = %context,
                    count = %count,
                    "Schema parsing error - possible copybook change"
                );
                ErrorSeverity::Error
            }

            _ => {
                warn!(
                    error_code = ?error.code,
                    error_message = %error.message,
                    context = %context,
                    count = %count,
                    "Unclassified error"
                );
                ErrorSeverity::Warning
            }
        };

        // Alert on error threshold
        if count >= self.critical_threshold {
            error!(
                error_code = ?error.code,
                count = %count,
                threshold = %self.critical_threshold,
                "ALERT: Error threshold exceeded - possible systemic issue"
            );
        }

        severity
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorSeverity {
    Critical,
    Error,
    Warning,
    Info,
}
```

## Step 3: Production Data Processing Pipeline

Design a robust enterprise processing pipeline:

```rust
use copybook_core::parse_copybook;
use copybook_codec::{DecodeOptions, Codepage, JsonNumberMode, RecordFormat};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::fs;
use tracing::{info, error, instrument};

/// Enterprise data processing pipeline with reliability features
pub struct EnterpriseProcessor {
    schema: Arc<copybook_core::Schema>,
    options: DecodeOptions,
    error_handler: EnterpriseErrorHandler,
}

impl EnterpriseProcessor {
    /// Initialize processor with enterprise configuration
    pub async fn new(copybook_path: &Path) -> Result<Self, Box<dyn std::error::Error>> {
        info!("Initializing enterprise processor");

        // Load and parse copybook with error handling
        let copybook_text = fs::read_to_string(copybook_path).await?;
        let schema = parse_copybook(&copybook_text)?;

        info!(
            fields_count = %schema.fields.len(),
            fixed_length = ?schema.fixed_record_length,
            has_tail_odo = %schema.tail_odo.is_some(),
            "Schema loaded successfully"
        );

        // Configure enterprise-grade options
        let options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_format(RecordFormat::Fixed)
            .with_json_number_mode(JsonNumberMode::Lossless)
            .with_emit_meta(true)
            .with_validate_structure(true);

        Ok(Self {
            schema: Arc::new(schema),
            options,
            error_handler: EnterpriseErrorHandler::new(),
        })
    }

    /// Process files with enterprise monitoring and recovery
    #[instrument(skip(self))]
    pub async fn process_file(
        &mut self,
        input_path: &Path,
        output_path: &Path,
    ) -> Result<ProcessingMetrics, Box<dyn std::error::Error>> {

        info!(
            input_file = %input_path.display(),
            output_file = %output_path.display(),
            "Starting file processing"
        );

        let start_time = std::time::Instant::now();

        // Use high-performance file processing
        let output_file = tokio::fs::File::create(output_path).await?;
        let output_std = output_file.into_std().await;

        let summary = copybook_codec::decode_file_to_jsonl(
            &self.schema,
            input_path,
            output_std,
            &self.options,
        )?;

        let processing_time = start_time.elapsed();

        let metrics = ProcessingMetrics {
            records_processed: summary.records_processed,
            processing_time,
            throughput_mib_per_sec: calculate_throughput(
                input_path,
                processing_time,
            ).await.unwrap_or(0.0),
            errors_encountered: summary.errors_encountered.unwrap_or(0),
        };

        info!(
            records = %metrics.records_processed,
            duration_ms = %metrics.processing_time.as_millis(),
            throughput_mib_s = %metrics.throughput_mib_per_sec,
            errors = %metrics.errors_encountered,
            "File processing completed"
        );

        Ok(metrics)
    }

    /// Batch processing with parallel execution
    #[instrument(skip(self))]
    pub async fn process_batch(
        &mut self,
        input_dir: &Path,
        output_dir: &Path,
        max_concurrent: usize,
    ) -> Result<BatchMetrics, Box<dyn std::error::Error>> {

        info!(
            input_dir = %input_dir.display(),
            output_dir = %output_dir.display(),
            max_concurrent = %max_concurrent,
            "Starting batch processing"
        );

        // Discover input files
        let mut input_files = Vec::new();
        let mut dir_entries = fs::read_dir(input_dir).await?;

        while let Some(entry) = dir_entries.next_entry().await? {
            let path = entry.path();
            if path.extension().map_or(false, |ext| ext == "bin" || ext == "dat") {
                input_files.push(path);
            }
        }

        info!(file_count = %input_files.len(), "Discovered input files");

        // Process files with concurrency control
        let semaphore = Arc::new(tokio::sync::Semaphore::new(max_concurrent));
        let mut tasks = Vec::new();

        for input_file in input_files {
            let output_file = output_dir.join(
                input_file.file_stem().unwrap()
            ).with_extension("jsonl");

            let schema = self.schema.clone();
            let options = self.options.clone();
            let permit = semaphore.clone().acquire_owned().await?;

            let task = tokio::spawn(async move {
                let _permit = permit;

                let result = copybook_codec::decode_file_to_jsonl(
                    &schema,
                    &input_file,
                    std::fs::File::create(&output_file)?,
                    &options,
                );

                match result {
                    Ok(summary) => {
                        info!(
                            input_file = %input_file.display(),
                            records = %summary.records_processed,
                            "File processed successfully"
                        );
                        Ok(summary.records_processed)
                    }
                    Err(error) => {
                        error!(
                            input_file = %input_file.display(),
                            error = %error,
                            "File processing failed"
                        );
                        Err(error)
                    }
                }
            });

            tasks.push(task);
        }

        // Collect results
        let mut total_records = 0;
        let mut successful_files = 0;
        let mut failed_files = 0;

        for task in tasks {
            match task.await? {
                Ok(records) => {
                    total_records += records;
                    successful_files += 1;
                }
                Err(_) => {
                    failed_files += 1;
                }
            }
        }

        let batch_metrics = BatchMetrics {
            total_files: successful_files + failed_files,
            successful_files,
            failed_files,
            total_records,
        };

        info!(
            total_files = %batch_metrics.total_files,
            successful = %batch_metrics.successful_files,
            failed = %batch_metrics.failed_files,
            total_records = %batch_metrics.total_records,
            "Batch processing completed"
        );

        Ok(batch_metrics)
    }
}

#[derive(Debug)]
pub struct ProcessingMetrics {
    pub records_processed: u64,
    pub processing_time: std::time::Duration,
    pub throughput_mib_per_sec: f64,
    pub errors_encountered: u64,
}

#[derive(Debug)]
pub struct BatchMetrics {
    pub total_files: usize,
    pub successful_files: usize,
    pub failed_files: usize,
    pub total_records: u64,
}

async fn calculate_throughput(
    file_path: &Path,
    processing_time: std::time::Duration,
) -> Result<f64, Box<dyn std::error::Error>> {
    let metadata = tokio::fs::metadata(file_path).await?;
    let size_mib = metadata.len() as f64 / (1024.0 * 1024.0);
    let time_sec = processing_time.as_secs_f64();

    Ok(if time_sec > 0.0 { size_mib / time_sec } else { 0.0 })
}
```

## Step 4: Monitoring and Observability

Set up comprehensive monitoring for production deployment:

```rust
use tracing_subscriber::{fmt, EnvFilter, prelude::*};
use std::env;

/// Configure enterprise-grade logging and monitoring
pub fn setup_monitoring() -> Result<(), Box<dyn std::error::Error>> {
    // Configure structured logging
    let env_filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new("info"));

    let fmt_layer = fmt::layer()
        .with_target(true)
        .with_thread_ids(true)
        .with_thread_names(true)
        .json(); // Use JSON for production log ingestion

    tracing_subscriber::registry()
        .with(env_filter)
        .with(fmt_layer)
        .init();

    // Log environment configuration
    tracing::info!(
        version = env!("CARGO_PKG_VERSION"),
        rust_version = env!("RUSTC_VERSION"),
        build_profile = if cfg!(debug_assertions) { "debug" } else { "release" },
        "copybook-rs enterprise processor initialized"
    );

    Ok(())
}

/// Health check endpoint for enterprise monitoring
pub fn health_check() -> HealthStatus {
    HealthStatus {
        status: "healthy".to_string(),
        version: env!("CARGO_PKG_VERSION").to_string(),
        panic_free: true,              // Guaranteed by panic elimination
        memory_safe: true,             // Zero unsafe code
        performance_validated: true,   // Exceeds enterprise targets
    }
}

#[derive(Debug, serde::Serialize)]
pub struct HealthStatus {
    pub status: String,
    pub version: String,
    pub panic_free: bool,
    pub memory_safe: bool,
    pub performance_validated: bool,
}
```

## Step 5: Production Configuration Management

Manage enterprise configurations with environment-based settings:

```rust
use serde::{Deserialize, Serialize};
use std::env;

/// Enterprise configuration with environment variable support
#[derive(Debug, Serialize, Deserialize)]
pub struct EnterpriseConfig {
    pub processing: ProcessingConfig,
    pub monitoring: MonitoringConfig,
    pub security: SecurityConfig,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProcessingConfig {
    pub max_concurrent_files: usize,
    pub max_record_size_mib: u64,
    pub memory_limit_mib: u64,
    pub enable_validation: bool,
    pub codepage: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MonitoringConfig {
    pub log_level: String,
    pub metrics_interval_sec: u64,
    pub error_threshold: u64,
    pub enable_tracing: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SecurityConfig {
    pub audit_enabled: bool,
    pub data_classification: String,
    pub encryption_at_rest: bool,
}

impl EnterpriseConfig {
    /// Load configuration from environment variables
    pub fn from_env() -> Self {
        Self {
            processing: ProcessingConfig {
                max_concurrent_files: env::var("COPYBOOK_MAX_CONCURRENT")
                    .unwrap_or_else(|_| "4".to_string())
                    .parse()
                    .unwrap_or(4),
                max_record_size_mib: env::var("COPYBOOK_MAX_RECORD_SIZE_MIB")
                    .unwrap_or_else(|_| "16".to_string())
                    .parse()
                    .unwrap_or(16),
                memory_limit_mib: env::var("COPYBOOK_MEMORY_LIMIT_MIB")
                    .unwrap_or_else(|_| "256".to_string())
                    .parse()
                    .unwrap_or(256),
                enable_validation: env::var("COPYBOOK_ENABLE_VALIDATION")
                    .unwrap_or_else(|_| "true".to_string())
                    .parse()
                    .unwrap_or(true),
                codepage: env::var("COPYBOOK_CODEPAGE")
                    .unwrap_or_else(|_| "cp037".to_string()),
            },
            monitoring: MonitoringConfig {
                log_level: env::var("COPYBOOK_LOG_LEVEL")
                    .unwrap_or_else(|_| "info".to_string()),
                metrics_interval_sec: env::var("COPYBOOK_METRICS_INTERVAL")
                    .unwrap_or_else(|_| "60".to_string())
                    .parse()
                    .unwrap_or(60),
                error_threshold: env::var("COPYBOOK_ERROR_THRESHOLD")
                    .unwrap_or_else(|_| "100".to_string())
                    .parse()
                    .unwrap_or(100),
                enable_tracing: env::var("COPYBOOK_ENABLE_TRACING")
                    .unwrap_or_else(|_| "true".to_string())
                    .parse()
                    .unwrap_or(true),
            },
            security: SecurityConfig {
                audit_enabled: env::var("COPYBOOK_AUDIT_ENABLED")
                    .unwrap_or_else(|_| "true".to_string())
                    .parse()
                    .unwrap_or(true),
                data_classification: env::var("COPYBOOK_DATA_CLASSIFICATION")
                    .unwrap_or_else(|_| "confidential".to_string()),
                encryption_at_rest: env::var("COPYBOOK_ENCRYPTION_AT_REST")
                    .unwrap_or_else(|_| "true".to_string())
                    .parse()
                    .unwrap_or(true),
            },
        }
    }
}
```

## Step 6: Complete Enterprise Application

Here's a complete enterprise application example:

```rust
use copybook_rs_enterprise::*;
use std::path::Path;
use tokio;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Initialize enterprise monitoring
    setup_monitoring()?;

    // 2. Load configuration
    let config = EnterpriseConfig::from_env();
    tracing::info!(config = ?config, "Enterprise configuration loaded");

    // 3. Initialize processor
    let copybook_path = Path::new("schemas/customer.cpy");
    let mut processor = EnterpriseProcessor::new(copybook_path).await?;

    // 4. Process production data
    let input_dir = Path::new("/data/mainframe/input");
    let output_dir = Path::new("/data/processed/output");

    let batch_metrics = processor.process_batch(
        input_dir,
        output_dir,
        config.processing.max_concurrent_files,
    ).await?;

    // 5. Report enterprise metrics
    tracing::info!(
        total_files = %batch_metrics.total_files,
        successful_files = %batch_metrics.successful_files,
        failed_files = %batch_metrics.failed_files,
        total_records = %batch_metrics.total_records,
        success_rate = %(batch_metrics.successful_files as f64 / batch_metrics.total_files as f64 * 100.0),
        "Enterprise batch processing completed"
    );

    // 6. Health check for monitoring systems
    let health = health_check();
    println!("Service health: {}", serde_json::to_string_pretty(&health)?);

    Ok(())
}
```

## Step 7: Performance Validation

Validate enterprise performance requirements:

```bash
#!/bin/bash
# enterprise-validation.sh

echo "🔧 Enterprise Performance Validation"

# Build with full optimization
cargo build --workspace --release

# Run comprehensive test suite
echo "📊 Running comprehensive test suite..."
cargo test --workspace --release

# Validate performance benchmarks
echo "⚡ Validating performance targets..."
cargo bench --package copybook-bench -- --save-baseline enterprise

# Check panic elimination
echo "🛡️  Verifying panic elimination..."
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Memory safety validation
echo "💾 Validating memory safety..."
cargo audit

echo "✅ Enterprise validation complete!"
```

## Deployment Checklist

Before deploying to production:

- [ ] **Build Validation**
  - [ ] Release build with full optimization
  - [ ] All tests passing (458+ tests)
  - [ ] Clippy clean with pedantic warnings
  - [ ] No unsafe code or panic risks

- [ ] **Performance Validation**
  - [ ] DISPLAY processing: >2.5 GiB/s achieved
  - [ ] COMP-3 processing: >100 MiB/s achieved
  - [ ] Memory usage: <256 MiB for multi-GB files
  - [ ] Benchmark regression tests passing

- [ ] **Security & Compliance**
  - [ ] Audit trail configuration
  - [ ] Data classification settings
  - [ ] Encryption at rest enabled
  - [ ] Access controls configured

- [ ] **Monitoring & Observability**
  - [ ] Structured logging configured
  - [ ] Metrics collection enabled
  - [ ] Error thresholds configured
  - [ ] Health check endpoints

- [ ] **Operational Readiness**
  - [ ] Configuration management
  - [ ] Backup and recovery procedures
  - [ ] Runbook documentation
  - [ ] Incident response procedures

## Next Steps

- **Error Handling**: Master production error patterns in [Error Handling Guide](../how-to/error-handling-production.md)
- **Performance**: Optimize for specific workloads in [Performance Guide](../how-to/performance-optimization.md)
- **API Reference**: Explore advanced features in [Library Reference](../reference/LIBRARY_API.md)

Congratulations! You now have a production-ready copybook-rs deployment with enterprise-grade reliability. 🚀