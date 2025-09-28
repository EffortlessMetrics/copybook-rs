# How to Handle Errors in Production

A task-oriented guide for implementing robust error handling patterns with copybook-rs in production environments.

## Overview

This guide shows you how to implement enterprise-grade error handling using copybook-rs's comprehensive error taxonomy and panic-safe operations. Learn to handle specific error scenarios, implement monitoring, and ensure graceful failure recovery.

## Quick Reference

copybook-rs provides structured error codes in four main categories:

- **`CBKP*`** - Parse errors (syntax, unsupported features)
- **`CBKS*`** - Schema validation (ODO counters, record limits)
- **`CBKD*`** - Data errors (invalid decimals, truncated records)
- **`CBKE*`** - Encoding errors (type mismatches, bounds)

## Common Error Scenarios

### Handling Parse Errors

**Problem**: COBOL copybook syntax errors during schema loading.

**Solution**: Implement comprehensive parse error handling:

```rust
use copybook_core::{parse_copybook, Error, ErrorCode};

fn load_schema_safely(copybook_path: &str) -> Result<copybook_core::Schema, ProcessingError> {
    let copybook_text = std::fs::read_to_string(copybook_path)
        .map_err(|e| ProcessingError::FileAccess {
            path: copybook_path.to_string(),
            source: e,
        })?;

    match parse_copybook(&copybook_text) {
        Ok(schema) => {
            tracing::info!(
                fields = %schema.fields.len(),
                fixed_length = ?schema.fixed_record_length,
                "Schema loaded successfully"
            );
            Ok(schema)
        }
        Err(error) => {
            match error.code {
                ErrorCode::CBKP001_SYNTAX => {
                    tracing::error!(
                        error_code = ?error.code,
                        message = %error.message,
                        file = %copybook_path,
                        "Syntax error in copybook - check field definitions"
                    );

                    // Extract line information if available
                    if error.message.contains("line") {
                        Err(ProcessingError::SyntaxError {
                            file: copybook_path.to_string(),
                            details: error.message,
                            suggestion: "Verify COBOL syntax, PIC clauses, and level numbers".to_string(),
                        })
                    } else {
                        Err(ProcessingError::ParseError(error))
                    }
                }

                ErrorCode::CBKP021_ODO_NOT_TAIL => {
                    tracing::error!(
                        error_code = ?error.code,
                        message = %error.message,
                        file = %copybook_path,
                        "ODO array must be positioned at end of structure"
                    );

                    Err(ProcessingError::StructuralError {
                        file: copybook_path.to_string(),
                        issue: "ODO array positioning".to_string(),
                        details: error.message,
                        fix: "Move OCCURS DEPENDING ON array to end of record structure".to_string(),
                    })
                }

                _ => {
                    tracing::error!(
                        error_code = ?error.code,
                        message = %error.message,
                        file = %copybook_path,
                        "Unexpected parse error"
                    );
                    Err(ProcessingError::ParseError(error))
                }
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum ProcessingError {
    #[error("File access error for {path}: {source}")]
    FileAccess {
        path: String,
        #[source]
        source: std::io::Error,
    },

    #[error("Syntax error in {file}: {details}\nSuggestion: {suggestion}")]
    SyntaxError {
        file: String,
        details: String,
        suggestion: String,
    },

    #[error("Structural error in {file}: {issue}\nDetails: {details}\nFix: {fix}")]
    StructuralError {
        file: String,
        issue: String,
        details: String,
        fix: String,
    },

    #[error("Parse error: {0}")]
    ParseError(#[from] Error),
}
```

### Handling Data Validation Errors

**Problem**: Invalid or corrupted mainframe data during processing.

**Solution**: Implement graceful data error recovery:

```rust
use copybook_codec::{decode_record, DecodeOptions, Error as CodecError};

fn process_record_safely(
    schema: &copybook_core::Schema,
    record_data: &[u8],
    options: &DecodeOptions,
    record_number: u64,
) -> RecordResult {
    match decode_record(schema, record_data, options) {
        Ok(json_value) => {
            RecordResult::Success(json_value)
        }
        Err(error) => {
            match &error {
                CodecError::DataError { code, message, field_path, .. } => {
                    tracing::warn!(
                        record_number = %record_number,
                        error_code = ?code,
                        field_path = ?field_path,
                        message = %message,
                        "Data validation error - record skipped"
                    );

                    // Categorize data errors for different handling
                    match code {
                        copybook_codec::ErrorCode::CBKD101_INVALID_FIELD_TYPE => {
                            RecordResult::DataError {
                                record_number,
                                field: field_path.clone().unwrap_or_default(),
                                issue: "Invalid field type".to_string(),
                                action: "Record skipped - verify data format".to_string(),
                            }
                        }

                        copybook_codec::ErrorCode::CBKD201_TRUNCATED_RECORD => {
                            RecordResult::DataError {
                                record_number,
                                field: "record_length".to_string(),
                                issue: "Record truncated".to_string(),
                                action: "Record skipped - check file integrity".to_string(),
                            }
                        }

                        _ => RecordResult::DataError {
                            record_number,
                            field: field_path.clone().unwrap_or_default(),
                            issue: message.clone(),
                            action: "Record skipped".to_string(),
                        }
                    }
                }

                _ => {
                    tracing::error!(
                        record_number = %record_number,
                        error = %error,
                        "Unexpected decode error"
                    );

                    RecordResult::UnexpectedError {
                        record_number,
                        error: error.to_string(),
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
enum RecordResult {
    Success(serde_json::Value),
    DataError {
        record_number: u64,
        field: String,
        issue: String,
        action: String,
    },
    UnexpectedError {
        record_number: u64,
        error: String,
    },
}
```

### Handling Performance and Resource Errors

**Problem**: Memory limits, processing timeouts, or resource exhaustion.

**Solution**: Implement resource monitoring and circuit breakers:

```rust
use std::time::{Duration, Instant};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

pub struct ResourceMonitor {
    max_memory_mb: u64,
    processing_timeout: Duration,
    error_count: Arc<AtomicU64>,
    error_threshold: u64,
}

impl ResourceMonitor {
    pub fn new(max_memory_mb: u64, timeout_sec: u64, error_threshold: u64) -> Self {
        Self {
            max_memory_mb,
            processing_timeout: Duration::from_secs(timeout_sec),
            error_count: Arc::new(AtomicU64::new(0)),
            error_threshold,
        }
    }

    /// Check if processing should continue or circuit breaker should activate
    pub fn should_continue_processing(&self) -> Result<(), ResourceError> {
        // Check error rate
        let current_errors = self.error_count.load(Ordering::Relaxed);
        if current_errors > self.error_threshold {
            return Err(ResourceError::CircuitBreakerOpen {
                error_count: current_errors,
                threshold: self.error_threshold,
            });
        }

        // Check memory usage (simplified - use actual memory monitoring in production)
        if let Ok(memory_usage) = get_memory_usage_mb() {
            if memory_usage > self.max_memory_mb {
                return Err(ResourceError::MemoryExhausted {
                    current_mb: memory_usage,
                    limit_mb: self.max_memory_mb,
                });
            }
        }

        Ok(())
    }

    /// Process with timeout and resource monitoring
    pub fn process_with_timeout<T, F>(&self, operation: F) -> Result<T, ResourceError>
    where
        F: FnOnce() -> Result<T, Box<dyn std::error::Error>> + Send,
        T: Send,
    {
        self.should_continue_processing()?;

        let start_time = Instant::now();

        // Execute operation with timeout (simplified - use tokio::timeout in real implementation)
        let result = operation();

        let elapsed = start_time.elapsed();
        if elapsed > self.processing_timeout {
            self.error_count.fetch_add(1, Ordering::Relaxed);
            return Err(ResourceError::ProcessingTimeout {
                elapsed,
                limit: self.processing_timeout,
            });
        }

        match result {
            Ok(value) => Ok(value),
            Err(error) => {
                self.error_count.fetch_add(1, Ordering::Relaxed);
                Err(ResourceError::ProcessingFailed {
                    details: error.to_string(),
                })
            }
        }
    }

    /// Reset error counter (call after successful batch completion)
    pub fn reset_error_count(&self) {
        self.error_count.store(0, Ordering::Relaxed);
    }
}

#[derive(Debug, thiserror::Error)]
enum ResourceError {
    #[error("Circuit breaker open: {error_count} errors exceed threshold {threshold}")]
    CircuitBreakerOpen {
        error_count: u64,
        threshold: u64,
    },

    #[error("Memory exhausted: {current_mb}MB exceeds limit {limit_mb}MB")]
    MemoryExhausted {
        current_mb: u64,
        limit_mb: u64,
    },

    #[error("Processing timeout: {elapsed:?} exceeds limit {limit:?}")]
    ProcessingTimeout {
        elapsed: Duration,
        limit: Duration,
    },

    #[error("Processing failed: {details}")]
    ProcessingFailed {
        details: String,
    },
}

// Placeholder - implement actual memory monitoring
fn get_memory_usage_mb() -> Result<u64, Box<dyn std::error::Error>> {
    // Use system-specific memory monitoring
    Ok(128) // Placeholder
}
```

### Implementing Comprehensive Error Aggregation

**Problem**: Need to collect and analyze error patterns across large processing jobs.

**Solution**: Create an error aggregation system:

```rust
use std::collections::HashMap;
use serde::{Serialize, Deserialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct ErrorSummary {
    pub total_records: u64,
    pub successful_records: u64,
    pub failed_records: u64,
    pub error_breakdown: HashMap<String, ErrorStats>,
    pub processing_duration: Duration,
    pub recommendations: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ErrorStats {
    pub count: u64,
    pub percentage: f64,
    pub first_occurrence: String,
    pub sample_message: String,
    pub suggested_action: String,
}

pub struct ErrorAggregator {
    errors: HashMap<String, Vec<ProcessingError>>,
    total_records: u64,
    successful_records: u64,
    start_time: Instant,
}

impl ErrorAggregator {
    pub fn new() -> Self {
        Self {
            errors: HashMap::new(),
            total_records: 0,
            successful_records: 0,
            start_time: Instant::now(),
        }
    }

    pub fn record_success(&mut self) {
        self.total_records += 1;
        self.successful_records += 1;
    }

    pub fn record_error(&mut self, error: ProcessingError) {
        self.total_records += 1;

        let error_key = match &error {
            ProcessingError::SyntaxError { .. } => "syntax_error",
            ProcessingError::StructuralError { .. } => "structural_error",
            ProcessingError::DataError { .. } => "data_error",
            ProcessingError::ResourceError { .. } => "resource_error",
            _ => "other_error",
        };

        self.errors.entry(error_key.to_string())
            .or_insert_with(Vec::new)
            .push(error);
    }

    pub fn generate_summary(&self) -> ErrorSummary {
        let failed_records = self.total_records - self.successful_records;
        let mut error_breakdown = HashMap::new();
        let mut recommendations = Vec::new();

        for (error_type, error_list) in &self.errors {
            let count = error_list.len() as u64;
            let percentage = if self.total_records > 0 {
                (count as f64 / self.total_records as f64) * 100.0
            } else {
                0.0
            };

            let (sample_message, suggested_action) = if let Some(first_error) = error_list.first() {
                match first_error {
                    ProcessingError::SyntaxError { details, suggestion, .. } => {
                        (details.clone(), suggestion.clone())
                    }
                    ProcessingError::StructuralError { details, fix, .. } => {
                        (details.clone(), fix.clone())
                    }
                    _ => {
                        (first_error.to_string(), "Review error details and data quality".to_string())
                    }
                }
            } else {
                ("No sample available".to_string(), "No action required".to_string())
            };

            error_breakdown.insert(error_type.clone(), ErrorStats {
                count,
                percentage,
                first_occurrence: chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC").to_string(),
                sample_message,
                suggested_action: suggested_action.clone(),
            });

            // Generate recommendations based on error patterns
            if percentage > 10.0 {
                recommendations.push(format!(
                    "High {error_type} rate ({percentage:.1}%): {suggested_action}"
                ));
            }
        }

        // Add general recommendations
        if failed_records == 0 {
            recommendations.push("✅ All records processed successfully".to_string());
        } else if (failed_records as f64 / self.total_records as f64) < 0.01 {
            recommendations.push("✅ Low error rate (<1%) - monitoring recommended".to_string());
        } else {
            recommendations.push("⚠️  Elevated error rate - review data quality and schema validation".to_string());
        }

        ErrorSummary {
            total_records: self.total_records,
            successful_records: self.successful_records,
            failed_records,
            error_breakdown,
            processing_duration: self.start_time.elapsed(),
            recommendations,
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum ProcessingError {
    #[error("Syntax error: {details}")]
    SyntaxError {
        details: String,
        suggestion: String,
    },

    #[error("Structural error: {details}")]
    StructuralError {
        details: String,
        fix: String,
    },

    #[error("Data error: {details}")]
    DataError {
        details: String,
    },

    #[error("Resource error: {details}")]
    ResourceError {
        details: String,
    },

    #[error("Other error: {details}")]
    Other {
        details: String,
    },
}
```

## Production Integration Example

Here's how to integrate all error handling patterns in a production system:

```rust
use tracing::{info, warn, error};

pub async fn process_production_batch(
    copybook_path: &str,
    input_files: Vec<&str>,
    output_dir: &str,
) -> Result<BatchSummary, Box<dyn std::error::Error>> {

    info!("Starting production batch processing");

    // 1. Initialize error handling and monitoring
    let mut error_aggregator = ErrorAggregator::new();
    let resource_monitor = ResourceMonitor::new(
        256,   // 256MB memory limit
        300,   // 5 minute timeout per file
        100,   // Error threshold
    );

    // 2. Load schema with comprehensive error handling
    let schema = match load_schema_safely(copybook_path) {
        Ok(schema) => schema,
        Err(error) => {
            error!(error = %error, "Failed to load schema");
            return Err(error.into());
        }
    };

    // 3. Configure processing options
    let options = copybook_codec::DecodeOptions::new()
        .with_codepage(copybook_codec::Codepage::CP037)
        .with_format(copybook_codec::RecordFormat::Fixed)
        .with_json_number_mode(copybook_codec::JsonNumberMode::Lossless)
        .with_emit_meta(true)
        .with_validate_structure(true);

    // 4. Process files with error handling and monitoring
    let mut successful_files = 0;
    let mut failed_files = 0;

    for (file_index, input_file) in input_files.iter().enumerate() {
        // Check resource limits before processing each file
        if let Err(resource_error) = resource_monitor.should_continue_processing() {
            warn!(
                resource_error = %resource_error,
                remaining_files = %(input_files.len() - file_index),
                "Stopping batch due to resource constraints"
            );
            break;
        }

        let output_file = format!("{}/output_{}.jsonl", output_dir, file_index);

        // Process file with timeout and monitoring
        let result = resource_monitor.process_with_timeout(|| {
            let output = std::fs::File::create(&output_file)?;
            copybook_codec::decode_file_to_jsonl(&schema, input_file, output, &options)
                .map_err(|e| e.into())
        });

        match result {
            Ok(summary) => {
                info!(
                    file = %input_file,
                    records = %summary.records_processed,
                    "File processed successfully"
                );
                successful_files += 1;
                error_aggregator.record_success();
            }
            Err(resource_error) => {
                error!(
                    file = %input_file,
                    error = %resource_error,
                    "File processing failed"
                );
                failed_files += 1;
                error_aggregator.record_error(ProcessingError::ResourceError {
                    details: resource_error.to_string(),
                });
            }
        }
    }

    // 5. Generate comprehensive error summary
    let error_summary = error_aggregator.generate_summary();

    info!(
        total_files = %(successful_files + failed_files),
        successful_files = %successful_files,
        failed_files = %failed_files,
        error_summary = ?error_summary,
        "Batch processing completed"
    );

    // 6. Reset error counters for successful batches
    if failed_files == 0 {
        resource_monitor.reset_error_count();
    }

    Ok(BatchSummary {
        successful_files,
        failed_files,
        error_summary,
    })
}

#[derive(Debug)]
pub struct BatchSummary {
    pub successful_files: usize,
    pub failed_files: usize,
    pub error_summary: ErrorSummary,
}
```

## Monitoring and Alerting

Set up monitoring for error patterns:

```rust
use tracing::{event, Level};

// Custom metrics for enterprise monitoring
pub fn emit_error_metrics(error_summary: &ErrorSummary) {
    // Emit structured metrics for monitoring systems
    event!(
        Level::INFO,
        metric_type = "batch_summary",
        total_records = %error_summary.total_records,
        successful_records = %error_summary.successful_records,
        failed_records = %error_summary.failed_records,
        success_rate = %(error_summary.successful_records as f64 / error_summary.total_records as f64 * 100.0),
        processing_duration_ms = %error_summary.processing_duration.as_millis(),
        "Batch processing metrics"
    );

    // Emit error-specific metrics
    for (error_type, stats) in &error_summary.error_breakdown {
        event!(
            Level::WARN,
            metric_type = "error_stats",
            error_type = %error_type,
            count = %stats.count,
            percentage = %stats.percentage,
            "Error type statistics"
        );

        // Alert on high error rates
        if stats.percentage > 5.0 {
            event!(
                Level::ERROR,
                alert_type = "high_error_rate",
                error_type = %error_type,
                percentage = %stats.percentage,
                suggested_action = %stats.suggested_action,
                "High error rate detected - immediate attention required"
            );
        }
    }
}
```

## Best Practices Summary

1. **Always use structured error handling** - Never ignore or suppress errors
2. **Implement circuit breakers** - Protect against cascading failures
3. **Monitor resource usage** - Prevent memory exhaustion and timeouts
4. **Aggregate error patterns** - Identify systemic issues early
5. **Provide actionable error messages** - Include specific suggestions for resolution
6. **Log with structured data** - Enable automated monitoring and alerting
7. **Test error scenarios** - Validate error handling under various failure conditions

## Related Guides

- [Performance Optimization](performance-optimization.md) - Optimize processing speed while maintaining error handling
- [Enterprise Deployment Tutorial](../tutorials/enterprise-deployment.md) - Complete production deployment patterns
- [Error Code Reference](../reference/ERROR_CODES.md) - Complete error code documentation

With these patterns, your copybook-rs production system will gracefully handle errors and provide comprehensive insights for operational monitoring. 🛡️
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
