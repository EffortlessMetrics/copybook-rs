//! Structured error reporting and handling system
//!
//! This module provides comprehensive error reporting with configurable handling modes,
//! detailed logging, and summary statistics for copybook processing operations.

use crate::error::{Error, ErrorCode};
use std::collections::HashMap;
use std::fmt;
use tracing::{debug, error, warn};

/// Error handling mode configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorMode {
    /// Stop processing on first error
    Strict,
    /// Continue processing, skip bad records
    Lenient,
}

/// Error severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorSeverity {
    /// Informational messages
    Info,
    /// Warning conditions that don't prevent processing
    Warning,
    /// Error conditions that affect individual records
    Error,
    /// Fatal conditions that prevent further processing
    Fatal,
}

/// Detailed error report with context and metadata
#[derive(Debug, Clone)]
pub struct ErrorReport {
    /// The underlying error
    pub error: Error,
    /// Error severity level
    pub severity: ErrorSeverity,
    /// Timestamp when error occurred
    pub timestamp: std::time::SystemTime,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

/// Comprehensive error statistics and summary
#[derive(Debug, Clone, Default)]
pub struct ErrorSummary {
    /// Total number of errors by severity
    pub error_counts: HashMap<ErrorSeverity, u64>,
    /// Error counts by error code
    pub error_codes: HashMap<ErrorCode, u64>,
    /// Records that had errors
    pub records_with_errors: u64,
    /// Total records processed
    pub total_records: u64,
    /// First error encountered (for debugging)
    pub first_error: Option<ErrorReport>,
    /// Most recent error (for debugging)
    pub last_error: Option<ErrorReport>,
    /// Transfer corruption warnings detected
    pub corruption_warnings: u64,
}

/// Structured error reporter with configurable handling
pub struct ErrorReporter {
    /// Error handling mode
    mode: ErrorMode,
    /// Maximum errors before stopping (None = unlimited)
    max_errors: Option<u64>,
    /// Current error summary
    summary: ErrorSummary,
    /// Whether to log detailed error context
    verbose_logging: bool,
}

impl ErrorReporter {
    /// Create a new error reporter with the specified mode
    pub fn new(mode: ErrorMode, max_errors: Option<u64>) -> Self {
        Self {
            mode,
            max_errors,
            summary: ErrorSummary::default(),
            verbose_logging: true,
        }
    }

    /// Set verbose logging mode
    pub fn with_verbose_logging(mut self, verbose: bool) -> Self {
        self.verbose_logging = verbose;
        self
    }

    /// Report an error and determine if processing should continue
    ///
    /// Returns `Ok(())` if processing should continue, `Err(error)` if it should stop
    pub fn report_error(&mut self, error: Error) -> Result<(), Error> {
        let severity = self.determine_severity(&error);
        let report = ErrorReport {
            error: error.clone(),
            severity,
            timestamp: std::time::SystemTime::now(),
            metadata: HashMap::new(),
        };

        // Check if we should stop processing before updating statistics
        let should_continue = match severity {
            ErrorSeverity::Fatal => false,
            ErrorSeverity::Error => {
                if self.mode == ErrorMode::Strict {
                    false
                } else if let Some(max) = self.max_errors {
                    let current_error_count = self
                        .summary
                        .error_counts
                        .get(&ErrorSeverity::Error)
                        .unwrap_or(&0);
                    *current_error_count < max
                } else {
                    true
                }
            }
            ErrorSeverity::Warning | ErrorSeverity::Info => true,
        };

        // Update statistics
        self.update_statistics(&report);

        // Log the error with appropriate level
        self.log_error(&report);

        // Return result based on decision
        if should_continue {
            Ok(())
        } else if matches!(severity, ErrorSeverity::Error) && self.max_errors.is_some() {
            Err(Error::new(
                ErrorCode::CBKS141_RECORD_TOO_LARGE, // Reusing for "too many errors"
                format!("Maximum error limit reached: {}", self.max_errors.unwrap())
            ))
        } else {
            Err(error)
        }
    }

    /// Report a warning (always continues processing)
    pub fn report_warning(&mut self, error: Error) {
        let mut report = ErrorReport {
            error,
            severity: ErrorSeverity::Warning,
            timestamp: std::time::SystemTime::now(),
            metadata: HashMap::new(),
        };

        // Check for transfer corruption patterns
        if self.is_corruption_warning(&report.error) {
            self.summary.corruption_warnings += 1;
            report
                .metadata
                .insert("corruption_type".to_string(), "transfer".to_string());
        }

        self.update_statistics(&report);
        self.log_error(&report);
    }

    /// Report record processing start (for context tracking)
    pub fn start_record(&mut self, record_index: u64) {
        self.summary.total_records = record_index;
        debug!("Processing record {}", record_index);
    }

    /// Get the current error summary
    pub fn summary(&self) -> &ErrorSummary {
        &self.summary
    }

    /// Check if any errors have been reported
    pub fn has_errors(&self) -> bool {
        self.summary
            .error_counts
            .get(&ErrorSeverity::Error)
            .unwrap_or(&0)
            > &0
            || self
                .summary
                .error_counts
                .get(&ErrorSeverity::Fatal)
                .unwrap_or(&0)
                > &0
    }

    /// Check if any warnings have been reported
    pub fn has_warnings(&self) -> bool {
        self.summary
            .error_counts
            .get(&ErrorSeverity::Warning)
            .unwrap_or(&0)
            > &0
    }

    /// Get total error count (excluding warnings)
    pub fn error_count(&self) -> u64 {
        self.summary
            .error_counts
            .get(&ErrorSeverity::Error)
            .unwrap_or(&0)
            + self
                .summary
                .error_counts
                .get(&ErrorSeverity::Fatal)
                .unwrap_or(&0)
    }

    /// Get total warning count
    pub fn warning_count(&self) -> u64 {
        *self
            .summary
            .error_counts
            .get(&ErrorSeverity::Warning)
            .unwrap_or(&0)
    }

    /// Generate a detailed error report for display
    pub fn generate_report(&self) -> String {
        let mut report = String::new();

        report.push_str("=== Error Summary ===\n");
        report.push_str(&format!(
            "Total records processed: {}\n",
            self.summary.total_records
        ));
        report.push_str(&format!(
            "Records with errors: {}\n",
            self.summary.records_with_errors
        ));

        if !self.summary.error_counts.is_empty() {
            report.push_str("\nError counts by severity:\n");
            for (severity, count) in &self.summary.error_counts {
                if *count > 0 {
                    report.push_str(&format!("  {:?}: {}\n", severity, count));
                }
            }
        }

        if !self.summary.error_codes.is_empty() {
            report.push_str("\nError counts by code:\n");
            for (code, count) in &self.summary.error_codes {
                if *count > 0 {
                    report.push_str(&format!("  {}: {}\n", code, count));
                }
            }
        }

        if self.summary.corruption_warnings > 0 {
            report.push_str(&format!(
                "\nTransfer corruption warnings: {}\n",
                self.summary.corruption_warnings
            ));
        }

        if let Some(ref first_error) = self.summary.first_error {
            report.push_str(&format!("\nFirst error: {}\n", first_error.error));
        }

        report
    }

    /// Determine error severity based on error code
    fn determine_severity(&self, error: &Error) -> ErrorSeverity {
        match error.code {
            // Parse errors are typically fatal
            ErrorCode::CBKP001_SYNTAX
            | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
            | ErrorCode::CBKP021_ODO_NOT_TAIL
            | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC => ErrorSeverity::Fatal,

            // Schema errors can be fatal or errors depending on context
            ErrorCode::CBKS121_COUNTER_NOT_FOUND | ErrorCode::CBKS141_RECORD_TOO_LARGE => {
                ErrorSeverity::Fatal
            }

            // ODO clipping is a warning in lenient mode, error in strict mode
            ErrorCode::CBKS301_ODO_CLIPPED | ErrorCode::CBKS302_ODO_RAISED => {
                if self.mode == ErrorMode::Strict {
                    ErrorSeverity::Fatal
                } else {
                    ErrorSeverity::Warning
                }
            }

            // Record format warnings
            ErrorCode::CBKR211_RDW_RESERVED_NONZERO => {
                if self.mode == ErrorMode::Strict {
                    ErrorSeverity::Fatal
                } else {
                    ErrorSeverity::Warning
                }
            }
            ErrorCode::CBKR221_RDW_UNDERFLOW => ErrorSeverity::Error,

            // Character conversion warnings
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE => ErrorSeverity::Warning,

            // Data decode errors
            ErrorCode::CBKD101_INVALID_FIELD_TYPE
            | ErrorCode::CBKD301_RECORD_TOO_SHORT
            | ErrorCode::CBKD401_COMP3_INVALID_NIBBLE
            | ErrorCode::CBKD411_ZONED_BAD_SIGN => ErrorSeverity::Error,

            // BLANK WHEN ZERO is informational
            ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO => ErrorSeverity::Warning,

            // Encode errors
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH | ErrorCode::CBKE521_ARRAY_LEN_OOB => {
                ErrorSeverity::Error
            }

            // Transfer corruption warnings
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII => ErrorSeverity::Warning,

            // JSON write errors are typically fatal
            ErrorCode::CBKC201_JSON_WRITE_ERROR => ErrorSeverity::Fatal,
        }
    }

    /// Update error statistics
    fn update_statistics(&mut self, report: &ErrorReport) {
        // Update severity counts
        *self
            .summary
            .error_counts
            .entry(report.severity)
            .or_insert(0) += 1;

        // Update error code counts
        *self
            .summary
            .error_codes
            .entry(report.error.code)
            .or_insert(0) += 1;

        // Track records with errors
        if matches!(report.severity, ErrorSeverity::Error | ErrorSeverity::Fatal)
            && let Some(ref context) = report.error.context
            && let Some(record_index) = context.record_index
        {
            // Only count each record once
            if self.summary.records_with_errors < record_index {
                self.summary.records_with_errors = record_index;
            }
        }

        // Track first and last errors
        if self.summary.first_error.is_none() {
            self.summary.first_error = Some(report.clone());
        }
        self.summary.last_error = Some(report.clone());
    }

    /// Log error with appropriate level and detail
    fn log_error(&self, report: &ErrorReport) {
        let error_msg = if self.verbose_logging {
            format!("{}", report.error)
        } else {
            format!("{}: {}", report.error.code, report.error.message)
        };

        match report.severity {
            ErrorSeverity::Fatal => error!("{}", error_msg),
            ErrorSeverity::Error => error!("{}", error_msg),
            ErrorSeverity::Warning => warn!("{}", error_msg),
            ErrorSeverity::Info => debug!("{}", error_msg),
        }

        // Log additional context if available and verbose
        if self.verbose_logging
            && let Some(ref context) = report.error.context
            && (context.record_index.is_some() || context.field_path.is_some() || context.byte_offset.is_some())
        {
            debug!("  Context: {}", context);
        }
    }

    /// Check if error indicates transfer corruption
    fn is_corruption_warning(&self, error: &Error) -> bool {
        matches!(
            error.code,
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII | ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
        )
    }
}

impl fmt::Display for ErrorSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorSeverity::Info => write!(f, "INFO"),
            ErrorSeverity::Warning => write!(f, "WARN"),
            ErrorSeverity::Error => write!(f, "ERROR"),
            ErrorSeverity::Fatal => write!(f, "FATAL"),
        }
    }
}

impl ErrorSummary {
    /// Check if processing had any errors
    pub fn has_errors(&self) -> bool {
        self.error_counts.get(&ErrorSeverity::Error).unwrap_or(&0) > &0
            || self.error_counts.get(&ErrorSeverity::Fatal).unwrap_or(&0) > &0
    }

    /// Check if processing had any warnings
    pub fn has_warnings(&self) -> bool {
        self.error_counts.get(&ErrorSeverity::Warning).unwrap_or(&0) > &0
    }

    /// Get total error count (excluding warnings)
    pub fn error_count(&self) -> u64 {
        self.error_counts.get(&ErrorSeverity::Error).unwrap_or(&0)
            + self.error_counts.get(&ErrorSeverity::Fatal).unwrap_or(&0)
    }

    /// Get total warning count
    pub fn warning_count(&self) -> u64 {
        *self.error_counts.get(&ErrorSeverity::Warning).unwrap_or(&0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::ErrorCode;

    #[test]
    fn test_error_reporter_strict_mode() {
        let mut reporter = ErrorReporter::new(ErrorMode::Strict, None);

        let error = Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "Invalid nibble")
            .with_record(1)
            .with_field("CUSTOMER.ID")
            .with_offset(42);

        // In strict mode, errors should stop processing
        let result = reporter.report_error(error);
        assert!(result.is_err());
        assert!(reporter.has_errors());
        assert_eq!(reporter.error_count(), 1);
    }

    #[test]
    fn test_error_reporter_lenient_mode() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        let error = Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "Invalid nibble")
            .with_record(1)
            .with_field("CUSTOMER.ID")
            .with_offset(42);

        // In lenient mode, errors should allow continuation
        let result = reporter.report_error(error);
        assert!(result.is_ok());
        assert!(reporter.has_errors());
        assert_eq!(reporter.error_count(), 1);
    }

    #[test]
    fn test_max_errors_limit() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, Some(1));

        // First error should be OK
        let error1 = Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "Error 1");
        assert!(reporter.report_error(error1).is_ok());

        // Second error should stop processing (we've reached the limit)
        let error2 = Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "Error 2");
        assert!(reporter.report_error(error2).is_err());
    }

    #[test]
    fn test_warning_reporting() {
        let mut reporter = ErrorReporter::new(ErrorMode::Strict, None);

        let warning = Error::new(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "Blank field");
        reporter.report_warning(warning);

        assert!(!reporter.has_errors());
        assert!(reporter.has_warnings());
        assert_eq!(reporter.warning_count(), 1);
    }

    #[test]
    fn test_corruption_detection() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        let corruption_error = Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            "ASCII corruption detected",
        );
        reporter.report_warning(corruption_error);

        assert_eq!(reporter.summary().corruption_warnings, 1);
    }

    #[test]
    fn test_error_summary_generation() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        reporter.start_record(1);
        let error =
            Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "Test error").with_record(1);
        let _ = reporter.report_error(error);

        let report = reporter.generate_report();
        assert!(report.contains("Total records processed: 1"));
        assert!(report.contains("CBKD401_COMP3_INVALID_NIBBLE: 1"));
    }
}
