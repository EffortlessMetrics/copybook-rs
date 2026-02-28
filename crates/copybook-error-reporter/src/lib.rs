// SPDX-License-Identifier: AGPL-3.0-or-later
//! Structured error reporting and handling system
//!
//! This module provides comprehensive error reporting with configurable handling modes,
//! detailed logging, and summary statistics for copybook processing operations.

use copybook_error::{Error, ErrorCode};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write as _;
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
    #[must_use]
    #[inline]
    pub fn new(mode: ErrorMode, max_errors: Option<u64>) -> Self {
        Self {
            mode,
            max_errors,
            summary: ErrorSummary::default(),
            verbose_logging: true,
        }
    }

    /// Set verbose logging mode
    #[must_use]
    #[inline]
    pub fn with_verbose_logging(mut self, verbose: bool) -> Self {
        self.verbose_logging = verbose;
        self
    }

    /// Report an error and determine if processing should continue.
    ///
    /// Returns `Ok(())` when work may proceed and `Err(error)` when it must stop.
    ///
    /// # Panics
    /// Panics if `max_errors` is configured but becomes `None` during processing.
    ///
    /// # Errors
    /// Returns an error when severity or error limits require halting processing.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
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
                format!(
                    "Maximum error limit reached: {}",
                    self.max_errors.unwrap_or(0)
                ),
            ))
        } else {
            Err(error)
        }
    }

    /// Report a warning (always continues processing)
    #[inline]
    pub fn report_warning(&mut self, error: Error) {
        let mut report = ErrorReport {
            error,
            severity: ErrorSeverity::Warning,
            timestamp: std::time::SystemTime::now(),
            metadata: HashMap::new(),
        };

        // Check for transfer corruption patterns
        if is_corruption_warning(&report.error) {
            self.summary.corruption_warnings += 1;
            report
                .metadata
                .insert("corruption_type".to_string(), "transfer".to_string());
        }

        self.update_statistics(&report);
        self.log_error(&report);
    }

    /// Report record processing start (for context tracking)
    #[inline]
    pub fn start_record(&mut self, record_index: u64) {
        self.summary.total_records = record_index;
        debug!("Processing record {record_index}");
    }

    /// Get the current error summary
    #[must_use]
    #[inline]
    pub fn summary(&self) -> &ErrorSummary {
        &self.summary
    }

    /// Check if any errors have been reported
    #[must_use]
    #[inline]
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
    #[must_use]
    #[inline]
    pub fn has_warnings(&self) -> bool {
        self.summary
            .error_counts
            .get(&ErrorSeverity::Warning)
            .unwrap_or(&0)
            > &0
    }

    /// Get total error count (excluding warnings)
    #[must_use]
    #[inline]
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
    #[must_use]
    #[inline]
    pub fn warning_count(&self) -> u64 {
        *self
            .summary
            .error_counts
            .get(&ErrorSeverity::Warning)
            .unwrap_or(&0)
    }

    /// Generate a detailed error report for display
    #[must_use]
    #[inline]
    pub fn generate_report(&self) -> String {
        let mut report = String::new();

        report.push_str("=== Error Summary ===\n");
        let _ = writeln!(
            report,
            "Total records processed: {}",
            self.summary.total_records
        );
        let _ = writeln!(
            report,
            "Records with errors: {}",
            self.summary.records_with_errors
        );

        if !self.summary.error_counts.is_empty() {
            report.push_str("\nError counts by severity:\n");
            for (severity, count) in &self.summary.error_counts {
                if *count > 0 {
                    let _ = writeln!(report, "  {severity:?}: {count}");
                }
            }
        }

        if !self.summary.error_codes.is_empty() {
            report.push_str("\nError counts by code:\n");
            for (code, count) in &self.summary.error_codes {
                if *count > 0 {
                    let _ = writeln!(report, "  {code}: {count}");
                }
            }
        }

        if self.summary.corruption_warnings > 0 {
            report.push('\n');
            let _ = writeln!(
                report,
                "Transfer corruption warnings: {}",
                self.summary.corruption_warnings
            );
        }

        if let Some(ref first_error) = self.summary.first_error {
            report.push('\n');
            let _ = writeln!(report, "First error: {}", first_error.error);
        }

        report
    }

    /// Determine error severity based on error code
    #[inline]
    fn determine_severity(&self, error: &Error) -> ErrorSeverity {
        match error.code {
            // Parse errors are typically fatal
            ErrorCode::CBKP001_SYNTAX
            | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
            | ErrorCode::CBKP021_ODO_NOT_TAIL
            | ErrorCode::CBKP022_NESTED_ODO
            | ErrorCode::CBKP023_ODO_REDEFINES
            | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
            | ErrorCode::CBKP101_INVALID_PIC
            // Schema errors are fatal
            | ErrorCode::CBKS121_COUNTER_NOT_FOUND
            | ErrorCode::CBKS141_RECORD_TOO_LARGE
            | ErrorCode::CBKS601_RENAME_UNKNOWN_FROM
            | ErrorCode::CBKS602_RENAME_UNKNOWN_THRU
            | ErrorCode::CBKS603_RENAME_NOT_CONTIGUOUS
            | ErrorCode::CBKS604_RENAME_REVERSED_RANGE
            | ErrorCode::CBKS605_RENAME_FROM_CROSSES_GROUP
            | ErrorCode::CBKS606_RENAME_THRU_CROSSES_GROUP
            | ErrorCode::CBKS607_RENAME_CROSSES_OCCURS
            | ErrorCode::CBKS608_RENAME_QUALIFIED_NAME_NOT_FOUND
            | ErrorCode::CBKS609_RENAME_OVER_REDEFINES
            | ErrorCode::CBKS610_RENAME_MULTIPLE_REDEFINES
            | ErrorCode::CBKS611_RENAME_PARTIAL_OCCURS
            | ErrorCode::CBKS612_RENAME_ODO_NOT_SUPPORTED
            | ErrorCode::CBKS701_PROJECTION_INVALID_ODO
            | ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS
            | ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND
            | ErrorCode::CBKE505_SCALE_MISMATCH
            | ErrorCode::CBKE510_NUMERIC_OVERFLOW
            | ErrorCode::CBKE515_STRING_LENGTH_VIOLATION
            // JSON write errors are typically fatal
            | ErrorCode::CBKC201_JSON_WRITE_ERROR
            // Iterator/internal state errors are fatal
            | ErrorCode::CBKI001_INVALID_STATE => ErrorSeverity::Fatal,

            // ODO clipping is a warning in lenient mode, error in strict mode
            ErrorCode::CBKS301_ODO_CLIPPED
            | ErrorCode::CBKS302_ODO_RAISED
            // Record format warnings
            | ErrorCode::CBKR211_RDW_RESERVED_NONZERO => {
                if self.mode == ErrorMode::Strict {
                    ErrorSeverity::Fatal
                } else {
                    ErrorSeverity::Warning
                }
            }

            // Character conversion warnings, BLANK WHEN ZERO, transfer corruption
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
            | ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO
            | ErrorCode::CBKD423_EDITED_PIC_BLANK_WHEN_ZERO
            | ErrorCode::CBKF104_RDW_SUSPECT_ASCII => ErrorSeverity::Warning,

            // Data decode errors, encode errors, format errors, audit, arrow/writer
            ErrorCode::CBKD101_INVALID_FIELD_TYPE
            | ErrorCode::CBKD301_RECORD_TOO_SHORT
            | ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED
            | ErrorCode::CBKD401_COMP3_INVALID_NIBBLE
            | ErrorCode::CBKD410_ZONED_OVERFLOW
            | ErrorCode::CBKD411_ZONED_BAD_SIGN
            | ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
            | ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH
            | ErrorCode::CBKD431_FLOAT_NAN
            | ErrorCode::CBKD432_FLOAT_INFINITY
            | ErrorCode::CBKD413_ZONED_INVALID_ENCODING
            | ErrorCode::CBKD414_ZONED_MIXED_ENCODING
            | ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS
            | ErrorCode::CBKE501_JSON_TYPE_MISMATCH
            | ErrorCode::CBKE521_ARRAY_LEN_OOB
            | ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR
            | ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW
            | ErrorCode::CBKF102_RECORD_LENGTH_INVALID
            | ErrorCode::CBKF221_RDW_UNDERFLOW
            | ErrorCode::CBKA001_BASELINE_ERROR
            | ErrorCode::CBKW001_SCHEMA_CONVERSION
            | ErrorCode::CBKW002_TYPE_MAPPING
            | ErrorCode::CBKW003_DECIMAL_OVERFLOW
            | ErrorCode::CBKW004_BATCH_BUILD
            | ErrorCode::CBKW005_PARQUET_WRITE => ErrorSeverity::Error,
        }
    }

    /// Update error statistics
    #[inline]
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
    #[inline]
    fn log_error(&self, report: &ErrorReport) {
        let error_msg = if self.verbose_logging {
            format!("{}", report.error)
        } else {
            format!("{}: {}", report.error.code, report.error.message)
        };

        match report.severity {
            ErrorSeverity::Fatal | ErrorSeverity::Error => error!("{error_msg}"),
            ErrorSeverity::Warning => warn!("{error_msg}"),
            ErrorSeverity::Info => debug!("{error_msg}"),
        }

        // Log additional context if available and verbose
        if self.verbose_logging
            && let Some(ref context) = report.error.context
            && (context.record_index.is_some()
                || context.field_path.is_some()
                || context.byte_offset.is_some())
        {
            debug!("  Context: {context}");
        }
    }
}

/// Check if error indicates transfer corruption
#[inline]
fn is_corruption_warning(error: &Error) -> bool {
    matches!(
        error.code,
        ErrorCode::CBKF104_RDW_SUSPECT_ASCII | ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
    )
}

impl fmt::Display for ErrorSeverity {
    #[inline]
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
    #[must_use]
    #[inline]
    pub fn has_errors(&self) -> bool {
        self.error_counts.get(&ErrorSeverity::Error).unwrap_or(&0) > &0
            || self.error_counts.get(&ErrorSeverity::Fatal).unwrap_or(&0) > &0
    }

    /// Check if processing had any warnings
    #[must_use]
    #[inline]
    pub fn has_warnings(&self) -> bool {
        self.error_counts.get(&ErrorSeverity::Warning).unwrap_or(&0) > &0
    }

    /// Get total error count (excluding warnings)
    #[must_use]
    #[inline]
    pub fn error_count(&self) -> u64 {
        self.error_counts.get(&ErrorSeverity::Error).unwrap_or(&0)
            + self.error_counts.get(&ErrorSeverity::Fatal).unwrap_or(&0)
    }

    /// Get total warning count
    #[must_use]
    #[inline]
    pub fn warning_count(&self) -> u64 {
        *self.error_counts.get(&ErrorSeverity::Warning).unwrap_or(&0)
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use copybook_error::ErrorCode;

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

    // --- New tests below ---

    #[test]
    fn test_empty_reporter_no_errors_no_warnings() {
        let reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        assert!(!reporter.has_errors());
        assert!(!reporter.has_warnings());
        assert_eq!(reporter.error_count(), 0);
        assert_eq!(reporter.warning_count(), 0);

        let summary = reporter.summary();
        assert!(!summary.has_errors());
        assert!(!summary.has_warnings());
        assert_eq!(summary.error_count(), 0);
        assert_eq!(summary.warning_count(), 0);
        assert!(summary.first_error.is_none());
        assert!(summary.last_error.is_none());
        assert_eq!(summary.corruption_warnings, 0);
        assert_eq!(summary.total_records, 0);
        assert_eq!(summary.records_with_errors, 0);
    }

    #[test]
    fn test_empty_reporter_report_output() {
        let reporter = ErrorReporter::new(ErrorMode::Strict, None);
        let report = reporter.generate_report();

        assert!(report.contains("=== Error Summary ==="));
        assert!(report.contains("Total records processed: 0"));
        assert!(report.contains("Records with errors: 0"));
        // No error counts section when empty
        assert!(!report.contains("Error counts by severity:"));
        assert!(!report.contains("Error counts by code:"));
    }

    #[test]
    fn test_fatal_error_stops_in_lenient_mode() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        // Fatal errors stop processing regardless of mode
        let error = Error::new(ErrorCode::CBKP001_SYNTAX, "Unexpected token");
        let result = reporter.report_error(error);
        assert!(result.is_err());
        assert!(reporter.has_errors());
    }

    #[test]
    fn test_multiple_errors_summary_counts() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        let errors = [
            Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "nibble 1"),
            Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "nibble 2"),
            Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "bad sign"),
        ];
        for e in errors {
            let _ = reporter.report_error(e);
        }

        assert_eq!(reporter.error_count(), 3);
        let summary = reporter.summary();
        assert_eq!(
            *summary
                .error_codes
                .get(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
                .unwrap(),
            2
        );
        assert_eq!(
            *summary
                .error_codes
                .get(&ErrorCode::CBKD411_ZONED_BAD_SIGN)
                .unwrap(),
            1
        );
    }

    #[test]
    fn test_first_and_last_error_tracked() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        let _ = reporter.report_error(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            "first error",
        ));
        let _ = reporter.report_error(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "second error",
        ));
        let _ = reporter.report_error(Error::new(ErrorCode::CBKD410_ZONED_OVERFLOW, "third error"));

        let summary = reporter.summary();
        let first = summary.first_error.as_ref().unwrap();
        let last = summary.last_error.as_ref().unwrap();
        assert_eq!(first.error.code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE);
        assert_eq!(first.error.message, "first error");
        assert_eq!(last.error.code, ErrorCode::CBKD410_ZONED_OVERFLOW);
        assert_eq!(last.error.message, "third error");
    }

    #[test]
    fn test_max_errors_boundary_exact_limit() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, Some(3));

        // Errors 1-3 should be OK (under limit)
        for i in 1..=3 {
            let e = Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, format!("err {i}"));
            assert!(reporter.report_error(e).is_ok(), "error {i} should succeed");
        }

        // Error 4 should fail (limit reached)
        let e4 = Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, "err 4");
        assert!(reporter.report_error(e4).is_err());
        assert_eq!(reporter.error_count(), 4);
    }

    #[test]
    fn test_max_errors_unlimited_in_lenient() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        for i in 0..100 {
            let e = Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("error {i}"),
            );
            assert!(reporter.report_error(e).is_ok());
        }
        assert_eq!(reporter.error_count(), 100);
    }

    #[test]
    fn test_severity_display_formatting() {
        assert_eq!(format!("{}", ErrorSeverity::Info), "INFO");
        assert_eq!(format!("{}", ErrorSeverity::Warning), "WARN");
        assert_eq!(format!("{}", ErrorSeverity::Error), "ERROR");
        assert_eq!(format!("{}", ErrorSeverity::Fatal), "FATAL");
    }

    #[test]
    fn test_severity_ordering() {
        assert!(ErrorSeverity::Info < ErrorSeverity::Warning);
        assert!(ErrorSeverity::Warning < ErrorSeverity::Error);
        assert!(ErrorSeverity::Error < ErrorSeverity::Fatal);
    }

    #[test]
    fn test_odo_clipped_severity_depends_on_mode() {
        // In lenient mode: warning (continues)
        let mut lenient = ErrorReporter::new(ErrorMode::Lenient, None);
        let e = Error::new(ErrorCode::CBKS301_ODO_CLIPPED, "ODO clipped");
        assert!(lenient.report_error(e).is_ok());
        assert!(!lenient.has_errors());
        assert!(lenient.has_warnings());

        // In strict mode: fatal (stops)
        let mut strict = ErrorReporter::new(ErrorMode::Strict, None);
        let e = Error::new(ErrorCode::CBKS301_ODO_CLIPPED, "ODO clipped");
        assert!(strict.report_error(e).is_err());
    }

    #[test]
    fn test_mixed_errors_and_warnings_report() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        reporter.report_warning(Error::new(ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO, "blank"));
        let _ = reporter.report_error(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            "bad nibble",
        ));
        reporter.report_warning(Error::new(
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
            "bad byte",
        ));

        let report = reporter.generate_report();
        assert!(report.contains("Error counts by severity:"));
        assert!(report.contains("Error counts by code:"));
        assert_eq!(reporter.error_count(), 1);
        assert_eq!(reporter.warning_count(), 2);
    }

    #[test]
    fn test_corruption_detection_ebcdic_byte() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        reporter.report_warning(Error::new(
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
            "Invalid EBCDIC byte 0xFF",
        ));

        assert_eq!(reporter.summary().corruption_warnings, 1);
        assert!(reporter.has_warnings());
        assert!(!reporter.has_errors());
    }

    #[test]
    fn test_multiple_corruption_warnings_counted() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        for _ in 0..5 {
            reporter.report_warning(Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                "ASCII suspected",
            ));
        }
        reporter.report_warning(Error::new(
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
            "Bad EBCDIC",
        ));

        assert_eq!(reporter.summary().corruption_warnings, 6);
        assert_eq!(reporter.warning_count(), 6);
    }

    #[test]
    fn test_verbose_logging_toggle() {
        let reporter = ErrorReporter::new(ErrorMode::Lenient, None).with_verbose_logging(false);
        // Verify the builder pattern works and reporter is usable
        assert!(!reporter.has_errors());
        assert_eq!(reporter.error_count(), 0);
    }

    #[test]
    fn test_very_long_error_message() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);
        let long_msg = "x".repeat(10_000);
        let e = Error::new(ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, long_msg.clone());
        let _ = reporter.report_error(e);

        let first = reporter.summary().first_error.as_ref().unwrap();
        assert_eq!(first.error.message.len(), 10_000);

        let report = reporter.generate_report();
        assert!(report.contains("First error:"));
    }

    #[test]
    fn test_start_record_tracks_total_records() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        reporter.start_record(0);
        assert_eq!(reporter.summary().total_records, 0);

        reporter.start_record(42);
        assert_eq!(reporter.summary().total_records, 42);

        reporter.start_record(100);
        assert_eq!(reporter.summary().total_records, 100);

        let report = reporter.generate_report();
        assert!(report.contains("Total records processed: 100"));
    }

    #[test]
    fn test_error_report_contains_first_error_in_output() {
        let mut reporter = ErrorReporter::new(ErrorMode::Lenient, None);

        let _ = reporter.report_error(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            "the very first problem",
        ));
        let _ = reporter.report_error(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "second problem",
        ));

        let report = reporter.generate_report();
        assert!(report.contains("First error:"));
        assert!(report.contains("the very first problem"));
        // Last error is not shown in generate_report output
    }

    #[test]
    fn test_error_summary_default_is_clean() {
        let summary = ErrorSummary::default();
        assert!(!summary.has_errors());
        assert!(!summary.has_warnings());
        assert_eq!(summary.error_count(), 0);
        assert_eq!(summary.warning_count(), 0);
        assert!(summary.first_error.is_none());
        assert!(summary.last_error.is_none());
        assert_eq!(summary.records_with_errors, 0);
        assert_eq!(summary.total_records, 0);
        assert_eq!(summary.corruption_warnings, 0);
    }
}
