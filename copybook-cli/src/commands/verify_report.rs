//! Versioned verification report structures with stable contract

use serde::Serialize;

/// Versioned verification report for stable API contract
#[derive(Serialize)]
pub struct VerifyReport {
    /// Report format version (bump on breaking changes)
    pub report_version: u32,
    /// Schema fingerprint for identity tracking
    pub schema_fingerprint: String,
    /// Record format used for processing
    pub record_format: String,
    /// Input file path for pipeline integration
    pub file: String,
    /// Input file size in bytes
    pub file_size_bytes: u64,
    /// CLI options used for verification (for reproducibility)
    pub cli_opts: VerifyCliEcho,
    /// Total number of records processed
    pub records_total: u64,
    /// Total number of errors encountered
    pub errors_total: u64,
    /// Whether error reporting was truncated due to max_errors limit
    pub truncated: bool,
    /// List of verification errors
    pub errors: Vec<VerifyError>,
    /// Sample of record data for debugging
    pub sample: Vec<VerifySample>,
}

/// CLI options echo for reproducibility
#[derive(Serialize)]
pub struct VerifyCliEcho {
    /// Codepage used for character conversion
    pub codepage: String,
    /// Whether strict mode was enabled
    pub strict: bool,
    /// Maximum errors before truncation
    pub max_errors: u32,
    /// Number of sample records to include
    pub sample: u32,
}

/// Verification error entry
#[derive(Serialize)]
pub struct VerifyError {
    /// Record index (0-based)
    pub index: u64,
    /// Error code (e.g., "CBKP051")
    pub code: String,
    /// Field path where error occurred (e.g., "REC.AMT")
    pub field: Option<String>,
    /// Byte offset in record/file if known
    pub offset: Option<u64>,
    /// Human-readable error message
    pub msg: String,
    /// Optional hex dump of problematic data
    pub hex: Option<String>,
}

/// Sample record for debugging
#[derive(Serialize)]
pub struct VerifySample {
    /// Record index (0-based)
    pub index: u64,
    /// Hex representation of record data
    pub hex: String,
}

impl VerifyReport {
    /// Create a new verification report with version 1
    pub fn new(
        schema_fingerprint: String,
        record_format: String,
        file: String,
        file_size_bytes: u64,
        cli_opts: VerifyCliEcho,
    ) -> Self {
        Self {
            report_version: 1,
            schema_fingerprint,
            record_format,
            file,
            file_size_bytes,
            cli_opts,
            records_total: 0,
            errors_total: 0,
            truncated: false,
            errors: Vec::new(),
            sample: Vec::new(),
        }
    }

    /// Add an error to the report
    pub fn add_error(&mut self, error: VerifyError) {
        if self.errors.len() < self.cli_opts.max_errors as usize {
            self.errors.push(error);
        } else {
            self.truncated = true;
        }
        self.errors_total += 1;
    }

    /// Add a sample record
    pub fn add_sample(&mut self, sample: VerifySample) {
        if self.sample.len() < self.cli_opts.sample as usize {
            self.sample.push(sample);
        }
    }

    /// Set the total number of records processed
    pub fn set_records_total(&mut self, total: u64) {
        self.records_total = total;
    }

    /// Get the exit code based on errors
    pub fn exit_code(&self) -> i32 {
        if self.errors_total > 0 {
            3 // Validation errors
        } else {
            0 // Success
        }
    }
}
