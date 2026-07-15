// SPDX-License-Identifier: AGPL-3.0-or-later
//! Processing run statistics and display formatting.

use std::fmt;

/// Summary of a processing run with comprehensive statistics.
///
/// Captures record counts, error rates, throughput, and resource usage
/// for a complete decode or encode operation.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct RunSummary {
    /// Total number of records decoded or encoded successfully.
    pub records_processed: u64,
    /// Number of records that encountered errors during processing.
    pub records_with_errors: u64,
    /// Number of non-fatal warnings generated during processing.
    pub warnings: u64,
    /// Wall-clock processing time in milliseconds.
    pub processing_time_ms: u64,
    /// Total bytes read from input.
    ///
    /// Fixed records count payload bytes; RDW records count header-plus-payload
    /// physical bytes.
    pub bytes_processed: u64,
    /// SHA-256 fingerprint of the schema used for processing.
    pub schema_fingerprint: String,
    /// Processing throughput in MiB/s.
    pub throughput_mbps: f64,
    /// Peak memory usage in bytes, if available from the runtime.
    pub peak_memory_bytes: Option<u64>,
    /// Number of worker threads used for parallel processing.
    pub threads_used: usize,
}

impl RunSummary {
    /// Create a new run summary with default values
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new run summary with specified thread count
    #[must_use]
    pub fn with_threads(threads: usize) -> Self {
        Self {
            threads_used: threads,
            ..Self::default()
        }
    }

    /// Calculate throughput based on bytes and time
    #[allow(clippy::cast_precision_loss)]
    pub fn calculate_throughput(&mut self) {
        if self.processing_time_ms > 0 {
            let seconds = self.processing_time_ms as f64 / 1000.0;
            let megabytes = self.bytes_processed as f64 / (1024.0 * 1024.0);
            self.throughput_mbps = megabytes / seconds;
        }
    }

    /// Check if processing had any errors
    #[must_use]
    pub const fn has_errors(&self) -> bool {
        self.records_with_errors > 0
    }

    /// Check if processing had any warnings
    #[must_use]
    pub const fn has_warnings(&self) -> bool {
        self.warnings > 0
    }

    /// Check if processing was successful (no errors)
    #[must_use]
    pub const fn is_successful(&self) -> bool {
        !self.has_errors()
    }

    /// Get the total number of records attempted (processed + errors)
    #[must_use]
    pub const fn total_records(&self) -> u64 {
        self.records_processed + self.records_with_errors
    }

    /// Get the success rate as a percentage (0.0 to 100.0)
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
    pub fn success_rate(&self) -> f64 {
        let total = self.total_records();
        if total == 0 {
            100.0
        } else {
            (self.records_processed as f64 / total as f64) * 100.0
        }
    }

    /// Get the error rate as a percentage (0.0 to 100.0)
    #[must_use]
    pub fn error_rate(&self) -> f64 {
        100.0 - self.success_rate()
    }

    /// Get processing time in seconds
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
    pub fn processing_time_seconds(&self) -> f64 {
        self.processing_time_ms as f64 / 1000.0
    }

    /// Get bytes processed in megabytes
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
    pub fn bytes_processed_mb(&self) -> f64 {
        self.bytes_processed as f64 / (1024.0 * 1024.0)
    }

    /// Set the schema fingerprint
    pub fn set_schema_fingerprint(&mut self, fingerprint: String) {
        self.schema_fingerprint = fingerprint;
    }

    /// Set the peak memory usage
    pub fn set_peak_memory_bytes(&mut self, bytes: u64) {
        self.peak_memory_bytes = Some(bytes);
    }
}

impl fmt::Display for RunSummary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Processing Summary:")?;
        writeln!(f, "  Records processed: {}", self.records_processed)?;
        writeln!(f, "  Records with errors: {}", self.records_with_errors)?;
        writeln!(f, "  Warnings: {}", self.warnings)?;
        writeln!(f, "  Success rate: {:.1}%", self.success_rate())?;
        writeln!(
            f,
            "  Processing time: {:.2}s",
            self.processing_time_seconds()
        )?;
        writeln!(f, "  Bytes processed: {:.2} MB", self.bytes_processed_mb())?;
        writeln!(f, "  Throughput: {:.2} MB/s", self.throughput_mbps)?;
        writeln!(f, "  Threads used: {}", self.threads_used)?;
        if let Some(peak_memory) = self.peak_memory_bytes {
            #[allow(clippy::cast_precision_loss)]
            let peak_mb = peak_memory as f64 / (1024.0 * 1024.0);
            writeln!(f, "  Peak memory: {peak_mb:.2} MB")?;
        }
        if !self.schema_fingerprint.is_empty() {
            writeln!(f, "  Schema fingerprint: {}", self.schema_fingerprint)?;
        }
        Ok(())
    }
}
