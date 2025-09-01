//! Encoding and decoding codecs for COBOL data types
//!
//! This crate provides the actual encoding/decoding logic for all COBOL data types,
//! character set conversion, and record framing (fixed/RDW).

pub mod charset;
pub mod corruption;
pub mod json;
pub mod numeric;
pub mod options;
pub mod processor;
pub mod record;
pub mod roundtrip;

pub use options::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
pub use numeric::{
    SmallDecimal, decode_zoned_decimal, decode_packed_decimal, decode_binary_int,
    encode_zoned_decimal, encode_packed_decimal, encode_binary_int, encode_alphanumeric,
    encode_zoned_decimal_with_bwz, get_binary_width_from_digits, validate_explicit_binary_width,
    should_encode_as_blank_when_zero,
};
pub use charset::{ebcdic_to_utf8, utf8_to_ebcdic, get_zoned_sign_table};
pub use record::{
    read_record, write_record, FixedRecordReader, FixedRecordWriter, 
    RDWRecordReader, RDWRecordWriter, RDWRecord
};
pub use json::{JsonWriter, JsonEncoder, OrderedJsonWriter};
pub use roundtrip::{RoundTripConfig, RoundTripResult, RoundTripTestSuite, create_comprehensive_test_suite};
pub use corruption::{detect_rdw_ascii_corruption, detect_ebcdic_corruption, detect_packed_corruption};
pub use processor::{DecodeProcessor, EncodeProcessor};

use copybook_core::{Result, Schema, Error, ErrorCode};
use serde_json::Value;
use std::io::{Read, Write};

/// Decode binary data to JSON using the provided schema
/// 
/// # Errors
/// 
/// Returns an error if the data cannot be decoded according to the schema
pub fn decode_record(schema: &Schema, data: &[u8], options: &DecodeOptions) -> Result<Value> {
    use crate::json::JsonWriter;
    use std::io::Cursor;
    
    // Create a JSON writer with a buffer
    let mut buffer = Vec::new();
    let mut writer = JsonWriter::new(Cursor::new(&mut buffer), options.clone());
    
    // Write the record
    writer.write_record(schema, data, 0, 0)?;
    
    // Parse the JSON line back to Value
    let json_str = String::from_utf8(buffer)
        .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, format!("UTF-8 error: {}", e)))?;
    
    // Parse the first line (remove trailing newline)
    let json_line = json_str.trim_end();
    serde_json::from_str(json_line)
        .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, format!("JSON parse error: {}", e)))
}

/// Encode JSON data to binary using the provided schema
/// 
/// # Errors
/// 
/// Returns an error if the JSON data cannot be encoded according to the schema
pub fn encode_record(schema: &Schema, json: &Value, options: &EncodeOptions) -> Result<Vec<u8>> {
    let encoder = JsonEncoder::new(options.clone());
    encoder.encode_record(schema, json)
}

/// Decode a file to JSONL format
/// 
/// # Errors
/// 
/// Returns an error if the file cannot be decoded or written
pub fn decode_file_to_jsonl(
    schema: &Schema,
    input: impl Read,
    output: impl Write,
    options: &DecodeOptions,
) -> Result<RunSummary> {
    let mut processor = processor::DecodeProcessor::new(options.clone());
    processor.process_file(schema, input, output)
}

/// Encode JSONL to binary file
/// 
/// # Errors
/// 
/// Returns an error if the JSONL cannot be encoded or written
pub fn encode_jsonl_to_file(
    schema: &Schema,
    input: impl Read,
    output: impl Write,
    options: &EncodeOptions,
) -> Result<RunSummary> {
    let mut processor = processor::EncodeProcessor::new(options.clone());
    processor.process_file(schema, input, output)
}

/// Summary of processing run
#[derive(Debug, Default, Clone)]
pub struct RunSummary {
    /// Total records processed
    pub records_processed: u64,
    /// Number of records with errors
    pub records_with_errors: u64,
    /// Number of warnings generated
    pub warnings: u64,
    /// Processing time in milliseconds
    pub processing_time_ms: u64,
    /// Bytes processed
    pub bytes_processed: u64,
    /// Schema fingerprint used
    pub schema_fingerprint: String,
    /// Input file hash (for provenance)
    pub input_file_hash: Option<String>,
    /// Processing throughput (MB/s)
    pub throughput_mbps: f64,
    /// Detailed error summary
    pub error_summary: Option<copybook_core::ErrorSummary>,
    /// Transfer corruption warnings detected
    pub corruption_warnings: u64,
}

impl RunSummary {
    /// Create a new run summary
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Calculate throughput based on bytes and time
    pub fn calculate_throughput(&mut self) {
        if self.processing_time_ms > 0 {
            #[allow(clippy::cast_precision_loss)]
            let seconds = self.processing_time_ms as f64 / 1000.0;
            #[allow(clippy::cast_precision_loss)]
            let megabytes = self.bytes_processed as f64 / (1024.0 * 1024.0);
            self.throughput_mbps = megabytes / seconds;
        }
    }

    /// Check if processing had any errors
    pub fn has_errors(&self) -> bool {
        self.records_with_errors > 0
    }

    /// Check if processing had any warnings
    pub fn has_warnings(&self) -> bool {
        self.warnings > 0 || self.corruption_warnings > 0
    }

    /// Update summary from error reporter
    pub fn update_from_error_summary(&mut self, error_summary: &copybook_core::ErrorSummary) {
        self.records_with_errors = error_summary.records_with_errors;
        self.warnings = error_summary.warning_count();
        self.corruption_warnings = error_summary.corruption_warnings;
        self.error_summary = Some(error_summary.clone());
    }

    /// Generate detailed error report
    pub fn generate_error_report(&self) -> Option<String> {
        self.error_summary.as_ref().map(|summary| {
            let mut report = String::new();
            
            report.push_str("=== Processing Summary ===\n");
            report.push_str(&format!("Total records processed: {}\n", self.records_processed));
            report.push_str(&format!("Records with errors: {}\n", self.records_with_errors));
            report.push_str(&format!("Warnings: {}\n", self.warnings));
            report.push_str(&format!("Processing time: {}ms\n", self.processing_time_ms));
            report.push_str(&format!("Bytes processed: {}\n", self.bytes_processed));
            report.push_str(&format!("Throughput: {:.2} MB/s\n", self.throughput_mbps));
            
            if self.corruption_warnings > 0 {
                report.push_str(&format!("Transfer corruption warnings: {}\n", self.corruption_warnings));
            }
            
            if !summary.error_codes.is_empty() {
                report.push_str("\nError breakdown by code:\n");
                for (code, count) in &summary.error_codes {
                    if *count > 0 {
                        report.push_str(&format!("  {}: {}\n", code, count));
                    }
                }
            }
            
            report
        })
    }
}
