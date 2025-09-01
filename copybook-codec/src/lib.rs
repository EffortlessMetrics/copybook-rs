//! Encoding and decoding codecs for COBOL data types
//!
//! This crate provides the actual encoding/decoding logic for all COBOL data types,
//! character set conversion, and record framing (fixed/RDW).

pub mod charset;
pub mod json;
pub mod numeric;
pub mod options;
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
    _schema: &Schema,
    _input: impl Read,
    _output: impl Write,
    _options: &DecodeOptions,
) -> Result<RunSummary> {
    // Placeholder implementation - will be implemented in later tasks
    Ok(RunSummary::default())
}

/// Encode JSONL to binary file
/// 
/// # Errors
/// 
/// Returns an error if the JSONL cannot be encoded or written
pub fn encode_jsonl_to_file(
    _schema: &Schema,
    _input: impl Read,
    _output: impl Write,
    _options: &EncodeOptions,
) -> Result<RunSummary> {
    // Placeholder implementation - will be implemented in later tasks
    Ok(RunSummary::default())
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
        self.warnings > 0
    }
}
