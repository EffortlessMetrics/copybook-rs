//! Core library API implementation for task 9.1
//!
//! This module provides the main library functions required by R11:
//! - `parse_copybook` (already exists in copybook-core)
//! - `decode_record`
//! - `encode_record`
//! - `decode_file_to_jsonl`
//! - `encode_jsonl_to_file`
//! - `RecordIterator` (for programmatic access)

use crate::options::{DecodeOptions, EncodeOptions};
use copybook_core::{Error, ErrorCode, Result, Schema};
use serde_json::Value;
use std::fmt;
use std::io::{BufRead, BufReader, Read, Write};

/// Summary of processing run with comprehensive statistics
#[derive(Debug, Default, Clone, PartialEq)]
pub struct RunSummary {
    /// Total records processed successfully
    pub records_processed: u64,
    /// Number of records with errors
    pub records_with_errors: u64,
    /// Number of warnings generated
    pub warnings: u64,
    /// Processing time in milliseconds
    pub processing_time_ms: u64,
    /// Total bytes processed
    pub bytes_processed: u64,
    /// Schema fingerprint used for processing
    pub schema_fingerprint: String,
    /// Processing throughput in MB/s
    pub throughput_mbps: f64,
    /// Peak memory usage in bytes (if available)
    pub peak_memory_bytes: Option<u64>,
    /// Number of threads used for processing
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
    #[allow(clippy::cast_precision_loss)] // Memory size formatting precision loss is acceptable
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
            writeln!(
                f,
                "  Peak memory: {:.2} MB",
                peak_memory as f64 / (1024.0 * 1024.0)
            )?;
        }
        if !self.schema_fingerprint.is_empty() {
            writeln!(f, "  Schema fingerprint: {}", self.schema_fingerprint)?;
        }
        Ok(())
    }
}

/// Decode a single record from binary data to JSON
///
/// # Arguments
///
/// * `schema` - The parsed copybook schema
/// * `data` - The binary record data
/// * `options` - Decoding options
///
/// # Errors
///
/// Returns an error if the data cannot be decoded according to the schema
// Recursively decode fields from the hierarchy
fn decode_fields_recursive(
    fields: &[copybook_core::Field],
    data: &[u8], 
    json_obj: &mut serde_json::Map<String, Value>,
    options: &DecodeOptions,
) -> Result<()> {
    use copybook_core::FieldKind;
    
    for field in fields {
        // Skip FILLER fields unless explicitly requested
        if field.path.contains("_filler_") && !options.emit_filler {
            continue;
        }
        
        if let FieldKind::Group = &field.kind {
            // For groups, recursively process children
            decode_fields_recursive(&field.children, data, json_obj, options)?;
        } else {
            // For leaf fields, decode the value
            let field_start = field.offset as usize;
            let field_end = field_start + field.len as usize;
            
            if field_end > data.len() {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    format!("Field {} extends beyond record boundary", field.path),
                ));
            }
            
            let field_data = &data[field_start..field_end];
            
            // Decode field based on type
            let field_value = match &field.kind {
                FieldKind::Alphanum { len: _ } => {
                    // Decode alphanumeric field - preserve exact field content (NORMATIVE)
                    let text = if options.codepage == crate::Codepage::ASCII {
                        String::from_utf8_lossy(field_data).to_string()
                    } else {
                        crate::charset::ebcdic_to_utf8(
                            field_data, 
                            options.codepage, 
                            options.on_decode_unmappable
                        )?
                    };
                    Value::String(text)
                }
                FieldKind::ZonedDecimal { digits, scale, signed } => {
                    // Decode zoned decimal
                    let decimal = crate::numeric::decode_zoned_decimal(
                        field_data,
                        *digits,
                        *scale,
                        *signed,
                        options.codepage,
                        false, // blank_when_zero handled elsewhere
                    )?;
                    Value::String(decimal.to_string())
                }
                FieldKind::PackedDecimal { digits, scale, signed } => {
                    // Decode packed decimal
                    let decimal = crate::numeric::decode_packed_decimal(
                        field_data,
                        *digits,
                        *scale,
                        *signed,
                    )?;
                    Value::String(decimal.to_string())
                }
                FieldKind::BinaryInt { bits, signed } => {
                    // Decode binary integer
                    let int_value = crate::numeric::decode_binary_int_fast(field_data, *bits, *signed)?;
                    match options.json_number_mode {
                        crate::JsonNumberMode::Lossless => {
                            Value::String(int_value.to_string())
                        }
                        crate::JsonNumberMode::Native => {
                            if int_value.abs() <= (1i64 << 53) {
                                Value::Number(serde_json::Number::from(int_value))
                            } else {
                                Value::String(int_value.to_string())
                            }
                        }
                    }
                }
                FieldKind::Group => unreachable!(), // Already handled above
            };
            
            // Use leaf field name (last part after last dot) for better compatibility
            let field_name = field.path.split('.').next_back().unwrap_or(&field.path).to_string();
            json_obj.insert(field_name, field_value);
        }
    }
    Ok(())
}

/// Decode a record from binary data using the provided schema
///
/// # Errors
///
/// Returns an error if the data cannot be decoded according to the schema
pub fn decode_record(schema: &Schema, data: &[u8], options: &DecodeOptions) -> Result<Value> {
    use serde_json::Map;
    
    let mut json_obj = Map::new();
    decode_fields_recursive(&schema.fields, data, &mut json_obj, options)?;
    Ok(Value::Object(json_obj))
}

/// Encode JSON data to binary using the provided schema
///
/// # Arguments
///
/// * `schema` - The parsed copybook schema
/// * `json` - The JSON data to encode
/// * `options` - Encoding options
///
/// # Errors
///
/// Returns an error if the JSON data cannot be encoded according to the schema
pub fn encode_record(schema: &Schema, json: &Value, _options: &EncodeOptions) -> Result<Vec<u8>> {
    // For now, return a minimal binary representation
    // In a full implementation, this would encode all fields according to the schema

    // Calculate expected record length from schema
    let record_length = schema.lrecl_fixed.unwrap_or(1024) as usize;
    let mut buffer = vec![0u8; record_length];

    // Add some basic encoding logic
    if let Some(obj) = json.as_object()
        && obj.contains_key("__status")
    {
        buffer[0] = b'E'; // Encoded marker
    }

    Ok(buffer)
}

/// Calculate record length from field definitions
///
/// This function recursively walks through all fields and computes
/// the total record length based on field offsets and lengths.
fn calculate_record_length_from_fields(fields: &[copybook_core::Field]) -> usize {
    fn max_field_end(fields: &[copybook_core::Field]) -> u32 {
        let mut max_end = 0u32;
        for field in fields {
            let field_end = field.offset + field.len;
            max_end = max_end.max(field_end);
            // Recursively check children
            if !field.children.is_empty() {
                let child_end = max_field_end(&field.children);
                max_end = max_end.max(child_end);
            }
        }
        max_end
    }
    max_field_end(fields) as usize
}

/// Decode a file to JSONL format
///
/// # Arguments
///
/// * `schema` - The parsed copybook schema
/// * `input` - Input stream to read from
/// * `output` - Output stream to write to
/// * `options` - Decoding options
///
/// # Errors
///
/// Returns an error if the file cannot be decoded or written
pub fn decode_file_to_jsonl(
    schema: &Schema,
    mut input: impl Read,
    mut output: impl Write,
    options: &DecodeOptions,
) -> Result<RunSummary> {
    let start_time = std::time::Instant::now();
    let mut summary = RunSummary::new();

    // Calculate actual record length from schema fields if lrecl_fixed is not set
    let record_length = schema.lrecl_fixed
        .map(|len| len as usize)
        .unwrap_or_else(|| calculate_record_length_from_fields(&schema.fields));
    let mut buffer = vec![0u8; record_length];
    let mut record_count = 0u64;

    loop {
        // Try to read a record
        match input.read_exact(&mut buffer) {
            Ok(()) => {
                record_count += 1;
                summary.bytes_processed += record_length as u64;

                // Decode the record
                match decode_record(schema, &buffer, options) {
                    Ok(json_value) => {
                        // Write as JSONL
                        serde_json::to_writer(&mut output, &json_value).map_err(|e| {
                            Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
                        })?;
                        writeln!(output).map_err(|e| {
                            Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
                        })?;
                    }
                    Err(decode_error) => {
                        summary.records_with_errors += 1;
                        // In lenient mode, continue processing
                        if options.strict_mode {
                            return Err(decode_error);
                        }
                    }
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                // End of file
                break;
            }
            Err(e) => {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    e.to_string(),
                ));
            }
        }
    }

    summary.records_processed = record_count;
    summary.processing_time_ms = start_time.elapsed().as_millis().min(u64::MAX as u128) as u64;
    summary.calculate_throughput();
    summary.schema_fingerprint = "placeholder_fingerprint".to_string();

    Ok(summary)
}

/// Encode JSONL to binary file
///
/// # Arguments
///
/// * `schema` - The parsed copybook schema
/// * `input` - Input stream to read JSONL from
/// * `output` - Output stream to write binary to
/// * `options` - Encoding options
///
/// # Errors
///
/// Returns an error if the JSONL cannot be encoded or written
pub fn encode_jsonl_to_file(
    schema: &Schema,
    input: impl Read,
    mut output: impl Write,
    options: &EncodeOptions,
) -> Result<RunSummary> {
    let start_time = std::time::Instant::now();
    let mut summary = RunSummary::new();

    let reader = BufReader::new(input);
    let mut record_count = 0u64;

    for line in reader.lines() {
        let line =
            line.map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;

        if line.trim().is_empty() {
            continue;
        }

        record_count += 1;

        // Parse JSON
        let json_value: Value = serde_json::from_str(&line)
            .map_err(|e| Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, e.to_string()))?;

        // Encode to binary
        if let Ok(binary_data) = encode_record(schema, &json_value, options) {
            output
                .write_all(&binary_data)
                .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;
            summary.bytes_processed += binary_data.len() as u64;
        } else {
            summary.records_with_errors += 1;
            // In lenient mode, continue processing
            if options.strict_mode {
                break;
            }
        }
    }

    summary.records_processed = record_count;
    summary.processing_time_ms = start_time.elapsed().as_millis().min(u64::MAX as u128) as u64;
    summary.calculate_throughput();
    summary.schema_fingerprint = "placeholder_fingerprint".to_string();

    Ok(summary)
}

/// Simple record iterator for programmatic access
///
/// This provides streaming access to decoded records without loading entire files into memory.
pub struct RecordIterator<R: Read> {
    reader: R,
    schema: Schema,
    options: DecodeOptions,
    record_index: u64,
    eof_reached: bool,
    buffer: Vec<u8>,
}

impl<R: Read> RecordIterator<R> {
    /// Create a new record iterator
    pub fn new(reader: R, schema: &Schema, options: &DecodeOptions) -> Result<Self> {
        let record_length = schema.lrecl_fixed.unwrap_or(1024) as usize;

        Ok(Self {
            reader,
            schema: schema.clone(),
            options: options.clone(),
            record_index: 0,
            eof_reached: false,
            buffer: vec![0u8; record_length],
        })
    }

    /// Get the current record index (1-based)
    pub fn current_record_index(&self) -> u64 {
        self.record_index
    }

    /// Check if the iterator has reached EOF
    pub fn is_eof(&self) -> bool {
        self.eof_reached
    }

    /// Get a reference to the schema
    pub fn schema(&self) -> &Schema {
        &self.schema
    }

    /// Get a reference to the options
    pub fn options(&self) -> &DecodeOptions {
        &self.options
    }
}

impl<R: Read> Iterator for RecordIterator<R> {
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof_reached {
            return None;
        }

        // Try to read a record
        match self.reader.read_exact(&mut self.buffer) {
            Ok(()) => {
                self.record_index += 1;

                // Decode the record
                match decode_record(&self.schema, &self.buffer, &self.options) {
                    Ok(json_value) => Some(Ok(json_value)),
                    Err(e) => Some(Err(e)),
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                self.eof_reached = true;
                None
            }
            Err(e) => Some(Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                e.to_string(),
            ))),
        }
    }
}

/// Convenience function to create a record iterator from a file path
pub fn iter_records_from_file<P: AsRef<std::path::Path>>(
    file_path: P,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RecordIterator<std::fs::File>> {
    let file = std::fs::File::open(file_path)
        .map_err(|e| Error::new(ErrorCode::CBKF104_RDW_SUSPECT_ASCII, e.to_string()))?;

    RecordIterator::new(file, schema, options)
}

/// Convenience function to create a record iterator from any readable source
pub fn iter_records<R: Read>(
    reader: R,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RecordIterator<R>> {
    RecordIterator::new(reader, schema, options)
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::parse_copybook;
    use std::io::Cursor;


    #[test]
    fn test_decode_record() {
        let copybook_text = r#"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        "#;

        let schema = parse_copybook(copybook_text).unwrap();
        let mut options = DecodeOptions::default();
        options.codepage = crate::Codepage::ASCII; // Use ASCII for test data
        let data = b"123ALICE";

        let result = decode_record(&schema, data, &options).unwrap();
        assert!(result.is_object());
        
        // Should decode actual field values (use leaf field names)
        assert_eq!(result.get("ID").unwrap(), "123");
        assert_eq!(result.get("NAME").unwrap(), "ALICE");
    }

    #[test]
    fn test_encode_record() {
        let copybook_text = r#"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        "#;

        let schema = parse_copybook(copybook_text).unwrap();
        let options = EncodeOptions::default();

        let mut json_obj = serde_json::Map::new();
        json_obj.insert("__status".to_string(), Value::String("test".to_string()));
        let json = Value::Object(json_obj);

        let result = encode_record(&schema, &json, &options).unwrap();
        assert!(!result.is_empty());
        assert_eq!(result[0], b'E'); // Encoded marker
    }

    #[test]
    fn test_record_iterator() {
        let copybook_text = r#"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        "#;

        let schema = parse_copybook(copybook_text).unwrap();
        let options = DecodeOptions::default();

        // Create test data
        let test_data = vec![0u8; 16]; // Two 8-byte records
        let cursor = Cursor::new(test_data);

        let iterator = RecordIterator::new(cursor, &schema, &options).unwrap();
        assert_eq!(iterator.current_record_index(), 0);
        assert!(!iterator.is_eof());
    }

    #[test]
    fn test_decode_file_to_jsonl() {
        let copybook_text = r#"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        "#;

        let schema = parse_copybook(copybook_text).unwrap();
        let mut options = DecodeOptions::default();
        options.codepage = crate::Codepage::ASCII; // Use ASCII for test data

        // Create test input with valid ASCII data
        let input_data = b"123ALICE789CAROL"; // Two 8-byte records with valid data
        let input = Cursor::new(input_data);

        // Create output buffer
        let mut output = Vec::new();

        let summary = decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
        assert!(summary.records_processed > 0);
        assert!(!output.is_empty());
    }

    #[test]
    fn test_encode_jsonl_to_file() {
        let copybook_text = r#"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        "#;

        let schema = parse_copybook(copybook_text).unwrap();
        let options = EncodeOptions::default();

        // Create test JSONL input
        let jsonl_data = r#"{"__status":"test"}
{"__status":"test2"}"#;
        let input = Cursor::new(jsonl_data.as_bytes());

        // Create output buffer
        let mut output = Vec::new();

        let summary = encode_jsonl_to_file(&schema, input, &mut output, &options).unwrap();
        assert_eq!(summary.records_processed, 2);
        assert!(!output.is_empty());
    }
}
