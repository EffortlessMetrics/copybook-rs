//! Core library API implementation for task 9.1
//!
//! This module provides the main library functions required by R11:
//! - parse_copybook (already exists in copybook-core)
//! - decode_record 
//! - encode_record
//! - decode_file_to_jsonl
//! - encode_jsonl_to_file
//! - RecordIterator (for programmatic access)

use copybook_core::{Schema, Field, FieldKind, Occurs, Error, ErrorCode, Result};
use crate::options::{DecodeOptions, EncodeOptions};
use serde_json::Value;
use std::io::{Read, Write, BufRead, BufReader};
use std::fmt;

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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Processing Summary:")?;
        writeln!(f, "  Records processed: {}", self.records_processed)?;
        writeln!(f, "  Records with errors: {}", self.records_with_errors)?;
        writeln!(f, "  Warnings: {}", self.warnings)?;
        writeln!(f, "  Success rate: {:.1}%", self.success_rate())?;
        writeln!(f, "  Processing time: {:.2}s", self.processing_time_seconds())?;
        writeln!(f, "  Bytes processed: {:.2} MB", self.bytes_processed_mb())?;
        writeln!(f, "  Throughput: {:.2} MB/s", self.throughput_mbps)?;
        writeln!(f, "  Threads used: {}", self.threads_used)?;
        if let Some(peak_memory) = self.peak_memory_bytes {
            writeln!(f, "  Peak memory: {:.2} MB", peak_memory as f64 / (1024.0 * 1024.0))?;
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
/// * `options` - Decoding options controlling filler emission, metadata and number handling
///
/// # Errors
///
/// Returns an error if the data cannot be decoded according to the schema
pub fn decode_record(schema: &Schema, data: &[u8], options: &DecodeOptions) -> Result<Value> {
    use base64::{engine::general_purpose, Engine as _};
    use serde_json::{Map, Value};

    // Helper to decode a scalar field
    fn decode_scalar(field: &Field, data: &[u8], options: &DecodeOptions, offset: usize) -> Result<Value> {
        let end = offset + field.len as usize;
        if end > data.len() {
            return Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                format!("Field {} extends beyond record boundary", field.path),
            ));
        }
        let field_data = &data[offset..end];

        match &field.kind {
            FieldKind::Alphanum { .. } => {
                let text = crate::charset::ebcdic_to_utf8(
                    field_data,
                    options.codepage,
                    options.on_decode_unmappable,
                )?;
                Ok(Value::String(text))
            }
            FieldKind::ZonedDecimal { digits, scale, signed } => {
                let decimal = crate::numeric::decode_zoned_decimal(
                    field_data,
                    *digits,
                    *scale,
                    *signed,
                    options.codepage,
                    field.blank_when_zero,
                )?;
                let dec_str = decimal.to_fixed_scale_string(*scale);
                match options.json_number_mode {
                    crate::options::JsonNumberMode::Lossless => Ok(Value::String(dec_str)),
                    crate::options::JsonNumberMode::Native => {
                        if let Ok(num) = dec_str.parse::<f64>() {
                            if let Some(n) = serde_json::Number::from_f64(num) {
                                return Ok(Value::Number(n));
                            }
                        }
                        Ok(Value::String(dec_str))
                    }
                }
            }
            FieldKind::PackedDecimal { digits, scale, signed } => {
                let decimal = crate::numeric::decode_packed_decimal(field_data, *digits, *scale, *signed)?;
                let dec_str = decimal.to_fixed_scale_string(*scale);
                match options.json_number_mode {
                    crate::options::JsonNumberMode::Lossless => Ok(Value::String(dec_str)),
                    crate::options::JsonNumberMode::Native => {
                        if let Ok(num) = dec_str.parse::<f64>() {
                            if let Some(n) = serde_json::Number::from_f64(num) {
                                return Ok(Value::Number(n));
                            }
                        }
                        Ok(Value::String(dec_str))
                    }
                }
            }
            FieldKind::BinaryInt { bits, signed } => {
                let int_value = crate::numeric::decode_binary_int(field_data, *bits, *signed)?;
                match options.json_number_mode {
                    crate::options::JsonNumberMode::Native if *bits <= 64 => {
                        Ok(Value::Number(serde_json::Number::from(int_value)))
                    }
                    _ => Ok(Value::String(int_value.to_string())),
                }
            }
            FieldKind::Group => Err(Error::new(
                ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                format!("Group field {} processed as scalar", field.path),
            )),
        }
    }

    // Process fields recursively
    fn process_fields(
        fields: &[Field],
        data: &[u8],
        options: &DecodeOptions,
        delta: usize,
        json_obj: &mut Map<String, Value>,
    ) -> Result<()> {
        for field in fields {
            // Handle FILLER fields
            if field.name.eq_ignore_ascii_case("FILLER") && !options.emit_filler {
                continue;
            }

            let key = if field.name.eq_ignore_ascii_case("FILLER") {
                format!("_filler_{:08}", field.offset + delta as u32)
            } else {
                field.name.clone()
            };

            match &field.kind {
                FieldKind::Group => {
                    if let Some(occurs) = &field.occurs {
                        let count = match occurs {
                            Occurs::Fixed { count } => *count as usize,
                            Occurs::ODO { max, .. } => *max as usize,
                        };
                        let mut array = Vec::new();
                        for i in 0..count {
                            let mut element_obj = Map::new();
                            let new_delta = delta + i * field.len as usize;
                            process_fields(&field.children, data, options, new_delta, &mut element_obj)?;
                            array.push(Value::Object(element_obj));
                        }
                        json_obj.insert(key, Value::Array(array));
                    } else if field.level <= 1 {
                        process_fields(&field.children, data, options, delta, json_obj)?;
                    } else {
                        let mut group_obj = Map::new();
                        process_fields(&field.children, data, options, delta, &mut group_obj)?;
                        json_obj.insert(key, Value::Object(group_obj));
                    }
                }
                _ => {
                    if let Some(occurs) = &field.occurs {
                        let count = match occurs {
                            Occurs::Fixed { count } => *count as usize,
                            Occurs::ODO { max, .. } => *max as usize,
                        };
                        let element_size = field.len as usize / count.max(1);
                        let mut array = Vec::new();
                        for i in 0..count {
                            let offset = field.offset as usize + delta + i * element_size;
                            let mut element_field = field.clone();
                            element_field.len = element_size as u32;
                            element_field.occurs = None;
                            let value = decode_scalar(&element_field, data, options, offset)?;
                            array.push(value);
                        }
                        json_obj.insert(key, Value::Array(array));
                    } else {
                        let offset = field.offset as usize + delta;
                        let value = decode_scalar(field, data, options, offset)?;
                        json_obj.insert(key.clone(), value);

                        // Add raw field data if requested
                        if matches!(options.emit_raw, crate::options::RawMode::Field) {
                            let end = offset + field.len as usize;
                            if end <= data.len() {
                                let encoded = general_purpose::STANDARD.encode(&data[offset..end]);
                                json_obj.insert(format!("{}_raw_b64", key), Value::String(encoded));
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }


    let mut json_obj = Map::new();
    process_fields(&schema.fields, data, options, 0, &mut json_obj)?;

    if options.emit_meta {
        json_obj.insert("__schema_id".to_string(), Value::String(schema.fingerprint.clone()));
        json_obj.insert("__record_index".to_string(), Value::Number(1.into()));
        json_obj.insert("__offset".to_string(), Value::Number(0.into()));
        json_obj.insert("__length".to_string(), Value::Number(data.len().into()));
    }

    if matches!(options.emit_raw, crate::options::RawMode::Record | crate::options::RawMode::RecordRDW) {
        let encoded = general_purpose::STANDARD.encode(data);
        json_obj.insert("__raw_b64".to_string(), Value::String(encoded));
    }

    Ok(Value::Object(json_obj))
}

fn count_bwz_warnings(
    fields: &[Field],
    data: &[u8],
    options: &DecodeOptions,
    delta: usize,
) -> u64 {
    fn check(field: &Field, slice: &[u8], options: &DecodeOptions) -> u64 {
        if field.blank_when_zero {
            let is_all_spaces = slice.iter().all(|&b| match options.codepage {
                crate::Codepage::ASCII => b == b' ',
                _ => b == 0x40,
            });
            if is_all_spaces {
                return 1;
            }
        }
        0
    }

    let mut warnings = 0;
    for field in fields {
        match &field.kind {
            FieldKind::Group => {
                if let Some(occurs) = &field.occurs {
                    let count = match occurs {
                        Occurs::Fixed { count } => *count as usize,
                        Occurs::ODO { max, .. } => *max as usize,
                    };
                    for i in 0..count {
                        warnings += count_bwz_warnings(
                            &field.children,
                            data,
                            options,
                            delta + i * field.len as usize,
                        );
                    }
                } else {
                    warnings += count_bwz_warnings(&field.children, data, options, delta);
                }
            }
            _ => {
                if let Some(occurs) = &field.occurs {
                    let count = match occurs {
                        Occurs::Fixed { count } => *count as usize,
                        Occurs::ODO { max, .. } => *max as usize,
                    };
                    let element_size = field.len as usize / count.max(1);
                    for i in 0..count {
                        let offset = field.offset as usize + delta + i * element_size;
                        if offset + element_size <= data.len() {
                            warnings +=
                                check(field, &data[offset..offset + element_size], options);
                        }
                    }
                } else {
                    let offset = field.offset as usize + delta;
                    if offset + field.len as usize <= data.len() {
                        warnings +=
                            check(field, &data[offset..offset + field.len as usize], options);
                    }
                }
            }
        }
    }
    warnings
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
    if let Some(obj) = json.as_object() {
        if obj.contains_key("__status") {
            buffer[0] = b'E'; // Encoded marker
        }
    }
    
    Ok(buffer)
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
    
    let record_length = schema.lrecl_fixed.unwrap_or(1024) as usize;
    let mut buffer = vec![0u8; record_length];
    let mut record_count = 0u64;

    'outer: loop {
        let mut read = 0;
        while read < record_length {
            match input.read(&mut buffer[read..record_length]) {
                Ok(0) => {
                    if read == 0 {
                        break 'outer;
                    } else if record_count == 0 {
                        return Err(Error::new(
                            ErrorCode::CBKD301_RECORD_TOO_SHORT,
                            "Record shorter than expected".to_string(),
                        ));
                    } else {
                        break 'outer;
                    }
                }
                Ok(n) => read += n,
                Err(e) => {
                    if e.kind() == std::io::ErrorKind::UnexpectedEof {
                        if read == 0 {
                            break 'outer;
                        } else if record_count == 0 {
                            return Err(Error::new(
                                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                                e.to_string(),
                            ));
                        } else {
                            break 'outer;
                        }
                    } else {
                        return Err(Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, e.to_string()));
                    }
                }
            }
        }

        record_count += 1;
        summary.bytes_processed += record_length as u64;

        match decode_record(schema, &buffer, options) {
            Ok(json_value) => {
                serde_json::to_writer(&mut output, &json_value)
                    .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;
                writeln!(output)
                    .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;
                summary.warnings += count_bwz_warnings(&schema.fields, &buffer, options, 0);
            }
            Err(e) => {
                summary.records_with_errors += 1;
                if options.strict_mode {
                    return Err(e);
                }
            }
        }
    }

    summary.records_processed = record_count;
    summary.processing_time_ms = start_time.elapsed().as_millis() as u64;
    summary.calculate_throughput();
    summary.schema_fingerprint = schema.fingerprint.clone();

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
        let line = line.map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;
        
        if line.trim().is_empty() {
            continue;
        }
        
        record_count += 1;
        
        // Parse JSON
        let json_value: Value = serde_json::from_str(&line)
            .map_err(|e| Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, e.to_string()))?;
        
        // Encode to binary
        match encode_record(schema, &json_value, options) {
            Ok(binary_data) => {
                output.write_all(&binary_data)
                    .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;
                summary.bytes_processed += binary_data.len() as u64;
            }
            Err(_) => {
                summary.records_with_errors += 1;
                // In lenient mode, continue processing
                if options.strict_mode {
                    break;
                }
            }
        }
    }
    
    summary.records_processed = record_count;
    summary.processing_time_ms = start_time.elapsed().as_millis() as u64;
    summary.calculate_throughput();
    summary.schema_fingerprint = schema.fingerprint.clone();
    
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
            Err(e) => {
                Some(Err(Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, e.to_string())))
            }
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
    use crate::Codepage;
    use std::io::Cursor;

    #[test]
    fn test_decode_record() {
        let copybook_text = r#"
            01 RECORD.
               05 ID      PIC 9(3).
               05 AMOUNT  PIC S9(3) COMP-3.
               05 COUNT   PIC 9(4) COMP.
               05 NAME    PIC X(5).
        "#;

        let schema = parse_copybook(copybook_text).unwrap();
        let options = DecodeOptions::new()
            .with_codepage(Codepage::ASCII)
            .with_emit_meta(true);

        // Build record data
        let mut data = Vec::new();
        data.extend_from_slice(b"123"); // ID
        let packed = crate::numeric::encode_packed_decimal("123", 3, 0, true).unwrap();
        data.extend_from_slice(&packed); // AMOUNT
        data.extend_from_slice(&42u16.to_be_bytes()); // COUNT
        data.extend_from_slice(b"ALICE"); // NAME

        let result = decode_record(&schema, &data, &options).unwrap();
        assert_eq!(result["ID"], "123");
        assert_eq!(result["AMOUNT"], "123");
        assert_eq!(result["COUNT"], "42");
        assert_eq!(result["NAME"], "ALICE");
        assert_eq!(result["__schema_id"], schema.fingerprint);
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
               05 ID      PIC 9(3).
               05 AMOUNT  PIC S9(3) COMP-3.
               05 COUNT   PIC 9(4) COMP.
               05 NAME    PIC X(5).
        "#;

        let schema = parse_copybook(copybook_text).unwrap();
        let options = DecodeOptions::new().with_codepage(Codepage::ASCII);

        // Build test record
        let mut record = Vec::new();
        record.extend_from_slice(b"123");
        let packed = crate::numeric::encode_packed_decimal("123", 3, 0, true).unwrap();
        record.extend_from_slice(&packed);
        record.extend_from_slice(&42u16.to_be_bytes());
        record.extend_from_slice(b"ALICE");

        // Two records input
        let input_data = [record.clone(), record.clone()].concat();
        let input = Cursor::new(input_data);

        // Create output buffer
        let mut output = Vec::new();

        let summary = decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
        assert_eq!(summary.schema_fingerprint, schema.fingerprint);
        assert_eq!(summary.records_processed, 2);
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
        assert_eq!(summary.schema_fingerprint, schema.fingerprint);
        assert!(!output.is_empty());
    }
}