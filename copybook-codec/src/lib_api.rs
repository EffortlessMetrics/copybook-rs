//! Core library API implementation for task 9.1
//!
//! This module provides the main library functions required by R11:
//! - `parse_copybook` (already exists in copybook-core)
//! - `decode_record`
//! - `encode_record`
//! - `decode_file_to_jsonl`
//! - `encode_jsonl_to_file`
//! - `RecordIterator` (for programmatic access)

use crate::options::{DecodeOptions, EncodeOptions, RecordFormat, RawMode};
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

    /// Update run summary from error reporter summary
    pub fn update_from_error_summary(&mut self, error_summary: &copybook_core::ErrorSummary) {
        use copybook_core::ErrorSeverity;
        
        // Count warnings from error severity
        self.warnings = error_summary.error_counts.get(&ErrorSeverity::Warning).copied().unwrap_or(0);
        
        // Count records with errors
        self.records_with_errors = error_summary.records_with_errors;
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
/// Result carrying both successful data and any warnings generated
struct DecodeResult {
    warnings: Vec<copybook_core::Error>,
}

impl DecodeResult {
    fn new() -> Self {
        Self { warnings: Vec::new() }
    }
    
    fn add_warnings(&mut self, mut warnings: Vec<copybook_core::Error>) {
        self.warnings.append(&mut warnings);
    }
}

fn decode_fields_recursive(
    fields: &[copybook_core::Field],
    data: &[u8], 
    json_obj: &mut serde_json::Map<String, Value>,
    options: &DecodeOptions,
) -> Result<DecodeResult> {
    use copybook_core::FieldKind;
    
    let mut result = DecodeResult::new();
    
    for field in fields {
        // Skip FILLER fields unless explicitly requested
        if field.path.contains("_filler_") && !options.emit_filler {
            continue;
        }
        
        if let FieldKind::Group = &field.kind {
            // For groups, recursively process children
            let child_result = decode_fields_recursive(&field.children, data, json_obj, options)?;
            result.add_warnings(child_result.warnings);
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
                    let numeric_result = crate::numeric::decode_zoned_decimal(
                        field_data,
                        *digits,
                        *scale,
                        *signed,
                        options.codepage,
                        field.blank_when_zero,
                    )?;
                    // Collect warnings from numeric result
                    result.add_warnings(numeric_result.warnings);
                    
                    // Format with proper padding for integer fields (scale = 0)
                    let formatted_value = if *scale == 0 && !field.blank_when_zero && !*signed {
                        // Unsigned integer field without blank-when-zero - preserve leading zeros as per PIC specification
                        format!("{:0width$}", numeric_result.value.value, width = *digits as usize)
                    } else {
                        // Signed field, decimal field, or blank-when-zero field - use the SmallDecimal display format
                        numeric_result.value.to_string()
                    };
                    Value::String(formatted_value)
                }
                FieldKind::PackedDecimal { digits, scale, signed } => {
                    // Decode packed decimal
                    let numeric_result = crate::numeric::decode_packed_decimal(
                        field_data,
                        *digits,
                        *scale,
                        *signed,
                    )?;
                    // Collect warnings from numeric result
                    result.add_warnings(numeric_result.warnings);
                    Value::String(numeric_result.value.to_string())
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
    Ok(result)
}

/// Decode a record from binary data using the provided schema
///
/// # Errors
///
/// Returns an error if the data cannot be decoded according to the schema
pub fn decode_record(schema: &Schema, data: &[u8], options: &DecodeOptions) -> Result<Value> {
    use serde_json::Map;
    
    let mut json_obj = Map::new();
    let _decode_result = decode_fields_recursive(&schema.fields, data, &mut json_obj, options)?;
    // TODO: Return warnings somehow - for now just ignore them in this function
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
pub fn encode_record(schema: &Schema, json: &Value, options: &EncodeOptions) -> Result<Vec<u8>> {
    // Handle RDW format with raw data preservation
    if options.format == RecordFormat::RDW && options.use_raw {
        if let Some(Value::String(raw_b64)) = json.get("__raw_b64") {
            use base64::prelude::*;
            if let Ok(raw_data) = BASE64_STANDARD.decode(raw_b64) {
                // If the raw data looks like it has an RDW header, use it directly
                if raw_data.len() >= 4 {
                    let claimed_length = u16::from_be_bytes([raw_data[0], raw_data[1]]) as usize;
                    if claimed_length == raw_data.len() - 4 {
                        return Ok(raw_data);
                    }
                }
            }
        }
    }
    
    // Enhanced encoding with ODO support
    let mut encoded_data = Vec::new();
    
    if let Some(obj) = json.as_object() {
        // First pass: collect ODO updates needed
        let mut odo_updates = std::collections::HashMap::new();
        collect_odo_updates(schema, &schema.fields, obj, &mut odo_updates);
        
        // Debug output (remove in production)
        // eprintln!("DEBUG: Schema fields: {:?}", schema.fields.iter().map(|f| &f.name).collect::<Vec<_>>());
        // eprintln!("DEBUG: JSON object keys: {:?}", obj.keys().collect::<Vec<_>>());
        // eprintln!("DEBUG: ODO updates found: {:?}", odo_updates);
        
        // Second pass: encode fields with ODO counter updates applied
        for field in &schema.fields {
            encode_field_with_odo(field, obj, &mut encoded_data, &odo_updates)?;
        }
    }
    
    // Handle RDW format wrapping if needed
    if options.format == RecordFormat::RDW {
        use crate::record::RDWRecord;
        
        let rdw_record = RDWRecord::new(encoded_data);
        let mut full_data = Vec::with_capacity(4 + rdw_record.payload.len());
        full_data.extend_from_slice(&rdw_record.header_bytes());
        full_data.extend_from_slice(&rdw_record.payload);
        return Ok(full_data);
    }
    
    Ok(encoded_data)
}

/// Collect ODO counter updates needed (recursive search through all fields)
fn collect_odo_updates(
    _schema: &copybook_core::Schema, 
    fields: &[copybook_core::Field], 
    obj: &serde_json::Map<String, Value>, 
    odo_updates: &mut std::collections::HashMap<String, u32>
) {
    for field in fields {
        // eprintln!("DEBUG: Checking field '{}' for ODO", field.name);
        
        if let Some(ref occurs) = field.occurs {
            if let copybook_core::Occurs::ODO { counter_path, .. } = occurs {
                // eprintln!("DEBUG: Found ODO field '{}' with counter_path '{}'", field.name, counter_path);
                // Check if this field has an array in JSON (search in flat JSON structure)
                if let Some(Value::Array(array)) = obj.get(&field.name) {
                    // eprintln!("DEBUG: Found array for ODO field '{}' with {} elements", field.name, array.len());
                    odo_updates.insert(counter_path.clone(), array.len() as u32);
                }
            }
        }
        
        // Always recurse into child fields, using the same flattened JSON object
        if !field.children.is_empty() {
            // eprintln!("DEBUG: Recursing into children of field '{}'", field.name);
            collect_odo_updates(_schema, &field.children, obj, odo_updates);
        }
    }
}

/// Enhanced field encoding with ODO counter support
fn encode_field_with_odo(
    field: &copybook_core::Field, 
    obj: &serde_json::Map<String, Value>, 
    buffer: &mut Vec<u8>, 
    odo_updates: &std::collections::HashMap<String, u32>
) -> Result<()> {
    // eprintln!("DEBUG: Processing field '{}' (len: {}, has_children: {})", field.name, field.len, !field.children.is_empty());
    
    // If this is a group field (has children), process children recursively
    if !field.children.is_empty() {
        for child in &field.children {
            encode_field_with_odo(child, obj, buffer, odo_updates)?;
        }
        return Ok(());
    }
    
    // Check if this field has array data (ODO or fixed array)
    if let Some(Value::Array(array)) = obj.get(&field.name) {
        // eprintln!("DEBUG: Encoding array field '{}' with {} elements", field.name, array.len());
        
        // For arrays, each element has the field size (field.len is per element)
        let element_size = field.len as usize;
        
        for (i, element) in array.iter().enumerate() {
            // eprintln!("DEBUG: Encoding array element {} of {}: {:?}", i + 1, array.len(), element);
            match element {
                Value::String(s) => {
                    let mut field_data = s.as_bytes().to_vec();
                    field_data.resize(element_size, b' '); // Pad with spaces
                    buffer.extend_from_slice(&field_data[..element_size]); // Truncate if needed
                }
                _ => {
                    // For non-string array elements, fill with spaces
                    buffer.extend(vec![b' '; element_size]);
                }
            }
        }
        return Ok(());
    }
    
    // This is a leaf field - encode it as a single value
    let field_value = if odo_updates.contains_key(&field.path) || odo_updates.contains_key(&field.name) {
        // This is an ODO counter field - use the updated count
        let counter_value = odo_updates.get(&field.path)
            .or_else(|| odo_updates.get(&field.name))
            .unwrap_or(&0);
        
        // eprintln!("DEBUG: Updating ODO counter '{}' (path: '{}') to value {}", field.name, field.path, counter_value);
        Value::String(format!("{:0width$}", counter_value, width = field.len as usize))
    } else {
        // Use original value from JSON
        let orig_value = obj.get(&field.name).cloned().unwrap_or(Value::Null);
        // eprintln!("DEBUG: Encoding field '{}' (path: '{}') with original value: {:?}", field.name, field.path, orig_value);
        orig_value
    };
    
    // Use the original simple encoding logic
    match field_value {
        Value::String(s) => {
            // Simple string encoding - pad or truncate to field size
            let field_size = field.len as usize;
            let mut field_data = s.as_bytes().to_vec();
            field_data.resize(field_size, b' '); // Pad with spaces
            buffer.extend_from_slice(&field_data[..field_size]); // Truncate if needed
        }
        Value::Number(n) => {
            // Simple numeric encoding - convert to string and pad
            let s = n.to_string();
            let field_size = field.len as usize;
            let mut field_data = s.as_bytes().to_vec();
            field_data.resize(field_size, b'0'); // Pad with zeros
            buffer.extend_from_slice(&field_data[..field_size]);
        }
        _ => {
            // Default: encode as empty field
            let field_size = field.len as usize;
            buffer.extend(vec![b' '; field_size]);
        }
    }
    
    Ok(())
}

/// Simple field encoding for basic functionality
fn encode_field_simple(field: &copybook_core::Field, obj: &serde_json::Map<String, Value>, buffer: &mut Vec<u8>) -> Result<()> {
    if let Some(value) = obj.get(&field.name) {
        match value {
            Value::String(s) => {
                // Simple string encoding - pad or truncate to field size
                let field_size = field.len as usize;
                let mut field_data = s.as_bytes().to_vec();
                field_data.resize(field_size, b' '); // Pad with spaces
                buffer.extend_from_slice(&field_data[..field_size]); // Truncate if needed
            }
            Value::Number(n) => {
                // Simple numeric encoding - convert to string and pad
                let s = n.to_string();
                let field_size = field.len as usize;
                let mut field_data = s.as_bytes().to_vec();
                field_data.resize(field_size, b'0'); // Pad with zeros
                buffer.extend_from_slice(&field_data[..field_size]);
            }
            _ => {
                // Default: encode as empty field
                let field_size = field.len as usize;
                buffer.extend(vec![b' '; field_size]);
            }
        }
    } else {
        // Field not found: encode as empty
        let field_size = field.len as usize;
        buffer.extend(vec![b' '; field_size]);
    }
    
    // Handle child fields recursively
    for child in &field.children {
        encode_field_simple(child, obj, buffer)?;
    }
    
    Ok(())
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
    input: impl Read,
    mut output: impl Write,
    options: &DecodeOptions,
) -> Result<RunSummary> {
    let start_time = std::time::Instant::now();
    let mut summary = RunSummary::new();

    match options.format {
        RecordFormat::Fixed => {
            decode_fixed_file_to_jsonl(schema, input, &mut output, options, &mut summary)
        }
        RecordFormat::RDW => {
            decode_rdw_file_to_jsonl(schema, input, &mut output, options, &mut summary)
        }
    }?;

    // Calculate final timing and throughput
    let processing_time = start_time.elapsed();
    summary.processing_time_ms = processing_time.as_millis() as u64;
    
    if summary.processing_time_ms > 0 {
        summary.throughput_mbps = (summary.bytes_processed as f64 / 1_048_576.0) 
            / (summary.processing_time_ms as f64 / 1000.0);
    }
    
    summary.schema_fingerprint = format!("{:x}", calculate_schema_hash(schema));
    summary.threads_used = 1; // Single-threaded for now
    
    Ok(summary)
}

fn decode_fixed_file_to_jsonl(
    schema: &Schema,
    input: impl Read,
    output: &mut impl Write,
    options: &DecodeOptions,
    summary: &mut RunSummary,
) -> Result<()> {
    use crate::record::{FixedRecordReader};
    
    let mut reader = FixedRecordReader::new(input, schema.lrecl_fixed)?;
    let mut record_count = 0u64;

    loop {
        match reader.read_record()? {
            Some(record_data) => {
                record_count += 1;
                summary.bytes_processed += record_data.len() as u64;

                // Validate record length against schema
                reader.validate_record_length(schema, &record_data)?;

                // Decode the record
                decode_and_write_record(schema, &record_data, output, options, summary, None)?;
            }
            None => break, // EOF
        }
    }
    
    summary.records_processed = record_count;
    Ok(())
}

fn decode_rdw_file_to_jsonl(
    schema: &Schema,
    input: impl Read,
    output: &mut impl Write,
    options: &DecodeOptions,
    summary: &mut RunSummary,
) -> Result<()> {
    use crate::record::{RDWRecordReader, RDWParsingMode};
    
    let parsing_mode = if options.strict_mode {
        RDWParsingMode::Strict
    } else {
        RDWParsingMode::Lenient  
    };
    
    let mut reader = RDWRecordReader::new_with_mode(input, parsing_mode);
    
    // For RDW records, don't set a strict maximum based on schema fixed length
    // RDW records are variable-length by definition
    // Only set a reasonable maximum to prevent memory exhaustion
    reader.set_max_record_length(1024 * 1024); // 1MB max record size
    
    loop {
        match reader.read_record()? {
            Some(rdw_record) => {
                summary.bytes_processed += (4 + rdw_record.payload.len()) as u64; // RDW header + payload
                
                // Check for RDW warnings and collect them
                let metadata = rdw_record.get_metadata();
                if metadata.has_non_zero_reserved && !options.strict_mode {
                    // In lenient mode, non-zero reserved bytes generate a warning
                    summary.warnings += 1;
                }
                if metadata.suspect_ascii_corruption {
                    // ASCII corruption is always a concern
                    summary.warnings += 1;
                }
                
                // Validate zero-length records if applicable
                if rdw_record.payload.is_empty() {
                    reader.validate_zero_length_record(schema)?;
                }
                
                // Validate record length against schema
                reader.validate_record_length_against_schema(schema, rdw_record.length() as u32)?;

                // Decode the record payload with optional RDW header for raw preservation
                let full_rdw_data = if options.emit_raw == RawMode::RecordRDW {
                    let mut full_data = Vec::with_capacity(4 + rdw_record.payload.len());
                    full_data.extend_from_slice(&rdw_record.header_bytes());
                    full_data.extend_from_slice(&rdw_record.payload);
                    Some(full_data)
                } else {
                    None
                };
                
                decode_and_write_record(schema, &rdw_record.payload, output, options, summary, full_rdw_data.as_deref())?;
            }
            None => break, // EOF
        }
    }
    
    summary.records_processed = reader.record_count();
    Ok(())
}

fn decode_and_write_record(
    schema: &Schema,
    record_data: &[u8],
    output: &mut impl Write,
    options: &DecodeOptions,
    summary: &mut RunSummary,
    rdw_raw_data: Option<&[u8]>,
) -> Result<()> {
    // Decode the record directly to collect warnings
    use serde_json::Map;
    let mut json_obj = Map::new();
    match decode_fields_recursive(&schema.fields, record_data, &mut json_obj, options) {
        Ok(decode_result) => {
            // Count warnings
            summary.warnings += decode_result.warnings.len() as u64;
            
            // Add raw data if requested
            if let Some(raw_data) = rdw_raw_data {
                if options.emit_raw == RawMode::RecordRDW {
                    use base64::prelude::*;
                    let encoded = BASE64_STANDARD.encode(raw_data);
                    json_obj.insert("__raw_b64".to_string(), Value::String(encoded));
                }
            } else if options.emit_raw == RawMode::Record {
                use base64::prelude::*;
                let encoded = BASE64_STANDARD.encode(record_data);
                json_obj.insert("__raw_b64".to_string(), Value::String(encoded));
            }
            
            // Write as JSONL
            let json_value = Value::Object(json_obj);
            serde_json::to_writer(&mut *output, &json_value).map_err(|e| {
                Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
            })?;
            writeln!(&mut *output).map_err(|e| {
                Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
            })?;
            
            Ok(())
        }
        Err(decode_error) => {
            summary.records_with_errors += 1;
            // In lenient mode, continue processing
            if options.strict_mode {
                return Err(decode_error);
            }
            // Log error and continue in lenient mode
            eprintln!("Warning: Record decode error: {}", decode_error);
            Ok(())
        }
    }
}

/// Calculate a simple hash of the schema for fingerprinting
fn calculate_schema_hash(_schema: &Schema) -> u64 {
    // Simple placeholder hash since Schema doesn't implement Hash
    0xDEADBEEF
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
        json_obj.insert("ID".to_string(), Value::String("123".to_string()));
        json_obj.insert("NAME".to_string(), Value::String("ALICE".to_string()));
        let json = Value::Object(json_obj);

        let result = encode_record(&schema, &json, &options).unwrap();
        assert!(!result.is_empty());
        
        // Verify basic encoding functionality
        assert!(result.len() > 0);
        let result_str = String::from_utf8_lossy(&result);
        assert!(result_str.contains("123"), "ID value should be present");
        assert!(result_str.contains("ALICE"), "NAME value should be present");
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
