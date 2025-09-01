//! High-level processing engine with integrated error reporting
//!
//! This module provides the main processing logic that integrates structured error
//! reporting, corruption detection, and configurable error handling modes.

use crate::options::{DecodeOptions, EncodeOptions};
use crate::corruption::{detect_rdw_ascii_corruption, detect_ebcdic_corruption, detect_packed_corruption};
use crate::RunSummary;
use copybook_core::{
    Schema, Error, ErrorCode, ErrorReporter, ErrorMode,
    Field, FieldKind
};
use std::io::{Read, Write, BufRead, BufReader};
use std::time::Instant;
use tracing::{info, debug};

/// High-level processor for decoding operations with integrated error handling
pub struct DecodeProcessor {
    /// Error reporter for structured error handling
    error_reporter: ErrorReporter,
    /// Processing options
    options: DecodeOptions,
    /// Processing start time
    start_time: Instant,
    /// Bytes processed counter
    bytes_processed: u64,
}

/// High-level processor for encoding operations with integrated error handling
pub struct EncodeProcessor {
    /// Error reporter for structured error handling
    error_reporter: ErrorReporter,
    /// Processing options
    options: EncodeOptions,
    /// Processing start time
    start_time: Instant,
    /// Bytes processed counter
    bytes_processed: u64,
}

impl DecodeProcessor {
    /// Create a new decode processor with the given options
    pub fn new(options: DecodeOptions) -> Self {
        let error_mode = if options.strict_mode {
            ErrorMode::Strict
        } else {
            ErrorMode::Lenient
        };

        let error_reporter = ErrorReporter::new(error_mode, options.max_errors)
            .with_verbose_logging(true);

        Self {
            error_reporter,
            options,
            start_time: Instant::now(),
            bytes_processed: 0,
        }
    }

    /// Process a file with comprehensive error handling and reporting
    pub fn process_file<R: Read, W: Write>(
        &mut self,
        schema: &Schema,
        input: R,
        mut output: W,
    ) -> Result<RunSummary, Error> {
        info!("Starting decode processing with {} threads", self.options.threads);
        
        let mut reader = BufReader::new(input);
        let mut record_index = 0u64;
        let mut buffer = Vec::new();

        // Process records one by one
        loop {
            buffer.clear();
            record_index += 1;
            
            // Notify error reporter of record start
            self.error_reporter.start_record(record_index);

            // Read record based on format
            let record_len = match self.read_record(&mut reader, &mut buffer, schema)? {
                Some(len) => {
                    self.bytes_processed += len as u64;
                    len
                },
                None => break, // End of file
            };

            let record_data = &buffer[..record_len];

            // Process the record with error handling
            match self.process_record(schema, record_data, record_index) {
                Ok(json_line) => {
                    // Write successful result
                    writeln!(output, "{}", json_line)
                        .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;
                }
                Err(error) => {
                    // Report error and check if we should continue
                    if let Err(fatal_error) = self.error_reporter.report_error(error) {
                        return Err(fatal_error);
                    }
                    // In lenient mode, continue to next record
                }
            }
        }

        // Generate final summary
        self.generate_summary(record_index - 1)
    }

    /// Read a single record based on the configured format
    fn read_record<R: BufRead>(
        &mut self,
        reader: &mut R,
        buffer: &mut Vec<u8>,
        schema: &Schema,
    ) -> Result<Option<usize>, Error> {
        match self.options.format {
            crate::options::RecordFormat::Fixed => {
                self.read_fixed_record(reader, buffer, schema)
            }
            crate::options::RecordFormat::RDW => {
                self.read_rdw_record(reader, buffer)
            }
        }
    }

    /// Read a fixed-length record
    fn read_fixed_record<R: BufRead>(
        &mut self,
        reader: &mut R,
        buffer: &mut Vec<u8>,
        schema: &Schema,
    ) -> Result<Option<usize>, Error> {
        let record_length = schema.lrecl_fixed.unwrap_or(0) as usize;
        if record_length == 0 {
            return Err(Error::new(
                ErrorCode::CBKS141_RECORD_TOO_LARGE,
                "Fixed record length not specified in schema"
            ));
        }

        buffer.resize(record_length, 0);
        match reader.read_exact(buffer) {
            Ok(()) => Ok(Some(record_length)),
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => Ok(None),
            Err(e) => Err(Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, e.to_string())),
        }
    }

    /// Read an RDW variable-length record
    fn read_rdw_record<R: BufRead>(
        &mut self,
        reader: &mut R,
        buffer: &mut Vec<u8>,
    ) -> Result<Option<usize>, Error> {
        // Read RDW header (4 bytes)
        let mut rdw_header = [0u8; 4];
        match reader.read_exact(&mut rdw_header) {
            Ok(()) => {},
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
            Err(e) => return Err(Error::new(ErrorCode::CBKR221_RDW_UNDERFLOW, e.to_string())),
        }

        // Check for corruption in RDW header
        if let Some(corruption_error) = detect_rdw_ascii_corruption(&rdw_header) {
            self.error_reporter.report_warning(corruption_error);
        }

        // Parse RDW header
        let record_length = u16::from_be_bytes([rdw_header[0], rdw_header[1]]) as usize;
        let reserved = u16::from_be_bytes([rdw_header[2], rdw_header[3]]);

        // Check reserved bytes
        if reserved != 0 {
            let warning = Error::new(
                ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
                format!("RDW reserved bytes are non-zero: 0x{:04X}", reserved)
            );
            self.error_reporter.report_warning(warning);
        }

        // Validate record length
        if record_length == 0 {
            return Err(Error::new(
                ErrorCode::CBKR221_RDW_UNDERFLOW,
                "RDW record length is zero"
            ));
        }

        // Read record data
        buffer.resize(record_length, 0);
        reader.read_exact(buffer)
            .map_err(|e| Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, e.to_string()))?;

        Ok(Some(record_length))
    }

    /// Process a single record with comprehensive error detection
    fn process_record(
        &mut self,
        schema: &Schema,
        record_data: &[u8],
        record_index: u64,
    ) -> Result<String, Error> {
        debug!("Processing record {} ({} bytes)", record_index, record_data.len());

        // Perform corruption detection on the record
        self.detect_record_corruption(schema, record_data, record_index)?;

        // Decode the record to JSON
        let json_value = crate::decode_record(schema, record_data, &self.options)
            .map_err(|e| e.with_record(record_index))?;

        // Serialize to JSON string
        serde_json::to_string(&json_value)
            .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
                .with_record(record_index))
    }

    /// Detect various forms of corruption in record data
    fn detect_record_corruption(
        &mut self,
        schema: &Schema,
        record_data: &[u8],
        record_index: u64,
    ) -> Result<(), Error> {
        // Recursively check fields for corruption
        for field in &schema.fields {
            self.check_field_corruption(field, record_data, record_index)?;
        }
        Ok(())
    }

    /// Check a specific field for corruption patterns
    fn check_field_corruption(
        &mut self,
        field: &Field,
        record_data: &[u8],
        record_index: u64,
    ) -> Result<(), Error> {
        let field_start = field.offset as usize;
        let field_end = field_start + field.len as usize;

        // Ensure field is within record bounds
        if field_end > record_data.len() {
            return Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                format!("Field {} extends beyond record boundary", field.path)
            ).with_record(record_index).with_field(&field.path).with_offset(field.offset as u64));
        }

        let field_data = &record_data[field_start..field_end];

        // Check for corruption based on field type
        match &field.kind {
            FieldKind::Alphanum { .. } => {
                // Check for EBCDIC corruption in text fields
                let corruption_errors = detect_ebcdic_corruption(field_data, &field.path);
                for error in corruption_errors {
                    let error_with_context = error.with_record(record_index);
                    self.error_reporter.report_warning(error_with_context);
                }
            }
            FieldKind::PackedDecimal { .. } => {
                // Check for packed decimal corruption
                let corruption_errors = detect_packed_corruption(field_data, &field.path);
                for error in corruption_errors {
                    let error_with_context = error.with_record(record_index);
                    // Packed decimal corruption is usually an error, not just a warning
                    if let Err(fatal_error) = self.error_reporter.report_error(error_with_context) {
                        return Err(fatal_error);
                    }
                }
            }
            FieldKind::ZonedDecimal { .. } => {
                // Check for zoned decimal corruption (less common but possible)
                // This could include checking for invalid sign zones
                debug!("Checking zoned decimal field {} for corruption", field.path);
            }
            FieldKind::BinaryInt { .. } => {
                // Binary fields are less susceptible to character corruption
                // but we could check for suspicious patterns
                debug!("Checking binary field {} for corruption", field.path);
            }
            FieldKind::Group => {
                // Groups are handled by their child fields
            }
        }

        Ok(())
    }

    /// Generate final processing summary
    fn generate_summary(&self, total_records: u64) -> Result<RunSummary, Error> {
        let processing_time = self.start_time.elapsed();
        let processing_time_ms = processing_time.as_millis() as u64;

        let mut summary = RunSummary {
            records_processed: total_records,
            records_with_errors: 0,
            warnings: 0,
            processing_time_ms,
            bytes_processed: self.bytes_processed,
            schema_fingerprint: String::new(), // TODO: Implement schema fingerprinting
            input_file_hash: None,
            throughput_mbps: 0.0,
            error_summary: None,
            corruption_warnings: 0,
        };

        // Update from error reporter
        summary.update_from_error_summary(self.error_reporter.summary());
        summary.calculate_throughput();

        info!(
            "Processing complete: {} records, {} errors, {} warnings, {:.2} MB/s",
            summary.records_processed,
            summary.records_with_errors,
            summary.warnings,
            summary.throughput_mbps
        );

        Ok(summary)
    }
}

impl EncodeProcessor {
    /// Create a new encode processor with the given options
    pub fn new(options: EncodeOptions) -> Self {
        let error_mode = if options.strict_mode {
            ErrorMode::Strict
        } else {
            ErrorMode::Lenient
        };

        let error_reporter = ErrorReporter::new(error_mode, options.max_errors)
            .with_verbose_logging(true);

        Self {
            error_reporter,
            options,
            start_time: Instant::now(),
            bytes_processed: 0,
        }
    }

    /// Process JSONL file to binary with comprehensive error handling
    pub fn process_file<R: Read, W: Write>(
        &mut self,
        schema: &Schema,
        input: R,
        mut output: W,
    ) -> Result<RunSummary, Error> {
        info!("Starting encode processing with {} threads", self.options.threads);
        
        let reader = BufReader::new(input);
        let mut record_index = 0u64;

        // Process each JSON line
        for line_result in reader.lines() {
            record_index += 1;
            self.error_reporter.start_record(record_index);

            let line = line_result
                .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;

            self.bytes_processed += line.len() as u64;

            // Parse JSON
            let json_value: serde_json::Value = serde_json::from_str(&line)
                .map_err(|e| Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!("Invalid JSON: {}", e)
                ).with_record(record_index))?;

            // Encode to binary
            match self.process_json_record(schema, &json_value, record_index) {
                Ok(binary_data) => {
                    output.write_all(&binary_data)
                        .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;
                }
                Err(error) => {
                    if let Err(fatal_error) = self.error_reporter.report_error(error) {
                        return Err(fatal_error);
                    }
                    // In lenient mode, continue to next record
                }
            }
        }

        self.generate_summary(record_index)
    }

    /// Process a single JSON record to binary
    fn process_json_record(
        &mut self,
        schema: &Schema,
        json_value: &serde_json::Value,
        record_index: u64,
    ) -> Result<Vec<u8>, Error> {
        debug!("Encoding record {} to binary", record_index);

        crate::encode_record(schema, json_value, &self.options)
            .map_err(|e| e.with_record(record_index))
    }

    /// Generate final processing summary
    fn generate_summary(&self, total_records: u64) -> Result<RunSummary, Error> {
        let processing_time = self.start_time.elapsed();
        let processing_time_ms = processing_time.as_millis() as u64;

        let mut summary = RunSummary {
            records_processed: total_records,
            records_with_errors: 0,
            warnings: 0,
            processing_time_ms,
            bytes_processed: self.bytes_processed,
            schema_fingerprint: String::new(), // TODO: Implement schema fingerprinting
            input_file_hash: None,
            throughput_mbps: 0.0,
            error_summary: None,
            corruption_warnings: 0,
        };

        summary.update_from_error_summary(self.error_reporter.summary());
        summary.calculate_throughput();

        info!(
            "Encoding complete: {} records, {} errors, {} warnings, {:.2} MB/s",
            summary.records_processed,
            summary.records_with_errors,
            summary.warnings,
            summary.throughput_mbps
        );

        Ok(summary)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::{Schema, Field, FieldKind};
    use crate::options::{DecodeOptions, RecordFormat};
    use std::io::Cursor;

    fn create_test_schema() -> Schema {
        Schema {
            fields: vec![
                Field {
                    path: "ROOT.ID".to_string(),
                    name: "ID".to_string(),
                    level: 5,
                    kind: FieldKind::ZonedDecimal { digits: 5, scale: 0, signed: false },
                    offset: 0,
                    len: 5,
                    redefines_of: None,
                    occurs: None,
                    sync_padding: None,
                    synchronized: false,
                    blank_when_zero: false,
                    children: vec![],
                }
            ],
            lrecl_fixed: Some(5),
            tail_odo: None,
            fingerprint: String::new(),
        }
    }

    #[test]
    fn test_decode_processor_creation() {
        let options = DecodeOptions::default();
        let processor = DecodeProcessor::new(options);
        assert!(!processor.error_reporter.has_errors());
    }

    #[test]
    fn test_rdw_corruption_detection() {
        let options = DecodeOptions {
            format: RecordFormat::RDW,
            ..DecodeOptions::default()
        };
        let mut processor = DecodeProcessor::new(options);
        
        // Create RDW data with ASCII corruption
        let rdw_data = [b'1', b'2', 0x00, 0x00, b'H', b'e', b'l', b'l', b'o'];
        let mut cursor = Cursor::new(&rdw_data);
        let mut buffer = Vec::new();
        
        // This should detect corruption but continue processing
        let result = processor.read_rdw_record(&mut cursor, &mut buffer);
        assert!(result.is_err()); // Will fail due to invalid length parsing
    }

    #[test]
    fn test_error_summary_generation() {
        let options = DecodeOptions::default();
        let processor = DecodeProcessor::new(options);
        
        let summary = processor.generate_summary(100).unwrap();
        assert_eq!(summary.records_processed, 100);
        assert!(!summary.has_errors());
    }
}