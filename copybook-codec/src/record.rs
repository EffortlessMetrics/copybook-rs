//! Record framing and I/O utilities
//!
//! This module handles fixed-length and RDW variable-length record processing.

use crate::options::RecordFormat;
use copybook_core::{Error, ErrorCode, Result, Schema};
use copybook_core::error::ErrorContext;
use std::io::{Read, Write, ErrorKind};
use crate::options::RawMode;
use tracing::{debug, warn};

/// Fixed record reader for processing fixed-length records
#[derive(Debug)]
pub struct FixedRecordReader<R: Read> {
    input: R,
    lrecl: u32,
    record_count: u64,
}

impl<R: Read> FixedRecordReader<R> {
    /// Create a new fixed record reader
    /// 
    /// # Errors
    /// 
    /// Returns an error if LRECL is not provided or is zero
    pub fn new(input: R, lrecl: Option<u32>) -> Result<Self> {
        let lrecl = lrecl.ok_or_else(|| {
            Error::new(ErrorCode::CBKP001_SYNTAX, "Fixed format requires LRECL")
        })?;
        
        if lrecl == 0 {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "LRECL must be greater than zero"
            ));
        }

        Ok(Self {
            input,
            lrecl,
            record_count: 0,
        })
    }

    /// Read the next record
    /// 
    /// # Errors
    /// 
    /// Returns an error if the record cannot be read due to I/O errors
    pub fn read_record(&mut self) -> Result<Option<Vec<u8>>> {
        // Try to read one byte first to check for EOF
        let mut first_byte = [0u8; 1];
        match self.input.read_exact(&mut first_byte) {
            Ok(()) => {
                // We got the first byte, now read the rest
                let mut buffer = vec![0u8; self.lrecl as usize];
                buffer[0] = first_byte[0];
                
                if self.lrecl > 1 {
                    match self.input.read_exact(&mut buffer[1..]) {
                        Ok(()) => {
                            self.record_count += 1;
                            debug!("Read fixed record {} of {} bytes", self.record_count, self.lrecl);
                            Ok(Some(buffer))
                        }
                        Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                            // Partial record - this is an error
                            Err(Error::new(
                                ErrorCode::CBKR221_RDW_UNDERFLOW,
                                format!("Incomplete record at end of file: expected {} bytes", self.lrecl)
                            ).with_context(ErrorContext {
                                record_index: Some(self.record_count + 1),
                                field_path: None,
                                byte_offset: None,
                                line_number: None,
                                details: Some("File ends with partial record".to_string()),
                            }))
                        }
                        Err(e) => {
                            Err(Error::new(
                                ErrorCode::CBKR201_RDW_READ_ERROR,
                                format!("I/O error reading record: {e}")
                            ).with_context(ErrorContext {
                                record_index: Some(self.record_count + 1),
                                field_path: None,
                                byte_offset: None,
                                line_number: None,
                                details: None,
                            }))
                        }
                    }
                } else {
                    // LRECL is 1, we already have the complete record
                    self.record_count += 1;
                    debug!("Read fixed record {} of {} bytes", self.record_count, self.lrecl);
                    Ok(Some(buffer))
                }
            }
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                // True EOF - no more data
                debug!("Reached EOF after {} records", self.record_count);
                Ok(None)
            }
            Err(e) => {
                Err(Error::new(
                    ErrorCode::CBKR201_RDW_READ_ERROR,
                    format!("I/O error reading record: {e}")
                ).with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: None,
                    line_number: None,
                    details: None,
                }))
            }
        }
    }

    /// Validate record length against schema
    /// 
    /// # Errors
    /// 
    /// Returns an error if the record length doesn't match schema expectations
    pub fn validate_record_length(&self, schema: &Schema, record_data: &[u8]) -> Result<()> {
        // For fixed records, the data length should match LRECL
        if record_data.len() != self.lrecl as usize {
            return Err(Error::new(
                ErrorCode::CBKR221_RDW_UNDERFLOW,
                format!("Record length mismatch: expected {}, got {}", self.lrecl, record_data.len())
            ).with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("Fixed record length validation failed".to_string()),
            }));
        }

        // If schema has a fixed LRECL, validate against it
        if let Some(schema_lrecl) = schema.lrecl_fixed {
            if self.lrecl != schema_lrecl {
                warn!(
                    "LRECL mismatch: reader configured for {}, schema expects {}",
                    self.lrecl, schema_lrecl
                );
            }
        }

        // For ODO tail records, the actual record might be shorter than LRECL
        if schema.tail_odo.is_some() {
            debug!("Record has ODO tail, variable length within fixed LRECL is expected");
        }

        Ok(())
    }

    /// Get the current record count
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Get the configured LRECL
    #[must_use]
    pub fn lrecl(&self) -> u32 {
        self.lrecl
    }
}

/// Fixed record writer for writing fixed-length records
#[derive(Debug)]
pub struct FixedRecordWriter<W: Write> {
    output: W,
    lrecl: u32,
    record_count: u64,
}

impl<W: Write> FixedRecordWriter<W> {
    /// Create a new fixed record writer
    /// 
    /// # Errors
    /// 
    /// Returns an error if LRECL is not provided or is zero
    pub fn new(output: W, lrecl: Option<u32>) -> Result<Self> {
        let lrecl = lrecl.ok_or_else(|| {
            Error::new(ErrorCode::CBKP001_SYNTAX, "Fixed format requires LRECL")
        })?;
        
        if lrecl == 0 {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "LRECL must be greater than zero"
            ));
        }

        Ok(Self {
            output,
            lrecl,
            record_count: 0,
        })
    }

    /// Write a record with proper padding
    /// 
    /// # Errors
    /// 
    /// Returns an error if the record cannot be written due to I/O errors
    pub fn write_record(&mut self, data: &[u8]) -> Result<()> {
        let data_len = data.len();
        let lrecl = self.lrecl as usize;

        if data_len > lrecl {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Record too long: {} bytes exceeds LRECL of {}", data_len, lrecl)
            ).with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("Record exceeds fixed length".to_string()),
            }));
        }

        // Write the data
        self.output.write_all(data).map_err(|e| {
            Error::new(
                ErrorCode::CBKR201_RDW_READ_ERROR,
                format!("I/O error writing record: {e}")
            ).with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: None,
            })
        })?;

        // Pad with zeros if necessary
        if data_len < lrecl {
            let padding = vec![0u8; lrecl - data_len];
            self.output.write_all(&padding).map_err(|e| {
                Error::new(
                    ErrorCode::CBKR201_RDW_READ_ERROR,
                    format!("I/O error writing padding: {e}")
                ).with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(data_len as u64),
                    line_number: None,
                    details: Some("Error writing record padding".to_string()),
                })
            })?;
        }

        self.record_count += 1;
        debug!("Wrote fixed record {} of {} bytes (data: {}, padding: {})", 
               self.record_count, lrecl, data_len, lrecl - data_len);

        Ok(())
    }

    /// Flush the output
    /// 
    /// # Errors
    /// 
    /// Returns an error if the flush operation fails
    pub fn flush(&mut self) -> Result<()> {
        self.output.flush().map_err(|e| {
            Error::new(
                ErrorCode::CBKR201_RDW_READ_ERROR,
                format!("I/O error flushing output: {e}")
            )
        })
    }

    /// Get the current record count
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Get the configured LRECL
    #[must_use]
    pub fn lrecl(&self) -> u32 {
        self.lrecl
    }
}

/// RDW parsing mode for handling different scenarios
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RDWParsingMode {
    /// Strict mode: Non-zero reserved bytes are errors
    Strict,
    /// Lenient mode: Non-zero reserved bytes are warnings  
    Lenient,
    /// Permissive mode: Non-zero reserved bytes are ignored
    Permissive,
}

/// RDW (Record Descriptor Word) record reader for processing variable-length records
#[derive(Debug)]
pub struct RDWRecordReader<R: Read> {
    input: R,
    record_count: u64,
    parsing_mode: RDWParsingMode,
    /// Maximum allowed record length for overflow detection
    max_record_length: Option<u32>,
}

impl<R: Read> RDWRecordReader<R> {
    /// Create a new RDW record reader with parsing mode
    pub fn new_with_mode(input: R, parsing_mode: RDWParsingMode) -> Self {
        Self {
            input,
            record_count: 0,
            parsing_mode,
            max_record_length: None,
        }
    }
    
    /// Create a new RDW record reader (legacy interface for backward compatibility)
    pub fn new(input: R, strict_mode: bool) -> Self {
        let parsing_mode = if strict_mode { 
            RDWParsingMode::Strict 
        } else { 
            RDWParsingMode::Lenient 
        };
        Self::new_with_mode(input, parsing_mode)
    }
    
    /// Set maximum allowed record length for overflow detection
    pub fn set_max_record_length(&mut self, max_length: u32) {
        self.max_record_length = Some(max_length);
    }

    /// Read the next RDW record
    /// 
    /// # Errors
    /// 
    /// Returns an error if the record cannot be read due to I/O errors or format issues
    pub fn read_record(&mut self) -> Result<Option<RDWRecord>> {
        // Read the 4-byte RDW header
        let mut rdw_header = [0u8; 4];
        match self.input.read_exact(&mut rdw_header) {
            Ok(()) => {
                // Parse RDW header
                let length = u16::from_be_bytes([rdw_header[0], rdw_header[1]]) as u32;
                let reserved = u16::from_be_bytes([rdw_header[2], rdw_header[3]]);

                self.record_count += 1;
                debug!("Read RDW header for record {}: length={}, reserved={:04X}", 
                       self.record_count, length, reserved);

                // Check for ASCII transfer corruption heuristic FIRST (before other validations)
                if RDWRecord::is_suspect_ascii_corruption(&rdw_header) {
                    let corruption_error = Error::new(
                        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                        format!("RDW appears to be ASCII-corrupted: {:02X} {:02X} {:02X} {:02X}", 
                              rdw_header[0], rdw_header[1], rdw_header[2], rdw_header[3])
                    ).with_context(ErrorContext {
                        record_index: Some(self.record_count),
                        field_path: None,
                        byte_offset: Some(0),
                        line_number: None,
                        details: Some("RDW header bytes look like ASCII digits".to_string()),
                    });
                    
                    return Err(corruption_error); // Always fail immediately for ASCII corruption
                }

                // Validate reserved bytes based on parsing mode
                if reserved != 0 {
                    let error = Error::new(
                        ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
                        format!("RDW reserved bytes are non-zero: {:04X}", reserved)
                    ).with_context(ErrorContext {
                        record_index: Some(self.record_count),
                        field_path: None,
                        byte_offset: Some(2), // Reserved bytes are at offset 2-3
                        line_number: None,
                        details: Some(format!("Expected 0000, got {:04X}", reserved)),
                    });

                    match self.parsing_mode {
                        RDWParsingMode::Strict => return Err(error),
                        RDWParsingMode::Lenient => {
                            warn!("RDW reserved bytes non-zero (record {}): {:04X}", self.record_count, reserved);
                        },
                        RDWParsingMode::Permissive => {
                            debug!("RDW reserved bytes non-zero (record {}, ignored): {:04X}", self.record_count, reserved);
                        },
                    }
                }

                // Validate record length for overflow
                if let Some(max_length) = self.max_record_length {
                    if length > max_length {
                        return Err(Error::new(
                            ErrorCode::CBKR222_RDW_OVERFLOW,
                            format!("RDW record length {} exceeds maximum {}", length, max_length)
                        ).with_context(ErrorContext {
                            record_index: Some(self.record_count),
                            field_path: None,
                            byte_offset: Some(0), // Length bytes are at offset 0-1
                            line_number: None,
                            details: Some(format!("Record length validation failed")),
                        }));
                    }
                }


                // Handle zero-length records
                if length == 0 {
                    debug!("Zero-length RDW record {}", self.record_count);
                    return Ok(Some(RDWRecord {
                        header: rdw_header,
                        payload: Vec::new(),
                    }));
                }

                // Read the payload with enhanced error handling
                match self.read_payload_with_recovery(length as u16) {
                    Ok(payload) => {
                        debug!("Read RDW record {} payload: {} bytes", self.record_count, length);
                        Ok(Some(RDWRecord {
                            header: rdw_header,
                            payload,
                        }))
                    }
                    Err(e) => Err(e),
                }
            }
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                // True EOF - no more data
                debug!("Reached EOF after {} RDW records", self.record_count);
                Ok(None)
            }
            Err(e) => {
                Err(Error::new(
                    ErrorCode::CBKR201_RDW_READ_ERROR,
                    format!("I/O error reading RDW header: {e}")
                ).with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: None,
                    line_number: None,
                    details: None,
                }))
            }
        }
    }

    /// Validate record length against schema requirements
    /// 
    /// # Errors
    /// 
    /// Returns an error if the record length is invalid for the schema
    pub fn validate_record_length_against_schema(&self, schema: &Schema, record_length: u32) -> Result<()> {
        // Check minimum record size based on schema fixed fields
        let min_size = Self::calculate_schema_minimum_size(schema);
        
        if record_length < min_size {
            return Err(Error::new(
                ErrorCode::CBKR221_RDW_UNDERFLOW,
                format!("RDW record length {} is less than schema minimum {} bytes", record_length, min_size)
            ).with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: Some(0), // Length is at the beginning of RDW
                line_number: None,
                details: Some(format!("Schema requires minimum {} bytes", min_size)),
            }));
        }

        // For RDW records, schema.lrecl_fixed represents minimum required length
        // Records can be longer than this for variable-length data
        // Only check maximum in strict mode and only for unreasonable sizes
        
        // Note: For true fixed-length validation, this should be called differently
        // For RDW, we're primarily checking minimum requirements

        Ok(())
    }
    
    /// Validate zero-length record against schema
    /// 
    /// # Errors
    /// 
    /// Returns an error if zero-length record is invalid for the schema
    pub fn validate_zero_length_record(&self, schema: &Schema) -> Result<()> {
        self.validate_record_length_against_schema(schema, 0)
    }

    /// Calculate the minimum size required by a schema
    fn calculate_schema_minimum_size(schema: &Schema) -> u32 {
        // Use schema's fixed record length if available
        if let Some(lrecl) = schema.lrecl_fixed {
            return lrecl;
        }
        
        // Otherwise calculate from field layout
        Self::calculate_fields_minimum_size(&schema.fields)
    }
    
    fn calculate_fields_minimum_size(fields: &[copybook_core::Field]) -> u32 {
        let mut total_size = 0u32;
        
        for field in fields {
            // For groups, recursively calculate child fields
            if !field.children.is_empty() {
                total_size = total_size.max(field.offset + Self::calculate_fields_minimum_size(&field.children));
            } else {
                // For leaf fields, add offset + length
                total_size = total_size.max(field.offset + field.len);
            }
        }
        
        total_size
    }


    /// Read payload data with enhanced error recovery
    fn read_payload_with_recovery(&mut self, expected_length: u16) -> Result<Vec<u8>> {
        if expected_length == 0 {
            return Ok(Vec::new());
        }
        
        let mut payload = vec![0u8; expected_length as usize];
        
        match self.input.read_exact(&mut payload) {
            Ok(()) => Ok(payload),
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                // Enhanced error reporting for partial reads
                Err(Error::new(
                    ErrorCode::CBKR221_RDW_UNDERFLOW,
                    format!("Incomplete RDW record payload: expected {} bytes", expected_length)
                ).with_context(ErrorContext {
                    record_index: Some(self.record_count),
                    field_path: None,
                    byte_offset: Some(4), // Payload starts after 4-byte header
                    line_number: None,
                    details: Some(format!("File ends with incomplete RDW payload (record {})", self.record_count)),
                }))
            }
            Err(e) => {
                // Use more specific error code for I/O errors
                Err(Error::new(
                    ErrorCode::CBKR201_RDW_READ_ERROR,
                    format!("I/O error reading RDW payload: {e}")
                ).with_context(ErrorContext {
                    record_index: Some(self.record_count),
                    field_path: None,
                    byte_offset: Some(4),
                    line_number: None,
                    details: Some(format!("Failed to read {} bytes for record {}", expected_length, self.record_count)),
                }))
            }
        }
    }

    /// Get the current record count
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }
    
    /// Get the current parsing mode
    #[must_use]
    pub fn parsing_mode(&self) -> RDWParsingMode {
        self.parsing_mode
    }
    
    /// Get the maximum record length limit
    #[must_use]
    pub fn max_record_length(&self) -> Option<u32> {
        self.max_record_length
    }
}

/// RDW record writer for writing variable-length records
#[derive(Debug)]
pub struct RDWRecordWriter<W: Write> {
    output: W,
    record_count: u64,
}

impl<W: Write> RDWRecordWriter<W> {
    /// Create a new RDW record writer
    pub fn new(output: W) -> Self {
        Self {
            output,
            record_count: 0,
        }
    }

    /// Write an RDW record
    /// 
    /// # Errors
    /// 
    /// Returns an error if the record cannot be written due to I/O errors
    pub fn write_record(&mut self, record: &RDWRecord) -> Result<()> {
        // Write the RDW header
        self.output.write_all(&record.header).map_err(|e| {
            Error::new(
                ErrorCode::CBKR201_RDW_READ_ERROR,
                format!("I/O error writing RDW header: {e}")
            ).with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: None,
            })
        })?;

        // Write the payload
        self.output.write_all(&record.payload).map_err(|e| {
            Error::new(
                ErrorCode::CBKR201_RDW_READ_ERROR,
                format!("I/O error writing RDW payload: {e}")
            ).with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: Some(4),
                line_number: None,
                details: None,
            })
        })?;

        self.record_count += 1;
        debug!("Wrote RDW record {} with {} byte payload", 
               self.record_count, record.payload.len());

        Ok(())
    }

    /// Write a record from payload data, generating the RDW header
    /// 
    /// # Errors
    /// 
    /// Returns an error if the record cannot be written due to I/O errors
    pub fn write_record_from_payload(&mut self, payload: &[u8], preserve_reserved: Option<u16>) -> Result<()> {
        let length = payload.len();
        
        if length > u16::MAX as usize {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("RDW payload too large: {} bytes exceeds maximum of {}", length, u16::MAX)
            ).with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("RDW length field is 16-bit".to_string()),
            }));
        }

        // Create RDW header
        let length_bytes = (length as u16).to_be_bytes();
        let reserved_bytes = preserve_reserved.unwrap_or(0).to_be_bytes();
        let header = [length_bytes[0], length_bytes[1], reserved_bytes[0], reserved_bytes[1]];

        let record = RDWRecord {
            header,
            payload: payload.to_vec(),
        };

        self.write_record(&record)
    }

    /// Flush the output
    /// 
    /// # Errors
    /// 
    /// Returns an error if the flush operation fails
    pub fn flush(&mut self) -> Result<()> {
        self.output.flush().map_err(|e| {
            Error::new(
                ErrorCode::CBKR201_RDW_READ_ERROR,
                format!("I/O error flushing output: {e}")
            )
        })
    }

    /// Get the current record count
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }
}

/// An RDW record with header and payload
#[derive(Debug, Clone)]
pub struct RDWRecord {
    /// 4-byte RDW header (length + reserved)
    pub header: [u8; 4],
    /// Record payload data
    pub payload: Vec<u8>,
}

impl RDWRecord {
    /// Create a new RDW record from payload
    #[must_use]
    pub fn new(payload: Vec<u8>) -> Self {
        let length = payload.len().min(u16::MAX as usize) as u16;
        let length_bytes = length.to_be_bytes();
        let header = [length_bytes[0], length_bytes[1], 0, 0]; // Reserved bytes are zero
        
        Self { header, payload }
    }

    /// Create a new RDW record with preserved reserved bytes
    #[must_use]
    pub fn with_reserved(payload: Vec<u8>, reserved: u16) -> Self {
        let length = payload.len().min(u16::MAX as usize) as u16;
        let length_bytes = length.to_be_bytes();
        let reserved_bytes = reserved.to_be_bytes();
        let header = [length_bytes[0], length_bytes[1], reserved_bytes[0], reserved_bytes[1]];
        
        Self { header, payload }
    }

    /// Get the length from the RDW header
    #[must_use]
    pub fn length(&self) -> u16 {
        u16::from_be_bytes([self.header[0], self.header[1]])
    }

    /// Get the reserved bytes from the RDW header
    #[must_use]
    pub fn reserved(&self) -> u16 {
        u16::from_be_bytes([self.header[2], self.header[3]])
    }

    /// Get the 4-byte RDW header as bytes
    #[must_use]
    pub fn header_bytes(&self) -> [u8; 4] {
        self.header
    }

    /// Update the length field to match the payload size
    pub fn recompute_length(&mut self) {
        let length = self.payload.len().min(u16::MAX as usize) as u16;
        let length_bytes = length.to_be_bytes();
        self.header[0] = length_bytes[0];
        self.header[1] = length_bytes[1];
    }

    /// Get the complete record data (header + payload)
    #[must_use]
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(4 + self.payload.len());
        result.extend_from_slice(&self.header);
        result.extend_from_slice(&self.payload);
        result
    }

    /// Get the record data for specific raw modes
    #[must_use]
    pub fn get_data_for_raw_mode(&self, raw_mode: RawMode) -> Vec<u8> {
        match raw_mode {
            RawMode::RecordRDW => self.as_bytes(), // Include RDW header
            _ => self.payload.clone(), // Just payload
        }
    }

    /// Get detailed RDW metadata for diagnostics
    #[must_use]
    pub fn get_metadata(&self) -> RDWMetadata {
        RDWMetadata {
            length: self.length(),
            reserved: self.reserved(),
            payload_size: self.payload.len(),
            header_bytes: self.header,
            has_non_zero_reserved: self.reserved() != 0,
            suspect_ascii_corruption: Self::is_suspect_ascii_corruption(&self.header),
        }
    }
    
    /// Check if this record has suspected ASCII corruption
    #[must_use]
    pub fn is_suspect_ascii_corruption(header: &[u8; 4]) -> bool {
        // Heuristic: if the length bytes look like ASCII digits, it might be corrupted
        // ASCII digits are 0x30-0x39
        let length_bytes = [header[0], header[1]];
        
        // Check if both length bytes are ASCII digits
        length_bytes.iter().all(|&b| (0x30..=0x39).contains(&b))
    }

    /// Validate the RDW record for consistency
    #[must_use]
    pub fn validate_consistency(&self) -> Vec<String> {
        let mut issues = Vec::new();
        
        // Check if length matches payload size
        if self.length() as usize != self.payload.len() {
            issues.push(format!(
                "Length mismatch: header claims {} bytes, payload is {} bytes",
                self.length(),
                self.payload.len()
            ));
        }
        
        // Check for non-zero reserved bytes
        if self.reserved() != 0 {
            issues.push(format!("Non-zero reserved bytes: {:04X}", self.reserved()));
        }
        
        // Check for suspected ASCII corruption
        if Self::is_suspect_ascii_corruption(&self.header) {
            issues.push("Suspected ASCII corruption in length bytes".to_string());
        }
        
        issues
    }
}

/// RDW metadata for detailed diagnostics and raw preservation
#[derive(Debug, Clone, PartialEq)]
pub struct RDWMetadata {
    /// Length field from RDW header
    pub length: u16,
    /// Reserved bytes from RDW header
    pub reserved: u16,
    /// Actual payload size
    pub payload_size: usize,
    /// Complete RDW header bytes
    pub header_bytes: [u8; 4],
    /// Whether reserved bytes are non-zero
    pub has_non_zero_reserved: bool,
    /// Whether length bytes look like ASCII corruption
    pub suspect_ascii_corruption: bool,
}

/// Read a single record from input (legacy interface)
/// 
/// # Errors
/// 
/// Returns an error if the record cannot be read due to I/O errors or format issues
pub fn read_record(
    input: &mut impl Read,
    format: RecordFormat,
    lrecl: Option<u32>,
) -> Result<Option<Vec<u8>>> {
    match format {
        RecordFormat::Fixed => {
            let mut reader = FixedRecordReader::new(input, lrecl)?;
            reader.read_record()
        }
        RecordFormat::RDW => {
            let mut reader = RDWRecordReader::new(input, false); // Default to lenient mode
            match reader.read_record()? {
                Some(rdw_record) => Ok(Some(rdw_record.payload)),
                None => Ok(None),
            }
        }
    }
}

/// Write a single record to output (legacy interface)
/// 
/// # Errors
/// 
/// Returns an error if the record cannot be written due to I/O errors
pub fn write_record(output: &mut impl Write, data: &[u8], format: RecordFormat) -> Result<()> {
    match format {
        RecordFormat::Fixed => {
            // For legacy interface, we don't know LRECL, so just write the data as-is
            output.write_all(data).map_err(|e| {
                Error::new(
                    ErrorCode::CBKR201_RDW_READ_ERROR,
                    format!("Write error: {e}"),
                )
            })?;
        }
        RecordFormat::RDW => {
            let mut writer = RDWRecordWriter::new(output);
            writer.write_record_from_payload(data, None)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::Schema;
    use std::io::Cursor;

    #[test]
    fn test_fixed_record_reader_basic() {
        let data = b"ABCD1234EFGH5678";
        let mut reader = FixedRecordReader::new(Cursor::new(data), Some(8)).unwrap();
        
        // Read first record
        let record1 = reader.read_record().unwrap().unwrap();
        assert_eq!(record1, b"ABCD1234");
        assert_eq!(reader.record_count(), 1);
        
        // Read second record
        let record2 = reader.read_record().unwrap().unwrap();
        assert_eq!(record2, b"EFGH5678");
        assert_eq!(reader.record_count(), 2);
        
        // EOF
        let record3 = reader.read_record().unwrap();
        assert!(record3.is_none());
    }

    #[test]
    fn test_fixed_record_reader_partial_record() {
        let data = b"ABCD123"; // 7 bytes, but LRECL is 8
        let mut reader = FixedRecordReader::new(Cursor::new(data), Some(8)).unwrap();
        
        // Should get an error for incomplete record
        let result = reader.read_record();
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR221_RDW_UNDERFLOW);
    }

    #[test]
    fn test_fixed_record_reader_zero_lrecl() {
        let data = b"test";
        let result = FixedRecordReader::new(Cursor::new(data), Some(0));
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP001_SYNTAX);
    }

    #[test]
    fn test_fixed_record_reader_no_lrecl() {
        let data = b"test";
        let result = FixedRecordReader::new(Cursor::new(data), None);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP001_SYNTAX);
    }

    #[test]
    fn test_fixed_record_writer_basic() {
        let mut output = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut output, Some(8)).unwrap();
        
        // Write first record (exact length)
        writer.write_record(b"ABCD1234").unwrap();
        assert_eq!(writer.record_count(), 1);
        
        // Write second record (shorter, should be padded)
        writer.write_record(b"XYZ").unwrap();
        assert_eq!(writer.record_count(), 2);
        
        writer.flush().unwrap();
        
        // Check output
        assert_eq!(output, b"ABCD1234XYZ\x00\x00\x00\x00\x00");
    }

    #[test]
    fn test_fixed_record_writer_too_long() {
        let mut output = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
        
        // Try to write record longer than LRECL
        let result = writer.write_record(b"ABCDEFGH");
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    }

    #[test]
    fn test_fixed_record_writer_zero_lrecl() {
        let mut output = Vec::new();
        let result = FixedRecordWriter::new(&mut output, Some(0));
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP001_SYNTAX);
    }

    #[test]
    fn test_fixed_record_writer_no_lrecl() {
        let mut output = Vec::new();
        let result = FixedRecordWriter::new(&mut output, None);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP001_SYNTAX);
    }

    #[test]
    fn test_validate_record_length() {
        let data = b"ABCD1234";
        let mut reader = FixedRecordReader::new(Cursor::new(data), Some(8)).unwrap();
        let record = reader.read_record().unwrap().unwrap();
        
        let schema = Schema::new();
        let result = reader.validate_record_length(&schema, &record);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_record_length_mismatch() {
        let data = b"ABCD1234";
        let mut reader = FixedRecordReader::new(Cursor::new(data), Some(8)).unwrap();
        let _record = reader.read_record().unwrap().unwrap();
        
        let schema = Schema::new();
        // Simulate wrong length record
        let wrong_record = b"ABC";
        let result = reader.validate_record_length(&schema, wrong_record);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR221_RDW_UNDERFLOW);
    }

    #[test]
    fn test_legacy_read_record_fixed() {
        let mut data = Cursor::new(b"ABCD1234");
        let record = read_record(&mut data, RecordFormat::Fixed, Some(8)).unwrap().unwrap();
        assert_eq!(record, b"ABCD1234");
    }

    #[test]
    fn test_legacy_read_record_rdw_invalid_header() {
        // Test with invalid RDW data (not enough bytes for header)
        let mut data = Cursor::new(b"te");
        let result = read_record(&mut data, RecordFormat::RDW, None);
        assert!(result.is_ok());
        assert!(result.unwrap().is_none()); // Should be EOF due to incomplete header
    }

    #[test]
    fn test_legacy_write_record_fixed() {
        let mut output = Vec::new();
        write_record(&mut output, b"ABCD1234", RecordFormat::Fixed).unwrap();
        assert_eq!(output, b"ABCD1234");
    }

    #[test]
    fn test_legacy_write_record_rdw() {
        let mut output = Vec::new();
        write_record(&mut output, b"test", RecordFormat::RDW).unwrap();
        
        // Should have 4-byte header + 4-byte payload
        assert_eq!(output.len(), 8);
        
        // Check RDW header: length=4, reserved=0
        assert_eq!(output[0..2], [0, 4]); // Big-endian length
        assert_eq!(output[2..4], [0, 0]); // Reserved bytes
        assert_eq!(output[4..8], *b"test"); // Payload
    }

    #[test]
    fn test_rdw_record_new() {
        let record = RDWRecord::new(b"hello".to_vec());
        assert_eq!(record.length(), 5);
        assert_eq!(record.reserved(), 0);
        assert_eq!(record.payload, b"hello");
    }

    #[test]
    fn test_rdw_record_with_reserved() {
        let record = RDWRecord::with_reserved(b"test".to_vec(), 0x1234);
        assert_eq!(record.length(), 4);
        assert_eq!(record.reserved(), 0x1234);
        assert_eq!(record.payload, b"test");
    }

    #[test]
    fn test_rdw_record_recompute_length() {
        let mut record = RDWRecord::new(b"test".to_vec());
        record.payload = b"longer_payload".to_vec();
        record.recompute_length();
        assert_eq!(record.length(), 14);
    }

    #[test]
    fn test_rdw_record_as_bytes() {
        let record = RDWRecord::new(b"hi".to_vec());
        let bytes = record.as_bytes();
        assert_eq!(bytes, vec![0, 2, 0, 0, b'h', b'i']);
    }

    #[test]
    fn test_rdw_record_get_data_for_raw_mode() {
        let record = RDWRecord::new(b"test".to_vec());
        
        // Record+RDW mode includes header
        let with_header = record.get_data_for_raw_mode(RawMode::RecordRDW);
        assert_eq!(with_header, vec![0, 4, 0, 0, b't', b'e', b's', b't']);
        
        // Other modes just return payload
        let payload_only = record.get_data_for_raw_mode(RawMode::Record);
        assert_eq!(payload_only, b"test");
    }

    #[test]
    fn test_rdw_reader_basic() {
        // Create test data: RDW header (length=5, reserved=0) + payload "hello"
        let data = vec![0, 5, 0, 0, b'h', b'e', b'l', b'l', b'o'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.length(), 5);
        assert_eq!(record.reserved(), 0);
        assert_eq!(record.payload, b"hello");
        assert_eq!(reader.record_count(), 1);
    }

    #[test]
    fn test_rdw_reader_multiple_records() {
        // Two records: "hi" (length=2) and "bye" (length=3)
        let data = vec![
            0, 2, 0, 0, b'h', b'i',           // First record
            0, 3, 0, 0, b'b', b'y', b'e'      // Second record
        ];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        
        // Read first record
        let record1 = reader.read_record().unwrap().unwrap();
        assert_eq!(record1.payload, b"hi");
        assert_eq!(reader.record_count(), 1);
        
        // Read second record
        let record2 = reader.read_record().unwrap().unwrap();
        assert_eq!(record2.payload, b"bye");
        assert_eq!(reader.record_count(), 2);
        
        // EOF
        let record3 = reader.read_record().unwrap();
        assert!(record3.is_none());
    }

    #[test]
    fn test_rdw_reader_zero_length() {
        // Zero-length record
        let data = vec![0, 0, 0, 0]; // Length=0, reserved=0, no payload
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.length(), 0);
        assert_eq!(record.payload.len(), 0);
    }

    #[test]
    fn test_rdw_reader_reserved_nonzero_lenient() {
        // Record with non-zero reserved bytes
        let data = vec![0, 4, 0x12, 0x34, b't', b'e', b's', b't'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false); // Lenient mode
        
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.reserved(), 0x1234);
        assert_eq!(record.payload, b"test");
    }

    #[test]
    fn test_rdw_reader_reserved_nonzero_strict() {
        // Record with non-zero reserved bytes in strict mode
        let data = vec![0, 4, 0x12, 0x34, b't', b'e', b's', b't'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), true); // Strict mode
        
        let result = reader.read_record();
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
    }

    #[test]
    fn test_rdw_reader_incomplete_header() {
        // Incomplete RDW header (only 2 bytes)
        let data = vec![0, 4];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        
        let result = reader.read_record();
        assert!(result.is_ok());
        assert!(result.unwrap().is_none()); // Should be EOF
    }

    #[test]
    fn test_rdw_reader_incomplete_payload() {
        // Complete header but incomplete payload
        let data = vec![0, 5, 0, 0, b'h', b'i']; // Says length=5 but only 2 payload bytes
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        
        let result = reader.read_record();
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR221_RDW_UNDERFLOW);
    }

    #[test]
    fn test_rdw_reader_ascii_corruption_detection() {
        // RDW header that looks like ASCII digits (suspicious) but with a reasonable length
        // Use '0' '4' which gives length 0x3034 = 12340, still too large for our test data
        // Let's use a smaller example: '0' '8' = 0x3038 = 12344, still too large
        // Better: use bytes that look like ASCII but give a reasonable length
        let data = vec![0, 8, b'3', b'4', b't', b'e', b's', b't', b'x', b'x', b'x', b'x'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        
        // Should read the record successfully (length=8, reserved=0x3334)
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.length(), 8);
        assert_eq!(record.reserved(), 0x3334); // '3' '4' as big-endian
        assert_eq!(record.payload, b"testxxxx");
        
        // The ASCII corruption detection is based on the length bytes being ASCII digits
        // In this case, the length bytes are 0, 8 which are not ASCII digits, so no warning
        // Let's test the actual detection logic separately
        assert!(!RDWRecord::is_suspect_ascii_corruption(&[0, 8, b'3', b'4']));
        assert!(RDWRecord::is_suspect_ascii_corruption(&[b'1', b'2', 0, 0]));
    }

    #[test]
    fn test_rdw_writer_basic() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        
        let record = RDWRecord::new(b"test".to_vec());
        writer.write_record(&record).unwrap();
        
        assert_eq!(writer.record_count(), 1);
        assert_eq!(output, vec![0, 4, 0, 0, b't', b'e', b's', b't']);
    }

    #[test]
    fn test_rdw_writer_from_payload() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        
        writer.write_record_from_payload(b"hello", None).unwrap();
        
        assert_eq!(writer.record_count(), 1);
        assert_eq!(output, vec![0, 5, 0, 0, b'h', b'e', b'l', b'l', b'o']);
    }

    #[test]
    fn test_rdw_writer_preserve_reserved() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        
        writer.write_record_from_payload(b"test", Some(0x1234)).unwrap();
        
        assert_eq!(output, vec![0, 4, 0x12, 0x34, b't', b'e', b's', b't']);
    }

    #[test]
    fn test_rdw_writer_payload_too_large() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        
        // Create payload larger than u16::MAX
        let large_payload = vec![0u8; (u16::MAX as usize) + 1];
        let result = writer.write_record_from_payload(&large_payload, None);
        
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    }

    #[test]
    fn test_legacy_read_record_rdw() {
        let data = vec![0, 4, 0, 0, b't', b'e', b's', b't'];
        let mut cursor = Cursor::new(data);
        
        let record = read_record(&mut cursor, RecordFormat::RDW, None).unwrap().unwrap();
        assert_eq!(record, b"test"); // Should return just the payload
    }
}