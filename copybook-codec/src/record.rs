//! Record framing and I/O utilities
//!
//! This module handles fixed-length and RDW variable-length record processing.

use crate::options::RawMode;
use crate::options::RecordFormat;
use copybook_core::error::ErrorContext;
use copybook_core::{Error, ErrorCode, Result, Schema};
use std::io::{ErrorKind, Read, Write};
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
        let lrecl = lrecl
            .ok_or_else(|| Error::new(ErrorCode::CBKP001_SYNTAX, "Fixed format requires LRECL"))?;

        if lrecl == 0 {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "LRECL must be greater than zero",
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
                            debug!(
                                "Read fixed record {} of {} bytes",
                                self.record_count, self.lrecl
                            );
                            Ok(Some(buffer))
                        }
                        Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                            // Partial record - this is an error
                            Err(Error::new(
                                ErrorCode::CBKR221_RDW_UNDERFLOW,
                                format!(
                                    "Incomplete record at end of file: expected {} bytes",
                                    self.lrecl
                                ),
                            )
                            .with_context(ErrorContext {
                                record_index: Some(self.record_count + 1),
                                field_path: None,
                                byte_offset: None,
                                line_number: None,
                                details: Some("File ends with partial record".to_string()),
                            }))
                        }
                        Err(e) => Err(Error::new(
                            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                            format!("I/O error reading record: {e}"),
                        )
                        .with_context(ErrorContext {
                            record_index: Some(self.record_count + 1),
                            field_path: None,
                            byte_offset: None,
                            line_number: None,
                            details: None,
                        })),
                    }
                } else {
                    // LRECL is 1, we already have the complete record
                    self.record_count += 1;
                    debug!(
                        "Read fixed record {} of {} bytes",
                        self.record_count, self.lrecl
                    );
                    Ok(Some(buffer))
                }
            }
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                // True EOF - no more data
                debug!("Reached EOF after {} records", self.record_count);
                Ok(None)
            }
            Err(e) => Err(Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error reading record: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: None,
            })),
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
                format!(
                    "Record length mismatch: expected {}, got {}",
                    self.lrecl,
                    record_data.len()
                ),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("Fixed record length validation failed".to_string()),
            }));
        }

        // If schema has a fixed LRECL, validate against it
        if let Some(schema_lrecl) = schema.lrecl_fixed
            && self.lrecl != schema_lrecl
        {
            warn!(
                "LRECL mismatch: reader configured for {}, schema expects {}",
                self.lrecl, schema_lrecl
            );
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
        let lrecl = lrecl
            .ok_or_else(|| Error::new(ErrorCode::CBKP001_SYNTAX, "Fixed format requires LRECL"))?;

        if lrecl == 0 {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "LRECL must be greater than zero",
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
                format!(
                    "Record too long: {} bytes exceeds LRECL of {}",
                    data_len, lrecl
                ),
            )
            .with_context(ErrorContext {
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
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error writing record: {e}"),
            )
            .with_context(ErrorContext {
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
                    ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                    format!("I/O error writing padding: {e}"),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(data_len as u64),
                    line_number: None,
                    details: Some("Error writing record padding".to_string()),
                })
            })?;
        }

        self.record_count += 1;
        debug!(
            "Wrote fixed record {} of {} bytes (data: {}, padding: {})",
            self.record_count,
            lrecl,
            data_len,
            lrecl - data_len
        );

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
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error flushing output: {e}"),
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

/// RDW (Record Descriptor Word) record reader for processing variable-length records
#[derive(Debug)]
pub struct RDWRecordReader<R: Read> {
    input: R,
    record_count: u64,
    strict_mode: bool,
}

impl<R: Read> RDWRecordReader<R> {
    /// Create a new RDW record reader
    pub fn new(input: R, strict_mode: bool) -> Self {
        Self {
            input,
            record_count: 0,
            strict_mode,
        }
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
                let length = u32::from(u16::from_be_bytes([rdw_header[0], rdw_header[1]]));
                let reserved = u16::from_be_bytes([rdw_header[2], rdw_header[3]]);

                self.record_count += 1;
                debug!(
                    "Read RDW header for record {}: length={}, reserved={:04X}",
                    self.record_count, length, reserved
                );

                // Validate reserved bytes
                if reserved != 0 {
                    let error = Error::new(
                        ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
                        format!("RDW reserved bytes are non-zero: {:04X}", reserved),
                    )
                    .with_context(ErrorContext {
                        record_index: Some(self.record_count),
                        field_path: None,
                        byte_offset: Some(2), // Reserved bytes are at offset 2-3
                        line_number: None,
                        details: Some(format!("Expected 0000, got {:04X}", reserved)),
                    });

                    if self.strict_mode {
                        return Err(error);
                    }
                    warn!(
                        "RDW reserved bytes non-zero (record {}): {:04X}",
                        self.record_count, reserved
                    );
                }

                // Check for ASCII transfer corruption heuristic
                if Self::is_suspect_ascii_corruption(rdw_header) {
                    warn!(
                        "RDW appears to be ASCII-corrupted (record {}): {:02X} {:02X} {:02X} {:02X}",
                        self.record_count,
                        rdw_header[0],
                        rdw_header[1],
                        rdw_header[2],
                        rdw_header[3]
                    );
                }

                // Handle zero-length records
                if length == 0 {
                    debug!("Zero-length RDW record {}", self.record_count);
                    return Ok(Some(RDWRecord {
                        header: rdw_header,
                        payload: Vec::new(),
                    }));
                }

                // Read the payload
                let mut payload = vec![0u8; length as usize];
                match self.input.read_exact(&mut payload) {
                    Ok(()) => {
                        debug!(
                            "Read RDW record {} payload: {} bytes",
                            self.record_count, length
                        );
                        Ok(Some(RDWRecord {
                            header: rdw_header,
                            payload,
                        }))
                    }
                    Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                        Err(Error::new(
                            ErrorCode::CBKR221_RDW_UNDERFLOW,
                            format!("Incomplete RDW record payload: expected {} bytes", length),
                        )
                        .with_context(ErrorContext {
                            record_index: Some(self.record_count),
                            field_path: None,
                            byte_offset: Some(4), // Payload starts after 4-byte header
                            line_number: None,
                            details: Some("File ends with incomplete RDW payload".to_string()),
                        }))
                    }
                    Err(e) => Err(Error::new(
                        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                        format!("I/O error reading RDW payload: {e}"),
                    )
                    .with_context(ErrorContext {
                        record_index: Some(self.record_count),
                        field_path: None,
                        byte_offset: Some(4),
                        line_number: None,
                        details: None,
                    })),
                }
            }
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                // True EOF - no more data
                debug!("Reached EOF after {} RDW records", self.record_count);
                Ok(None)
            }
            Err(e) => Err(Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error reading RDW header: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: None,
            })),
        }
    }

    /// Validate zero-length record against schema
    ///
    /// # Errors
    ///
    /// Returns an error if zero-length record is invalid for the schema
    pub fn validate_zero_length_record(&self, schema: &Schema) -> Result<()> {
        // Zero-length records are valid only when schema fixed prefix == 0
        // For now, we'll assume this is valid since we don't have layout calculation yet
        // This will be properly implemented when layout resolution is available

        // Calculate the minimum record size based on schema
        let min_size = Self::calculate_schema_fixed_prefix(schema);

        if min_size > 0 {
            return Err(Error::new(
                ErrorCode::CBKR221_RDW_UNDERFLOW,
                format!(
                    "Zero-length RDW record invalid: schema requires minimum {} bytes",
                    min_size
                ),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("Zero-length record with non-zero schema prefix".to_string()),
            }));
        }

        Ok(())
    }

    /// Calculate the fixed prefix size of a schema (placeholder implementation)
    fn calculate_schema_fixed_prefix(_schema: &Schema) -> u32 {
        // Placeholder: This should calculate the size of fields that appear
        // before any ODO arrays. For now, return 0 to allow zero-length records.
        // This will be properly implemented when layout resolution is available.
        0
    }

    /// Detect ASCII transfer corruption heuristic
    fn is_suspect_ascii_corruption(rdw_header: [u8; 4]) -> bool {
        // Heuristic: if the length bytes look like ASCII digits, it might be corrupted
        // ASCII digits are 0x30-0x39
        let length_bytes = [rdw_header[0], rdw_header[1]];

        // Check if both length bytes are ASCII digits
        length_bytes.iter().all(|&b| (0x30..=0x39).contains(&b))
    }

    /// Get the current record count
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
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
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error writing RDW header: {e}"),
            )
            .with_context(ErrorContext {
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
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error writing RDW payload: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: Some(4),
                line_number: None,
                details: None,
            })
        })?;

        self.record_count += 1;
        debug!(
            "Wrote RDW record {} with {} byte payload",
            self.record_count,
            record.payload.len()
        );

        Ok(())
    }

    /// Write a record from payload data, generating the RDW header
    ///
    /// # Errors
    ///
    /// Returns an error if the record cannot be written due to I/O errors
    pub fn write_record_from_payload(
        &mut self,
        payload: &[u8],
        preserve_reserved: Option<u16>,
    ) -> Result<()> {
        let length = payload.len();

        if length > u16::MAX as usize {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!(
                    "RDW payload too large: {} bytes exceeds maximum of {}",
                    length,
                    u16::MAX
                ),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("RDW length field is 16-bit".to_string()),
            }));
        }

        // Create RDW header
        let length_bytes = u16::try_from(length).expect("RDW length should fit in u16").to_be_bytes();
        let reserved_bytes = preserve_reserved.unwrap_or(0).to_be_bytes();
        let header = [
            length_bytes[0],
            length_bytes[1],
            reserved_bytes[0],
            reserved_bytes[1],
        ];

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
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error flushing output: {e}"),
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
        let length = u16::try_from(payload.len().min(u16::MAX as usize)).expect("payload length should fit in u16");
        let length_bytes = length.to_be_bytes();
        let header = [length_bytes[0], length_bytes[1], 0, 0]; // Reserved bytes are zero

        Self { header, payload }
    }

    /// Create a new RDW record with preserved reserved bytes
    #[must_use]
    pub fn with_reserved(payload: Vec<u8>, reserved: u16) -> Self {
        let length = u16::try_from(payload.len().min(u16::MAX as usize)).expect("payload length should fit in u16");
        let length_bytes = length.to_be_bytes();
        let reserved_bytes = reserved.to_be_bytes();
        let header = [
            length_bytes[0],
            length_bytes[1],
            reserved_bytes[0],
            reserved_bytes[1],
        ];

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

    /// Update the length field to match the payload size
    pub fn recompute_length(&mut self) {
        let length = u16::try_from(self.payload.len().min(u16::MAX as usize)).expect("payload length should fit in u16");
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
            _ => self.payload.clone(),             // Just payload
        }
    }
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
                    ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
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
        let record = read_record(&mut data, RecordFormat::Fixed, Some(8))
            .unwrap()
            .unwrap();
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
            0, 2, 0, 0, b'h', b'i', // First record
            0, 3, 0, 0, b'b', b'y', b'e', // Second record
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
        let data = vec![
            0, 8, b'3', b'4', b't', b'e', b's', b't', b'x', b'x', b'x', b'x',
        ];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);

        // Should read the record successfully (length=8, reserved=0x3334)
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.length(), 8);
        assert_eq!(record.reserved(), 0x3334); // '3' '4' as big-endian
        assert_eq!(record.payload, b"testxxxx");

        // The ASCII corruption detection is based on the length bytes being ASCII digits
        // In this case, the length bytes are 0, 8 which are not ASCII digits, so no warning
        // Let's test the actual detection logic separately
        assert!(
            !RDWRecordReader::<std::io::Cursor<Vec<u8>>>::is_suspect_ascii_corruption([
                0, 8, b'3', b'4'
            ])
        );
        assert!(
            RDWRecordReader::<std::io::Cursor<Vec<u8>>>::is_suspect_ascii_corruption([
                b'1', b'2', 0, 0
            ])
        );
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

        writer
            .write_record_from_payload(b"test", Some(0x1234))
            .unwrap();

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

        let record = read_record(&mut cursor, RecordFormat::RDW, None)
            .unwrap()
            .unwrap();
        assert_eq!(record, b"test"); // Should return just the payload
    }
}
