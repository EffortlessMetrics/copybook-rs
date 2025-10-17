//! Record framing and I/O utilities
//!
//! This module handles fixed-length and RDW variable-length record processing.

use crate::options::RawMode;
use crate::options::RecordFormat;
use copybook_core::error::ErrorContext;
use copybook_core::{Error, ErrorCode, Result, Schema};
use std::convert::TryFrom;
use std::io::{BufRead, BufReader, ErrorKind, Read, Write};
use tracing::{debug, warn};

/// Read a 2-byte big-endian RDW *body length* and consume the header.
///
/// # Errors
/// Returns an error if the header is malformed or cannot be read.
/// EOF/short header handling is done by the caller (see `rdw_try_peek_len`).
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub(crate) fn rdw_read_len<R: BufRead>(r: &mut R) -> Result<u16> {
    let buf = r.fill_buf().map_err(|e| {
        Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!("I/O error peeking RDW length: {e}"),
        )
    })?;
    if buf.len() < 2 {
        return Err(Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            format!(
                "Incomplete RDW header: expected 2 bytes for length (have {})",
                buf.len()
            ),
        ));
    }

    let hi = buf[0];
    let lo = buf[1];
    r.consume(2);

    Ok(u16::from_be_bytes([hi, lo]))
}

/// Borrow the RDW body slice for `len` bytes without consuming.
///
/// # Errors
/// Returns an error if fewer than `len` bytes are currently available in the buffer.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub(crate) fn rdw_slice_body<R: BufRead>(r: &mut R, len: u16) -> Result<&[u8]> {
    let need = usize::from(len);
    if need == 0 {
        return Ok(&[]);
    }

    let buf = r.fill_buf().map_err(|e| {
        Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!("I/O error reading RDW payload: {e}"),
        )
    })?;

    if buf.len() < need {
        return Err(Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            format!(
                "Incomplete RDW record payload: expected {} bytes (have {})",
                need,
                buf.len()
            ),
        ));
    }

    Ok(&buf[..need])
}

/// Validate the RDW body slice (placeholder; extend with CRC/terminator later).
///
/// # Errors
/// Returns an error if additional format checks fail (currently none).
#[inline]
pub(crate) fn rdw_validate_and_finish(body: &[u8]) -> &[u8] {
    body
}

/// Helper for EOF/short-header detection used by readers/iterators.
/// - 0 or 1 buffered bytes → `Ok(None)` (treat as EOF/short read, not an error)
/// - ≥ 2 bytes             → `Ok(Some(()))` (caller may proceed to `rdw_read_len`)
///
/// # Errors
/// Returns an error if the RDW header cannot be peeked from the underlying reader.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub(crate) fn rdw_try_peek_len<R: BufRead>(r: &mut R) -> Result<Option<()>> {
    let buf = r.fill_buf().map_err(|e| {
        Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!("I/O error peeking RDW header: {e}"),
        )
    })?;
    if buf.len() <= 1 {
        return Ok(None);
    }
    Ok(Some(()))
}

/// Fixed record reader for processing fixed-length records
#[derive(Debug)]
pub struct FixedRecordReader<R: Read> {
    input: R,
    lrecl: u32,
    record_count: u64,
}

impl<R: Read> FixedRecordReader<R> {
    #[allow(clippy::too_many_lines)]
    /// Create a new fixed record reader.
    ///
    /// # Errors
    /// Returns an error if no LRECL is provided or if it is zero.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn new(input: R, lrecl: Option<u32>) -> Result<Self> {
        let lrecl = lrecl.ok_or_else(|| {
            Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                "Fixed format requires LRECL",
            )
        })?;

        if lrecl == 0 {
            return Err(Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
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
    /// Returns an error if the record cannot be read due to I/O errors.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn read_record(&mut self) -> Result<Option<Vec<u8>>> {
        // Try to read one byte first to check for EOF
        let mut first_byte = [0u8; 1];
        match self.input.read_exact(&mut first_byte) {
            Ok(()) => {
                // We got the first byte, now read the rest
                let lrecl_len = self.lrecl_usize()?;
                let mut buffer = vec![0u8; lrecl_len];
                buffer[0] = first_byte[0];

                if lrecl_len > 1 {
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
                                ErrorCode::CBKF221_RDW_UNDERFLOW,
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
    /// Returns an error if the record length doesn't match schema expectations.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn validate_record_length(&self, schema: &Schema, record_data: &[u8]) -> Result<()> {
        // For fixed records, the data length should match LRECL
        let lrecl_len = self.lrecl_usize()?;
        if record_data.len() != lrecl_len {
            return Err(Error::new(
                ErrorCode::CBKF221_RDW_UNDERFLOW,
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
    #[inline]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Get the configured LRECL
    #[must_use]
    #[inline]
    pub fn lrecl(&self) -> u32 {
        self.lrecl
    }

    #[inline]
    fn lrecl_usize(&self) -> Result<usize> {
        usize::try_from(self.lrecl).map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "LRECL exceeds platform addressable size",
            )
        })
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
    /// Returns an error if LRECL is not provided or is zero.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn new(output: W, lrecl: Option<u32>) -> Result<Self> {
        let lrecl = lrecl.ok_or_else(|| {
            Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                "Fixed format requires LRECL",
            )
        })?;

        if lrecl == 0 {
            return Err(Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
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
    /// Returns an error if the record cannot be written due to I/O errors.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record(&mut self, data: &[u8]) -> Result<()> {
        let data_len = data.len();
        let lrecl = self.lrecl_usize()?;

        if data_len > lrecl {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Record too long: {data_len} bytes exceeds LRECL of {lrecl}"),
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
                    byte_offset: Some(u64::try_from(data_len).unwrap_or(u64::MAX)),
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
    /// Returns an error if the flush operation fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
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
    #[inline]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Get the configured LRECL
    #[must_use]
    #[inline]
    pub fn lrecl(&self) -> u32 {
        self.lrecl
    }

    #[inline]
    fn lrecl_usize(&self) -> Result<usize> {
        usize::try_from(self.lrecl).map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "LRECL exceeds platform addressable size",
            )
        })
    }
}

/// RDW (Record Descriptor Word) record reader for processing variable-length records
#[derive(Debug)]
pub struct RDWRecordReader<R: Read> {
    input: BufReader<R>,
    record_count: u64,
    strict_mode: bool,
}

const RDW_READER_BUF_CAPACITY: usize = (u16::MAX as usize) + 4;

impl<R: Read> RDWRecordReader<R> {
    /// Create a new RDW record reader
    #[inline]
    pub fn new(input: R, strict_mode: bool) -> Self {
        Self {
            input: BufReader::with_capacity(RDW_READER_BUF_CAPACITY, input),
            record_count: 0,
            strict_mode,
        }
    }

    #[inline]
    fn peek_header(&mut self) -> Result<Option<[u8; 4]>> {
        let peek = rdw_try_peek_len(&mut self.input).map_err(|error| {
            error.with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: Some(0),
                line_number: None,
                details: Some("Unable to peek RDW header".to_string()),
            })
        })?;

        if peek.is_none() {
            let buf = self.input.fill_buf().map_err(|e| {
                Error::new(
                    ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                    format!("I/O error reading RDW header: {e}"),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(0),
                    line_number: None,
                    details: Some("Unable to read RDW header".to_string()),
                })
            })?;

            if buf.is_empty() {
                debug!("Reached EOF after {} RDW records", self.record_count);
                return Ok(None);
            }

            if self.strict_mode {
                return Err(Error::new(
                    ErrorCode::CBKF221_RDW_UNDERFLOW,
                    "Incomplete RDW header: expected 4 bytes".to_string(),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(0),
                    line_number: None,
                    details: Some("File ends with incomplete RDW header".to_string()),
                }));
            }

            debug!(
                "Reached EOF after {} RDW records (truncated header ignored)",
                self.record_count
            );
            let remaining = buf.len();
            self.input.consume(remaining);
            return Ok(None);
        }

        let buf = self.input.fill_buf().map_err(|e| {
            Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error reading RDW header: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: Some(0),
                line_number: None,
                details: Some("Unable to read RDW header".to_string()),
            })
        })?;

        if buf.len() < 4 {
            if self.strict_mode {
                return Err(Error::new(
                    ErrorCode::CBKF221_RDW_UNDERFLOW,
                    "Incomplete RDW header: expected 4 bytes".to_string(),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(0),
                    line_number: None,
                    details: Some("File ends with incomplete RDW header".to_string()),
                }));
            }

            debug!(
                "Reached EOF after {} RDW records (truncated header ignored)",
                self.record_count
            );
            let remaining = buf.len();
            self.input.consume(remaining);
            return Ok(None);
        }

        Ok(Some([buf[0], buf[1], buf[2], buf[3]]))
    }

    /// Read the next RDW record
    ///
    /// # Errors
    /// Returns an error if the record cannot be read due to I/O errors or format issues.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn read_record(&mut self) -> Result<Option<RDWRecord>> {
        let Some(header) = self.peek_header()? else {
            return Ok(None);
        };

        let length = match rdw_read_len(&mut self.input) {
            Ok(len) => len,
            Err(error) => {
                return Err(error.with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(0),
                    line_number: None,
                    details: Some("Unable to read RDW body length".to_string()),
                }));
            }
        };

        // Consume reserved bytes so the buffer now points at the body
        self.input.consume(2);
        let reserved = u16::from_be_bytes([header[2], header[3]]);

        self.record_count += 1;
        debug!(
            "Read RDW header for record {}: length={}, reserved={:04X}",
            self.record_count,
            u32::from(length),
            reserved
        );

        // Validate reserved bytes
        if reserved != 0 {
            let error = Error::new(
                ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
                format!("RDW reserved bytes are non-zero: {reserved:04X}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: Some(2), // Reserved bytes are at offset 2-3
                line_number: None,
                details: Some(format!("Expected 0000, got {reserved:04X}")),
            });

            if self.strict_mode {
                return Err(error);
            }
            warn!(
                "RDW reserved bytes non-zero (record {}): {:04X}",
                self.record_count, reserved
            );
            // Increment warning counter for summary
            crate::lib_api::increment_warning_counter();
        }

        // Check for ASCII transfer corruption heuristic
        if Self::is_suspect_ascii_corruption(header) {
            warn!(
                "RDW appears to be ASCII-corrupted (record {}): {:02X} {:02X} {:02X} {:02X}",
                self.record_count, header[0], header[1], header[2], header[3]
            );
            // For ASCII corruption, return an error instead of just warning
            return Err(Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!(
                    "RDW appears to be ASCII-corrupted: {:02X} {:02X} {:02X} {:02X}",
                    header[0], header[1], header[2], header[3]
                ),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: Some(0), // Header bytes are at offset 0-3
                line_number: None,
                details: Some("Suspected ASCII transfer corruption".to_string()),
            }));
        }

        // Handle zero-length records
        if length == 0 {
            debug!("Zero-length RDW record {}", self.record_count);
            return Ok(Some(RDWRecord {
                header,
                payload: Vec::new(),
            }));
        }

        let payload_len = usize::from(length);
        let body_slice = match rdw_slice_body(&mut self.input, length) {
            Ok(slice) => slice,
            Err(error) => {
                return Err(error.with_context(ErrorContext {
                    record_index: Some(self.record_count),
                    field_path: None,
                    byte_offset: Some(4), // Payload starts after 4-byte header
                    line_number: None,
                    details: Some("File ends with incomplete RDW payload".to_string()),
                }));
            }
        };

        let payload = rdw_validate_and_finish(body_slice).to_vec();
        self.input.consume(payload_len);

        debug!(
            "Read RDW record {} payload: {} bytes",
            self.record_count, length
        );
        Ok(Some(RDWRecord { header, payload }))
    }

    /// Validate zero-length record against schema
    ///
    /// # Errors
    /// Returns an error if zero-length record is invalid for the schema.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn validate_zero_length_record(&self, schema: &Schema) -> Result<()> {
        // Zero-length records are valid only when schema fixed prefix == 0

        // Calculate the minimum record size based on schema
        let min_size = Self::calculate_schema_fixed_prefix(schema);

        if min_size > 0 {
            return Err(Error::new(
                ErrorCode::CBKF221_RDW_UNDERFLOW,
                format!("Zero-length RDW record invalid: schema requires minimum {min_size} bytes"),
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

    /// Calculate the fixed prefix size of a schema
    ///
    /// This walks the schema to find the first field that has an ODO
    /// (OCCURS DEPENDING ON) clause and returns its byte offset. If the
    /// schema has no ODO arrays, the total fixed record length is returned.
    fn calculate_schema_fixed_prefix(schema: &Schema) -> u32 {
        use copybook_core::{Field, Occurs};

        fn find_first_odo_offset(fields: &[Field], current: &mut Option<u32>) {
            for field in fields {
                if let Some(Occurs::ODO { .. }) = &field.occurs {
                    let offset = field.offset;
                    match current {
                        Some(existing) => {
                            if offset < *existing {
                                *current = Some(offset);
                            }
                        }
                        None => *current = Some(offset),
                    }
                }
                if !field.children.is_empty() {
                    find_first_odo_offset(&field.children, current);
                }
            }
        }

        let mut first_odo_offset: Option<u32> = None;
        find_first_odo_offset(&schema.fields, &mut first_odo_offset);

        if let Some(offset) = first_odo_offset {
            offset
        } else if let Some(lrecl) = schema.lrecl_fixed {
            lrecl
        } else {
            fn find_record_end(fields: &[Field], max_end: &mut u32) {
                for field in fields {
                    let end = field.offset + field.len;
                    if end > *max_end {
                        *max_end = end;
                    }
                    if !field.children.is_empty() {
                        find_record_end(&field.children, max_end);
                    }
                }
            }

            let mut max_end = 0;
            find_record_end(&schema.fields, &mut max_end);
            max_end
        }
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
    #[inline]
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
    #[inline]
    pub fn new(output: W) -> Self {
        Self {
            output,
            record_count: 0,
        }
    }

    /// Write an RDW record
    ///
    /// # Errors
    /// Returns an error if the record cannot be written due to I/O errors.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
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
    /// Returns an error if the record cannot be written due to I/O errors.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record_from_payload(
        &mut self,
        payload: &[u8],
        preserve_reserved: Option<u16>,
    ) -> Result<()> {
        let length = payload.len();

        let length_u16 = u16::try_from(length).map_err(|_| {
            Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!(
                    "RDW payload too large: {length} bytes exceeds maximum of {}",
                    u16::MAX
                ),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("RDW length field is 16-bit".to_string()),
            })
        })?;

        // Create RDW header
        let length_bytes = length_u16.to_be_bytes();
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
    /// Returns an error if the flush operation fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
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
    #[inline]
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
    #[inline]
    pub fn new(payload: Vec<u8>) -> Self {
        let length = u16::try_from(payload.len()).unwrap_or(u16::MAX);
        let length_bytes = length.to_be_bytes();
        let header = [length_bytes[0], length_bytes[1], 0, 0]; // Reserved bytes are zero

        Self { header, payload }
    }

    /// Create a new RDW record with preserved reserved bytes
    #[must_use]
    #[inline]
    pub fn with_reserved(payload: Vec<u8>, reserved: u16) -> Self {
        let length = u16::try_from(payload.len()).unwrap_or(u16::MAX);
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
    #[inline]
    pub fn length(&self) -> u16 {
        u16::from_be_bytes([self.header[0], self.header[1]])
    }

    /// Get the reserved bytes from the RDW header
    #[must_use]
    #[inline]
    pub fn reserved(&self) -> u16 {
        u16::from_be_bytes([self.header[2], self.header[3]])
    }

    /// Update the length field to match the payload size
    #[inline]
    pub fn recompute_length(&mut self) {
        let length = u16::try_from(self.payload.len()).unwrap_or(u16::MAX);
        let length_bytes = length.to_be_bytes();
        self.header[0] = length_bytes[0];
        self.header[1] = length_bytes[1];
    }

    /// Get the complete record data (header + payload)
    #[must_use]
    #[inline]
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(4 + self.payload.len());
        result.extend_from_slice(&self.header);
        result.extend_from_slice(&self.payload);
        result
    }

    /// Get the record data for specific raw modes
    #[must_use]
    #[inline]
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
/// Returns an error if the record cannot be read due to I/O errors or format issues.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn read_record(
    input: &mut impl Read,
    format: RecordFormat,
    lrecl: Option<u32>,
) -> Result<Option<Vec<u8>>> {
    match format {
        RecordFormat::Fixed => read_fixed_record_legacy(input, lrecl),
        RecordFormat::RDW => read_rdw_record_legacy(input),
    }
}

#[inline]
fn read_fixed_record_legacy(input: &mut impl Read, lrecl: Option<u32>) -> Result<Option<Vec<u8>>> {
    let mut reader = FixedRecordReader::new(input, lrecl)?;
    reader.read_record()
}

#[inline]
fn read_rdw_record_legacy(input: &mut impl Read) -> Result<Option<Vec<u8>>> {
    let mut reader = RDWRecordReader::new(input, false);
    match reader.read_record()? {
        Some(rdw_record) => Ok(Some(rdw_record.payload)),
        None => Ok(None),
    }
}
/// Write a single record to output (legacy interface)
///
/// # Errors
/// Returns an error if the record cannot be written due to I/O errors.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
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
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use copybook_core::Schema;
    use std::io::{BufRead, Cursor};

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
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
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
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
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
    fn rdw_eof_short_header_is_none() {
        let mut cur = Cursor::new(Vec::<u8>::new());
        assert!(rdw_try_peek_len(&mut cur).unwrap().is_none());

        let mut cur = Cursor::new(vec![0x00]);
        assert!(rdw_try_peek_len(&mut cur).unwrap().is_none());
    }

    #[test]
    fn rdw_malformed_len_body_too_short_is_cbkf102() {
        // Header says 0x0010 (16 bytes), body has only 4 → CBKF102
        let mut cur = Cursor::new(vec![0x00, 0x10, 0xAA, 0xBB, 0xCC, 0xDD]);

        // Peek: we have ≥ 2 bytes
        assert!(rdw_try_peek_len(&mut cur).unwrap().is_some());
        let len = rdw_read_len(&mut cur).unwrap();
        assert_eq!(len, 16);

        // Skip reserved bytes to position at the payload
        cur.consume(2);

        let err = rdw_slice_body(&mut cur, len).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(
            err.message.contains("Incomplete RDW record payload"),
            "message: {}",
            err.message
        );
    }

    #[test]
    fn rdw_round_trip_ok() {
        // Header 0x0003 with zero reserved bytes and payload "ABC"
        let mut cur = Cursor::new(vec![0x00, 0x03, 0x00, 0x00, b'A', b'B', b'C']);

        assert!(rdw_try_peek_len(&mut cur).unwrap().is_some());
        let len = rdw_read_len(&mut cur).unwrap();
        assert_eq!(len, 3);

        // Skip reserved before borrowing the body
        cur.consume(2);

        let body = rdw_slice_body(&mut cur, len).unwrap();
        let body = rdw_validate_and_finish(body);
        assert_eq!(body, b"ABC");
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
        assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
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
        let large_payload = vec![0u8; usize::from(u16::MAX) + 1];
        let result = writer.write_record_from_payload(&large_payload, None);

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    }

    #[test]
    fn test_zero_length_record_rejected_without_odo() {
        use copybook_core::{Field, FieldKind};

        let mut field = Field::with_kind(1, "FIELD".to_string(), FieldKind::Alphanum { len: 5 });
        field.offset = 0;
        field.len = 5;

        let schema = Schema {
            fields: vec![field],
            lrecl_fixed: Some(5),
            tail_odo: None,
            fingerprint: String::new(),
        };

        let reader = RDWRecordReader::new(Cursor::new(Vec::new()), false);
        let result = reader.validate_zero_length_record(&schema);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);

        let prefix = RDWRecordReader::<Cursor<Vec<u8>>>::calculate_schema_fixed_prefix(&schema);
        assert_eq!(prefix, 5);
    }

    #[test]
    fn test_zero_length_record_rejected_with_odo() {
        use copybook_core::{Field, FieldKind, Occurs, TailODO};

        let mut counter = Field::with_kind(
            5,
            "CTR".to_string(),
            FieldKind::BinaryInt {
                bits: 16,
                signed: false,
            },
        );
        counter.offset = 0;
        counter.len = 2;

        let mut array = Field::with_kind(5, "ARR".to_string(), FieldKind::Alphanum { len: 1 });
        array.offset = 2;
        array.len = 1;
        array.occurs = Some(Occurs::ODO {
            min: 0,
            max: 5,
            counter_path: "CTR".to_string(),
        });

        let schema = Schema {
            fields: vec![counter, array],
            lrecl_fixed: None,
            tail_odo: Some(TailODO {
                counter_path: "CTR".to_string(),
                min_count: 0,
                max_count: 5,
                array_path: "ARR".to_string(),
            }),
            fingerprint: String::new(),
        };

        let reader = RDWRecordReader::new(Cursor::new(Vec::new()), false);
        let result = reader.validate_zero_length_record(&schema);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);

        let prefix = RDWRecordReader::<Cursor<Vec<u8>>>::calculate_schema_fixed_prefix(&schema);
        assert_eq!(prefix, 2);
    }

    #[test]
    fn test_zero_length_record_allowed_for_empty_schema() {
        let schema = Schema::new();
        let reader = RDWRecordReader::new(Cursor::new(Vec::new()), false);

        let result = reader.validate_zero_length_record(&schema);
        assert!(result.is_ok());

        let prefix = RDWRecordReader::<Cursor<Vec<u8>>>::calculate_schema_fixed_prefix(&schema);
        assert_eq!(prefix, 0);
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
