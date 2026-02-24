#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW (Record Descriptor Word) header primitives.
//!
//! This crate intentionally focuses on one concern:
//! parsing and constructing RDW framing metadata plus minimal buffered helpers.

use copybook_core::Schema;
use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use std::io::{BufRead, BufReader, Read, Write};
use tracing::{debug, warn};

/// Size of an RDW header in bytes.
pub const RDW_HEADER_LEN: usize = 4;

/// Maximum payload size representable in RDW (`u16::MAX`).
pub const RDW_MAX_PAYLOAD_LEN: usize = u16::MAX as usize;

const RDW_READER_BUF_CAPACITY: usize = (u16::MAX as usize) + RDW_HEADER_LEN;

/// Parsed RDW header (`length + reserved`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RdwHeader {
    bytes: [u8; RDW_HEADER_LEN],
}

impl RdwHeader {
    /// Construct from raw 4-byte header bytes.
    #[must_use]
    #[inline]
    pub const fn from_bytes(bytes: [u8; RDW_HEADER_LEN]) -> Self {
        Self { bytes }
    }

    /// Construct from payload length and reserved bytes.
    ///
    /// # Errors
    /// Returns `CBKF102_RECORD_LENGTH_INVALID` when `payload_len > u16::MAX`.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn from_payload_len(payload_len: usize, reserved: u16) -> Result<Self> {
        let len = rdw_payload_len_to_u16(payload_len)?;
        let len_bytes = len.to_be_bytes();
        let reserved_bytes = reserved.to_be_bytes();
        Ok(Self {
            bytes: [
                len_bytes[0],
                len_bytes[1],
                reserved_bytes[0],
                reserved_bytes[1],
            ],
        })
    }

    /// Return raw bytes.
    #[must_use]
    #[inline]
    pub const fn bytes(self) -> [u8; RDW_HEADER_LEN] {
        self.bytes
    }

    /// Extract payload length.
    #[must_use]
    #[inline]
    pub const fn length(self) -> u16 {
        u16::from_be_bytes([self.bytes[0], self.bytes[1]])
    }

    /// Extract reserved bytes.
    #[must_use]
    #[inline]
    pub const fn reserved(self) -> u16 {
        u16::from_be_bytes([self.bytes[2], self.bytes[3]])
    }

    /// ASCII-corruption heuristic for the length bytes.
    ///
    /// Returns `true` when both length bytes look like ASCII digits.
    #[must_use]
    #[inline]
    pub const fn looks_ascii_corrupt(self) -> bool {
        rdw_is_suspect_ascii_corruption(self.bytes)
    }
}

/// Convert payload length to `u16` with RDW error mapping.
///
/// # Errors
/// Returns `CBKF102_RECORD_LENGTH_INVALID` when `len > u16::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn rdw_payload_len_to_u16(len: usize) -> Result<u16> {
    u16::try_from(len).map_err(|_| {
        Error::new(
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
            format!(
                "RDW payload too large: {} bytes exceeds maximum of {}",
                len,
                u16::MAX
            ),
        )
    })
}

/// Heuristic to detect ASCII-corrupted RDW headers.
///
/// Returns `true` when both RDW length bytes are ASCII digits (`0x30..=0x39`).
#[must_use]
#[inline]
pub const fn rdw_is_suspect_ascii_corruption(rdw_header: [u8; RDW_HEADER_LEN]) -> bool {
    copybook_rdw_predicates::rdw_is_suspect_ascii_corruption(rdw_header)
}

/// Read a 2-byte big-endian RDW body length and consume those two bytes.
///
/// # Errors
/// Returns:
/// - `CBKF104_RDW_SUSPECT_ASCII` for I/O errors while peeking.
/// - `CBKF102_RECORD_LENGTH_INVALID` for incomplete headers.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn rdw_read_len<R: BufRead>(reader: &mut R) -> Result<u16> {
    let buf = reader.fill_buf().map_err(|e| {
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
    reader.consume(2);
    Ok(u16::from_be_bytes([hi, lo]))
}

/// Borrow the RDW body slice for `len` bytes without consuming.
///
/// # Errors
/// Returns:
/// - `CBKF104_RDW_SUSPECT_ASCII` for I/O errors while peeking.
/// - `CBKF102_RECORD_LENGTH_INVALID` when fewer than `len` bytes are available.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn rdw_slice_body<R: BufRead>(reader: &mut R, len: u16) -> Result<&[u8]> {
    let need = usize::from(len);
    if need == 0 {
        return Ok(&[]);
    }

    let buf = reader.fill_buf().map_err(|e| {
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

/// Placeholder for future RDW body validation hooks.
#[inline]
#[must_use]
pub const fn rdw_validate_and_finish(body: &[u8]) -> &[u8] {
    body
}

/// Probe if enough bytes exist to attempt RDW length parsing.
///
/// - `0` or `1` byte buffered => `Ok(None)`
/// - `>= 2` bytes buffered => `Ok(Some(()))`
///
/// # Errors
/// Returns `CBKF104_RDW_SUSPECT_ASCII` for I/O errors while peeking.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn rdw_try_peek_len<R: BufRead>(reader: &mut R) -> Result<Option<()>> {
    let buf = reader.fill_buf().map_err(|e| {
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

/// An RDW record with header and payload bytes.
#[derive(Debug, Clone)]
pub struct RDWRecord {
    /// 4-byte RDW header (length + reserved).
    pub header: [u8; RDW_HEADER_LEN],
    /// Record payload bytes.
    pub payload: Vec<u8>,
}

impl RDWRecord {
    /// Create a new RDW record from payload (fallible constructor).
    ///
    /// # Errors
    /// Returns an error when payload length exceeds `u16::MAX`.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_new(payload: Vec<u8>) -> Result<Self> {
        let header = RdwHeader::from_payload_len(payload.len(), 0)?.bytes();
        Ok(Self { header, payload })
    }

    /// Create a new RDW record from payload.
    ///
    /// # Panics
    /// Panics when payload length exceeds `u16::MAX`.
    #[deprecated(
        since = "0.4.3",
        note = "use try_new() instead for fallible construction"
    )]
    #[allow(clippy::expect_used)] // Intentional panic for deprecated API
    #[inline]
    #[must_use]
    pub fn new(payload: Vec<u8>) -> Self {
        Self::try_new(payload).expect("RDW payload exceeds maximum size (65535 bytes)")
    }

    /// Create an RDW record preserving reserved bytes (fallible constructor).
    ///
    /// # Errors
    /// Returns an error when payload length exceeds `u16::MAX`.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_with_reserved(payload: Vec<u8>, reserved: u16) -> Result<Self> {
        let header = RdwHeader::from_payload_len(payload.len(), reserved)?.bytes();
        Ok(Self { header, payload })
    }

    /// Create an RDW record preserving reserved bytes.
    ///
    /// # Panics
    /// Panics when payload length exceeds `u16::MAX`.
    #[deprecated(
        since = "0.4.3",
        note = "use try_with_reserved() instead for fallible construction"
    )]
    #[allow(clippy::expect_used)] // Intentional panic for deprecated API
    #[inline]
    #[must_use]
    pub fn with_reserved(payload: Vec<u8>, reserved: u16) -> Self {
        Self::try_with_reserved(payload, reserved)
            .expect("RDW payload exceeds maximum size (65535 bytes)")
    }

    /// Get payload length from header.
    #[inline]
    #[must_use]
    pub fn length(&self) -> u16 {
        RdwHeader::from_bytes(self.header).length()
    }

    /// Get reserved bytes from header.
    #[inline]
    #[must_use]
    pub fn reserved(&self) -> u16 {
        RdwHeader::from_bytes(self.header).reserved()
    }

    /// Recompute the header length field from payload length.
    ///
    /// # Errors
    /// Returns an error when payload length exceeds `u16::MAX`.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_recompute_length(&mut self) -> Result<()> {
        self.header = RdwHeader::from_payload_len(self.payload.len(), self.reserved())?.bytes();
        Ok(())
    }

    /// Recompute the header length field from payload length.
    ///
    /// # Panics
    /// Panics when payload length exceeds `u16::MAX`.
    #[deprecated(
        since = "0.4.3",
        note = "use try_recompute_length() instead for fallible operation"
    )]
    #[allow(clippy::expect_used)] // Intentional panic for deprecated API
    #[inline]
    pub fn recompute_length(&mut self) {
        self.try_recompute_length()
            .expect("RDW payload exceeds maximum size (65535 bytes)");
    }

    /// Serialize record as `header + payload`.
    #[inline]
    #[must_use]
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(RDW_HEADER_LEN + self.payload.len());
        result.extend_from_slice(&self.header);
        result.extend_from_slice(&self.payload);
        result
    }
}

/// RDW (Record Descriptor Word) record reader for variable-length records.
#[derive(Debug)]
pub struct RDWRecordReader<R: Read> {
    input: BufReader<R>,
    record_count: u64,
    strict_mode: bool,
}

impl<R: Read> RDWRecordReader<R> {
    /// Create a new RDW record reader.
    #[inline]
    #[must_use]
    pub fn new(input: R, strict_mode: bool) -> Self {
        Self {
            input: BufReader::with_capacity(RDW_READER_BUF_CAPACITY, input),
            record_count: 0,
            strict_mode,
        }
    }

    #[inline]
    fn peek_header(&mut self) -> Result<Option<[u8; RDW_HEADER_LEN]>> {
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

        if buf.len() < RDW_HEADER_LEN {
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

    /// Read the next RDW record.
    ///
    /// # Errors
    /// Returns an error if the record cannot be read due to I/O errors or
    /// framing issues.
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

        // Consume reserved bytes so the buffer now points at the body.
        self.input.consume(2);
        let reserved = u16::from_be_bytes([header[2], header[3]]);

        self.record_count += 1;
        debug!(
            "Read RDW header for record {}: length={}, reserved={:04X}",
            self.record_count,
            u32::from(length),
            reserved
        );

        if reserved != 0 {
            let error = Error::new(
                ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
                format!("RDW reserved bytes are non-zero: {reserved:04X}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: Some(2),
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
        }

        if Self::is_suspect_ascii_corruption(header) {
            warn!(
                "RDW appears to be ASCII-corrupted (record {}): {:02X} {:02X} {:02X} {:02X}",
                self.record_count, header[0], header[1], header[2], header[3]
            );

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
                byte_offset: Some(0),
                line_number: None,
                details: Some("Suspected ASCII transfer corruption".to_string()),
            }));
        }

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
                    byte_offset: Some(4),
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

    /// Validate a zero-length record against schema requirements.
    ///
    /// # Errors
    /// Returns an error when the schema requires non-zero fixed bytes.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn validate_zero_length_record(&self, schema: &Schema) -> Result<()> {
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

    /// Number of RDW records consumed from the stream.
    #[inline]
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    #[inline]
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

    #[inline]
    fn is_suspect_ascii_corruption(rdw_header: [u8; RDW_HEADER_LEN]) -> bool {
        rdw_is_suspect_ascii_corruption(rdw_header)
    }
}

/// RDW record writer for variable-length records.
#[derive(Debug)]
pub struct RDWRecordWriter<W: Write> {
    output: W,
    record_count: u64,
}

impl<W: Write> RDWRecordWriter<W> {
    /// Create a new RDW record writer.
    #[inline]
    #[must_use]
    pub fn new(output: W) -> Self {
        Self {
            output,
            record_count: 0,
        }
    }

    /// Write an RDW record.
    ///
    /// # Errors
    /// Returns an error if writing header or payload fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record(&mut self, record: &RDWRecord) -> Result<()> {
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

    /// Write an RDW record directly from payload.
    ///
    /// # Errors
    /// Returns an error if payload length exceeds `u16::MAX` or I/O fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record_from_payload(
        &mut self,
        payload: &[u8],
        preserve_reserved: Option<u16>,
    ) -> Result<()> {
        let length = payload.len();
        let header =
            RdwHeader::from_payload_len(length, preserve_reserved.unwrap_or(0)).map_err(|_| {
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

        let record = RDWRecord {
            header: header.bytes(),
            payload: payload.to_vec(),
        };
        self.write_record(&record)
    }

    /// Flush writer output.
    ///
    /// # Errors
    /// Returns an error when flush fails.
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

    /// Number of written RDW records.
    #[inline]
    #[must_use]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
mod tests {
    use super::*;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use std::io::{BufRead, Cursor};

    #[test]
    fn header_from_payload_len_roundtrips() {
        let header = RdwHeader::from_payload_len(10, 0x1234).unwrap();
        assert_eq!(header.length(), 10);
        assert_eq!(header.reserved(), 0x1234);
        assert_eq!(header.bytes(), [0x00, 0x0A, 0x12, 0x34]);
    }

    #[test]
    fn header_from_payload_len_oversize_fails() {
        let err = RdwHeader::from_payload_len(RDW_MAX_PAYLOAD_LEN + 1, 0).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    }

    #[test]
    fn ascii_corruption_heuristic_matches_digits_only() {
        assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', 0, 0]));
        assert!(!rdw_is_suspect_ascii_corruption([0, 12, 0, 0]));
    }

    #[test]
    fn rdw_peek_len_none_on_short_buffer() {
        let mut cur = Cursor::new(Vec::<u8>::new());
        assert!(rdw_try_peek_len(&mut cur).unwrap().is_none());

        let mut cur = Cursor::new(vec![0x00]);
        assert!(rdw_try_peek_len(&mut cur).unwrap().is_none());
    }

    #[test]
    fn rdw_read_len_consumes_two_bytes() {
        let mut cur = Cursor::new(vec![0x00, 0x03, 0xAA, 0xBB, b'A', b'B', b'C']);
        let len = rdw_read_len(&mut cur).unwrap();
        assert_eq!(len, 3);

        cur.consume(2);
        let body = rdw_slice_body(&mut cur, len).unwrap();
        assert_eq!(rdw_validate_and_finish(body), b"ABC");
    }

    #[test]
    fn rdw_slice_body_short_is_cbkf102() {
        let mut cur = Cursor::new(vec![0x00, 0x10, 0xAA, 0xBB, 0xCC, 0xDD]);
        let len = rdw_read_len(&mut cur).unwrap();
        cur.consume(2);
        let err = rdw_slice_body(&mut cur, len).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    }

    #[test]
    fn rdw_record_try_new_roundtrip() {
        let record = RDWRecord::try_new(b"hello".to_vec()).unwrap();
        assert_eq!(record.length(), 5);
        assert_eq!(record.reserved(), 0);
        assert_eq!(record.payload, b"hello");
    }

    #[test]
    fn rdw_record_try_with_reserved_roundtrip() {
        let record = RDWRecord::try_with_reserved(b"test".to_vec(), 0x1234).unwrap();
        assert_eq!(record.length(), 4);
        assert_eq!(record.reserved(), 0x1234);
        assert_eq!(record.payload, b"test");
    }

    #[test]
    fn rdw_record_try_recompute_updates_length() {
        let mut record = RDWRecord::try_new(b"test".to_vec()).unwrap();
        record.payload = b"longer_payload".to_vec();
        record.try_recompute_length().unwrap();
        assert_eq!(record.length(), 14);
    }

    #[test]
    fn rdw_record_as_bytes_prepends_header() {
        let record = RDWRecord::try_new(b"hi".to_vec()).unwrap();
        assert_eq!(record.as_bytes(), vec![0, 2, 0, 0, b'h', b'i']);
    }

    #[test]
    fn rdw_writer_writes_record() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        let record = RDWRecord::try_new(b"test".to_vec()).unwrap();
        writer.write_record(&record).unwrap();
        assert_eq!(writer.record_count(), 1);
        assert_eq!(output, vec![0, 4, 0, 0, b't', b'e', b's', b't']);
    }

    #[test]
    fn rdw_writer_writes_record_from_payload_with_reserved() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        writer
            .write_record_from_payload(b"test", Some(0x1234))
            .unwrap();
        assert_eq!(output, vec![0, 4, 0x12, 0x34, b't', b'e', b's', b't']);
    }

    #[test]
    fn rdw_reader_reads_single_record() {
        let data = vec![0, 5, 0, 0, b'h', b'e', b'l', b'l', b'o'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);

        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.length(), 5);
        assert_eq!(record.reserved(), 0);
        assert_eq!(record.payload, b"hello");
        assert_eq!(reader.record_count(), 1);
    }

    #[test]
    fn rdw_reader_reads_multiple_records() {
        let data = vec![
            0, 2, 0, 0, b'h', b'i', //
            0, 3, 0, 0, b'b', b'y', b'e',
        ];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);

        let first = reader.read_record().unwrap().unwrap();
        assert_eq!(first.payload, b"hi");
        assert_eq!(reader.record_count(), 1);

        let second = reader.read_record().unwrap().unwrap();
        assert_eq!(second.payload, b"bye");
        assert_eq!(reader.record_count(), 2);

        assert!(reader.read_record().unwrap().is_none());
    }

    #[test]
    fn rdw_reader_reserved_nonzero_is_warning_in_lenient_mode() {
        let data = vec![0, 4, 0x12, 0x34, b't', b'e', b's', b't'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);

        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.reserved(), 0x1234);
        assert_eq!(record.payload, b"test");
    }

    #[test]
    fn rdw_reader_reserved_nonzero_is_error_in_strict_mode() {
        let data = vec![0, 4, 0x12, 0x34, b't', b'e', b's', b't'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), true);

        let error = reader.read_record().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
    }

    #[test]
    fn rdw_reader_incomplete_header_lenient_is_eof() {
        let data = vec![0, 4];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        let result = reader.read_record().unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn rdw_reader_incomplete_header_strict_is_underflow() {
        let data = vec![0, 4];
        let mut reader = RDWRecordReader::new(Cursor::new(data), true);
        let error = reader.read_record().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    }

    #[test]
    fn rdw_reader_incomplete_payload_is_cbkf102() {
        let data = vec![0, 5, 0, 0, b'h', b'i'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);

        let error = reader.read_record().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    }

    #[test]
    fn rdw_reader_ascii_corruption_is_detected() {
        let data = vec![b'1', b'2', 0, 0, b'H', b'E', b'L', b'L', b'O'];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);

        let error = reader.read_record().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    }

    #[test]
    fn rdw_reader_zero_length_validation_obeys_schema_prefix() {
        use copybook_core::{Field, FieldKind, Occurs, Schema, TailODO};

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

        let reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
        let error = reader.validate_zero_length_record(&schema).unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);

        let empty_schema = Schema::new();
        reader.validate_zero_length_record(&empty_schema).unwrap();
    }

    #[test]
    fn rdw_writer_payload_too_large_is_cbke501() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        let large_payload = vec![0u8; usize::from(u16::MAX) + 1];
        let err = writer
            .write_record_from_payload(&large_payload, None)
            .unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    }

    #[test]
    fn rdw_record_oversize_try_new_is_cbkf102() {
        let large_payload = vec![0u8; usize::from(u16::MAX) + 1];
        let err = RDWRecord::try_new(large_payload).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(err.message.contains("RDW payload too large"));
    }

    #[test]
    fn rdw_record_oversize_try_with_reserved_is_cbkf102() {
        let large_payload = vec![0u8; usize::from(u16::MAX) + 1];
        let err = RDWRecord::try_with_reserved(large_payload, 0x1234).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(err.message.contains("RDW payload too large"));
    }

    #[test]
    fn rdw_record_oversize_try_recompute_is_cbkf102() {
        let mut record = RDWRecord::try_new(b"test".to_vec()).unwrap();
        record.payload = vec![0u8; usize::from(u16::MAX) + 1];
        let err = record.try_recompute_length().unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(err.message.contains("RDW payload too large"));
    }

    #[test]
    #[should_panic(expected = "RDW payload exceeds maximum size")]
    #[allow(deprecated)]
    fn rdw_record_new_panics_on_oversize_payload() {
        let payload = vec![0u8; usize::from(u16::MAX) + 1];
        let _ = RDWRecord::new(payload);
    }

    proptest! {
        #[test]
        fn prop_header_payload_len_roundtrip(payload_len in 0u16..=u16::MAX, reserved in any::<u16>()) {
            let header = RdwHeader::from_payload_len(payload_len as usize, reserved).unwrap();
            prop_assert_eq!(header.length(), payload_len);
            prop_assert_eq!(header.reserved(), reserved);
            prop_assert_eq!(RdwHeader::from_bytes(header.bytes()).length(), payload_len);
        }

        #[test]
        fn prop_ascii_corruption_heuristic_matches_manual(b0 in any::<u8>(), b1 in any::<u8>(), b2 in any::<u8>(), b3 in any::<u8>()) {
            let header = [b0, b1, b2, b3];
            let expected = (b'0'..=b'9').contains(&b0) && (b'0'..=b'9').contains(&b1);
            prop_assert_eq!(rdw_is_suspect_ascii_corruption(header), expected);
            prop_assert_eq!(RdwHeader::from_bytes(header).looks_ascii_corrupt(), expected);
        }

        #[test]
        fn prop_rdw_record_length_matches_payload(payload in vec(any::<u8>(), 0..=1024), reserved in any::<u16>()) {
            let record = RDWRecord::try_with_reserved(payload.clone(), reserved).unwrap();
            prop_assert_eq!(usize::from(record.length()), payload.len());
            prop_assert_eq!(record.reserved(), reserved);
            let bytes = record.as_bytes();
            prop_assert_eq!(bytes.len(), RDW_HEADER_LEN + payload.len());
            prop_assert_eq!(&bytes[RDW_HEADER_LEN..], payload.as_slice());
        }

        #[test]
        fn prop_rdw_writer_from_payload_encodes_header(payload in vec(any::<u8>(), 0..=512), reserved in any::<u16>()) {
            let mut output = Vec::new();
            let mut writer = RDWRecordWriter::new(&mut output);
            writer.write_record_from_payload(&payload, Some(reserved)).unwrap();
            prop_assert_eq!(writer.record_count(), 1);
            let header = RdwHeader::from_bytes(output[0..RDW_HEADER_LEN].try_into().unwrap());
            prop_assert_eq!(usize::from(header.length()), payload.len());
            prop_assert_eq!(header.reserved(), reserved);
            prop_assert_eq!(&output[RDW_HEADER_LEN..], payload.as_slice());
        }

        #[test]
        fn prop_rdw_writer_reader_roundtrip(
            payload in vec(any::<u8>(), 0..=1024),
            reserved in any::<u16>(),
        ) {
            let mut encoded = Vec::new();
            let mut writer = RDWRecordWriter::new(&mut encoded);
            writer.write_record_from_payload(&payload, Some(reserved)).unwrap();

            let mut reader = RDWRecordReader::new(Cursor::new(encoded), false);
            let decoded = reader.read_record().unwrap().unwrap();
            prop_assert_eq!(decoded.payload.as_slice(), payload.as_slice());
            prop_assert_eq!(decoded.reserved(), reserved);
            prop_assert!(reader.read_record().unwrap().is_none());
        }
    }
}
