#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW (Record Descriptor Word) header primitives.
//!
//! This crate intentionally focuses on one concern:
//! parsing and constructing RDW framing metadata plus minimal buffered helpers.

use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use std::io::{BufRead, Write};
use tracing::debug;

/// Size of an RDW header in bytes.
pub const RDW_HEADER_LEN: usize = 4;

/// Maximum payload size representable in RDW (`u16::MAX`).
pub const RDW_MAX_PAYLOAD_LEN: usize = u16::MAX as usize;

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
    let b0 = rdw_header[0];
    let b1 = rdw_header[1];
    (b'0' <= b0 && b0 <= b'9') && (b'0' <= b1 && b1 <= b'9')
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
    }
}
