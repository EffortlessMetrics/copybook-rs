#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW (Record Descriptor Word) header primitives.
//!
//! This crate intentionally focuses on one concern:
//! parsing and constructing RDW framing metadata plus minimal buffered helpers.

use copybook_error::{Error, ErrorCode, Result};
use std::io::BufRead;

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

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
mod tests {
    use super::*;
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
    }
}
