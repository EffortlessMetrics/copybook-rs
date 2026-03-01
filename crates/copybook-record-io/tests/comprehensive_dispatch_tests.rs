// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive integration tests for record-io dispatch covering
//! fixed-length reading, RDW reading, error handling, and edge cases.

use copybook_error::ErrorCode;
use copybook_options::RecordFormat;
use copybook_record_io::{read_record, write_record};
use std::io::{self, Cursor, Read, Write};

// ===========================================================================
// Fixed-length dispatch
// ===========================================================================

#[test]
fn fixed_reads_multiple_consecutive_records() {
    let data = b"AAABBBCCC";
    let mut cursor = Cursor::new(data.to_vec());

    let r1 = read_record(&mut cursor, RecordFormat::Fixed, Some(3))
        .unwrap()
        .unwrap();
    let r2 = read_record(&mut cursor, RecordFormat::Fixed, Some(3))
        .unwrap()
        .unwrap();
    let r3 = read_record(&mut cursor, RecordFormat::Fixed, Some(3))
        .unwrap()
        .unwrap();

    assert_eq!(r1, b"AAA");
    assert_eq!(r2, b"BBB");
    assert_eq!(r3, b"CCC");

    // EOF after all records consumed
    assert!(
        read_record(&mut cursor, RecordFormat::Fixed, Some(3))
            .unwrap()
            .is_none()
    );
}

#[test]
fn fixed_single_byte_lrecl() {
    let mut cursor = Cursor::new(vec![0x42]);
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(1))
        .unwrap()
        .unwrap();
    assert_eq!(rec, vec![0x42]);
}

#[test]
fn fixed_large_lrecl_exact() {
    let data = vec![0xAB; 4096];
    let mut cursor = Cursor::new(data.clone());
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(4096))
        .unwrap()
        .unwrap();
    assert_eq!(rec.len(), 4096);
    assert!(rec.iter().all(|&b| b == 0xAB));
}

#[test]
fn fixed_missing_lrecl_returns_invalid_state_error() {
    let mut cursor = Cursor::new(b"DATA".to_vec());
    let err = read_record(&mut cursor, RecordFormat::Fixed, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn fixed_truncated_input_returns_error() {
    let mut cursor = Cursor::new(b"AB".to_vec());
    let result = read_record(&mut cursor, RecordFormat::Fixed, Some(10));
    assert!(result.is_err(), "partial record should error");
}

// ===========================================================================
// RDW dispatch
// ===========================================================================

#[test]
fn rdw_reads_zero_length_payload() {
    // RDW header with length=0 and reserved=0
    let mut cursor = Cursor::new(vec![0x00, 0x00, 0x00, 0x00]);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert!(rec.is_empty());
}

#[test]
fn rdw_reads_multiple_variable_length_records() {
    let mut data = Vec::new();
    write_record(&mut data, b"A", RecordFormat::RDW).unwrap();
    write_record(&mut data, b"BB", RecordFormat::RDW).unwrap();
    write_record(&mut data, b"CCC", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    let r1 = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    let r2 = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    let r3 = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();

    assert_eq!(r1, b"A");
    assert_eq!(r2, b"BB");
    assert_eq!(r3, b"CCC");

    assert!(
        read_record(&mut cursor, RecordFormat::RDW, None)
            .unwrap()
            .is_none()
    );
}

#[test]
fn rdw_ignores_lrecl_hint() {
    let mut data = Vec::new();
    write_record(&mut data, b"HELLO", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    // lrecl=1 should be ignored for RDW format
    let rec = read_record(&mut cursor, RecordFormat::RDW, Some(1))
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"HELLO");
}

#[test]
fn rdw_truncated_header_returns_error() {
    // Only 2 bytes instead of 4-byte RDW header
    let mut cursor = Cursor::new(vec![0x00, 0x05]);
    let result = read_record(&mut cursor, RecordFormat::RDW, None);
    assert!(result.is_err());
}

#[test]
fn rdw_empty_stream_returns_none() {
    let mut cursor = Cursor::new(Vec::<u8>::new());
    let result = read_record(&mut cursor, RecordFormat::RDW, None).unwrap();
    assert!(result.is_none());
}

// ===========================================================================
// Write dispatch
// ===========================================================================

#[test]
fn write_fixed_produces_exact_bytes() {
    let mut out = Vec::new();
    write_record(&mut out, b"HELLO WORLD", RecordFormat::Fixed).unwrap();
    assert_eq!(out, b"HELLO WORLD");
}

#[test]
fn write_rdw_produces_header_plus_payload() {
    let mut out = Vec::new();
    write_record(&mut out, b"TEST", RecordFormat::RDW).unwrap();
    // Header: 2 bytes length (big-endian) + 2 bytes reserved (0x0000)
    assert_eq!(out.len(), 8); // 4 header + 4 payload
    assert_eq!(&out[4..], b"TEST");
}

#[test]
fn write_rdw_rejects_oversized_payload() {
    let mut out = Vec::new();
    let oversized = vec![0u8; usize::from(u16::MAX) + 1];
    let err = write_record(&mut out, &oversized, RecordFormat::RDW).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ===========================================================================
// Roundtrip tests
// ===========================================================================

#[test]
fn roundtrip_fixed_various_sizes() {
    for size in [1, 4, 80, 256, 1024] {
        let payload: Vec<u8> = (0..size).map(|i| (i % 256) as u8).collect();
        let mut buf = Vec::new();
        write_record(&mut buf, &payload, RecordFormat::Fixed).unwrap();

        let mut cursor = Cursor::new(buf);
        let decoded = read_record(
            &mut cursor,
            RecordFormat::Fixed,
            Some(u32::try_from(payload.len()).unwrap()),
        )
        .unwrap()
        .unwrap();
        assert_eq!(decoded, payload, "failed for size={size}");
    }
}

#[test]
fn roundtrip_rdw_various_sizes() {
    for size in [0, 1, 4, 80, 256, 1024, 8000] {
        let payload: Vec<u8> = (0..size).map(|i| (i % 256) as u8).collect();
        let mut buf = Vec::new();
        write_record(&mut buf, &payload, RecordFormat::RDW).unwrap();

        let mut cursor = Cursor::new(buf);
        let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
            .unwrap()
            .unwrap();
        assert_eq!(decoded, payload, "failed for size={size}");
    }
}

#[test]
fn roundtrip_mixed_formats_do_not_cross() {
    // Write as Fixed, read as Fixed
    let mut fixed_buf = Vec::new();
    write_record(&mut fixed_buf, b"ABCD", RecordFormat::Fixed).unwrap();
    let mut cursor = Cursor::new(fixed_buf);
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(4))
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"ABCD");

    // Write as RDW, read as RDW
    let mut rdw_buf = Vec::new();
    write_record(&mut rdw_buf, b"EFGH", RecordFormat::RDW).unwrap();
    let mut cursor = Cursor::new(rdw_buf);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"EFGH");
}

// ===========================================================================
// I/O error propagation
// ===========================================================================

struct FailingReader;
impl Read for FailingReader {
    fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
        Err(io::Error::new(io::ErrorKind::BrokenPipe, "read failure"))
    }
}

struct FailingWriter;
impl Write for FailingWriter {
    fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
        Err(io::Error::new(io::ErrorKind::BrokenPipe, "write failure"))
    }
    fn flush(&mut self) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::BrokenPipe, "flush failure"))
    }
}

#[test]
fn read_fixed_io_error_propagates() {
    let mut r = FailingReader;
    assert!(read_record(&mut r, RecordFormat::Fixed, Some(4)).is_err());
}

#[test]
fn read_rdw_io_error_propagates() {
    let mut r = FailingReader;
    assert!(read_record(&mut r, RecordFormat::RDW, None).is_err());
}

#[test]
fn write_fixed_io_error_propagates() {
    let mut w = FailingWriter;
    assert!(write_record(&mut w, b"FAIL", RecordFormat::Fixed).is_err());
}

#[test]
fn write_rdw_io_error_propagates() {
    let mut w = FailingWriter;
    assert!(write_record(&mut w, b"FAIL", RecordFormat::RDW).is_err());
}

// ===========================================================================
// RecordFormat enum coverage
// ===========================================================================

#[test]
fn record_format_fixed_properties() {
    let fmt = RecordFormat::Fixed;
    assert!(fmt.is_fixed());
    assert!(!fmt.is_variable());
    assert!(!fmt.description().is_empty());
    assert_eq!(format!("{fmt}"), "fixed");
}

#[test]
fn record_format_rdw_properties() {
    let fmt = RecordFormat::RDW;
    assert!(!fmt.is_fixed());
    assert!(fmt.is_variable());
    assert!(!fmt.description().is_empty());
    assert_eq!(format!("{fmt}"), "rdw");
}

// ===========================================================================
// Binary content preservation
// ===========================================================================

#[test]
fn fixed_preserves_binary_content() {
    // Ensure all 256 byte values round-trip
    let payload: Vec<u8> = (0..=255).collect();
    let mut buf = Vec::new();
    write_record(&mut buf, &payload, RecordFormat::Fixed).unwrap();

    let mut cursor = Cursor::new(buf);
    let decoded = read_record(&mut cursor, RecordFormat::Fixed, Some(256))
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

#[test]
fn rdw_preserves_binary_content() {
    let payload: Vec<u8> = (0..=255).collect();
    let mut buf = Vec::new();
    write_record(&mut buf, &payload, RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(buf);
    let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}
