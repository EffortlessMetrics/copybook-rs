// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep integration tests for fixed-length record framing: exact LRECL read/write,
//! short record padding, long record rejection, multi-record streaming, empty file,
//! large records, record count accuracy, and writer flush behaviour.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::{self, Cursor, Read, Write};

// =========================================================================
// Helpers
// =========================================================================

/// A writer that tracks flush calls.
struct FlushTracker {
    buf: Vec<u8>,
    flush_count: usize,
}

impl FlushTracker {
    fn new() -> Self {
        Self {
            buf: Vec::new(),
            flush_count: 0,
        }
    }
}

impl Write for FlushTracker {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.buf.extend_from_slice(data);
        Ok(data.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        self.flush_count += 1;
        Ok(())
    }
}

struct AlwaysFailReader;
impl Read for AlwaysFailReader {
    fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
        Err(io::Error::new(io::ErrorKind::BrokenPipe, "broken"))
    }
}

struct AlwaysFailWriter;
impl Write for AlwaysFailWriter {
    fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
        Err(io::Error::new(io::ErrorKind::BrokenPipe, "broken"))
    }
    fn flush(&mut self) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::BrokenPipe, "broken"))
    }
}

/// Writer that fails on flush but succeeds on write.
struct FlushFailWriter {
    buf: Vec<u8>,
}

impl FlushFailWriter {
    fn new() -> Self {
        Self { buf: Vec::new() }
    }
}

impl Write for FlushFailWriter {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.buf.extend_from_slice(data);
        Ok(data.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::BrokenPipe, "flush failure"))
    }
}

// =========================================================================
// 1. Exact LRECL read/write
// =========================================================================

#[test]
fn exact_lrecl_read_single_record() {
    let data = b"EXACT_80B_RECORD____";
    let mut r = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(20)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert_eq!(rec, data.as_slice());
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn exact_lrecl_write_no_padding() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(10)).unwrap();
    w.write_record(b"0123456789").unwrap();
    w.flush().unwrap();
    assert_eq!(out, b"0123456789");
}

#[test]
fn exact_lrecl_roundtrip_various_sizes() {
    for lrecl in [1u32, 2, 4, 8, 16, 80, 132, 256, 512, 1024] {
        let payload: Vec<u8> = (0..lrecl).map(|i| (i % 256) as u8).collect();
        let mut wire = Vec::new();
        {
            let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
            w.write_record(&payload).unwrap();
            w.flush().unwrap();
        }
        assert_eq!(wire.len(), lrecl as usize);

        let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
        let rec = r.read_record().unwrap().unwrap();
        assert_eq!(rec, payload, "failed for lrecl={lrecl}");
    }
}

#[test]
fn exact_lrecl_multi_record_read() {
    let data = b"AAABBBCCCDDD";
    let mut r = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(3)).unwrap();
    assert_eq!(r.read_record().unwrap().unwrap(), b"AAA");
    assert_eq!(r.read_record().unwrap().unwrap(), b"BBB");
    assert_eq!(r.read_record().unwrap().unwrap(), b"CCC");
    assert_eq!(r.read_record().unwrap().unwrap(), b"DDD");
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn exact_lrecl_preserves_all_byte_values() {
    let payload: Vec<u8> = (0..=255).collect();
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(256)).unwrap();
        w.write_record(&payload).unwrap();
        w.flush().unwrap();
    }
    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(256)).unwrap();
    assert_eq!(r.read_record().unwrap().unwrap(), payload);
}

#[test]
fn exact_lrecl_preserves_null_bytes() {
    let payload = vec![0u8; 64];
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(64)).unwrap();
        w.write_record(&payload).unwrap();
        w.flush().unwrap();
    }
    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(64)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert!(rec.iter().all(|&b| b == 0));
}

#[test]
fn exact_lrecl_preserves_0xff_bytes() {
    let payload = vec![0xFF; 32];
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(32)).unwrap();
        w.write_record(&payload).unwrap();
        w.flush().unwrap();
    }
    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(32)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert!(rec.iter().all(|&b| b == 0xFF));
}

// =========================================================================
// 2. Short record handling (padding)
// =========================================================================

#[test]
fn short_record_padded_with_null() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(8)).unwrap();
    w.write_record(b"AB").unwrap();
    w.flush().unwrap();
    assert_eq!(out, b"AB\x00\x00\x00\x00\x00\x00");
}

#[test]
fn short_record_one_byte_in_lrecl_10() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(10)).unwrap();
    w.write_record(b"X").unwrap();
    w.flush().unwrap();
    assert_eq!(out.len(), 10);
    assert_eq!(out[0], b'X');
    assert!(out[1..].iter().all(|&b| b == 0));
}

#[test]
fn short_record_empty_payload_full_padding() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(16)).unwrap();
    w.write_record(b"").unwrap();
    w.flush().unwrap();
    assert_eq!(out.len(), 16);
    assert!(out.iter().all(|&b| b == 0));
}

#[test]
fn short_record_roundtrip_preserves_data_prefix() {
    let lrecl = 20u32;
    let payload = b"HELLO";
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(payload).unwrap();
        w.flush().unwrap();
    }
    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert_eq!(&rec[..5], b"HELLO");
    assert!(rec[5..].iter().all(|&b| b == 0));
}

#[test]
fn short_record_padding_amounts() {
    for (data_len, lrecl) in [(0, 1), (1, 2), (3, 10), (0, 100), (99, 100)] {
        let payload = vec![0xAA; data_len];
        let mut out = Vec::new();
        let mut w = FixedRecordWriter::new(&mut out, Some(lrecl as u32)).unwrap();
        w.write_record(&payload).unwrap();
        w.flush().unwrap();
        assert_eq!(
            out.len(),
            lrecl,
            "output len wrong for data={data_len}, lrecl={lrecl}"
        );
        assert_eq!(&out[..data_len], payload.as_slice());
        assert!(
            out[data_len..].iter().all(|&b| b == 0),
            "padding not null for data={data_len}, lrecl={lrecl}"
        );
    }
}

#[test]
fn short_record_lrecl_minus_one() {
    let lrecl = 50u32;
    let payload = vec![0xBB; 49];
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(lrecl)).unwrap();
    w.write_record(&payload).unwrap();
    w.flush().unwrap();
    assert_eq!(out.len(), 50);
    assert_eq!(&out[..49], payload.as_slice());
    assert_eq!(out[49], 0);
}

// =========================================================================
// 3. Long record handling (truncation or error)
// =========================================================================

#[test]
fn long_record_rejected_with_error_code() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(4)).unwrap();
    let err = w.write_record(b"TOOLONG").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn long_record_one_byte_over_rejected() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(5)).unwrap();
    let err = w.write_record(b"ABCDEF").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn long_record_does_not_write_partial_data() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(3)).unwrap();
    assert!(w.write_record(b"ABCDEFGH").is_err());
    assert!(
        out.is_empty(),
        "no data should be written for rejected record"
    );
}

#[test]
fn long_record_does_not_increment_count() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(4)).unwrap();
    assert!(w.write_record(b"ABCDEFGH").is_err());
    assert_eq!(w.record_count(), 0);
}

#[test]
fn long_record_writer_remains_usable() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(4)).unwrap();
    assert!(w.write_record(b"TOOLONG").is_err());
    w.write_record(b"OK").unwrap();
    w.flush().unwrap();
    assert_eq!(w.record_count(), 1);
    assert_eq!(out.len(), 4);
}

#[test]
fn long_record_various_excess_sizes() {
    for (lrecl, excess) in [(1, 1), (4, 1), (10, 100), (80, 1)] {
        let mut out = Vec::new();
        let mut w = FixedRecordWriter::new(&mut out, Some(lrecl)).unwrap();
        let payload = vec![0x41; lrecl as usize + excess];
        let err = w.write_record(&payload).unwrap_err();
        assert_eq!(
            err.code,
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            "wrong error for lrecl={lrecl}, excess={excess}"
        );
    }
}

// =========================================================================
// 4. Multi-record streaming
// =========================================================================

#[test]
fn multi_record_write_read_3_records() {
    let lrecl = 12u32;
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(b"FIRST_REC___").unwrap();
        w.write_record(b"SECOND______").unwrap();
        w.write_record(b"THIRD_REC___").unwrap();
        w.flush().unwrap();
    }
    assert_eq!(wire.len(), 36);

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    assert_eq!(r.read_record().unwrap().unwrap(), b"FIRST_REC___");
    assert_eq!(r.read_record().unwrap().unwrap(), b"SECOND______");
    assert_eq!(r.read_record().unwrap().unwrap(), b"THIRD_REC___");
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn multi_record_mixed_sizes_with_padding() {
    let lrecl = 10u32;
    let payloads: &[&[u8]] = &[b"FULL_RECRD", b"SHORT", b"", b"0123456789"];
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for &p in payloads {
            w.write_record(p).unwrap();
        }
        w.flush().unwrap();
    }
    assert_eq!(wire.len(), 40);

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    for &expected in payloads {
        let rec = r.read_record().unwrap().unwrap();
        assert_eq!(&rec[..expected.len()], expected);
        assert!(rec[expected.len()..].iter().all(|&b| b == 0));
    }
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn multi_record_100_records_sequential_content() {
    let lrecl = 8u32;
    let count = 100;
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for i in 0u8..count {
            let payload = [i; 8];
            w.write_record(&payload).unwrap();
        }
        w.flush().unwrap();
    }

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    for i in 0u8..count {
        let rec = r.read_record().unwrap().unwrap();
        assert!(rec.iter().all(|&b| b == i), "record {i} content wrong");
    }
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn multi_record_output_size_always_n_times_lrecl() {
    for lrecl in [1u32, 5, 80, 256] {
        for count in [0, 1, 3, 10, 50] {
            let mut wire = Vec::new();
            {
                let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
                for _ in 0..count {
                    w.write_record(b"").unwrap();
                }
                w.flush().unwrap();
            }
            assert_eq!(
                wire.len(),
                lrecl as usize * count,
                "size wrong for lrecl={lrecl}, count={count}"
            );
        }
    }
}

// =========================================================================
// 5. Empty file handling
// =========================================================================

#[test]
fn empty_file_read_returns_none() {
    let mut r = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(80)).unwrap();
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn empty_file_record_count_stays_zero() {
    let mut r = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(80)).unwrap();
    r.read_record().unwrap();
    assert_eq!(r.record_count(), 0);
}

#[test]
fn empty_file_repeated_reads_all_none() {
    let mut r = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(10)).unwrap();
    for _ in 0..20 {
        assert!(r.read_record().unwrap().is_none());
    }
    assert_eq!(r.record_count(), 0);
}

#[test]
fn empty_writer_flush_produces_nothing() {
    let mut out = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut out, Some(80)).unwrap();
        w.flush().unwrap();
    }
    assert!(out.is_empty());
}

#[test]
fn empty_writer_record_count_is_zero() {
    let mut out = Vec::new();
    let w = FixedRecordWriter::new(&mut out, Some(10)).unwrap();
    assert_eq!(w.record_count(), 0);
}

// =========================================================================
// 6. Large record sizes (32KB)
// =========================================================================

#[test]
fn large_record_32kb_exact_roundtrip() {
    let lrecl = 32_768u32;
    let payload: Vec<u8> = (0..lrecl).map(|i| (i % 256) as u8).collect();
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(&payload).unwrap();
        w.flush().unwrap();
    }
    assert_eq!(wire.len(), lrecl as usize);

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert_eq!(rec, payload);
}

#[test]
fn large_record_32kb_partial_payload_padded() {
    let lrecl = 32_768u32;
    let payload = vec![0xAA; 100];
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(&payload).unwrap();
        w.flush().unwrap();
    }
    assert_eq!(wire.len(), lrecl as usize);

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert_eq!(&rec[..100], payload.as_slice());
    assert!(rec[100..].iter().all(|&b| b == 0));
}

#[test]
fn large_record_32kb_multiple_records() {
    let lrecl = 32_768u32;
    let count = 3;
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for i in 0u8..count {
            let payload = vec![i; lrecl as usize];
            w.write_record(&payload).unwrap();
        }
        w.flush().unwrap();
    }
    assert_eq!(wire.len(), lrecl as usize * count as usize);

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    for i in 0u8..count {
        let rec = r.read_record().unwrap().unwrap();
        assert!(rec.iter().all(|&b| b == i));
    }
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn large_record_64kb_roundtrip() {
    let lrecl = 65_536u32;
    let payload = vec![0xDD; lrecl as usize];
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(&payload).unwrap();
        w.flush().unwrap();
    }
    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert_eq!(rec, payload);
}

#[test]
fn large_record_100k_with_small_data() {
    let lrecl = 100_000u32;
    let payload = vec![0xEE; 10];
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(&payload).unwrap();
        w.flush().unwrap();
    }
    assert_eq!(wire.len(), lrecl as usize);

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert_eq!(&rec[..10], payload.as_slice());
    assert!(rec[10..].iter().all(|&b| b == 0));
}

// =========================================================================
// 7. Record count accuracy
// =========================================================================

#[test]
fn record_count_reader_starts_at_zero() {
    let r = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(10)).unwrap();
    assert_eq!(r.record_count(), 0);
}

#[test]
fn record_count_writer_starts_at_zero() {
    let mut out = Vec::new();
    let w = FixedRecordWriter::new(&mut out, Some(10)).unwrap();
    assert_eq!(w.record_count(), 0);
}

#[test]
fn record_count_reader_increments_exactly() {
    let data = vec![0x41; 50];
    let mut r = FixedRecordReader::new(Cursor::new(&data), Some(10)).unwrap();
    for expected in 1..=5u64 {
        r.read_record().unwrap().unwrap();
        assert_eq!(r.record_count(), expected);
    }
}

#[test]
fn record_count_writer_increments_exactly() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(5)).unwrap();
    for expected in 1..=7u64 {
        w.write_record(b"12345").unwrap();
        assert_eq!(w.record_count(), expected);
    }
}

#[test]
fn record_count_reader_unchanged_at_eof() {
    let data = b"ABCD";
    let mut r = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(4)).unwrap();
    r.read_record().unwrap().unwrap();
    assert_eq!(r.record_count(), 1);
    r.read_record().unwrap(); // EOF
    assert_eq!(r.record_count(), 1);
}

#[test]
fn record_count_writer_unchanged_on_error() {
    let mut out = Vec::new();
    let mut w = FixedRecordWriter::new(&mut out, Some(3)).unwrap();
    w.write_record(b"OK_").unwrap();
    assert_eq!(w.record_count(), 1);
    assert!(w.write_record(b"TOOLONG").is_err());
    assert_eq!(w.record_count(), 1);
}

#[test]
fn record_count_reader_1000_records() {
    let lrecl = 4u32;
    let count = 1_000u64;
    let data = vec![0x42; lrecl as usize * count as usize];
    let mut r = FixedRecordReader::new(Cursor::new(&data), Some(lrecl)).unwrap();
    while r.read_record().unwrap().is_some() {}
    assert_eq!(r.record_count(), count);
}

#[test]
fn record_count_writer_1000_records() {
    let mut out = Vec::new();
    let count = 1_000u64;
    let mut w = FixedRecordWriter::new(&mut out, Some(4)).unwrap();
    for _ in 0..count {
        w.write_record(b"TEST").unwrap();
    }
    w.flush().unwrap();
    assert_eq!(w.record_count(), count);
}

// =========================================================================
// 8. Writer flush behaviour
// =========================================================================

#[test]
fn flush_is_forwarded_to_underlying_writer() {
    let mut tracker = FlushTracker::new();
    {
        let mut w = FixedRecordWriter::new(&mut tracker, Some(4)).unwrap();
        w.write_record(b"ABCD").unwrap();
        w.flush().unwrap();
    }
    assert_eq!(tracker.flush_count, 1);
}

#[test]
fn multiple_flushes_all_forwarded() {
    let mut tracker = FlushTracker::new();
    {
        let mut w = FixedRecordWriter::new(&mut tracker, Some(4)).unwrap();
        w.write_record(b"ABCD").unwrap();
        w.flush().unwrap();
        w.flush().unwrap();
        w.flush().unwrap();
    }
    assert_eq!(tracker.flush_count, 3);
}

#[test]
fn flush_without_writes_is_safe() {
    let mut tracker = FlushTracker::new();
    {
        let mut w = FixedRecordWriter::new(&mut tracker, Some(10)).unwrap();
        w.flush().unwrap();
    }
    assert_eq!(tracker.flush_count, 1);
    assert!(tracker.buf.is_empty());
}

#[test]
fn flush_failure_returns_error() {
    let mut fail_flusher = FlushFailWriter::new();
    let mut w = FixedRecordWriter::new(&mut fail_flusher, Some(4)).unwrap();
    w.write_record(b"DATA").unwrap();
    let err = w.flush();
    assert!(err.is_err());
}

#[test]
fn data_visible_after_write_and_flush_increments() {
    let mut tracker = FlushTracker::new();
    {
        let mut w = FixedRecordWriter::new(&mut tracker, Some(4)).unwrap();
        w.write_record(b"ABCD").unwrap();
        w.flush().unwrap();
    }
    // After dropping writer, verify data was written and flush occurred
    assert_eq!(tracker.buf, b"ABCD");
    assert_eq!(tracker.flush_count, 1);
}

#[test]
fn flush_interleaved_with_writes() {
    let mut tracker = FlushTracker::new();
    {
        let mut w = FixedRecordWriter::new(&mut tracker, Some(2)).unwrap();
        w.write_record(b"AA").unwrap();
        w.flush().unwrap();
        w.write_record(b"BB").unwrap();
        w.flush().unwrap();
    }
    assert_eq!(tracker.flush_count, 2);
    assert_eq!(tracker.buf, b"AABB");
}

// =========================================================================
// 9. Construction validation
// =========================================================================

#[test]
fn reader_rejects_zero_lrecl() {
    let err = FixedRecordReader::new(Cursor::new(b"x".as_slice()), Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn reader_rejects_none_lrecl() {
    let err = FixedRecordReader::new(Cursor::new(b"x".as_slice()), None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn writer_rejects_zero_lrecl() {
    let mut out = Vec::new();
    let err = FixedRecordWriter::new(&mut out, Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn writer_rejects_none_lrecl() {
    let mut out = Vec::new();
    let err = FixedRecordWriter::new(&mut out, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

// =========================================================================
// 10. LRECL accessor
// =========================================================================

#[test]
fn reader_lrecl_accessor_returns_configured_value() {
    for lrecl in [1u32, 80, 256, 32768, 100_000] {
        let r = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(lrecl)).unwrap();
        assert_eq!(r.lrecl(), lrecl);
    }
}

#[test]
fn writer_lrecl_accessor_returns_configured_value() {
    for lrecl in [1u32, 80, 256, 32768, 100_000] {
        let mut out = Vec::new();
        let w = FixedRecordWriter::new(&mut out, Some(lrecl)).unwrap();
        assert_eq!(w.lrecl(), lrecl);
    }
}

// =========================================================================
// 11. I/O error propagation
// =========================================================================

#[test]
fn read_io_error_propagates() {
    let mut r = FixedRecordReader::new(AlwaysFailReader, Some(10)).unwrap();
    assert!(r.read_record().is_err());
}

#[test]
fn write_io_error_propagates() {
    let mut w = FixedRecordWriter::new(AlwaysFailWriter, Some(4)).unwrap();
    assert!(w.write_record(b"FAIL").is_err());
}

#[test]
fn flush_io_error_propagates() {
    let mut w = FixedRecordWriter::new(AlwaysFailWriter, Some(4)).unwrap();
    assert!(w.flush().is_err());
}

// =========================================================================
// 12. Partial/truncated record error handling
// =========================================================================

#[test]
fn truncated_single_byte_with_large_lrecl() {
    let mut r = FixedRecordReader::new(Cursor::new(vec![0x42u8]), Some(1000)).unwrap();
    let err = r.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn truncated_after_good_records_returns_error() {
    // 3 good records of 4 bytes + 2 leftover bytes
    let data = vec![0x41; 14];
    let mut r = FixedRecordReader::new(Cursor::new(&data), Some(4)).unwrap();
    for _ in 0..3 {
        r.read_record().unwrap().unwrap();
    }
    let err = r.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    assert_eq!(r.record_count(), 3);
}

#[test]
fn truncated_lrecl_minus_one_bytes() {
    let data = vec![0x43; 79]; // LRECL=80, only 79 bytes
    let mut r = FixedRecordReader::new(Cursor::new(&data), Some(80)).unwrap();
    let err = r.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// =========================================================================
// 13. EBCDIC-like binary content preservation
// =========================================================================

#[test]
fn ebcdic_content_roundtrip() {
    // CP037 EBCDIC for "HELLO WORLD"
    let ebcdic = [
        0xC8u8, 0xC5, 0xD3, 0xD3, 0xD6, 0x40, 0xE6, 0xD6, 0xD9, 0xD3, 0xC4,
    ];
    let lrecl = 16u32;
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(&ebcdic).unwrap();
        w.flush().unwrap();
    }
    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    assert_eq!(&rec[..11], &ebcdic);
    assert!(rec[11..].iter().all(|&b| b == 0));
}

// =========================================================================
// 14. Validate record length
// =========================================================================

#[test]
fn validate_record_length_matching_ok() {
    let data = b"ABCDEFGH";
    let mut r = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(8)).unwrap();
    let rec = r.read_record().unwrap().unwrap();
    let schema = copybook_core::Schema::new();
    r.validate_record_length(&schema, &rec).unwrap();
}

#[test]
fn validate_record_length_mismatch_errors() {
    let data = b"ABCDEFGH";
    let mut r = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(8)).unwrap();
    let _ = r.read_record().unwrap().unwrap();
    let schema = copybook_core::Schema::new();
    let err = r.validate_record_length(&schema, b"SHORT").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// =========================================================================
// 15. Streaming 10,000 records
// =========================================================================

#[test]
fn streaming_10000_records_count_correct() {
    let lrecl = 20u32;
    let count = 10_000u64;
    let payload = vec![0x20; lrecl as usize];

    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for _ in 0..count {
            w.write_record(&payload).unwrap();
        }
        w.flush().unwrap();
        assert_eq!(w.record_count(), count);
    }

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let mut n = 0u64;
    while r.read_record().unwrap().is_some() {
        n += 1;
    }
    assert_eq!(n, count);
    assert_eq!(r.record_count(), count);
}

#[test]
fn streaming_indexed_records_preserve_order() {
    let lrecl = 16u32;
    let count = 500u64;
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for i in 0..count {
            let mut payload = vec![0u8; lrecl as usize];
            payload[..8].copy_from_slice(&i.to_be_bytes());
            w.write_record(&payload).unwrap();
        }
        w.flush().unwrap();
    }

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    for expected_idx in 0..count {
        let rec = r.read_record().unwrap().unwrap();
        let idx = u64::from_be_bytes(rec[..8].try_into().unwrap());
        assert_eq!(idx, expected_idx);
    }
    assert!(r.read_record().unwrap().is_none());
}
