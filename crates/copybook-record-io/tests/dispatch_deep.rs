// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep integration tests for record-io dispatch: format routing, reader/writer
//! creation, multi-record roundtrips, error handling, record counting, and
//! streaming memory-bounded behaviour.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_options::RecordFormat;
use copybook_record_io::{
    FixedRecordReader, FixedRecordWriter, RDWRecord, RDWRecordReader, RDWRecordWriter, read_record,
    write_record,
};
use std::io::{self, Cursor, Read, Write};

// =========================================================================
// Helpers
// =========================================================================

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

// =========================================================================
// 1. RecordFormat dispatch – Fixed vs RDW routing
// =========================================================================

#[test]
fn dispatch_fixed_routes_to_fixed_reader() {
    let data = b"ABCDEFGH";
    let mut c = Cursor::new(data.to_vec());
    let rec = read_record(&mut c, RecordFormat::Fixed, Some(8))
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"ABCDEFGH");
}

#[test]
fn dispatch_rdw_routes_to_rdw_reader() {
    let mut wire = Vec::new();
    write_record(&mut wire, b"TEST", RecordFormat::RDW).unwrap();
    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"TEST");
}

#[test]
fn dispatch_write_fixed_is_passthrough() {
    let mut out = Vec::new();
    write_record(&mut out, b"RAW", RecordFormat::Fixed).unwrap();
    assert_eq!(out, b"RAW");
}

#[test]
fn dispatch_write_rdw_adds_header() {
    let mut out = Vec::new();
    write_record(&mut out, b"AB", RecordFormat::RDW).unwrap();
    assert_eq!(out.len(), 6); // 4-byte header + 2 payload
    assert_eq!(&out[4..], b"AB");
}

#[test]
fn dispatch_format_enum_display() {
    assert_eq!(format!("{}", RecordFormat::Fixed), "fixed");
    assert_eq!(format!("{}", RecordFormat::RDW), "rdw");
}

#[test]
fn dispatch_format_is_fixed_vs_variable() {
    assert!(RecordFormat::Fixed.is_fixed());
    assert!(!RecordFormat::Fixed.is_variable());
    assert!(RecordFormat::RDW.is_variable());
    assert!(!RecordFormat::RDW.is_fixed());
}

// =========================================================================
// 2. Reader creation for each format
// =========================================================================

#[test]
fn create_fixed_reader_with_valid_lrecl() {
    let reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(80)).unwrap();
    assert_eq!(reader.lrecl(), 80);
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn create_fixed_reader_rejects_none_lrecl() {
    let err = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn create_fixed_reader_rejects_zero_lrecl() {
    let err = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn create_rdw_reader_lenient_mode() {
    let reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn create_rdw_reader_strict_mode() {
    let reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), true);
    assert_eq!(reader.record_count(), 0);
}

// =========================================================================
// 3. Writer creation for each format
// =========================================================================

#[test]
fn create_fixed_writer_with_valid_lrecl() {
    let mut buf = Vec::new();
    let writer = FixedRecordWriter::new(&mut buf, Some(100)).unwrap();
    assert_eq!(writer.lrecl(), 100);
    assert_eq!(writer.record_count(), 0);
}

#[test]
fn create_fixed_writer_rejects_none_lrecl() {
    let mut buf = Vec::new();
    let err = FixedRecordWriter::new(&mut buf, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn create_fixed_writer_rejects_zero_lrecl() {
    let mut buf = Vec::new();
    let err = FixedRecordWriter::new(&mut buf, Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn create_rdw_writer_succeeds() {
    let mut buf = Vec::new();
    let writer = RDWRecordWriter::new(&mut buf);
    assert_eq!(writer.record_count(), 0);
}

// =========================================================================
// 4. Multi-record read/write roundtrip
// =========================================================================

#[test]
fn roundtrip_fixed_10_records() {
    let lrecl = 16u32;
    let count = 10;
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for i in 0u8..count {
            let mut payload = vec![i; lrecl as usize];
            payload[0] = i;
            w.write_record(&payload).unwrap();
        }
        w.flush().unwrap();
        assert_eq!(w.record_count(), u64::from(count));
    }

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    for i in 0u8..count {
        let rec = r.read_record().unwrap().unwrap();
        assert_eq!(rec[0], i, "record {i} header byte mismatch");
    }
    assert!(r.read_record().unwrap().is_none());
    assert_eq!(r.record_count(), u64::from(count));
}

#[test]
fn roundtrip_rdw_10_variable_length_records() {
    let mut wire = Vec::new();
    {
        let mut w = RDWRecordWriter::new(&mut wire);
        for i in 1..=10u8 {
            let payload = vec![i; usize::from(i) * 10];
            w.write_record_from_payload(&payload, None).unwrap();
        }
        w.flush().unwrap();
        assert_eq!(w.record_count(), 10);
    }

    let mut r = RDWRecordReader::new(Cursor::new(&wire), false);
    for i in 1..=10u8 {
        let rec = r.read_record().unwrap().unwrap();
        assert_eq!(rec.payload.len(), usize::from(i) * 10);
        assert!(rec.payload.iter().all(|&b| b == i));
    }
    assert!(r.read_record().unwrap().is_none());
    assert_eq!(r.record_count(), 10);
}

#[test]
fn roundtrip_dispatch_fixed_multi() {
    let payloads: &[&[u8]] = &[b"AAAA", b"BBBB", b"CCCC"];
    let mut wire = Vec::new();
    for &p in payloads {
        write_record(&mut wire, p, RecordFormat::Fixed).unwrap();
    }

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(4)).unwrap();
    for &expected in payloads {
        let rec = r.read_record().unwrap().unwrap();
        assert_eq!(rec.as_slice(), expected);
    }
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_dispatch_rdw_multi() {
    let payloads: &[&[u8]] = &[b"X", b"YY", b"ZZZ", b"WWWW"];
    let mut wire = Vec::new();
    for &p in payloads {
        write_record(&mut wire, p, RecordFormat::RDW).unwrap();
    }

    let mut r = RDWRecordReader::new(Cursor::new(&wire), false);
    for &expected in payloads {
        let rec = r.read_record().unwrap().unwrap();
        assert_eq!(rec.payload, expected);
    }
    assert!(r.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_fixed_with_padding_preserves_data_prefix() {
    let lrecl = 20u32;
    let payloads: &[&[u8]] = &[b"SHORT", b"MEDIUM_DATA_12345", b""];
    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for &p in payloads {
            w.write_record(p).unwrap();
        }
        w.flush().unwrap();
    }

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    for &expected in payloads {
        let rec = r.read_record().unwrap().unwrap();
        assert_eq!(rec.len(), lrecl as usize);
        assert_eq!(&rec[..expected.len()], expected);
        assert!(rec[expected.len()..].iter().all(|&b| b == 0));
    }
}

#[test]
fn roundtrip_rdw_preserves_reserved_bytes() {
    let reserved = 0x1234u16;
    let mut wire = Vec::new();
    {
        let mut w = RDWRecordWriter::new(&mut wire);
        w.write_record_from_payload(b"DATA", Some(reserved))
            .unwrap();
        w.flush().unwrap();
    }

    let mut r = RDWRecordReader::new(Cursor::new(&wire), false);
    let rec = r.read_record().unwrap().unwrap();
    assert_eq!(rec.payload, b"DATA");
    assert_eq!(rec.reserved(), reserved);
}

// =========================================================================
// 5. Error handling
// =========================================================================

#[test]
fn error_fixed_read_missing_lrecl() {
    let mut c = Cursor::new(b"DATA".to_vec());
    let err = read_record(&mut c, RecordFormat::Fixed, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn error_fixed_read_truncated_record() {
    let mut c = Cursor::new(b"AB".to_vec());
    let err = read_record(&mut c, RecordFormat::Fixed, Some(10)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn error_rdw_write_oversized_payload() {
    let mut out = Vec::new();
    let big = vec![0u8; usize::from(u16::MAX) + 1];
    let err = write_record(&mut out, &big, RecordFormat::RDW).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn error_fixed_write_io_failure() {
    let mut w = AlwaysFailWriter;
    let err = write_record(&mut w, b"FAIL", RecordFormat::Fixed);
    assert!(err.is_err());
}

#[test]
fn error_rdw_write_io_failure() {
    let mut w = AlwaysFailWriter;
    let err = write_record(&mut w, b"FAIL", RecordFormat::RDW);
    assert!(err.is_err());
}

#[test]
fn error_fixed_read_io_failure() {
    let mut r = AlwaysFailReader;
    assert!(read_record(&mut r, RecordFormat::Fixed, Some(4)).is_err());
}

#[test]
fn error_rdw_read_io_failure() {
    let mut r = AlwaysFailReader;
    assert!(read_record(&mut r, RecordFormat::RDW, None).is_err());
}

#[test]
fn error_rdw_truncated_payload() {
    // Header says 10 bytes, only 3 present
    let mut data = Vec::new();
    data.extend_from_slice(&[0x00, 0x0A, 0x00, 0x00]);
    data.extend_from_slice(b"ABC");
    let mut c = Cursor::new(data);
    assert!(read_record(&mut c, RecordFormat::RDW, None).is_err());
}

#[test]
fn error_rdw_header_only_2_bytes_lenient_returns_none() {
    let mut c = Cursor::new(vec![0x00, 0x05]);
    let result = read_record(&mut c, RecordFormat::RDW, None).unwrap();
    assert!(result.is_none());
}

#[test]
fn error_rdw_header_only_2_bytes_strict_returns_error() {
    let mut r = RDWRecordReader::new(Cursor::new(vec![0x00, 0x05]), true);
    assert!(r.read_record().is_err());
}

#[test]
fn error_fixed_writer_rejects_oversized_data() {
    let mut buf = Vec::new();
    let mut w = FixedRecordWriter::new(&mut buf, Some(4)).unwrap();
    let err = w.write_record(b"TOOLONG").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn error_fixed_writer_remains_usable_after_rejection() {
    let mut buf = Vec::new();
    let mut w = FixedRecordWriter::new(&mut buf, Some(4)).unwrap();
    assert!(w.write_record(b"TOOLONG").is_err());
    w.write_record(b"OK").unwrap();
    w.flush().unwrap();
    assert_eq!(w.record_count(), 1);
}

// =========================================================================
// 6. Record count tracking
// =========================================================================

#[test]
fn fixed_reader_count_increments_per_record() {
    let data = vec![0x41u8; 30]; // 6 records of 5 bytes
    let mut r = FixedRecordReader::new(Cursor::new(&data), Some(5)).unwrap();
    for expected in 1..=6u64 {
        r.read_record().unwrap().unwrap();
        assert_eq!(r.record_count(), expected);
    }
}

#[test]
fn fixed_reader_count_unchanged_on_eof() {
    let data = b"ABCD";
    let mut r = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(4)).unwrap();
    r.read_record().unwrap().unwrap();
    assert_eq!(r.record_count(), 1);
    r.read_record().unwrap(); // EOF
    assert_eq!(r.record_count(), 1);
    r.read_record().unwrap(); // still EOF
    assert_eq!(r.record_count(), 1);
}

#[test]
fn fixed_writer_count_increments_per_write() {
    let mut buf = Vec::new();
    let mut w = FixedRecordWriter::new(&mut buf, Some(4)).unwrap();
    for expected in 1..=5u64 {
        w.write_record(b"TEST").unwrap();
        assert_eq!(w.record_count(), expected);
    }
}

#[test]
fn fixed_writer_count_unchanged_on_error() {
    let mut buf = Vec::new();
    let mut w = FixedRecordWriter::new(&mut buf, Some(2)).unwrap();
    w.write_record(b"OK").unwrap();
    assert_eq!(w.record_count(), 1);
    assert!(w.write_record(b"TOOLONG").is_err());
    assert_eq!(w.record_count(), 1);
}

#[test]
fn rdw_reader_count_increments_per_record() {
    let mut wire = Vec::new();
    for _ in 0..5 {
        write_record(&mut wire, b"REC", RecordFormat::RDW).unwrap();
    }
    let mut r = RDWRecordReader::new(Cursor::new(&wire), false);
    for expected in 1..=5u64 {
        r.read_record().unwrap().unwrap();
        assert_eq!(r.record_count(), expected);
    }
}

#[test]
fn rdw_writer_count_increments_per_write() {
    let mut buf = Vec::new();
    let mut w = RDWRecordWriter::new(&mut buf);
    for expected in 1..=5u64 {
        w.write_record_from_payload(b"X", None).unwrap();
        assert_eq!(w.record_count(), expected);
    }
}

// =========================================================================
// 7. Streaming behaviour (memory-bounded)
// =========================================================================

#[test]
fn streaming_fixed_1000_records_bounded_memory() {
    let lrecl = 80u32;
    let count = 1_000u64;
    let payload = vec![0x20u8; lrecl as usize];

    let mut wire = Vec::new();
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for _ in 0..count {
            w.write_record(&payload).unwrap();
        }
        w.flush().unwrap();
    }
    assert_eq!(wire.len(), lrecl as usize * count as usize);

    let mut r = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let mut n = 0u64;
    while r.read_record().unwrap().is_some() {
        n += 1;
    }
    assert_eq!(n, count);
    assert_eq!(r.record_count(), count);
}

#[test]
fn streaming_rdw_1000_variable_records() {
    let count = 1_000u64;
    let mut wire = Vec::new();
    {
        let mut w = RDWRecordWriter::new(&mut wire);
        for i in 0..count {
            let len = (i % 100) as usize + 1;
            let payload = vec![0xAA; len];
            w.write_record_from_payload(&payload, None).unwrap();
        }
        w.flush().unwrap();
    }

    let mut r = RDWRecordReader::new(Cursor::new(&wire), false);
    let mut n = 0u64;
    while let Some(rec) = r.read_record().unwrap() {
        let expected_len = (n % 100) as usize + 1;
        assert_eq!(rec.payload.len(), expected_len);
        n += 1;
    }
    assert_eq!(n, count);
    assert_eq!(r.record_count(), count);
}

#[test]
fn streaming_interleaved_read_write_fixed() {
    // Write a batch, read it, write more, read more — simulates streaming.
    let lrecl = 8u32;
    let mut wire = Vec::new();

    // Batch 1
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(b"BATCH1_A").unwrap();
        w.write_record(b"BATCH1_B").unwrap();
        w.flush().unwrap();
    }

    // Read batch 1
    let mut r = FixedRecordReader::new(Cursor::new(wire.clone()), Some(lrecl)).unwrap();
    assert_eq!(r.read_record().unwrap().unwrap(), b"BATCH1_A");
    assert_eq!(r.read_record().unwrap().unwrap(), b"BATCH1_B");
    assert!(r.read_record().unwrap().is_none());

    // Batch 2
    {
        let mut w = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        w.write_record(b"BATCH2_C").unwrap();
        w.flush().unwrap();
    }

    // Read all 3 from combined buffer
    let mut r2 = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    let mut all = Vec::new();
    while let Some(rec) = r2.read_record().unwrap() {
        all.push(rec);
    }
    assert_eq!(all.len(), 3);
}

// =========================================================================
// 8. RDWRecord type coverage
// =========================================================================

#[test]
fn rdw_record_try_new_valid() {
    let rec = RDWRecord::try_new(b"HELLO".to_vec()).unwrap();
    assert_eq!(rec.length(), 5);
    assert_eq!(rec.reserved(), 0);
    assert_eq!(rec.payload, b"HELLO");
}

#[test]
fn rdw_record_try_with_reserved() {
    let rec = RDWRecord::try_with_reserved(b"AB".to_vec(), 0x00FF).unwrap();
    assert_eq!(rec.length(), 2);
    assert_eq!(rec.reserved(), 0x00FF);
}

#[test]
fn rdw_record_as_bytes_includes_header_and_payload() {
    let rec = RDWRecord::try_new(b"XY".to_vec()).unwrap();
    let bytes = rec.as_bytes();
    assert_eq!(bytes.len(), 6); // 4 header + 2 payload
    assert_eq!(&bytes[4..], b"XY");
}

#[test]
fn rdw_record_empty_payload() {
    let rec = RDWRecord::try_new(Vec::new()).unwrap();
    assert_eq!(rec.length(), 0);
    assert!(rec.payload.is_empty());
}

// =========================================================================
// 9. Cross-format isolation
// =========================================================================

#[test]
fn fixed_data_misread_as_rdw_gives_garbage_or_error() {
    // Write 8 bytes as fixed, try reading as RDW
    let mut wire = Vec::new();
    write_record(&mut wire, b"FIXEDDAT", RecordFormat::Fixed).unwrap();

    // Interpreting fixed data as RDW: first 2 bytes = 0x4649 ('FI') = 17993
    // That's huge — reading will either return an error or wrong data.
    let mut c = Cursor::new(wire);
    let result = read_record(&mut c, RecordFormat::RDW, None);
    // It should error because there isn't 17993 bytes of payload
    assert!(result.is_err());
}

#[test]
fn rdw_data_misread_as_fixed_gives_header_in_data() {
    let mut wire = Vec::new();
    write_record(&mut wire, b"TEST", RecordFormat::RDW).unwrap();
    // wire = [0x00, 0x04, 0x00, 0x00, 'T', 'E', 'S', 'T']

    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::Fixed, Some(8))
        .unwrap()
        .unwrap();
    // Reading RDW data as fixed includes the 4-byte header
    assert_eq!(rec.len(), 8);
    assert_eq!(&rec[..4], &[0x00, 0x04, 0x00, 0x00]);
    assert_eq!(&rec[4..], b"TEST");
}

// =========================================================================
// 10. Empty streams
// =========================================================================

#[test]
fn empty_fixed_read_returns_none() {
    let mut c = Cursor::new(Vec::<u8>::new());
    assert!(
        read_record(&mut c, RecordFormat::Fixed, Some(4))
            .unwrap()
            .is_none()
    );
}

#[test]
fn empty_rdw_read_returns_none() {
    let mut c = Cursor::new(Vec::<u8>::new());
    assert!(
        read_record(&mut c, RecordFormat::RDW, None)
            .unwrap()
            .is_none()
    );
}

#[test]
fn empty_fixed_write_produces_nothing() {
    let mut out = Vec::new();
    write_record(&mut out, b"", RecordFormat::Fixed).unwrap();
    assert!(out.is_empty());
}

#[test]
fn empty_rdw_write_produces_header_only() {
    let mut out = Vec::new();
    write_record(&mut out, b"", RecordFormat::RDW).unwrap();
    assert_eq!(out.len(), 4);
}

// =========================================================================
// 11. Binary content preservation through dispatch
// =========================================================================

#[test]
fn all_256_byte_values_roundtrip_fixed() {
    let payload: Vec<u8> = (0..=255).collect();
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::Fixed).unwrap();
    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::Fixed, Some(256))
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

#[test]
fn all_256_byte_values_roundtrip_rdw() {
    let payload: Vec<u8> = (0..=255).collect();
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::RDW).unwrap();
    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

#[test]
fn null_filled_record_roundtrip_fixed() {
    let payload = vec![0u8; 128];
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::Fixed).unwrap();
    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::Fixed, Some(128))
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

#[test]
fn null_filled_record_roundtrip_rdw() {
    let payload = vec![0u8; 128];
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::RDW).unwrap();
    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

// =========================================================================
// 12. Large records through dispatch
// =========================================================================

#[test]
fn large_fixed_record_4096_bytes() {
    let payload = vec![0xBB; 4096];
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::Fixed).unwrap();
    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::Fixed, Some(4096))
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

#[test]
fn large_rdw_record_32000_bytes() {
    let payload = vec![0xCC; 32_000];
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::RDW).unwrap();
    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

// =========================================================================
// 13. Multiple consecutive EOF reads are stable
// =========================================================================

#[test]
fn repeated_eof_reads_fixed_stable() {
    let mut c = Cursor::new(b"ABCD".to_vec());
    read_record(&mut c, RecordFormat::Fixed, Some(4))
        .unwrap()
        .unwrap();
    for _ in 0..10 {
        // Each dispatch call creates a new FixedRecordReader, but the cursor
        // is at EOF so each returns None.
        assert!(
            read_record(&mut c, RecordFormat::Fixed, Some(4))
                .unwrap()
                .is_none()
        );
    }
}

#[test]
fn repeated_eof_reads_rdw_stable() {
    let mut wire = Vec::new();
    write_record(&mut wire, b"ONCE", RecordFormat::RDW).unwrap();
    let mut r = RDWRecordReader::new(Cursor::new(wire), false);
    r.read_record().unwrap().unwrap();
    for _ in 0..10 {
        assert!(r.read_record().unwrap().is_none());
    }
}

// =========================================================================
// 14. RDW max-boundary payload (u16::MAX)
// =========================================================================

#[test]
fn rdw_max_payload_size_roundtrip() {
    let payload = vec![0xDD; usize::from(u16::MAX)];
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::RDW).unwrap();
    let mut c = Cursor::new(wire);
    let rec = read_record(&mut c, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec.len(), usize::from(u16::MAX));
}

// =========================================================================
// 15. Dispatch consistency: dispatch == direct reader
// =========================================================================

#[test]
fn dispatch_fixed_matches_direct_reader() {
    let data = b"CONSIST8";
    let mut c1 = Cursor::new(data.to_vec());
    let via_dispatch = read_record(&mut c1, RecordFormat::Fixed, Some(8))
        .unwrap()
        .unwrap();

    let mut r = FixedRecordReader::new(Cursor::new(data.to_vec()), Some(8)).unwrap();
    let via_reader = r.read_record().unwrap().unwrap();
    assert_eq!(via_dispatch, via_reader);
}

#[test]
fn dispatch_rdw_matches_direct_reader() {
    let mut wire = Vec::new();
    write_record(&mut wire, b"CONSIST", RecordFormat::RDW).unwrap();

    let mut c1 = Cursor::new(wire.clone());
    let via_dispatch = read_record(&mut c1, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();

    let mut r = RDWRecordReader::new(Cursor::new(wire), false);
    let via_reader = r.read_record().unwrap().unwrap().payload;
    assert_eq!(via_dispatch, via_reader);
}
