#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW (Record Descriptor Word) header primitives.
//!
//! This crate intentionally focuses on one concern:
//! parsing and constructing RDW framing metadata plus minimal buffered helpers.
//!
//! The RDW format prefixes each variable-length record with a 4-byte header
//! (2-byte big-endian payload length + 2 reserved bytes). Use [`RDWRecordReader`]
//! and [`RDWRecordWriter`] for streaming record I/O, or the lower-level helpers
//! ([`rdw_read_len`], [`rdw_slice_body`]) for custom framing.

mod buffer;
mod header;
mod reader;
mod record;
mod schema_prefix;
mod writer;

pub use buffer::{rdw_read_len, rdw_slice_body, rdw_try_peek_len, rdw_validate_and_finish};
pub use header::{RdwHeader, rdw_is_suspect_ascii_corruption, rdw_payload_len_to_u16};
pub use reader::RDWRecordReader;
pub use record::RDWRecord;
pub use writer::RDWRecordWriter;

/// Size of an RDW header in bytes.
pub const RDW_HEADER_LEN: usize = 4;

/// Maximum payload size representable in RDW (`u16::MAX`).
pub const RDW_MAX_PAYLOAD_LEN: usize = u16::MAX as usize;

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
mod tests {
    use super::*;
    use copybook_error::ErrorCode;
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
    fn rdw_writer_rejects_header_shorter_than_payload() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        let record = RDWRecord {
            header: [0, 2, 0, 0],
            payload: b"toolong".to_vec(),
        };

        let error = writer.write_record(&record).unwrap_err();

        assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(error.message.contains("header declares 2 bytes"));
        assert_eq!(writer.record_count(), 0);
        assert!(output.is_empty());
    }

    #[test]
    fn rdw_writer_rejects_header_longer_than_payload() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        let record = RDWRecord {
            header: [0, 4, 0, 0],
            payload: b"hi".to_vec(),
        };

        let error = writer.write_record(&record).unwrap_err();

        assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(error.message.contains("payload has 2 bytes"));
        assert_eq!(writer.record_count(), 0);
        assert!(output.is_empty());
    }

    #[test]
    fn rdw_writer_rejects_manual_oversize_record() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        let record = RDWRecord {
            header: [0xFF, 0xFF, 0, 0],
            payload: vec![0u8; RDW_MAX_PAYLOAD_LEN + 1],
        };

        let error = writer.write_record(&record).unwrap_err();

        assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(error.message.contains("exceeds maximum"));
        assert_eq!(writer.record_count(), 0);
        assert!(output.is_empty());
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
            let expected = b0.is_ascii_digit() && b1.is_ascii_digit();
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

    // ---- additional coverage for RDW framing ----

    #[test]
    fn rdw_header_big_endian_length_parsing() {
        // 0x0100 big-endian = 256
        let header = RdwHeader::from_bytes([0x01, 0x00, 0x00, 0x00]);
        assert_eq!(header.length(), 256);
        assert_eq!(header.reserved(), 0);

        // 0xFF_FF big-endian = 65535 (max)
        let header = RdwHeader::from_bytes([0xFF, 0xFF, 0x00, 0x00]);
        assert_eq!(header.length(), u16::MAX);
    }

    #[test]
    fn rdw_header_reserved_bytes_preserved() {
        let header = RdwHeader::from_bytes([0x00, 0x0A, 0xDE, 0xAD]);
        assert_eq!(header.length(), 10);
        assert_eq!(header.reserved(), 0xDEAD);
    }

    #[test]
    fn rdw_reader_empty_file_returns_none() {
        let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
        assert!(reader.read_record().unwrap().is_none());
        assert_eq!(reader.record_count(), 0);
    }

    #[test]
    fn rdw_reader_empty_file_strict_returns_none() {
        let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), true);
        assert!(reader.read_record().unwrap().is_none());
        assert_eq!(reader.record_count(), 0);
    }

    #[test]
    fn rdw_reader_zero_length_record() {
        let data = vec![0x00, 0x00, 0x00, 0x00];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.length(), 0);
        assert!(record.payload.is_empty());
        assert_eq!(reader.record_count(), 1);
        assert!(reader.read_record().unwrap().is_none());
    }

    #[test]
    fn rdw_reader_max_record_size() {
        let payload = vec![0xABu8; u16::MAX as usize];
        let mut data = Vec::with_capacity(RDW_HEADER_LEN + payload.len());
        data.extend_from_slice(&[0xFF, 0xFF, 0x00, 0x00]);
        data.extend_from_slice(&payload);

        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.length(), u16::MAX);
        assert_eq!(record.payload.len(), u16::MAX as usize);
        assert!(record.payload.iter().all(|&b| b == 0xAB));
    }

    #[test]
    fn rdw_multi_record_write_read_roundtrip() {
        let payloads: Vec<&[u8]> = vec![b"alpha", b"", b"gamma delta", b"x"];
        let mut encoded = Vec::new();
        {
            let mut writer = RDWRecordWriter::new(&mut encoded);
            for p in &payloads {
                writer.write_record_from_payload(p, None).unwrap();
            }
            writer.flush().unwrap();
            assert_eq!(writer.record_count(), 4);
        }

        let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
        for expected in &payloads {
            let record = reader.read_record().unwrap().unwrap();
            assert_eq!(record.payload.as_slice(), *expected);
        }
        assert!(reader.read_record().unwrap().is_none());
        assert_eq!(reader.record_count(), 4);
    }

    #[test]
    fn rdw_streaming_many_records() {
        let record_count = 500;
        let payload = b"STREAMING_TEST";
        let mut encoded = Vec::new();
        {
            let mut writer = RDWRecordWriter::new(&mut encoded);
            for _ in 0..record_count {
                writer.write_record_from_payload(payload, None).unwrap();
            }
            writer.flush().unwrap();
        }

        let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
        let mut count = 0u64;
        while let Some(record) = reader.read_record().unwrap() {
            assert_eq!(record.payload, payload);
            count += 1;
        }
        assert_eq!(count, record_count);
        assert_eq!(reader.record_count(), record_count);
    }

    #[test]
    fn rdw_reader_single_byte_header_lenient_is_eof() {
        let data = vec![0x00];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        assert!(reader.read_record().unwrap().is_none());
    }

    #[test]
    fn rdw_reader_single_byte_header_strict_is_underflow() {
        let data = vec![0x00];
        let mut reader = RDWRecordReader::new(Cursor::new(data), true);
        let err = reader.read_record().unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    }

    // --- Additional coverage ---

    #[test]
    fn rdw_header_zero_length_zero_reserved() {
        let header = RdwHeader::from_payload_len(0, 0).unwrap();
        assert_eq!(header.length(), 0);
        assert_eq!(header.reserved(), 0);
        assert_eq!(header.bytes(), [0, 0, 0, 0]);
    }

    #[test]
    fn rdw_header_max_payload_len() {
        let header = RdwHeader::from_payload_len(RDW_MAX_PAYLOAD_LEN, 0).unwrap();
        assert_eq!(header.length(), u16::MAX);
    }

    #[test]
    fn rdw_header_max_payload_len_plus_one_fails() {
        let err = RdwHeader::from_payload_len(RDW_MAX_PAYLOAD_LEN + 1, 0).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    }

    #[test]
    fn rdw_header_length_one() {
        let header = RdwHeader::from_payload_len(1, 0).unwrap();
        assert_eq!(header.length(), 1);
        assert_eq!(header.bytes(), [0, 1, 0, 0]);
    }

    #[test]
    fn rdw_header_looks_ascii_corrupt_false_for_binary() {
        let header = RdwHeader::from_bytes([0x00, 0x0A, 0x00, 0x00]);
        assert!(!header.looks_ascii_corrupt());
    }

    #[test]
    fn rdw_header_looks_ascii_corrupt_true_for_digits() {
        let header = RdwHeader::from_bytes([b'0', b'5', 0x00, 0x00]);
        assert!(header.looks_ascii_corrupt());
    }

    #[test]
    fn rdw_payload_len_to_u16_zero() {
        assert_eq!(rdw_payload_len_to_u16(0).unwrap(), 0);
    }

    #[test]
    fn rdw_payload_len_to_u16_max() {
        assert_eq!(
            rdw_payload_len_to_u16(usize::from(u16::MAX)).unwrap(),
            u16::MAX
        );
    }

    #[test]
    fn rdw_payload_len_to_u16_too_large() {
        let err = rdw_payload_len_to_u16(usize::from(u16::MAX) + 1).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(err.message.contains("RDW payload too large"));
    }

    #[test]
    fn rdw_slice_body_zero_length_returns_empty() {
        let mut cur = Cursor::new(vec![0xAA, 0xBB]);
        let body = rdw_slice_body(&mut cur, 0).unwrap();
        assert!(body.is_empty());
    }

    #[test]
    fn rdw_validate_and_finish_identity() {
        let data = b"test_data";
        let result = rdw_validate_and_finish(data);
        assert_eq!(result, data);
    }

    #[test]
    fn rdw_record_clone() {
        let record = RDWRecord::try_new(b"clone_me".to_vec()).unwrap();
        let cloned = record.clone();
        assert_eq!(cloned.payload, record.payload);
        assert_eq!(cloned.header, record.header);
    }

    #[test]
    fn rdw_record_debug_format() {
        let record = RDWRecord::try_new(b"dbg".to_vec()).unwrap();
        let debug = format!("{record:?}");
        assert!(debug.contains("RDWRecord"));
    }

    #[test]
    fn rdw_record_empty_payload() {
        let record = RDWRecord::try_new(Vec::new()).unwrap();
        assert_eq!(record.length(), 0);
        assert!(record.payload.is_empty());
        assert_eq!(record.as_bytes().len(), RDW_HEADER_LEN);
    }

    #[test]
    fn rdw_reader_three_byte_header_lenient_is_eof() {
        let data = vec![0x00, 0x05, 0x00];
        let mut reader = RDWRecordReader::new(Cursor::new(data), false);
        assert!(reader.read_record().unwrap().is_none());
    }

    #[test]
    fn rdw_reader_three_byte_header_strict_is_underflow() {
        let data = vec![0x00, 0x05, 0x00];
        let mut reader = RDWRecordReader::new(Cursor::new(data), true);
        let err = reader.read_record().unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    }

    #[test]
    fn rdw_writer_flush_succeeds() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), 0);
    }

    #[test]
    fn rdw_writer_multiple_records_count() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        for i in 0..5 {
            writer.write_record_from_payload(&[i], None).unwrap();
        }
        assert_eq!(writer.record_count(), 5);
    }

    #[test]
    fn rdw_try_peek_len_two_bytes_returns_some() {
        let mut cur = Cursor::new(vec![0x00, 0x05]);
        assert!(rdw_try_peek_len(&mut cur).unwrap().is_some());
    }

    #[test]
    fn rdw_read_len_incomplete_is_error() {
        let mut cur = Cursor::new(vec![0x00]);
        let err = rdw_read_len(&mut cur).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    }
}
