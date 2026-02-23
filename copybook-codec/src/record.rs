// SPDX-License-Identifier: AGPL-3.0-or-later
//! Record framing and I/O utilities
//!
//! This module handles fixed-length and RDW variable-length record processing.

use crate::options::RecordFormat;
use copybook_error::{Error, ErrorCode, Result};
pub use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::{Read, Write};

pub use copybook_rdw::{RDWRecord, RDWRecordReader, RDWRecordWriter};

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
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use copybook_core::Schema;
    use copybook_rdw::{
        rdw_is_suspect_ascii_corruption, rdw_read_len, rdw_slice_body, rdw_try_peek_len,
        rdw_validate_and_finish,
    };
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
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
    }

    #[test]
    fn test_fixed_record_reader_no_lrecl() {
        let data = b"test";
        let result = FixedRecordReader::new(Cursor::new(data), None);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
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
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
    }

    #[test]
    fn test_fixed_record_writer_no_lrecl() {
        let mut output = Vec::new();
        let result = FixedRecordWriter::new(&mut output, None);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
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
    fn test_rdw_record_try_new() {
        let record = RDWRecord::try_new(b"hello".to_vec()).unwrap();
        assert_eq!(record.length(), 5);
        assert_eq!(record.reserved(), 0);
        assert_eq!(record.payload, b"hello");
    }

    #[test]
    fn test_rdw_record_try_with_reserved() {
        let record = RDWRecord::try_with_reserved(b"test".to_vec(), 0x1234).unwrap();
        assert_eq!(record.length(), 4);
        assert_eq!(record.reserved(), 0x1234);
        assert_eq!(record.payload, b"test");
    }

    #[test]
    fn test_rdw_record_try_recompute_length() {
        let mut record = RDWRecord::try_new(b"test".to_vec()).unwrap();
        record.payload = b"longer_payload".to_vec();
        record.try_recompute_length().unwrap();
        assert_eq!(record.length(), 14);
    }

    #[test]
    fn test_rdw_record_as_bytes() {
        let record = RDWRecord::try_new(b"hi".to_vec()).unwrap();
        let bytes = record.as_bytes();
        assert_eq!(bytes, vec![0, 2, 0, 0, b'h', b'i']);
    }

    #[test]
    fn test_rdw_record_payload_and_header_views() {
        let record = RDWRecord::try_new(b"test".to_vec()).unwrap();
        assert_eq!(record.as_bytes(), vec![0, 4, 0, 0, b't', b'e', b's', b't']);
        assert_eq!(record.payload, b"test");
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
        assert!(!rdw_is_suspect_ascii_corruption([0, 8, b'3', b'4']));
        assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', 0, 0]));
    }

    #[test]
    fn test_rdw_writer_basic() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);

        let record = RDWRecord::try_new(b"test".to_vec()).unwrap();
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
    fn test_rdw_record_try_new_payload_too_large() {
        // Create payload larger than u16::MAX
        let large_payload = vec![0u8; usize::from(u16::MAX) + 1];
        let result = RDWRecord::try_new(large_payload);

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(error.message.contains("RDW payload too large"));
    }

    #[test]
    fn test_rdw_record_try_with_reserved_payload_too_large() {
        // Create payload larger than u16::MAX
        let large_payload = vec![0u8; usize::from(u16::MAX) + 1];
        let result = RDWRecord::try_with_reserved(large_payload, 0x1234);

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(error.message.contains("RDW payload too large"));
    }

    #[test]
    fn test_rdw_record_try_recompute_length_payload_too_large() {
        let mut record = RDWRecord::try_new(b"test".to_vec()).unwrap();
        // Create oversized payload
        record.payload = vec![0u8; usize::from(u16::MAX) + 1];
        let result = record.try_recompute_length();

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
        assert!(error.message.contains("RDW payload too large"));
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
    }

    #[test]
    fn test_zero_length_record_allowed_for_empty_schema() {
        let schema = Schema::new();
        let reader = RDWRecordReader::new(Cursor::new(Vec::new()), false);

        let result = reader.validate_zero_length_record(&schema);
        assert!(result.is_ok());
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

    #[test]
    #[should_panic(expected = "RDW payload exceeds maximum size")]
    #[allow(deprecated)]
    fn test_rdw_record_new_panics_on_oversize_payload() {
        let payload = vec![0u8; usize::from(u16::MAX) + 1];
        let _ = RDWRecord::new(payload);
    }
}
