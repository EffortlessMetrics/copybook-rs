// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for VB/BDW block framing: header policy, bounded block
//! reading, deterministic writer packing, and the #953 negative matrix.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_rdw::{BDW_MAX_BLOCK_LEN, BdwHeader, VB_MAX_RECORD_LEN, VbBlockReader, VbBlockWriter};
use std::io::Cursor;

fn block_bytes(payloads: &[&[u8]]) -> Vec<u8> {
    let mut out = Vec::new();
    let mut writer = VbBlockWriter::new(&mut out);
    for payload in payloads {
        writer.write_record_from_payload(payload, 0).unwrap();
    }
    writer.finish().unwrap();
    out
}

fn read_all(data: &[u8], strict: bool) -> Vec<copybook_rdw::VbRecord> {
    let mut reader = VbBlockReader::new(Cursor::new(data.to_vec()), strict);
    let mut records = Vec::new();
    while let Some(record) = reader.read_record().unwrap() {
        records.push(record);
    }
    records
}

#[test]
fn bdw_header_layout_includes_itself() {
    let header = BdwHeader::from_block_len(10).unwrap();
    assert_eq!(header.bytes(), [0x00, 0x0A, 0x00, 0x00]);
    assert_eq!(header.length(), 10);
    assert_eq!(header.reserved(), 0);
}

#[test]
fn bdw_from_block_len_rejects_out_of_bounds() {
    for bad in [0, 1, 2, 3, BDW_MAX_BLOCK_LEN + 1, BDW_MAX_BLOCK_LEN + 100] {
        let err = BdwHeader::from_block_len(bad).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKF222_BDW_LENGTH_INVALID);
    }
    assert_eq!(BdwHeader::from_block_len(4).unwrap().length(), 4);
    assert_eq!(
        BdwHeader::from_block_len(BDW_MAX_BLOCK_LEN)
            .unwrap()
            .length(),
        32760
    );
}

#[test]
fn bdw_zero_block_length_is_invalid() {
    let err = BdwHeader::from_block_len(0).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF222_BDW_LENGTH_INVALID);
}

#[test]
fn bdw_undersized_block_length_is_invalid() {
    let data = [0x00, 0x02, 0x00, 0x00, 0xAA, 0xBB];
    let mut reader = VbBlockReader::new(Cursor::new(data.to_vec()), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF222_BDW_LENGTH_INVALID);
}

#[test]
fn bdw_oversized_block_length_is_invalid() {
    let data = [0x80, 0x01, 0x00, 0x00];
    let mut reader = VbBlockReader::new(Cursor::new(data.to_vec()), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF222_BDW_LENGTH_INVALID);
}

#[test]
fn bdw_truncated_block_is_underflow() {
    // Declares 20 bytes, carries 2: no complete RDW fits.
    let mut data = vec![0x00, 0x14, 0x00, 0x00];
    data.extend_from_slice(&[0xAA; 2]);
    let mut reader = VbBlockReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF223_BDW_UNDERFLOW);
}

#[test]
fn bdw_short_input_lenient_is_eof() {
    let mut reader = VbBlockReader::new(Cursor::new(vec![0x00, 0x10]), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn bdw_short_input_strict_is_underflow() {
    let mut reader = VbBlockReader::new(Cursor::new(vec![0x00, 0x10]), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF223_BDW_UNDERFLOW);
}

#[test]
fn vb_empty_input_is_eof() {
    let mut reader = VbBlockReader::new(Cursor::new(Vec::new()), true);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.block_count(), 0);
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn bdw_reserved_nonzero_strict_is_rejected() {
    let mut data = vec![0x00, 0x08, 0x12, 0x34];
    data.extend_from_slice(&[0x00, 0x04, 0x00, 0x00]);
    let mut reader = VbBlockReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF225_BDW_RESERVED_NONZERO);
}

#[test]
fn bdw_reserved_nonzero_lenient_warns_and_continues() {
    let mut data = vec![0x00, 0x08, 0x12, 0x34];
    data.extend_from_slice(&[0x00, 0x04, 0x00, 0x00]);
    let records = read_all(&data, false);
    assert_eq!(records.len(), 1);
    assert!(records[0].payload.is_empty());
}

#[test]
fn vb_block_with_partial_rdw_is_underflow() {
    // BDW length 6 with only 2 content bytes: no complete RDW fits.
    let data = vec![0x00, 0x06, 0x00, 0x00, 0xAA, 0xBB];
    let mut reader = VbBlockReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF223_BDW_UNDERFLOW);
}

#[test]
fn vb_rdw_shorter_than_header_is_invalid() {
    let mut data = vec![0x00, 0x08, 0x00, 0x00];
    data.extend_from_slice(&[0x00, 0x02, 0x00, 0x00]);
    let mut reader = VbBlockReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF222_BDW_LENGTH_INVALID);
}

#[test]
fn vb_rdw_beyond_block_is_rejected() {
    // Block of 8 bytes, RDW claims 12.
    let mut data = vec![0x00, 0x08, 0x00, 0x00];
    data.extend_from_slice(&[0x00, 0x0C, 0x00, 0x00, 0xAA, 0xBB, 0xCC, 0xDD]);
    let mut reader = VbBlockReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF224_RDW_BEYOND_BLOCK);
}

#[test]
fn vb_writer_oversize_record_is_rejected_before_allocation() {
    let mut out = Vec::new();
    let mut writer = VbBlockWriter::new(&mut out);
    let payload = vec![0xAAu8; VB_MAX_RECORD_LEN - 4 + 2];
    let err = writer.write_record_from_payload(&payload, 0).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF222_BDW_LENGTH_INVALID);
    assert_eq!(writer.record_count(), 0);
    assert!(out.is_empty());
}

#[test]
fn vb_trailing_bytes_strict_is_underflow() {
    let mut data = block_bytes(&[b"ok"]);
    data.extend_from_slice(&[0x00, 0x09]);
    let mut reader = VbBlockReader::new(Cursor::new(data), true);
    assert_eq!(reader.read_record().unwrap().unwrap().payload, b"ok");
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF223_BDW_UNDERFLOW);
}

#[test]
fn vb_trailing_bytes_lenient_is_eof() {
    let mut data = block_bytes(&[b"ok"]);
    data.extend_from_slice(&[0x00, 0x09]);
    let records = read_all(&data, false);
    assert_eq!(records.len(), 1);
}

#[test]
fn vb_empty_block_yields_no_records() {
    let mut data = BdwHeader::from_block_len(4).unwrap().bytes().to_vec();
    data.extend_from_slice(&block_bytes(&[b"after"]));
    let records = read_all(&data, true);
    assert_eq!(records.len(), 1);
    assert_eq!(records[0].payload, b"after");
    assert_eq!(records[0].block_index, 1);
}

#[test]
fn vb_single_block_roundtrip_positions() {
    let data = block_bytes(&[b"alpha", b"beta"]);
    let mut reader = VbBlockReader::new(Cursor::new(data.clone()), true);
    let first = reader.read_record().unwrap().unwrap();
    assert_eq!(first.payload, b"alpha");
    assert_eq!((first.block_index, first.record_index_in_block), (0, 0));
    assert_eq!((first.block_offset, first.physical_offset), (0, 4));
    let second = reader.read_record().unwrap().unwrap();
    assert_eq!(second.payload, b"beta");
    assert_eq!((second.block_index, second.record_index_in_block), (0, 1));
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 2);
    assert_eq!(reader.block_count(), 1);
    assert_eq!(reader.physical_bytes(), data.len() as u64);
}

#[test]
fn vb_writer_packs_blocks_deterministically() {
    // Two 20004-byte framed records exceed one 32760-byte block; the third
    // record joins the second block.
    let big = vec![0xABu8; 20000];
    let mut out = Vec::new();
    let mut writer = VbBlockWriter::new(&mut out);
    writer.write_record_from_payload(&big, 0).unwrap();
    writer.write_record_from_payload(&big, 0).unwrap();
    writer.write_record_from_payload(b"tail", 0).unwrap();
    writer.finish().unwrap();
    assert_eq!(writer.block_count(), 2);
    assert_eq!(writer.record_count(), 3);
    assert_eq!(writer.physical_bytes(), out.len() as u64);

    let records = read_all(&out, true);
    assert_eq!(records.len(), 3);
    assert_eq!(records[0].payload, big);
    assert_eq!(records[0].block_index, 0);
    assert_eq!(records[1].block_index, 1);
    assert_eq!(records[2].payload, b"tail");
    assert_eq!(records[2].block_index, 1);
}

#[test]
fn vb_many_tiny_records_stay_bounded() {
    let tiny = vec![0x01u8; 1];
    let mut out = Vec::new();
    let mut writer = VbBlockWriter::new(&mut out);
    for _ in 0..2000 {
        writer.write_record_from_payload(&tiny, 0).unwrap();
    }
    writer.finish().unwrap();
    let records = read_all(&out, true);
    assert_eq!(records.len(), 2000);
    assert!(records.iter().all(|record| record.payload == tiny));
}

#[test]
fn vb_zero_length_rdw_payload_is_valid() {
    let data = block_bytes(&[b""]);
    let records = read_all(&data, true);
    assert_eq!(records.len(), 1);
    assert!(records[0].payload.is_empty());
}

#[test]
fn vb_rdw_reserved_preserved_per_record() {
    let mut out = Vec::new();
    let mut writer = VbBlockWriter::new(&mut out);
    writer.write_record_from_payload(b"data", 0xBEEF).unwrap();
    writer.finish().unwrap();
    let records = read_all(&out, true);
    assert_eq!(records[0].rdw_reserved, 0xBEEF);
    assert_eq!(records[0].payload, b"data");
}

#[test]
fn vb_block_stray_bytes_are_underflow_not_next_block() {
    // Block 1 declares 15 bytes: one 9-byte record plus 2 stray bytes that
    // cannot form an RDW. Without the pre-read guard the reader would glue
    // the strays to block 2's BDW and report CBKF222 instead of CBKF223.
    let mut data = vec![0x00, 0x0F, 0x00, 0x00];
    data.extend_from_slice(&[0x00, 0x09, 0x00, 0x00, b'H', b'E', b'L', b'L', b'O']);
    data.extend_from_slice(&[0xAA, 0xBB]);
    data.extend_from_slice(&block_bytes(&[b"OK"]));
    let mut reader = VbBlockReader::new(Cursor::new(data), true);
    let first = reader.read_record().unwrap().unwrap();
    assert_eq!(first.payload, b"HELLO");
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF223_BDW_UNDERFLOW);
}

#[test]
fn bdw_reserved_reports_absolute_offset() {
    // An empty block (4 bytes) shifts the offending block to offset 4, so
    // its reserved bytes live at absolute offset 6.
    let mut data = vec![0x00, 0x04, 0x00, 0x00];
    data.extend_from_slice(&[0x00, 0x08, 0x12, 0x34, 0x00, 0x04, 0x00, 0x00]);
    let mut reader = VbBlockReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF225_BDW_RESERVED_NONZERO);
    let context = err.context.expect("context should be populated");
    assert_eq!(context.byte_offset, Some(6));
}
