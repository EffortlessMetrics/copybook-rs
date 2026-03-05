// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for record framing across fixed and RDW formats.
//!
//! Complements `fixed.rs`, `rdw.rs`, and `record_io.rs` with cross-format
//! properties, boundary conditions, padding invariants, and record count
//! consistency that aren't covered by the existing roundtrip tests.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use copybook_rdw::{RDW_HEADER_LEN, RDWRecordReader, RDWRecordWriter, RdwHeader};
use proptest::collection::vec;
use proptest::prelude::*;
use std::io::Cursor;

use super::config::*;

// ---------------------------------------------------------------------------
// 1. Fixed framing: padding bytes are always zero-filled
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_fixed_padding_is_zero(
        lrecl in 2u16..=256u16,
        payload in vec(any::<u8>(), 1..=128),
    ) {
        prop_assume!(payload.len() < usize::from(lrecl));

        let mut encoded = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(u32::from(lrecl))).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();

        // Padding region must be all zeros
        let padding = &encoded[payload.len()..];
        prop_assert!(
            padding.iter().all(|&b| b == 0),
            "Padding contains non-zero bytes"
        );
    }
}

// ---------------------------------------------------------------------------
// 2. Fixed framing: multi-record write/read preserves boundaries
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_fixed_multi_record_boundaries(
        lrecl in 4u16..=128u16,
        payloads in vec(vec(any::<u8>(), 1..=64), 2..=8),
    ) {
        let lrecl_usize = usize::from(lrecl);
        let valid_payloads: Vec<&Vec<u8>> = payloads
            .iter()
            .filter(|p| p.len() <= lrecl_usize)
            .collect();
        prop_assume!(!valid_payloads.is_empty());

        let mut encoded = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(u32::from(lrecl))).unwrap();
        for p in &valid_payloads {
            writer.write_record(p).unwrap();
        }
        writer.flush().unwrap();

        // Total bytes = records × lrecl
        prop_assert_eq!(encoded.len(), valid_payloads.len() * lrecl_usize);

        // Read back and verify each record starts with the payload
        let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(u32::from(lrecl))).unwrap();
        for payload in &valid_payloads {
            let record = reader.read_record().unwrap().unwrap();
            prop_assert_eq!(record.len(), lrecl_usize);
            prop_assert_eq!(&record[..payload.len()], payload.as_slice());
        }
        prop_assert!(reader.read_record().unwrap().is_none());
    }
}

// ---------------------------------------------------------------------------
// 3. Fixed framing: record count matches writes
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_fixed_record_count_consistent(
        lrecl in 1u16..=64u16,
        n in 1usize..=20,
    ) {
        let lrecl_usize = usize::from(lrecl);
        let payload = vec![0x41u8; lrecl_usize];

        let mut encoded = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(u32::from(lrecl))).unwrap();
        for _ in 0..n {
            writer.write_record(&payload).unwrap();
        }
        writer.flush().unwrap();

        prop_assert_eq!(writer.record_count(), n as u64);

        let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(u32::from(lrecl))).unwrap();
        let mut count = 0;
        while reader.read_record().unwrap().is_some() {
            count += 1;
        }
        prop_assert_eq!(count, n);
        prop_assert_eq!(reader.record_count(), n as u64);
    }
}

// ---------------------------------------------------------------------------
// 4. RDW framing: header length field matches payload size
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_header_length_matches_payload(
        payload in vec(any::<u8>(), 0..=2048),
    ) {
        let mut encoded = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();

        // Verify header bytes encode correct length
        let header_bytes: [u8; 4] = encoded[..RDW_HEADER_LEN].try_into().unwrap();
        let header = RdwHeader::from_bytes(header_bytes);
        prop_assert_eq!(header.length() as usize, payload.len());

        // Total wire size = header + payload
        prop_assert_eq!(encoded.len(), RDW_HEADER_LEN + payload.len());
    }
}

// ---------------------------------------------------------------------------
// 5. RDW framing: multi-record boundaries preserved
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_multi_record_boundaries(
        payloads in vec(vec(any::<u8>(), 0..=512), 2..=8),
    ) {
        let mut encoded = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for p in &payloads {
            writer.write_record_from_payload(p, None).unwrap();
        }

        // Verify total wire size
        let expected_size: usize = payloads
            .iter()
            .map(|p| RDW_HEADER_LEN + p.len())
            .sum();
        prop_assert_eq!(encoded.len(), expected_size);

        // Read back and verify each payload
        let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
        for (i, expected) in payloads.iter().enumerate() {
            let record = reader.read_record().unwrap()
                .unwrap_or_else(|| panic!("Expected record {}", i));
            prop_assert_eq!(
                record.payload.as_slice(),
                expected.as_slice(),
                "Record {} payload mismatch", i
            );
        }
        prop_assert!(reader.read_record().unwrap().is_none());
    }
}

// ---------------------------------------------------------------------------
// 6. Fixed framing: empty payload produces full-zero record
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_fixed_empty_payload_all_zeros(lrecl in 1u16..=128u16) {
        let mut encoded = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(u32::from(lrecl))).unwrap();
        writer.write_record(&[]).unwrap();
        writer.flush().unwrap();

        prop_assert_eq!(encoded.len(), usize::from(lrecl));
        prop_assert!(encoded.iter().all(|&b| b == 0));
    }
}

// ---------------------------------------------------------------------------
// 7. RDW framing: reserved field is preserved through roundtrip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_reserved_preserved(
        payload in vec(any::<u8>(), 0..=512),
        reserved in any::<u16>(),
    ) {
        let mut encoded = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, Some(reserved)).unwrap();

        let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
        let record = reader.read_record().unwrap().unwrap();

        prop_assert_eq!(record.reserved(), reserved);
        prop_assert_eq!(record.payload.as_slice(), payload.as_slice());
    }
}

// ---------------------------------------------------------------------------
// 8. Fixed framing: exact-length payload produces no padding
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_fixed_exact_length_no_padding(lrecl in 1u16..=256u16) {
        let lrecl_usize = usize::from(lrecl);
        let payload: Vec<u8> = (0..lrecl_usize).map(|i| (i % 256) as u8).collect();

        let mut encoded = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(u32::from(lrecl))).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();

        // Output equals input exactly (no padding added)
        prop_assert_eq!(encoded, payload);
    }
}

// ---------------------------------------------------------------------------
// 9. Cross-format: same payload framed with Fixed and RDW both roundtrip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_cross_format_roundtrip(
        payload in vec(any::<u8>(), 1..=512),
    ) {
        // Fixed roundtrip
        let lrecl = u32::try_from(payload.len()).unwrap();
        let mut fixed_buf = Vec::new();
        let mut fw = FixedRecordWriter::new(&mut fixed_buf, Some(lrecl)).unwrap();
        fw.write_record(&payload).unwrap();
        fw.flush().unwrap();
        let mut fr = FixedRecordReader::new(Cursor::new(&fixed_buf), Some(lrecl)).unwrap();
        let fixed_record = fr.read_record().unwrap().unwrap();

        // RDW roundtrip
        let mut rdw_buf = Vec::new();
        let mut rw = RDWRecordWriter::new(&mut rdw_buf);
        rw.write_record_from_payload(&payload, None).unwrap();
        let mut rr = RDWRecordReader::new(Cursor::new(&rdw_buf), false);
        let rdw_record = rr.read_record().unwrap().unwrap();

        // Both should recover the original payload
        prop_assert_eq!(fixed_record.as_slice(), payload.as_slice());
        prop_assert_eq!(rdw_record.payload.as_slice(), payload.as_slice());
    }
}

// ---------------------------------------------------------------------------
// 10. RDW: arbitrary truncated input never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_truncated_no_panic(
        data in vec(any::<u8>(), 0..=64),
        cut in any::<usize>(),
    ) {
        let cut_point = if data.is_empty() { 0 } else { cut % data.len() };
        let truncated = &data[..cut_point];

        let mut reader = RDWRecordReader::new(Cursor::new(truncated), false);
        // Must not panic, may error
        while let Ok(Some(_)) = reader.read_record() {}
    }

    /// Fixed reader with truncated data returns error, never panics.
    #[test]
    fn prop_fixed_truncated_no_panic(
        lrecl in 2u16..=64u16,
        data in vec(any::<u8>(), 1..=63),
    ) {
        prop_assume!(data.len() < usize::from(lrecl));
        // Data shorter than lrecl should either error or return None
        let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(u32::from(lrecl))).unwrap();
        let result = reader.read_record();
        // Either error (truncated) or EOF—never panic
        let _ = result;
    }
}
