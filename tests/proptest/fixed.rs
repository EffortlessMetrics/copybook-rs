#![allow(unused_doc_comments, unused_imports, dead_code)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for fixed-record microcrate invariants.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use proptest::collection::vec;
use proptest::prelude::*;
use std::io::Cursor;

use super::config::DEFAULT_CASES;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_fixed_writer_output_roundtrips_through_reader(
        lrecl in 1u16..=512u16,
        payload in vec(any::<u8>(), 0..=512),
    ) {
        prop_assume!(payload.len() <= usize::from(lrecl));

        let mut encoded = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(u32::from(lrecl)))
            .expect("writer should build");
        writer.write_record(&payload).expect("write should succeed");
        writer.flush().expect("flush should succeed");

        prop_assert_eq!(encoded.len(), usize::from(lrecl));
        prop_assert_eq!(&encoded[..payload.len()], payload.as_slice());

        let mut reader = FixedRecordReader::new(Cursor::new(encoded.clone()), Some(u32::from(lrecl)))
            .expect("reader should build");
        let record = reader.read_record().expect("read should succeed");
        prop_assert_eq!(record.expect("record should exist"), encoded);
        prop_assert_eq!(reader.read_record().expect("second read"), None);
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_fixed_writer_rejects_payload_larger_than_lrecl(
        lrecl in 1u16..=128u16,
        oversize_payload in vec(any::<u8>(), 129..=512),
    ) {
        prop_assume!(oversize_payload.len() > usize::from(lrecl));
        let mut output = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut output, Some(u32::from(lrecl)))
            .expect("writer should build");
        let error = writer.write_record(&oversize_payload).unwrap_err();
        prop_assert!(error.message.contains("Record too long"));
    }
}
