#![allow(unused_doc_comments, unused_imports, dead_code)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for RDW microcrate invariants.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_rdw::{
    RDW_HEADER_LEN, RDWRecord, RDWRecordWriter, RdwHeader, rdw_read_len, rdw_slice_body,
    rdw_try_peek_len, rdw_validate_and_finish,
};
use proptest::collection::vec;
use proptest::prelude::*;
use std::io::{BufRead, Cursor};

use super::config::DEFAULT_CASES;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_record_header_matches_payload(
        payload in vec(any::<u8>(), 0..=2048),
        reserved in any::<u16>(),
    ) {
        let record = RDWRecord::try_with_reserved(payload.clone(), reserved)
            .expect("payload size is bounded to fit RDW length");
        let bytes = record.as_bytes();
        let header = RdwHeader::from_bytes(bytes[0..RDW_HEADER_LEN].try_into().expect("header"));

        prop_assert_eq!(usize::from(header.length()), payload.len());
        prop_assert_eq!(header.reserved(), reserved);
        prop_assert_eq!(&bytes[RDW_HEADER_LEN..], payload.as_slice());
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_writer_output_is_readable_with_primitives(
        payload in vec(any::<u8>(), 0..=1024),
        reserved in any::<u16>(),
    ) {
        let mut encoded = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer
            .write_record_from_payload(&payload, Some(reserved))
            .expect("bounded payload should encode");

        let mut cursor = Cursor::new(encoded);
        prop_assert!(rdw_try_peek_len(&mut cursor).expect("peek").is_some());
        let length = rdw_read_len(&mut cursor).expect("length");
        cursor.consume(2);
        let body = rdw_slice_body(&mut cursor, length).expect("body");
        let body = rdw_validate_and_finish(body);

        prop_assert_eq!(usize::from(length), payload.len());
        prop_assert_eq!(body, payload.as_slice());
    }
}
