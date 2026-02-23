#![allow(unused_doc_comments, unused_imports, dead_code)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for record I/O dispatch microcrate invariants.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::RecordFormat;
use copybook_record_io::{read_record, write_record};
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
    fn prop_fixed_dispatch_roundtrip_when_lrecl_matches(payload in vec(any::<u8>(), 1..=512)) {
        let mut encoded = Vec::new();
        write_record(&mut encoded, &payload, RecordFormat::Fixed)
            .expect("fixed write should succeed");

        let mut cursor = Cursor::new(encoded);
        let lrecl = u32::try_from(payload.len()).expect("bounded payload fits u32");
        let decoded = read_record(&mut cursor, RecordFormat::Fixed, Some(lrecl))
            .expect("fixed read should succeed")
            .expect("one record expected");
        prop_assert_eq!(decoded, payload);
        prop_assert!(read_record(&mut cursor, RecordFormat::Fixed, Some(lrecl))
            .expect("EOF read")
            .is_none());
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_dispatch_roundtrip(payload in vec(any::<u8>(), 0..=1024)) {
        let mut encoded = Vec::new();
        write_record(&mut encoded, &payload, RecordFormat::RDW)
            .expect("rdw write should succeed");

        let mut cursor = Cursor::new(encoded);
        let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
            .expect("rdw read should succeed")
            .expect("one record expected");
        prop_assert_eq!(decoded, payload);
        prop_assert!(read_record(&mut cursor, RecordFormat::RDW, None)
            .expect("EOF read")
            .is_none());
    }

    #[test]
    fn prop_codec_record_facade_matches_microcrate(payload in vec(any::<u8>(), 0..=512)) {
        let mut encoded = Vec::new();
        write_record(&mut encoded, &payload, RecordFormat::RDW)
            .expect("rdw write should succeed");

        let mut cursor = Cursor::new(encoded);
        let decoded = copybook_codec::record::read_record(&mut cursor, RecordFormat::RDW, None)
            .expect("codec facade read should succeed")
            .expect("one record expected");
        prop_assert_eq!(decoded, payload);
    }
}
