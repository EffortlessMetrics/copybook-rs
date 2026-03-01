// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property-based "no-panic" fuzzing tests.
//!
//! These tests verify that core APIs never panic on arbitrary input.
//! Every test feeds random data to a public API and asserts the call
//! returns either `Ok` or `Err` — never an unwinding panic.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::io::Cursor;
use std::panic::catch_unwind;

use copybook_charset::ebcdic_to_utf8;
use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record};
use copybook_codepage::UnmappablePolicy;
use copybook_core::parse_copybook;
use copybook_fixed::FixedRecordReader;
use copybook_rdw::RDWRecordReader;
use proptest::collection::vec;
use proptest::prelude::*;
use serde_json::{Map, Value};

use super::config::DEFAULT_CASES;

// ---------------------------------------------------------------------------
// Helper: a fixed mixed-type schema used by decode/encode fuzz tests
// ---------------------------------------------------------------------------
const MIXED_SCHEMA: &str = "\
       01 REC.
         05 FLD-X     PIC X(10).
         05 FLD-9     PIC 9(5).
         05 FLD-P     PIC S9(7)V99 COMP-3.
";

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
}

// ---------------------------------------------------------------------------
// 1. Arbitrary bytes → decode_record never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_decode_arbitrary_bytes_no_panic(data in vec(any::<u8>(), 0..500)) {
        let schema = parse_copybook(MIXED_SCHEMA).expect("static schema");
        let opts = decode_opts();
        let result = catch_unwind(|| {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode_record panicked on input len={}", data.len());
    }
}

// ---------------------------------------------------------------------------
// 2. Arbitrary JSON string → encode_record never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_encode_arbitrary_json_no_panic(
        fld_x in ".*",
        fld_9 in ".*",
        fld_p in ".*",
    ) {
        let schema = parse_copybook(MIXED_SCHEMA).expect("static schema");
        let opts = encode_opts();
        let mut map = Map::new();
        map.insert("FLD-X".into(), Value::String(fld_x));
        map.insert("FLD-9".into(), Value::String(fld_9));
        map.insert("FLD-P".into(), Value::String(fld_p));
        let json = Value::Object({
            let mut outer = Map::new();
            outer.insert("REC".into(), Value::Object(map));
            outer
        });
        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &json, &opts);
        });
        prop_assert!(result.is_ok(), "encode_record panicked");
    }
}

// ---------------------------------------------------------------------------
// 3. Arbitrary copybook text → parse_copybook never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_parse_arbitrary_text_no_panic(text in ".{0,1000}") {
        let result = catch_unwind(move || {
            let _ = parse_copybook(&text);
        });
        prop_assert!(result.is_ok(), "parse_copybook panicked");
    }
}

// ---------------------------------------------------------------------------
// 4. Arbitrary bytes → EBCDIC conversion never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_ebcdic_conversion_no_panic(data in vec(any::<u8>(), 0..500)) {
        let data_len = data.len();
        let result = catch_unwind(move || {
            let _ = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace);
        });
        prop_assert!(result.is_ok(), "ebcdic_to_utf8 panicked on len={}", data_len);
    }
}

// ---------------------------------------------------------------------------
// 5. Arbitrary record length → fixed reader never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_fixed_reader_arbitrary_no_panic(
        lrecl in 1u32..100_000u32,
        data in vec(any::<u8>(), 0..500),
    ) {
        let result = catch_unwind(move || {
            if let Ok(mut reader) = FixedRecordReader::new(Cursor::new(data), Some(lrecl)) {
                while let Ok(Some(_)) = reader.read_record() {}
            }
        });
        prop_assert!(result.is_ok(), "FixedRecordReader panicked with lrecl={lrecl}");
    }
}

// ---------------------------------------------------------------------------
// 6. Arbitrary bytes → RDW reader never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_reader_arbitrary_no_panic(data in vec(any::<u8>(), 0..500)) {
        let result = catch_unwind(move || {
            let mut reader = RDWRecordReader::new(Cursor::new(data), false);
            while let Ok(Some(_)) = reader.read_record() {}
        });
        prop_assert!(result.is_ok(), "RDWRecordReader panicked");
    }
}

// ---------------------------------------------------------------------------
// 7. Arbitrary field count schema + arbitrary data → decode never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_variable_schema_decode_no_panic(
        field_count in 1usize..=20,
        field_lengths in vec(1u32..=50, 1..=20),
        data in vec(any::<u8>(), 0..500),
    ) {
        let count = field_count.min(field_lengths.len());
        let mut copybook = String::from("       01 REC.\n");
        for (i, len) in field_lengths.iter().take(count).enumerate() {
            copybook.push_str(&format!("         05 FLD-{i:03} PIC X({len}).\n"));
        }

        let result = catch_unwind(move || {
            if let Ok(schema) = parse_copybook(&copybook) {
                let opts = DecodeOptions::new()
                    .with_format(RecordFormat::Fixed)
                    .with_codepage(Codepage::ASCII);
                let _ = decode_record(&schema, &data, &opts);
            }
        });
        prop_assert!(result.is_ok(), "decode panicked with {count} fields");
    }
}
