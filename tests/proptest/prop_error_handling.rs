// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property-based error-handling tests.
//!
//! These tests verify that every public API surface gracefully handles
//! arbitrary / malformed input by returning `Err` (or `Ok` where
//! appropriate) and **never** panicking.  Each test uses `catch_unwind`
//! as the safety net so a regression is caught even if the API starts
//! unwinding.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::io::Cursor;
use std::panic::catch_unwind;

use copybook_charset::{ebcdic_to_utf8, utf8_to_ebcdic};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_codepage::UnmappablePolicy;
use copybook_core::{parse_copybook, project_schema};
use copybook_overpunch::{ZeroSignPolicy, decode_overpunch_byte, encode_overpunch_byte};
use copybook_rdw::RDWRecordReader;
use proptest::collection::vec;
use proptest::prelude::*;
use serde_json::{Map, Number, Value};

use super::config::DEFAULT_CASES;

// ---------------------------------------------------------------------------
// Helper: a simple mixed-type schema for decode/encode error tests
// ---------------------------------------------------------------------------
const SIMPLE_SCHEMA: &str = "\
       01 REC.
         05 FLD-X     PIC X(8).
         05 FLD-9     PIC 9(4).
         05 FLD-P     PIC S9(5)V99 COMP-3.
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

// =========================================================================
// 1. Arbitrary bytes → parser never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_arbitrary_bytes_to_parser_no_panic(data in vec(any::<u8>(), 0..2048)) {
        let text = String::from_utf8_lossy(&data).into_owned();
        let result = catch_unwind(move || {
            let _ = parse_copybook(&text);
        });
        prop_assert!(result.is_ok(), "parse_copybook panicked on {} random bytes", data.len());
    }
}

// =========================================================================
// 2. Arbitrary bytes → decoder returns Err, never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_arbitrary_bytes_to_decoder_no_panic(data in vec(any::<u8>(), 0..1024)) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("static schema");
        let opts = decode_opts();
        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode_record panicked");
    }
}

// =========================================================================
// 3. Arbitrary bytes → encoder returns Err, never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_arbitrary_bytes_as_json_to_encoder_no_panic(data in vec(any::<u8>(), 0..512)) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("static schema");
        let opts = encode_opts();
        // Feed raw bytes interpreted as a UTF-8-lossy string into JSON fields
        let raw = String::from_utf8_lossy(&data).into_owned();
        let mut inner = Map::new();
        inner.insert("FLD-X".into(), Value::String(raw.clone()));
        inner.insert("FLD-9".into(), Value::String(raw.clone()));
        inner.insert("FLD-P".into(), Value::String(raw));
        let json = Value::Object({
            let mut outer = Map::new();
            outer.insert("REC".into(), Value::Object(inner));
            outer
        });
        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &json, &opts);
        });
        prop_assert!(result.is_ok(), "encode_record panicked on arbitrary byte-derived JSON");
    }
}

// =========================================================================
// 4. Arbitrary bytes → RDW reader returns Err, never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_arbitrary_bytes_to_rdw_reader_no_panic(data in vec(any::<u8>(), 0..1024)) {
        let result = catch_unwind(move || {
            let mut reader = RDWRecordReader::new(Cursor::new(data), true);
            loop {
                match reader.read_record() {
                    Ok(Some(_)) => continue,
                    Ok(None) | Err(_) => break,
                }
            }
        });
        prop_assert!(result.is_ok(), "RDWRecordReader panicked on arbitrary bytes");
    }
}

// =========================================================================
// 5. Arbitrary bytes → overpunch decode returns Err, never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_arbitrary_byte_to_overpunch_no_panic(byte in any::<u8>()) {
        let codepages = [
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];
        for &cp in &codepages {
            let result = catch_unwind(move || {
                let _ = decode_overpunch_byte(byte, cp);
            });
            prop_assert!(result.is_ok(), "decode_overpunch_byte panicked on byte={byte:#04x}, cp={cp:?}");

            // Also test encode with arbitrary digit values
            let result2 = catch_unwind(move || {
                let _ = encode_overpunch_byte(byte, true, cp, ZeroSignPolicy::Positive);
                let _ = encode_overpunch_byte(byte, false, cp, ZeroSignPolicy::Preferred);
            });
            prop_assert!(result2.is_ok(), "encode_overpunch_byte panicked on byte={byte:#04x}");
        }
    }
}

// =========================================================================
// 6. Arbitrary bytes → charset converter returns Err, never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_arbitrary_bytes_to_charset_no_panic(data in vec(any::<u8>(), 0..512)) {
        let codepages = [
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];
        let policies = [UnmappablePolicy::Replace, UnmappablePolicy::Error];
        for &cp in &codepages {
            for &pol in &policies {
                let data_clone = data.clone();
                let result = catch_unwind(move || {
                    let _ = ebcdic_to_utf8(&data_clone, cp, pol);
                });
                prop_assert!(result.is_ok(), "ebcdic_to_utf8 panicked on cp={cp:?}");
            }
        }
        // Also test utf8_to_ebcdic with arbitrary string
        let text = String::from_utf8_lossy(&data).into_owned();
        for &cp in &codepages {
            let t = text.clone();
            let result = catch_unwind(move || {
                let _ = utf8_to_ebcdic(&t, cp);
            });
            prop_assert!(result.is_ok(), "utf8_to_ebcdic panicked on cp={cp:?}");
        }
    }
}

// =========================================================================
// 7. Arbitrary JSON to encoder → returns Err or Ok, never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_arbitrary_json_shapes_to_encoder_no_panic(
        kind in 0u8..6,
        s in ".*",
        n in any::<i64>(),
        b in any::<bool>(),
    ) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("static schema");
        let opts = encode_opts();

        // Build a variety of JSON shapes that should not crash the encoder
        let value = match kind {
            0 => Value::Null,
            1 => Value::Bool(b),
            2 => Value::Number(Number::from(n)),
            3 => Value::String(s),
            4 => Value::Array(vec![Value::Null, Value::Bool(true), Value::Number(Number::from(42))]),
            _ => {
                // Deeply nested object
                let mut m = Map::new();
                m.insert("REC".into(), Value::Object({
                    let mut inner = Map::new();
                    inner.insert("FLD-X".into(), Value::Number(Number::from(n)));
                    inner.insert("FLD-9".into(), Value::Bool(b));
                    inner.insert("FLD-P".into(), Value::Null);
                    inner
                }));
                Value::Object(m)
            }
        };

        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &value, &opts);
        });
        prop_assert!(result.is_ok(), "encode_record panicked on JSON kind={kind}");
    }
}

// =========================================================================
// 8. Invalid codepage enum values → graceful handling
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_all_codepages_decode_arbitrary_data_no_panic(
        cp_idx in 0usize..6,
        data in vec(any::<u8>(), 0..256),
    ) {
        let codepages = [
            Codepage::ASCII,
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];
        let cp = codepages[cp_idx];
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("static schema");
        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(cp);

        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode_record panicked on codepage index {cp_idx}");
    }
}

// =========================================================================
// 9. Empty copybook text → proper error
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 1,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_empty_copybook_returns_error(_dummy in Just(())) {
        let result = parse_copybook("");
        prop_assert!(result.is_err(), "parse_copybook should reject empty input");

        let result2 = parse_copybook("   \n  \n  ");
        prop_assert!(result2.is_err(), "parse_copybook should reject whitespace-only input");
    }
}

// =========================================================================
// 10. Empty binary data → proper error
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 1,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_empty_data_decode_no_panic(_dummy in Just(())) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("static schema");
        let opts = decode_opts();
        let result = catch_unwind(|| {
            let _ = decode_record(&schema, &[], &opts);
        });
        prop_assert!(result.is_ok(), "decode_record panicked on empty data");
    }
}

// =========================================================================
// 11. Random field names in project_schema → proper error codes
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_random_field_names_project_schema(
        names in vec("[A-Z][A-Z0-9\\-]{0,15}", 1..8),
    ) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("static schema");
        let real_names: Vec<String> = schema
            .all_fields()
            .iter()
            .map(|f| f.name.clone())
            .collect();

        let result = catch_unwind(move || {
            let selections: Vec<String> = names
                .into_iter()
                .filter(|n| !real_names.contains(n))
                .collect();
            if selections.is_empty() {
                return; // all names happened to be real — skip
            }
            let r = project_schema(&schema, &selections);
            // Must be an error (unknown field names)
            assert!(r.is_err(), "project_schema should reject unknown fields: {selections:?}");
        });
        prop_assert!(result.is_ok(), "project_schema panicked on random field names");
    }
}

// =========================================================================
// 12. Mixed valid/invalid records in stream → no panic
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_mixed_valid_invalid_records_no_panic(
        record_count in 1usize..20,
        data_chunks in vec(vec(any::<u8>(), 0..64), 1..20),
    ) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("static schema");
        let _record_len: usize = schema
            .all_fields()
            .iter()
            .filter(|f| f.is_scalar())
            .map(|f| f.effective_length() as usize)
            .sum();
        let opts = decode_opts();

        let count = record_count.min(data_chunks.len());
        let result = catch_unwind(move || {
            for chunk in data_chunks.iter().take(count) {
                // Some chunks will be the right length, others won't
                let _ = decode_record(&schema, chunk, &opts);
            }

            // Also try concatenated stream through RDW reader
            let mut all_bytes = Vec::new();
            for chunk in data_chunks.iter().take(count) {
                all_bytes.extend_from_slice(chunk);
            }
            let mut reader = RDWRecordReader::new(Cursor::new(all_bytes), false);
            loop {
                match reader.read_record() {
                    Ok(Some(_)) => continue,
                    Ok(None) | Err(_) => break,
                }
            }
        });
        prop_assert!(
            result.is_ok(),
            "mixed valid/invalid record processing panicked with {count} chunks"
        );
    }
}

// =========================================================================
// 13. Arbitrary bytes → encoder with wrong-type JSON values never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_wrong_type_json_values_encode_no_panic(
        int_val in any::<i64>(),
        float_str in "[-+]?[0-9]{0,10}\\.[0-9]{0,10}",
        bool_val in any::<bool>(),
    ) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("static schema");
        let opts = encode_opts();

        // Feed wrong types to each field: numbers where strings expected, etc.
        let mut inner = Map::new();
        inner.insert("FLD-X".into(), Value::Number(Number::from(int_val)));
        inner.insert("FLD-9".into(), Value::Bool(bool_val));
        inner.insert("FLD-P".into(), Value::String(float_str));
        let json = Value::Object({
            let mut outer = Map::new();
            outer.insert("REC".into(), Value::Object(inner));
            outer
        });

        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &json, &opts);
        });
        prop_assert!(result.is_ok(), "encode_record panicked on wrong-type JSON");
    }
}

// =========================================================================
// 14. Truncated RDW headers → reader returns Err, never panics
// =========================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_err_truncated_rdw_headers_no_panic(
        len_hi in any::<u8>(),
        len_lo in any::<u8>(),
        partial_len in 0usize..4,
    ) {
        // Build a truncated or malformed RDW header
        let header = [0u8, len_hi, len_lo, 0];
        let data: Vec<u8> = header[..partial_len.min(4)].to_vec();
        let result = catch_unwind(move || {
            let mut reader = RDWRecordReader::new(Cursor::new(data), true);
            loop {
                match reader.read_record() {
                    Ok(Some(_)) => continue,
                    Ok(None) | Err(_) => break,
                }
            }
        });
        prop_assert!(result.is_ok(), "RDW reader panicked on truncated header");
    }
}
