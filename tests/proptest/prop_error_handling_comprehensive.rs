// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive property-based error-handling tests.
//!
//! Verifies that decode and encode APIs never panic on arbitrary, malformed,
//! truncated, or empty input. Each test uses `catch_unwind` as a safety net
//! to detect regressions that would otherwise cause unwinding panics.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::panic::catch_unwind;

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use proptest::collection::vec;
use proptest::prelude::*;
use serde_json::{Map, Number, Value};

use super::config::{DEFAULT_CASES, QUICK_CASES};

// ---------------------------------------------------------------------------
// Helper schemas
// ---------------------------------------------------------------------------
const MIXED_SCHEMA: &str = "\
       01 REC.
         05 FLD-X     PIC X(10).
         05 FLD-9     PIC 9(5).
         05 FLD-P     PIC S9(7)V99 COMP-3.
";

const MULTI_TYPE_SCHEMA: &str = "\
       01 REC.
         05 FLD-A     PIC X(8).
         05 FLD-N     PIC 9(4).
         05 FLD-S     PIC S9(3).
         05 FLD-P     PIC S9(5) COMP-3.
         05 FLD-B     PIC S9(4) BINARY.
";

fn decode_opts_ascii() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

fn decode_opts_cp037() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
}

fn encode_opts_cp037() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
}

// ============================================================================
// 1. Random byte sequences never cause panics in decode
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Arbitrary bytes fed to decode_record with a multi-type schema never panic.
    #[test]
    fn prop_ehc_random_bytes_decode_no_panic(data in vec(any::<u8>(), 0..512)) {
        let schema = parse_copybook(MULTI_TYPE_SCHEMA).expect("static schema");
        let opts = decode_opts_cp037();
        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode_record panicked on random bytes");
    }

    /// Random bytes with ASCII codepage never cause panics.
    #[test]
    fn prop_ehc_random_bytes_decode_ascii_no_panic(data in vec(any::<u8>(), 0..256)) {
        let schema = parse_copybook(MIXED_SCHEMA).expect("static schema");
        let opts = decode_opts_ascii();
        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode_record panicked on random ASCII bytes");
    }
}

// ============================================================================
// 2. Random JSON never causes panics in encode
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Arbitrary JSON shapes (null, bool, number, string, array, object) never panic.
    #[test]
    fn prop_ehc_random_json_encode_no_panic(
        kind in 0u8..8,
        s in "[\\x20-\\x7E]{0,50}",
        n in any::<i64>(),
        b in any::<bool>(),
    ) {
        let schema = parse_copybook(MULTI_TYPE_SCHEMA).expect("static schema");
        let opts = encode_opts_cp037();

        let value = match kind {
            0 => Value::Null,
            1 => Value::Bool(b),
            2 => Value::Number(Number::from(n)),
            3 => Value::String(s),
            4 => Value::Array(vec![Value::Null]),
            5 => Value::Object(Map::new()),
            6 => {
                let mut m = Map::new();
                m.insert("unexpected_key".into(), Value::Number(Number::from(n)));
                Value::Object(m)
            }
            _ => {
                let mut inner = Map::new();
                inner.insert("FLD-A".into(), Value::Number(Number::from(n)));
                inner.insert("FLD-N".into(), Value::Bool(b));
                inner.insert("FLD-P".into(), Value::Null);
                let mut outer = Map::new();
                outer.insert("REC".into(), Value::Object(inner));
                Value::Object(outer)
            }
        };

        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &value, &opts);
        });
        prop_assert!(result.is_ok(), "encode_record panicked on JSON kind={kind}");
    }

    /// Deeply nested JSON objects never cause panics in encode.
    #[test]
    fn prop_ehc_nested_json_encode_no_panic(
        depth in 1usize..=10,
        leaf_val in any::<i64>(),
    ) {
        let schema = parse_copybook(MIXED_SCHEMA).expect("static schema");
        let opts = encode_opts_cp037();

        let mut value = Value::Number(Number::from(leaf_val));
        for i in 0..depth {
            let mut m = Map::new();
            m.insert(format!("level-{i}"), value);
            value = Value::Object(m);
        }

        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &value, &opts);
        });
        prop_assert!(result.is_ok(), "encode_record panicked on depth={depth}");
    }
}

// ============================================================================
// 3. Truncated records produce errors, not panics
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Records shorter than LRECL never cause panics.
    #[test]
    fn prop_ehc_truncated_record_no_panic(
        short_len in 0usize..=19,
    ) {
        let schema = parse_copybook(MULTI_TYPE_SCHEMA).expect("static schema");
        let data = vec![0x40u8; short_len];
        let opts = decode_opts_cp037();

        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode panicked on truncated record len={short_len}");
    }

    /// Truncated records with varying byte patterns never panic.
    #[test]
    fn prop_ehc_truncated_random_bytes_no_panic(
        data in vec(any::<u8>(), 0..20),
    ) {
        let schema = parse_copybook(MULTI_TYPE_SCHEMA).expect("static schema");
        let opts = decode_opts_cp037();

        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode panicked on truncated random data");
    }

    /// Truncated data for PIC X fields specifically never causes panics.
    #[test]
    fn prop_ehc_truncated_pic_x_no_panic(
        field_len in 5u32..=30,
        data_len in 0usize..=4,
    ) {
        let copybook = format!("       01 REC.\n           05 FLD PIC X({field_len}).");
        let schema = parse_copybook(&copybook).expect("parse");
        let data = vec![b'A'; data_len];
        let opts = decode_opts_ascii();

        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode panicked on truncated PIC X data");
    }
}

// ============================================================================
// 4. Empty input produces errors, not panics
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Empty data with varying schemas never causes panics.
    #[test]
    fn prop_ehc_empty_data_varying_schema_no_panic(
        n_fields in 1usize..=10,
        field_len in 1u32..=20,
    ) {
        let copybook = (0..n_fields)
            .map(|i| format!("       05  EF-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let opts = decode_opts_ascii();

        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &[], &opts);
        });
        prop_assert!(result.is_ok(), "decode panicked on empty data");
    }

    /// Empty JSON object passed to encode never causes panics.
    #[test]
    fn prop_ehc_empty_json_encode_no_panic(
        n_fields in 1usize..=5,
        field_len in 1u32..=10,
    ) {
        let fields: String = (0..n_fields)
            .map(|i| format!("           05 EE-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01 REC.\n{fields}");

        let schema = parse_copybook(&copybook).expect("parse");
        let json = Value::Object(Map::new());
        let opts = encode_opts_cp037();

        let result = catch_unwind(move || {
            let _ = encode_record(&schema, &json, &opts);
        });
        prop_assert!(result.is_ok(), "encode panicked on empty JSON");
    }
}

// ============================================================================
// 5. Variable-schema decode with mismatched data sizes never panics
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Dynamically generated schemas with random-length data never panic.
    #[test]
    fn prop_ehc_variable_schema_random_data_no_panic(
        field_count in 1usize..=8,
        field_len in 1u32..=15,
        data in vec(any::<u8>(), 0..200),
    ) {
        let fields: String = (0..field_count)
            .map(|i| format!("         05 VF-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01 REC.\n{fields}");

        let result = catch_unwind(move || {
            if let Ok(schema) = parse_copybook(&copybook) {
                let opts = DecodeOptions::new()
                    .with_format(RecordFormat::Fixed)
                    .with_codepage(Codepage::ASCII);
                let _ = decode_record(&schema, &data, &opts);
            }
        });
        prop_assert!(result.is_ok(), "variable schema decode panicked");
    }

    /// Oversized data (longer than LRECL) doesn't cause panics.
    #[test]
    fn prop_ehc_oversized_data_no_panic(
        field_len in 1u32..=5,
        extra_len in 1usize..=100,
    ) {
        let copybook = format!("       01 REC.\n           05 FLD PIC X({field_len}).");
        let schema = parse_copybook(&copybook).expect("parse");
        let data = vec![b'X'; field_len as usize + extra_len];
        let opts = decode_opts_ascii();

        let result = catch_unwind(move || {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode panicked on oversized data");
    }
}

// ============================================================================
// 6. Random copybook text never causes parser panics
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Completely random text fed to parse_copybook never panics.
    #[test]
    fn prop_ehc_random_text_parser_no_panic(text in "[\\x20-\\x7E]{0,200}") {
        let result = catch_unwind(move || {
            let _ = parse_copybook(&text);
        });
        prop_assert!(result.is_ok(), "parse_copybook panicked on random text");
    }

    /// Random bytes interpreted as lossy UTF-8 never cause parser panics.
    #[test]
    fn prop_ehc_random_bytes_parser_no_panic(data in vec(any::<u8>(), 0..512)) {
        let text = String::from_utf8_lossy(&data).into_owned();
        let result = catch_unwind(move || {
            let _ = parse_copybook(&text);
        });
        prop_assert!(result.is_ok(), "parse_copybook panicked on random bytes");
    }
}
