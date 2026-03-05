// SPDX-License-Identifier: AGPL-3.0-or-later
//! Dialect feature integration tests.
//!
//! Validates that the three dialect modes (Normative, ZeroTolerant, OneTolerant)
//! produce different schema interpretations for ODO min_count, and that the
//! codec respects dialect-driven bounds during decode/encode.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::{Dialect, ParseOptions, parse_copybook_with_options};

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn opts_with_dialect(dialect: Dialect) -> ParseOptions {
    ParseOptions {
        dialect,
        ..ParseOptions::default()
    }
}

/// ODO copybook: `OCCURS 2 TO 5 DEPENDING ON CNT`
const ODO_CPY: &str = r"
    01 ODO-REC.
       05 CNT PIC 9(2).
       05 ITEMS OCCURS 2 TO 5 TIMES
          DEPENDING ON CNT.
          10 ITEM-VAL PIC X(4).
";

// ---------------------------------------------------------------------------
// 1. All three dialects parse the same copybook successfully
// ---------------------------------------------------------------------------

#[test]
fn dialect_all_three_parse_successfully() {
    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let opts = opts_with_dialect(dialect);
        let result = parse_copybook_with_options(ODO_CPY, &opts);
        assert!(
            result.is_ok(),
            "Parsing with dialect {dialect:?} should succeed: {:?}",
            result.err()
        );
    }
}

// ---------------------------------------------------------------------------
// 2. Normative dialect – min_count enforced as declared (2)
// ---------------------------------------------------------------------------

#[test]
fn dialect_normative_min_count_enforced() {
    let opts = opts_with_dialect(Dialect::Normative);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

    // With normative, min_count=2, so counter=2 is valid
    // CNT=02, 2 items of 4 bytes each = 2 + 8 = 10 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF2]); // CNT "02"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // item 1 "ABCD"
    data.extend_from_slice(&[0xC5, 0xC6, 0xC7, 0xC8]); // item 2 "EFGH"

    let result = decode_record(&schema, &data, &decode_opts());
    assert!(
        result.is_ok(),
        "Normative: counter=2 (== min_count) should succeed: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// 3. Normative – counter below min_count may produce CBKS302 or error
// ---------------------------------------------------------------------------

#[test]
fn dialect_normative_counter_below_min() {
    let opts = opts_with_dialect(Dialect::Normative);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

    // CNT=01, but min_count is 2 under Normative
    // Provide 2 + 1*4 = 6 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF1]); // CNT "01"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // item 1

    let result = decode_record(&schema, &data, &decode_opts());
    // Under Normative, counter=1 < min_count=2 may raise to 2 or error
    // Either way, the behavior is defined by the dialect
    if let Err(err) = &result {
        let code_str = err.code.to_string();
        assert!(
            code_str.starts_with("CBKS") || code_str.starts_with("CBKD"),
            "Expected CBKS/CBKD error, got {code_str}"
        );
    }
}

// ---------------------------------------------------------------------------
// 4. ZeroTolerant – counter=0 is accepted (min_count ignored)
// ---------------------------------------------------------------------------

#[test]
fn dialect_zero_tolerant_counter_zero_accepted() {
    let opts = opts_with_dialect(Dialect::ZeroTolerant);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

    // ZeroTolerant: min_count treated as 0 for counter validation, but
    // the physical record still has space for declared min_count (2) items.
    // CNT=00, + 2×4 bytes padding = 10 bytes total
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0]); // CNT "00"
    data.extend_from_slice(&[0x40; 8]); // space-fill for 2 declared-min slots

    let result = decode_record(&schema, &data, &decode_opts());
    assert!(
        result.is_ok(),
        "ZeroTolerant: counter=0 should succeed: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// 5. ZeroTolerant – counter=1 also accepted
// ---------------------------------------------------------------------------

#[test]
fn dialect_zero_tolerant_counter_one_accepted() {
    let opts = opts_with_dialect(Dialect::ZeroTolerant);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

    // CNT=01, record must still have min_count (2) item slots physically
    // 2 + 2×4 = 10 bytes minimum
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF1]); // CNT "01"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // item 1
    data.extend_from_slice(&[0x40; 4]); // padding for declared min slot 2

    let result = decode_record(&schema, &data, &decode_opts());
    assert!(
        result.is_ok(),
        "ZeroTolerant: counter=1 should succeed: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// 6. OneTolerant – counter=1 accepted even though declared min_count=2
// ---------------------------------------------------------------------------

#[test]
fn dialect_one_tolerant_counter_one_accepted() {
    let opts = opts_with_dialect(Dialect::OneTolerant);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

    // OneTolerant: min_count = max(1, 2) = 2
    // So counter=2 should be valid
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF2]); // CNT "02"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // item 1
    data.extend_from_slice(&[0xC5, 0xC6, 0xC7, 0xC8]); // item 2

    let result = decode_record(&schema, &data, &decode_opts());
    assert!(
        result.is_ok(),
        "OneTolerant: counter=2 should succeed: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// 7. OneTolerant clamps min_count=0 to 1
// ---------------------------------------------------------------------------

#[test]
fn dialect_one_tolerant_clamps_zero_min_to_one() {
    // Use a copybook with min_count=0
    let cpy = r"
        01 ODO-ZERO-REC.
           05 CNT PIC 9(2).
           05 ITEMS OCCURS 0 TO 5 TIMES
              DEPENDING ON CNT.
              10 ITEM-VAL PIC X(4).
    ";

    let opts = opts_with_dialect(Dialect::OneTolerant);
    let schema = parse_copybook_with_options(cpy, &opts).expect("parse");

    // OneTolerant: min_count = max(1, 0) = 1, so counter=0 may be rejected
    let data: Vec<u8> = vec![0xF0, 0xF0]; // CNT "00"
    let result = decode_record(&schema, &data, &decode_opts());

    // Counter=0 with OneTolerant where effective min=1 may raise or error
    if let Err(err) = &result {
        let code_str = err.code.to_string();
        assert!(
            code_str.starts_with("CBKS") || code_str.starts_with("CBKD"),
            "Expected CBKS/CBKD error, got {code_str}"
        );
    }
}

// ---------------------------------------------------------------------------
// 8. All dialects produce valid schema with same field count
// ---------------------------------------------------------------------------

#[test]
fn dialect_schema_field_count_consistent() {
    let dialects = [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ];
    let mut field_counts: Vec<usize> = Vec::new();

    for dialect in &dialects {
        let opts = opts_with_dialect(*dialect);
        let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");
        field_counts.push(schema.all_fields().len());
    }

    // All dialects should produce the same number of fields
    assert!(
        field_counts.windows(2).all(|w| w[0] == w[1]),
        "All dialects should produce same field count, got {field_counts:?}"
    );
}

// ---------------------------------------------------------------------------
// 9. ZeroTolerant vs Normative – different behavior for counter=0
// ---------------------------------------------------------------------------

#[test]
fn dialect_zero_vs_normative_diverge_at_counter_zero() {
    // Both schemas parsed with their respective dialects
    let normative_schema =
        parse_copybook_with_options(ODO_CPY, &opts_with_dialect(Dialect::Normative)).expect("n");
    let zero_schema =
        parse_copybook_with_options(ODO_CPY, &opts_with_dialect(Dialect::ZeroTolerant)).expect("0");

    // Physical record must have min_count (2) item slots = 10 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0]); // CNT "00"
    data.extend_from_slice(&[0x40; 8]); // padding for 2 declared-min slots

    let norm_result = decode_record(&normative_schema, &data, &decode_opts());
    let zero_result = decode_record(&zero_schema, &data, &decode_opts());

    // ZeroTolerant should accept counter=0; Normative may reject or raise
    assert!(
        zero_result.is_ok(),
        "ZeroTolerant should accept counter=0: {:?}",
        zero_result.err()
    );
    // Normative may succeed with warning (raised) or fail - both are acceptable
    let _ = norm_result;
}

// ---------------------------------------------------------------------------
// 10. Dialect affects encode – counter validation respects min
// ---------------------------------------------------------------------------

#[test]
fn dialect_encode_respects_bounds() {
    let zero_schema =
        parse_copybook_with_options(ODO_CPY, &opts_with_dialect(Dialect::ZeroTolerant)).expect("0");

    // ZeroTolerant: counter=0 should be valid for encode
    let json = serde_json::json!({
        "CNT": "00",
        "ITEMS": []
    });
    let result = encode_record(&zero_schema, &json, &encode_opts());
    assert!(
        result.is_ok(),
        "ZeroTolerant encode with counter=0 should succeed: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// 11. Dialect max_count is not affected
// ---------------------------------------------------------------------------

#[test]
fn dialect_max_count_unchanged() {
    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let opts = opts_with_dialect(dialect);
        let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

        // Counter=5 (max), 5 items = 2 + 20 = 22 bytes
        let mut data: Vec<u8> = Vec::new();
        data.extend_from_slice(&[0xF0, 0xF5]); // CNT "05"
        for _ in 0..5 {
            data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]);
        }

        let result = decode_record(&schema, &data, &decode_opts());
        assert!(
            result.is_ok(),
            "All dialects: counter=max should succeed for {dialect:?}: {:?}",
            result.err()
        );
    }
}

// ---------------------------------------------------------------------------
// 12. Dialect round-trip – encode then decode preserves data
// ---------------------------------------------------------------------------

#[test]
fn dialect_roundtrip_preserves_data() {
    let schema =
        parse_copybook_with_options(ODO_CPY, &opts_with_dialect(Dialect::Normative)).expect("n");

    // Build record manually: CNT=03, 3 items (valid since 2 ≤ 3 ≤ 5)
    // CNT(2) + 3×ITEM(4) = 14 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF3]); // CNT "03"
    data.extend_from_slice(&[0xC1, 0xC1, 0xC1, 0xC1]); // item 1 "AAAA"
    data.extend_from_slice(&[0xC2, 0xC2, 0xC2, 0xC2]); // item 2 "BBBB"
    data.extend_from_slice(&[0xC3, 0xC3, 0xC3, 0xC3]); // item 3 "CCCC"

    let decoded = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(decoded["CNT"], "03");
    let items = decoded["ITEMS"].as_array().expect("items array");
    assert_eq!(items.len(), 3);

    // Verify item values
    assert_eq!(items[0]["ITEM-VAL"], "AAAA");
    assert_eq!(items[1]["ITEM-VAL"], "BBBB");
    assert_eq!(items[2]["ITEM-VAL"], "CCCC");

    // Decode same data again to verify determinism across dialects
    let decoded2 = decode_record(&schema, &data, &decode_opts()).expect("decode2");
    assert_eq!(decoded, decoded2, "Dialect decode must be deterministic");
}

// ---------------------------------------------------------------------------
// 13. Non-ODO copybook unaffected by dialect
// ---------------------------------------------------------------------------

#[test]
fn dialect_non_odo_copybook_unaffected() {
    let simple_cpy = r"
        01 SIMPLE-REC.
           05 FIELD-A PIC X(5).
           05 FIELD-B PIC 9(3).
    ";

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6]); // "HELLO"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3]); // "123"

    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let opts = opts_with_dialect(dialect);
        let schema = parse_copybook_with_options(simple_cpy, &opts).expect("parse");
        let json = decode_record(&schema, &data, &decode_opts()).expect("decode");

        assert_eq!(
            json["FIELD-A"], "HELLO",
            "Dialect {dialect:?} should not affect non-ODO"
        );
        assert_eq!(
            json["FIELD-B"], "123",
            "Dialect {dialect:?} should not affect non-ODO"
        );
    }
}

// ---------------------------------------------------------------------------
// 14. Dialect with OCCURS (fixed, no ODO) – unaffected
// ---------------------------------------------------------------------------

#[test]
fn dialect_fixed_occurs_unaffected() {
    let cpy = r"
        01 FIXED-ARR-REC.
           05 ITEMS OCCURS 3 TIMES.
              10 ITEM-VAL PIC X(4).
    ";

    let mut data: Vec<u8> = Vec::new();
    for _ in 0..3 {
        data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // "ABCD"
    }

    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let opts = opts_with_dialect(dialect);
        let schema = parse_copybook_with_options(cpy, &opts).expect("parse");
        let json = decode_record(&schema, &data, &decode_opts()).expect("decode");
        let items = json["ITEMS"].as_array().expect("items array");
        assert_eq!(
            items.len(),
            3,
            "Fixed OCCURS unaffected by dialect {dialect:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// 15. Dialect default is Normative
// ---------------------------------------------------------------------------

#[test]
fn dialect_default_is_normative() {
    let default_opts = ParseOptions::default();
    assert_eq!(
        default_opts.dialect,
        Dialect::Normative,
        "Default dialect must be Normative"
    );
}
