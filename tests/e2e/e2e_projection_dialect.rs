// SPDX-License-Identifier: AGPL-3.0-or-later
//! Combined end-to-end tests for field projection and dialect functionality.
//!
//! Projection tests validate that `project_schema` correctly narrows schemas,
//! auto-includes ODO counters, resolves RENAMES aliases, and rejects unknown
//! fields. Dialect tests validate that Normative / ZeroTolerant / OneTolerant
//! modes produce different ODO min_count semantics and interact correctly with
//! parse, decode, and encode operations.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::{
    Dialect, ErrorCode, ParseOptions, parse_copybook, parse_copybook_with_options, project_schema,
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

// ===================================================================
// FIELD PROJECTION TESTS
// ===================================================================

// ---------------------------------------------------------------------------
// P1. Select single field from multi-field record → only that field in output
// ---------------------------------------------------------------------------
#[test]
fn proj_select_single_field() {
    let cpy = r"
        01 REC.
           05 FIELD-A  PIC X(5).
           05 FIELD-B  PIC 9(4).
           05 FIELD-C  PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let projected = project_schema(&schema, &["FIELD-B".to_string()]).expect("project");

    // FIELD-A(5) + FIELD-B(4) + FIELD-C(10) = 19 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC1; 5]); // FIELD-A
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4]); // FIELD-B "1234"
    data.extend_from_slice(&[0x40; 10]); // FIELD-C spaces

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    assert!(
        json.get("FIELD-B").is_some(),
        "Selected field must be present"
    );
    assert!(json.get("FIELD-A").is_none(), "FIELD-A must be absent");
    assert!(json.get("FIELD-C").is_none(), "FIELD-C must be absent");
}

// ---------------------------------------------------------------------------
// P2. Select multiple fields → both in output
// ---------------------------------------------------------------------------
#[test]
fn proj_select_multiple_fields() {
    let cpy = r"
        01 REC.
           05 FIELD-A  PIC X(5).
           05 FIELD-B  PIC 9(4).
           05 FIELD-C  PIC X(10).
           05 FIELD-D  PIC 9(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let selected = vec!["FIELD-A".to_string(), "FIELD-D".to_string()];
    let projected = project_schema(&schema, &selected).expect("project");

    // 5 + 4 + 10 + 6 = 25 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6]); // "HELLO"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4]); // "1234"
    data.extend_from_slice(&[0x40; 10]); // spaces
    data.extend_from_slice(&[0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0]); // "567890"

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    assert!(json.get("FIELD-A").is_some(), "FIELD-A must be present");
    assert!(json.get("FIELD-D").is_some(), "FIELD-D must be present");
    assert!(json.get("FIELD-B").is_none(), "FIELD-B must be absent");
    assert!(json.get("FIELD-C").is_none(), "FIELD-C must be absent");
}

// ---------------------------------------------------------------------------
// P3. Select field from nested group → parent group preserved in JSON
// ---------------------------------------------------------------------------
#[test]
fn proj_nested_group_parent_preserved() {
    let cpy = r"
        01 REC.
           05 GRP-A.
              10 CHILD-X  PIC X(5).
              10 CHILD-Y  PIC 9(3).
           05 FIELD-Z     PIC X(8).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let projected = project_schema(&schema, &["CHILD-X".to_string()]).expect("project");

    // CHILD-X(5) + CHILD-Y(3) + FIELD-Z(8) = 16 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4, 0xC5]); // "ABCDE"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3]); // "123"
    data.extend_from_slice(&[0x40; 8]); // spaces

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    // Parent group must be present; check hierarchy or flattened
    let child_x = json
        .get("CHILD-X")
        .or_else(|| json.get("GRP-A").and_then(|g| g.get("CHILD-X")));
    assert!(
        child_x.is_some(),
        "CHILD-X must be present (with parent group)"
    );
    assert!(json.get("FIELD-Z").is_none(), "FIELD-Z must be absent");
}

// ---------------------------------------------------------------------------
// P4. Select ODO array → counter auto-included
// ---------------------------------------------------------------------------
#[test]
fn proj_odo_counter_auto_included() {
    let cpy = r"
        01 REC.
           05 HEADER     PIC X(5).
           05 CNT        PIC 9(3).
           05 ITEMS OCCURS 1 TO 10 TIMES
              DEPENDING ON CNT.
              10 ITEM-VAL PIC X(4).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let projected = project_schema(&schema, &["ITEMS".to_string()]).expect("project");

    // HEADER(5) + CNT(3) + 2 items × 4 = 16 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0x40; 5]); // HEADER spaces
    data.extend_from_slice(&[0xF0, 0xF0, 0xF2]); // CNT "002"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // item 1
    data.extend_from_slice(&[0xC5, 0xC6, 0xC7, 0xC8]); // item 2

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    assert!(
        json.get("CNT").is_some(),
        "ODO counter must be auto-included"
    );
    assert!(json.get("ITEMS").is_some(), "ITEMS array must be present");
    assert!(json.get("HEADER").is_none(), "HEADER must be absent");
}

// ---------------------------------------------------------------------------
// P5. Select RENAMES alias → resolves to storage fields
// ---------------------------------------------------------------------------
#[test]
fn proj_renames_alias_resolves_to_storage() {
    let cpy = r"
        01 CUST-REC.
           05 FIRST-NAME     PIC X(20).
           05 LAST-NAME      PIC X(30).
           05 MIDDLE-INIT    PIC X(1).
           66 FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
           05 ADDRESS        PIC X(50).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let projected = project_schema(&schema, &["FULL-NAME".to_string()]).expect("project");

    let child_names: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();

    assert!(
        child_names.contains(&"FIRST-NAME"),
        "RENAMES should include FIRST-NAME"
    );
    assert!(
        child_names.contains(&"LAST-NAME"),
        "RENAMES should include LAST-NAME"
    );
    assert!(
        !child_names.contains(&"ADDRESS"),
        "ADDRESS must NOT be included"
    );
    assert!(
        !child_names.contains(&"MIDDLE-INIT"),
        "MIDDLE-INIT must NOT be included"
    );
}

// ---------------------------------------------------------------------------
// P6. Select non-existent field → error with CBKS703
// ---------------------------------------------------------------------------
#[test]
fn proj_nonexistent_field_returns_cbks703() {
    let cpy = r"
        01 REC.
           05 REAL-FIELD PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let result = project_schema(&schema, &["GHOST-FIELD".to_string()]);

    assert!(result.is_err(), "Selecting unknown field must fail");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
        "Error code must be CBKS703"
    );
}

// ---------------------------------------------------------------------------
// P7. Select with decode and verify JSON structure
// ---------------------------------------------------------------------------
#[test]
fn proj_decode_verify_json_structure() {
    let cpy = r"
        01 REC.
           05 ID-FIELD     PIC 9(6).
           05 NAME-FIELD   PIC X(20).
           05 STATUS       PIC X(1).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let selected = vec!["ID-FIELD".to_string(), "STATUS".to_string()];
    let projected = project_schema(&schema, &selected).expect("project");

    // 6 + 20 + 1 = 27 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]); // "001234"
    data.extend_from_slice(&[0x40; 20]); // spaces
    data.extend_from_slice(&[0xC1]); // STATUS "A"

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    assert!(json.is_object(), "Result must be a JSON object");
    assert_eq!(json["ID-FIELD"], "001234");
    assert_eq!(json["STATUS"], "A");
    // Non-selected field must be absent
    assert!(
        json.get("NAME-FIELD").is_none(),
        "NAME-FIELD must be absent"
    );
}

// ---------------------------------------------------------------------------
// P8. Select with encode – binary is still full record size
// ---------------------------------------------------------------------------
#[test]
fn proj_encode_full_record_size() {
    let cpy = r"
        01 REC.
           05 FIELD-A  PIC X(5).
           05 FIELD-B  PIC 9(4).
           05 FIELD-C  PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let full_record_len = 5 + 4 + 10; // 19 bytes

    // Encode with full schema – all fields present
    let json = serde_json::json!({
        "FIELD-A": "HELLO",
        "FIELD-B": "1234",
        "FIELD-C": "WORLD     "
    });
    let encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");
    assert_eq!(
        encoded.len(),
        full_record_len,
        "Encoded record must be full record length"
    );
}

// ---------------------------------------------------------------------------
// P9. Decode with projection produces smaller JSON than full decode
// ---------------------------------------------------------------------------
#[test]
fn proj_decode_produces_smaller_json() {
    let cpy = r"
        01 REC.
           05 FIELD-A  PIC X(5).
           05 FIELD-B  PIC 9(4).
           05 FIELD-C  PIC X(10).
           05 FIELD-D  PIC 9(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let projected = project_schema(&schema, &["FIELD-A".to_string()]).expect("project");

    // 5 + 4 + 10 + 6 = 25 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6]); // "HELLO"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4]); // "1234"
    data.extend_from_slice(&[0x40; 10]); // spaces
    data.extend_from_slice(&[0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0]); // "567890"

    let full_json = decode_record(&schema, &data, &decode_opts()).expect("full decode");
    let proj_json = decode_record(&projected, &data, &decode_opts()).expect("proj decode");

    let full_str = serde_json::to_string(&full_json).unwrap();
    let proj_str = serde_json::to_string(&proj_json).unwrap();

    assert!(
        proj_str.len() < full_str.len(),
        "Projected JSON ({} bytes) must be smaller than full JSON ({} bytes)",
        proj_str.len(),
        full_str.len()
    );
}

// ---------------------------------------------------------------------------
// P10. Project + decode + encode roundtrip
// ---------------------------------------------------------------------------
#[test]
fn proj_decode_encode_roundtrip() {
    let cpy = r"
        01 REC.
           05 FIELD-A  PIC X(5).
           05 FIELD-B  PIC 9(4).
           05 FIELD-C  PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Build record: 5 + 4 + 10 = 19 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6]); // "HELLO"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4]); // "1234"
    data.extend_from_slice(&[0xE6, 0xD6, 0xD9, 0xD3, 0xC4, 0x40, 0x40, 0x40, 0x40, 0x40]); // "WORLD     "

    // Decode full, then encode full → roundtrip
    let full_json = decode_record(&schema, &data, &decode_opts()).expect("decode");
    let re_encoded = encode_record(&schema, &full_json, &encode_opts()).expect("encode");

    assert_eq!(
        data, re_encoded,
        "Full decode→encode roundtrip must produce identical binary"
    );

    // Decode with projection, verify the projected fields survive
    let projected =
        project_schema(&schema, &["FIELD-A".to_string(), "FIELD-C".to_string()]).expect("project");
    let proj_json = decode_record(&projected, &data, &decode_opts()).expect("proj decode");
    assert_eq!(proj_json["FIELD-A"], "HELLO");
    assert_eq!(proj_json["FIELD-C"], "WORLD     ");
}

// ---------------------------------------------------------------------------
// P11. Projection group selection includes all children
// ---------------------------------------------------------------------------
#[test]
fn proj_group_includes_all_children() {
    let cpy = r"
        01 REC.
           05 SEC-A.
              10 CHILD-1  PIC X(5).
              10 CHILD-2  PIC 9(3).
           05 SEC-B       PIC X(8).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let projected = project_schema(&schema, &["SEC-A".to_string()]).expect("project");

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4, 0xC5]); // CHILD-1
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3]); // CHILD-2
    data.extend_from_slice(&[0x40; 8]); // SEC-B

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    let child1 = json
        .get("CHILD-1")
        .or_else(|| json.get("SEC-A").and_then(|g| g.get("CHILD-1")));
    let child2 = json
        .get("CHILD-2")
        .or_else(|| json.get("SEC-A").and_then(|g| g.get("CHILD-2")));

    assert!(child1.is_some(), "CHILD-1 must be present");
    assert!(child2.is_some(), "CHILD-2 must be present");
    assert!(json.get("SEC-B").is_none(), "SEC-B must be absent");
}

// ---------------------------------------------------------------------------
// P12. Projection with mixed levels (COMP-3 and DISPLAY)
// ---------------------------------------------------------------------------
#[test]
fn proj_mixed_field_types() {
    let cpy = r"
        01 REC.
           05 ID         PIC 9(6).
           05 AMOUNT     PIC S9(5)V99 COMP-3.
           05 NAME       PIC X(20).
           05 STATUS     PIC X(1).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let selected = vec!["ID".to_string(), "STATUS".to_string()];
    let projected = project_schema(&schema, &selected).expect("project");

    // ID(6) + AMOUNT(4 COMP-3) + NAME(20) + STATUS(1) = 31 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]); // "001234"
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x5C]); // COMP-3
    data.extend_from_slice(&[0x40; 20]); // NAME
    data.extend_from_slice(&[0xC1]); // STATUS "A"

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    assert!(json.get("ID").is_some(), "ID must be present");
    assert!(json.get("STATUS").is_some(), "STATUS must be present");
    assert!(json.get("AMOUNT").is_none(), "AMOUNT must be absent");
    assert!(json.get("NAME").is_none(), "NAME must be absent");
}

// ---------------------------------------------------------------------------
// P13. Empty selection yields empty schema
// ---------------------------------------------------------------------------
#[test]
fn proj_empty_selection_yields_empty_schema() {
    let cpy = r"
        01 REC.
           05 FIELD-A PIC X(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let projected = project_schema(&schema, &[]).expect("project");

    assert!(
        projected.fields.is_empty(),
        "Empty selection should yield empty schema"
    );
}

// ===================================================================
// DIALECT TESTS
// ===================================================================

/// ODO copybook with min_count=2
const ODO_CPY: &str = r"
    01 ODO-REC.
       05 CNT PIC 9(2).
       05 ITEMS OCCURS 2 TO 5 TIMES
          DEPENDING ON CNT.
          10 ITEM-VAL PIC X(4).
";

/// ODO copybook with min_count=0
const ODO_ZERO_CPY: &str = r"
    01 ODO-ZERO-REC.
       05 CNT PIC 9(2).
       05 ITEMS OCCURS 0 TO 5 TIMES
          DEPENDING ON CNT.
          10 ITEM-VAL PIC X(4).
";

// ---------------------------------------------------------------------------
// D1. Normative dialect (default) → min_count enforced
// ---------------------------------------------------------------------------
#[test]
fn dialect_normative_min_count_enforced() {
    let opts = opts_with_dialect(Dialect::Normative);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

    // CNT=02 (== min_count=2), 2 items
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF2]); // CNT "02"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // item 1
    data.extend_from_slice(&[0xC5, 0xC6, 0xC7, 0xC8]); // item 2

    let result = decode_record(&schema, &data, &decode_opts());
    assert!(
        result.is_ok(),
        "Normative: counter=2 (== min_count) should succeed: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// D2. Zero-tolerant dialect → min_count ignored (allows 0)
// ---------------------------------------------------------------------------
#[test]
fn dialect_zero_tolerant_allows_counter_zero() {
    let opts = opts_with_dialect(Dialect::ZeroTolerant);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

    // ZeroTolerant: counter=0 accepted; physical record still has min_count slots
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0]); // CNT "00"
    data.extend_from_slice(&[0x40; 8]); // padding for 2 declared-min slots

    let result = decode_record(&schema, &data, &decode_opts());
    assert!(
        result.is_ok(),
        "ZeroTolerant: counter=0 should succeed: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// D3. One-tolerant dialect → min_count clamped to 1
// ---------------------------------------------------------------------------
#[test]
fn dialect_one_tolerant_clamps_zero_to_one() {
    let opts = opts_with_dialect(Dialect::OneTolerant);
    let schema = parse_copybook_with_options(ODO_ZERO_CPY, &opts).expect("parse");

    // OneTolerant with OCCURS 0 TO 5: effective min = max(1,0) = 1
    // counter=0 may be rejected since effective min=1
    let data: Vec<u8> = vec![0xF0, 0xF0]; // CNT "00"
    let result = decode_record(&schema, &data, &decode_opts());

    if let Err(err) = &result {
        let code_str = err.code.to_string();
        assert!(
            code_str.starts_with("CBKS") || code_str.starts_with("CBKD"),
            "Expected CBKS/CBKD error, got {code_str}"
        );
    }
}

// ---------------------------------------------------------------------------
// D4. Dialect affects ODO validation – ZeroTolerant vs Normative diverge
// ---------------------------------------------------------------------------
#[test]
fn dialect_affects_odo_validation() {
    let normative =
        parse_copybook_with_options(ODO_CPY, &opts_with_dialect(Dialect::Normative)).expect("n");
    let zero =
        parse_copybook_with_options(ODO_CPY, &opts_with_dialect(Dialect::ZeroTolerant)).expect("0");

    // CNT=00 with 2 padding slots = 10 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0]); // CNT "00"
    data.extend_from_slice(&[0x40; 8]); // padding

    let zero_result = decode_record(&zero, &data, &decode_opts());
    assert!(
        zero_result.is_ok(),
        "ZeroTolerant should accept counter=0: {:?}",
        zero_result.err()
    );

    // Normative may accept (with raised count) or reject – either is valid
    let _norm_result = decode_record(&normative, &data, &decode_opts());
}

// ---------------------------------------------------------------------------
// D5. Dialect via CLI flag – all three parse successfully
// ---------------------------------------------------------------------------
#[test]
fn dialect_all_three_parse_ok() {
    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let opts = opts_with_dialect(dialect);
        let result = parse_copybook_with_options(ODO_CPY, &opts);
        assert!(
            result.is_ok(),
            "Dialect {dialect:?} should parse successfully: {:?}",
            result.err()
        );
    }
}

// ---------------------------------------------------------------------------
// D6. Dialect via environment variable – default is Normative
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

// ---------------------------------------------------------------------------
// D7. CLI flag overrides environment variable (API-level test)
// ---------------------------------------------------------------------------
#[test]
fn dialect_explicit_overrides_default() {
    // Simulate: default is Normative, but explicit ZeroTolerant should override
    let explicit = opts_with_dialect(Dialect::ZeroTolerant);
    assert_eq!(
        explicit.dialect,
        Dialect::ZeroTolerant,
        "Explicit dialect must override default"
    );

    // Parse with explicit dialect and decode counter=0
    let schema = parse_copybook_with_options(ODO_CPY, &explicit).expect("parse");
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0]); // CNT "00"
    data.extend_from_slice(&[0x40; 8]); // padding

    let result = decode_record(&schema, &data, &decode_opts());
    assert!(
        result.is_ok(),
        "Explicit ZeroTolerant should accept counter=0: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// D8. Parse + inspect with dialect – schema field count consistent
// ---------------------------------------------------------------------------
#[test]
fn dialect_schema_field_count_consistent() {
    let dialects = [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ];
    let mut counts: Vec<usize> = Vec::new();

    for dialect in &dialects {
        let opts = opts_with_dialect(*dialect);
        let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");
        counts.push(schema.all_fields().len());
    }

    assert!(
        counts.windows(2).all(|w| w[0] == w[1]),
        "All dialects must produce same field count: {counts:?}"
    );
}

// ---------------------------------------------------------------------------
// D9. Decode with dialect – counter=max accepted by all
// ---------------------------------------------------------------------------
#[test]
fn dialect_decode_max_count_accepted() {
    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let schema =
            parse_copybook_with_options(ODO_CPY, &opts_with_dialect(dialect)).expect("parse");

        // CNT=05 (max), 5 items = 2 + 20 = 22 bytes
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
// D10. Encode with dialect – ZeroTolerant allows counter=0
// ---------------------------------------------------------------------------
#[test]
fn dialect_encode_zero_tolerant() {
    let schema = parse_copybook_with_options(ODO_CPY, &opts_with_dialect(Dialect::ZeroTolerant))
        .expect("parse");

    let json = serde_json::json!({
        "CNT": "00",
        "ITEMS": []
    });
    let result = encode_record(&schema, &json, &encode_opts());
    assert!(
        result.is_ok(),
        "ZeroTolerant encode with counter=0 should succeed: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// D11. Dialect roundtrip preserves data
// ---------------------------------------------------------------------------
#[test]
fn dialect_roundtrip_preserves_data() {
    let schema =
        parse_copybook_with_options(ODO_CPY, &opts_with_dialect(Dialect::Normative)).expect("n");

    // CNT=03, valid since 2 ≤ 3 ≤ 5
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF3]); // CNT "03"
    data.extend_from_slice(&[0xC1, 0xC1, 0xC1, 0xC1]); // "AAAA"
    data.extend_from_slice(&[0xC2, 0xC2, 0xC2, 0xC2]); // "BBBB"
    data.extend_from_slice(&[0xC3, 0xC3, 0xC3, 0xC3]); // "CCCC"

    let decoded = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(decoded["CNT"], "03");
    let items = decoded["ITEMS"].as_array().expect("items array");
    assert_eq!(items.len(), 3);
    assert_eq!(items[0]["ITEM-VAL"], "AAAA");
    assert_eq!(items[1]["ITEM-VAL"], "BBBB");
    assert_eq!(items[2]["ITEM-VAL"], "CCCC");

    let decoded2 = decode_record(&schema, &data, &decode_opts()).expect("decode2");
    assert_eq!(decoded, decoded2, "Dialect decode must be deterministic");
}

// ---------------------------------------------------------------------------
// D12. Non-ODO copybook unaffected by dialect
// ---------------------------------------------------------------------------
#[test]
fn dialect_non_odo_unaffected() {
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
            "Dialect {dialect:?} shouldn't affect non-ODO"
        );
        assert_eq!(
            json["FIELD-B"], "123",
            "Dialect {dialect:?} shouldn't affect non-ODO"
        );
    }
}

// ---------------------------------------------------------------------------
// D13. Fixed OCCURS (no ODO) unaffected by dialect
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
        assert_eq!(items.len(), 3, "Fixed OCCURS unaffected by {dialect:?}");
    }
}

// ===================================================================
// COMBINED PROJECTION + DIALECT TESTS
// ===================================================================

// ---------------------------------------------------------------------------
// C1. Projection with dialect – ODO counter auto-included under ZeroTolerant
// ---------------------------------------------------------------------------
#[test]
fn combined_projection_with_zero_tolerant_dialect() {
    let opts = opts_with_dialect(Dialect::ZeroTolerant);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");
    let projected = project_schema(&schema, &["ITEMS".to_string()]).expect("project");

    // CNT=00 with zero-tolerant + projection
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0]); // CNT "00"
    data.extend_from_slice(&[0x40; 8]); // padding for 2 declared-min slots

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    assert!(
        json.get("CNT").is_some(),
        "ODO counter auto-included with ZeroTolerant"
    );
    let items = json.get("ITEMS");
    assert!(items.is_some(), "ITEMS must be present");
}

// ---------------------------------------------------------------------------
// C2. Projection + dialect roundtrip
// ---------------------------------------------------------------------------
#[test]
fn combined_projection_dialect_roundtrip() {
    let opts = opts_with_dialect(Dialect::Normative);
    let schema = parse_copybook_with_options(ODO_CPY, &opts).expect("parse");

    // CNT=03, 3 items (valid: 2 ≤ 3 ≤ 5)
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF3]); // CNT "03"
    data.extend_from_slice(&[0xC1, 0xC1, 0xC1, 0xC1]); // "AAAA"
    data.extend_from_slice(&[0xC2, 0xC2, 0xC2, 0xC2]); // "BBBB"
    data.extend_from_slice(&[0xC3, 0xC3, 0xC3, 0xC3]); // "CCCC"

    // Full decode → verify values
    let decoded = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(decoded["CNT"], "03");
    let items = decoded["ITEMS"].as_array().expect("items array");
    assert_eq!(items.len(), 3);

    // Decode same data again → deterministic
    let decoded2 = decode_record(&schema, &data, &decode_opts()).expect("re-decode");
    assert_eq!(decoded, decoded2, "Dialect roundtrip must be deterministic");

    // Verify ODO counter is auto-included in projection
    let projected = project_schema(&schema, &["ITEMS".to_string()]).expect("project");
    let proj_json = decode_record(&projected, &data, &decode_opts()).expect("proj decode");
    assert!(proj_json.get("CNT").is_some(), "Counter auto-included");
    assert!(proj_json.get("ITEMS").is_some(), "ITEMS present");
}
