// SPDX-License-Identifier: AGPL-3.0-or-later
//! End-to-end field projection tests.
//!
//! Validates that `project_schema` correctly narrows the schema to selected
//! fields, auto-includes ODO counter dependencies, and rejects unknown names.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{decode_record, Codepage, DecodeOptions, JsonNumberMode, RecordFormat};
use copybook_core::{parse_copybook, project_schema, ErrorCode};

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

// ---------------------------------------------------------------------------
// 1. Select a subset of fields – only those appear in output
// ---------------------------------------------------------------------------

#[test]
fn projection_select_subset() {
    let cpy = r"
        01 PROJ-REC.
           05 FIELD-A  PIC X(5).
           05 FIELD-B  PIC 9(4).
           05 FIELD-C  PIC X(10).
           05 FIELD-D  PIC 9(6).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let selected = vec!["FIELD-A".to_string(), "FIELD-C".to_string()];
    let projected = project_schema(&schema, &selected).expect("project");

    // Build a full record (5 + 4 + 10 + 6 = 25 bytes)
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6]); // "HELLO"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4]); // "1234"
    data.extend_from_slice(&[0xE6, 0xD6, 0xD9, 0xD3, 0xC4, 0x40, 0x40, 0x40, 0x40, 0x40]); // "WORLD     "
    data.extend_from_slice(&[0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0]); // "567890"

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    // Selected fields must be present
    assert!(json.get("FIELD-A").is_some(), "FIELD-A must be in output");
    assert!(json.get("FIELD-C").is_some(), "FIELD-C must be in output");
    // Non-selected fields must be absent
    assert!(json.get("FIELD-B").is_none(), "FIELD-B must NOT be in output");
    assert!(json.get("FIELD-D").is_none(), "FIELD-D must NOT be in output");
}

// ---------------------------------------------------------------------------
// 2. ODO counter auto-inclusion
// ---------------------------------------------------------------------------

#[test]
fn projection_odo_counter_auto_included() {
    let cpy = r"
        01 ODO-REC.
           05 HEADER       PIC X(5).
           05 ITEM-COUNT   PIC 9(3).
           05 ITEMS OCCURS 1 TO 10 TIMES
              DEPENDING ON ITEM-COUNT.
              10 ITEM-VAL  PIC X(4).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    // Select only the ODO array – counter should be auto-included
    let selected = vec!["ITEMS".to_string()];
    let projected = project_schema(&schema, &selected).expect("project");

    // Build record: HEADER(5) + ITEM-COUNT(3) + 2 items × 4 bytes = 16 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0x40; 5]); // HEADER (spaces)
    data.extend_from_slice(&[0xF0, 0xF0, 0xF2]); // ITEM-COUNT "002"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // item 1 "ABCD"
    data.extend_from_slice(&[0xC5, 0xC6, 0xC7, 0xC8]); // item 2 "EFGH"

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    // ODO counter must be auto-included for correct decoding
    assert!(
        json.get("ITEM-COUNT").is_some(),
        "ODO counter ITEM-COUNT must be auto-included"
    );
    // The array must be present
    assert!(json.get("ITEMS").is_some(), "ITEMS array must be present");
}

// ---------------------------------------------------------------------------
// 3. Error on unknown field name (CBKS703)
// ---------------------------------------------------------------------------

#[test]
fn projection_unknown_field_returns_cbks703() {
    let cpy = r"
        01 SIMPLE-REC.
           05 REAL-FIELD  PIC X(10).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let selected = vec!["NONEXISTENT-FIELD".to_string()];
    let result = project_schema(&schema, &selected);

    assert!(result.is_err(), "Selecting unknown field must fail");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
        "Error code must be CBKS703"
    );
}

// ---------------------------------------------------------------------------
// 4. Selecting a group includes its children
// ---------------------------------------------------------------------------

#[test]
fn projection_group_includes_children() {
    let cpy = r"
        01 GRP-REC.
           05 SEC-A.
              10 CHILD-1  PIC X(5).
              10 CHILD-2  PIC 9(3).
           05 SEC-B       PIC X(8).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let selected = vec!["SEC-A".to_string()];
    let projected = project_schema(&schema, &selected).expect("project");

    // Build record: CHILD-1(5) + CHILD-2(3) + SEC-B(8) = 16 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4, 0xC5]); // "ABCDE"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3]); // "123"
    data.extend_from_slice(&[0x40; 8]); // SEC-B spaces

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    // When a group is selected, its children are included in the output.
    // The decoder may flatten the group or preserve hierarchy.
    let child1 = json.get("CHILD-1").or_else(|| {
        json.get("SEC-A")
            .and_then(|g| g.get("CHILD-1"))
    });
    let child2 = json.get("CHILD-2").or_else(|| {
        json.get("SEC-A")
            .and_then(|g| g.get("CHILD-2"))
    });

    assert!(child1.is_some(), "CHILD-1 must be present in projected output");
    assert!(child2.is_some(), "CHILD-2 must be present in projected output");

    // Non-selected group must be absent
    assert!(json.get("SEC-B").is_none(), "SEC-B must NOT be in output");
}

// ---------------------------------------------------------------------------
// 5. Multiple --select fields with mixed levels
// ---------------------------------------------------------------------------

#[test]
fn projection_multiple_fields_mixed() {
    let cpy = r"
        01 MIX-REC.
           05 ID-FIELD    PIC 9(6).
           05 NAME-FIELD  PIC X(20).
           05 AMOUNT      PIC S9(5)V99 COMP-3.
           05 STATUS      PIC X(1).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let selected = vec!["ID-FIELD".to_string(), "STATUS".to_string()];
    let projected = project_schema(&schema, &selected).expect("project");

    // Build a full record: 6 + 20 + 4 + 1 = 31 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]); // "001234"
    data.extend_from_slice(&[0x40; 20]); // NAME-FIELD spaces
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x5C]); // AMOUNT COMP-3
    data.extend_from_slice(&[0xC1]); // STATUS "A"

    let json = decode_record(&projected, &data, &decode_opts()).expect("decode");

    assert!(json.get("ID-FIELD").is_some(), "ID-FIELD must be present");
    assert!(json.get("STATUS").is_some(), "STATUS must be present");
    assert!(
        json.get("NAME-FIELD").is_none(),
        "NAME-FIELD must NOT be present"
    );
    assert!(
        json.get("AMOUNT").is_none(),
        "AMOUNT must NOT be present"
    );
}
