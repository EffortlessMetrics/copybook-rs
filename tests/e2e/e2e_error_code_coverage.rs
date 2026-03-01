// SPDX-License-Identifier: AGPL-3.0-or-later
//! End-to-end error code coverage tests for issue #194.
//!
//! Tests 9 specific error codes that were previously untested at the e2e level.
//! Each test triggers the error condition through the public API and verifies
//! the error code and message context.
//!
//! **Overlap note**: Some codes already have *loose* coverage in
//! `e2e_error_handling.rs` or `e2e_field_projection.rs`. Tests here assert the
//! *exact* `ErrorCode` variant so every code has at least one precise test.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::record::{FixedRecordReader, RDWRecordReader};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::schema::{Field, FieldKind, Occurs, Schema};
use copybook_core::{ErrorCode, parse_copybook, project_schema};
use serde_json::json;
use std::io::Cursor;

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

// ===========================================================================
// 1. CBKS701 – Projection invalid ODO
//    Select an ODO array whose counter field does not exist in the schema.
// ===========================================================================

#[test]
fn error_cbks701_projection_invalid_odo() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut odo_array = Field::new(5, "ITEMS".to_string());
    odo_array.path = "ROOT.ITEMS".to_string();
    odo_array.kind = FieldKind::Group;
    odo_array.occurs = Some(Occurs::ODO {
        min: 1,
        max: 10,
        counter_path: "MISSING-COUNTER".to_string(),
    });

    let mut child = Field::new(10, "ITEM-VAL".to_string());
    child.path = "ROOT.ITEMS.ITEM-VAL".to_string();
    child.kind = FieldKind::Alphanum { len: 4 };
    child.len = 4;

    odo_array.children = vec![child];
    root.children = vec![odo_array];

    let schema = Schema::from_fields(vec![root]);

    let result = project_schema(&schema, &["ITEMS".to_string()]);

    assert!(result.is_err(), "Expected CBKS701 for missing ODO counter");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
        "Expected CBKS701_PROJECTION_INVALID_ODO, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("ITEMS"),
        "Error should mention the ODO array field: {}",
        err.message
    );
}

// ===========================================================================
// 2. CBKS702 – Projection unresolved alias
//    Select a level-66 RENAMES alias that has no resolved_renames metadata.
// ===========================================================================

#[test]
fn error_cbks702_projection_unresolved_alias() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field1 = Field::new(5, "FIELD1".to_string());
    field1.path = "ROOT.FIELD1".to_string();
    field1.kind = FieldKind::Alphanum { len: 10 };
    field1.len = 10;

    // Level-66 RENAMES alias *without* resolved_renames
    let mut alias = Field::new(66, "MY-ALIAS".to_string());
    alias.path = "ROOT.MY-ALIAS".to_string();
    alias.level = 66;
    alias.kind = FieldKind::Renames {
        from_field: "FIELD1".to_string(),
        thru_field: "FIELD1".to_string(),
    };
    alias.resolved_renames = None;

    root.children = vec![field1, alias];

    let schema = Schema::from_fields(vec![root]);

    let result = project_schema(&schema, &["MY-ALIAS".to_string()]);

    assert!(result.is_err(), "Expected CBKS702 for unresolved alias");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS,
        "Expected CBKS702_PROJECTION_UNRESOLVED_ALIAS, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("MY-ALIAS"),
        "Error should mention the alias name: {}",
        err.message
    );
}

// ===========================================================================
// 3. CBKS703 – Projection field not found
//    Select a field name that does not exist in the schema.
// ===========================================================================

#[test]
fn error_cbks703_projection_field_not_found() {
    let cpy = r"
        01 SIMPLE-REC.
           05 REAL-FIELD  PIC X(10).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let result = project_schema(&schema, &["NONEXISTENT".to_string()]);

    assert!(result.is_err(), "Expected CBKS703 for missing field");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
        "Expected CBKS703_PROJECTION_FIELD_NOT_FOUND, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("NONEXISTENT"),
        "Error should mention the missing field name: {}",
        err.message
    );
}

// ===========================================================================
// 4. CBKD101 – Data validation error (invalid field type)
//    Decode a synthetic RENAMES field that has no resolved_renames metadata.
// ===========================================================================

#[test]
fn error_cbkd101_invalid_field_type() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field1 = Field::new(5, "FIELD1".to_string());
    field1.path = "ROOT.FIELD1".to_string();
    field1.kind = FieldKind::Alphanum { len: 10 };
    field1.len = 10;
    field1.offset = 0;

    // RENAMES without resolved metadata → triggers CBKD101 during decode
    let mut renames = Field::new(66, "ALIAS".to_string());
    renames.path = "ROOT.ALIAS".to_string();
    renames.level = 66;
    renames.kind = FieldKind::Renames {
        from_field: "FIELD1".to_string(),
        thru_field: "FIELD1".to_string(),
    };
    renames.resolved_renames = None;
    renames.offset = 0;
    renames.len = 10;

    root.children = vec![field1, renames];

    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(10);

    let data = b"ABCDEFGHIJ";
    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    let result = decode_record(&schema, data, &options);

    assert!(result.is_err(), "Expected CBKD101 for unresolved RENAMES");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKD101_INVALID_FIELD_TYPE,
        "Expected CBKD101_INVALID_FIELD_TYPE, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("ALIAS") || err.message.contains("RENAMES"),
        "Error should mention the field: {}",
        err.message
    );
}

// ===========================================================================
// 5. CBKE510 – Encoding bounds error (numeric overflow)
//    Encode a value that exceeds the PIC digit capacity.
// ===========================================================================

#[test]
fn error_cbke510_numeric_overflow() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field = Field::new(5, "SMALL-NUM".to_string());
    field.path = "ROOT.SMALL-NUM".to_string();
    field.kind = FieldKind::ZonedDecimal {
        digits: 3,
        scale: 0,
        signed: false,
        sign_separate: None,
    };
    field.len = 3;
    field.offset = 0;

    root.children = vec![field];

    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(3);

    // 5-digit value in a 3-digit field
    let json_value = json!({
        "ROOT": {
            "SMALL-NUM": "99999"
        }
    });

    let result = encode_record(&schema, &json_value, &encode_opts());

    assert!(result.is_err(), "Expected CBKE510 for numeric overflow");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKE510_NUMERIC_OVERFLOW,
        "Expected CBKE510_NUMERIC_OVERFLOW, got {:?}",
        err.code
    );
}

// ===========================================================================
// 6. CBKE515 – Encoding string length violation
//    Encode a string longer than the PIC X capacity.
// ===========================================================================

#[test]
fn error_cbke515_string_length_violation() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field = Field::new(5, "SHORT-FIELD".to_string());
    field.path = "ROOT.SHORT-FIELD".to_string();
    field.kind = FieldKind::Alphanum { len: 5 };
    field.len = 5;
    field.offset = 0;

    root.children = vec![field];

    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(5);

    let json_value = json!({
        "ROOT": {
            "SHORT-FIELD": "THIS STRING IS WAY TOO LONG FOR FIVE BYTES"
        }
    });

    let result = encode_record(&schema, &json_value, &encode_opts());

    assert!(result.is_err(), "Expected CBKE515 for string too long");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
        "Expected CBKE515_STRING_LENGTH_VIOLATION, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("String length") || err.message.contains("exceeds"),
        "Error should mention length violation: {}",
        err.message
    );
}

// ===========================================================================
// 7. CBKF102 – Format validation error (RDW record length invalid)
//    Feed an RDW stream whose header length exceeds available payload bytes.
// ===========================================================================

#[test]
fn error_cbkf102_record_length_invalid() {
    // RDW header: length = 100 (big-endian), reserved = 0x0000
    // But only provide 2 bytes of payload instead of 100
    let data: Vec<u8> = vec![
        0x00, 0x64, // length = 100
        0x00, 0x00, // reserved
        0xAA, 0xBB, // only 2 payload bytes (need 100)
    ];

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let result = reader.read_record();

    assert!(result.is_err(), "Expected CBKF102 for incomplete payload");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
        "Expected CBKF102_RECORD_LENGTH_INVALID, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("Incomplete") || err.message.contains("expected"),
        "Error should mention incomplete record: {}",
        err.message
    );
}

// ===========================================================================
// 8. CBKF104 – Format constraint error (RDW suspect ASCII corruption)
//    Feed an RDW stream whose first two header bytes are ASCII digits,
//    indicating the file was likely transferred in text/ASCII mode.
// ===========================================================================

#[test]
fn error_cbkf104_rdw_suspect_ascii() {
    // ASCII digits '1' and '2' in first two bytes → suspect ASCII corruption
    let data: Vec<u8> = vec![
        b'1', b'2', // ASCII digits → CBKF104 trigger
        0x00, 0x00, // reserved
        0x41, 0x42, 0x43, 0x44, // some payload
    ];

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let result = reader.read_record();

    assert!(
        result.is_err(),
        "Expected CBKF104 for ASCII-corrupted RDW header"
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
        "Expected CBKF104_RDW_SUSPECT_ASCII, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("ASCII") || err.message.contains("corrupted"),
        "Error should mention ASCII corruption: {}",
        err.message
    );
}

// ===========================================================================
// 9. CBKI001 – Internal error (invalid state)
//    Create a FixedRecordReader without LRECL (None) which is a
//    prerequisite violation.
// ===========================================================================

#[test]
fn error_cbki001_invalid_state() {
    let data = Cursor::new(b"some test data");

    let result = FixedRecordReader::new(data, None);

    assert!(result.is_err(), "Expected CBKI001 for missing LRECL");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKI001_INVALID_STATE,
        "Expected CBKI001_INVALID_STATE, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("LRECL"),
        "Error should mention LRECL requirement: {}",
        err.message
    );
}

// ===========================================================================
// 9b. CBKI001 – Internal error (zero LRECL)
//    Create a FixedRecordReader with LRECL=0.
// ===========================================================================

#[test]
fn error_cbki001_invalid_state_zero_lrecl() {
    let data = Cursor::new(b"some test data");

    let result = FixedRecordReader::new(data, Some(0));

    assert!(result.is_err(), "Expected CBKI001 for zero LRECL");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKI001_INVALID_STATE,
        "Expected CBKI001_INVALID_STATE, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("greater than zero"),
        "Error should mention LRECL must be > 0: {}",
        err.message
    );
}
