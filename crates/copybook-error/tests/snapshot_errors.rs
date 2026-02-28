// SPDX-License-Identifier: AGPL-3.0-or-later
//! Snapshot tests for error display formatting.
//!
//! These tests verify that the Display implementation for Error, ErrorCode,
//! and ErrorContext produces stable, expected output. Any change to error
//! formatting will cause these tests to fail, signaling a potentially
//! breaking change in user-visible diagnostics.

#![allow(clippy::unwrap_used)]

use copybook_error::{Error, ErrorCode, ErrorContext};

// ---------------------------------------------------------------------------
// ErrorCode Display: each family renders its variant name exactly
// ---------------------------------------------------------------------------

#[test]
fn snapshot_parse_error_code_display() {
    // Validates that CBKP family codes render as their variant name
    assert_eq!(
        format!("{}", ErrorCode::CBKP001_SYNTAX),
        "CBKP001_SYNTAX"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKP011_UNSUPPORTED_CLAUSE),
        "CBKP011_UNSUPPORTED_CLAUSE"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKP021_ODO_NOT_TAIL),
        "CBKP021_ODO_NOT_TAIL"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKP101_INVALID_PIC),
        "CBKP101_INVALID_PIC"
    );
}

#[test]
fn snapshot_schema_error_code_display() {
    // Validates that CBKS family codes render as their variant name
    assert_eq!(
        format!("{}", ErrorCode::CBKS121_COUNTER_NOT_FOUND),
        "CBKS121_COUNTER_NOT_FOUND"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKS301_ODO_CLIPPED),
        "CBKS301_ODO_CLIPPED"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKS601_RENAME_UNKNOWN_FROM),
        "CBKS601_RENAME_UNKNOWN_FROM"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND),
        "CBKS703_PROJECTION_FIELD_NOT_FOUND"
    );
}

#[test]
fn snapshot_data_error_code_display() {
    // Validates that CBKD family codes render as their variant name
    assert_eq!(
        format!("{}", ErrorCode::CBKD301_RECORD_TOO_SHORT),
        "CBKD301_RECORD_TOO_SHORT"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKD401_COMP3_INVALID_NIBBLE),
        "CBKD401_COMP3_INVALID_NIBBLE"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKD411_ZONED_BAD_SIGN),
        "CBKD411_ZONED_BAD_SIGN"
    );
}

#[test]
fn snapshot_encode_error_code_display() {
    // Validates that CBKE family codes render as their variant name
    assert_eq!(
        format!("{}", ErrorCode::CBKE501_JSON_TYPE_MISMATCH),
        "CBKE501_JSON_TYPE_MISMATCH"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKE510_NUMERIC_OVERFLOW),
        "CBKE510_NUMERIC_OVERFLOW"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKE515_STRING_LENGTH_VIOLATION),
        "CBKE515_STRING_LENGTH_VIOLATION"
    );
}

#[test]
fn snapshot_file_error_code_display() {
    // Validates that CBKF and CBKR family codes render correctly
    assert_eq!(
        format!("{}", ErrorCode::CBKF102_RECORD_LENGTH_INVALID),
        "CBKF102_RECORD_LENGTH_INVALID"
    );
    assert_eq!(
        format!("{}", ErrorCode::CBKR211_RDW_RESERVED_NONZERO),
        "CBKR211_RDW_RESERVED_NONZERO"
    );
}

// ---------------------------------------------------------------------------
// Error Display: "CODE: message" format without context
// ---------------------------------------------------------------------------

#[test]
fn snapshot_error_display_without_context() {
    // Validates the base format: "CODE: message"
    let error = Error::new(
        ErrorCode::CBKP001_SYNTAX,
        "Unexpected token at line 5",
    );
    assert_eq!(
        format!("{error}"),
        "CBKP001_SYNTAX: Unexpected token at line 5"
    );
}

#[test]
fn snapshot_error_display_with_field_context() {
    // Validates format: "CODE: message (field PATH)"
    let error = Error::new(
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        "Invalid sign zone in zoned decimal field",
    )
    .with_field("CUSTOMER-RECORD.BALANCE");

    assert_eq!(
        format!("{error}"),
        "CBKD411_ZONED_BAD_SIGN: Invalid sign zone in zoned decimal field (field CUSTOMER-RECORD.BALANCE)"
    );
}

#[test]
fn snapshot_error_display_with_record_context() {
    // Validates format: "CODE: message (record N)"
    let error = Error::new(
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Record data too short for field requirements",
    )
    .with_record(42);

    assert_eq!(
        format!("{error}"),
        "CBKD301_RECORD_TOO_SHORT: Record data too short for field requirements (record 42)"
    );
}

#[test]
fn snapshot_error_display_with_full_context() {
    // Validates format with all context parts: record, field, offset, details
    let ctx = ErrorContext {
        record_index: Some(100),
        field_path: Some("ORDER.LINE-ITEMS.AMOUNT".to_string()),
        byte_offset: Some(256),
        line_number: None,
        details: Some("expected 5 bytes, got 3".to_string()),
    };
    let error = Error::new(
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Truncated record",
    )
    .with_context(ctx);

    assert_eq!(
        format!("{error}"),
        "CBKD301_RECORD_TOO_SHORT: Truncated record (record 100, field ORDER.LINE-ITEMS.AMOUNT, offset 256, expected 5 bytes, got 3)"
    );
}

// ---------------------------------------------------------------------------
// ErrorContext Display: standalone context formatting
// ---------------------------------------------------------------------------

#[test]
fn snapshot_error_context_display_all_fields() {
    let ctx = ErrorContext {
        record_index: Some(7),
        field_path: Some("ROOT.CHILD".to_string()),
        byte_offset: Some(32),
        line_number: Some(15),
        details: Some("extra info".to_string()),
    };
    assert_eq!(
        format!("{ctx}"),
        "record 7, field ROOT.CHILD, offset 32, line 15, extra info"
    );
}

#[test]
fn snapshot_error_context_display_partial() {
    let ctx = ErrorContext {
        record_index: None,
        field_path: Some("AMOUNT".to_string()),
        byte_offset: None,
        line_number: Some(3),
        details: None,
    };
    assert_eq!(format!("{ctx}"), "field AMOUNT, line 3");
}

// ---------------------------------------------------------------------------
// ErrorCode family_prefix: stable prefix extraction
// ---------------------------------------------------------------------------

#[test]
fn snapshot_family_prefix_mapping() {
    // Each family prefix is a stable 4-char string used for programmatic grouping
    assert_eq!(ErrorCode::CBKP001_SYNTAX.family_prefix(), "CBKP");
    assert_eq!(ErrorCode::CBKS121_COUNTER_NOT_FOUND.family_prefix(), "CBKS");
    assert_eq!(ErrorCode::CBKR211_RDW_RESERVED_NONZERO.family_prefix(), "CBKR");
    assert_eq!(ErrorCode::CBKC201_JSON_WRITE_ERROR.family_prefix(), "CBKC");
    assert_eq!(ErrorCode::CBKD301_RECORD_TOO_SHORT.family_prefix(), "CBKD");
    assert_eq!(ErrorCode::CBKI001_INVALID_STATE.family_prefix(), "CBKI");
    assert_eq!(ErrorCode::CBKE501_JSON_TYPE_MISMATCH.family_prefix(), "CBKE");
    assert_eq!(ErrorCode::CBKF102_RECORD_LENGTH_INVALID.family_prefix(), "CBKF");
    assert_eq!(ErrorCode::CBKA001_BASELINE_ERROR.family_prefix(), "CBKA");
    assert_eq!(ErrorCode::CBKW001_SCHEMA_CONVERSION.family_prefix(), "CBKW");
}

// ---------------------------------------------------------------------------
// ErrorCode serde round-trip: serialization format is stable
// ---------------------------------------------------------------------------

#[test]
fn snapshot_error_code_json_serialization() {
    // Error codes serialize as quoted variant names for API stability
    let code = ErrorCode::CBKD411_ZONED_BAD_SIGN;
    let json = serde_json::to_string(&code).unwrap();
    assert_eq!(json, r#""CBKD411_ZONED_BAD_SIGN""#);

    let code2 = ErrorCode::CBKE521_ARRAY_LEN_OOB;
    let json2 = serde_json::to_string(&code2).unwrap();
    assert_eq!(json2, r#""CBKE521_ARRAY_LEN_OOB""#);
}
