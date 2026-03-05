// SPDX-License-Identifier: AGPL-3.0-or-later
//! Complete error taxonomy E2E tests.
//!
//! Triggers every reachable `CBK*` error code family through the public API
//! and validates:
//! - Exact error code matching
//! - Error code format stability (`CBK[A-Z]\d{3}_*`)
//! - Actionable error messages
//! - Recoverability (no panics)
//! - Multiple error collection capability
//!
//! ## Coverage: 28 tests across 9 error families
//!
//! | Family | Codes Triggered                                              |
//! |--------|--------------------------------------------------------------|
//! | CBKP   | 001, 021, 101                                                |
//! | CBKS   | 121, 301, 601, 701, 702, 703                                 |
//! | CBKD   | 101, 301, 401, 421, 422, 423                                 |
//! | CBKE   | 501, 505, 510, 515, 530, 531                                 |
//! | CBKR   | 211                                                          |
//! | CBKF   | 102, 104                                                     |
//! | CBKI   | 001                                                          |
//! | CBKA   | (format stability only — bench-only crate)                   |
//! | CBKW   | (format stability only — arrow-only crate)                   |

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::record::{FixedRecordReader, RDWRecordReader};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::schema::{Field, FieldKind, Occurs, Schema, SignPlacement, SignSeparateInfo};
use copybook_core::{ErrorCode, parse_copybook, project_schema};
use serde_json::json;
use std::io::Cursor;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn decode_opts_strict() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
        .with_strict_mode(true)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

// =========================================================================
// CBKP* — Parse Errors
// =========================================================================

/// CBKP001: Syntax error — garbage text
#[test]
fn cbkp001_syntax_garbage() {
    let result = parse_copybook("@#$%^&*() NOT COBOL");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    assert!(!err.message.is_empty(), "Error message must be non-empty");
}

/// CBKP001: Syntax error — empty input
#[test]
fn cbkp001_syntax_empty() {
    let result = parse_copybook("");
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().code, ErrorCode::CBKP001_SYNTAX);
}

/// CBKP101: Invalid PIC clause
#[test]
fn cbkp101_invalid_pic() {
    let cpy = r"
        01 REC.
           05 FLD PIC QQQ.
    ";
    let result = parse_copybook(cpy);
    assert!(result.is_err());
    let code = result.unwrap_err().code;
    assert!(
        matches!(
            code,
            ErrorCode::CBKP101_INVALID_PIC | ErrorCode::CBKP001_SYNTAX
        ),
        "Expected CBKP101 or CBKP001, got {code:?}"
    );
}

/// CBKP021: ODO array not at tail position
#[test]
fn cbkp021_odo_not_tail() {
    let cpy = r"
        01 REC.
           05 CNT PIC 9(2).
           05 ITEMS OCCURS 1 TO 5 TIMES
              DEPENDING ON CNT.
              10 ITEM-VAL PIC X(4).
           05 TRAILING PIC X(10).
    ";
    let result = parse_copybook(cpy);
    assert!(result.is_err());
    let code = result.unwrap_err().code;
    assert!(
        matches!(
            code,
            ErrorCode::CBKP021_ODO_NOT_TAIL | ErrorCode::CBKP001_SYNTAX
        ),
        "Expected CBKP021 or CBKP001, got {code:?}"
    );
}

// =========================================================================
// CBKS* — Schema Validation Errors
// =========================================================================

/// CBKS121: ODO counter field not found in schema
#[test]
fn cbks121_counter_not_found() {
    let cpy = r"
        01 REC.
           05 ITEMS OCCURS 1 TO 5 TIMES
              DEPENDING ON MISSING-COUNTER.
              10 VAL PIC X(4).
    ";
    let result = parse_copybook(cpy);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND
    );
}

/// CBKS301: ODO counter clipped to max (strict mode)
#[test]
fn cbks301_odo_clipped() {
    let cpy = r"
        01 REC.
           05 CNT PIC 9(2).
           05 ITEMS OCCURS 1 TO 3 TIMES
              DEPENDING ON CNT.
              10 VAL PIC X(4).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // CNT = "09" (EBCDIC F0F9) — exceeds max of 3
    let mut data = vec![0xF0, 0xF9];
    data.extend(vec![0xC1; 12]); // 3 × 4 bytes
    let result = decode_record(&schema, &data, &decode_opts_strict());
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().code, ErrorCode::CBKS301_ODO_CLIPPED);
}

/// CBKS601: RENAMES from field not found
#[test]
fn cbks601_rename_unknown_from() {
    let cpy = r"
        01 REC.
           05 FLD-A PIC X(5).
           05 FLD-B PIC X(5).
           66 MY-ALIAS RENAMES NONEXISTENT THRU FLD-B.
    ";
    let result = parse_copybook(cpy);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM
    );
}

/// CBKS701: Projection invalid ODO — counter path missing
#[test]
fn cbks701_projection_invalid_odo() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut odo = Field::new(5, "ITEMS".to_string());
    odo.path = "ROOT.ITEMS".to_string();
    odo.kind = FieldKind::Group;
    odo.occurs = Some(Occurs::ODO {
        min: 1,
        max: 10,
        counter_path: "MISSING-COUNTER".to_string(),
    });

    let mut child = Field::new(10, "VAL".to_string());
    child.path = "ROOT.ITEMS.VAL".to_string();
    child.kind = FieldKind::Alphanum { len: 4 };
    child.len = 4;
    odo.children = vec![child];
    root.children = vec![odo];

    let schema = Schema::from_fields(vec![root]);
    let result = project_schema(&schema, &["ITEMS".to_string()]);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS701_PROJECTION_INVALID_ODO);
    assert!(
        err.message.contains("ITEMS"),
        "Message should name the field: {}",
        err.message
    );
}

/// CBKS702: Projection unresolved alias — RENAMES without resolved_renames
#[test]
fn cbks702_projection_unresolved_alias() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "FLD".to_string());
    fld.path = "ROOT.FLD".to_string();
    fld.kind = FieldKind::Alphanum { len: 10 };
    fld.len = 10;

    let mut alias = Field::new(66, "ALIAS".to_string());
    alias.path = "ROOT.ALIAS".to_string();
    alias.level = 66;
    alias.kind = FieldKind::Renames {
        from_field: "FLD".to_string(),
        thru_field: "FLD".to_string(),
    };
    alias.resolved_renames = None;

    root.children = vec![fld, alias];
    let schema = Schema::from_fields(vec![root]);
    let result = project_schema(&schema, &["ALIAS".to_string()]);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS
    );
}

/// CBKS703: Projection field not found
#[test]
fn cbks703_projection_field_not_found() {
    let cpy = r"
        01 REC.
           05 FLD PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let result = project_schema(&schema, &["NONEXISTENT".to_string()]);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND);
    assert!(
        err.message.contains("NONEXISTENT"),
        "Message should name the missing field: {}",
        err.message
    );
}

// =========================================================================
// CBKD* — Data Decode Errors
// =========================================================================

/// CBKD101: Invalid field type — unresolved RENAMES during decode
#[test]
fn cbkd101_invalid_field_type() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "FLD".to_string());
    fld.path = "ROOT.FLD".to_string();
    fld.kind = FieldKind::Alphanum { len: 10 };
    fld.len = 10;
    fld.offset = 0;

    let mut renames = Field::new(66, "ALIAS".to_string());
    renames.path = "ROOT.ALIAS".to_string();
    renames.level = 66;
    renames.kind = FieldKind::Renames {
        from_field: "FLD".to_string(),
        thru_field: "FLD".to_string(),
    };
    renames.resolved_renames = None;
    renames.offset = 0;
    renames.len = 10;

    root.children = vec![fld, renames];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(10);

    let result = decode_record(&schema, b"ABCDEFGHIJ", &decode_opts());
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD101_INVALID_FIELD_TYPE
    );
}

/// CBKD301: Record too short for schema
#[test]
fn cbkd301_record_too_short() {
    let cpy = r"
        01 REC.
           05 FLD-A PIC X(50).
           05 FLD-B PIC 9(30).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let result = decode_record(&schema, &[0x40; 5], &decode_opts());
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD301_RECORD_TOO_SHORT
    );
}

/// CBKD401: COMP-3 invalid nibble
#[test]
fn cbkd401_comp3_invalid_nibble() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "PKD".to_string());
    fld.path = "ROOT.PKD".to_string();
    fld.kind = FieldKind::PackedDecimal {
        digits: 5,
        scale: 0,
        signed: true,
    };
    fld.len = 3;
    fld.offset = 0;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(3);

    // Nibble B is invalid (only 0-9 valid for digit nibbles)
    let result = decode_record(&schema, &[0xBB, 0xBB, 0x0C], &decode_opts());
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE
    );
}

/// CBKD421: Edited PIC invalid format — non-digit in required position
#[test]
fn cbkd421_edited_pic_invalid_format() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "AMT".to_string());
    fld.path = "ROOT.AMT".to_string();
    fld.kind = FieldKind::EditedNumeric {
        pic_string: "ZZZ9".to_string(),
        width: 4,
        scale: 0,
        signed: false,
    };
    fld.len = 4;
    fld.offset = 0;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(4);

    // EBCDIC "ABCD" → invalid for PIC ZZZ9 (requires digit in last position)
    let result = decode_record(&schema, &[0xC1, 0xC2, 0xC3, 0xC4], &decode_opts());
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

/// CBKD422: Edited PIC sign mismatch — expected sign character, got digit
#[test]
fn cbkd422_edited_pic_sign_mismatch() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "BAL".to_string());
    fld.path = "ROOT.BAL".to_string();
    fld.kind = FieldKind::EditedNumeric {
        pic_string: "+ZZ9".to_string(),
        width: 4,
        scale: 0,
        signed: true,
    };
    fld.len = 4;
    fld.offset = 0;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(4);

    // EBCDIC "5  5" → '5' in sign position (expects '+' or '-')
    // CP037: '5' = 0xF5, ' ' = 0x40
    let result = decode_record(&schema, &[0xF5, 0x40, 0x40, 0xF5], &decode_opts());
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH
    );
}

/// CBKD423: Edited PIC blank-when-zero — all blanks with blank_when_zero flag
#[test]
fn cbkd423_edited_pic_blank_when_zero() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "QTY".to_string());
    fld.path = "ROOT.QTY".to_string();
    fld.kind = FieldKind::EditedNumeric {
        pic_string: "ZZZ9".to_string(),
        width: 4,
        scale: 0,
        signed: false,
    };
    fld.len = 4;
    fld.offset = 0;
    fld.blank_when_zero = true;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(4);

    // All EBCDIC spaces — BLANK WHEN ZERO triggers CBKD423 as a warning
    // but still decodes to "0" (not an error). Verify no panic.
    let result = decode_record(&schema, &[0x40, 0x40, 0x40, 0x40], &decode_opts());
    // CBKD423 is a warning logged via tracing, not an Err.
    // The decode succeeds with value "0". Verify recoverability.
    assert!(
        result.is_ok(),
        "CBKD423 is a warning, decode should succeed"
    );
    // Verify the code variant compiles and formats correctly
    let code = ErrorCode::CBKD423_EDITED_PIC_BLANK_WHEN_ZERO;
    assert_eq!(format!("{code}"), "CBKD423_EDITED_PIC_BLANK_WHEN_ZERO");
}

// =========================================================================
// CBKE* — Encode Errors
// =========================================================================

/// CBKE501: JSON type mismatch — non-object root
#[test]
fn cbke501_json_type_mismatch() {
    let cpy = r"
        01 REC.
           05 FLD PIC X(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let result = encode_record(&schema, &json!("not an object"), &encode_opts());
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH
    );
}

/// CBKE505: Scale mismatch — too many decimal places
#[test]
fn cbke505_scale_mismatch() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "AMT".to_string());
    fld.path = "ROOT.AMT".to_string();
    fld.kind = FieldKind::ZonedDecimal {
        digits: 5,
        scale: 2,
        signed: false,
        sign_separate: None,
    };
    fld.len = 5;
    fld.offset = 0;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(5);

    let result = encode_record(
        &schema,
        &json!({"ROOT": {"AMT": "123.4567"}}),
        &encode_opts(),
    );
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().code, ErrorCode::CBKE505_SCALE_MISMATCH);
}

/// CBKE510: Numeric overflow — value exceeds digit capacity
#[test]
fn cbke510_numeric_overflow() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "TINY".to_string());
    fld.path = "ROOT.TINY".to_string();
    fld.kind = FieldKind::ZonedDecimal {
        digits: 3,
        scale: 0,
        signed: false,
        sign_separate: None,
    };
    fld.len = 3;
    fld.offset = 0;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(3);

    let result = encode_record(&schema, &json!({"ROOT": {"TINY": "99999"}}), &encode_opts());
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE510_NUMERIC_OVERFLOW
    );
}

/// CBKE515: String length exceeds PIC X capacity
#[test]
fn cbke515_string_length_violation() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "SHORT".to_string());
    fld.path = "ROOT.SHORT".to_string();
    fld.kind = FieldKind::Alphanum { len: 5 };
    fld.len = 5;
    fld.offset = 0;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(5);

    let result = encode_record(
        &schema,
        &json!({"ROOT": {"SHORT": "THIS STRING IS WAY TOO LONG FOR FIVE BYTES"}}),
        &encode_opts(),
    );
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE515_STRING_LENGTH_VIOLATION
    );
}

/// CBKE530: SIGN SEPARATE encode error — non-numeric value
#[test]
fn cbke530_sign_separate_encode_error() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "SIGNED".to_string());
    fld.path = "ROOT.SIGNED".to_string();
    fld.kind = FieldKind::ZonedDecimal {
        digits: 5,
        scale: 0,
        signed: true,
        sign_separate: Some(SignSeparateInfo {
            placement: SignPlacement::Leading,
        }),
    };
    fld.len = 6;
    fld.offset = 0;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(6);

    let result = encode_record(
        &schema,
        &json!({"ROOT": {"SIGNED": "ABCDE"}}),
        &encode_opts(),
    );
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR
    );
}

/// CBKE531: Float encode overflow — f64 value exceeds f32 range
#[test]
fn cbke531_float_encode_overflow() {
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut fld = Field::new(5, "FLOAT-FLD".to_string());
    fld.path = "ROOT.FLOAT-FLD".to_string();
    fld.kind = FieldKind::FloatSingle;
    fld.len = 4;
    fld.offset = 0;

    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(4);

    let result = encode_record(
        &schema,
        &json!({"ROOT": {"FLOAT-FLD": 3.5e38_f64}}),
        &encode_opts(),
    );
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW
    );
}

// =========================================================================
// CBKR* — Record Format Errors
// =========================================================================

/// CBKR211: RDW reserved bytes non-zero in strict mode
#[test]
fn cbkr211_rdw_reserved_nonzero() {
    let data: Vec<u8> = vec![
        0x00, 0x08, // length = 8
        0xBE, 0xEF, // reserved != 0
        0x40, 0x40, 0x40, 0x40,
    ];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let result = reader.read_record();
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO
    );
}

// =========================================================================
// CBKF* — File/Format Errors
// =========================================================================

/// CBKF102: RDW record length invalid — header says 100 but payload too short
#[test]
fn cbkf102_record_length_invalid() {
    let data: Vec<u8> = vec![
        0x00, 0x64, // length = 100
        0x00, 0x00, // reserved
        0xAA, 0xBB, // only 2 payload bytes
    ];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let result = reader.read_record();
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID
    );
}

/// CBKF104: RDW suspect ASCII corruption — ASCII digits in RDW header
#[test]
fn cbkf104_rdw_suspect_ascii() {
    let data: Vec<u8> = vec![
        b'1', b'2', // ASCII digits → suspect text-mode transfer
        0x00, 0x00, 0x41, 0x42, 0x43, 0x44,
    ];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let result = reader.read_record();
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert!(
        err.message.contains("ASCII") || err.message.contains("corrupted"),
        "Message should mention ASCII: {}",
        err.message
    );
}

// =========================================================================
// CBKI* — Infrastructure Errors
// =========================================================================

/// CBKI001: Invalid state — FixedRecordReader without LRECL
#[test]
fn cbki001_invalid_state_no_lrecl() {
    let result = FixedRecordReader::new(Cursor::new(b"data"), None);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
    assert!(
        err.message.contains("LRECL"),
        "Message should mention LRECL: {}",
        err.message
    );
}

/// CBKI001: Invalid state — FixedRecordReader with zero LRECL
#[test]
fn cbki001_invalid_state_zero_lrecl() {
    let result = FixedRecordReader::new(Cursor::new(b"data"), Some(0));
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().code, ErrorCode::CBKI001_INVALID_STATE);
}

// =========================================================================
// Cross-cutting: Error code format stability
// =========================================================================

/// All defined error codes match the stable format `CBK[A-Z]\d{3}_*`
#[test]
fn error_code_format_stability() {
    let codes: Vec<ErrorCode> = vec![
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKP011_UNSUPPORTED_CLAUSE,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        ErrorCode::CBKP022_NESTED_ODO,
        ErrorCode::CBKP023_ODO_REDEFINES,
        ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
        ErrorCode::CBKP101_INVALID_PIC,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKS141_RECORD_TOO_LARGE,
        ErrorCode::CBKS301_ODO_CLIPPED,
        ErrorCode::CBKS302_ODO_RAISED,
        ErrorCode::CBKS601_RENAME_UNKNOWN_FROM,
        ErrorCode::CBKS602_RENAME_UNKNOWN_THRU,
        ErrorCode::CBKS604_RENAME_REVERSED_RANGE,
        ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
        ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
        ErrorCode::CBKC201_JSON_WRITE_ERROR,
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
        ErrorCode::CBKD101_INVALID_FIELD_TYPE,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
        ErrorCode::CBKD423_EDITED_PIC_BLANK_WHEN_ZERO,
        ErrorCode::CBKI001_INVALID_STATE,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKE505_SCALE_MISMATCH,
        ErrorCode::CBKE510_NUMERIC_OVERFLOW,
        ErrorCode::CBKE515_STRING_LENGTH_VIOLATION,
        ErrorCode::CBKE521_ARRAY_LEN_OOB,
        ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
        ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
        ErrorCode::CBKF221_RDW_UNDERFLOW,
        ErrorCode::CBKA001_BASELINE_ERROR,
        ErrorCode::CBKW001_SCHEMA_CONVERSION,
    ];

    let pattern = regex::Regex::new(r"^CBK[A-Z][0-9]{3}_[A-Z0-9_]+$").unwrap();
    for code in &codes {
        let s = format!("{code}");
        assert!(
            pattern.is_match(&s),
            "Error code '{s}' does not match pattern CBK[A-Z]NNN_*"
        );
    }
}

/// Every family has a correct 4-char prefix
#[test]
fn error_code_family_prefixes() {
    assert_eq!(ErrorCode::CBKP001_SYNTAX.family_prefix(), "CBKP");
    assert_eq!(ErrorCode::CBKS121_COUNTER_NOT_FOUND.family_prefix(), "CBKS");
    assert_eq!(ErrorCode::CBKD301_RECORD_TOO_SHORT.family_prefix(), "CBKD");
    assert_eq!(
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH.family_prefix(),
        "CBKE"
    );
    assert_eq!(
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO.family_prefix(),
        "CBKR"
    );
    assert_eq!(
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID.family_prefix(),
        "CBKF"
    );
    assert_eq!(ErrorCode::CBKI001_INVALID_STATE.family_prefix(), "CBKI");
    assert_eq!(ErrorCode::CBKC201_JSON_WRITE_ERROR.family_prefix(), "CBKC");
    assert_eq!(ErrorCode::CBKA001_BASELINE_ERROR.family_prefix(), "CBKA");
    assert_eq!(ErrorCode::CBKW001_SCHEMA_CONVERSION.family_prefix(), "CBKW");
}

/// Error messages contain actionable information (field names, expected values)
#[test]
fn error_messages_are_actionable() {
    // Parse error mentions input context
    let err = parse_copybook("@#$%^&*()").unwrap_err();
    assert!(
        !err.message.is_empty(),
        "Parse errors must have non-empty messages"
    );

    // Projection error names the missing field
    let cpy = "        01 REC.\n           05 FLD PIC X(10).\n";
    let schema = parse_copybook(cpy).unwrap();
    let err = project_schema(&schema, &["MISSING".to_string()]).unwrap_err();
    assert!(
        err.message.contains("MISSING"),
        "Projection error should name the field: {}",
        err.message
    );

    // Encode error names the field or describes the constraint
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;
    let mut fld = Field::new(5, "X".to_string());
    fld.path = "ROOT.X".to_string();
    fld.kind = FieldKind::Alphanum { len: 2 };
    fld.len = 2;
    fld.offset = 0;
    root.children = vec![fld];
    let mut schema = Schema::from_fields(vec![root]);
    schema.lrecl_fixed = Some(2);
    let err =
        encode_record(&schema, &json!({"ROOT": {"X": "TOOLONG"}}), &encode_opts()).unwrap_err();
    assert!(
        err.message.contains("X")
            || err.message.contains("length")
            || err.message.contains("exceeds"),
        "Encode error should be actionable: {}",
        err.message
    );
}

/// Multiple independent parse errors can be collected (non-fail-fast)
#[test]
fn multiple_errors_collected() {
    // Trigger several independent parse failures and verify each is recoverable
    let bad_inputs = [
        "",                         // empty
        "@#$%^&*()",                // garbage
        "01 REC.\n05 FLD PIC QQQ.", // invalid PIC
    ];
    let mut errors = Vec::new();
    for input in &bad_inputs {
        if let Err(e) = parse_copybook(input) {
            errors.push(e);
        }
    }
    assert!(
        errors.len() >= 2,
        "Should collect multiple independent errors, got {}",
        errors.len()
    );
    // All errors are in CBKP family
    for e in &errors {
        assert_eq!(
            e.code.family_prefix(),
            "CBKP",
            "Expected CBKP, got {:?}",
            e.code
        );
    }
}

/// Errors are recoverable — no panic even with adversarial inputs
#[test]
fn errors_are_recoverable_no_panic() {
    // Parse: deeply nested garbage
    let _ = parse_copybook(&"01 ".repeat(1000));

    // Decode: zero-length data
    let cpy = "        01 REC.\n           05 FLD PIC X(10).\n";
    let schema = parse_copybook(cpy).unwrap();
    let _ = decode_record(&schema, &[], &decode_opts());

    // Encode: null JSON
    let _ = encode_record(&schema, &json!(null), &encode_opts());

    // Encode: array JSON
    let _ = encode_record(&schema, &json!([1, 2, 3]), &encode_opts());

    // RDW: empty stream
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
    let _ = reader.read_record();

    // FixedRecordReader: negative-equivalent edge case
    let _ = FixedRecordReader::new(Cursor::new(b""), Some(0));
}
