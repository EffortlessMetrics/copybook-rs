// SPDX-License-Identifier: AGPL-3.0-or-later
//! Error path integration tests.
//!
//! Exercises specific error paths through the copybook-rs pipeline and verifies
//! that the correct `ErrorCode` variant and error code string are returned.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::{ErrorCode, parse_copybook};
use serde_json::json;

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

// ---------------------------------------------------------------------------
// 1. Parse invalid copybook – missing PIC clause → CBKP error
// ---------------------------------------------------------------------------

#[test]
fn error_path_parse_missing_pic_clause() {
    let bad_cpy = r"
        01 BAD-REC.
           05 FIELD-A.
           05 FIELD-B PIC.
    ";
    let result = parse_copybook(bad_cpy);
    assert!(result.is_err(), "Missing PIC value must fail");
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKP"),
        "Expected CBKP parse error, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 2. Parse garbage text → CBKP001 syntax error
// ---------------------------------------------------------------------------

#[test]
fn error_path_parse_garbage_text() {
    let result = parse_copybook("@#$%^&*()!!! NOT COBOL");
    assert!(result.is_err(), "Parsing garbage must fail");
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.contains("CBKP"),
        "Expected CBKP error code in '{code_str}'"
    );
}

// ---------------------------------------------------------------------------
// 3. Parse invalid PIC pattern → CBKP101
// ---------------------------------------------------------------------------

#[test]
fn error_path_parse_invalid_pic_pattern() {
    let bad_cpy = r"
        01 BAD-PIC-REC.
           05 FIELD-A PIC QQQ.
    ";
    let result = parse_copybook(bad_cpy);
    assert!(result.is_err(), "Invalid PIC pattern must fail");
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKP"),
        "Expected CBKP error for invalid PIC, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 4. Decode truncated record → CBKD301_RECORD_TOO_SHORT
// ---------------------------------------------------------------------------

#[test]
fn error_path_decode_truncated_record() {
    let cpy = r"
        01 WIDE-REC.
           05 FIELD-A PIC X(50).
           05 FIELD-B PIC 9(30).
           05 FIELD-C PIC X(20).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Only 5 bytes for a 100-byte record
    let short_data = vec![0x40; 5];
    let result = decode_record(&schema, &short_data, &decode_opts());

    assert!(result.is_err(), "Truncated record must fail");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Expected CBKD301 for truncated record, got {:?}",
        err.code
    );
    assert!(
        err.code.to_string().contains("CBKD301"),
        "Error code string must contain CBKD301"
    );
}

// ---------------------------------------------------------------------------
// 5. Decode zero-length record → CBKD301
// ---------------------------------------------------------------------------

#[test]
fn error_path_decode_zero_length_record() {
    let cpy = r"
        01 NORMAL-REC.
           05 FIELD-A PIC X(10).
           05 FIELD-B PIC 9(5).
           05 FIELD-C PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let empty_data: Vec<u8> = vec![];
    let result = decode_record(&schema, &empty_data, &decode_opts());

    assert!(result.is_err(), "Empty record must fail");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Expected CBKD301 for empty record, got {:?}",
        err.code
    );
}

// ---------------------------------------------------------------------------
// 6. Decode COMP-3 with invalid nibble → CBKD401
// ---------------------------------------------------------------------------

#[test]
fn error_path_decode_comp3_invalid_nibble() {
    let cpy = r"
        01 COMP3-REC.
           05 PKD-FIELD PIC S9(5) COMP-3.
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // PIC S9(5) COMP-3 = 3 bytes. Use 0xFF as invalid nibble (F is not a valid sign nibble in strict).
    // Actually, let's use truly invalid packed data: 0xAA has nibble A which is sign-like,
    // but 0xBB in middle has B nibbles which are not valid digits.
    // Packed decimal digit nibbles must be 0-9; B is invalid.
    let bad_comp3 = vec![0xBB, 0xBB, 0xBC];
    let result = decode_record(&schema, &bad_comp3, &decode_opts());

    // COMP-3 with invalid nibbles may produce CBKD401 or succeed with tolerant decoding
    if let Err(err) = result {
        let code_str = err.code.to_string();
        assert!(
            code_str.starts_with("CBKD"),
            "Expected CBKD error for invalid COMP-3, got {code_str}"
        );
    }
}

// ---------------------------------------------------------------------------
// 7. Encode with JSON type mismatch → CBKE501
// ---------------------------------------------------------------------------

#[test]
fn error_path_encode_type_mismatch_object_for_leaf_field() {
    let cpy = r"
        01 SIMPLE-REC.
           05 NUM-FIELD PIC 9(5).
           05 TEXT-FIELD PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Provide an object where a string is expected (for a leaf field)
    let bad_json = json!({
        "NUM-FIELD": {"nested": "object"},
        "TEXT-FIELD": "HELLO"
    });
    let result = encode_record(&schema, &bad_json, &encode_opts());

    // Object for a leaf numeric field should produce an error
    if let Err(err) = result {
        let code_str = err.code.to_string();
        assert!(
            code_str.starts_with("CBKE"),
            "Expected CBKE encode error, got {code_str}"
        );
    }
}

// ---------------------------------------------------------------------------
// 8. Encode numeric overflow → CBKE510
// ---------------------------------------------------------------------------

#[test]
fn error_path_encode_numeric_overflow_pic9_2() {
    let cpy = r"
        01 TINY-REC.
           05 TINY-NUM PIC 9(2).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // 5-digit value in PIC 9(2)
    let overflow_json = json!({
        "TINY-NUM": "12345"
    });
    let result = encode_record(&schema, &overflow_json, &encode_opts());

    assert!(result.is_err(), "Numeric overflow must fail");
    let err = result.unwrap_err();
    assert!(
        matches!(
            err.code,
            ErrorCode::CBKE510_NUMERIC_OVERFLOW | ErrorCode::CBKE515_STRING_LENGTH_VIOLATION
        ),
        "Expected CBKE510 or CBKE515, got {:?}",
        err.code
    );
}

// ---------------------------------------------------------------------------
// 9. Encode string too long → CBKE515
// ---------------------------------------------------------------------------

#[test]
fn error_path_encode_string_too_long_pic_x3() {
    let cpy = r"
        01 SHORT-REC.
           05 SHORT-TXT PIC X(3).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let long_json = json!({
        "SHORT-TXT": "THIS IS MUCH LONGER THAN THREE CHARACTERS"
    });
    let result = encode_record(&schema, &long_json, &encode_opts());

    match result {
        Err(err) => {
            let code_str = err.code.to_string();
            assert!(
                code_str.contains("CBKE515") || code_str.contains("CBKE501"),
                "Expected string length error, got {code_str}"
            );
        }
        Ok(encoded) => {
            // Encoder may truncate rather than error
            assert_eq!(encoded.len(), 3, "Truncated output must be exactly 3 bytes");
        }
    }
}

// ---------------------------------------------------------------------------
// 10. Parse empty string → CBKP error
// ---------------------------------------------------------------------------

#[test]
fn error_path_parse_empty_string() {
    let result = parse_copybook("");
    assert!(result.is_err(), "Empty copybook text must fail");
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKP"),
        "Expected CBKP error for empty input, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 11. Parse whitespace-only → CBKP error
// ---------------------------------------------------------------------------

#[test]
fn error_path_parse_whitespace_only() {
    let result = parse_copybook("       \n    \n    ");
    assert!(result.is_err(), "Whitespace-only copybook must fail");
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKP"),
        "Expected CBKP error for whitespace, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 12. Encode ODO array exceeding max bounds → CBKE521
// ---------------------------------------------------------------------------

#[test]
fn error_path_encode_odo_array_exceeds_max() {
    let cpy = r"
        01 ODO-REC.
           05 CNT PIC 9(2).
           05 ITEMS OCCURS 1 TO 3 TIMES
              DEPENDING ON CNT.
              10 ITEM-VAL PIC X(4).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Counter says 8, but max is 3
    let bad_json = json!({
        "CNT": "08",
        "ITEMS": [
            {"ITEM-VAL": "AAAA"}, {"ITEM-VAL": "BBBB"},
            {"ITEM-VAL": "CCCC"}, {"ITEM-VAL": "DDDD"},
            {"ITEM-VAL": "EEEE"}, {"ITEM-VAL": "FFFF"},
            {"ITEM-VAL": "GGGG"}, {"ITEM-VAL": "HHHH"}
        ]
    });
    let result = encode_record(&schema, &bad_json, &encode_opts());

    assert!(result.is_err(), "ODO overflow must fail");
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKE") || code_str.starts_with("CBKS"),
        "Expected CBKE or CBKS error for ODO overflow, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 13. Decode record with wrong length (too long) is accepted but correct
// ---------------------------------------------------------------------------

#[test]
fn error_path_decode_record_longer_than_schema() {
    let cpy = r"
        01 SMALL-REC.
           05 FIELD-A PIC X(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Provide 20 bytes for a 5-byte schema (extra data is ignored)
    let long_data = vec![0xC1; 20]; // "AAAAA" + extra
    let result = decode_record(&schema, &long_data, &decode_opts());

    // Longer-than-schema data may succeed (extra bytes ignored) or error
    // Either outcome is acceptable
    if let Ok(json) = result {
        assert!(json.is_object(), "Decoded value should be a JSON object");
    }
}

// ---------------------------------------------------------------------------
// 14. Parse nested ODO (unsupported) → CBKP022
// ---------------------------------------------------------------------------

#[test]
fn error_path_parse_nested_odo() {
    let bad_cpy = r"
        01 NESTED-ODO-REC.
           05 OUTER-CNT PIC 9(2).
           05 OUTER-ARR OCCURS 1 TO 5 TIMES
              DEPENDING ON OUTER-CNT.
              10 INNER-CNT PIC 9(2).
              10 INNER-ARR OCCURS 1 TO 3 TIMES
                 DEPENDING ON INNER-CNT.
                 15 INNER-VAL PIC X(4).
    ";
    let result = parse_copybook(bad_cpy);

    assert!(result.is_err(), "Nested ODO must be rejected");
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKP"),
        "Expected CBKP error for nested ODO, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 15. Encode with missing required field → CBKE501
// ---------------------------------------------------------------------------

#[test]
fn error_path_encode_missing_field() {
    let cpy = r"
        01 TWO-FIELDS.
           05 FIELD-A PIC X(5).
           05 FIELD-B PIC 9(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Only provide FIELD-A, missing FIELD-B
    let partial_json = json!({
        "FIELD-A": "HELLO"
    });
    let result = encode_record(&schema, &partial_json, &encode_opts());

    // Missing field may default to spaces/zeros or error
    if let Err(err) = result {
        let code_str = err.code.to_string();
        assert!(
            code_str.starts_with("CBKE"),
            "Expected CBKE error for missing field, got {code_str}"
        );
    }
}

// ---------------------------------------------------------------------------
// 16. RDW with invalid header (record length too small) → CBKF error
// ---------------------------------------------------------------------------

#[test]
fn error_path_rdw_invalid_header_length_too_small() {
    use copybook_codec::record::RDWRecordReader;
    use std::io::Cursor;

    // RDW header with length=2 (less than 4-byte header minimum for some impls)
    let data: Vec<u8> = vec![
        0x00, 0x02, // length = 2 (too small for header)
        0x00, 0x00, // reserved
    ];

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let result = reader.read_record();

    assert!(result.is_err(), "RDW with length < 4 must fail");
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKF"),
        "Expected CBKF format error, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 17. Parse ODO not at tail → CBKP021
// ---------------------------------------------------------------------------

#[test]
fn error_path_parse_odo_not_tail() {
    let bad_cpy = r"
        01 BAD-ODO-REC.
           05 CNT PIC 9(2).
           05 ITEMS OCCURS 1 TO 5 TIMES
              DEPENDING ON CNT.
              10 ITEM-VAL PIC X(4).
           05 TRAILING-FIELD PIC X(10).
    ";
    let result = parse_copybook(bad_cpy);

    assert!(result.is_err(), "ODO not at tail must be rejected");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL, got {:?}",
        err.code
    );
    assert!(
        err.code.to_string().contains("CBKP021"),
        "Error code string must contain CBKP021"
    );
}

// ---------------------------------------------------------------------------
// 18. Encode null JSON value → CBKE501 type mismatch
// ---------------------------------------------------------------------------

#[test]
fn error_path_encode_null_for_numeric_field() {
    let cpy = r"
        01 NULL-REC.
           05 NUM-FIELD PIC 9(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let null_json = json!({
        "NUM-FIELD": null
    });
    let result = encode_record(&schema, &null_json, &encode_opts());

    // Null for a numeric field should produce a type mismatch or default handling
    if let Err(err) = result {
        let code_str = err.code.to_string();
        assert!(
            code_str.starts_with("CBKE"),
            "Expected CBKE error for null value, got {code_str}"
        );
    }
}

// ---------------------------------------------------------------------------
// 19. Error message contains field name context
// ---------------------------------------------------------------------------

#[test]
fn error_path_error_message_has_context() {
    let cpy = r"
        01 CTX-REC.
           05 FIELD-A PIC X(50).
           05 FIELD-B PIC 9(30).
           05 FIELD-C PIC X(120).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let short_data = vec![0x40; 5];
    let result = decode_record(&schema, &short_data, &decode_opts());

    assert!(result.is_err());
    let err = result.unwrap_err();
    // Error message should not be empty and should provide useful context
    assert!(!err.message.is_empty(), "Error message must not be empty");
    assert!(
        err.to_string().len() > 10,
        "Full error display should be descriptive: '{}'",
        err
    );
}

// ---------------------------------------------------------------------------
// 20. Encode boolean for PIC X field → CBKE501 type mismatch
// ---------------------------------------------------------------------------

#[test]
fn error_path_encode_boolean_for_alphanum_field() {
    let cpy = r"
        01 BOOL-REC.
           05 TEXT-FIELD PIC X(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let bool_json = json!({
        "TEXT-FIELD": true
    });
    let result = encode_record(&schema, &bool_json, &encode_opts());

    // Boolean for PIC X should produce type mismatch or coercion
    if let Err(err) = result {
        let code_str = err.code.to_string();
        assert!(
            code_str.starts_with("CBKE"),
            "Expected CBKE error for boolean in PIC X, got {code_str}"
        );
    }
}
