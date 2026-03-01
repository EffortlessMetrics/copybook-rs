// SPDX-License-Identifier: AGPL-3.0-or-later
//! End-to-end error-handling tests.
//!
//! Each test exercises a specific error path through the copybook-rs pipeline
//! and verifies that the correct `ErrorCode` is returned with a meaningful
//! message – no panics, no generic catch-alls.

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
// 1. Invalid copybook syntax → CBKP001
// ---------------------------------------------------------------------------

#[test]
fn error_invalid_copybook_syntax() {
    let bad_cpy = "THIS IS NOT A VALID COPYBOOK AT ALL !!!";
    let result = parse_copybook(bad_cpy);

    assert!(result.is_err(), "Parsing garbage text must fail");
    let err = result.unwrap_err();
    assert!(
        matches!(
            err.code,
            ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP101_INVALID_PIC
        ),
        "Expected CBKP parse error, got {:?}",
        err.code
    );
    assert!(!err.message.is_empty(), "Error message must not be empty");
}

// ---------------------------------------------------------------------------
// 2. Truncated binary record → CBKD301
// ---------------------------------------------------------------------------

#[test]
fn error_truncated_record() {
    let cpy = r"
        01 BIG-REC.
           05 FIELD-A  PIC X(100).
           05 FIELD-B  PIC 9(50).
    ";

    let schema = parse_copybook(cpy).expect("parse");

    // Provide only 10 bytes for a 150-byte schema
    let short_data: Vec<u8> = vec![0x40; 10];
    let result = decode_record(&schema, &short_data, &decode_opts());

    assert!(result.is_err(), "Decoding a truncated record must fail");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        "Expected CBKD301 for short record, got {:?}",
        err.code
    );
}

// ---------------------------------------------------------------------------
// 3. Type mismatch during encode → CBKE501
// ---------------------------------------------------------------------------

#[test]
fn error_encode_odo_bounds() {
    let cpy = r"
        01 ODO-REC.
           05 ITEM-COUNT  PIC 9(3).
           05 ITEMS OCCURS 1 TO 5 TIMES
              DEPENDING ON ITEM-COUNT.
              10 ITEM-VAL  PIC X(4).
    ";

    let schema = parse_copybook(cpy).expect("parse");

    // Provide 10 array items when max is 5 and counter says 10
    let bad_json = json!({
        "ITEM-COUNT": "010",
        "ITEMS": [
            {"ITEM-VAL": "AAAA"}, {"ITEM-VAL": "BBBB"}, {"ITEM-VAL": "CCCC"},
            {"ITEM-VAL": "DDDD"}, {"ITEM-VAL": "EEEE"}, {"ITEM-VAL": "FFFF"},
            {"ITEM-VAL": "GGGG"}, {"ITEM-VAL": "HHHH"}, {"ITEM-VAL": "IIII"},
            {"ITEM-VAL": "JJJJ"}
        ]
    });

    let result = encode_record(&schema, &bad_json, &encode_opts());

    assert!(
        result.is_err(),
        "Encoding over-capacity ODO array must fail"
    );
    let err = result.unwrap_err();
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKE") || code_str.starts_with("CBKS"),
        "Expected CBKE or CBKS error, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 4. Numeric overflow during encode → CBKE510
// ---------------------------------------------------------------------------

#[test]
fn error_encode_numeric_overflow() {
    let cpy = r"
        01 OVERFLOW-REC.
           05 SMALL-NUM  PIC 9(3).
    ";

    let schema = parse_copybook(cpy).expect("parse");

    // Value exceeds PIC 9(3) capacity (max 999)
    let overflow_json = json!({
        "SMALL-NUM": "999999"
    });

    let result = encode_record(&schema, &overflow_json, &encode_opts());

    assert!(result.is_err(), "Encoding overflow value must fail");
    let err = result.unwrap_err();
    assert!(
        matches!(
            err.code,
            ErrorCode::CBKE510_NUMERIC_OVERFLOW | ErrorCode::CBKE515_STRING_LENGTH_VIOLATION
        ),
        "Expected numeric overflow or length error, got {:?}",
        err.code
    );
}

// ---------------------------------------------------------------------------
// 5. Empty copybook → parse error
// ---------------------------------------------------------------------------

#[test]
fn error_empty_copybook() {
    let result = parse_copybook("");
    assert!(result.is_err(), "Parsing empty string must fail");
    let err = result.unwrap_err();
    // Any CBKP error code is acceptable for empty input
    let code_str = err.code.to_string();
    assert!(
        code_str.starts_with("CBKP"),
        "Expected CBKP parse error for empty input, got {code_str}"
    );
}

// ---------------------------------------------------------------------------
// 6. String too long for PIC X field → CBKE515
// ---------------------------------------------------------------------------

#[test]
fn error_encode_string_too_long() {
    let cpy = r"
        01 STR-REC.
           05 SHORT-FIELD  PIC X(5).
    ";

    let schema = parse_copybook(cpy).expect("parse");

    // Provide a string longer than 5 characters
    let long_json = json!({
        "SHORT-FIELD": "THIS STRING IS WAY TOO LONG FOR A 5-BYTE FIELD"
    });

    let result = encode_record(&schema, &long_json, &encode_opts());

    // Depending on configuration, this may truncate or error
    match result {
        Err(err) => {
            assert!(
                matches!(
                    err.code,
                    ErrorCode::CBKE515_STRING_LENGTH_VIOLATION
                        | ErrorCode::CBKE501_JSON_TYPE_MISMATCH
                ),
                "Expected string length error, got {:?}",
                err.code
            );
        }
        Ok(encoded) => {
            // If the encoder truncates rather than errors, verify the
            // output length matches the schema
            assert_eq!(encoded.len(), 5, "Encoded output must be exactly 5 bytes");
        }
    }
}
