// SPDX-License-Identifier: AGPL-3.0-or-later
//! Determinism validation for COBOL copybook encoding and decoding operations.
#![allow(clippy::missing_inline_in_public_items)]
//!
//! This module verifies that encode/decode operations produce identical outputs
//! across repeated runs with the same schema, data, and options.

use crate::lib_api::{decode_record, encode_record};
use crate::options::{DecodeOptions, EncodeOptions};
use copybook_core::{Error, ErrorCode, Result, Schema};
use copybook_determinism::compare_outputs;

pub use copybook_determinism::{ByteDiff, DeterminismMode, DeterminismResult};

fn serialize_json(value: &serde_json::Value, context: &str) -> Result<Vec<u8>> {
    serde_json::to_vec(value).map_err(|e| {
        Error::new(
            ErrorCode::CBKC201_JSON_WRITE_ERROR,
            format!("Failed to serialize {context}: {e}"),
        )
    })
}

/// Check that decoding the same binary data twice produces identical JSON output.
///
/// # Errors
///
/// Returns an error if decoding or JSON serialization fails.
pub fn check_decode_determinism(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
) -> Result<DeterminismResult> {
    let value1 = decode_record(schema, data, options)?;
    let value2 = decode_record(schema, data, options)?;

    let json1 = serialize_json(&value1, "first decode result")?;
    let json2 = serialize_json(&value2, "second decode result")?;

    Ok(compare_outputs(DeterminismMode::DecodeOnly, &json1, &json2))
}

/// Check that encoding the same JSON twice produces identical binary output.
///
/// # Errors
///
/// Returns an error if encoding fails.
pub fn check_encode_determinism(
    schema: &Schema,
    json_data: &serde_json::Value,
    options: &EncodeOptions,
) -> Result<DeterminismResult> {
    let binary1 = encode_record(schema, json_data, options)?;
    let binary2 = encode_record(schema, json_data, options)?;

    Ok(compare_outputs(
        DeterminismMode::EncodeOnly,
        &binary1,
        &binary2,
    ))
}

/// Check full round-trip determinism: decode->encode->decode.
///
/// # Errors
///
/// Returns an error if any decode/encode or JSON serialization step fails.
pub fn check_round_trip_determinism(
    schema: &Schema,
    data: &[u8],
    decode_opts: &DecodeOptions,
    encode_opts: &EncodeOptions,
) -> Result<DeterminismResult> {
    let json1 = decode_record(schema, data, decode_opts)?;
    let binary = encode_record(schema, &json1, encode_opts)?;
    let json2 = decode_record(schema, &binary, decode_opts)?;

    let serialized1 = serialize_json(&json1, "first round-trip decode result")?;
    let serialized2 = serialize_json(&json2, "second round-trip decode result")?;

    Ok(compare_outputs(
        DeterminismMode::RoundTrip,
        &serialized1,
        &serialized2,
    ))
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::options::{Codepage, RecordFormat};
    use copybook_core::parse_copybook;

    fn decode_opts() -> DecodeOptions {
        DecodeOptions::new().with_codepage(Codepage::CP037)
    }

    fn encode_opts() -> EncodeOptions {
        EncodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_format(RecordFormat::Fixed)
    }

    #[test]
    fn decode_deterministic_for_display_schema() {
        let copybook = r"
            01 RECORD.
               05 FIELD-A PIC X(10).
        ";
        let schema = parse_copybook(copybook).expect("parse copybook");

        let data: Vec<u8> = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1];

        let result =
            check_decode_determinism(&schema, &data, &decode_opts()).expect("determinism check");

        assert!(
            result.is_deterministic,
            "Expected deterministic decode for DISPLAY-only schema"
        );
        assert_eq!(result.mode, DeterminismMode::DecodeOnly);
        assert!(result.byte_differences.is_none());
        assert_eq!(result.diff_count(), 0);
        assert!(result.passed());
    }

    #[test]
    fn decode_deterministic_for_comp3_schema() {
        let copybook = r"
            01 RECORD.
               05 AMOUNT PIC S9(7)V99 COMP-3.
        ";
        let schema = parse_copybook(copybook).expect("parse copybook");

        let data = vec![0x12, 0x34, 0x56, 0x78, 0x9C];

        let result =
            check_decode_determinism(&schema, &data, &decode_opts()).expect("determinism check");

        assert!(
            result.is_deterministic,
            "Expected deterministic decode for COMP-3 schema"
        );
        assert!(result.passed());
    }

    #[test]
    fn encode_deterministic_for_display_schema() {
        let copybook = r"
            01 RECORD.
               05 FIELD-A PIC X(5).
        ";
        let schema = parse_copybook(copybook).expect("parse copybook");
        let json = serde_json::json!({"FIELD-A": "HELLO"});

        let result =
            check_encode_determinism(&schema, &json, &encode_opts()).expect("determinism check");

        assert!(
            result.is_deterministic,
            "Expected deterministic encode for DISPLAY-only schema"
        );
        assert_eq!(result.mode, DeterminismMode::EncodeOnly);
        assert!(result.byte_differences.is_none());
    }

    #[test]
    fn round_trip_deterministic() {
        let copybook = r"
            01 RECORD.
               05 NAME PIC X(10).
               05 AGE  PIC 9(3).
        ";
        let schema = parse_copybook(copybook).expect("parse copybook");

        let data: Vec<u8> = vec![
            0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0xF1, 0xF2, 0xF3,
        ];

        let result = check_round_trip_determinism(&schema, &data, &decode_opts(), &encode_opts())
            .expect("round-trip check");

        assert!(result.is_deterministic, "Expected deterministic round-trip");
        assert_eq!(result.mode, DeterminismMode::RoundTrip);
    }

    #[test]
    fn detect_json_serialization_nondeterminism() {
        let json1 = serde_json::json!({"FIELD": "VALUE1"});
        let json2 = serde_json::json!({"FIELD": "VALUE2"});

        let bytes1 = serde_json::to_vec(&json1).expect("serialize json1");
        let bytes2 = serde_json::to_vec(&json2).expect("serialize json2");

        let result = compare_outputs(DeterminismMode::DecodeOnly, &bytes1, &bytes2);
        assert!(!result.is_deterministic);
        assert!(result.diff_count() > 0);
    }

    #[test]
    fn decode_error_propagates_correctly() {
        let copybook = r"
            01 RECORD.
               05 AMOUNT PIC S9(7)V99 COMP-3.
        ";
        let schema = parse_copybook(copybook).expect("parse copybook");

        let truncated_data = vec![0x12, 0x34];

        let result = check_decode_determinism(&schema, &truncated_data, &decode_opts());

        assert!(
            result.is_err(),
            "Should return error for truncated COMP-3 data"
        );
    }

    #[test]
    fn encode_error_propagates_correctly() {
        let copybook = r"
            01 RECORD.
               05 FIELD PIC 9(5).
        ";
        let schema = parse_copybook(copybook).expect("parse copybook");

        let invalid_json = serde_json::json!({"FIELD": "NOT_A_NUMBER"});

        let result = check_encode_determinism(&schema, &invalid_json, &encode_opts());

        assert!(
            result.is_err(),
            "Should return error for type mismatch in encoding"
        );
    }

    #[test]
    fn round_trip_error_propagates() {
        let copybook = r"
            01 RECORD.
               05 AMOUNT PIC S9(7)V99 COMP-3.
        ";
        let schema = parse_copybook(copybook).expect("parse copybook");

        let bad_data = vec![0x12, 0x34];

        let result =
            check_round_trip_determinism(&schema, &bad_data, &decode_opts(), &encode_opts());

        assert!(
            result.is_err(),
            "Should return error for truncated data in round-trip"
        );
    }

    #[test]
    fn insufficient_data_handling_is_stable() {
        let copybook = r"
            01 RECORD.
               05 FIELD PIC X(5).
        ";
        let schema = parse_copybook(copybook).expect("parse copybook");

        let insufficient_data = vec![0x40, 0x40, 0x40];

        let result = check_decode_determinism(&schema, &insufficient_data, &decode_opts());

        if let Ok(det_result) = result {
            assert!(
                det_result.is_deterministic,
                "If insufficient data is handled, it must be deterministic"
            );
        }
    }
}
