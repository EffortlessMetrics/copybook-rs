// SPDX-License-Identifier: AGPL-3.0-or-later
//! Determinism validation for COBOL copybook encoding and decoding operations.
#![allow(clippy::missing_inline_in_public_items)]
//!
//! This module verifies that encode/decode operations produce identical outputs
//! across repeated runs with the same schema, data, and options.

use crate::lib_api::{decode_record, encode_record};
use crate::options::{DecodeOptions, EncodeOptions};
use copybook_core::{Error, ErrorCode, Result, Schema};
use copybook_rdw::RdwHeader;

/// Default cap used when collecting byte-level differences.
pub const DEFAULT_MAX_DIFFS: usize = 100;

/// Hex-encoded BLAKE3 digest length in characters.
pub const BLAKE3_HEX_LEN: usize = 64;

/// Mode of determinism checking (decode-only, encode-only, or full round-trip).
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DeterminismMode {
    /// Check that decoding the same binary data twice produces identical JSON.
    DecodeOnly,
    /// Check that encoding the same JSON twice produces identical binary data.
    EncodeOnly,
    /// Check that decode→encode→decode produces identical JSON.
    RoundTrip,
}

/// Details about a byte difference found during determinism checking.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ByteDiff {
    /// Byte offset where the difference was found.
    pub offset: usize,
    /// Byte value from the first run.
    pub round1_byte: u8,
    /// Byte value from the second run.
    pub round2_byte: u8,
}

/// Result of a determinism check operation.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct DeterminismResult {
    /// The mode of checking that was performed.
    pub mode: DeterminismMode,
    /// BLAKE3 hash of the first run's output.
    pub round1_hash: String,
    /// BLAKE3 hash of the second run's output.
    pub round2_hash: String,
    /// Whether the two runs produced identical outputs.
    pub is_deterministic: bool,
    /// If non-deterministic, details of the byte differences.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byte_differences: Option<Vec<ByteDiff>>,
}

impl DeterminismResult {
    /// Returns true if both runs produced identical outputs.
    #[must_use]
    #[inline]
    pub fn passed(&self) -> bool {
        self.is_deterministic
    }

    /// Returns the number of byte differences found (0 if deterministic).
    #[must_use]
    #[inline]
    pub fn diff_count(&self) -> usize {
        self.byte_differences.as_ref().map_or(0, Vec::len)
    }
}

/// Compute a lowercase hex BLAKE3 hash for a byte slice.
#[must_use]
#[inline]
pub fn blake3_hex(data: &[u8]) -> String {
    blake3::hash(data).to_hex().to_string()
}

/// Compare two byte slices and build a determinism result with the default diff limit.
#[must_use]
#[inline]
pub fn compare_outputs(mode: DeterminismMode, round1: &[u8], round2: &[u8]) -> DeterminismResult {
    compare_outputs_with_limit(mode, round1, round2, DEFAULT_MAX_DIFFS)
}

/// Compare two byte slices and build a determinism result with an explicit diff limit.
#[must_use]
pub fn compare_outputs_with_limit(
    mode: DeterminismMode,
    round1: &[u8],
    round2: &[u8],
    max_diffs: usize,
) -> DeterminismResult {
    let hash1 = blake3::hash(round1);
    let hash2 = blake3::hash(round2);
    let is_deterministic = hash1 == hash2;

    DeterminismResult {
        mode,
        round1_hash: hash1.to_hex().to_string(),
        round2_hash: hash2.to_hex().to_string(),
        is_deterministic,
        byte_differences: if is_deterministic {
            None
        } else {
            Some(find_byte_differences_with_limit(round1, round2, max_diffs))
        },
    }
}

/// Find byte-level differences between two slices using [`DEFAULT_MAX_DIFFS`] entries at most.
#[must_use]
#[inline]
pub fn find_byte_differences(round1: &[u8], round2: &[u8]) -> Vec<ByteDiff> {
    find_byte_differences_with_limit(round1, round2, DEFAULT_MAX_DIFFS)
}

/// Find byte-level differences between two slices with an explicit limit.
#[must_use]
pub fn find_byte_differences_with_limit(
    round1: &[u8],
    round2: &[u8],
    max_diffs: usize,
) -> Vec<ByteDiff> {
    if max_diffs == 0 {
        return Vec::new();
    }

    let min_len = round1.len().min(round2.len());
    let max_len = round1.len().max(round2.len());
    let mut diffs = Vec::with_capacity(max_diffs.min(max_len));

    for (offset, (&byte_a, &byte_b)) in round1.iter().zip(round2.iter()).enumerate() {
        if byte_a != byte_b {
            diffs.push(ByteDiff {
                offset,
                round1_byte: byte_a,
                round2_byte: byte_b,
            });
            if diffs.len() >= max_diffs {
                return diffs;
            }
        }
    }

    if round1.len() != round2.len() {
        for offset in min_len..max_len {
            let byte_a = round1.get(offset).copied().unwrap_or(0);
            let byte_b = round2.get(offset).copied().unwrap_or(0);
            diffs.push(ByteDiff {
                offset,
                round1_byte: byte_a,
                round2_byte: byte_b,
            });
            if diffs.len() >= max_diffs {
                return diffs;
            }
        }
    }

    diffs
}

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
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn check_decode_determinism(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
) -> Result<DeterminismResult> {
    let payload = payload_for_format(data, options.format)?;
    let value1 = decode_record(schema, payload, options)?;
    let value2 = decode_record(schema, payload, options)?;

    let json1 = serialize_json(&value1, "first decode result")?;
    let json2 = serialize_json(&value2, "second decode result")?;

    Ok(compare_outputs(DeterminismMode::DecodeOnly, &json1, &json2))
}

/// Check that encoding the same JSON twice produces identical binary output.
///
/// # Errors
///
/// Returns an error if encoding fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
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
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn check_round_trip_determinism(
    schema: &Schema,
    data: &[u8],
    decode_opts: &DecodeOptions,
    encode_opts: &EncodeOptions,
) -> Result<DeterminismResult> {
    let decoded_payload = payload_for_format(data, decode_opts.format)?;
    let json1 = decode_record(schema, decoded_payload, decode_opts)?;
    let binary = encode_record(schema, &json1, encode_opts)?;
    let encoded_payload = payload_for_format(&binary, decode_opts.format)?;
    let json2 = decode_record(schema, encoded_payload, decode_opts)?;

    let serialized1 = serialize_json(&json1, "first round-trip decode result")?;
    let serialized2 = serialize_json(&json2, "second round-trip decode result")?;

    Ok(compare_outputs(
        DeterminismMode::RoundTrip,
        &serialized1,
        &serialized2,
    ))
}

#[inline]
fn payload_for_format(data: &[u8], format: crate::options::RecordFormat) -> Result<&[u8]> {
    if format != crate::options::RecordFormat::RDW {
        return Ok(data);
    }

    if data.len() < copybook_rdw::RDW_HEADER_LEN {
        return Err(Error::new(
            ErrorCode::CBKF221_RDW_UNDERFLOW,
            "RDW data is shorter than the 4-byte RDW header",
        ));
    }

    let header_slice = data.get(..copybook_rdw::RDW_HEADER_LEN).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKF221_RDW_UNDERFLOW,
            "RDW data is shorter than the 4-byte RDW header",
        )
    })?;

    let header_bytes: [u8; copybook_rdw::RDW_HEADER_LEN] =
        header_slice.try_into().map_err(|_| {
            Error::new(
                ErrorCode::CBKF221_RDW_UNDERFLOW,
                "RDW header must be exactly 4 bytes",
            )
        })?;

    let header = RdwHeader::from_bytes(header_bytes);

    let payload_len = usize::from(header.length());
    let expected_len = copybook_rdw::RDW_HEADER_LEN.saturating_add(payload_len);
    if data.len() != expected_len {
        return Err(Error::new(
            ErrorCode::CBKF221_RDW_UNDERFLOW,
            format!(
                "RDW payload mismatch: expected {expected_len} bytes, got {}",
                data.len()
            ),
        ));
    }

    Ok(&data[copybook_rdw::RDW_HEADER_LEN..expected_len])
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
    fn primitive_comparison_reports_bounded_differences() {
        let result =
            compare_outputs_with_limit(DeterminismMode::EncodeOnly, b"ABCDEF", b"ABxDEy", 1);

        assert!(!result.passed());
        assert_eq!(result.diff_count(), 1);
        assert_eq!(
            result.byte_differences.as_ref().expect("diffs")[0].offset,
            2
        );
    }

    #[test]
    fn primitive_hash_and_result_serde_are_stable() {
        let hash = blake3_hex(b"copybook");
        let result = compare_outputs(DeterminismMode::RoundTrip, b"copybook", b"copybook");

        assert_eq!(hash.len(), BLAKE3_HEX_LEN);
        assert_eq!(result.round1_hash, hash);
        assert!(result.byte_differences.is_none());

        let json = serde_json::to_string(&result).expect("serialize determinism result");
        let decoded: DeterminismResult =
            serde_json::from_str(&json).expect("deserialize determinism result");
        assert_eq!(decoded, result);
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
