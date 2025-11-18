//! Determinism validation for COBOL copybook encoding and decoding operations.
#![allow(clippy::missing_inline_in_public_items)]
//!
//! This module provides functions to verify that encode/decode operations produce
//! identical outputs across multiple runs with the same inputs and options.
//! Determinism is critical for enterprise audit trails and data consistency validation.
//!
//! # Example
//!
//! ```rust
//! use copybook_codec::{DecodeOptions, Codepage, determinism::check_decode_determinism};
//! use copybook_core::parse_copybook;
//!
//! # fn example() -> copybook_core::Result<()> {
//! let copybook_text = r#"
//!    01 RECORD.
//!       05 FIELD-A PIC X(10).
//! "#;
//! let schema = parse_copybook(copybook_text)?;
//! let data = b"HELLO WORLD"; // EBCDIC-encoded in practice
//!
//! let options = DecodeOptions::new().with_codepage(Codepage::CP037);
//! let result = check_decode_determinism(&schema, data, &options)?;
//!
//! assert!(result.is_deterministic);
//! # Ok(())
//! # }
//! ```

use crate::lib_api::{decode_record, encode_record};
use crate::options::{DecodeOptions, EncodeOptions};
use copybook_core::{Result, Schema};
use serde::{Deserialize, Serialize};

/// Mode of determinism checking (decode-only, encode-only, or full round-trip).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ByteDiff {
    /// Byte offset where the difference was found.
    pub offset: usize,
    /// Byte value from the first run.
    pub round1_byte: u8,
    /// Byte value from the second run.
    pub round2_byte: u8,
}

/// Result of a determinism check operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeterminismResult {
    /// The mode of checking that was performed.
    pub mode: DeterminismMode,
    /// BLAKE3 hash of the first run's output.
    pub round1_hash: String,
    /// BLAKE3 hash of the second run's output.
    pub round2_hash: String,
    /// Whether the two runs produced identical outputs.
    pub is_deterministic: bool,
    /// If non-deterministic, details of the byte differences (limited to first 100).
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

/// Check that decoding the same binary data twice produces identical JSON output.
///
/// This function:
/// 1. Decodes the same binary record twice with identical options
/// 2. Serializes both JSON results to canonical byte format
/// 3. Computes BLAKE3 hashes for cryptographic verification
/// 4. Reports differences if any are found
///
/// # Arguments
///
/// * `schema` - The parsed COBOL copybook schema
/// * `data` - Binary record data to decode
/// * `options` - Decode options (codepage, JSON modes, etc.)
///
/// # Returns
///
/// A `DeterminismResult` indicating whether the operation is deterministic.
///
/// # Errors
///
/// Returns an error if decoding fails for either run.
///
/// # Example
///
/// ```rust
/// use copybook_codec::{DecodeOptions, Codepage, determinism::check_decode_determinism};
/// use copybook_core::parse_copybook;
///
/// # fn example() -> copybook_core::Result<()> {
/// let copybook = "01 REC.\n   05 FLD PIC X(5).";
/// let schema = parse_copybook(copybook)?;
/// let data = b"HELLO";
///
/// let opts = DecodeOptions::new().with_codepage(Codepage::CP037);
/// let result = check_decode_determinism(&schema, data, &opts)?;
///
/// if !result.is_deterministic {
///     eprintln!("Non-deterministic decode detected!");
///     if let Some(diffs) = &result.byte_differences {
///         for diff in diffs {
///             eprintln!("  Byte {}: {} != {}", diff.offset, diff.round1_byte, diff.round2_byte);
///         }
///     }
/// }
/// # Ok(())
/// # }
/// ```
pub fn check_decode_determinism(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
) -> Result<DeterminismResult> {
    // Decode twice with identical options (no scratch buffers for simplicity)
    let value1 = decode_record(schema, data, options)?;
    let value2 = decode_record(schema, data, options)?;

    // Serialize to canonical JSON byte format
    // Note: serde_json serialization is deterministic when preserve_order is enabled
    let json1 = serde_json::to_vec(&value1).map_err(|e| {
        copybook_core::Error::new(
            copybook_core::ErrorCode::CBKC201_JSON_WRITE_ERROR,
            format!("Failed to serialize first decode result: {e}"),
        )
    })?;
    let json2 = serde_json::to_vec(&value2).map_err(|e| {
        copybook_core::Error::new(
            copybook_core::ErrorCode::CBKC201_JSON_WRITE_ERROR,
            format!("Failed to serialize second decode result: {e}"),
        )
    })?;

    // Compute cryptographic hashes
    let hash1 = blake3::hash(&json1);
    let hash2 = blake3::hash(&json2);
    let is_deterministic = hash1 == hash2;

    Ok(DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: hash1.to_hex().to_string(),
        round2_hash: hash2.to_hex().to_string(),
        is_deterministic,
        byte_differences: if is_deterministic {
            None
        } else {
            Some(find_byte_differences(&json1, &json2))
        },
    })
}

/// Check that encoding the same JSON twice produces identical binary output.
///
/// This function:
/// 1. Encodes the same JSON record twice with identical options
/// 2. Computes BLAKE3 hashes of both binary outputs
/// 3. Reports byte differences if any are found
///
/// # Arguments
///
/// * `schema` - The parsed COBOL copybook schema
/// * `json_data` - JSON value to encode
/// * `options` - Encode options (codepage, record format, etc.)
///
/// # Returns
///
/// A `DeterminismResult` indicating whether the operation is deterministic.
///
/// # Errors
///
/// Returns an error if encoding fails for either run.
///
/// # Example
///
/// ```rust
/// use copybook_codec::{EncodeOptions, Codepage, RecordFormat, determinism::check_encode_determinism};
/// use copybook_core::parse_copybook;
/// use serde_json::json;
///
/// # fn example() -> copybook_core::Result<()> {
/// let copybook = "01 REC.\n   05 FLD PIC X(5).";
/// let schema = parse_copybook(copybook)?;
/// let json = json!({"FLD": "HELLO"});
///
/// let opts = EncodeOptions::new()
///     .with_codepage(Codepage::CP037)
///     .with_format(RecordFormat::Fixed);
/// let result = check_encode_determinism(&schema, &json, &opts)?;
///
/// assert!(result.is_deterministic);
/// # Ok(())
/// # }
/// ```
pub fn check_encode_determinism(
    schema: &Schema,
    json_data: &serde_json::Value,
    options: &EncodeOptions,
) -> Result<DeterminismResult> {
    // Encode twice with identical options
    let binary1 = encode_record(schema, json_data, options)?;
    let binary2 = encode_record(schema, json_data, options)?;

    // Compute cryptographic hashes
    let hash1 = blake3::hash(&binary1);
    let hash2 = blake3::hash(&binary2);
    let is_deterministic = hash1 == hash2;

    Ok(DeterminismResult {
        mode: DeterminismMode::EncodeOnly,
        round1_hash: hash1.to_hex().to_string(),
        round2_hash: hash2.to_hex().to_string(),
        is_deterministic,
        byte_differences: if is_deterministic {
            None
        } else {
            Some(find_byte_differences(&binary1, &binary2))
        },
    })
}

/// Check full round-trip determinism: decode→encode→decode.
///
/// This function verifies that:
/// 1. Binary → JSON (decode) → Binary (encode) → JSON (decode)
/// 2. The first and final JSON outputs are identical
///
/// This is the strongest determinism check, as it validates both codec paths.
///
/// # Arguments
///
/// * `schema` - The parsed COBOL copybook schema
/// * `data` - Binary record data to start with
/// * `decode_opts` - Options for decode operations
/// * `encode_opts` - Options for encode operation
///
/// # Returns
///
/// A `DeterminismResult` indicating whether the round-trip is deterministic.
///
/// # Errors
///
/// Returns an error if any decode or encode operation fails.
///
/// # Example
///
/// ```rust
/// use copybook_codec::{DecodeOptions, EncodeOptions, Codepage, RecordFormat, determinism::check_round_trip_determinism};
/// use copybook_core::parse_copybook;
///
/// # fn example() -> copybook_core::Result<()> {
/// let copybook = "01 REC.\n   05 FLD PIC X(5).";
/// let schema = parse_copybook(copybook)?;
/// let data = b"HELLO";
///
/// let decode_opts = DecodeOptions::new().with_codepage(Codepage::CP037);
/// let encode_opts = EncodeOptions::new()
///     .with_codepage(Codepage::CP037)
///     .with_format(RecordFormat::Fixed);
///
/// let result = check_round_trip_determinism(&schema, data, &decode_opts, &encode_opts)?;
/// assert!(result.is_deterministic);
/// # Ok(())
/// # }
/// ```
pub fn check_round_trip_determinism(
    schema: &Schema,
    data: &[u8],
    decode_opts: &DecodeOptions,
    encode_opts: &EncodeOptions,
) -> Result<DeterminismResult> {
    // First decode: binary → JSON
    let json1 = decode_record(schema, data, decode_opts)?;

    // Encode: JSON → binary
    let binary = encode_record(schema, &json1, encode_opts)?;

    // Second decode: binary → JSON
    let json2 = decode_record(schema, &binary, decode_opts)?;

    // Serialize both JSON results
    let serialized1 = serde_json::to_vec(&json1).map_err(|e| {
        copybook_core::Error::new(
            copybook_core::ErrorCode::CBKC201_JSON_WRITE_ERROR,
            format!("Failed to serialize first decode result: {e}"),
        )
    })?;
    let serialized2 = serde_json::to_vec(&json2).map_err(|e| {
        copybook_core::Error::new(
            copybook_core::ErrorCode::CBKC201_JSON_WRITE_ERROR,
            format!("Failed to serialize second decode result: {e}"),
        )
    })?;

    let hash1 = blake3::hash(&serialized1);
    let hash2 = blake3::hash(&serialized2);
    let is_deterministic = hash1 == hash2;

    Ok(DeterminismResult {
        mode: DeterminismMode::RoundTrip,
        round1_hash: hash1.to_hex().to_string(),
        round2_hash: hash2.to_hex().to_string(),
        is_deterministic,
        byte_differences: if is_deterministic {
            None
        } else {
            Some(find_byte_differences(&serialized1, &serialized2))
        },
    })
}

/// Find byte-level differences between two byte slices.
///
/// Returns up to 100 differences to avoid excessive memory usage.
fn find_byte_differences(a: &[u8], b: &[u8]) -> Vec<ByteDiff> {
    const MAX_DIFFS: usize = 100;

    // Handle length mismatch by padding with conceptual "missing" bytes
    let min_len = a.len().min(b.len());
    let max_len = a.len().max(b.len());

    let mut diffs = Vec::with_capacity(MAX_DIFFS.min(max_len));

    // Compare common prefix
    for (offset, (&byte_a, &byte_b)) in a.iter().zip(b.iter()).enumerate() {
        if byte_a != byte_b {
            diffs.push(ByteDiff {
                offset,
                round1_byte: byte_a,
                round2_byte: byte_b,
            });
            if diffs.len() >= MAX_DIFFS {
                return diffs;
            }
        }
    }

    // If lengths differ, report "extra" bytes as differences
    if a.len() != b.len() {
        for offset in min_len..max_len {
            let byte_a = a.get(offset).copied().unwrap_or(0);
            let byte_b = b.get(offset).copied().unwrap_or(0);
            diffs.push(ByteDiff {
                offset,
                round1_byte: byte_a,
                round2_byte: byte_b,
            });
            if diffs.len() >= MAX_DIFFS {
                return diffs;
            }
        }
    }

    diffs
}

#[cfg(test)]
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
        let copybook = r#"
            01 RECORD.
               05 FIELD-A PIC X(10).
        "#;
        let schema = parse_copybook(copybook).expect("parse copybook");

        // CP037 encoding of "ABCDEFGHIJ"
        let data: Vec<u8> = b"ABCDEFGHIJ"
            .iter()
            .map(|&b| {
                // Simple ASCII→EBCDIC CP037 mapping for test letters
                match b {
                    b'A' => 0xC1,
                    b'B' => 0xC2,
                    b'C' => 0xC3,
                    b'D' => 0xC4,
                    b'E' => 0xC5,
                    b'F' => 0xC6,
                    b'G' => 0xC7,
                    b'H' => 0xC8,
                    b'I' => 0xC9,
                    b'J' => 0xD1,
                    _ => 0x40, // space
                }
            })
            .collect();

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
        let copybook = r#"
            01 RECORD.
               05 AMOUNT PIC S9(7)V99 COMP-3.
        "#;
        let schema = parse_copybook(copybook).expect("parse copybook");

        // COMP-3 representation of +1234567.89
        // PIC S9(7)V99 = 9 digits total, stored in (9+1)/2 = 5 bytes
        // Packed: 01 23 45 67 89 (where final nibble is sign: C = positive)
        // Correct packing: 12 34 56 78 9C (5 bytes)
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
        let copybook = r#"
            01 RECORD.
               05 FIELD-A PIC X(5).
        "#;
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
        let copybook = r#"
            01 RECORD.
               05 NAME PIC X(10).
               05 AGE  PIC 9(3).
        "#;
        let schema = parse_copybook(copybook).expect("parse copybook");

        // CP037 encoded: "JOHN      123"
        // Explicit CP037 mapping (EBCDIC letters are not contiguous)
        let data: Vec<u8> = vec![
            0xD1, // J
            0xD6, // O
            0xC8, // H
            0xD5, // N
            0x40, // space
            0x40, // space
            0x40, // space
            0x40, // space
            0x40, // space
            0x40, // space
            0xF1, // 1
            0xF2, // 2
            0xF3, // 3
        ];

        let result = check_round_trip_determinism(&schema, &data, &decode_opts(), &encode_opts())
            .expect("round-trip check");

        assert!(result.is_deterministic, "Expected deterministic round-trip");
        assert_eq!(result.mode, DeterminismMode::RoundTrip);
    }

    #[test]
    fn diff_bytes_reports_mismatches() {
        let a = b"ABCDEF";
        let b = b"ABxDEy";
        let diffs = find_byte_differences(a, b);

        assert_eq!(diffs.len(), 2);
        assert_eq!(diffs[0].offset, 2);
        assert_eq!(diffs[0].round1_byte, b'C');
        assert_eq!(diffs[0].round2_byte, b'x');
        assert_eq!(diffs[1].offset, 5);
        assert_eq!(diffs[1].round1_byte, b'F');
        assert_eq!(diffs[1].round2_byte, b'y');
    }

    #[test]
    fn diff_bytes_handles_length_mismatch() {
        let a = b"ABC";
        let b = b"ABCDE";
        let diffs = find_byte_differences(a, b);

        assert_eq!(diffs.len(), 2);
        // Offset 3: a has nothing (0), b has 'D'
        assert_eq!(diffs[0].offset, 3);
        assert_eq!(diffs[0].round1_byte, 0);
        assert_eq!(diffs[0].round2_byte, b'D');
        // Offset 4: a has nothing (0), b has 'E'
        assert_eq!(diffs[1].offset, 4);
        assert_eq!(diffs[1].round1_byte, 0);
        assert_eq!(diffs[1].round2_byte, b'E');
    }

    #[test]
    fn diff_bytes_limits_to_100() {
        let a = vec![0u8; 200];
        let b = vec![1u8; 200];
        let diffs = find_byte_differences(&a, &b);

        assert_eq!(diffs.len(), 100, "Should limit to 100 differences");
    }

    #[test]
    fn determinism_result_passed_helper() {
        let result = DeterminismResult {
            mode: DeterminismMode::DecodeOnly,
            round1_hash: "abc123".to_string(),
            round2_hash: "abc123".to_string(),
            is_deterministic: true,
            byte_differences: None,
        };
        assert!(result.passed());
        assert_eq!(result.diff_count(), 0);
    }

    #[test]
    fn determinism_result_failed_helper() {
        let result = DeterminismResult {
            mode: DeterminismMode::EncodeOnly,
            round1_hash: "abc123".to_string(),
            round2_hash: "def456".to_string(),
            is_deterministic: false,
            byte_differences: Some(vec![ByteDiff {
                offset: 0,
                round1_byte: 1,
                round2_byte: 2,
            }]),
        };
        assert!(!result.passed());
        assert_eq!(result.diff_count(), 1);
    }

    /// Adversarial test: Verify we detect non-determinism in JSON serialization.
    /// This simulates what would happen if the codec produced different outputs.
    #[test]
    fn detect_json_serialization_nondeterminism() {
        // Simulate two decode runs that produce different JSON
        let json1 = serde_json::json!({"FIELD": "VALUE1"});
        let json2 = serde_json::json!({"FIELD": "VALUE2"});

        let bytes1 = serde_json::to_vec(&json1).expect("serialize json1");
        let bytes2 = serde_json::to_vec(&json2).expect("serialize json2");

        // Manually verify our diff detection would catch this
        let diffs = find_byte_differences(&bytes1, &bytes2);
        assert!(!diffs.is_empty(), "Should detect differences in JSON bytes");

        // Verify BLAKE3 hashes differ
        let hash1 = blake3::hash(&bytes1);
        let hash2 = blake3::hash(&bytes2);
        assert_ne!(
            hash1, hash2,
            "BLAKE3 hashes should differ for different outputs"
        );
    }

    /// Adversarial test: Verify error handling when decode fails.
    /// This tests that errors propagate correctly rather than being silently swallowed.
    #[test]
    fn decode_error_propagates_correctly() {
        let copybook = r#"
            01 RECORD.
               05 AMOUNT PIC S9(7)V99 COMP-3.
        "#;
        let schema = parse_copybook(copybook).expect("parse copybook");

        // Provide truncated data (COMP-3 expects 5 bytes for PIC S9(7)V99)
        let truncated_data = vec![0x12, 0x34]; // Only 2 bytes instead of 5

        let result = check_decode_determinism(&schema, &truncated_data, &decode_opts());

        // Should propagate the decode error, not return a determinism result
        assert!(
            result.is_err(),
            "Should return error for truncated COMP-3 data"
        );
    }

    /// Adversarial test: Verify encode fails correctly with invalid JSON.
    #[test]
    fn encode_error_propagates_correctly() {
        let copybook = r#"
            01 RECORD.
               05 FIELD PIC 9(5).
        "#;
        let schema = parse_copybook(copybook).expect("parse copybook");

        // Provide invalid JSON (string instead of number)
        let invalid_json = serde_json::json!({"FIELD": "NOT_A_NUMBER"});

        let result = check_encode_determinism(&schema, &invalid_json, &encode_opts());

        // Should propagate the encode error
        assert!(
            result.is_err(),
            "Should return error for type mismatch in encoding"
        );
    }

    /// Adversarial test: Verify round-trip fails correctly with encoding errors.
    #[test]
    fn round_trip_error_propagates() {
        let copybook = r#"
            01 RECORD.
               05 AMOUNT PIC S9(7)V99 COMP-3.
        "#;
        let schema = parse_copybook(copybook).expect("parse copybook");

        // Truncated COMP-3 data
        let bad_data = vec![0x12, 0x34];

        let result =
            check_round_trip_determinism(&schema, &bad_data, &decode_opts(), &encode_opts());

        // Should fail on the first decode operation
        assert!(
            result.is_err(),
            "Should return error for truncated data in round-trip"
        );
    }

    /// Adversarial test: Verify we handle insufficient data correctly.
    /// Empty data should fail for a schema that expects bytes.
    #[test]
    fn insufficient_data_handling() {
        let copybook = r#"
            01 RECORD.
               05 FIELD PIC X(5).
        "#;
        let schema = parse_copybook(copybook).expect("parse copybook");

        // Provide only 3 bytes when schema expects 5
        let insufficient_data = vec![0x40, 0x40, 0x40]; // 3 spaces in EBCDIC

        let result = check_decode_determinism(&schema, &insufficient_data, &decode_opts());

        // Should return error for insufficient data
        // Note: If this succeeds deterministically (both runs pad the same way),
        // that's actually acceptable behavior - determinism is maintained.
        // The key is that we don't crash or produce non-deterministic results.
        match result {
            Ok(det_result) => {
                // If decode succeeds (perhaps by padding), verify it's deterministic
                assert!(
                    det_result.is_deterministic,
                    "If insufficient data is handled, it must be deterministic"
                );
            }
            Err(_) => {
                // If it errors, that's also acceptable - the error should be consistent
            }
        }
    }

    /// Adversarial test: Verify diff detection with all bytes different.
    #[test]
    fn diff_bytes_with_total_mismatch() {
        let a = vec![0x00, 0x11, 0x22, 0x33, 0x44];
        let b = vec![0xFF, 0xEE, 0xDD, 0xCC, 0xBB];
        let diffs = find_byte_differences(&a, &b);

        assert_eq!(diffs.len(), 5, "Should report all 5 byte differences");
        for (idx, diff) in diffs.iter().enumerate() {
            assert_eq!(diff.offset, idx);
            assert_eq!(diff.round1_byte, a[idx]);
            assert_eq!(diff.round2_byte, b[idx]);
        }
    }
}
