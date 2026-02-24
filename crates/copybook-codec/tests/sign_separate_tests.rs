#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]
//! Tests for SIGN SEPARATE decode dispatch and encode functionality.
//!
//! Validates:
//! - Decode dispatch wiring (sign_separate fields route to dedicated decoder)
//! - Encode function for SIGN SEPARATE zoned decimals
//! - Round-trip fidelity (decode -> encode -> match original binary)
//! - Error cases

use copybook_codec::numeric::{
    decode_zoned_decimal_sign_separate, encode_zoned_decimal_sign_separate,
};
use copybook_codec::options::Codepage;
use copybook_core::{SignPlacement, SignSeparateInfo};

// =============================================================================
// Decode tests (unit-level, verifying the function works correctly)
// =============================================================================

#[test]
fn test_decode_ebcdic_leading_positive() {
    // EBCDIC '+' = 0x4E, digits 0xF1 0xF2 0xF3
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "123");
    assert!(!result.is_negative());
}

#[test]
fn test_decode_ebcdic_leading_negative() {
    // EBCDIC '-' = 0x60, digits 0xF4 0xF5 0xF6
    let data: &[u8] = &[0x60, 0xF4, 0xF5, 0xF6];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-456");
    assert!(result.is_negative());
}

#[test]
fn test_decode_ebcdic_trailing_positive() {
    // digits 0xF7 0xF8 0xF9, EBCDIC '+' = 0x4E
    let data: &[u8] = &[0xF7, 0xF8, 0xF9, 0x4E];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "789");
    assert!(!result.is_negative());
}

#[test]
fn test_decode_ebcdic_trailing_negative() {
    // digits 0xF1 0xF0 0xF0, EBCDIC '-' = 0x60
    let data: &[u8] = &[0xF1, 0xF0, 0xF0, 0x60];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-100");
    assert!(result.is_negative());
}

#[test]
fn test_decode_with_scale() {
    // EBCDIC '+' = 0x4E, digits 0xF1 0xF2 0xF3 0xF4 0xF5
    // scale=2 means value is 123.45
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 5, 2, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "123.45");
}

// =============================================================================
// Encode tests
// =============================================================================

#[test]
fn test_encode_ebcdic_leading_positive() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buffer = vec![0u8; 4]; // 3 digits + 1 sign

    encode_zoned_decimal_sign_separate("123", 3, 0, &sign_info, Codepage::CP037, &mut buffer)
        .expect("Should encode successfully");

    // EBCDIC '+' = 0x4E, 0xF1, 0xF2, 0xF3
    assert_eq!(buffer, vec![0x4E, 0xF1, 0xF2, 0xF3]);
}

#[test]
fn test_encode_ebcdic_leading_negative() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buffer = vec![0u8; 4];

    encode_zoned_decimal_sign_separate("-456", 3, 0, &sign_info, Codepage::CP037, &mut buffer)
        .expect("Should encode successfully");

    // EBCDIC '-' = 0x60, 0xF4, 0xF5, 0xF6
    assert_eq!(buffer, vec![0x60, 0xF4, 0xF5, 0xF6]);
}

#[test]
fn test_encode_ebcdic_trailing_positive() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let mut buffer = vec![0u8; 4];

    encode_zoned_decimal_sign_separate("789", 3, 0, &sign_info, Codepage::CP037, &mut buffer)
        .expect("Should encode successfully");

    // 0xF7, 0xF8, 0xF9, EBCDIC '+' = 0x4E
    assert_eq!(buffer, vec![0xF7, 0xF8, 0xF9, 0x4E]);
}

#[test]
fn test_encode_ebcdic_trailing_negative() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let mut buffer = vec![0u8; 4];

    encode_zoned_decimal_sign_separate("-100", 3, 0, &sign_info, Codepage::CP037, &mut buffer)
        .expect("Should encode successfully");

    // 0xF1, 0xF0, 0xF0, EBCDIC '-' = 0x60
    assert_eq!(buffer, vec![0xF1, 0xF0, 0xF0, 0x60]);
}

#[test]
fn test_encode_with_scale() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buffer = vec![0u8; 6]; // 5 digits + 1 sign

    encode_zoned_decimal_sign_separate("123.45", 5, 2, &sign_info, Codepage::CP037, &mut buffer)
        .expect("Should encode successfully");

    // EBCDIC '+' = 0x4E, digits: 0xF1 0xF2 0xF3 0xF4 0xF5
    assert_eq!(buffer, vec![0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5]);
}

#[test]
fn test_encode_with_scale_negative() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let mut buffer = vec![0u8; 6]; // 5 digits + 1 sign

    encode_zoned_decimal_sign_separate("-123.45", 5, 2, &sign_info, Codepage::CP037, &mut buffer)
        .expect("Should encode successfully");

    // digits: 0xF1 0xF2 0xF3 0xF4 0xF5, EBCDIC '-' = 0x60
    assert_eq!(buffer, vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0x60]);
}

#[test]
fn test_encode_leading_zero_padding() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buffer = vec![0u8; 6]; // 5 digits + 1 sign

    encode_zoned_decimal_sign_separate("42", 5, 0, &sign_info, Codepage::CP037, &mut buffer)
        .expect("Should encode successfully");

    // EBCDIC '+' = 0x4E, 0xF0 0xF0 0xF0 0xF4 0xF2
    assert_eq!(buffer, vec![0x4E, 0xF0, 0xF0, 0xF0, 0xF4, 0xF2]);
}

#[test]
fn test_encode_ascii_leading_positive() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buffer = vec![0u8; 4];

    encode_zoned_decimal_sign_separate("123", 3, 0, &sign_info, Codepage::ASCII, &mut buffer)
        .expect("Should encode successfully");

    assert_eq!(buffer, b"+123".to_vec());
}

#[test]
fn test_encode_ascii_trailing_negative() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let mut buffer = vec![0u8; 4];

    encode_zoned_decimal_sign_separate("-456", 3, 0, &sign_info, Codepage::ASCII, &mut buffer)
        .expect("Should encode successfully");

    assert_eq!(buffer, b"456-".to_vec());
}

// =============================================================================
// Round-trip tests (decode binary -> value -> encode back -> match original)
// =============================================================================

#[test]
fn test_roundtrip_ebcdic_leading_positive() {
    let original: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    // Decode
    let decimal = decode_zoned_decimal_sign_separate(original, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode");

    // Encode back
    let mut encoded = vec![0u8; 4];
    encode_zoned_decimal_sign_separate(
        &decimal.to_string(),
        3,
        0,
        &sign_info,
        Codepage::CP037,
        &mut encoded,
    )
    .expect("Should encode");

    assert_eq!(original, encoded.as_slice());
}

#[test]
fn test_roundtrip_ebcdic_trailing_negative() {
    let original: &[u8] = &[0xF4, 0xF5, 0xF6, 0x60];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    // Decode
    let decimal = decode_zoned_decimal_sign_separate(original, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode");

    // Encode back
    let mut encoded = vec![0u8; 4];
    encode_zoned_decimal_sign_separate(
        &decimal.to_string(),
        3,
        0,
        &sign_info,
        Codepage::CP037,
        &mut encoded,
    )
    .expect("Should encode");

    assert_eq!(original, encoded.as_slice());
}

#[test]
fn test_roundtrip_with_scale() {
    let original: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    // Decode (scale=2 -> "123.45")
    let decimal = decode_zoned_decimal_sign_separate(original, 5, 2, &sign_info, Codepage::CP037)
        .expect("Should decode");

    assert_eq!(decimal.to_string(), "123.45");

    // Encode back
    let mut encoded = vec![0u8; 6];
    encode_zoned_decimal_sign_separate(
        &decimal.to_string(),
        5,
        2,
        &sign_info,
        Codepage::CP037,
        &mut encoded,
    )
    .expect("Should encode");

    assert_eq!(original, encoded.as_slice());
}

#[test]
fn test_roundtrip_ascii_leading_negative() {
    let original: &[u8] = b"-789";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let decimal = decode_zoned_decimal_sign_separate(original, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode");
    assert_eq!(decimal.to_string(), "-789");

    let mut encoded = vec![0u8; 4];
    encode_zoned_decimal_sign_separate(
        &decimal.to_string(),
        3,
        0,
        &sign_info,
        Codepage::ASCII,
        &mut encoded,
    )
    .expect("Should encode");

    assert_eq!(original, encoded.as_slice());
}

// =============================================================================
// Error tests
// =============================================================================

#[test]
fn test_decode_record_too_short() {
    let data: &[u8] = &[0x4E, 0xF1]; // Only 2 bytes, need 4 (3 digits + sign)
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, copybook_core::ErrorCode::CBKD301_RECORD_TOO_SHORT);
}

#[test]
fn test_encode_buffer_too_small() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buffer = vec![0u8; 2]; // Too small for 3 digits + 1 sign

    let result =
        encode_zoned_decimal_sign_separate("123", 3, 0, &sign_info, Codepage::CP037, &mut buffer);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        copybook_core::ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR
    );
}

#[test]
fn test_encode_zero_value() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buffer = vec![0u8; 4];

    encode_zoned_decimal_sign_separate("0", 3, 0, &sign_info, Codepage::CP037, &mut buffer)
        .expect("Should encode zero");

    // EBCDIC '+' = 0x4E, 0xF0 0xF0 0xF0
    assert_eq!(buffer, vec![0x4E, 0xF0, 0xF0, 0xF0]);
}

#[test]
fn test_roundtrip_ebcdic_cp500() {
    // Verify with a different EBCDIC codepage
    let original: &[u8] = &[0x4E, 0xF5, 0xF0, 0xF0];
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let decimal = decode_zoned_decimal_sign_separate(original, 3, 0, &sign_info, Codepage::CP500)
        .expect("Should decode");
    assert_eq!(decimal.to_string(), "500");

    let mut encoded = vec![0u8; 4];
    encode_zoned_decimal_sign_separate(
        &decimal.to_string(),
        3,
        0,
        &sign_info,
        Codepage::CP500,
        &mut encoded,
    )
    .expect("Should encode");

    assert_eq!(original, encoded.as_slice());
}
