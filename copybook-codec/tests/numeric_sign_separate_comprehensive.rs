#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]
#![cfg(feature = "comprehensive-tests")]
//! Comprehensive tests for SIGN SEPARATE zoned decimal decoding
//!
//! This test suite validates the decode_zoned_decimal_sign_separate function
//! with all edge cases including:
//! - Leading and trailing sign placement
//! - ASCII and EBCDIC codepages
//! - Positive and negative signs
//! - Various sign byte representations
//! - Scale handling
//! - Error cases

use copybook_codec::numeric::decode_zoned_decimal_sign_separate;
use copybook_codec::options::Codepage;
use copybook_core::{ErrorCode, SignPlacement, SignSeparateInfo};

#[test]
fn test_sign_separate_ascii_leading_positive() {
    // ASCII with leading '+' sign
    let data = b"+123";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(
        data,
        3, // digits
        0, // scale
        &sign_info,
        Codepage::ASCII,
    )
    .expect("Should decode successfully");

    assert_eq!(result.to_string(), "123");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_ascii_leading_negative() {
    // ASCII with leading '-' sign
    let data = b"-456";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-456");
    assert!(result.is_negative());
}

#[test]
fn test_sign_separate_ascii_trailing_positive() {
    // ASCII with trailing '+' sign
    let data = b"789+";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "789");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_ascii_trailing_negative() {
    // ASCII with trailing '-' sign
    let data = b"012-";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-12");
    assert!(result.is_negative());
}

#[test]
fn test_sign_separate_ascii_space_positive() {
    // ASCII with space as positive sign
    let data = b" 345";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "345");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_ascii_zero_positive() {
    // ASCII with '0' as positive sign
    let data = b"0678";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "678");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_ebcdic_leading_positive() {
    // EBCDIC with leading positive sign (0x4E = '+')
    let data = &[0x4E, 0xF1, 0xF2, 0xF3]; // "+123" in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "123");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_ebcdic_leading_negative() {
    // EBCDIC with leading negative sign (0x60 = '-')
    let data = &[0x60, 0xF4, 0xF5, 0xF6]; // "-456" in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-456");
    assert!(result.is_negative());
}

#[test]
fn test_sign_separate_ebcdic_trailing_positive() {
    // EBCDIC with trailing positive sign
    let data = &[0xF7, 0xF8, 0xF9, 0x4E]; // "789+" in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "789");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_ebcdic_trailing_negative() {
    // EBCDIC with trailing negative sign
    let data = &[0xF0, 0xF1, 0xF2, 0x60]; // "012-" in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-12");
    assert!(result.is_negative());
}

#[test]
fn test_sign_separate_ebcdic_space_positive() {
    // EBCDIC with space as positive sign (0x40 = ' ')
    let data = &[0x40, 0xF3, 0xF4, 0xF5]; // " 345" in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "345");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_ebcdic_zero_positive() {
    // EBCDIC with '0' as positive sign (0xF0)
    let data = &[0xF0, 0xF6, 0xF7, 0xF8]; // "0678" in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "678");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_with_scale() {
    // Test with decimal places
    let data = b"+12345";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(
        data,
        5,
        2, // scale = 2 decimal places
        &sign_info,
        Codepage::ASCII,
    )
    .expect("Should decode successfully");

    assert_eq!(result.to_string(), "123.45");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_negative_with_scale() {
    // Test negative with decimal places
    let data = b"-98765";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 5, 2, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-987.65");
    assert!(result.is_negative());
}

#[test]
fn test_sign_separate_large_number() {
    // Values beyond i64 capacity should fail with a decode overflow error
    let data = b"+12345678901234567890";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 20, 0, &sign_info, Codepage::ASCII);
    assert!(result.is_err(), "Should fail on i64 overflow");
    if let Err(err) = result {
        assert_eq!(err.code, ErrorCode::CBKD410_ZONED_OVERFLOW);
    }
}

#[test]
fn test_sign_separate_zero_value() {
    // Test zero value
    let data = b"+000";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "0");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_negative_zero_normalizes() {
    // Test that -0 normalizes to 0
    let data = b"-000";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "0");
    assert!(!result.is_negative()); // Should normalize to positive zero
}

#[test]
fn test_sign_separate_invalid_length() {
    // Test with incorrect length
    let data = b"+12"; // Too short (should be 3 digits + 1 sign = 4 bytes)
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII);

    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code(),
        copybook_core::ErrorCode::CBKD301_RECORD_TOO_SHORT
    );
}

#[test]
fn test_sign_separate_invalid_ascii_sign() {
    // Test with invalid ASCII sign byte
    let data = b"*123"; // '*' is not a valid sign
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII);

    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code(),
        copybook_core::ErrorCode::CBKD411_ZONED_BAD_SIGN
    );
}

#[test]
fn test_sign_separate_invalid_ebcdic_sign() {
    // Test with invalid EBCDIC sign byte
    let data = &[0xFF, 0xF1, 0xF2, 0xF3]; // 0xFF is not a valid sign
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037);

    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code(),
        copybook_core::ErrorCode::CBKD411_ZONED_BAD_SIGN
    );
}

#[test]
fn test_sign_separate_invalid_ascii_digit() {
    // Test with invalid ASCII digit
    let data = b"+12A"; // 'A' is not a valid digit
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII);

    assert!(result.is_err());
}

#[test]
fn test_sign_separate_invalid_ebcdic_digit() {
    // Test with invalid EBCDIC digit
    let data = &[0x4E, 0xF1, 0xF2, 0xC1]; // 0xC1 is 'A' in EBCDIC, not a digit
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037);

    assert!(result.is_err());
}

#[test]
fn test_sign_separate_cp1047_ebcdic() {
    // Test with CP1047 codepage (another EBCDIC variant)
    let data = &[0x4E, 0xF1, 0xF2, 0xF3]; // "+123" in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP1047)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "123");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_cp500_ebcdic() {
    // Test with CP500 codepage (another EBCDIC variant)
    let data = &[0x60, 0xF9, 0xF8, 0xF7]; // "-987" in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP500)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-987");
    assert!(result.is_negative());
}

#[test]
fn test_sign_separate_all_nines() {
    // Test with all nines (boundary value)
    let data = b"+999";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "999");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_all_zeros_with_scale() {
    // Test with all zeros and scale
    let data = b"+00000";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 5, 2, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "0.00");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_negative_scale() {
    // Test with negative scale (multiplier)
    let data = b"+12345";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(
        data,
        5,
        -2, // negative scale means multiply by 100
        &sign_info,
        Codepage::ASCII,
    )
    .expect("Should decode successfully");

    assert_eq!(result.to_string(), "1234500");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_single_digit() {
    // Test with single digit
    let data = b"+9";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 1, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "9");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_single_digit_negative() {
    // Test with single negative digit
    let data = b"-7";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 1, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "-7");
    assert!(result.is_negative());
}

#[test]
fn test_sign_separate_trailing_space_positive() {
    // Test trailing space as positive sign
    let data = b"123 ";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::ASCII)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "123");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_ebcdic_trailing_space() {
    // Test EBCDIC trailing space
    let data = &[0xF1, 0xF2, 0xF3, 0x40]; // "123 " in EBCDIC
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };

    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &sign_info, Codepage::CP037)
        .expect("Should decode successfully");

    assert_eq!(result.to_string(), "123");
    assert!(!result.is_negative());
}

#[test]
fn test_sign_separate_empty_data() {
    // Test with empty data
    let data = b"";
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };

    let result = decode_zoned_decimal_sign_separate(data, 0, 0, &sign_info, Codepage::ASCII);

    // Empty data should fail because SIGN SEPARATE requires at least a sign byte
    assert!(result.is_err());
}
