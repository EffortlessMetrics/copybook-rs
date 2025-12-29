//! Phase E3.1: Edited PIC encode comprehensive tests
//!
//! Tests minimal edited PIC encode functionality for copybook-rs v0.5.0 milestone.
//!
//! ## E3.1 Scope
//!
//! The following edited PIC patterns are supported in E3.1:
//!
//! ### Supported Tokens (E3.1)
//! - **Digit (9)**: Always displays digit
//! - **ZeroSuppress (Z)**: Displays space if leading zero, otherwise displays digit
//! - **ZeroInsert (0)**: Always displays digit (used for padding with leading zeros)
//! - **DecimalPoint (.)**: Decimal separator
//! - **LeadingPlus (+)**: Leading plus sign (+ for positive, - for negative)
//! - **LeadingMinus (-)**: Leading minus sign (space for positive, - for negative)
//!
//! ### Explicitly Out of Scope (Future E3.x)
//! - **AsteriskFill (*)**: Check-protect with asterisks (E3.2)
//! - **Space (B)**: Blank space insertion (E3.2)
//! - **Comma (,)**: Thousands separator (E3.2)
//! - **Slash (/)**: Date separator (E3.2)
//! - **Currency ($)**: Currency symbol (E3.2)
//! - **TrailingPlus (+)**: Trailing plus sign (E3.2)
//! - **TrailingMinus (-)**: Trailing minus sign (E3.2)
//! - **Credit (CR)**: Credit indicator (E3.2)
//! - **Debit (DB)**: Debit indicator (E3.2)
//!
//! ## Test Coverage
//!
//! This test file covers:
//! - Basic digit patterns (9, 99, 999)
//! - Zero suppression patterns (Z, ZZ, ZZZ)
//! - Zero insert patterns (0, 00, 000)
//! - Decimal patterns (9.9, ZZ.99, 00.00)
//! - Sign patterns (+9, -9, +ZZ9, -ZZ9)
//! - Edge cases: zero values, negative values, overflow, underflow
//! - Mixed patterns (ZZ9.99, +00.00)
//! - Error handling for unsupported tokens

use copybook_codec::{edited_pic::encode_edited_numeric, edited_pic::tokenize_edited_pic};
use copybook_core::ErrorCode;

/// Helper function to test encode with pattern
fn test_encode(pattern_str: &str, value: &str, scale: u16, expected: &str) {
    let pattern = tokenize_edited_pic(pattern_str).unwrap();
    let result = encode_edited_numeric(value, &pattern, scale, false).unwrap();
    assert_eq!(
        result.len(),
        pattern.len(),
        "Output width must equal pattern width"
    );
    assert_eq!(
        expected.len(),
        pattern.len(),
        "Expected width must equal pattern width"
    );
    assert_eq!(
        result, expected,
        "Pattern: {}, Value: {}",
        pattern_str, value
    );
}

/// Helper function to test encode error
fn test_encode_error(pattern_str: &str, value: &str, scale: u16, expected_error: ErrorCode) {
    let pattern = tokenize_edited_pic(pattern_str).unwrap();
    let result = encode_edited_numeric(value, &pattern, scale, false);
    assert!(
        result.is_err(),
        "Expected error for Pattern: {}, Value: {}",
        pattern_str,
        value
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code, expected_error,
        "Pattern: {}, Value: {}",
        pattern_str, value
    );
}

// ===== Basic Digit Patterns =====

#[test]
fn test_e3_1_basic_digit_9() {
    test_encode("9", "5", 0, "5");
}

#[test]
fn test_e3_1_basic_digit_99() {
    test_encode("99", "42", 0, "42");
}

#[test]
fn test_e3_1_basic_digit_999() {
    test_encode("999", "123", 0, "123");
}

#[test]
fn test_e3_1_basic_digit_9999() {
    test_encode("9999", "1234", 0, "1234");
}

#[test]
fn test_e3_1_basic_digit_zero() {
    test_encode("999", "0", 0, "000");
}

#[test]
fn test_e3_1_basic_digit_negative() {
    // Digit patterns don't support signs, so negative values are encoded as positive
    test_encode("999", "-123", 0, "123");
}

// ===== Zero Suppression Patterns =====

#[test]
fn test_e3_1_zero_suppress_z() {
    test_encode("Z", "5", 0, "5");
}

#[test]
fn test_e3_1_zero_suppress_zz() {
    test_encode("ZZ", "42", 0, "42");
}

#[test]
fn test_e3_1_zero_suppress_zzz() {
    test_encode("ZZZ", "123", 0, "123");
}

#[test]
fn test_e3_1_zero_suppress_zzz9() {
    test_encode("ZZZ9", "123", 0, " 123");
}

#[test]
fn test_e3_1_zero_suppress_zzz9_zero() {
    test_encode("ZZZ9", "0", 0, "   0");
}

#[test]
fn test_e3_1_zero_suppress_zzz9_single_digit() {
    test_encode("ZZZ9", "1", 0, "   1");
}

#[test]
fn test_e3_1_zero_suppress_zzz9_two_digits() {
    test_encode("ZZZ9", "12", 0, "  12");
}

#[test]
fn test_e3_1_zero_suppress_zzz9_full() {
    test_encode("ZZZ9", "1234", 0, "1234");
}

#[test]
fn test_e3_1_zero_suppress_zzzzz() {
    test_encode("ZZZZZ", "123", 0, "  123");
}

#[test]
fn test_e3_1_zero_suppress_zzzzz_zero() {
    test_encode("ZZZZZ", "0", 0, "    0");
}

// ===== Zero Insert Patterns =====

#[test]
fn test_e3_1_zero_insert_0() {
    test_encode("0", "5", 0, "5");
}

#[test]
fn test_e3_1_zero_insert_00() {
    test_encode("00", "42", 0, "42");
}

#[test]
fn test_e3_1_zero_insert_000() {
    test_encode("000", "123", 0, "123");
}

#[test]
fn test_e3_1_zero_insert_0009() {
    test_encode("0009", "123", 0, "0123");
}

#[test]
fn test_e3_1_zero_insert_0009_zero() {
    test_encode("0009", "0", 0, "0000");
}

#[test]
fn test_e3_1_zero_insert_0009_single_digit() {
    test_encode("0009", "1", 0, "0001");
}

#[test]
fn test_e3_1_zero_insert_0009_two_digits() {
    test_encode("0009", "12", 0, "0012");
}

#[test]
fn test_e3_1_zero_insert_0009_full() {
    test_encode("0009", "1234", 0, "1234");
}

#[test]
fn test_e3_1_zero_insert_00000() {
    test_encode("00000", "123", 0, "00123");
}

#[test]
fn test_e3_1_zero_insert_00000_zero() {
    test_encode("00000", "0", 0, "00000");
}

// ===== Decimal Patterns =====

#[test]
fn test_e3_1_decimal_9_9() {
    test_encode("9.9", "5.5", 1, "5.5");
}

#[test]
fn test_e3_1_decimal_99_99() {
    test_encode("99.99", "12.34", 2, "12.34");
}

#[test]
fn test_e3_1_decimal_999_99() {
    test_encode("999.99", "123.45", 2, "123.45");
}

#[test]
fn test_e3_1_decimal_zz_99() {
    test_encode("ZZ.99", "12.34", 2, "12.34");
}

#[test]
fn test_e3_1_decimal_zz_99_zero() {
    test_encode("ZZ.99", "0.00", 2, " 0.00");
}

#[test]
fn test_e3_1_decimal_zz_99_single_digit() {
    test_encode("ZZ.99", "1.23", 2, " 1.23");
}

#[test]
fn test_e3_1_decimal_00_00() {
    test_encode("00.00", "12.34", 2, "12.34");
}

#[test]
fn test_e3_1_decimal_00_00_zero() {
    test_encode("00.00", "0.00", 2, "00.00");
}

#[test]
fn test_e3_1_decimal_00_00_single_digit() {
    test_encode("00.00", "1.23", 2, "01.23");
}

#[test]
fn test_e3_1_decimal_9_9_zero() {
    test_encode("9.9", "0.0", 1, "0.0");
}

#[test]
fn test_e3_1_decimal_zzz9_99() {
    test_encode("ZZZ9.99", "123.45", 2, " 123.45");
}

#[test]
fn test_e3_1_decimal_zzz9_99_zero() {
    test_encode("ZZZ9.99", "0.00", 2, "   0.00");
}

#[test]
fn test_e3_1_decimal_zzz9_99_single_digit() {
    test_encode("ZZZ9.99", "1.23", 2, "   1.23");
}

#[test]
fn test_e3_1_decimal_0009_99() {
    test_encode("0009.99", "123.45", 2, "0123.45");
}

#[test]
fn test_e3_1_decimal_0009_99_zero() {
    test_encode("0009.99", "0.00", 2, "0000.00");
}

// ===== Sign Patterns =====

#[test]
fn test_e3_1_sign_plus_9_positive() {
    test_encode("+9", "5", 0, "+5");
}

#[test]
fn test_e3_1_sign_plus_9_negative() {
    test_encode("+9", "-5", 0, "-5");
}

#[test]
fn test_e3_1_sign_plus_9_zero() {
    test_encode("+9", "0", 0, "+0");
}

#[test]
fn test_e3_1_sign_plus_999_positive() {
    test_encode("+999", "123", 0, "+123");
}

#[test]
fn test_e3_1_sign_plus_999_negative() {
    test_encode("+999", "-123", 0, "-123");
}

#[test]
fn test_e3_1_sign_minus_9_positive() {
    test_encode("-9", "5", 0, " 5");
}

#[test]
fn test_e3_1_sign_minus_9_negative() {
    test_encode("-9", "-5", 0, "-5");
}

#[test]
fn test_e3_1_sign_minus_9_zero() {
    test_encode("-9", "0", 0, " 0");
}

#[test]
fn test_e3_1_sign_minus_999_positive() {
    test_encode("-999", "123", 0, " 123");
}

#[test]
fn test_e3_1_sign_minus_999_negative() {
    test_encode("-999", "-123", 0, "-123");
}

#[test]
fn test_e3_1_sign_plus_zz9_positive() {
    test_encode("+ZZ9", "123", 0, "+123");
}

#[test]
fn test_e3_1_sign_plus_zz9_negative() {
    test_encode("+ZZ9", "-123", 0, "-123");
}

#[test]
fn test_e3_1_sign_plus_zz9_zero() {
    test_encode("+ZZ9", "0", 0, "+  0");
}

#[test]
fn test_e3_1_sign_minus_zz9_positive() {
    test_encode("-ZZ9", "123", 0, " 123");
}

#[test]
fn test_e3_1_sign_minus_zz9_negative() {
    test_encode("-ZZ9", "-123", 0, "-123");
}

#[test]
fn test_e3_1_sign_minus_zz9_zero() {
    test_encode("-ZZ9", "0", 0, "   0");
}

#[test]
fn test_e3_1_sign_plus_decimal_positive() {
    test_encode("+99.99", "12.34", 2, "+12.34");
}

#[test]
fn test_e3_1_sign_plus_decimal_negative() {
    test_encode("+99.99", "-12.34", 2, "-12.34");
}

#[test]
fn test_e3_1_sign_minus_decimal_positive() {
    test_encode("-99.99", "12.34", 2, " 12.34");
}

#[test]
fn test_e3_1_sign_minus_decimal_negative() {
    test_encode("-99.99", "-12.34", 2, "-12.34");
}

// ===== Mixed Patterns =====

#[test]
fn test_e3_1_mixed_zz9_99() {
    test_encode("ZZ9.99", "123.45", 2, "123.45");
}

#[test]
fn test_e3_1_mixed_zz9_99_zero() {
    test_encode("ZZ9.99", "0.00", 2, "  0.00");
}

#[test]
fn test_e3_1_mixed_zz9_99_single_digit() {
    test_encode("ZZ9.99", "1.23", 2, "  1.23");
}

#[test]
fn test_e3_1_mixed_plus_00_00() {
    test_encode("+00.00", "12.34", 2, "+12.34");
}

#[test]
fn test_e3_1_mixed_plus_00_00_zero() {
    test_encode("+00.00", "0.00", 2, "+00.00");
}

#[test]
fn test_e3_1_mixed_minus_00_00() {
    test_encode("-00.00", "12.34", 2, " 12.34");
}

#[test]
fn test_e3_1_mixed_minus_00_00_zero() {
    test_encode("-00.00", "0.00", 2, " 00.00");
}

#[test]
fn test_e3_1_mixed_zzz9_99_with_sign() {
    test_encode("+ZZZ9.99", "123.45", 2, "+ 123.45");
}

#[test]
fn test_e3_1_mixed_zzz9_99_with_minus() {
    test_encode("-ZZZ9.99", "123.45", 2, "  123.45");
}

// ===== Edge Cases =====

#[test]
fn test_e3_1_edge_zero_value_digit() {
    test_encode("999", "0", 0, "000");
}

#[test]
fn test_e3_1_edge_zero_value_z() {
    test_encode("ZZZ", "0", 0, "  0");
}

#[test]
fn test_e3_1_edge_zero_value_0() {
    test_encode("000", "0", 0, "000");
}

#[test]
fn test_e3_1_edge_negative_zero_forces_positive() {
    test_encode("-999", "-0", 0, " 000");
}

#[test]
fn test_e3_1_edge_negative_zero_forces_positive_plus() {
    test_encode("+999", "-0", 0, "+000");
}

#[test]
fn test_e3_1_edge_overflow_value_too_long() {
    test_encode_error(
        "999",
        "1234",
        0,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
    );
}

#[test]
fn test_e3_1_edge_overflow_value_too_long_decimal() {
    test_encode_error(
        "99.99",
        "123.45",
        2,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
    );
}

#[test]
fn test_e3_1_edge_underflow_fits_in_pattern() {
    test_encode("9999", "123", 0, "0123");
}

#[test]
fn test_e3_1_edge_underflow_fits_in_pattern_z() {
    test_encode("ZZZZ", "123", 0, " 123");
}

#[test]
fn test_e3_1_edge_large_value() {
    test_encode("999999", "123456", 0, "123456");
}

#[test]
fn test_e3_1_edge_large_value_with_decimal() {
    test_encode("999999.99", "123456.78", 2, "123456.78");
}

#[test]
fn test_e3_1_edge_max_single_digit() {
    test_encode("9", "9", 0, "9");
}

#[test]
fn test_e3_1_edge_max_single_digit_z() {
    test_encode("Z", "9", 0, "9");
}

#[test]
fn test_e3_1_edge_max_single_digit_0() {
    test_encode("0", "9", 0, "9");
}

// ===== Error Handling for Unsupported Tokens =====

#[test]
fn test_e3_1_error_asterisk_fill() {
    test_encode_error(
        "999*",
        "123",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_space() {
    test_encode_error(
        "999B",
        "123",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_comma() {
    test_encode_error(
        "999,999",
        "123456",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_slash() {
    test_encode_error(
        "99/99",
        "1234",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_currency() {
    test_encode_error(
        "$999",
        "123",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_trailing_plus() {
    test_encode_error(
        "999+",
        "123",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_trailing_minus() {
    test_encode_error(
        "999-",
        "123",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_credit() {
    test_encode_error(
        "999CR",
        "123",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_debit() {
    test_encode_error(
        "999DB",
        "123",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

#[test]
fn test_e3_1_error_complex_unsupported() {
    test_encode_error(
        "$ZZ,ZZZ.99CR",
        "123456",
        2,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

// ===== Additional Edge Cases =====

#[test]
fn test_e3_1_edge_empty_string() {
    test_encode_error("999", "", 0, ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT);
}

#[test]
fn test_e3_1_edge_invalid_character() {
    test_encode_error(
        "999",
        "12a",
        0,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
    );
}

#[test]
fn test_e3_1_edge_multiple_decimals_in_value() {
    test_encode_error(
        "999",
        "12.34.56",
        0,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
    );
}

#[test]
fn test_e3_1_edge_scale_mismatch_truncate() {
    // Value has more decimal places than scale, should truncate
    test_encode("99.99", "12.3456", 2, "12.34");
}

#[test]
fn test_e3_1_edge_scale_mismatch_pad() {
    // Value has fewer decimal places than scale, should pad
    test_encode("99.99", "12.3", 2, "12.30");
}

#[test]
fn test_e3_1_edge_scale_mismatch_pad_zero() {
    // Value has no decimal places but scale requires them
    test_encode("99.99", "12", 2, "12.00");
}
