//! Phase E3.1/E3.2: Edited PIC encode comprehensive tests
//!
//! Tests edited PIC encode functionality for copybook-rs v0.5.0 milestone.
//!
//! ## E3.1 Scope (Complete)
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
//! ## E3.2 Scope (Complete)
//!
//! ### Supported Tokens (E3.2)
//! - **TrailingPlus (+)**: Trailing plus sign (+ for positive, - for negative)
//! - **TrailingMinus (-)**: Trailing minus sign (space for positive, - for negative)
//!
//! ## E3.5 Scope (Complete)
//!
//! ### Supported Tokens (E3.5)
//! - **AsteriskFill (*)**: Check-protect with asterisks (fills leading zeros with '*')
//!
//! ## E3.4 Scope (Complete)
//!
//! ### Supported Tokens (E3.4)
//! - **Comma (,)**: Thousands separator
//! - **Slash (/)**: Date separator
//!
//! ## E3.6 Scope (Complete)
//!
//! ### Supported Tokens (E3.6)
//! - **Currency ($)**: Currency symbol (fixed position)
//!
//! ### Explicitly Out of Scope (Future E3.x)
//! - **Space (B)**: Blank space insertion (E3.7+)
//! - **Credit (CR)**: Credit indicator (E3.7+)
//! - **Debit (DB)**: Debit indicator (E3.7+)
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
        expected.len(),
        "Output width must match expected (pattern: {}, got: {}, expected: {})",
        pattern_str,
        result,
        expected
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
fn test_e3_1_error_space() {
    test_encode_error(
        "999B",
        "123",
        0,
        ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
    );
}

// ===== E3.2: Trailing Sign Tests =====

#[test]
fn test_e3_2_trailing_plus_positive() {
    test_encode("999+", "123", 0, "123+");
}

#[test]
fn test_e3_2_trailing_plus_negative() {
    test_encode("999+", "-123", 0, "123-");
}

#[test]
fn test_e3_2_trailing_plus_zero() {
    test_encode("999+", "0", 0, "000+");
}

#[test]
fn test_e3_2_trailing_minus_positive() {
    test_encode("999-", "123", 0, "123 ");
}

#[test]
fn test_e3_2_trailing_minus_negative() {
    test_encode("999-", "-123", 0, "123-");
}

#[test]
fn test_e3_2_trailing_minus_zero() {
    test_encode("999-", "0", 0, "000 ");
}

#[test]
fn test_e3_2_trailing_plus_with_z() {
    test_encode("ZZZ9+", "123", 0, " 123+");
}

#[test]
fn test_e3_2_trailing_plus_with_z_negative() {
    test_encode("ZZZ9+", "-123", 0, " 123-");
}

#[test]
fn test_e3_2_trailing_plus_with_z_zero() {
    test_encode("ZZZ9+", "0", 0, "   0+");
}

#[test]
fn test_e3_2_trailing_minus_with_z() {
    test_encode("ZZZ9-", "123", 0, " 123 ");
}

#[test]
fn test_e3_2_trailing_minus_with_z_negative() {
    test_encode("ZZZ9-", "-123", 0, " 123-");
}

#[test]
fn test_e3_2_trailing_minus_with_z_zero() {
    test_encode("ZZZ9-", "0", 0, "   0 ");
}

#[test]
fn test_e3_2_trailing_plus_with_decimal() {
    test_encode("99.99+", "12.34", 2, "12.34+");
}

#[test]
fn test_e3_2_trailing_plus_with_decimal_negative() {
    test_encode("99.99+", "-12.34", 2, "12.34-");
}

#[test]
fn test_e3_2_trailing_minus_with_decimal() {
    test_encode("99.99-", "12.34", 2, "12.34 ");
}

#[test]
fn test_e3_2_trailing_minus_with_decimal_negative() {
    test_encode("99.99-", "-12.34", 2, "12.34-");
}

#[test]
fn test_e3_2_trailing_plus_with_zero_insert() {
    test_encode("0009+", "123", 0, "0123+");
}

#[test]
fn test_e3_2_trailing_minus_with_zero_insert() {
    test_encode("0009-", "123", 0, "0123 ");
}

#[test]
fn test_e3_2_trailing_sign_negative_zero_forces_positive() {
    test_encode("999+", "-0", 0, "000+");
}

#[test]
fn test_e3_2_trailing_minus_negative_zero_forces_positive() {
    test_encode("999-", "-0", 0, "000 ");
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

// ===== E3.3: CR/DB (Credit/Debit) Tests =====

#[test]
fn test_e3_3_credit_positive() {
    test_encode("999CR", "123", 0, "123  ");
}

#[test]
fn test_e3_3_credit_negative() {
    test_encode("999CR", "-123", 0, "123CR");
}

#[test]
fn test_e3_3_credit_zero() {
    test_encode("999CR", "0", 0, "000  ");
}

#[test]
fn test_e3_3_debit_positive() {
    test_encode("999DB", "123", 0, "123  ");
}

#[test]
fn test_e3_3_debit_negative() {
    test_encode("999DB", "-123", 0, "123DB");
}

#[test]
fn test_e3_3_debit_zero() {
    test_encode("999DB", "0", 0, "000  ");
}

#[test]
fn test_e3_3_credit_with_zero_suppress_positive() {
    test_encode("ZZZ9CR", "123", 0, " 123  ");
}

#[test]
fn test_e3_3_credit_with_zero_suppress_negative() {
    test_encode("ZZZ9CR", "-123", 0, " 123CR");
}

#[test]
fn test_e3_3_credit_with_zero_suppress_zero() {
    test_encode("ZZZ9CR", "0", 0, "   0  ");
}

#[test]
fn test_e3_3_debit_with_zero_suppress_negative() {
    test_encode("ZZZ9DB", "-123", 0, " 123DB");
}

#[test]
fn test_e3_3_debit_with_zero_suppress_positive() {
    test_encode("ZZZ9DB", "123", 0, " 123  ");
}

#[test]
fn test_e3_3_credit_with_zero_insert_positive() {
    test_encode("0009CR", "123", 0, "0123  ");
}

#[test]
fn test_e3_3_credit_with_zero_insert_negative() {
    test_encode("0009CR", "-123", 0, "0123CR");
}

#[test]
fn test_e3_3_debit_with_zero_insert_negative() {
    test_encode("0009DB", "-123", 0, "0123DB");
}

#[test]
fn test_e3_3_debit_with_zero_insert_positive() {
    test_encode("0009DB", "123", 0, "0123  ");
}

#[test]
fn test_e3_3_credit_with_decimal_positive() {
    test_encode("99.99CR", "12.34", 2, "12.34  ");
}

#[test]
fn test_e3_3_credit_with_decimal_negative() {
    test_encode("99.99CR", "-12.34", 2, "12.34CR");
}

#[test]
fn test_e3_3_debit_with_decimal_negative() {
    test_encode("99.99DB", "-12.34", 2, "12.34DB");
}

#[test]
fn test_e3_3_credit_negative_zero_forces_positive() {
    test_encode("999CR", "-0", 0, "000  ");
}

#[test]
fn test_e3_3_debit_negative_zero_forces_positive() {
    test_encode("999DB", "-0", 0, "000  ");
}

// ===== E3.5: Asterisk Fill (Check Protection) Tests =====

#[test]
fn test_e3_5_asterisk_basic_star9() {
    test_encode("***9", "123", 0, "*123");
}

#[test]
fn test_e3_5_asterisk_basic_star9_single_digit() {
    test_encode("***9", "5", 0, "***5");
}

#[test]
fn test_e3_5_asterisk_basic_star9_full_width() {
    test_encode("***9", "1234", 0, "1234");
}

#[test]
fn test_e3_5_asterisk_all_zero() {
    test_encode("***9", "0", 0, "***0");
}

#[test]
fn test_e3_5_asterisk_with_decimal() {
    test_encode("**9.99", "12.34", 2, "*12.34");
}

#[test]
fn test_e3_5_asterisk_with_decimal_zero() {
    test_encode("**9.99", "0.00", 2, "**0.00");
}

#[test]
fn test_e3_5_asterisk_with_decimal_single_digit() {
    test_encode("**9.99", "1.23", 2, "**1.23");
}

#[test]
fn test_e3_5_asterisk_with_decimal_full_width() {
    test_encode("**9.99", "123.45", 2, "123.45");
}

#[test]
fn test_e3_5_asterisk_with_leading_plus() {
    test_encode("+***9", "123", 0, "+*123");
}

#[test]
fn test_e3_5_asterisk_with_leading_plus_negative() {
    test_encode("+***9", "-123", 0, "-*123");
}

#[test]
fn test_e3_5_asterisk_with_leading_plus_zero() {
    test_encode("+***9", "0", 0, "+***0");
}

#[test]
fn test_e3_5_asterisk_with_leading_minus() {
    test_encode("-***9", "123", 0, " *123");
}

#[test]
fn test_e3_5_asterisk_with_leading_minus_negative() {
    test_encode("-***9", "-123", 0, "-*123");
}

#[test]
fn test_e3_5_asterisk_with_leading_minus_zero() {
    test_encode("-***9", "0", 0, " ***0");
}

#[test]
fn test_e3_5_asterisk_with_trailing_plus() {
    test_encode("***9+", "123", 0, "*123+");
}

#[test]
fn test_e3_5_asterisk_with_trailing_plus_negative() {
    test_encode("***9+", "-123", 0, "*123-");
}

#[test]
fn test_e3_5_asterisk_with_trailing_plus_zero() {
    test_encode("***9+", "0", 0, "***0+");
}

#[test]
fn test_e3_5_asterisk_with_trailing_minus() {
    test_encode("***9-", "123", 0, "*123 ");
}

#[test]
fn test_e3_5_asterisk_with_trailing_minus_negative() {
    test_encode("***9-", "-123", 0, "*123-");
}

#[test]
fn test_e3_5_asterisk_with_trailing_minus_zero() {
    test_encode("***9-", "0", 0, "***0 ");
}

#[test]
fn test_e3_5_asterisk_long_pattern() {
    test_encode("*******9", "12345", 0, "***12345");
}

#[test]
fn test_e3_5_asterisk_long_pattern_zero() {
    test_encode("*******9", "0", 0, "*******0");
}

#[test]
fn test_e3_5_asterisk_long_pattern_full_width() {
    test_encode("*******9", "12345678", 0, "12345678");
}

#[test]
fn test_e3_5_asterisk_mixed_with_z() {
    // Asterisk fill before zero suppression
    test_encode("**ZZ9", "123", 0, "**123");
}

#[test]
fn test_e3_5_asterisk_mixed_with_0() {
    // Asterisk fill before zero insert
    test_encode("**009", "123", 0, "**123");
}

#[test]
fn test_e3_5_asterisk_check_protect_example() {
    // Real-world check protection example: $***9.99
    // Currency ($) is now supported in E3.6
    test_encode("$***9.99", "123.45", 2, "$*123.45");
}

#[test]
fn test_e3_5_asterisk_two_digit_value() {
    test_encode("***9", "12", 0, "**12");
}

#[test]
fn test_e3_5_asterisk_with_decimal_two_digit_value() {
    test_encode("**9.99", "2.34", 2, "**2.34");
}

// ===== E3.4: Commas and Slashes Tests =====

#[test]
fn test_e3_4_comma_basic() {
    // Basic comma separator
    test_encode("999,999", "123456", 0, "123,456");
}

#[test]
fn test_e3_4_comma_zero_suppress() {
    // Comma with zero suppression - comma preserved before significant digits
    test_encode("ZZZ,ZZ9", "123", 0, "   ,123");
}

#[test]
fn test_e3_4_comma_all_zeros() {
    // Comma with zero suppression - all zeros, comma becomes space
    test_encode("ZZZ,ZZ9", "0", 0, "      0");
}

#[test]
fn test_e3_4_comma_with_decimal() {
    // Comma with decimal point
    test_encode("ZZ,ZZ9.99", "1234.56", 2, " 1,234.56");
}

#[test]
fn test_e3_4_comma_with_decimal_zero() {
    // Comma with decimal point, zero value
    test_encode("ZZ,ZZ9.99", "0.00", 2, "     0.00");
}

#[test]
fn test_e3_4_comma_multiple() {
    // Multiple commas
    test_encode("9,999,999", "1234567", 0, "1,234,567");
}

#[test]
fn test_e3_4_comma_multiple_partial() {
    // Multiple commas with smaller value
    test_encode("9,999,999", "12345", 0, "0,012,345");
}

#[test]
fn test_e3_4_comma_with_z_multiple() {
    // Multiple commas with zero suppression
    test_encode("Z,ZZZ,ZZ9", "12345", 0, " , 12,345");
}

#[test]
fn test_e3_4_comma_with_z_small_value() {
    // Small value with commas and zero suppression
    test_encode("ZZZ,ZZ9", "12", 0, "   , 12");
}

#[test]
fn test_e3_4_slash_date_format() {
    // Date format with slashes
    test_encode("99/99/99", "123199", 0, "12/31/99");
}

#[test]
fn test_e3_4_slash_date_format_zero_pad() {
    // Date format with leading zeros
    test_encode("99/99/99", "010199", 0, "01/01/99");
}

#[test]
fn test_e3_4_slash_multiple() {
    // Multiple slashes
    test_encode("99/99/9999", "12312025", 0, "12/31/2025");
}

#[test]
fn test_e3_4_slash_with_zero_insert() {
    // Slash with zero insert
    test_encode("00/00/00", "123199", 0, "12/31/99");
}

#[test]
fn test_e3_4_slash_with_z() {
    // Slash with zero suppression (slashes always display)
    test_encode("ZZ/ZZ/ZZ", "123199", 0, "12/31/99");
}

#[test]
fn test_e3_4_comma_and_slash_combined() {
    // Comma and slash combined (unusual but valid)
    test_encode("99,999/99", "1234567", 0, "12,345/67");
}

#[test]
fn test_e3_4_comma_with_sign() {
    // Comma with leading sign
    test_encode("+999,999", "123456", 0, "+123,456");
}

#[test]
fn test_e3_4_comma_with_sign_negative() {
    // Comma with leading sign negative
    test_encode("+999,999", "-123456", 0, "-123,456");
}

#[test]
fn test_e3_4_comma_with_trailing_sign() {
    // Comma with trailing sign
    test_encode("999,999+", "123456", 0, "123,456+");
}

#[test]
fn test_e3_4_comma_with_trailing_sign_negative() {
    // Comma with trailing sign negative
    test_encode("999,999+", "-123456", 0, "123,456-");
}

#[test]
fn test_e3_4_slash_with_sign() {
    // Slash with sign (unusual but valid)
    test_encode("+99/99/99", "123199", 0, "+12/31/99");
}

#[test]
fn test_e3_4_comma_with_decimal_and_sign() {
    // Complex pattern: comma, decimal, and sign
    test_encode("+ZZ,ZZ9.99", "1234.56", 2, "+ 1,234.56");
}

#[test]
fn test_e3_4_comma_with_decimal_and_sign_negative() {
    // Complex pattern: comma, decimal, and sign negative
    test_encode("+ZZ,ZZ9.99", "-1234.56", 2, "- 1,234.56");
}

#[test]
fn test_e3_4_comma_edge_single_comma() {
    // Single comma in pattern
    test_encode("99,99", "1234", 0, "12,34");
}

#[test]
fn test_e3_4_slash_edge_single_slash() {
    // Single slash in pattern
    test_encode("99/99", "1234", 0, "12/34");
}

#[test]
fn test_e3_4_comma_large_value() {
    // Large value with commas
    test_encode("9,999,999,999", "1234567890", 0, "1,234,567,890");
}

#[test]
fn test_e3_4_comma_zero_suppression_partial() {
    // Zero suppression with partial value
    test_encode("Z,ZZZ,ZZ9", "1234", 0, " ,  1,234");
}
// ===== E3.6: Currency Symbol ($) Tests =====

#[test]
fn test_e3_6_currency_fixed() {
    test_encode("$999", "123", 0, "$123");
}

#[test]
fn test_e3_6_currency_fixed_full_value() {
    test_encode("$999", "999", 0, "$999");
}

#[test]
fn test_e3_6_currency_with_zero_suppress() {
    test_encode("$ZZZ9", "5", 0, "$   5");
}

#[test]
fn test_e3_6_currency_with_zero_suppress_123() {
    test_encode("$ZZZ9", "123", 0, "$ 123");
}

#[test]
fn test_e3_6_currency_with_zero_suppress_full() {
    test_encode("$ZZZ9", "1234", 0, "$1234");
}

#[test]
fn test_e3_6_currency_with_decimal() {
    test_encode("$ZZ9.99", "12.34", 2, "$ 12.34");
}

#[test]
fn test_e3_6_currency_with_decimal_small() {
    test_encode("$ZZ9.99", "5.67", 2, "$  5.67");
}

#[test]
fn test_e3_6_currency_with_decimal_zero() {
    test_encode("$ZZ9.99", "0.00", 2, "$  0.00");
}

#[test]
fn test_e3_6_currency_with_comma() {
    test_encode("$Z,ZZ9.99", "1234.56", 2, "$1,234.56");
}

#[test]
fn test_e3_6_currency_with_comma_small() {
    // Comma becomes space during zero suppression
    test_encode("$Z,ZZ9.99", "5.67", 2, "$ ,  5.67");
}

#[test]
fn test_e3_6_currency_with_comma_zero() {
    // Comma suppressed when all zeros
    test_encode("$Z,ZZ9.99", "0.00", 2, "$    0.00");
}

#[test]
fn test_e3_6_currency_with_sign_positive() {
    test_encode("$ZZZ9+", "123", 0, "$ 123+");
}

#[test]
fn test_e3_6_currency_with_sign_negative() {
    test_encode("$ZZZ9+", "-123", 0, "$ 123-");
}

#[test]
fn test_e3_6_currency_multiple_patterns() {
    test_encode("$999.99", "123.45", 2, "$123.45");
}

#[test]
fn test_e3_6_currency_with_asterisk() {
    // Check protection with currency: $***9.99
    test_encode("$***9.99", "123.45", 2, "$*123.45");
}

#[test]
fn test_e3_6_currency_with_asterisk_small() {
    test_encode("$***9.99", "5.67", 2, "$***5.67");
}

#[test]
fn test_e3_6_currency_with_asterisk_zero() {
    test_encode("$***9.99", "0.00", 2, "$***0.00");
}
