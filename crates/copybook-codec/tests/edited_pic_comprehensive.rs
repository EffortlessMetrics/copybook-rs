// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive edited PIC tests: tokenization, decode, encode, roundtrip, errors.
//!
//! Covers every token type and combination:
//!   Z-suppression, currency ($), sign (+, -, CR, DB), asterisk (*),
//!   comma, slash, space (B), BLANK WHEN ZERO, combined patterns,
//!   roundtrip fidelity, and error cases.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::edited_pic::{
    NumericValue, PicToken, Sign, decode_edited_numeric, encode_edited_numeric, tokenize_edited_pic,
};
use copybook_core::ErrorCode;

// ====================================================================
// Helpers
// ====================================================================

fn encode(pat: &str, val: &str, scale: u16) -> String {
    let tokens = tokenize_edited_pic(pat).unwrap();
    encode_edited_numeric(val, &tokens, scale, false).unwrap()
}

fn decode(pat: &str, input: &str, scale: u16) -> NumericValue {
    let tokens = tokenize_edited_pic(pat).unwrap();
    decode_edited_numeric(input, &tokens, scale, false).unwrap()
}

fn decode_bwz(pat: &str, input: &str, scale: u16) -> NumericValue {
    let tokens = tokenize_edited_pic(pat).unwrap();
    decode_edited_numeric(input, &tokens, scale, true).unwrap()
}

fn roundtrip(pat: &str, val: &str, scale: u16) {
    let encoded = encode(pat, val, scale);
    let decoded = decode(pat, &encoded, scale);
    let dec_str = decoded.to_decimal_string();
    // Normalise: strip leading minus if value is "0"
    let normalised_input = val.trim_start_matches('+');
    assert_eq!(
        dec_str, normalised_input,
        "Roundtrip failed for pattern '{pat}', value '{val}': encoded='{encoded}', decoded='{dec_str}'"
    );
}

// ====================================================================
// 1. Tokenization
// ====================================================================

#[test]
fn tokenize_z_suppression_variants() {
    assert_eq!(
        tokenize_edited_pic("Z").unwrap(),
        vec![PicToken::ZeroSuppress]
    );
    assert_eq!(
        tokenize_edited_pic("ZZ").unwrap(),
        vec![PicToken::ZeroSuppress, PicToken::ZeroSuppress]
    );
    assert_eq!(
        tokenize_edited_pic("ZZZ").unwrap(),
        vec![
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress
        ]
    );
    assert_eq!(
        tokenize_edited_pic("ZZZZ").unwrap(),
        vec![
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress
        ]
    );
}

#[test]
fn tokenize_zzz9() {
    let tokens = tokenize_edited_pic("ZZZ9").unwrap();
    assert_eq!(
        tokens,
        vec![
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::Digit
        ]
    );
}

#[test]
fn tokenize_zz99() {
    let tokens = tokenize_edited_pic("ZZ99").unwrap();
    assert_eq!(
        tokens,
        vec![
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::Digit,
            PicToken::Digit
        ]
    );
}

#[test]
fn tokenize_z999() {
    let tokens = tokenize_edited_pic("Z999").unwrap();
    assert_eq!(
        tokens,
        vec![
            PicToken::ZeroSuppress,
            PicToken::Digit,
            PicToken::Digit,
            PicToken::Digit
        ]
    );
}

#[test]
fn tokenize_currency_pattern() {
    let tokens = tokenize_edited_pic("$ZZ,ZZZ.99").unwrap();
    assert_eq!(tokens[0], PicToken::Currency);
    assert!(tokens.contains(&PicToken::Comma));
    assert!(tokens.contains(&PicToken::DecimalPoint));
    // Count numeric positions: 5 Z + 2 digit = 7
    let numeric_count = tokens
        .iter()
        .filter(|t| matches!(t, PicToken::ZeroSuppress | PicToken::Digit))
        .count();
    assert_eq!(numeric_count, 7);
}

#[test]
fn tokenize_sign_patterns() {
    let plus = tokenize_edited_pic("+999").unwrap();
    assert_eq!(plus[0], PicToken::LeadingPlus);

    let minus = tokenize_edited_pic("-999").unwrap();
    assert_eq!(minus[0], PicToken::LeadingMinus);

    let trailing_plus = tokenize_edited_pic("999+").unwrap();
    assert_eq!(*trailing_plus.last().unwrap(), PicToken::TrailingPlus);

    let trailing_minus = tokenize_edited_pic("999-").unwrap();
    assert_eq!(*trailing_minus.last().unwrap(), PicToken::TrailingMinus);
}

#[test]
fn tokenize_cr_and_db() {
    let cr = tokenize_edited_pic("ZZZ9CR").unwrap();
    assert_eq!(*cr.last().unwrap(), PicToken::Credit);

    let db = tokenize_edited_pic("ZZZ9DB").unwrap();
    assert_eq!(*db.last().unwrap(), PicToken::Debit);
}

#[test]
fn tokenize_asterisk_fill() {
    let tokens = tokenize_edited_pic("***9").unwrap();
    assert_eq!(
        tokens,
        vec![
            PicToken::AsteriskFill,
            PicToken::AsteriskFill,
            PicToken::AsteriskFill,
            PicToken::Digit
        ]
    );
}

#[test]
fn tokenize_slash_insertion() {
    let tokens = tokenize_edited_pic("99/99/99").unwrap();
    assert_eq!(tokens.len(), 8);
    assert_eq!(tokens[2], PicToken::Slash);
    assert_eq!(tokens[5], PicToken::Slash);
}

#[test]
fn tokenize_space_insertion() {
    let tokens = tokenize_edited_pic("99B99").unwrap();
    assert_eq!(tokens[2], PicToken::Space);
}

#[test]
fn tokenize_repetition_syntax() {
    let tokens = tokenize_edited_pic("Z(4)9").unwrap();
    assert_eq!(tokens.len(), 5);
    assert_eq!(
        tokens,
        vec![
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::ZeroSuppress,
            PicToken::Digit
        ]
    );
}

#[test]
fn tokenize_rejects_multiple_decimal_points() {
    let result = tokenize_edited_pic("99.99.99");
    assert!(result.is_err());
}

#[test]
fn tokenize_rejects_empty_pattern() {
    let result = tokenize_edited_pic("");
    assert!(result.is_err());
}

// ====================================================================
// 2. Z-suppression decode patterns
// ====================================================================

#[test]
fn decode_z_single_digit() {
    let nv = decode("Z", "5", 0);
    assert_eq!(nv.to_decimal_string(), "5");
}

#[test]
fn decode_z_single_suppressed_zero() {
    let nv = decode("Z", " ", 0);
    assert_eq!(nv.to_decimal_string(), "0");
}

#[test]
fn decode_zz_partial_suppression() {
    let nv = decode("ZZ", " 5", 0);
    assert_eq!(nv.to_decimal_string(), "5");
}

#[test]
fn decode_zzz9_value_12() {
    let nv = decode("ZZZ9", "  12", 0);
    assert_eq!(nv.to_decimal_string(), "12");
}

#[test]
fn decode_zzz9_all_zeros() {
    let nv = decode("ZZZ9", "   0", 0);
    assert_eq!(nv.to_decimal_string(), "0");
}

#[test]
fn decode_zzz9_full_width() {
    let nv = decode("ZZZ9", "9999", 0);
    assert_eq!(nv.to_decimal_string(), "9999");
}

#[test]
fn decode_zz99_value() {
    let nv = decode("ZZ99", " 123", 0);
    assert_eq!(nv.to_decimal_string(), "123");
}

#[test]
fn decode_z999_value() {
    let nv = decode("Z999", " 123", 0);
    assert_eq!(nv.to_decimal_string(), "123");
}

// ====================================================================
// 3. Decimal point handling
// ====================================================================

#[test]
fn decode_zzz9_dot_99() {
    let nv = decode("ZZZ9.99", "  12.34", 2);
    assert_eq!(nv.to_decimal_string(), "12.34");
}

#[test]
fn decode_99_dot_99_full() {
    let nv = decode("99.99", "12.34", 2);
    assert_eq!(nv.to_decimal_string(), "12.34");
}

#[test]
fn decode_99_dot_99_zero() {
    let nv = decode("99.99", "00.00", 2);
    assert_eq!(nv.to_decimal_string(), "0");
}

// ====================================================================
// 4. Currency patterns
// ====================================================================

#[test]
fn decode_dollar_zzz_dot_99() {
    let nv = decode("$ZZZ.99", "$ 12.34", 2);
    assert_eq!(nv.to_decimal_string(), "12.34");
}

#[test]
fn decode_dollar_zz_comma_zzz_dot_99() {
    let nv = decode("$ZZ,ZZZ.99", "$ 1,234.56", 2);
    assert_eq!(nv.to_decimal_string(), "1234.56");
}

// ====================================================================
// 5. Sign patterns (leading/trailing +/-)
// ====================================================================

#[test]
fn decode_leading_plus_positive() {
    let nv = decode("+999", "+123", 0);
    assert_eq!(nv.sign, Sign::Positive);
    assert_eq!(nv.to_decimal_string(), "123");
}

#[test]
fn decode_leading_plus_negative() {
    let nv = decode("+999", "-123", 0);
    assert_eq!(nv.sign, Sign::Negative);
    assert_eq!(nv.to_decimal_string(), "-123");
}

#[test]
fn decode_leading_minus_positive() {
    let nv = decode("-999", " 123", 0);
    assert_eq!(nv.sign, Sign::Positive);
    assert_eq!(nv.to_decimal_string(), "123");
}

#[test]
fn decode_leading_minus_negative() {
    let nv = decode("-999", "-123", 0);
    assert_eq!(nv.sign, Sign::Negative);
    assert_eq!(nv.to_decimal_string(), "-123");
}

#[test]
fn decode_trailing_plus_positive() {
    let nv = decode("999+", "123+", 0);
    assert_eq!(nv.sign, Sign::Positive);
    assert_eq!(nv.to_decimal_string(), "123");
}

#[test]
fn decode_trailing_plus_negative() {
    let nv = decode("999+", "123-", 0);
    assert_eq!(nv.sign, Sign::Negative);
    assert_eq!(nv.to_decimal_string(), "-123");
}

#[test]
fn decode_trailing_minus_positive() {
    let nv = decode("999-", "123 ", 0);
    assert_eq!(nv.sign, Sign::Positive);
    assert_eq!(nv.to_decimal_string(), "123");
}

#[test]
fn decode_trailing_minus_negative() {
    let nv = decode("999-", "123-", 0);
    assert_eq!(nv.sign, Sign::Negative);
    assert_eq!(nv.to_decimal_string(), "-123");
}

// ====================================================================
// 6. CR and DB sign indicators
// ====================================================================

#[test]
fn decode_cr_negative() {
    let nv = decode("ZZZ9CR", "  12CR", 0);
    assert_eq!(nv.sign, Sign::Negative);
    assert_eq!(nv.to_decimal_string(), "-12");
}

#[test]
fn decode_cr_positive_spaces() {
    let nv = decode("ZZZ9CR", "  12  ", 0);
    assert_eq!(nv.sign, Sign::Positive);
    assert_eq!(nv.to_decimal_string(), "12");
}

#[test]
fn decode_db_negative() {
    let nv = decode("ZZZ9DB", "  12DB", 0);
    assert_eq!(nv.sign, Sign::Negative);
    assert_eq!(nv.to_decimal_string(), "-12");
}

#[test]
fn decode_db_positive_spaces() {
    let nv = decode("ZZZ9DB", "  12  ", 0);
    assert_eq!(nv.sign, Sign::Positive);
    assert_eq!(nv.to_decimal_string(), "12");
}

// ====================================================================
// 7. Asterisk check-protect
// ====================================================================

#[test]
fn decode_asterisk_fill_with_value() {
    let nv = decode("***9", "**12", 0);
    assert_eq!(nv.to_decimal_string(), "12");
}

#[test]
fn decode_asterisk_fill_all_stars() {
    let nv = decode("***9", "***0", 0);
    assert_eq!(nv.to_decimal_string(), "0");
}

#[test]
fn decode_asterisk_fill_full_value() {
    let nv = decode("***9", "1234", 0);
    assert_eq!(nv.to_decimal_string(), "1234");
}

// ====================================================================
// 8. Comma insertion
// ====================================================================

#[test]
fn decode_comma_pattern() {
    let nv = decode("9,999", "1,234", 0);
    assert_eq!(nv.to_decimal_string(), "1234");
}

// ====================================================================
// 9. Slash insertion
// ====================================================================

#[test]
fn decode_slash_date_pattern() {
    let nv = decode("99/99/99", "12/31/24", 0);
    assert_eq!(nv.to_decimal_string(), "123124");
}

// ====================================================================
// 10. Space (B) insertion
// ====================================================================

#[test]
fn decode_space_insertion() {
    let nv = decode("99B99", "12 34", 0);
    assert_eq!(nv.to_decimal_string(), "1234");
}

// ====================================================================
// 11. BLANK WHEN ZERO
// ====================================================================

#[test]
fn decode_blank_when_zero_all_spaces() {
    let nv = decode_bwz("ZZZ9", "    ", 0);
    assert_eq!(nv.to_decimal_string(), "0");
}

#[test]
fn decode_blank_when_zero_with_decimal() {
    let nv = decode_bwz("ZZZ9.99", "       ", 2);
    assert_eq!(nv.to_decimal_string(), "0");
}

// ====================================================================
// 12. Z-suppression encode patterns
// ====================================================================

#[test]
fn encode_z_single() {
    assert_eq!(encode("Z", "5", 0), "5");
}

#[test]
fn encode_z_zero_gives_space() {
    // Single Z: zero suppression still produces the digit "0" (only position)
    assert_eq!(encode("Z", "0", 0), "0");
}

#[test]
fn encode_zz_value() {
    assert_eq!(encode("ZZ", "42", 0), "42");
}

#[test]
fn encode_zzz_value() {
    assert_eq!(encode("ZZZ", "123", 0), "123");
}

#[test]
fn encode_zzzz_zero() {
    // All-Z with value 0: last digit position still shows '0'
    assert_eq!(encode("ZZZZ", "0", 0), "   0");
}

#[test]
fn encode_zzz9_zero() {
    assert_eq!(encode("ZZZ9", "0", 0), "   0");
}

#[test]
fn encode_zz99_zero() {
    assert_eq!(encode("ZZ99", "0", 0), "  00");
}

#[test]
fn encode_z999_zero() {
    assert_eq!(encode("Z999", "0", 0), " 000");
}

#[test]
fn encode_zzz9_small_value() {
    assert_eq!(encode("ZZZ9", "7", 0), "   7");
}

#[test]
fn encode_zzz9_medium_value() {
    assert_eq!(encode("ZZZ9", "123", 0), " 123");
}

#[test]
fn encode_zzz9_full_value() {
    assert_eq!(encode("ZZZ9", "9999", 0), "9999");
}

// ====================================================================
// 13. Currency encode patterns
// ====================================================================

#[test]
fn encode_dollar_999() {
    assert_eq!(encode("$999", "123", 0), "$123");
}

#[test]
fn encode_dollar_zzz9_dot_99() {
    assert_eq!(encode("$ZZZ9.99", "123.45", 2), "$ 123.45");
}

// ====================================================================
// 14. Sign encode patterns
// ====================================================================

#[test]
fn encode_leading_plus_positive() {
    assert_eq!(encode("+999", "123", 0), "+123");
}

#[test]
fn encode_leading_plus_negative() {
    assert_eq!(encode("+999", "-123", 0), "-123");
}

#[test]
fn encode_leading_minus_positive() {
    assert_eq!(encode("-999", "123", 0), " 123");
}

#[test]
fn encode_leading_minus_negative() {
    assert_eq!(encode("-999", "-123", 0), "-123");
}

#[test]
fn encode_trailing_plus_positive() {
    assert_eq!(encode("999+", "123", 0), "123+");
}

#[test]
fn encode_trailing_plus_negative() {
    assert_eq!(encode("999+", "-123", 0), "123-");
}

#[test]
fn encode_trailing_minus_positive() {
    assert_eq!(encode("999-", "123", 0), "123 ");
}

#[test]
fn encode_trailing_minus_negative() {
    assert_eq!(encode("999-", "-123", 0), "123-");
}

// ====================================================================
// 15. CR / DB encode
// ====================================================================

#[test]
fn encode_cr_positive() {
    assert_eq!(encode("999CR", "123", 0), "123  ");
}

#[test]
fn encode_cr_negative() {
    assert_eq!(encode("999CR", "-123", 0), "123CR");
}

#[test]
fn encode_db_positive() {
    assert_eq!(encode("999DB", "123", 0), "123  ");
}

#[test]
fn encode_db_negative() {
    assert_eq!(encode("999DB", "-123", 0), "123DB");
}

// ====================================================================
// 16. Asterisk check-protect encode
// ====================================================================

#[test]
fn encode_asterisk_fill_zero() {
    assert_eq!(encode("***9", "0", 0), "***0");
}

#[test]
fn encode_asterisk_fill_small() {
    assert_eq!(encode("***9", "7", 0), "***7");
}

#[test]
fn encode_asterisk_fill_value() {
    assert_eq!(encode("***9", "123", 0), "*123");
}

#[test]
fn encode_asterisk_fill_full() {
    assert_eq!(encode("***9", "9999", 0), "9999");
}

#[test]
fn encode_asterisk_with_decimal() {
    assert_eq!(encode("***9.99", "123.45", 2), "*123.45");
}

// ====================================================================
// 17. Comma insertion encode
// ====================================================================

#[test]
fn encode_comma_pattern() {
    assert_eq!(encode("9,999", "1234", 0), "1,234");
}

// ====================================================================
// 18. Slash insertion encode
// ====================================================================

#[test]
fn encode_slash_date_pattern() {
    assert_eq!(encode("99/99/99", "123124", 0), "12/31/24");
}

// ====================================================================
// 19. Space (B) insertion encode
// ====================================================================

#[test]
fn encode_space_insertion() {
    assert_eq!(encode("99B99", "1234", 0), "12 34");
}

// ====================================================================
// 20. Combined patterns encode
// ====================================================================

#[test]
fn encode_dollar_zz_comma_zzz_dot_99() {
    assert_eq!(encode("$ZZ,ZZZ.99", "1234.56", 2), "$ 1,234.56");
}

#[test]
fn encode_plus_zzz9_dot_99() {
    assert_eq!(encode("+ZZZ9.99", "123.45", 2), "+ 123.45");
}

#[test]
fn encode_plus_zzz9_dot_99_negative() {
    assert_eq!(encode("+ZZZ9.99", "-123.45", 2), "- 123.45");
}

#[test]
fn encode_asterisk_comma_pattern() {
    assert_eq!(encode("***,**9.99", "1234.56", 2), "**1,234.56");
}

// ====================================================================
// 21. Roundtrip: encode then decode
// ====================================================================

#[test]
fn roundtrip_zzz9_value() {
    roundtrip("ZZZ9", "123", 0);
}

#[test]
fn roundtrip_zzz9_zero() {
    roundtrip("ZZZ9", "0", 0);
}

#[test]
fn roundtrip_99_dot_99() {
    roundtrip("99.99", "12.34", 2);
}

#[test]
fn roundtrip_leading_plus_positive() {
    roundtrip("+999", "123", 0);
}

#[test]
fn roundtrip_leading_plus_negative() {
    roundtrip("+999", "-123", 0);
}

#[test]
fn roundtrip_trailing_minus_negative() {
    roundtrip("999-", "-123", 0);
}

#[test]
fn roundtrip_cr_negative() {
    roundtrip("999CR", "-123", 0);
}

#[test]
fn roundtrip_db_negative() {
    roundtrip("999DB", "-123", 0);
}

#[test]
fn roundtrip_asterisk_fill() {
    roundtrip("***9", "123", 0);
}

// ====================================================================
// 22. Error cases – decode
// ====================================================================

#[test]
fn decode_error_input_too_short() {
    let tokens = tokenize_edited_pic("ZZZ9").unwrap();
    let result = decode_edited_numeric("12", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn decode_error_input_too_long() {
    let tokens = tokenize_edited_pic("ZZZ9").unwrap();
    let result = decode_edited_numeric("12345", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn decode_error_digit_position_has_letter() {
    let tokens = tokenize_edited_pic("9999").unwrap();
    let result = decode_edited_numeric("12X4", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn decode_error_sign_mismatch_leading_plus() {
    let tokens = tokenize_edited_pic("+999").unwrap();
    let result = decode_edited_numeric("X123", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH
    );
}

#[test]
fn decode_error_sign_mismatch_cr() {
    let tokens = tokenize_edited_pic("ZZZ9CR").unwrap();
    let result = decode_edited_numeric("  12XY", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH
    );
}

#[test]
fn decode_error_sign_mismatch_db() {
    let tokens = tokenize_edited_pic("ZZZ9DB").unwrap();
    let result = decode_edited_numeric("  12XY", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH
    );
}

#[test]
fn decode_error_expected_comma() {
    let tokens = tokenize_edited_pic("9,999").unwrap();
    let result = decode_edited_numeric("1X234", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn decode_error_expected_decimal_point() {
    let tokens = tokenize_edited_pic("99.99").unwrap();
    let result = decode_edited_numeric("12X34", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

// ====================================================================
// 23. Error cases – encode
// ====================================================================

#[test]
fn encode_error_overflow() {
    let tokens = tokenize_edited_pic("99").unwrap();
    let result = encode_edited_numeric("12345", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn encode_error_empty_value() {
    let tokens = tokenize_edited_pic("999").unwrap();
    let result = encode_edited_numeric("", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn encode_error_non_numeric_value() {
    let tokens = tokenize_edited_pic("999").unwrap();
    let result = encode_edited_numeric("abc", &tokens, 0, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn encode_error_multiple_decimal_points() {
    let tokens = tokenize_edited_pic("99.99").unwrap();
    let result = encode_edited_numeric("1.2.3", &tokens, 2, false);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

// ====================================================================
// 24. NumericValue::to_decimal_string edge cases
// ====================================================================

#[test]
fn numeric_value_zero_decimal_string() {
    let nv = NumericValue {
        sign: Sign::Positive,
        digits: "0".to_string(),
        scale: 0,
    };
    assert_eq!(nv.to_decimal_string(), "0");
}

#[test]
fn numeric_value_negative_zero_displays_as_zero() {
    // After decoding, negative zero should become positive zero
    let nv = NumericValue {
        sign: Sign::Positive,
        digits: "0".to_string(),
        scale: 2,
    };
    assert_eq!(nv.to_decimal_string(), "0");
}

#[test]
fn numeric_value_leading_zero_fraction() {
    let nv = NumericValue {
        sign: Sign::Positive,
        digits: "5".to_string(),
        scale: 3,
    };
    assert_eq!(nv.to_decimal_string(), "0.005");
}

#[test]
fn numeric_value_negative_with_scale() {
    let nv = NumericValue {
        sign: Sign::Negative,
        digits: "12345".to_string(),
        scale: 2,
    };
    assert_eq!(nv.to_decimal_string(), "-123.45");
}

// ====================================================================
// 25. PicToken Display
// ====================================================================

#[test]
fn pic_token_display_all_variants() {
    assert_eq!(format!("{}", PicToken::Digit), "9");
    assert_eq!(format!("{}", PicToken::ZeroSuppress), "Z");
    assert_eq!(format!("{}", PicToken::ZeroInsert), "0");
    assert_eq!(format!("{}", PicToken::AsteriskFill), "*");
    assert_eq!(format!("{}", PicToken::Space), "B");
    assert_eq!(format!("{}", PicToken::Comma), ",");
    assert_eq!(format!("{}", PicToken::Slash), "/");
    assert_eq!(format!("{}", PicToken::DecimalPoint), ".");
    assert_eq!(format!("{}", PicToken::Currency), "$");
    assert_eq!(format!("{}", PicToken::LeadingPlus), "+");
    assert_eq!(format!("{}", PicToken::LeadingMinus), "-");
    assert_eq!(format!("{}", PicToken::TrailingPlus), "+");
    assert_eq!(format!("{}", PicToken::TrailingMinus), "-");
    assert_eq!(format!("{}", PicToken::Credit), "CR");
    assert_eq!(format!("{}", PicToken::Debit), "DB");
}
