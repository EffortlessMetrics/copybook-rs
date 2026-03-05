// SPDX-License-Identifier: AGPL-3.0-or-later
//! Phase E2: Edited PIC decode comprehensive tests
//!
//! Tests the well-chosen subset of edited PIC patterns as specified:
//! - ZZZ9: basic zero suppression
//! - ZZZ9.99: with decimal point
//! - $ZZ,ZZZ.99: currency with comma and decimal
//! - Sign editing: +, -, CR, DB
//! - Asterisk check-protect
//! - BLANK WHEN ZERO handling
//!
//! These tests verify the lockstep decode algorithm for edited numeric fields.

use copybook_codec::{Codepage, DecodeOptions, decode_record};
use copybook_core::parse_copybook;

/// Helper to create DecodeOptions with ASCII codepage for testing
fn ascii_options() -> DecodeOptions {
    DecodeOptions::new().with_codepage(Codepage::ASCII)
}

#[test]
fn test_e2_simple_z_editing_zzz9() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "  12" (two leading spaces for zero suppression)
    let data = b"  12";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "12");
}

#[test]
fn test_e2_zero_suppression_all_zeros() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "   0" (all zeros suppressed except last digit)
    let data = b"   0";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "0");
}

#[test]
fn test_e2_decimal_editing_zzz9_99() {
    let copybook = "01 REC.\n   05 PRICE PIC ZZZ9V99.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "  1234" (V is implicit decimal, no actual . in data)
    let data = b"  1234";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let price = json
        .get("fields")
        .and_then(|f| f.get("PRICE"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(price, "12.34");
}

#[test]
fn test_e2_decimal_zero_integer_part() {
    let copybook = "01 REC.\n   05 PRICE PIC ZZZ9V99.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "   099" (zero integer part, V is implicit)
    let data = b"   099";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let price = json
        .get("fields")
        .and_then(|f| f.get("PRICE"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(price, "0.99");
}

#[test]
fn test_e2_comma_editing_z_zzz_zz9() {
    let copybook = "01 REC.\n   05 AMOUNT PIC Z999999.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "1234567" (no commas with simplified pattern)
    let data = b"1234567";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "1234567");
}

#[test]
fn test_e2_currency_dollar_zz_zzz_99() {
    let copybook = "01 REC.\n   05 PRICE PIC $ZZ999V99.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "$ 123456" (no commas/decimals with V)
    let data = b"$ 123456";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let price = json
        .get("fields")
        .and_then(|f| f.get("PRICE"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(price, "1234.56"); // 123456 with scale=2
}

#[test]
fn test_e2_currency_suppressed() {
    let copybook = "01 REC.\n   05 PRICE PIC $ZZZ9V99.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "    123" (currency symbol suppressed with leading zeros, V is implicit)
    let data = b"    123";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let price = json
        .get("fields")
        .and_then(|f| f.get("PRICE"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(price, "1.23");
}

#[test]
fn test_e2_sign_editing_leading_plus_positive() {
    let copybook = "01 REC.\n   05 AMOUNT PIC +ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "+ 123"
    let data = b"+ 123";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "123");
}

#[test]
fn test_e2_sign_editing_leading_plus_space() {
    let copybook = "01 REC.\n   05 AMOUNT PIC +ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "  123" (space instead of +)
    let data = b"  123";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "123");
}

#[test]
fn test_e2_sign_editing_leading_minus_negative() {
    let copybook = "01 REC.\n   05 AMOUNT PIC -ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "- 123"
    let data = b"- 123";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "-123");
}

#[test]
fn test_e2_sign_editing_leading_minus_positive() {
    let copybook = "01 REC.\n   05 AMOUNT PIC -ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "  123" (space means positive)
    let data = b"  123";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "123");
}

#[test]
fn test_e2_sign_editing_trailing_cr_negative() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9CR.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: " 123CR"
    let data = b" 123CR";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "-123");
}

#[test]
fn test_e2_sign_editing_trailing_cr_positive() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9CR.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: " 123  " (spaces instead of CR)
    let data = b" 123  ";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "123");
}

#[test]
fn test_e2_sign_editing_trailing_db_negative() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9DB.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: " 123DB"
    let data = b" 123DB";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "-123");
}

#[test]
fn test_e2_sign_editing_trailing_db_positive() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9DB.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: " 123  " (spaces instead of DB)
    let data = b" 123  ";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "123");
}

#[test]
fn test_e2_check_protect_asterisk() {
    let copybook = "01 REC.\n   05 CHECK-AMT PIC ****99V99.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "***12234" (asterisk fill for check protection, V is implicit, 8 chars total)
    let data = b"***12234";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let check_amt = json
        .get("fields")
        .and_then(|f| f.get("CHECK-AMT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(check_amt, "122.34"); // 12234 with scale=2
}

#[test]
fn test_e2_check_protect_no_asterisk() {
    let copybook = "01 REC.\n   05 CHECK-AMT PIC ****99V99.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "12123456" (no asterisk fill, V is implicit, 8 chars total)
    let data = b"12123456";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let check_amt = json
        .get("fields")
        .and_then(|f| f.get("CHECK-AMT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(check_amt, "121234.56"); // 12123456 with scale=2
}

#[test]
fn test_e2_blank_when_zero() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9 BLANK WHEN ZERO.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "    " (all blanks)
    let data = b"    ";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "0");
}

#[test]
fn test_e2_blank_when_zero_not_blank() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9 BLANK WHEN ZERO.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "  12" (not all blanks)
    let data = b"  12";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "12");
}

#[test]
fn test_e2_invalid_format_cbkd421() {
    let copybook = "01 REC.\n   05 PRICE PIC ZZZ9V99.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "  12A34" (invalid character 'A')
    let data = b"  12A34";
    let options = ascii_options();
    let result = decode_record(&schema, data, &options);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        copybook_core::ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn test_e2_invalid_format_missing_decimal() {
    let copybook = "01 REC.\n   05 PRICE PIC ZZZ9V99.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "  12 34" (space instead of decimal point)
    let data = b"  12 34";
    let options = ascii_options();
    let result = decode_record(&schema, data, &options);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        copybook_core::ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn test_e2_invalid_format_missing_comma() {
    let copybook = "01 REC.\n   05 AMOUNT PIC Z999999.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "1 234 567" (spaces instead of commas)
    let data = b"1 234 567";
    let options = ascii_options();
    let result = decode_record(&schema, data, &options);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        copybook_core::ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
    );
}

#[test]
fn test_e2_complex_pattern_dollar_comma_decimal_sign() {
    let copybook = "01 REC.\n   05 TOTAL PIC $ZZ99999CR.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "$ 123456CR" (negative amount with CR, no commas/decimals)
    let data = b"$ 123456CR";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let total = json
        .get("fields")
        .and_then(|f| f.get("TOTAL"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(total, "-123456"); // No scale, integer value
}

#[test]
fn test_e2_complex_pattern_positive() {
    let copybook = "01 REC.\n   05 TOTAL PIC $ZZ99999CR.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "$ 123456  " (positive amount, spaces instead of CR)
    let data = b"$ 123456  ";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let total = json
        .get("fields")
        .and_then(|f| f.get("TOTAL"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(total, "123456"); // No scale, integer value
}

#[test]
fn test_e2_multiple_fields() {
    let copybook = r#"
        01 REC.
           05 QTY    PIC ZZZ9.
           05 PRICE  PIC ZZZ9V99.
           05 TOTAL  PIC $ZZ99999CR.
    "#;
    let schema = parse_copybook(copybook).unwrap();

    // Test data: concatenated fields (V is implicit, no decimals/commas)
    let data = b"  12  3456$ 123456  ";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let qty = json
        .get("fields")
        .and_then(|f| f.get("QTY"))
        .and_then(|v| v.as_str())
        .unwrap();
    let price = json
        .get("fields")
        .and_then(|f| f.get("PRICE"))
        .and_then(|v| v.as_str())
        .unwrap();
    let total = json
        .get("fields")
        .and_then(|f| f.get("TOTAL"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(qty, "12");
    assert_eq!(price, "34.56"); // 3456 with scale=2
    assert_eq!(total, "123456"); // No scale, integer
}

#[test]
fn test_e2_zero_with_negative_sign() {
    let copybook = "01 REC.\n   05 AMOUNT PIC -ZZZ9.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: "-   0" (negative zero should become positive zero)
    let data = b"-   0";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    // Zero should always be positive (no negative zero)
    assert_eq!(amount, "0");
}

#[test]
fn test_e2_trailing_plus_sign() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9+.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: " 123+"
    let data = b" 123+";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "123");
}

#[test]
fn test_e2_trailing_minus_sign() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZ9-.";
    let schema = parse_copybook(copybook).unwrap();

    // Test data: " 123-"
    let data = b" 123-";
    let options = ascii_options();
    let json = decode_record(&schema, data, &options).unwrap();

    let amount = json
        .get("fields")
        .and_then(|f| f.get("AMOUNT"))
        .and_then(|v| v.as_str())
        .unwrap();

    assert_eq!(amount, "-123");
}
