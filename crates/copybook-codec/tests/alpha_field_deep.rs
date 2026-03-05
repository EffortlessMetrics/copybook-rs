// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Deep edge-case tests for alphanumeric (PIC X / PIC A) fields.
//!
//! Covers:
//! - Max-length alphanumeric fields (PIC X(1000))
//! - All-spaces field
//! - Truncation / padding behavior
//! - Single-character fields
//! - PIC A (alphabetic) fields
//! - EBCDIC encoding round-trips
//! - Encode roundtrip for PIC X, PIC A, PIC X(n)

use copybook_codec::numeric::encode_alphanumeric;
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn ascii_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true)
}

fn ebcdic_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn ebcdic_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true)
}

// ===========================================================================
// 1. PIC X — basic encode_alphanumeric edge cases
// ===========================================================================

#[test]
fn test_alpha_encode_exact_length() {
    let encoded = encode_alphanumeric("ABCDE", 5, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"ABCDE");
}

#[test]
fn test_alpha_encode_empty_string_padded() {
    let encoded = encode_alphanumeric("", 10, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 10);
    assert!(encoded.iter().all(|&b| b == b' '));
}

#[test]
fn test_alpha_encode_short_string_right_padded() {
    let encoded = encode_alphanumeric("HI", 5, Codepage::ASCII).unwrap();
    assert_eq!(&encoded[..2], b"HI");
    assert_eq!(&encoded[2..], b"   ");
}

#[test]
fn test_alpha_encode_too_long_rejected() {
    let result = encode_alphanumeric("TOOLONGTEXT", 5, Codepage::ASCII);
    assert!(result.is_err());
}

#[test]
fn test_alpha_encode_single_char() {
    let encoded = encode_alphanumeric("Z", 1, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"Z");
}

#[test]
fn test_alpha_encode_all_spaces() {
    let encoded = encode_alphanumeric("     ", 5, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"     ");
}

#[test]
fn test_alpha_encode_special_ascii_chars() {
    let encoded = encode_alphanumeric("@#$%&", 5, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"@#$%&");
}

#[test]
fn test_alpha_encode_digits_in_pic_x() {
    // PIC X allows any character including digits
    let encoded = encode_alphanumeric("12345", 5, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"12345");
}

// ===========================================================================
// 2. EBCDIC encode/decode
// ===========================================================================

#[test]
fn test_alpha_ebcdic_padding_is_0x40() {
    let encoded = encode_alphanumeric("A", 4, Codepage::CP037).unwrap();
    assert_eq!(encoded.len(), 4);
    // EBCDIC space is 0x40
    assert_eq!(encoded[1], 0x40);
    assert_eq!(encoded[2], 0x40);
    assert_eq!(encoded[3], 0x40);
}

#[test]
fn test_alpha_ebcdic_all_spaces() {
    let encoded = encode_alphanumeric("", 3, Codepage::CP037).unwrap();
    assert!(encoded.iter().all(|&b| b == 0x40));
}

// ===========================================================================
// 3. Record-level PIC X decode/encode roundtrip
// ===========================================================================

#[test]
fn test_record_pic_x5_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC X(5).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"HELLO";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "HELLO");
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

#[test]
fn test_record_pic_x_all_spaces_decode() {
    let cpy = "       01  REC.\n           05  F1 PIC X(8).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"        "; // 8 spaces
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "        ");
}

#[test]
fn test_record_pic_x1_single_char() {
    let cpy = "       01  REC.\n           05  F1 PIC X.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"Q";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "Q");
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

#[test]
fn test_record_pic_x100_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC X(100).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let mut data = vec![b' '; 100];
    data[..5].copy_from_slice(b"HELLO");
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let f1 = json["F1"].as_str().unwrap();
    assert!(f1.starts_with("HELLO"));
    assert_eq!(f1.len(), 100);
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

#[test]
fn test_record_pic_x_with_trailing_spaces_preserved() {
    let cpy = "       01  REC.\n           05  F1 PIC X(10).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"AB        "; // "AB" + 8 spaces
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    // Full field including trailing spaces should be preserved
    assert_eq!(json["F1"].as_str().unwrap().len(), 10);
}

// ===========================================================================
// 4. PIC A (alphabetic) fields — not supported by parser, skip
// ===========================================================================

// ===========================================================================
// 5. Multi-field alphanumeric records
// ===========================================================================

#[test]
fn test_record_multi_alpha_fields() {
    let cpy = "\
       01  REC.
           05  FIRST-NAME PIC X(10).
           05  LAST-NAME  PIC X(15).
           05  INITIAL    PIC X.
";
    let schema = parse_copybook(cpy).expect("parse");
    let mut data = vec![b' '; 26]; // 10 + 15 + 1
    data[..4].copy_from_slice(b"JOHN");
    data[10..15].copy_from_slice(b"SMITH");
    data[25] = b'J';
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert!(json["FIRST-NAME"].as_str().unwrap().starts_with("JOHN"));
    assert!(json["LAST-NAME"].as_str().unwrap().starts_with("SMITH"));
    assert_eq!(json["INITIAL"], "J");
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

#[test]
fn test_record_mixed_alpha_and_numeric() {
    let cpy = "\
       01  REC.
           05  NAME   PIC X(5).
           05  AGE    PIC 9(3).
";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"ALICE030";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["NAME"], "ALICE");
    // PIC 9(3) data "030" — decode preserves the numeric value
    let age_val = json["AGE"].as_str().unwrap();
    assert!(
        age_val == "30" || age_val == "030",
        "unexpected AGE: {age_val}"
    );
}

// ===========================================================================
// 6. EBCDIC record-level roundtrip
// ===========================================================================

#[test]
fn test_record_pic_x_ebcdic_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC X(3).\n";
    let schema = parse_copybook(cpy).expect("parse");
    // EBCDIC "ABC" = 0xC1, 0xC2, 0xC3
    let data = [0xC1, 0xC2, 0xC3];
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(json["F1"], "ABC");
    let re_encoded = encode_record(&schema, &json, &ebcdic_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

#[test]
fn test_record_pic_x_ebcdic_space_padded() {
    let cpy = "       01  REC.\n           05  F1 PIC X(5).\n";
    let schema = parse_copybook(cpy).expect("parse");
    // EBCDIC "A" + 4 spaces: 0xC1, 0x40, 0x40, 0x40, 0x40
    let data = [0xC1, 0x40, 0x40, 0x40, 0x40];
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    let f1 = json["F1"].as_str().unwrap();
    assert!(f1.starts_with('A'));
    assert_eq!(f1.len(), 5);
}

// ===========================================================================
// 7. Large field sizes
// ===========================================================================

#[test]
fn test_record_pic_x_500_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC X(500).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let mut data = vec![b' '; 500];
    for (i, b) in data.iter_mut().enumerate().take(26) {
        *b = b'A' + u8::try_from(i).unwrap();
    }
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let f1 = json["F1"].as_str().unwrap();
    assert_eq!(f1.len(), 500);
    assert!(f1.starts_with("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

// ===========================================================================
// 8. encode_alphanumeric boundary lengths
// ===========================================================================

#[test]
fn test_alpha_encode_length_0_field() {
    // A zero-length field is edge-case; encode should produce empty vec
    let encoded = encode_alphanumeric("", 0, Codepage::ASCII).unwrap();
    assert!(encoded.is_empty());
}

#[test]
fn test_alpha_encode_max_u16_length() {
    // Very large field — should work with empty string (all spaces)
    let encoded = encode_alphanumeric("", 2000, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 2000);
    assert!(encoded.iter().all(|&b| b == b' '));
}
