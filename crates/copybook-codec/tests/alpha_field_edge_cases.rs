// SPDX-License-Identifier: AGPL-3.0-or-later
//! Alphanumeric field edge-case tests for PIC X / PIC A fields.
//!
//! Covers:
//! 1. Full-width fields (exactly fills PIC size)
//! 2. Right-padded with spaces
//! 3. Empty fields (all spaces)
//! 4. Special characters in alpha fields
//! 5. EBCDIC-specific characters and encoding
//! 6. Large fields (PIC X(1000))
//! 7. Single-character fields (PIC X)
//! 8. Mixed alphanumeric + numeric records
//! 9. Encode rejection for too-long values
//! 10. EBCDIC special character round-trips

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::encode_alphanumeric;
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;
use serde_json::json;

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

fn alpha_schema(size: usize) -> copybook_core::Schema {
    let cpy = format!("       01 REC.\n           05 FLD PIC X({size}).");
    parse_copybook(&cpy).expect("schema should parse")
}

// ===========================================================================
// 1. Full-width fields (exactly fills PIC size)
// ===========================================================================

#[test]
fn test_alpha_full_width_5_chars() {
    let schema = alpha_schema(5);
    let json = decode_record(&schema, b"HELLO", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "HELLO");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, b"HELLO");
}

#[test]
fn test_alpha_full_width_10_chars() {
    let schema = alpha_schema(10);
    let data = b"ABCDEFGHIJ";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "ABCDEFGHIJ");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_full_width_1_char() {
    let schema = alpha_schema(1);
    let json = decode_record(&schema, b"Z", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "Z");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, b"Z");
}

// ===========================================================================
// 2. Right-padded with spaces
// ===========================================================================

#[test]
fn test_alpha_short_value_right_padded() {
    let schema = alpha_schema(10);
    let mut data = vec![b' '; 10];
    data[..3].copy_from_slice(b"HI!");
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let val = json["FLD"].as_str().unwrap();
    assert_eq!(val.len(), 10);
    assert!(val.starts_with("HI!"));
}

#[test]
fn test_alpha_encode_short_value_pads_right() {
    let schema = alpha_schema(8);
    let json_in = json!({"FLD": "AB"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded.len(), 8);
    assert_eq!(&encoded[..2], b"AB");
    assert!(encoded[2..].iter().all(|&b| b == b' '));
}

#[test]
fn test_alpha_single_char_in_large_field() {
    let schema = alpha_schema(50);
    let json_in = json!({"FLD": "X"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded.len(), 50);
    assert_eq!(encoded[0], b'X');
    assert!(encoded[1..].iter().all(|&b| b == b' '));
}

// ===========================================================================
// 3. Empty fields (all spaces)
// ===========================================================================

#[test]
fn test_alpha_all_spaces_decode() {
    let schema = alpha_schema(5);
    let json = decode_record(&schema, b"     ", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "     ");
}

#[test]
fn test_alpha_all_spaces_roundtrip() {
    let schema = alpha_schema(8);
    let data = b"        ";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_encode_empty_string() {
    let schema = alpha_schema(5);
    let json_in = json!({"FLD": ""});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, b"     ");
}

// ===========================================================================
// 4. Special characters in alpha fields
// ===========================================================================

#[test]
fn test_alpha_special_chars_punctuation() {
    let schema = alpha_schema(10);
    let data = b"@#$%&*!?.," as &[u8];
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "@#$%&*!?.,");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_digits_in_pic_x() {
    let schema = alpha_schema(5);
    let data = b"12345";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "12345");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_mixed_case() {
    let schema = alpha_schema(8);
    let data = b"AbCdEfGh";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "AbCdEfGh");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_hyphens_and_underscores() {
    let schema = alpha_schema(10);
    let data = b"FIRST-NAME";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "FIRST-NAME");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

// ===========================================================================
// 5. EBCDIC-specific characters and encoding
// ===========================================================================

#[test]
fn test_alpha_ebcdic_uppercase_roundtrip() {
    let schema = alpha_schema(3);
    // EBCDIC A=0xC1, B=0xC2, C=0xC3
    let data = [0xC1, 0xC2, 0xC3];
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "ABC");
    let re = encode_record(&schema, &json, &ebcdic_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_ebcdic_space_is_0x40() {
    let schema = alpha_schema(4);
    let data = [0xC1, 0x40, 0x40, 0x40]; // "A" + 3 spaces in EBCDIC
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    let val = json["FLD"].as_str().unwrap();
    assert!(val.starts_with('A'));
    assert_eq!(val.len(), 4);
    let re = encode_record(&schema, &json, &ebcdic_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_ebcdic_all_spaces() {
    let schema = alpha_schema(5);
    let data = [0x40; 5]; // 5 EBCDIC spaces
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "     ");
}

#[test]
fn test_alpha_ebcdic_digits_roundtrip() {
    let schema = alpha_schema(3);
    // EBCDIC digits: 0=0xF0, 1=0xF1, 2=0xF2
    let data = [0xF0, 0xF1, 0xF2];
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "012");
    let re = encode_record(&schema, &json, &ebcdic_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_ebcdic_encode_pads_with_0x40() {
    let encoded = encode_alphanumeric("A", 4, Codepage::CP037).unwrap();
    assert_eq!(encoded.len(), 4);
    // Padding bytes should be EBCDIC space 0x40
    assert_eq!(encoded[1], 0x40);
    assert_eq!(encoded[2], 0x40);
    assert_eq!(encoded[3], 0x40);
}

// ===========================================================================
// 6. Large fields (PIC X(1000))
// ===========================================================================

#[test]
fn test_alpha_large_field_roundtrip() {
    let schema = alpha_schema(500);
    let mut data = vec![b' '; 500];
    for (i, b) in data.iter_mut().enumerate().take(26) {
        *b = b'A' + u8::try_from(i).unwrap();
    }
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    let val = json["FLD"].as_str().unwrap();
    assert_eq!(val.len(), 500);
    assert!(val.starts_with("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_large_field_all_spaces() {
    let schema = alpha_schema(1000);
    let data = vec![b' '; 1000];
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"].as_str().unwrap().len(), 1000);
}

// ===========================================================================
// 7. Encode API edge cases
// ===========================================================================

#[test]
fn test_encode_alphanumeric_exact_length() {
    let encoded = encode_alphanumeric("ABCDE", 5, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"ABCDE");
}

#[test]
fn test_encode_alphanumeric_too_long_rejected() {
    let result = encode_alphanumeric("TOOLONGTEXT", 5, Codepage::ASCII);
    assert!(result.is_err());
}

#[test]
fn test_encode_alphanumeric_empty_to_spaces() {
    let encoded = encode_alphanumeric("", 10, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 10);
    assert!(encoded.iter().all(|&b| b == b' '));
}

#[test]
fn test_encode_alphanumeric_zero_length() {
    let encoded = encode_alphanumeric("", 0, Codepage::ASCII).unwrap();
    assert!(encoded.is_empty());
}

// ===========================================================================
// 8. Multi-field records with alpha fields
// ===========================================================================

#[test]
fn test_multi_field_alpha_record() {
    let cpy = "\
       01  REC.
           05  FIRST-NAME PIC X(10).
           05  LAST-NAME  PIC X(15).
           05  INITIAL    PIC X.
";
    let schema = parse_copybook(cpy).expect("parse");
    let mut data = vec![b' '; 26];
    data[..4].copy_from_slice(b"JOHN");
    data[10..15].copy_from_slice(b"SMITH");
    data[25] = b'J';
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert!(json["FIRST-NAME"].as_str().unwrap().starts_with("JOHN"));
    assert!(json["LAST-NAME"].as_str().unwrap().starts_with("SMITH"));
    assert_eq!(json["INITIAL"], "J");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_mixed_alpha_numeric_record() {
    let cpy = "\
       01  REC.
           05  NAME   PIC X(8).
           05  AGE    PIC 9(3).
           05  CITY   PIC X(10).
";
    let schema = parse_copybook(cpy).expect("parse");
    let mut data = vec![b' '; 21]; // 8 + 3 + 10
    data[..5].copy_from_slice(b"ALICE");
    data[8..11].copy_from_slice(b"030");
    data[11..17].copy_from_slice(b"BOSTON");
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert!(json["NAME"].as_str().unwrap().starts_with("ALICE"));
    assert!(json["CITY"].as_str().unwrap().starts_with("BOSTON"));
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_multi_field_ebcdic_alpha_roundtrip() {
    let cpy = "\
       01  REC.
           05  CODE PIC X(3).
           05  DESC PIC X(10).
";
    let schema = parse_copybook(cpy).expect("parse");
    // EBCDIC: "ABC" + "HELLO     " (padded with 0x40)
    let mut data = vec![0x40u8; 13];
    data[0] = 0xC1; // A
    data[1] = 0xC2; // B
    data[2] = 0xC3; // C
    data[3] = 0xC8; // H
    data[4] = 0xC5; // E
    data[5] = 0xD3; // L
    data[6] = 0xD3; // L
    data[7] = 0xD6; // O
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(json["CODE"], "ABC");
    assert!(json["DESC"].as_str().unwrap().starts_with("HELLO"));
    let re = encode_record(&schema, &json, &ebcdic_encode_opts()).unwrap();
    assert_eq!(re, data);
}

// ===========================================================================
// 9. Boundary and identity tests
// ===========================================================================

#[test]
fn test_alpha_all_same_character() {
    let schema = alpha_schema(5);
    let data = b"AAAAA";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "AAAAA");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_alpha_bytes_identity_roundtrip() {
    let schema = alpha_schema(10);
    let data = b"TEST DATA!";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data, "encode(decode(bytes)) must equal original bytes");
}

#[test]
fn test_alpha_numeric_string_in_pic_x() {
    let schema = alpha_schema(10);
    let data = b"2024-01-15";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "2024-01-15");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}
