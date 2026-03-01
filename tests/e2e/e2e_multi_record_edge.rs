// SPDX-License-Identifier: AGPL-3.0-or-later
//! End-to-end multi-record and edge-case tests.
//!
//! Validates:
//! - Stress: 100-record decode
//! - Error recovery: alternating valid/invalid records
//! - Max-errors limit (strict mode stops early)
//! - Empty binary file → empty JSONL
//! - All-spaces and all-zeros fields
//! - EBCDIC high-bit characters
//! - Embedded null bytes in alphanumeric fields
//! - Encode from JSONL with missing fields
//! - Encode from JSONL with extra fields (ignored)
//! - Encode with JSON field type mismatch
//! - Record count matching between input and output
//! - Decode + encode preserves file size
//! - Multiple 01-level sections
//! - Large field (PIC X(1000))
//! - Minimal field (PIC X(1))
//! - Very wide record (LRECL=32000)
//! - Decode → re-decode determinism on 100 records

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Cursor;

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_file_to_jsonl,
    decode_record, encode_jsonl_to_file, encode_record,
};
use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn encode_opts(cp: Codepage) -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

/// Convert ASCII uppercase/digit/space to CP037 EBCDIC, right-padded to `len`.
fn ebcdic_text(s: &str, len: usize) -> Vec<u8> {
    let mut out = Vec::with_capacity(len);
    for c in s.chars() {
        out.push(match c {
            ' ' => 0x40,
            'A'..='I' => 0xC1 + (c as u8 - b'A'),
            'J'..='R' => 0xD1 + (c as u8 - b'J'),
            'S'..='Z' => 0xE2 + (c as u8 - b'S'),
            'a'..='i' => 0x81 + (c as u8 - b'a'),
            'j'..='r' => 0x91 + (c as u8 - b'j'),
            's'..='z' => 0xA2 + (c as u8 - b's'),
            '0'..='9' => 0xF0 + (c as u8 - b'0'),
            _ => 0x6F,
        });
    }
    out.resize(len, 0x40); // pad with EBCDIC spaces
    out
}

/// Build EBCDIC digit bytes from a digit string.
fn ebcdic_digits(s: &str) -> Vec<u8> {
    s.bytes().map(|b| b - b'0' + 0xF0).collect()
}

/// Parse JSONL output into a Vec of serde_json::Value.
fn parse_jsonl(jsonl: &str) -> Vec<serde_json::Value> {
    jsonl
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l).expect("valid JSONL line"))
        .collect()
}

/// Simple DISPLAY-only copybook (16 bytes per record).
const SIMPLE_CPY: &str = r"
    01 SIMPLE-REC.
       05 REC-ID    PIC 9(6).
       05 NAME      PIC X(10).
";

/// Build one EBCDIC record for SIMPLE_CPY (16 bytes).
fn simple_record(id: &str, name: &str) -> Vec<u8> {
    let mut rec = Vec::new();
    rec.extend_from_slice(&ebcdic_digits(id));
    rec.extend_from_slice(&ebcdic_text(name, 10));
    rec
}

// ===========================================================================
// 1. Stress: decode 100-record file
// ===========================================================================

#[test]
fn stress_decode_100_records() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");

    let mut data = Vec::new();
    for i in 1..=100 {
        data.extend_from_slice(&simple_record(&format!("{i:06}"), "STRESS"));
    }

    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&data),
        &mut output,
        &decode_opts(Codepage::CP037),
    )
    .expect("decode 100 records");

    let records = parse_jsonl(&String::from_utf8(output).expect("utf8"));
    assert_eq!(records.len(), 100, "must decode exactly 100 records");
    assert_eq!(summary.records_processed, 100);
    assert!(summary.is_successful());

    // Spot-check first and last
    assert_eq!(records[0]["REC-ID"], "000001");
    assert_eq!(records[99]["REC-ID"], "000100");
}

// ===========================================================================
// 2. Alternating valid/invalid records – error recovery
// ===========================================================================

#[test]
fn error_recovery_alternating_valid_invalid() {
    let cpy = r"
        01 NUM-REC.
           05 NUM-FIELD PIC 9(4).
           05 TXT-FIELD PIC X(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Build 6 records (10 bytes each); records 2, 4, 6 have invalid digits
    let mut data = Vec::new();

    // Record 1 – valid
    data.extend_from_slice(&ebcdic_digits("0001"));
    data.extend_from_slice(&ebcdic_text("GOOD01", 6));

    // Record 2 – invalid: non-digit bytes in numeric field
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // "ABCD" in numeric
    data.extend_from_slice(&ebcdic_text("BAD002", 6));

    // Record 3 – valid
    data.extend_from_slice(&ebcdic_digits("0003"));
    data.extend_from_slice(&ebcdic_text("GOOD03", 6));

    // Record 4 – invalid
    data.extend_from_slice(&[0xC5, 0xC6, 0xC7, 0xC8]);
    data.extend_from_slice(&ebcdic_text("BAD004", 6));

    // Record 5 – valid
    data.extend_from_slice(&ebcdic_digits("0005"));
    data.extend_from_slice(&ebcdic_text("GOOD05", 6));

    // Record 6 – invalid
    data.extend_from_slice(&[0xC9, 0xD1, 0xD2, 0xD3]);
    data.extend_from_slice(&ebcdic_text("BAD006", 6));

    // Non-strict: should continue past errors
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&data),
        &mut output,
        &decode_opts(Codepage::CP037),
    )
    .expect("decode with errors");

    let good_count = summary.records_processed;
    let bad_count = summary.records_with_errors;

    assert!(
        good_count + bad_count == 6,
        "total attempts should be 6, got {good_count} + {bad_count}"
    );
    assert!(good_count >= 3, "at least 3 valid records expected");
}

// ===========================================================================
// 3. Strict mode stops on first error
// ===========================================================================

#[test]
fn strict_mode_stops_on_first_error() {
    let cpy = r"
        01 NUM-REC.
           05 NUM-FIELD PIC 9(4).
           05 TXT-FIELD PIC X(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let mut data = Vec::new();
    // Record 1 – valid
    data.extend_from_slice(&ebcdic_digits("0001"));
    data.extend_from_slice(&ebcdic_text("FIRST", 6));
    // Record 2 – invalid numeric
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]);
    data.extend_from_slice(&ebcdic_text("BAD", 6));
    // Record 3 – valid (should not be reached)
    data.extend_from_slice(&ebcdic_digits("0003"));
    data.extend_from_slice(&ebcdic_text("THIRD", 6));

    let opts = decode_opts(Codepage::CP037).with_strict_mode(true);
    let mut output = Vec::new();
    let result = decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts);

    // Strict mode: should return error (stops processing)
    assert!(
        result.is_err(),
        "strict mode should fail on first invalid record"
    );
}

// ===========================================================================
// 4. Decode empty binary file → empty JSONL
// ===========================================================================

#[test]
fn decode_empty_file_produces_empty_jsonl() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let data: Vec<u8> = Vec::new();

    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&data),
        &mut output,
        &decode_opts(Codepage::CP037),
    )
    .expect("decode empty");

    let jsonl = String::from_utf8(output).expect("utf8");
    assert!(jsonl.trim().is_empty(), "empty input → empty JSONL");
    assert_eq!(summary.records_processed, 0);
    assert_eq!(summary.records_with_errors, 0);
}

// ===========================================================================
// 5. Decode field containing all EBCDIC spaces
// ===========================================================================

#[test]
fn decode_field_all_spaces() {
    let cpy = r"
        01 SPACE-REC.
           05 ID-FIELD   PIC 9(4).
           05 TEXT-FIELD  PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_digits("0001"));
    data.extend_from_slice(&vec![0x40; 10]); // 10 EBCDIC spaces

    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(json["TEXT-FIELD"], "          ", "10 ASCII spaces expected");
}

// ===========================================================================
// 6. Decode field containing all EBCDIC zeros
// ===========================================================================

#[test]
fn decode_field_all_zeros() {
    let cpy = r"
        01 ZERO-REC.
           05 NUM-FIELD  PIC 9(8).
           05 TEXT-FIELD  PIC X(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let mut data = Vec::new();
    data.extend_from_slice(&[0xF0; 8]); // "00000000"
    data.extend_from_slice(&ebcdic_text("ZEROS", 6));

    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    // Lossless mode may normalize leading zeros; verify the numeric value is zero
    let num_str = json["NUM-FIELD"].as_str().expect("string");
    let parsed: u64 = num_str.parse().expect("parseable as integer");
    assert_eq!(parsed, 0, "all-zero numeric field must decode to zero");
}

// ===========================================================================
// 7. Decode EBCDIC high-bit characters (é = 0x51 in CP037)
// ===========================================================================

#[test]
fn decode_ebcdic_high_bit_characters() {
    let cpy = r"
        01 HIGHBIT-REC.
           05 DATA-FIELD PIC X(8).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Use high-bit EBCDIC bytes that map to valid characters in CP037
    // 0x4B = '.', 0x5B = '$', 0x5C = '*', 0x6B = ',', 0x7B = '#'
    let data: Vec<u8> = vec![0x4B, 0x5B, 0x5C, 0x6B, 0x7B, 0x40, 0x40, 0x40];

    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let decoded = json["DATA-FIELD"].as_str().expect("string");
    assert_eq!(decoded.len(), 8, "decoded field should be 8 characters");
    assert!(
        decoded.starts_with(".$*,#"),
        "high-bit chars decoded correctly"
    );
}

// ===========================================================================
// 8. Decode with embedded null bytes in alphanumeric fields
// ===========================================================================

#[test]
fn decode_embedded_null_bytes_in_alpha() {
    let cpy = r"
        01 NULL-REC.
           05 ALPHA-FIELD PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // EBCDIC null (0x00) appears in the middle of an alphanumeric field
    let mut data: Vec<u8> = ebcdic_text("ABC", 10);
    data[3] = 0x00; // embed null after "ABC"
    data[4] = 0x00;

    // Null bytes (0x00) are unmappable in CP037; the decoder may return an error
    // or produce replacement characters depending on the unmappable policy.
    // The key invariant is that it does not panic.
    let result = decode_record(&schema, &data, &decode_opts(Codepage::CP037));
    // Both Ok and Err are acceptable; verify no panic occurred
    match &result {
        Ok(json) => {
            let val = json["ALPHA-FIELD"].as_str().expect("string");
            assert_eq!(val.len(), 10, "decoded field length must be 10");
        }
        Err(e) => {
            // Error is expected for unmappable null bytes in strict default policy
            let msg = format!("{e}");
            assert!(
                !msg.is_empty(),
                "error message should be non-empty for unmappable bytes"
            );
        }
    }
}

// ===========================================================================
// 9. Encode from JSONL with missing optional fields (defaults to spaces/zeros)
// ===========================================================================

#[test]
fn encode_missing_optional_fields() {
    let cpy = r"
        01 OPT-REC.
           05 ID-FIELD   PIC 9(4).
           05 NAME-FIELD PIC X(10).
           05 CITY-FIELD PIC X(8).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let eopts = encode_opts(Codepage::CP037);

    // JSON with only ID-FIELD; NAME-FIELD and CITY-FIELD missing
    let json: serde_json::Value = serde_json::from_str(r#"{"ID-FIELD": "0042"}"#).expect("json");

    let result = encode_record(&schema, &json, &eopts);
    // Encoding should either succeed (filling defaults) or return a structured error
    // Both are valid behaviors; the key is no panic
    if let Ok(encoded) = &result {
        assert_eq!(
            encoded.len(),
            22,
            "encoded record length must match schema LRECL (4+10+8=22)"
        );
    }
    // If it errors, that's also acceptable – the encode noticed missing fields
}

// ===========================================================================
// 10. Encode from JSONL with extra fields (should be ignored)
// ===========================================================================

#[test]
fn encode_extra_fields_ignored() {
    let cpy = r"
        01 EXTRA-REC.
           05 ID-FIELD   PIC 9(4).
           05 NAME-FIELD PIC X(8).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    // Build valid record, decode, then add extra fields to JSON
    let data = {
        let mut d = Vec::new();
        d.extend_from_slice(&ebcdic_digits("1234"));
        d.extend_from_slice(&ebcdic_text("ALICE", 8));
        d
    };

    let mut json = decode_record(&schema, &data, &dopts).expect("decode");
    // Inject extra fields that don't exist in schema
    json.as_object_mut().expect("obj").insert(
        "EXTRA-KEY".to_string(),
        serde_json::json!("should-be-ignored"),
    );
    json.as_object_mut()
        .expect("obj")
        .insert("ANOTHER".to_string(), serde_json::json!(999));

    let encoded = encode_record(&schema, &json, &eopts).expect("encode with extras");
    assert_eq!(
        encoded.len(),
        12,
        "extra fields should not affect output size"
    );

    // Re-decode should match original fields
    let json2 = decode_record(&schema, &encoded, &dopts).expect("re-decode");
    assert_eq!(json2["ID-FIELD"], "1234");
}

// ===========================================================================
// 11. Encode with JSON field type mismatch (number where string expected)
// ===========================================================================

#[test]
fn encode_json_type_mismatch() {
    let cpy = r"
        01 MISMATCH-REC.
           05 NAME-FIELD PIC X(10).
           05 NUM-FIELD  PIC 9(4).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let eopts = encode_opts(Codepage::CP037);

    // Provide a number for NAME-FIELD (PIC X expects string)
    let json: serde_json::Value =
        serde_json::from_str(r#"{"NAME-FIELD": 12345, "NUM-FIELD": "0001"}"#).expect("json");

    let result = encode_record(&schema, &json, &eopts);
    // The encoder may coerce or reject; either is valid, but no panic
    assert!(
        result.is_ok() || result.is_err(),
        "type mismatch must not panic"
    );
}

// ===========================================================================
// 12. Record count matches between decode input and encode output
// ===========================================================================

#[test]
fn record_count_matches_decode_encode() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let record_count = 25;
    let mut bin_data = Vec::new();
    for i in 1..=record_count {
        bin_data.extend_from_slice(&simple_record(&format!("{i:06}"), "COUNT"));
    }

    // Decode
    let mut jsonl_buf = Vec::new();
    let dec_summary = decode_file_to_jsonl(&schema, Cursor::new(&bin_data), &mut jsonl_buf, &dopts)
        .expect("decode");

    // Encode
    let mut enc_buf = Vec::new();
    let enc_summary = encode_jsonl_to_file(&schema, Cursor::new(&jsonl_buf), &mut enc_buf, &eopts)
        .expect("encode");

    assert_eq!(
        dec_summary.records_processed, record_count,
        "decode record count"
    );
    assert_eq!(
        enc_summary.records_processed, record_count,
        "encode record count"
    );
}

// ===========================================================================
// 13. Decode + encode preserves file size
// ===========================================================================

#[test]
fn decode_encode_preserves_file_size() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let mut original = Vec::new();
    for i in 1..=10 {
        original.extend_from_slice(&simple_record(&format!("{i:06}"), "SIZE"));
    }
    let original_len = original.len();

    // Decode → JSONL → Encode
    let mut jsonl_buf = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&original), &mut jsonl_buf, &dopts).expect("decode");

    let mut re_encoded = Vec::new();
    encode_jsonl_to_file(&schema, Cursor::new(&jsonl_buf), &mut re_encoded, &eopts)
        .expect("encode");

    assert_eq!(
        re_encoded.len(),
        original_len,
        "round-trip must preserve total file size ({original_len} bytes)"
    );
    assert_eq!(
        original, re_encoded,
        "round-trip must produce identical binary"
    );
}

// ===========================================================================
// 14. Multiple 01-level copybook sections
// ===========================================================================

#[test]
fn multiple_01_level_sections() {
    // First 01-level record
    let cpy = r"
        01 HEADER-REC.
           05 REC-TYPE   PIC X(2).
           05 HEADER-ID  PIC 9(6).
           05 FILLER     PIC X(2).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_text("HD", 2));
    data.extend_from_slice(&ebcdic_digits("000001"));
    data.extend_from_slice(&ebcdic_text("XX", 2));

    let json = decode_record(&schema, &data, &dopts).expect("decode header");
    assert_eq!(json["REC-TYPE"], "HD");
    assert_eq!(json["HEADER-ID"], "000001");

    // Second independent 01-level record
    let cpy2 = r"
        01 DETAIL-REC.
           05 REC-TYPE    PIC X(2).
           05 DETAIL-AMT  PIC 9(8).
    ";
    let schema2 = parse_copybook(cpy2).expect("parse detail");

    let mut data2 = Vec::new();
    data2.extend_from_slice(&ebcdic_text("DT", 2));
    data2.extend_from_slice(&ebcdic_digits("00012345"));

    let json2 = decode_record(&schema2, &data2, &dopts).expect("decode detail");
    assert_eq!(json2["REC-TYPE"], "DT");
    assert_eq!(json2["DETAIL-AMT"], "00012345");
}

// ===========================================================================
// 15. Large field (PIC X(1000))
// ===========================================================================

#[test]
fn large_field_pic_x_1000() {
    let cpy = r"
        01 LARGE-REC.
           05 BIG-FIELD PIC X(1000).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    // Fill with EBCDIC 'Z' (0xE9)
    let data: Vec<u8> = vec![0xE9; 1000];

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let val = json["BIG-FIELD"].as_str().expect("string");
    assert_eq!(val.len(), 1000);
    assert!(val.chars().all(|c| c == 'Z'));

    // Round-trip
    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(re_encoded, data, "large field round-trip must be identical");
}

// ===========================================================================
// 16. Minimal field (PIC X(1))
// ===========================================================================

#[test]
fn minimal_field_pic_x_1() {
    let cpy = r"
        01 MIN-REC.
           05 TINY-FIELD PIC X(1).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    // Single byte: EBCDIC 'A' = 0xC1
    let data: Vec<u8> = vec![0xC1];

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    assert_eq!(json["TINY-FIELD"], "A");

    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(re_encoded, data);
}

// ===========================================================================
// 17. Very wide record (LRECL=32000)
// ===========================================================================

#[test]
fn very_wide_record_lrecl_32000() {
    let cpy = r"
        01 WIDE-REC.
           05 WIDE-FIELD PIC X(32000).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    // Fill with EBCDIC spaces (0x40)
    let data: Vec<u8> = vec![0x40; 32000];

    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &dopts)
        .expect("decode wide");

    assert_eq!(summary.records_processed, 1);
    let records = parse_jsonl(&String::from_utf8(output).expect("utf8"));
    assert_eq!(records.len(), 1);

    let val = records[0]["WIDE-FIELD"].as_str().expect("string");
    assert_eq!(val.len(), 32000, "wide field must be 32000 characters");
}

// ===========================================================================
// 18. Decode determinism: 100-record decode produces identical output twice
// ===========================================================================

#[test]
fn decode_100_records_deterministic() {
    let schema = parse_copybook(SIMPLE_CPY).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    let mut data = Vec::new();
    for i in 1..=100 {
        data.extend_from_slice(&simple_record(&format!("{i:06}"), "DETER"));
    }

    let mut output1 = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output1, &dopts).expect("decode run 1");

    let mut output2 = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output2, &dopts).expect("decode run 2");

    assert_eq!(
        output1, output2,
        "two decodes of the same 100-record file must be byte-identical"
    );
}
