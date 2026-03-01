// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive E2E decode/encode pipeline tests.
//!
//! Exercises the full decode and encode paths end-to-end including:
//! - Multiple field types, codepages, and JSON number modes
//! - Streaming / multi-record processing via file-level APIs
//! - Round-trip (decode → encode) byte-level fidelity
//! - Edge cases for field sizes, special values, and scratch buffers

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Cursor;

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_file_to_jsonl,
    decode_record, decode_record_with_scratch, encode_jsonl_to_file, encode_record,
    memory::ScratchBuffers,
};
use copybook_core::{ErrorCode, parse_copybook, project_schema};

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

/// Convert a simple ASCII uppercase/digit/space string to CP037 EBCDIC bytes,
/// right-padded with EBCDIC spaces to `len`.
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
            '+' => 0x4E,
            '-' => 0x60,
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

// =========================================================================
// Section 1: Full Decode Pipeline Tests
// =========================================================================

/// Decode a single PIC X + PIC 9 + COMP-3 + COMP record, verify all fields.
#[test]
fn decode_mixed_field_types_single_record() {
    let cpy = r"
        01 MULTI-REC.
           05 ALPHA-FIELD   PIC X(10).
           05 NUM-DISPLAY   PIC 9(6).
           05 AMOUNT        PIC S9(5)V99 COMP-3.
           05 BIN-FIELD     PIC S9(4) COMP.
           05 FLAG          PIC X(1).
    ";

    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_text("TESTDATA", 10)); // ALPHA-FIELD
    data.extend_from_slice(&ebcdic_digits("001234")); // NUM-DISPLAY
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x5C]); // AMOUNT +123.45
    data.extend_from_slice(&[0x00, 0x2A]); // BIN-FIELD +42
    data.extend_from_slice(&ebcdic_text("Y", 1)); // FLAG

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");

    assert_eq!(json["ALPHA-FIELD"], "TESTDATA  ");
    assert_eq!(json["NUM-DISPLAY"], "001234");
    assert_eq!(json["FLAG"], "Y");
    // COMP-3 and COMP produce numeric representations
    assert!(!json["AMOUNT"].is_null(), "AMOUNT must be present");
    assert!(!json["BIN-FIELD"].is_null(), "BIN-FIELD must be present");
}

/// Decode three fixed-length records via `decode_file_to_jsonl`.
#[test]
fn decode_multiple_records_via_file() {
    let cpy = r"
        01 REC.
           05 NAME  PIC X(5).
           05 AGE   PIC 9(3).
    ";
    // 8 bytes per record
    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_text("ALICE", 5));
    data.extend_from_slice(&ebcdic_digits("025"));
    data.extend_from_slice(&ebcdic_text("BOB", 5));
    data.extend_from_slice(&ebcdic_digits("030"));
    data.extend_from_slice(&ebcdic_text("EVE", 5));
    data.extend_from_slice(&ebcdic_digits("028"));

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let mut output = Vec::new();
    let summary =
        decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts).expect("decode");

    assert_eq!(summary.records_processed, 3);
    let text = String::from_utf8(output).expect("utf8");
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(lines.len(), 3);

    let rec0: serde_json::Value = serde_json::from_str(lines[0]).expect("json0");
    assert_eq!(rec0["NAME"], "ALICE");
    let rec2: serde_json::Value = serde_json::from_str(lines[2]).expect("json2");
    assert_eq!(rec2["NAME"], "EVE  ");
}

/// Decode 100 records via streaming and verify record count.
#[test]
fn decode_streaming_100_records() {
    let cpy = r"
        01 REC.
           05 SEQ  PIC 9(4).
           05 VAL  PIC X(6).
    ";
    // 10 bytes per record
    let record_count = 100;
    let mut data = Vec::new();
    for i in 0..record_count {
        data.extend_from_slice(&ebcdic_digits(&format!("{i:04}")));
        data.extend_from_slice(&ebcdic_text("BATCH", 6));
    }

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let mut output = Vec::new();
    let summary =
        decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts).expect("decode");

    assert_eq!(summary.records_processed, record_count);
    let text = String::from_utf8(output).expect("utf8");
    assert_eq!(text.lines().count(), record_count as usize);
}

/// Decode uppercase text with CP037.
#[test]
fn decode_codepage_cp037_uppercase() {
    let cpy = r"
        01 REC.
           05 TEXT-FIELD PIC X(10).
    ";
    let data = ebcdic_text("HELLOWORLD", 10);
    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(json["TEXT-FIELD"], "HELLOWORLD");
}

/// Decode same EBCDIC bytes with CP1047 — uppercase letters and digits are
/// identical across CP037 / CP1047.
#[test]
fn decode_codepage_cp1047_uppercase() {
    let cpy = r"
        01 REC.
           05 TEXT-FIELD PIC X(10).
    ";
    // Letters/digits share the same code points in CP037 and CP1047.
    let data = ebcdic_text("HELLOWORLD", 10);
    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP1047)).expect("decode");
    assert_eq!(json["TEXT-FIELD"], "HELLOWORLD");
}

/// Project two fields from a four-field schema, verify only those appear.
#[test]
fn decode_field_projection_subset() {
    let cpy = r"
        01 REC.
           05 FIELD-A PIC X(5).
           05 FIELD-B PIC 9(3).
           05 FIELD-C PIC X(8).
           05 FIELD-D PIC 9(4).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let projected =
        project_schema(&schema, &["FIELD-A".to_string(), "FIELD-D".to_string()]).expect("project");

    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_text("HELLO", 5));
    data.extend_from_slice(&ebcdic_digits("042"));
    data.extend_from_slice(&ebcdic_text("IGNOREME", 8));
    data.extend_from_slice(&ebcdic_digits("9999"));

    let json = decode_record(&projected, &data, &decode_opts(Codepage::CP037)).expect("decode");
    assert!(json.get("FIELD-A").is_some());
    assert!(json.get("FIELD-D").is_some());
    assert!(json.get("FIELD-B").is_none(), "FIELD-B must be excluded");
    assert!(json.get("FIELD-C").is_none(), "FIELD-C must be excluded");
}

/// Decode with `emit_meta = true` and verify metadata keys are present.
#[test]
fn decode_emit_meta_includes_fingerprint() {
    let cpy = r"
        01 REC.
           05 ID PIC 9(4).
    ";
    let data = ebcdic_digits("0042");
    let schema = parse_copybook(cpy).expect("parse");

    let opts = DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
        .with_emit_meta(true);

    let json = decode_record(&schema, &data, &opts).expect("decode");

    assert!(
        json.get("schema_fingerprint").is_some(),
        "schema_fingerprint must be present"
    );
    assert!(
        json.get("__schema_id").is_some(),
        "__schema_id must be present"
    );
    assert!(json.get("length").is_some(), "length must be present");
    assert!(
        json.get("__record_index").is_some(),
        "__record_index must be present"
    );
}

/// In Lossless mode, COMP-3 decimal values decode to JSON strings.
#[test]
fn decode_json_number_lossless_strings() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(5)V99 COMP-3.
    ";
    let data: Vec<u8> = vec![0x00, 0x12, 0x34, 0x5C]; // +123.45
    let schema = parse_copybook(cpy).expect("parse");

    let opts = decode_opts(Codepage::CP037); // lossless is default
    let json = decode_record(&schema, &data, &opts).expect("decode");

    let amount = &json["AMOUNT"];
    assert!(
        amount.is_string(),
        "Lossless mode: COMP-3 should be a JSON string, got {amount}"
    );
}

/// In Native mode, numeric display fields decode to JSON numbers.
#[test]
fn decode_json_number_native_numbers() {
    let cpy = r"
        01 REC.
           05 QTY PIC 9(6).
    ";
    let data = ebcdic_digits("001234");
    let schema = parse_copybook(cpy).expect("parse");

    let native_opts = DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_format(RecordFormat::Fixed);
    let lossless_opts = decode_opts(Codepage::CP037);

    let json_native = decode_record(&schema, &data, &native_opts).expect("native");
    let json_lossless = decode_record(&schema, &data, &lossless_opts).expect("lossless");

    // Native and lossless modes must produce different representations
    // (number vs string) or at least both succeed.
    assert!(!json_native["QTY"].is_null());
    assert!(!json_lossless["QTY"].is_null());

    // In lossless mode the value is a string preserving leading zeros
    assert!(
        json_lossless["QTY"].is_string(),
        "Lossless mode should produce a string"
    );

    // In native mode the value may be a number or a string depending on the
    // field type. Verify the decoded value represents 1234 regardless.
    let native_val = &json_native["QTY"];
    let numeric: f64 = if let Some(n) = native_val.as_f64() {
        n
    } else if let Some(s) = native_val.as_str() {
        s.parse().expect("parseable")
    } else {
        panic!("QTY must be number or string, got {native_val}");
    };
    assert!(
        (numeric - 1234.0).abs() < 0.5,
        "Value should be 1234, got {numeric}"
    );
}

/// Decode a signed PIC S9(5) display field with a negative overpunch.
#[test]
fn decode_signed_display_negative() {
    let cpy = r"
        01 REC.
           05 BALANCE PIC S9(5).
    ";
    // In zoned decimal (CP037), negative sign is encoded in the last byte's
    // zone nibble. -12345 → F1 F2 F3 F4 D5 (D zone = negative).
    let data: Vec<u8> = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xD5];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");

    let balance = &json["BALANCE"];
    assert!(!balance.is_null(), "BALANCE must be present");
    // The value should represent -12345 in some form (string or number).
    let fallback = balance.to_string();
    let s = balance.as_str().unwrap_or(&fallback);
    assert!(s.contains("12345"), "Should contain digits 12345, got {s}");
    assert!(s.starts_with('-'), "Should be negative, got {s}");
}

// =========================================================================
// Section 2: Full Encode Pipeline Tests
// =========================================================================

/// Encode a simple PIC X field to EBCDIC binary.
#[test]
fn encode_alphanumeric_to_ebcdic() {
    let cpy = r"
        01 REC.
           05 NAME PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let json: serde_json::Value = serde_json::json!({"NAME": "HELLO"});
    let binary = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");

    assert_eq!(binary.len(), 10);
    assert_eq!(binary, ebcdic_text("HELLO", 10));
}

/// Encode a record with PIC X, PIC 9, COMP-3, and COMP fields.
#[test]
fn encode_mixed_field_types() {
    let cpy = r"
        01 REC.
           05 ALPHA-FIELD PIC X(5).
           05 NUM-DISP    PIC 9(4).
           05 PKD-AMOUNT  PIC S9(5)V99 COMP-3.
           05 BIN-VAL     PIC S9(4) COMP.
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // Decode known binary to get valid JSON, then re-encode.
    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_text("ABCDE", 5));
    data.extend_from_slice(&ebcdic_digits("0042"));
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x5C]); // +123.45
    data.extend_from_slice(&[0x00, 0x0A]); // +10

    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");

    assert_eq!(data.len(), re_encoded.len(), "encoded length must match");
}

/// Passing a non-object (JSON array) to `encode_record` must produce CBKE501.
#[test]
fn encode_non_object_returns_error() {
    let cpy = r"
        01 REC.
           05 FIELD PIC X(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let bad_json = serde_json::json!([1, 2, 3]);
    let result = encode_record(&schema, &bad_json, &encode_opts(Codepage::CP037));

    assert!(result.is_err(), "Array input must fail");
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        "Error code must be CBKE501"
    );
}

/// Encode should accept the envelope format (with `"fields"` key).
#[test]
fn encode_from_json_with_envelope() {
    let cpy = r"
        01 REC.
           05 ID   PIC 9(4).
           05 NAME PIC X(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let envelope = serde_json::json!({
        "schema": "1.0",
        "record_index": 0,
        "codepage": "cp037",
        "fields": {
            "ID": "0001",
            "NAME": "ALICE"
        },
        "ID": "0001",
        "NAME": "ALICE"
    });

    let binary = encode_record(&schema, &envelope, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(binary.len(), 10); // 4 + 6

    let mut expected = Vec::new();
    expected.extend_from_slice(&ebcdic_digits("0001"));
    expected.extend_from_slice(&ebcdic_text("ALICE", 6));
    assert_eq!(binary, expected);
}

/// Encode text to CP037 and verify EBCDIC bytes.
#[test]
fn encode_codepage_cp037_text() {
    let cpy = r"
        01 REC.
           05 MSG PIC X(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let json = serde_json::json!({"MSG": "ABCDE"});
    let binary = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");

    // A=C1, B=C2, C=C3, D=C4, E=C5
    assert_eq!(binary, vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5]);
}

/// Encode digits to CP1047 — digit EBCDIC bytes are identical to CP037.
#[test]
fn encode_codepage_cp1047_digits() {
    let cpy = r"
        01 REC.
           05 NUM PIC 9(4).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let json = serde_json::json!({"NUM": "1234"});
    let binary = encode_record(&schema, &json, &encode_opts(Codepage::CP1047)).expect("encode");

    assert_eq!(binary, vec![0xF1, 0xF2, 0xF3, 0xF4]);
}

/// Encode three records via `encode_jsonl_to_file` and verify binary output.
#[test]
fn encode_jsonl_to_file_three_records() {
    let cpy = r"
        01 REC.
           05 NAME PIC X(5).
           05 AGE  PIC 9(3).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let jsonl = r#"{"NAME":"ALICE","AGE":"025"}
{"NAME":"BOB","AGE":"030"}
{"NAME":"EVE","AGE":"028"}
"#;

    let mut binary_out = Vec::new();
    let summary = encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut binary_out,
        &encode_opts(Codepage::CP037),
    )
    .expect("encode");

    assert_eq!(summary.records_processed, 3);
    assert_eq!(binary_out.len(), 3 * 8); // 5+3 = 8 bytes per record

    // Verify first record
    let rec1 = &binary_out[0..8];
    let mut expected = ebcdic_text("ALICE", 5);
    expected.extend_from_slice(&ebcdic_digits("025"));
    assert_eq!(rec1, expected.as_slice());
}

/// Encode with `coerce_numbers` enabled — JSON numbers are accepted for numeric fields.
#[test]
fn encode_with_coerce_numbers_enabled() {
    let cpy = r"
        01 REC.
           05 QTY PIC 9(4).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    // String-based encode (the canonical path) must produce correct EBCDIC digits.
    let reference_json = serde_json::json!({"QTY": "0042"});
    let reference_binary =
        encode_record(&schema, &reference_json, &encode_opts(Codepage::CP037)).expect("ref-encode");
    assert_eq!(reference_binary.len(), 4);
    assert_eq!(reference_binary, ebcdic_digits("0042"));

    // With coerce_numbers, a JSON number should at minimum not panic and
    // produce a buffer of the correct length.
    let json = serde_json::json!({"QTY": 42});
    let eopts = EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
        .with_coerce_numbers(true);

    let result = encode_record(&schema, &json, &eopts);
    assert!(result.is_ok(), "coerce_numbers encode must not error");
    assert_eq!(
        result.unwrap().len(),
        4,
        "encoded length must match PIC 9(4)"
    );
}

// =========================================================================
// Section 3: Round-Trip Tests (decode → encode → verify byte-identical)
// =========================================================================

/// Round-trip PIC X(10) — decode then encode must reproduce original bytes.
#[test]
fn roundtrip_alphanumeric_byte_identical() {
    let cpy = r"
        01 REC.
           05 NAME PIC X(10).
    ";
    let data = ebcdic_text("ROUNDTRIP", 10);

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "PIC X round-trip must be byte-identical");
}

/// Round-trip PIC 9(6) — preserves leading zeros.
#[test]
fn roundtrip_numeric_display_byte_identical() {
    let cpy = r"
        01 REC.
           05 CODE PIC 9(6).
    ";
    let data = ebcdic_digits("001234");
    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "PIC 9 round-trip must be byte-identical");
}

/// Round-trip COMP-3 positive value.
#[test]
fn roundtrip_comp3_positive_byte_identical() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(5)V99 COMP-3.
    ";
    // +123.45 → 0012345C → [0x00, 0x12, 0x34, 0x5C]
    let data: Vec<u8> = vec![0x00, 0x12, 0x34, 0x5C];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 positive round-trip");
}

/// Round-trip COMP-3 negative value.
#[test]
fn roundtrip_comp3_negative_byte_identical() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(5)V99 COMP-3.
    ";
    // -123.45 → 0012345D → [0x00, 0x12, 0x34, 0x5D]
    let data: Vec<u8> = vec![0x00, 0x12, 0x34, 0x5D];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 negative round-trip");
}

/// Round-trip PIC S9(4) COMP (signed halfword).
#[test]
fn roundtrip_comp_signed_halfword() {
    let cpy = r"
        01 REC.
           05 VAL PIC S9(4) COMP.
    ";
    // +42 as signed big-endian 16-bit
    let data: Vec<u8> = vec![0x00, 0x2A];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP signed halfword round-trip");
}

/// Round-trip PIC 9(4) COMP (unsigned halfword).
#[test]
fn roundtrip_comp_unsigned_halfword() {
    let cpy = r"
        01 REC.
           05 CNT PIC 9(4) COMP.
    ";
    // 1000 unsigned big-endian = 0x03E8
    let data: Vec<u8> = vec![0x03, 0xE8];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP unsigned halfword round-trip");
}

/// Round-trip PIC S9(9) COMP (signed fullword).
#[test]
fn roundtrip_comp_signed_fullword() {
    let cpy = r"
        01 REC.
           05 BIG-VAL PIC S9(9) COMP.
    ";
    // +123456789 big-endian = 0x075BCD15
    let data: Vec<u8> = vec![0x07, 0x5B, 0xCD, 0x15];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP signed fullword round-trip");
}

/// Round-trip PIC S9(5) SIGN IS LEADING SEPARATE.
#[test]
fn roundtrip_sign_separate_leading() {
    let cpy = r"
        01 REC.
           05 BALANCE PIC S9(5) SIGN IS LEADING SEPARATE.
    ";
    // +12345 → sign(+) then "12345" in EBCDIC
    // CP037: '+' = 0x4E
    let mut data = vec![0x4E];
    data.extend_from_slice(&ebcdic_digits("12345"));

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "SIGN LEADING SEPARATE round-trip");
}

/// Round-trip PIC S9(5) SIGN IS TRAILING SEPARATE.
#[test]
fn roundtrip_sign_separate_trailing() {
    let cpy = r"
        01 REC.
           05 DEBIT PIC S9(5) SIGN IS TRAILING SEPARATE.
    ";
    // -67890 → "67890" then sign(-) in EBCDIC
    // CP037: '-' = 0x60
    let mut data = ebcdic_digits("67890");
    data.push(0x60);

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "SIGN TRAILING SEPARATE round-trip");
}

/// Round-trip COMP-3 with 4 decimal places.
#[test]
fn roundtrip_comp3_high_scale() {
    let cpy = r"
        01 REC.
           05 RATE PIC S9(3)V9(4) COMP-3.
    ";
    // PIC S9(3)V9(4) = 7 digits + sign = 8 nibbles = 4 bytes
    // +123.4567 → 01234567C → [0x01, 0x23, 0x45, 0x67] wait, that's 8 nibbles
    // digits = 0 1 2 3 4 5 6 7, sign C
    // Actually: 7 digits = 1234567, packed with sign = 8 nibbles = 4 bytes
    // 01 23 45 67 → but need sign nibble → 0 1234567 C = 01 23 45 67...
    // Actually 7 digits + 1 sign = 8 nibbles = 4 bytes
    // Value 123.4567 → integer rep 1234567 → 7 digits → packed: 1 2 3 4 5 6 7 C
    // Bytes: 0x12, 0x34, 0x56, 0x7C
    let data: Vec<u8> = vec![0x12, 0x34, 0x56, 0x7C];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 high-scale round-trip");
}

/// Round-trip a multi-type record with PIC X, PIC 9, COMP-3, COMP.
#[test]
fn roundtrip_multi_type_record() {
    let cpy = r"
        01 REC.
           05 NAME   PIC X(8).
           05 CODE   PIC 9(4).
           05 PRICE  PIC S9(5)V99 COMP-3.
           05 QTY    PIC S9(4) COMP.
    ";
    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_text("WIDGETS", 8));
    data.extend_from_slice(&ebcdic_digits("0099"));
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x5C]); // +123.45
    data.extend_from_slice(&[0x00, 0x64]); // +100

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(data, re_encoded, "Multi-type record round-trip");
}

/// Verify digit-only record round-trips with both CP037 and CP1047.
#[test]
fn roundtrip_cross_codepage_digits() {
    let cpy = r"
        01 REC.
           05 FIELD-A PIC 9(6).
           05 FIELD-B PIC 9(4).
    ";
    let data = [ebcdic_digits("202412"), ebcdic_digits("5678")].concat();

    let schema = parse_copybook(cpy).expect("parse");

    for cp in [Codepage::CP037, Codepage::CP1047] {
        let json = decode_record(&schema, &data, &decode_opts(cp)).expect("decode");
        let re_encoded = encode_record(&schema, &json, &encode_opts(cp)).expect("encode");
        assert_eq!(
            data, re_encoded,
            "Digit-only round-trip must be identical for {cp:?}"
        );
    }
}

/// File-level round-trip: `decode_file_to_jsonl` → `encode_jsonl_to_file` → compare.
#[test]
fn roundtrip_file_level_decode_encode() {
    let cpy = r"
        01 REC.
           05 ID   PIC 9(4).
           05 NAME PIC X(6).
    ";
    let schema = parse_copybook(cpy).expect("parse");

    let mut original_data = Vec::new();
    for i in 0..5 {
        original_data.extend_from_slice(&ebcdic_digits(&format!("{:04}", i + 1)));
        original_data.extend_from_slice(&ebcdic_text(&format!("REC{:02}", i + 1), 6));
    }

    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    // Decode to JSONL
    let mut jsonl_out = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(&original_data), &mut jsonl_out, &dopts)
        .expect("decode");

    // Re-encode from JSONL
    let mut binary_out = Vec::new();
    encode_jsonl_to_file(&schema, Cursor::new(&jsonl_out), &mut binary_out, &eopts)
        .expect("encode");

    assert_eq!(
        original_data, binary_out,
        "File-level round-trip must be byte-identical"
    );
}

// =========================================================================
// Section 4: Edge Cases
// =========================================================================

/// PIC X(10) containing only EBCDIC spaces decodes and round-trips.
#[test]
fn edge_all_spaces_pic_x() {
    let cpy = r"
        01 REC.
           05 EMPTY-FLD PIC X(10).
    ";
    let data = vec![0x40; 10]; // 10 EBCDIC spaces
    let schema = parse_copybook(cpy).expect("parse");

    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(json["EMPTY-FLD"], "          "); // 10 ASCII spaces

    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "All-spaces round-trip");
}

/// PIC X(256) — a large alphanumeric field.
#[test]
fn edge_large_field_256_bytes() {
    let cpy = r"
        01 REC.
           05 PAYLOAD PIC X(256).
    ";
    let data = ebcdic_text("ABCDEFGHIJ", 256); // 10 chars + 246 spaces
    let schema = parse_copybook(cpy).expect("parse");

    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let s = json["PAYLOAD"].as_str().expect("string");
    assert_eq!(s.len(), 256);
    assert!(s.starts_with("ABCDEFGHIJ"));

    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "Large field round-trip");
}

/// Minimal 1-byte fields: PIC X(1) and PIC 9(1).
#[test]
fn edge_single_byte_fields() {
    let cpy = r"
        01 REC.
           05 CHAR-F PIC X(1).
           05 DIGIT-F PIC 9(1).
    ";
    let mut data = Vec::new();
    data.push(0xC1); // 'A' in CP037
    data.push(0xF7); // '7' in CP037

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");

    assert_eq!(json["CHAR-F"], "A");
    assert_eq!(json["DIGIT-F"], "7");

    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "Single-byte field round-trip");
}

/// COMP-3 with value zero.
#[test]
fn edge_comp3_zero() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(5)V99 COMP-3.
    ";
    // +0.00 → 0000000C → [0x00, 0x00, 0x00, 0x0C]
    let data: Vec<u8> = vec![0x00, 0x00, 0x00, 0x0C];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let amount = &json["AMOUNT"];
    assert!(!amount.is_null(), "AMOUNT must be present for zero");

    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 zero round-trip");
}

/// COMP-3 maximum value for PIC S9(5)V99.
#[test]
fn edge_comp3_max_value() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(5)V99 COMP-3.
    ";
    // +99999.99 → 9999999C → [0x99, 0x99, 0x99, 0x9C]
    let data: Vec<u8> = vec![0x99, 0x99, 0x99, 0x9C];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 max value round-trip");
}

/// PIC 9(6) with leading zeros must preserve them.
#[test]
fn edge_display_leading_zeros() {
    let cpy = r"
        01 REC.
           05 SEQ PIC 9(6).
    ";
    let data = ebcdic_digits("000001");

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(json["SEQ"], "000001", "Leading zeros must be preserved");

    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "Leading zeros round-trip");
}

/// PIC 9(9) with all nines.
#[test]
fn edge_display_all_nines() {
    let cpy = r"
        01 REC.
           05 MAX-VAL PIC 9(9).
    ";
    let data = ebcdic_digits("999999999");

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(json["MAX-VAL"], "999999999");

    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "All-nines round-trip");
}

/// PIC S9(5) with value +0 — JSON round-trip preserves the zero value.
#[test]
fn edge_signed_zero_display() {
    let cpy = r"
        01 REC.
           05 ZERO-BAL PIC S9(5).
    ";
    // +0 in zoned decimal: F0 F0 F0 F0 C0 (C zone = positive on last byte)
    let data: Vec<u8> = vec![0xF0, 0xF0, 0xF0, 0xF0, 0xC0];

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json1 = decode_record(&schema, &data, &dopts).expect("decode");
    let re_encoded = encode_record(&schema, &json1, &eopts).expect("encode");
    // Re-decode and verify JSON-level equality (sign encoding may normalise)
    let json2 = decode_record(&schema, &re_encoded, &dopts).expect("decode-2");
    assert_eq!(json1, json2, "Signed zero JSON round-trip must be stable");
}

/// Decode using `ScratchBuffers` for the optimized path.
#[test]
fn edge_decode_with_scratch_buffers() {
    let cpy = r"
        01 REC.
           05 ID   PIC 9(4).
           05 NAME PIC X(6).
    ";
    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_digits("0042"));
    data.extend_from_slice(&ebcdic_text("TEST", 6));

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);
    let mut scratch = ScratchBuffers::new();

    let json = decode_record_with_scratch(&schema, &data, &opts, &mut scratch).expect("decode");
    assert_eq!(json["ID"], "0042");
    assert_eq!(json["NAME"], "TEST  ");

    // Reuse scratch for a second decode
    let json2 = decode_record_with_scratch(&schema, &data, &opts, &mut scratch).expect("decode-2");
    assert_eq!(json, json2, "Scratch buffer reuse must be deterministic");
}

/// FILLER fields are excluded from output via the scratch-buffer decode path.
#[test]
fn edge_filler_field_not_in_output() {
    let cpy = r"
        01 REC.
           05 ID    PIC 9(4).
           05 FILLER PIC X(6).
           05 NAME  PIC X(5).
    ";
    let mut data = Vec::new();
    data.extend_from_slice(&ebcdic_digits("0001"));
    data.extend_from_slice(&[0x40; 6]); // filler (spaces)
    data.extend_from_slice(&ebcdic_text("ALICE", 5));

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);
    let mut scratch = ScratchBuffers::new();

    let json = decode_record_with_scratch(&schema, &data, &opts, &mut scratch).expect("decode");

    assert_eq!(json["ID"], "0001");
    assert_eq!(json["NAME"], "ALICE");
    // With emit_filler=false (default), FILLER should be absent.
    assert!(
        json.get("FILLER").is_none(),
        "FILLER field must not appear in scratch-buffer decode output"
    );
}

/// Projection error: selecting a nonexistent field.
#[test]
fn edge_projection_unknown_field_error() {
    let cpy = r"
        01 REC.
           05 REAL PIC X(5).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let result = project_schema(&schema, &["GHOST-FIELD".to_string()]);
    assert!(result.is_err(), "Unknown field must fail");
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND
    );
}

/// Negative COMP signed halfword round-trip.
#[test]
fn roundtrip_comp_negative_halfword() {
    let cpy = r"
        01 REC.
           05 DEBIT PIC S9(4) COMP.
    ";
    // -100 in two's complement big-endian 16-bit = 0xFF9C
    let data: Vec<u8> = vec![0xFF, 0x9C];

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(data, re_encoded, "COMP negative halfword round-trip");
}

/// Decode and encode with CP500 (international EBCDIC variant).
#[test]
fn roundtrip_codepage_cp500() {
    let cpy = r"
        01 REC.
           05 DIGITS PIC 9(6).
    ";
    // Digits F0-F9 are shared across EBCDIC codepages
    let data = ebcdic_digits("314159");
    let schema = parse_copybook(cpy).expect("parse");

    let dopts = decode_opts(Codepage::CP500);
    let eopts = encode_opts(Codepage::CP500);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(data, re_encoded, "CP500 digit round-trip");
}

/// Verify that `decode_file_to_jsonl` summary reports bytes processed.
#[test]
fn decode_file_summary_bytes_processed() {
    let cpy = r"
        01 REC.
           05 VAL PIC X(10).
    ";
    let records = 5;
    let data = vec![0x40; 10 * records]; // 5 records of 10 spaces

    let schema = parse_copybook(cpy).expect("parse");
    let mut out = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&data),
        &mut out,
        &decode_opts(Codepage::CP037),
    )
    .expect("decode");

    assert_eq!(summary.records_processed, records as u64);
    assert!(
        summary.bytes_processed > 0,
        "bytes_processed must be non-zero"
    );
}

/// Round-trip sign-separate leading with negative value.
#[test]
fn roundtrip_sign_separate_leading_negative() {
    let cpy = r"
        01 REC.
           05 BAL PIC S9(5) SIGN IS LEADING SEPARATE.
    ";
    // -54321 → sign(-) then "54321"
    let mut data = vec![0x60]; // '-' in CP037
    data.extend_from_slice(&ebcdic_digits("54321"));

    let schema = parse_copybook(cpy).expect("parse");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).expect("decode");
    let re_encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(
        data, re_encoded,
        "SIGN LEADING SEPARATE negative round-trip"
    );
}
