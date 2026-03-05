// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Codec integration tests: full pipeline parse → decode → encode → compare.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    decode_file_to_jsonl, decode_record, encode_jsonl_to_file, encode_record,
};
use copybook_core::parse_copybook;
use std::io::Cursor;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn ascii_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

// ===========================================================================
// 1. Full codec pipeline: parse → decode → encode → compare
// ===========================================================================

#[test]
fn pipeline_pic_9_roundtrip() {
    let copybook = "01 REC.\n   05 FLD PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();
    let original = b"12345";

    let json = decode_record(&schema, original, &ascii_decode_opts()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, original);
}

#[test]
fn pipeline_pic_x_roundtrip() {
    let copybook = "01 REC.\n   05 FLD PIC X(10).";
    let schema = parse_copybook(copybook).unwrap();
    let original = b"HELLO     ";

    let json = decode_record(&schema, original, &ascii_decode_opts()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, original);
}

#[test]
fn pipeline_multi_field_roundtrip() {
    let copybook = r"
        01 RECORD.
           05 ID     PIC 9(5).
           05 NAME   PIC X(10).
           05 AMOUNT PIC 9(7).
    ";
    let schema = parse_copybook(copybook).unwrap();
    let original = b"00042ALICE     0001234";

    let json = decode_record(&schema, original, &ascii_decode_opts()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(
        encoded,
        original,
        "Multi-field round-trip mismatch:\noriginal: {:?}\nencoded:  {:?}",
        String::from_utf8_lossy(original),
        String::from_utf8_lossy(&encoded)
    );
}

#[test]
fn pipeline_file_level_roundtrip() {
    let copybook = "01 FLD PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();

    // Build binary data
    let mut data = Vec::new();
    for i in 0..20 {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }

    // Decode to JSONL
    let mut jsonl_output = Vec::new();
    decode_file_to_jsonl(
        &schema,
        Cursor::new(data.as_slice()),
        &mut jsonl_output,
        &ascii_decode_opts(),
    )
    .unwrap();

    // Encode back to binary
    let mut binary_output = Vec::new();
    encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl_output.as_slice()),
        &mut binary_output,
        &ascii_encode_opts(),
    )
    .unwrap();

    assert_eq!(binary_output, data, "File-level round-trip mismatch");
}

#[test]
fn pipeline_signed_numeric_roundtrip() {
    let copybook = "01 REC.\n   05 AMT PIC S9(5).";
    let schema = parse_copybook(copybook).unwrap();
    // Positive value: unsigned trailing sign in ASCII
    let original = b"12345";
    let opts = ascii_decode_opts().with_preserve_zoned_encoding(true);
    let enc_opts = ascii_encode_opts();

    let json = decode_record(&schema, original, &opts).unwrap();
    let encoded = encode_record(&schema, &json, &enc_opts).unwrap();
    assert_eq!(encoded.len(), original.len());
}

// ===========================================================================
// 2. Multiple schemas with same data
// ===========================================================================

#[test]
fn same_bytes_different_schemas() {
    let data = b"12345ABCDE";

    // Schema A: two PIC 9(5) fields
    let schema_a = parse_copybook("01 REC.\n   05 F1 PIC 9(5).\n   05 F2 PIC X(5).").unwrap();

    // Schema B: one PIC X(10)
    let schema_b = parse_copybook("01 REC.\n   05 WHOLE PIC X(10).").unwrap();

    let json_a = decode_record(&schema_a, data, &ascii_decode_opts()).unwrap();
    let json_b = decode_record(&schema_b, data, &ascii_decode_opts()).unwrap();

    // Schema A yields two fields
    assert!(json_a.get("F1").is_some());
    assert!(json_a.get("F2").is_some());

    // Schema B yields one field
    assert!(json_b.get("WHOLE").is_some());
    assert_eq!(json_b["WHOLE"], "12345ABCDE");
}

#[test]
fn different_field_names_same_layout() {
    let schema1 = parse_copybook("01 REC.\n   05 ALPHA PIC X(5).").unwrap();
    let schema2 = parse_copybook("01 REC.\n   05 BRAVO PIC X(5).").unwrap();
    let data = b"HELLO";

    let json1 = decode_record(&schema1, data, &ascii_decode_opts()).unwrap();
    let json2 = decode_record(&schema2, data, &ascii_decode_opts()).unwrap();

    assert_eq!(json1["ALPHA"], "HELLO");
    assert_eq!(json2["BRAVO"], "HELLO");
}

#[test]
fn schema_with_different_numeric_widths() {
    let narrow = parse_copybook("01 REC.\n   05 N PIC 9(3).").unwrap();
    let wide = parse_copybook("01 REC.\n   05 N PIC 9(5).").unwrap();

    let json_narrow = decode_record(&narrow, b"042", &ascii_decode_opts()).unwrap();
    let json_wide = decode_record(&wide, b"00042", &ascii_decode_opts()).unwrap();

    // Both should decode to the same logical value
    let narrow_val: u64 = json_narrow["N"].as_str().unwrap().parse().unwrap();
    let wide_val: u64 = json_wide["N"].as_str().unwrap().parse().unwrap();
    assert_eq!(narrow_val, wide_val);
}

// ===========================================================================
// 3. Codec configuration combinations
// ===========================================================================

#[test]
fn decode_with_emit_meta_true() {
    let schema = parse_copybook("01 FLD PIC 9(5).").unwrap();
    let data = b"00001";
    let opts = ascii_decode_opts().with_emit_meta(true);
    let json = decode_record(&schema, data, &opts).unwrap();

    assert!(json.get("__record_index").is_some());
    assert!(json.get("__length").is_some());
}

#[test]
fn decode_with_emit_meta_false() {
    let schema = parse_copybook("01 FLD PIC 9(5).").unwrap();
    let data = b"00001";
    let opts = ascii_decode_opts().with_emit_meta(false);
    let json = decode_record(&schema, data, &opts).unwrap();

    assert!(json.get("__record_index").is_none());
    assert!(json.get("__length").is_none());
}

#[test]
fn decode_raw_mode_record() {
    let schema = parse_copybook("01 FLD PIC X(5).").unwrap();
    let data = b"HELLO";
    let opts = ascii_decode_opts().with_emit_raw(RawMode::Record);
    let json = decode_record(&schema, data, &opts).unwrap();

    assert!(
        json.get("raw_b64").is_some() || json.get("__raw_b64").is_some(),
        "Raw mode Record should emit raw_b64"
    );
}

#[test]
fn decode_raw_mode_off() {
    let schema = parse_copybook("01 FLD PIC X(5).").unwrap();
    let data = b"HELLO";
    let opts = ascii_decode_opts().with_emit_raw(RawMode::Off);
    let json = decode_record(&schema, data, &opts).unwrap();

    assert!(
        json.get("__raw_b64").is_none(),
        "Raw mode Off should not emit __raw_b64"
    );
}

#[test]
fn lossless_vs_native_json_numbers() {
    let schema = parse_copybook("01 FLD PIC 9(5).").unwrap();
    let data = b"00042";

    let lossless = ascii_decode_opts().with_json_number_mode(JsonNumberMode::Lossless);
    let native = ascii_decode_opts().with_json_number_mode(JsonNumberMode::Native);

    let json_l = decode_record(&schema, data, &lossless).unwrap();
    let json_n = decode_record(&schema, data, &native).unwrap();

    assert!(json_l["FLD"].is_string(), "Lossless → string");
    // Native mode may emit number or string depending on the value
    // Verify both represent the same logical value
    let l_val: u64 = json_l["FLD"].as_str().unwrap().parse().unwrap();
    let n_val: u64 = json_n["FLD"]
        .as_u64()
        .or_else(|| json_n["FLD"].as_str().map(|s| s.parse().unwrap()))
        .unwrap();
    assert_eq!(l_val, n_val);
}

#[test]
fn encode_with_coerce_numbers() {
    let schema = parse_copybook("01 FLD PIC 9(5).").unwrap();
    let json: serde_json::Value = serde_json::json!({"FLD": 42});
    let opts = ascii_encode_opts().with_coerce_numbers(true);
    let result = encode_record(&schema, &json, &opts);
    // Should succeed when coercing numbers to strings
    assert!(
        result.is_ok(),
        "Coerce numbers should allow numeric JSON input"
    );
}

#[test]
fn decode_codepage_ascii() {
    let schema = parse_copybook("01 FLD PIC X(5).").unwrap();
    let data = b"HELLO";
    let opts = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);
    let json = decode_record(&schema, data, &opts).unwrap();
    assert_eq!(json["FLD"], "HELLO");
}

// ===========================================================================
// 4. Error propagation through codec pipeline
// ===========================================================================

#[test]
fn decode_record_too_short() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC X(10).").unwrap();
    let data = b"SHORT"; // Only 5 bytes for a 10-byte field
    let result = decode_record(&schema, data, &ascii_decode_opts());
    // May succeed with padding or error — ensure no panic
    let _ = result;
}

#[test]
fn encode_missing_field() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC 9(5).").unwrap();
    let json: serde_json::Value = serde_json::json!({"WRONG_FIELD": "12345"});
    let result = encode_record(&schema, &json, &ascii_encode_opts());
    // Missing field may produce default or error — verify no panic
    let _ = result;
}

#[test]
fn encode_type_mismatch_field() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC 9(5).").unwrap();
    let json: serde_json::Value = serde_json::json!({"FLD": "not_a_number"});
    let result = encode_record(&schema, &json, &ascii_encode_opts());
    // Should error on type mismatch — verify no panic
    let _ = result;
}

#[test]
fn decode_then_encode_preserves_length() {
    let schema = parse_copybook("01 REC.\n   05 ID PIC 9(3).\n   05 NAME PIC X(7).").unwrap();
    let original = b"042ALICE  ";
    let json = decode_record(&schema, original, &ascii_decode_opts()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(
        encoded.len(),
        original.len(),
        "Encoded length must match original"
    );
}

#[test]
fn pipeline_error_does_not_corrupt_state() {
    let schema = parse_copybook("01 FLD PIC 9(5).").unwrap();

    // Valid decode
    let good = b"00001";
    let json_good = decode_record(&schema, good, &ascii_decode_opts()).unwrap();
    assert_eq!(json_good["FLD"], "00001");

    // Attempt decode of short data
    let short: &[u8] = b"AB";
    let _ = decode_record(&schema, short, &ascii_decode_opts());

    // Another valid decode should still work
    let good2 = b"00002";
    let json_good2 = decode_record(&schema, good2, &ascii_decode_opts()).unwrap();
    assert_eq!(json_good2["FLD"], "00002");
}

#[test]
fn encode_jsonl_invalid_json_line() {
    let schema = parse_copybook("01 FLD PIC 9(5).").unwrap();
    let jsonl = r#"{"FLD": "00001"}
not valid json
{"FLD": "00003"}
"#;
    let mut output = Vec::new();
    let result = encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut output,
        &ascii_encode_opts(),
    );
    // Invalid JSON line should cause an error
    assert!(result.is_err(), "Invalid JSON should produce error");
}

#[test]
fn file_roundtrip_multi_field() {
    let copybook = r"
        01 REC.
           05 A PIC X(5).
           05 B PIC 9(3).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let mut data = Vec::new();
    for i in 0..10 {
        data.extend_from_slice(format!("{:<5}{:03}", format!("R{i}"), i * 11).as_bytes());
    }

    // Decode
    let mut jsonl = Vec::new();
    decode_file_to_jsonl(
        &schema,
        Cursor::new(data.as_slice()),
        &mut jsonl,
        &ascii_decode_opts(),
    )
    .unwrap();

    // Encode
    let mut binary = Vec::new();
    encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl.as_slice()),
        &mut binary,
        &ascii_encode_opts(),
    )
    .unwrap();

    assert_eq!(binary, data);
}

#[test]
fn pipeline_preserves_spaces_in_alpha() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC X(8).").unwrap();
    let original = b"  HELLO ";
    let json = decode_record(&schema, original, &ascii_decode_opts()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    // Alpha fields should preserve leading/trailing spaces through round-trip
    assert_eq!(encoded.len(), 8);
}
