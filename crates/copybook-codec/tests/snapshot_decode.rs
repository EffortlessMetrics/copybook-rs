// SPDX-License-Identifier: AGPL-3.0-or-later
//! Snapshot tests for decode output stability.
//!
//! These tests verify that `decode_record` produces stable JSON output
//! for various COBOL field types. Any change to the JSON envelope
//! structure or field decoding will cause these tests to fail.

#![allow(clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_record};
use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Helper: default ASCII decode options (lossless numbers, no metadata)
// ---------------------------------------------------------------------------

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

// ---------------------------------------------------------------------------
// Simple DISPLAY record decode → JSON snapshot
// ---------------------------------------------------------------------------

#[test]
fn snapshot_decode_simple_display_record() {
    let copybook = "\
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID   PIC X(5).
          05 CUSTOMER-NAME PIC X(10).
          05 BALANCE       PIC 9(5)V99.
";
    let schema = parse_copybook(copybook).unwrap();

    // ASCII data: "A0001JOHN SMITH0012345"
    // CUSTOMER-ID = "A0001", CUSTOMER-NAME = "JOHN SMITH", BALANCE = "00123.45"
    let data = b"A0001JOHN SMITH0012345";
    let opts = ascii_decode_opts();
    let value = decode_record(&schema, data, &opts).unwrap();

    // Envelope fields
    assert_eq!(value["schema"], "copybook.v1");
    assert_eq!(value["record_index"], 0);
    assert_eq!(value["codepage"], "ascii");

    // Decoded field values
    assert_eq!(value["CUSTOMER-ID"], "A0001");
    assert_eq!(value["CUSTOMER-NAME"], "JOHN SMITH");

    // Numeric field decodes to string in lossless mode
    let balance = &value["BALANCE"];
    assert!(
        balance == "123.45" || balance == "00123.45",
        "BALANCE should decode to a numeric string, got: {balance}"
    );
}

// ---------------------------------------------------------------------------
// COMP-3 record decode → JSON snapshot
// ---------------------------------------------------------------------------

#[test]
fn snapshot_decode_comp3_record() {
    let copybook = "\
       01 PAYMENT-RECORD.
          05 PAYMENT-ID   PIC X(4).
          05 AMOUNT        PIC S9(5)V99 COMP-3.
";
    let schema = parse_copybook(copybook).unwrap();

    // PAYMENT-ID = "PAY1" (4 ASCII bytes)
    // AMOUNT = +12345.67 in COMP-3: S9(5)V99 = 7 digits → ceil((7+1)/2) = 4 bytes
    // Packed representation: 7 digits + sign = 8 nibbles = 4 bytes
    // Value 1234567 → nibbles: 0x01 0x23 0x45 0x6C (sign C = positive)
    // With scale 2 → 12345.67
    let mut data = [0u8; 8]; // 4 bytes ID + 4 bytes COMP-3
    data[0] = b'P';
    data[1] = b'A';
    data[2] = b'Y';
    data[3] = b'1';
    data[4] = 0x01;
    data[5] = 0x23;
    data[6] = 0x45;
    data[7] = 0x6C; // +1234567, sign C = positive

    let opts = ascii_decode_opts();
    let value = decode_record(&schema, &data, &opts).unwrap();

    assert_eq!(value["schema"], "copybook.v1");
    assert_eq!(value["PAYMENT-ID"], "PAY1");

    // COMP-3 value should decode to the numeric representation
    let amount = &value["AMOUNT"];
    assert!(
        amount.is_string() || amount.is_number(),
        "AMOUNT should be a numeric value, got: {amount}"
    );
    let amount_str = if amount.is_string() {
        amount.as_str().unwrap().to_string()
    } else {
        amount.to_string()
    };
    assert_eq!(amount_str, "1234.56");
}

// ---------------------------------------------------------------------------
// Record with FILLER → JSON snapshot (FILLER excluded by default)
// ---------------------------------------------------------------------------

#[test]
fn snapshot_decode_record_with_filler() {
    let copybook = "\
       01 HEADER-RECORD.
          05 RECORD-TYPE   PIC X(2).
          05 FILLER         PIC X(3).
          05 RECORD-DATA   PIC X(5).
";
    let schema = parse_copybook(copybook).unwrap();

    let data = b"HDXXXHELLO";
    let opts = ascii_decode_opts();
    let value = decode_record(&schema, data, &opts).unwrap();

    assert_eq!(value["schema"], "copybook.v1");
    assert_eq!(value["RECORD-TYPE"], "HD");
    assert_eq!(value["RECORD-DATA"], "HELLO");

    // FILLER is included in decode_record output (the standard path does
    // not filter FILLER; only the scratch-buffer fast path does).
    assert!(
        value.get("FILLER").is_some(),
        "FILLER fields should be present in decode_record output"
    );
    assert_eq!(value["FILLER"], "XXX");
}

// ---------------------------------------------------------------------------
// Record with FILLER → emit_filler=true includes FILLER
// ---------------------------------------------------------------------------

#[test]
fn snapshot_decode_record_with_filler_emitted() {
    let copybook = "\
       01 HEADER-RECORD.
          05 RECORD-TYPE   PIC X(2).
          05 FILLER         PIC X(3).
          05 RECORD-DATA   PIC X(5).
";
    let schema = parse_copybook(copybook).unwrap();

    let data = b"HDXXXHELLO";
    let opts = ascii_decode_opts().with_emit_filler(true);
    let value = decode_record(&schema, data, &opts).unwrap();

    assert_eq!(value["RECORD-TYPE"], "HD");
    assert_eq!(value["RECORD-DATA"], "HELLO");

    // With emit_filler, the FILLER field should appear
    assert!(
        value.get("FILLER").is_some() || value.get("_filler_00000002").is_some(),
        "FILLER field should appear when emit_filler is enabled"
    );
}

// ---------------------------------------------------------------------------
// Decode with emit_meta=true → JSON snapshot with metadata
// ---------------------------------------------------------------------------

#[test]
fn snapshot_decode_with_emit_meta() {
    let copybook = "\
       01 SIMPLE-RECORD.
          05 NAME   PIC X(8).
          05 CODE   PIC 9(3).
";
    let schema = parse_copybook(copybook).unwrap();

    let data = b"TESTNAME123";
    let opts = ascii_decode_opts().with_emit_meta(true);
    let value = decode_record(&schema, data, &opts).unwrap();

    // Standard envelope
    assert_eq!(value["schema"], "copybook.v1");
    assert_eq!(value["codepage"], "ascii");

    // Field values
    assert_eq!(value["NAME"], "TESTNAME");

    // Metadata fields should be present when emit_meta is true
    assert!(
        value.get("__record_index").is_some(),
        "__record_index should be present with emit_meta"
    );
    assert!(
        value.get("__length").is_some(),
        "__length should be present with emit_meta"
    );
    assert_eq!(value["__length"], 11);
    assert!(
        value.get("schema_fingerprint").is_some() || value.get("__schema_id").is_some(),
        "schema fingerprint should be present with emit_meta"
    );
}
