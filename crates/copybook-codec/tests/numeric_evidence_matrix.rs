// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
//! Consolidated numeric-representation evidence matrix (issue #571).
//!
//! Existing numeric coverage is deep but split by family across many files
//! (several gated behind `comprehensive-tests`). This suite pins, in one
//! **default-feature** place, a per-family scenario row linking
//! **parse → layout → decode → encode round-trip** plus representative
//! rejection paths, for every supported numeric family:
//!
//! * **zoned / DISPLAY** (`PIC 9`) — decoded as a lossless decimal string,
//! * **COMP-3** (packed decimal) — decoded as a lossless decimal string,
//! * **COMP / BINARY** (`PIC 9 COMP`) — decoded as a lossless decimal string,
//! * **COMP-1** (IEEE-754 single, 4 bytes) — decoded as a JSON number,
//! * **COMP-2** (IEEE-754 double, 8 bytes) — decoded as a JSON number.
//!
//! COMP-1/COMP-2 are fully supported and enabled by default. `JsonNumberMode`
//! is honored for the integer/decimal families (Lossless → string) but is
//! ignored for floats, which always decode to a JSON number (or `null` for
//! NaN/±Infinity — see `float_special_values_decode_to_null`). The CLI
//! command-context coverage lives in `copybook-cli/tests/numeric_cli_matrix.rs`.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RecordFormat,
    decode_record, encode_record,
};
use copybook_core::{ErrorCode, parse_copybook};

// ---------------------------------------------------------------------------
// Option helpers (EBCDIC CP037, big-endian IEEE floats, lossless numbers).
// ---------------------------------------------------------------------------

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_float_format(FloatFormat::IeeeBigEndian)
        .with_emit_meta(false)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_float_format(FloatFormat::IeeeBigEndian)
}

// ===========================================================================
// Integer / decimal families: decode → lossless string, byte-identical
// round-trip, and expected fixed LRECL.
// ===========================================================================

/// `(family, copybook, field, bytes, expected_string, lrecl)`.
type StringRow = (
    &'static str,
    &'static str,
    &'static str,
    &'static [u8],
    &'static str,
    u32,
);

const STRING_FAMILIES: &[StringRow] = &[
    // Zoned / DISPLAY: EBCDIC "12345".
    (
        "zoned",
        "01 REC.\n   05 Z PIC 9(5).\n",
        "Z",
        &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5],
        "12345",
        5,
    ),
    // COMP-3 packed decimal: 12345 with unsigned (F) sign nibble → 3 bytes.
    (
        "comp3",
        "01 REC.\n   05 P PIC 9(5) COMP-3.\n",
        "P",
        &[0x12, 0x34, 0x5F],
        "12345",
        3,
    ),
    // COMP / BINARY: PIC 9(4) COMP → 2-byte big-endian, value 1234 = 0x04D2.
    (
        "comp-binary",
        "01 REC.\n   05 B PIC 9(4) COMP.\n",
        "B",
        &[0x04, 0xD2],
        "1234",
        2,
    ),
];

#[test]
fn string_families_decode_layout_and_roundtrip() {
    for (family, copybook, field, bytes, expected, lrecl) in STRING_FAMILIES {
        let schema = parse_copybook(copybook).expect("copybook parses");

        // Layout plane: fixed LRECL matches the family's byte width.
        assert_eq!(
            schema.lrecl_fixed,
            Some(*lrecl),
            "{family}: expected LRECL {lrecl}"
        );

        // Decode plane: lossless decimal string.
        let json = decode_record(&schema, bytes, &decode_opts())
            .unwrap_or_else(|e| panic!("{family} decode failed: {e}"));
        assert_eq!(
            json.get(*field).and_then(serde_json::Value::as_str),
            Some(*expected),
            "{family}: decode mismatch"
        );

        // Encode plane: byte-identical round-trip.
        let reencoded = encode_record(&schema, &json, &encode_opts())
            .unwrap_or_else(|e| panic!("{family} encode failed: {e}"));
        assert_eq!(reencoded, *bytes, "{family}: round-trip not byte-identical");
    }
}

// ===========================================================================
// Float families (COMP-1 / COMP-2): decode → JSON number, byte-identical
// round-trip, expected LRECL.
// ===========================================================================

/// `(family, copybook, field, bytes, expected_value, lrecl)`.
type FloatRow = (
    &'static str,
    &'static str,
    &'static str,
    &'static [u8],
    f64,
    u32,
);

const FLOAT_FAMILIES: &[FloatRow] = &[
    // COMP-1 IEEE-754 single 1.0 = 0x3F800000.
    (
        "comp1",
        "01 REC.\n   05 RATE COMP-1.\n",
        "RATE",
        &[0x3F, 0x80, 0x00, 0x00],
        1.0,
        4,
    ),
    // COMP-2 IEEE-754 double 1.0 = 0x3FF0000000000000.
    (
        "comp2",
        "01 REC.\n   05 RATE COMP-2.\n",
        "RATE",
        &[0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        1.0,
        8,
    ),
];

#[test]
fn float_families_decode_layout_and_roundtrip() {
    for (family, copybook, field, bytes, expected, lrecl) in FLOAT_FAMILIES {
        let schema = parse_copybook(copybook).expect("copybook parses");

        assert_eq!(
            schema.lrecl_fixed,
            Some(*lrecl),
            "{family}: expected LRECL {lrecl}"
        );

        let json = decode_record(&schema, bytes, &decode_opts())
            .unwrap_or_else(|e| panic!("{family} decode failed: {e}"));
        let value = json
            .get(*field)
            .and_then(serde_json::Value::as_f64)
            .unwrap_or_else(|| panic!("{family}: decoded value is not a JSON number"));
        assert!(
            (value - *expected).abs() < f64::EPSILON,
            "{family}: decode mismatch, got {value}"
        );

        let reencoded = encode_record(&schema, &json, &encode_opts())
            .unwrap_or_else(|e| panic!("{family} encode failed: {e}"));
        assert_eq!(reencoded, *bytes, "{family}: round-trip not byte-identical");
    }
}

// ===========================================================================
// Signed decode: zoned overpunch sign zones (C = positive, D = negative).
// ===========================================================================

#[test]
fn zoned_signed_overpunch_decode() {
    let schema = parse_copybook("01 REC.\n   05 S PIC S9(3).\n").expect("copybook parses");

    // EBCDIC "12" + C-zone '3' => +123.
    let pos = decode_record(&schema, &[0xF1, 0xF2, 0xC3], &decode_opts()).expect("decode");
    assert_eq!(
        pos.get("S").and_then(serde_json::Value::as_str),
        Some("123")
    );

    // EBCDIC "12" + D-zone '3' => -123.
    let neg = decode_record(&schema, &[0xF1, 0xF2, 0xD3], &decode_opts()).expect("decode");
    assert_eq!(
        neg.get("S").and_then(serde_json::Value::as_str),
        Some("-123")
    );
}

// ===========================================================================
// Rejection plane (representative per family).
// ===========================================================================

#[test]
fn comp3_invalid_sign_nibble_is_cbkd401() {
    // PIC 9(1) COMP-3 = one byte [digit|sign]; 0x50 has digit 5 and sign 0x0,
    // which is not a valid COMP-3 sign nibble.
    let schema = parse_copybook("01 REC.\n   05 P PIC 9(1) COMP-3.\n").expect("copybook parses");
    let err = decode_record(&schema, &[0x50], &decode_opts())
        .expect_err("invalid COMP-3 sign nibble must be rejected");
    assert_eq!(err.code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE);
}

#[test]
fn zoned_encode_overflow_is_cbke510() {
    // Encoding a 5-digit value into a 4-digit zoned field overflows.
    let schema = parse_copybook("01 REC.\n   05 N PIC 9(4).\n").expect("copybook parses");
    let json = serde_json::json!({ "N": "99999" });
    let err = encode_record(&schema, &json, &encode_opts())
        .expect_err("value exceeding field digits must be rejected");
    assert_eq!(err.code, ErrorCode::CBKE510_NUMERIC_OVERFLOW);
}

#[test]
fn comp1_encode_overflow_is_cbke531() {
    // f64::MAX cannot be represented in a COMP-1 (IEEE-754 single) field.
    let schema = parse_copybook("01 REC.\n   05 V COMP-1.\n").expect("copybook parses");
    let json = serde_json::json!({ "V": f64::MAX });
    let err = encode_record(&schema, &json, &encode_opts())
        .expect_err("f64::MAX must not fit a COMP-1 field");
    assert_eq!(err.code, ErrorCode::CBKE531_FLOAT_ENCODE_OVERFLOW);
}

// ===========================================================================
// Documented behavior: NaN / ±Infinity decode to JSON null rather than
// raising CBKD431/CBKD432 (those codes exist in the taxonomy but are not
// raised on the decode path). This pins the current, observed contract.
// ===========================================================================

#[test]
fn float_special_values_decode_to_null() {
    let schema = parse_copybook("01 REC.\n   05 RATE COMP-1.\n").expect("copybook parses");

    // 0x7F800000 = +Infinity, 0x7FC00000 = NaN (IEEE-754 single).
    for bytes in [[0x7F, 0x80, 0x00, 0x00], [0x7F, 0xC0, 0x00, 0x00]] {
        let json = decode_record(&schema, &bytes, &decode_opts())
            .expect("special float values decode without error");
        assert!(
            json.get("RATE").is_some_and(serde_json::Value::is_null),
            "special float must decode to null, got {:?}",
            json.get("RATE")
        );
    }
}
