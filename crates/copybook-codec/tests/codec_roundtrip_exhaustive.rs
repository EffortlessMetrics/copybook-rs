// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Exhaustive round-trip tests for the codec.
//!
//! Each test encodes binary data → decodes to JSON → re-encodes back to binary
//! and asserts byte-identity (or within-epsilon for floats).

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, ZonedEncodingFormat,
};
use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
        .with_preserve_zoned_encoding(true)
}

fn encode_opts(cp: Codepage, zoned: ZonedEncodingFormat) -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
        .with_zoned_encoding_format(zoned)
}

/// Convert ASCII text to EBCDIC bytes using `copybook_charset::utf8_to_ebcdic`.
fn to_ebcdic(text: &str, cp: Codepage) -> Vec<u8> {
    let charset_cp = match cp {
        Codepage::CP037 => copybook_charset::Codepage::CP037,
        Codepage::CP500 => copybook_charset::Codepage::CP500,
        Codepage::CP1047 => copybook_charset::Codepage::CP1047,
        Codepage::CP1140 => copybook_charset::Codepage::CP1140,
        Codepage::CP273 => copybook_charset::Codepage::CP273,
        _ => copybook_charset::Codepage::CP037,
    };
    copybook_charset::utf8_to_ebcdic(text, charset_cp).unwrap()
}

/// Round-trip helper: decode then re-encode; assert byte-identical output.
fn assert_roundtrip(copybook: &str, data: &[u8], cp: Codepage, zoned: ZonedEncodingFormat) {
    let schema = parse_copybook(copybook).unwrap();
    let decoded = copybook_codec::decode_record(&schema, data, &decode_opts(cp)).unwrap();
    let re_encoded = copybook_codec::encode_record(&schema, &decoded, &encode_opts(cp, zoned))
        .expect("encode_record failed");
    assert_eq!(
        re_encoded, data,
        "round-trip mismatch for copybook: {copybook}"
    );
}

// ---------------------------------------------------------------------------
// 1. DISPLAY alphanumeric
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_display_alphanumeric_ebcdic() {
    // "HELLO     " in EBCDIC CP037
    let data = to_ebcdic("HELLO     ", Codepage::CP037);
    assert_roundtrip(
        "01 REC.\n   05 NAME PIC X(10).",
        &data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_display_alphanumeric_ascii() {
    let data = b"ABCDE";
    assert_roundtrip(
        "01 REC.\n   05 NAME PIC X(5).",
        data,
        Codepage::ASCII,
        ZonedEncodingFormat::Ascii,
    );
}

// ---------------------------------------------------------------------------
// 2. DISPLAY numeric (zoned decimal)
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_display_numeric_ebcdic_unsigned() {
    // EBCDIC "12345" = 0xF1 F2 F3 F4 F5
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC 9(5).",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_display_numeric_ebcdic_signed_positive() {
    // EBCDIC +123 → F1 F2 C3 (positive overpunch on last digit)
    let data: &[u8] = &[0xF1, 0xF2, 0xC3];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(3).",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_display_numeric_ebcdic_signed_negative() {
    // EBCDIC -456 → F4 F5 D6 (negative overpunch on last digit)
    let data: &[u8] = &[0xF4, 0xF5, 0xD6];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(3).",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 3. COMP (binary integer) — 2, 4, 8 bytes
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_comp_2byte() {
    // PIC 9(4) COMP → 2-byte unsigned big-endian; value 1234 = 0x04D2
    let data: &[u8] = &[0x04, 0xD2];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC 9(4) COMP.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_comp_4byte() {
    // PIC S9(8) COMP → 4-byte signed big-endian; value -1 = 0xFFFFFFFF
    let data: &[u8] = &[0xFF, 0xFF, 0xFF, 0xFF];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(8) COMP.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_comp_8byte() {
    // PIC 9(18) COMP → 8-byte unsigned big-endian; value 1
    let data: &[u8] = &[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC 9(18) COMP.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 4. COMP-1 (float) — round-trip within epsilon
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_comp1_float() {
    use copybook_codec::FloatFormat;
    use copybook_core::feature_flags::{Feature, FeatureFlags};

    let mut flags = FeatureFlags::default();
    flags.enable(Feature::Comp1);
    FeatureFlags::set_global(flags);

    let schema = parse_copybook("01 REC.\n 05 RATE COMP-1.").unwrap();
    // IEEE 754 for 3.14 ≈ 0x4048F5C3
    let data: &[u8] = &[0x40, 0x48, 0xF5, 0xC3];

    let d_opts = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_meta(false);

    let decoded = copybook_codec::decode_record(&schema, data, &d_opts).unwrap();
    let rate = decoded["RATE"].as_f64().unwrap();
    assert!(
        (rate - std::f64::consts::PI).abs() < 1e-2,
        "decoded COMP-1 should be near PI"
    );

    let e_opts = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_float_format(FloatFormat::IeeeBigEndian);

    let re_encoded = copybook_codec::encode_record(&schema, &decoded, &e_opts).unwrap();
    assert_eq!(
        re_encoded, data,
        "COMP-1 round-trip should be byte-identical"
    );
}

// ---------------------------------------------------------------------------
// 5. COMP-2 (double) — round-trip within epsilon
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_comp2_double() {
    use copybook_codec::FloatFormat;
    use copybook_core::feature_flags::{Feature, FeatureFlags};

    let mut flags = FeatureFlags::default();
    flags.enable(Feature::Comp2);
    FeatureFlags::set_global(flags);

    let schema = parse_copybook("01 REC.\n 05 AMOUNT COMP-2.").unwrap();
    // IEEE 754 double for 1.0 = 0x3FF0000000000000
    let data: &[u8] = &[0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];

    let d_opts = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_meta(false);

    let decoded = copybook_codec::decode_record(&schema, data, &d_opts).unwrap();
    let val = decoded["AMOUNT"].as_f64().unwrap();
    assert!(
        (val - 1.0).abs() < f64::EPSILON,
        "decoded COMP-2 should == 1.0"
    );

    let e_opts = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_float_format(FloatFormat::IeeeBigEndian);

    let re_encoded = copybook_codec::encode_record(&schema, &decoded, &e_opts).unwrap();
    assert_eq!(
        re_encoded, data,
        "COMP-2 round-trip should be byte-identical"
    );
}

// ---------------------------------------------------------------------------
// 6. COMP-3 (packed decimal) — positive, negative, zero
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_comp3_positive() {
    // PIC 9(5) COMP-3 → 3 bytes; value 12345 → 0x12 0x34 0x5F
    let data: &[u8] = &[0x12, 0x34, 0x5F];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC 9(5) COMP-3.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_comp3_negative() {
    // PIC S9(5) COMP-3 → 3 bytes; value -678 → 0x00 0x67 0x8D
    let data: &[u8] = &[0x00, 0x67, 0x8D];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(5) COMP-3.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_comp3_zero() {
    // PIC S9(3) COMP-3 → 2 bytes; value +0 → 0x00 0x0C
    let data: &[u8] = &[0x00, 0x0C];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(3) COMP-3.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 7. SIGN SEPARATE — leading and trailing, positive and negative
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_sign_separate_leading_positive() {
    // SIGN LEADING SEPARATE: '+' (0x4E in EBCDIC) followed by digits
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3]; // +123 in EBCDIC
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(3) SIGN LEADING SEPARATE.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_sign_separate_trailing_negative() {
    // SIGN TRAILING SEPARATE: digits then '-' (0x60 in EBCDIC)
    let data: &[u8] = &[0xF4, 0xF5, 0xF6, 0x60]; // 456- in EBCDIC
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(3) SIGN TRAILING SEPARATE.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 8. Multi-field records with mixed types
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_mixed_record() {
    let copybook = r"
01 REC.
   05 NAME     PIC X(5).
   05 AMOUNT   PIC 9(5) COMP-3.
   05 COUNT    PIC 9(4) COMP.
";
    let schema = parse_copybook(copybook).unwrap();

    // NAME = "ABCDE" in EBCDIC CP037
    let mut data = to_ebcdic("ABCDE", Codepage::CP037);
    data.extend_from_slice(&[0x99, 0x99, 0x9F]); // COMP-3 99999
    data.extend_from_slice(&[0x00, 0x2A]); // COMP 42

    let d_opts = decode_opts(Codepage::CP037);
    let decoded = copybook_codec::decode_record(&schema, &data, &d_opts).unwrap();
    let e_opts = encode_opts(Codepage::CP037, ZonedEncodingFormat::Ebcdic);
    let re_encoded = copybook_codec::encode_record(&schema, &decoded, &e_opts).unwrap();
    assert_eq!(re_encoded, data, "mixed-record round-trip mismatch");
}

// ---------------------------------------------------------------------------
// 9. Codepage round-trips: CP037, CP500, CP1047
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_codepage_cp037() {
    let data = to_ebcdic("HELLO", Codepage::CP037);
    assert_roundtrip(
        "01 REC.\n   05 TXT PIC X(5).",
        &data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_codepage_cp500() {
    let data = to_ebcdic("HELLO", Codepage::CP500);
    assert_roundtrip(
        "01 REC.\n   05 TXT PIC X(5).",
        &data,
        Codepage::CP500,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_codepage_cp1047() {
    let data = to_ebcdic("HELLO", Codepage::CP1047);
    assert_roundtrip(
        "01 REC.\n   05 TXT PIC X(5).",
        &data,
        Codepage::CP1047,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 10. Boundary values: zero-fill, max size field, minimum signed
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_zero_fill_zoned() {
    // All-zero unsigned zoned: EBCDIC 0xF0 repeated
    let data: Vec<u8> = vec![0xF0; 9];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC 9(9).",
        &data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_comp_zero() {
    // 4-byte COMP = 0
    let data: &[u8] = &[0x00, 0x00, 0x00, 0x00];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC 9(9) COMP.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

#[test]
fn roundtrip_comp3_max_digits() {
    // PIC 9(7) COMP-3 → 4 bytes; value 9999999 → 0x99 0x99 0x99 0x9F
    let data: &[u8] = &[0x99, 0x99, 0x99, 0x9F];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC 9(7) COMP-3.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 11. Alphanumeric with space padding (codepage conversion stress)
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_alpha_space_padded() {
    // "A   B" in EBCDIC CP037 — spaces are 0x40
    let data = to_ebcdic("A   B", Codepage::CP037);
    assert_roundtrip(
        "01 REC.\n   05 TXT PIC X(5).",
        &data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 12. Scratch buffer: verify same results with and without scratch
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_scratch_buffer_equivalence() {
    let copybook = r"
01 REC.
   05 NAME   PIC X(5).
   05 AMT    PIC 9(5) COMP-3.
";
    let schema = parse_copybook(copybook).unwrap();

    let mut data = to_ebcdic("ABCDE", Codepage::CP037);
    data.extend_from_slice(&[0x12, 0x34, 0x5F]); // 12345 COMP-3

    let opts = decode_opts(Codepage::CP037);

    let without_scratch = copybook_codec::decode_record(&schema, &data, &opts).unwrap();

    let mut scratch = copybook_codec::memory::ScratchBuffers::new();
    let with_scratch =
        copybook_codec::decode_record_with_scratch(&schema, &data, &opts, &mut scratch).unwrap();

    assert_eq!(
        without_scratch, with_scratch,
        "decode with/without scratch must produce identical JSON"
    );

    // Both should re-encode to the same bytes
    let e_opts = encode_opts(Codepage::CP037, ZonedEncodingFormat::Ebcdic);
    let enc1 = copybook_codec::encode_record(&schema, &without_scratch, &e_opts).unwrap();
    let enc2 = copybook_codec::encode_record(&schema, &with_scratch, &e_opts).unwrap();
    assert_eq!(enc1, data);
    assert_eq!(enc2, data);
}

// ---------------------------------------------------------------------------
// 13. Decimal scale (implied decimal) round-trip
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_comp3_with_scale() {
    // PIC S9(5)V99 COMP-3 → 7 digits + sign = 8 nibbles = 4 bytes
    // value +12345.67 → 0x12 0x34 0x56 0x7C
    let data: &[u8] = &[0x12, 0x34, 0x56, 0x7C];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(5)V99 COMP-3.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 14. COMP signed 2-byte positive value
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_comp_signed_2byte_positive() {
    // PIC S9(4) COMP → 2-byte signed big-endian; value +100 = 0x0064
    let data: &[u8] = &[0x00, 0x64];
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(4) COMP.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}

// ---------------------------------------------------------------------------
// 15. Large multi-field record with diverse types
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_enterprise_record() {
    let copybook = r"
01 CUSTOMER-REC.
   05 CUST-ID       PIC 9(6).
   05 CUST-NAME     PIC X(20).
   05 BALANCE       PIC S9(7)V99 COMP-3.
   05 TXN-COUNT     PIC 9(4) COMP.
";
    let schema = parse_copybook(copybook).unwrap();

    // Build EBCDIC data
    let mut data = Vec::new();
    // CUST-ID: "000042" in EBCDIC zoned
    for &d in &[0u8, 0, 0, 0, 4, 2] {
        data.push(0xF0 + d);
    }
    // CUST-NAME: "JOHN DOE" padded to 20 in EBCDIC
    data.extend_from_slice(&to_ebcdic("JOHN DOE            ", Codepage::CP037));
    // BALANCE: PIC S9(7)V99 COMP-3 → 9 digits + sign = 10 nibbles = 5 bytes
    // +1234567.89 → 0x12 0x34 0x56 0x78 0x9C
    data.extend_from_slice(&[0x12, 0x34, 0x56, 0x78, 0x9C]);
    // TXN-COUNT: 10 = 0x000A
    data.extend_from_slice(&[0x00, 0x0A]);

    let d_opts = decode_opts(Codepage::CP037);
    let decoded = copybook_codec::decode_record(&schema, &data, &d_opts).unwrap();
    let e_opts = encode_opts(Codepage::CP037, ZonedEncodingFormat::Ebcdic);
    let re_encoded = copybook_codec::encode_record(&schema, &decoded, &e_opts).unwrap();

    assert_eq!(
        re_encoded, data,
        "enterprise record round-trip should be byte-identical"
    );
}

// ---------------------------------------------------------------------------
// 16. ASCII zoned numeric round-trip
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_ascii_zoned_unsigned() {
    let data = b"98765";
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC 9(5).",
        data,
        Codepage::ASCII,
        ZonedEncodingFormat::Ascii,
    );
}

// ---------------------------------------------------------------------------
// 17. Sign separate leading negative
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_sign_separate_leading_negative() {
    // '-' (0x60) then digits in EBCDIC
    let data: &[u8] = &[0x60, 0xF7, 0xF8, 0xF9]; // -789 in EBCDIC
    assert_roundtrip(
        "01 REC.\n   05 NUM PIC S9(3) SIGN LEADING SEPARATE.",
        data,
        Codepage::CP037,
        ZonedEncodingFormat::Ebcdic,
    );
}
