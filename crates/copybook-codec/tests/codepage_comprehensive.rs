// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
//! Comprehensive codepage conversion tests exercising EBCDIC ↔ UTF-8 through
//! the charset layer and full codec round-trips across all supported codepages.

use copybook_codec::charset::{ebcdic_to_utf8, utf8_to_ebcdic};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, UnmappablePolicy,
    decode_record, encode_record,
};
use copybook_core::parse_copybook;

// ===========================================================================
// Helpers
// ===========================================================================

fn decode_opts(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn decode_opts_with_policy(cp: Codepage, policy: UnmappablePolicy) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
        .with_unmappable_policy(policy)
}

fn encode_opts(cp: Codepage) -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
}

/// Roundtrip helper: UTF-8 → EBCDIC → UTF-8, asserting equality.
fn assert_roundtrip(text: &str, cp: Codepage) {
    let ebcdic =
        utf8_to_ebcdic(text, cp).unwrap_or_else(|e| panic!("utf8→ebcdic failed for {cp}: {e}"));
    let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
        .unwrap_or_else(|e| panic!("ebcdic→utf8 failed for {cp}: {e}"));
    assert_eq!(back, text, "roundtrip mismatch for codepage {cp}");
}

// ===========================================================================
// CP037 (US/Canada) — character-level tests
// ===========================================================================

#[test]
fn cp037_uppercase_a_to_z() {
    // EBCDIC CP037: A=0xC1..I=0xC9, J=0xD1..R=0xD9, S=0xE2..Z=0xE9
    let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    assert_roundtrip(alpha, Codepage::CP037);

    // Verify specific well-known byte positions
    let ebcdic = utf8_to_ebcdic("A", Codepage::CP037).unwrap();
    assert_eq!(ebcdic, vec![0xC1]);
    let ebcdic = utf8_to_ebcdic("Z", Codepage::CP037).unwrap();
    assert_eq!(ebcdic, vec![0xE9]);
}

#[test]
fn cp037_lowercase_a_to_z() {
    let alpha = "abcdefghijklmnopqrstuvwxyz";
    assert_roundtrip(alpha, Codepage::CP037);

    let ebcdic = utf8_to_ebcdic("a", Codepage::CP037).unwrap();
    assert_eq!(ebcdic, vec![0x81]);
    let ebcdic = utf8_to_ebcdic("z", Codepage::CP037).unwrap();
    assert_eq!(ebcdic, vec![0xA9]);
}

#[test]
fn cp037_digits_0_to_9() {
    let digits = "0123456789";
    assert_roundtrip(digits, Codepage::CP037);

    // All EBCDIC codepages map digits to 0xF0..0xF9
    let ebcdic = utf8_to_ebcdic(digits, Codepage::CP037).unwrap();
    let expected: Vec<u8> = (0xF0..=0xF9).collect();
    assert_eq!(ebcdic, expected);
}

#[test]
fn cp037_special_characters() {
    // @=0x7C  #=0x7B  $=0x5B  .=0x4B  <=0x4C  (=0x4D  +=0x4E  &=0x50
    let specials = "@#$.<(+&";
    assert_roundtrip(specials, Codepage::CP037);

    let ebcdic = utf8_to_ebcdic("@", Codepage::CP037).unwrap();
    assert_eq!(ebcdic, vec![0x7C]);
    let ebcdic = utf8_to_ebcdic("#", Codepage::CP037).unwrap();
    assert_eq!(ebcdic, vec![0x7B]);
    let ebcdic = utf8_to_ebcdic("$", Codepage::CP037).unwrap();
    assert_eq!(ebcdic, vec![0x5B]);
}

#[test]
fn cp037_full_printable_ascii_roundtrip() {
    // All printable ASCII 0x20..=0x7E
    let printable: String = (0x20u8..=0x7Eu8).map(|b| b as char).collect();
    assert_roundtrip(&printable, Codepage::CP037);
}

#[test]
fn cp037_space_is_0x40() {
    let ebcdic = utf8_to_ebcdic(" ", Codepage::CP037).unwrap();
    assert_eq!(ebcdic, vec![0x40]);
    let back = ebcdic_to_utf8(&[0x40], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, " ");
}

// ===========================================================================
// CP273 (Germany/Austria)
// ===========================================================================

#[test]
#[allow(clippy::similar_names)]
fn cp273_german_special_chars() {
    // CP273 maps German characters at specific positions that differ from CP037.
    // Ä=0x4A, ö=0x6A, Ü=0x5A, ä=0xC0, Ö=0xE0, ü=0xD0, ß=0x59
    let ebcdic_cap_a_uml = [0x4A];
    let result =
        ebcdic_to_utf8(&ebcdic_cap_a_uml, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "Ä");

    let ebcdic_low_o_uml = [0x6A];
    let result =
        ebcdic_to_utf8(&ebcdic_low_o_uml, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "ö");

    let ebcdic_cap_u_uml = [0x5A];
    let result =
        ebcdic_to_utf8(&ebcdic_cap_u_uml, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "Ü");

    let ebcdic_eszett = [0x59];
    let result = ebcdic_to_utf8(&ebcdic_eszett, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "ß");
}

#[test]
fn cp273_differs_from_cp037_at_specific_bytes() {
    // These bytes produce different characters in CP273 vs CP037.
    // CP037: 0x4A = ¢   CP273: 0x4A = Ä
    let byte = [0x4A];
    let cp037_result = ebcdic_to_utf8(&byte, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    let cp273_result = ebcdic_to_utf8(&byte, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_ne!(cp037_result, cp273_result);
    assert_eq!(cp037_result, "¢");
    assert_eq!(cp273_result, "Ä");

    // CP037: 0x5B = $   CP273: 0x5B = $  (same!)
    // CP037: 0x7C = @   CP273: 0x7C = §
    let byte = [0x7C];
    let cp037_result = ebcdic_to_utf8(&byte, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    let cp273_result = ebcdic_to_utf8(&byte, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_ne!(cp037_result, cp273_result);
    assert_eq!(cp037_result, "@");
    assert_eq!(cp273_result, "§");
}

#[test]
fn cp273_roundtrip_german_text() {
    // These are characters that CP273 can encode
    let text = "ÄÖÜäöüß";
    assert_roundtrip(text, Codepage::CP273);
}

#[test]
fn cp273_uppercase_letters_same_position() {
    // A-Z are at the same EBCDIC positions across all codepages
    let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let cp037_bytes = utf8_to_ebcdic(alpha, Codepage::CP037).unwrap();
    let cp273_bytes = utf8_to_ebcdic(alpha, Codepage::CP273).unwrap();
    assert_eq!(
        cp037_bytes, cp273_bytes,
        "A-Z must be at same positions in CP037 and CP273"
    );
}

#[test]
fn cp273_specific_byte_mappings() {
    // Verify well-known CP273-specific mappings
    // 0x43 = { in CP273 (vs ä in CP037)
    let result = ebcdic_to_utf8(&[0x43], Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "{");
    let result037 = ebcdic_to_utf8(&[0x43], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(result037, "ä");

    // 0xC0 = ä in CP273 (vs { in CP037)
    let result = ebcdic_to_utf8(&[0xC0], Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "ä");
    let result037 = ebcdic_to_utf8(&[0xC0], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(result037, "{");
}

// ===========================================================================
// CP500 (International)
// ===========================================================================

#[test]
fn cp500_differs_from_cp037_brackets() {
    // CP500: [ = 0x4A  ] = 0x5A
    // CP037: [ = 0xBA  ] = 0xBB
    let cp500_result = ebcdic_to_utf8(&[0x4A], Codepage::CP500, UnmappablePolicy::Error).unwrap();
    assert_eq!(cp500_result, "[");

    let cp037_result = ebcdic_to_utf8(&[0x4A], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(cp037_result, "¢"); // Not [

    let cp500_bracket = utf8_to_ebcdic("[", Codepage::CP500).unwrap();
    let cp037_bracket = utf8_to_ebcdic("[", Codepage::CP037).unwrap();
    assert_ne!(cp500_bracket, cp037_bracket);
}

#[test]
fn cp500_exclamation_and_caret() {
    // CP500: ! = 0x4F  ^ = 0x5F
    let result = ebcdic_to_utf8(&[0x4F], Codepage::CP500, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "!");
    let result = ebcdic_to_utf8(&[0x5F], Codepage::CP500, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "^");

    // CP037 has ! at 0x5A and ^ at 0xB0
    let cp037_bang = utf8_to_ebcdic("!", Codepage::CP037).unwrap();
    let cp500_bang = utf8_to_ebcdic("!", Codepage::CP500).unwrap();
    assert_ne!(cp037_bang, cp500_bang);
}

#[test]
fn cp500_international_roundtrip() {
    let text = "Hello, World! [Test] {data}";
    assert_roundtrip(text, Codepage::CP500);
}

#[test]
fn cp500_full_printable_ascii_roundtrip() {
    let printable: String = (0x20u8..=0x7Eu8).map(|b| b as char).collect();
    assert_roundtrip(&printable, Codepage::CP500);
}

// ===========================================================================
// CP1047 (Open Systems / z/OS Unix)
// ===========================================================================

#[test]
fn cp1047_differs_from_cp037_for_brackets() {
    // CP1047: [ = 0xAD  ] = 0xBD  (differs from CP037: [ = 0xBA, ] = 0xBB)
    let cp1047_result = ebcdic_to_utf8(&[0xAD], Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(cp1047_result, "[");

    let cp037_result = ebcdic_to_utf8(&[0xAD], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_ne!(cp037_result, "[");

    let cp1047_close = ebcdic_to_utf8(&[0xBD], Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(cp1047_close, "]");
}

#[test]
fn cp1047_tilde_and_backslash() {
    // CP1047 and CP037 both have ~ at 0xA1
    let result = ebcdic_to_utf8(&[0xA1], Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "~");

    // Backslash: 0xE0 in both CP037 and CP1047
    let result = ebcdic_to_utf8(&[0xE0], Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "\\");
}

#[test]
fn cp1047_commonly_used_in_zos_unix() {
    // Verify common Unix path characters work
    let path = "/usr/local/bin";
    assert_roundtrip(path, Codepage::CP1047);
}

#[test]
fn cp1047_full_printable_ascii_roundtrip() {
    let printable: String = (0x20u8..=0x7Eu8).map(|b| b as char).collect();
    assert_roundtrip(&printable, Codepage::CP1047);
}

#[test]
fn cp1047_yen_and_overline_differ_from_cp037() {
    // CP1047 moves Ý from 0xAD to 0xBA (vs CP037: Ý at 0xAD → not, [  at 0xAD → [)
    // In CP1047: 0xAD = [, 0xBA = Ý
    // In CP037:  0xAD = soft-hyphen, 0xBA = [
    let cp1047_ad = ebcdic_to_utf8(&[0xAD], Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    let cp037_ad = ebcdic_to_utf8(&[0xAD], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_ne!(
        cp1047_ad, cp037_ad,
        "0xAD maps differently between CP1047 and CP037"
    );
}

// ===========================================================================
// CP1140 (Euro variant of CP037)
// ===========================================================================

#[test]
fn cp1140_euro_sign_encoding() {
    // CP1140 differs from CP037 at exactly ONE byte: 0xFF
    // CP037:  0xFF = control char (U+009F)
    // CP1140: 0xFF = € (Euro sign U+20AC)
    let cp1140_result =
        ebcdic_to_utf8(&[0xFF], Codepage::CP1140, UnmappablePolicy::Replace).unwrap();
    assert_eq!(cp1140_result, "€");

    // CP037 0xFF maps to a control char (U+009F) which is unmappable
    let cp037_result = ebcdic_to_utf8(&[0xFF], Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_ne!(cp037_result, "€");
}

#[test]
fn cp1140_euro_roundtrip() {
    let text = "€";
    assert_roundtrip(text, Codepage::CP1140);
}

#[test]
fn cp1140_identical_to_cp037_except_0xff() {
    // Every byte except 0xFF should decode identically in CP037 and CP1140
    for byte in 0x00u8..=0xFEu8 {
        let data = [byte];
        let cp037_result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace);
        let cp1140_result = ebcdic_to_utf8(&data, Codepage::CP1140, UnmappablePolicy::Replace);

        match (cp037_result, cp1140_result) {
            (Ok(a), Ok(b)) => {
                assert_eq!(
                    a, b,
                    "CP037 vs CP1140 mismatch at byte 0x{byte:02X}: {a:?} vs {b:?}"
                );
            }
            (Err(_), Err(_)) => {} // both error — fine
            (a, b) => panic!("CP037 vs CP1140 divergence at byte 0x{byte:02X}: {a:?} vs {b:?}"),
        }
    }
}

#[test]
fn cp1140_currency_sign_available() {
    // ¤ (U+00A4) CAN be encoded in CP1140 at 0x9F (same position as CP037)
    let result = utf8_to_ebcdic("¤", Codepage::CP1140);
    assert!(result.is_ok(), "¤ should be mappable in CP1140");
    assert_eq!(result.unwrap(), vec![0x9F]);
}

// ===========================================================================
// Cross-codepage tests
// ===========================================================================

#[test]
fn cross_same_ebcdic_bytes_different_output() {
    // Byte 0x4A produces different results across codepages
    let byte = [0x4A];
    let cp037 = ebcdic_to_utf8(&byte, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    let cp273 = ebcdic_to_utf8(&byte, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    let cp500 = ebcdic_to_utf8(&byte, Codepage::CP500, UnmappablePolicy::Error).unwrap();

    // CP037: ¢, CP273: Ä, CP500: [
    assert_eq!(cp037, "¢");
    assert_eq!(cp273, "Ä");
    assert_eq!(cp500, "[");
}

#[test]
fn cross_same_ascii_string_different_ebcdic() {
    // The character "[" encodes to different EBCDIC bytes in different codepages
    let cp037_bytes = utf8_to_ebcdic("[", Codepage::CP037).unwrap();
    let cp500_bytes = utf8_to_ebcdic("[", Codepage::CP500).unwrap();
    let cp1047_bytes = utf8_to_ebcdic("[", Codepage::CP1047).unwrap();

    assert_ne!(cp037_bytes, cp500_bytes);
    assert_ne!(cp037_bytes, cp1047_bytes);
    assert_ne!(cp500_bytes, cp1047_bytes);
}

#[test]
fn cross_digits_codepage_independent() {
    // Digits 0-9 are at 0xF0-0xF9 across ALL EBCDIC codepages
    let digits = "0123456789";
    let expected: Vec<u8> = (0xF0..=0xF9).collect();

    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let ebcdic = utf8_to_ebcdic(digits, cp).unwrap();
        assert_eq!(ebcdic, expected, "digits encode differently in {cp}");
    }
}

#[test]
fn cross_uppercase_letters_codepage_independent() {
    // A-Z positions are the same across all supported EBCDIC codepages
    let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let ref_bytes = utf8_to_ebcdic(alpha, Codepage::CP037).unwrap();

    for cp in [
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let bytes = utf8_to_ebcdic(alpha, cp).unwrap();
        assert_eq!(bytes, ref_bytes, "A-Z encode differently in {cp}");
    }
}

#[test]
fn cross_lowercase_letters_codepage_independent() {
    let alpha = "abcdefghijklmnopqrstuvwxyz";
    let ref_bytes = utf8_to_ebcdic(alpha, Codepage::CP037).unwrap();

    for cp in [
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let bytes = utf8_to_ebcdic(alpha, cp).unwrap();
        assert_eq!(bytes, ref_bytes, "a-z encode differently in {cp}");
    }
}

#[test]
fn cross_numeric_field_codec_roundtrip_all_codepages() {
    // Numeric (DISPLAY) fields use zoned decimal (0xF0-0xF9) — codepage-independent
    let copybook = "01 REC.\n   05 FLD PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();

    // EBCDIC digits 12345 → 0xF1 0xF2 0xF3 0xF4 0xF5
    let ebcdic_data: Vec<u8> = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5];

    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let json = decode_record(&schema, &ebcdic_data, &decode_opts(cp)).unwrap();
        let fld = json.get("FLD").expect("FLD missing");
        assert_eq!(
            fld.as_str().unwrap_or_default(),
            "12345",
            "numeric decode differs for {cp}"
        );
    }
}

#[test]
fn cross_comp3_is_codepage_independent() {
    // COMP-3 (packed decimal) is pure binary — no character encoding involved
    let copybook = "01 REC.\n   05 FLD PIC 9(5) COMP-3.";
    let schema = parse_copybook(copybook).unwrap();

    // Packed decimal 12345: PIC 9(5) COMP-3 = 3 bytes: 0x12 0x34 0x5F
    let packed: Vec<u8> = vec![0x12, 0x34, 0x5F];

    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let json = decode_record(&schema, &packed, &decode_opts(cp)).unwrap();
        let fld = json.get("FLD").expect("FLD missing");
        assert_eq!(
            fld.as_str().unwrap_or_default(),
            "12345",
            "COMP-3 decode differs for {cp}"
        );
    }
}

#[test]
fn cross_comp_binary_is_codepage_independent() {
    // COMP (binary) is pure binary — no character encoding involved
    let copybook = "01 REC.\n   05 FLD PIC 9(4) COMP.";
    let schema = parse_copybook(copybook).unwrap();

    // Binary 42 = 0x00 0x2A (big-endian 2-byte)
    let binary_data: Vec<u8> = vec![0x00, 0x2A];

    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let json = decode_record(&schema, &binary_data, &decode_opts(cp)).unwrap();
        let fld = json.get("FLD").expect("FLD missing");
        assert_eq!(
            fld.as_str().unwrap_or_default(),
            "42",
            "COMP decode differs for {cp}"
        );
    }
}

#[test]
fn cross_pic_x_field_roundtrip_cp037() {
    // PIC X field with EBCDIC codepage: full codec roundtrip
    let copybook = "01 REC.\n   05 FLD PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();

    // "HELLO" in EBCDIC CP037: H=0xC8, E=0xC5, L=0xD3, L=0xD3, O=0xD6
    let ebcdic_hello: Vec<u8> = vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6];

    let json = decode_record(&schema, &ebcdic_hello, &decode_opts(Codepage::CP037)).unwrap();
    let fld = json.get("FLD").expect("FLD missing");
    assert_eq!(fld.as_str().unwrap(), "HELLO");

    let encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).unwrap();
    assert_eq!(encoded, ebcdic_hello);
}

#[test]
fn cross_pic_x_field_roundtrip_cp500() {
    let copybook = "01 REC.\n   05 FLD PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();

    // Letters A-Z are at same positions, so HELLO encodes identically
    let ebcdic_hello: Vec<u8> = vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6];

    let json = decode_record(&schema, &ebcdic_hello, &decode_opts(Codepage::CP500)).unwrap();
    let fld = json.get("FLD").expect("FLD missing");
    assert_eq!(fld.as_str().unwrap(), "HELLO");

    let encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP500)).unwrap();
    assert_eq!(encoded, ebcdic_hello);
}

#[test]
fn cross_pic_x_field_roundtrip_cp1140() {
    let copybook = "01 REC.\n   05 FLD PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();

    let ebcdic_hello: Vec<u8> = vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6];

    let json = decode_record(&schema, &ebcdic_hello, &decode_opts(Codepage::CP1140)).unwrap();
    let fld = json.get("FLD").expect("FLD missing");
    assert_eq!(fld.as_str().unwrap(), "HELLO");

    let encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP1140)).unwrap();
    assert_eq!(encoded, ebcdic_hello);
}

// ===========================================================================
// Error and edge cases
// ===========================================================================

#[test]
fn error_unmappable_policy_error() {
    // Byte 0x00 maps to NUL (U+0000) which is an unmappable control char
    let data = [0x00];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Error);
    assert!(result.is_err());
}

#[test]
fn error_unmappable_policy_replace() {
    let data = [0x00];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_eq!(result, "\u{FFFD}");
}

#[test]
fn error_unmappable_policy_skip() {
    let data = [0x00];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Skip).unwrap();
    assert_eq!(result, "");
}

#[test]
fn error_unmappable_in_codec_decode() {
    // Use a PIC X field and force a control byte through.
    // Byte 0x01 → U+0001 which is unmappable; with Replace policy it becomes U+FFFD.
    let copybook = "01 REC.\n   05 FLD PIC X(1).";
    let schema = parse_copybook(copybook).unwrap();

    let data = [0x01]; // SOH control
    let opts = decode_opts_with_policy(Codepage::CP037, UnmappablePolicy::Replace);
    let json = decode_record(&schema, &data, &opts).unwrap();
    let fld = json.get("FLD").expect("FLD missing");
    assert!(fld.as_str().unwrap().contains('\u{FFFD}'));
}

#[test]
fn error_utf8_to_ebcdic_unmappable_char() {
    // Chinese character cannot be encoded in any EBCDIC codepage
    let result = utf8_to_ebcdic("你", Codepage::CP037);
    assert!(result.is_err());
}

#[test]
fn edge_high_byte_ebcdic_values() {
    // High-byte values 0xFA..0xFF should decode without panic
    for byte in 0xFA..=0xFF {
        let data = [byte];
        let _ = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace);
    }
}

#[test]
fn edge_null_byte_handling() {
    // 0x00 in EBCDIC maps to NUL (U+0000)
    let data = [0x00];
    // Error policy should reject it
    assert!(ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Error).is_err());
    // Replace policy should produce U+FFFD
    let replaced = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_eq!(replaced, "\u{FFFD}");
    // Skip policy should produce empty string
    let skipped = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Skip).unwrap();
    assert_eq!(skipped, "");
}

#[test]
fn edge_empty_input() {
    let data: &[u8] = &[];
    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let result = ebcdic_to_utf8(data, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, "");
    }
}

#[test]
fn edge_full_256_byte_table_cp037_no_panic() {
    // Decode all 256 possible EBCDIC byte values — none should panic
    let all_bytes: Vec<u8> = (0x00..=0xFF).collect();
    let result = ebcdic_to_utf8(&all_bytes, Codepage::CP037, UnmappablePolicy::Replace);
    assert!(result.is_ok());
    let text = result.unwrap();
    // Should have produced something for each byte (replaced or actual character)
    assert!(!text.is_empty());
}

#[test]
fn edge_full_256_byte_table_all_codepages_no_panic() {
    let all_bytes: Vec<u8> = (0x00..=0xFF).collect();
    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let result = ebcdic_to_utf8(&all_bytes, cp, UnmappablePolicy::Replace);
        assert!(result.is_ok(), "full 256-byte decode panicked for {cp}");
    }
}

#[test]
fn edge_space_byte_all_codepages() {
    // 0x40 is space in every EBCDIC codepage
    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let result = ebcdic_to_utf8(&[0x40], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(result, " ", "0x40 should be space in {cp}");
    }
}

#[test]
fn edge_mixed_control_and_printable() {
    // Mix of printable and control bytes with Replace policy
    let data = [0x40, 0x00, 0xC1, 0x01, 0xF0]; // space, NUL, A, SOH, 0
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert!(result.contains(' '));
    assert!(result.contains('A'));
    assert!(result.contains('0'));
    assert!(result.contains('\u{FFFD}'));
}

#[test]
fn edge_repeated_space_bytes() {
    // Common in mainframe data: records padded with spaces
    let data = vec![0x40; 80]; // 80 EBCDIC spaces
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, " ".repeat(80));
}

#[test]
fn edge_encode_then_decode_consistency() {
    // For every codepage, encoding then decoding a string should be identity
    let test_strings = ["HELLO WORLD", "12345", "A B C"];

    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        for text in &test_strings {
            let ebcdic = utf8_to_ebcdic(text, cp).unwrap();
            let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(
                &back, text,
                "encode→decode roundtrip failed for {cp}: {text}"
            );
        }
    }
}
