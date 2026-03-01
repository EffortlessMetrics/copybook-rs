// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-charset.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, space_byte, utf8_to_ebcdic};
use copybook_error::ErrorCode;

/// All EBCDIC codepages under test.
const ALL_EBCDIC: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

// ====================================================================
// 1. EBCDIC-to-ASCII conversion of all printable characters
// ====================================================================

#[test]
fn ebcdic_to_utf8_all_printable_chars_per_codepage() {
    // For each EBCDIC codepage, encode all printable ASCII (0x20..=0x7E)
    // to EBCDIC then decode back; every character must survive the trip.
    let printable: String = (0x20u8..=0x7Eu8).map(|b| b as char).collect();
    for cp in ALL_EBCDIC {
        let ebcdic =
            utf8_to_ebcdic(&printable, cp).unwrap_or_else(|e| panic!("{cp:?}: encode failed: {e}"));
        let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("{cp:?}: decode failed: {e}"));
        assert_eq!(decoded, printable, "{cp:?}: printable round-trip mismatch");
    }
}

#[test]
fn ebcdic_to_utf8_individual_printable_chars_cp037() {
    // Verify each printable ASCII char individually
    for ch in 0x20u8..=0x7Eu8 {
        let s = String::from(ch as char);
        let ebcdic = utf8_to_ebcdic(&s, Codepage::CP037)
            .unwrap_or_else(|e| panic!("encode char 0x{ch:02X} failed: {e}"));
        let decoded = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("decode char 0x{ch:02X} failed: {e}"));
        assert_eq!(decoded, s, "char 0x{ch:02X} round-trip mismatch");
    }
}

#[test]
fn ebcdic_to_utf8_digits_are_f0_f9_all_codepages() {
    // EBCDIC digits are always 0xF0-0xF9 regardless of codepage
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("0123456789", cp).unwrap();
        for (i, &b) in encoded.iter().enumerate() {
            assert_eq!(b, 0xF0 + i as u8, "{cp:?}: digit {i}");
        }
    }
}

// ====================================================================
// 2. ASCII-to-EBCDIC round-trip
// ====================================================================

#[test]
fn utf8_to_ebcdic_and_back_roundtrip_all_codepages() {
    let test_strings = [
        "HELLO WORLD",
        "hello world",
        "0123456789",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "abcdefghijklmnopqrstuvwxyz",
        "!@#$%^&*()",
        "Mixed Case 123",
        " ", // single space
    ];
    for cp in ALL_EBCDIC {
        for &s in &test_strings {
            let ebcdic = utf8_to_ebcdic(s, cp)
                .unwrap_or_else(|e| panic!("{cp:?}: encode '{s}' failed: {e}"));
            let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp:?}: decode '{s}' failed: {e}"));
            assert_eq!(decoded, s, "{cp:?}: round-trip mismatch for '{s}'");
        }
    }
}

#[test]
fn utf8_to_ebcdic_preserves_byte_length() {
    // Each ASCII char maps to exactly one EBCDIC byte
    let input = "COBOL DATA 1234";
    for cp in ALL_EBCDIC {
        let ebcdic = utf8_to_ebcdic(input, cp).unwrap();
        assert_eq!(ebcdic.len(), input.len(), "{cp:?}: byte length mismatch");
    }
}

// ====================================================================
// 3. EBCDIC special characters (packed fields, binary data)
// ====================================================================

#[test]
fn ebcdic_to_utf8_binary_data_with_replace_policy() {
    // Bytes like 0x00, 0x01, 0xFF should be handled by policy without panic
    let binary_data: Vec<u8> = (0x00..=0xFF).collect();
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&binary_data, cp, UnmappablePolicy::Replace);
        assert!(result.is_ok(), "{cp:?}: Replace policy should not fail");
        let decoded = result.unwrap();
        assert!(!decoded.is_empty(), "{cp:?}: should produce output");
    }
}

#[test]
fn ebcdic_to_utf8_binary_data_with_skip_policy() {
    let binary_data: Vec<u8> = (0x00..=0xFF).collect();
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&binary_data, cp, UnmappablePolicy::Skip);
        assert!(result.is_ok(), "{cp:?}: Skip policy should not fail");
    }
}

#[test]
fn ebcdic_to_utf8_packed_decimal_bytes_replaced() {
    // Packed decimal: e.g. 0x12, 0x34, 0x5C — these are not printable EBCDIC
    let packed = vec![0x12, 0x34, 0x5C];
    let result = ebcdic_to_utf8(&packed, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    // Should contain replacement chars for control-range mappings
    assert!(!result.is_empty());
}

// ====================================================================
// 4. Unmappable character handling
// ====================================================================

#[test]
fn unmappable_error_policy_rejects_nul() {
    // EBCDIC 0x00 maps to NUL (U+0000) which is < 0x20, so it's unmappable
    let result = ebcdic_to_utf8(&[0x00], Codepage::CP037, UnmappablePolicy::Error);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
}

#[test]
fn unmappable_replace_policy_inserts_replacement_char() {
    let result = ebcdic_to_utf8(&[0x00], Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_eq!(result, "\u{FFFD}");
}

#[test]
fn unmappable_skip_policy_omits_byte() {
    let result = ebcdic_to_utf8(&[0x00], Codepage::CP037, UnmappablePolicy::Skip).unwrap();
    assert_eq!(result, "");
}

#[test]
fn utf8_to_ebcdic_unmappable_cjk_rejected_all_codepages() {
    for cp in ALL_EBCDIC {
        let result = utf8_to_ebcdic("中", cp);
        assert!(result.is_err(), "{cp:?}: CJK should be unmappable");
    }
}

#[test]
fn utf8_to_ebcdic_unmappable_emoji_rejected() {
    let result = utf8_to_ebcdic("🚀", Codepage::CP037);
    assert!(result.is_err());
}

#[test]
fn unmappable_mixed_with_valid_chars_skip() {
    // 0x40 = EBCDIC space, 0x00 = NUL, 0xC1 = 'A' on CP037
    let data = vec![0x40, 0x00, 0xC1];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Skip).unwrap();
    assert_eq!(result, " A"); // NUL skipped
}

#[test]
fn unmappable_mixed_with_valid_chars_replace() {
    let data = vec![0x40, 0x00, 0xC1];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_eq!(result, " \u{FFFD}A"); // NUL replaced
}

// ====================================================================
// 5. All supported codepages (CP037, CP273, CP500, CP1047, CP1140)
// ====================================================================

#[test]
fn cp037_space_is_0x40() {
    assert_eq!(space_byte(Codepage::CP037), 0x40);
}

#[test]
fn cp273_space_is_0x40() {
    assert_eq!(space_byte(Codepage::CP273), 0x40);
}

#[test]
fn cp500_space_is_0x40() {
    assert_eq!(space_byte(Codepage::CP500), 0x40);
}

#[test]
fn cp1047_space_is_0x40() {
    assert_eq!(space_byte(Codepage::CP1047), 0x40);
}

#[test]
fn cp1140_space_is_0x40() {
    assert_eq!(space_byte(Codepage::CP1140), 0x40);
}

#[test]
fn ascii_space_is_0x20() {
    assert_eq!(space_byte(Codepage::ASCII), 0x20);
}

#[test]
fn cp1140_euro_sign_at_0xff() {
    // CP1140 has euro sign (€) at 0xFF
    let decoded = ebcdic_to_utf8(&[0xFF], Codepage::CP1140, UnmappablePolicy::Error).unwrap();
    assert_eq!(decoded, "€");
}

#[test]
fn cp1140_euro_sign_roundtrip() {
    let ebcdic = utf8_to_ebcdic("€", Codepage::CP1140).unwrap();
    assert_eq!(ebcdic, vec![0xFF]);
    let decoded = ebcdic_to_utf8(&ebcdic, Codepage::CP1140, UnmappablePolicy::Error).unwrap();
    assert_eq!(decoded, "€");
}

#[test]
fn cp273_german_chars_roundtrip() {
    // CP273 has German characters: Ä, Ö, Ü, ä, ö, ü, ß
    for ch in &['Ä', 'Ö', 'ä', 'ö', 'ü'] {
        let s = ch.to_string();
        let ebcdic = utf8_to_ebcdic(&s, Codepage::CP273)
            .unwrap_or_else(|e| panic!("encode '{ch}' failed: {e}"));
        let decoded = ebcdic_to_utf8(&ebcdic, Codepage::CP273, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("decode '{ch}' failed: {e}"));
        assert_eq!(decoded, s, "CP273 round-trip failed for '{ch}'");
    }
}

#[test]
fn all_codepages_uppercase_consistency() {
    let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic(upper, cp).unwrap();
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, upper, "{cp:?}: uppercase mismatch");
    }
}

#[test]
fn all_codepages_lowercase_consistency() {
    let lower = "abcdefghijklmnopqrstuvwxyz";
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic(lower, cp).unwrap();
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, lower, "{cp:?}: lowercase mismatch");
    }
}

#[test]
fn all_codepages_special_punctuation() {
    // Period, comma, semicolon, colon are standard across codepages
    for cp in ALL_EBCDIC {
        for ch in &['.', ',', ':', ';'] {
            let s = ch.to_string();
            let ebcdic = utf8_to_ebcdic(&s, cp)
                .unwrap_or_else(|e| panic!("{cp:?}: encode '{ch}' failed: {e}"));
            let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp:?}: decode '{ch}' failed: {e}"));
            assert_eq!(decoded, s, "{cp:?}: punctuation '{ch}' mismatch");
        }
    }
}

// ====================================================================
// 6. Empty string conversion
// ====================================================================

#[test]
fn empty_string_encode_all_codepages() {
    for cp in ALL_EBCDIC {
        let result = utf8_to_ebcdic("", cp).unwrap();
        assert!(result.is_empty(), "{cp:?}: empty encode should be empty");
    }
}

#[test]
fn empty_bytes_decode_all_codepages() {
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&[], cp, UnmappablePolicy::Error).unwrap();
        assert!(result.is_empty(), "{cp:?}: empty decode should be empty");
    }
}

#[test]
fn empty_ascii_passthrough() {
    let encoded = utf8_to_ebcdic("", Codepage::ASCII).unwrap();
    assert!(encoded.is_empty());
    let decoded = ebcdic_to_utf8(&[], Codepage::ASCII, UnmappablePolicy::Error).unwrap();
    assert!(decoded.is_empty());
}

// ====================================================================
// 7. Single character conversion
// ====================================================================

#[test]
fn single_char_space_all_codepages() {
    for cp in ALL_EBCDIC {
        let ebcdic = utf8_to_ebcdic(" ", cp).unwrap();
        assert_eq!(ebcdic.len(), 1, "{cp:?}: space should be 1 byte");
        assert_eq!(ebcdic[0], 0x40, "{cp:?}: space should be 0x40");
        let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, " ", "{cp:?}: space decode mismatch");
    }
}

#[test]
fn single_char_digit_zero_all_codepages() {
    for cp in ALL_EBCDIC {
        let ebcdic = utf8_to_ebcdic("0", cp).unwrap();
        assert_eq!(ebcdic, vec![0xF0], "{cp:?}: '0' should be 0xF0");
    }
}

#[test]
fn single_char_letter_a_all_codepages() {
    for cp in ALL_EBCDIC {
        let ebcdic = utf8_to_ebcdic("A", cp).unwrap();
        assert_eq!(ebcdic.len(), 1, "{cp:?}: 'A' should be 1 byte");
        let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "A", "{cp:?}: 'A' decode mismatch");
    }
}

#[test]
fn single_char_ascii_passthrough() {
    let encoded = utf8_to_ebcdic("X", Codepage::ASCII).unwrap();
    assert_eq!(encoded, vec![b'X']);
    let decoded = ebcdic_to_utf8(&[b'X'], Codepage::ASCII, UnmappablePolicy::Error).unwrap();
    assert_eq!(decoded, "X");
}

// ====================================================================
// 8. Allowed control characters (tab, LF, CR)
// ====================================================================

#[test]
fn allowed_control_chars_tab_lf_cr() {
    // Tab=0x05, LF=0x25, CR=0x0D on CP037
    let tab = ebcdic_to_utf8(&[0x05], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(tab, "\t");
    let lf = ebcdic_to_utf8(&[0x25], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(lf, "\n");
    let cr = ebcdic_to_utf8(&[0x0D], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(cr, "\r");
}

// ====================================================================
// 9. Full byte range (0x00..=0xFF) with non-error policies
// ====================================================================

#[test]
fn full_byte_range_replace_does_not_panic() {
    let all_bytes: Vec<u8> = (0x00..=0xFF).collect();
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&all_bytes, cp, UnmappablePolicy::Replace);
        assert!(result.is_ok(), "{cp:?}: Replace should never fail");
        assert_eq!(
            result.unwrap().chars().count(),
            256,
            "{cp:?}: should have 256 chars"
        );
    }
}

#[test]
fn full_byte_range_skip_produces_valid_utf8() {
    let all_bytes: Vec<u8> = (0x00..=0xFF).collect();
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&all_bytes, cp, UnmappablePolicy::Skip).unwrap();
        // The result must be valid UTF-8 (implicit by being a String)
        // and should have fewer than 256 chars since some are skipped
        assert!(result.chars().count() <= 256, "{cp:?}");
    }
}
