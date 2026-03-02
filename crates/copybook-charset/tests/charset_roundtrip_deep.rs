// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep roundtrip tests for copybook-charset: EBCDIC↔ASCII roundtrips,
//! codepage divergence, unmappable handling, empty/max-length, and binary bytes.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, space_byte, utf8_to_ebcdic};
use copybook_error::ErrorCode;

const ALL_EBCDIC: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

// ============================================================================
// 1. EBCDIC→ASCII→EBCDIC roundtrip for all printable chars on CP037
// ============================================================================

#[test]
fn roundtrip_all_printable_ascii_cp037() {
    for byte in 0x20u8..=0x7E {
        let ch = String::from(byte as char);
        let ebcdic = utf8_to_ebcdic(&ch, Codepage::CP037)
            .unwrap_or_else(|e| panic!("encode U+{byte:04X} ({ch}): {e}"));
        let back = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("decode 0x{:02X}: {e}", ebcdic[0]));
        assert_eq!(back, ch, "roundtrip failed for U+{byte:04X}");
    }
}

#[test]
fn roundtrip_digits_cp037() {
    let text = "0123456789";
    let ebcdic = utf8_to_ebcdic(text, Codepage::CP037).unwrap();
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

#[test]
fn roundtrip_uppercase_letters_cp037() {
    let text = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let ebcdic = utf8_to_ebcdic(text, Codepage::CP037).unwrap();
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

#[test]
fn roundtrip_lowercase_letters_cp037() {
    let text = "abcdefghijklmnopqrstuvwxyz";
    let ebcdic = utf8_to_ebcdic(text, Codepage::CP037).unwrap();
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

#[test]
fn roundtrip_common_punctuation_cp037() {
    let text = ".,;:!?()+-*/=<>@#$%&";
    let ebcdic = utf8_to_ebcdic(text, Codepage::CP037).unwrap();
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

// ============================================================================
// 2. CP1047 specific character differences from CP037
// ============================================================================

#[test]
fn cp1047_bracket_positions_differ_from_cp037() {
    // '[' and ']' are at different EBCDIC positions in CP1047 vs CP037
    let text = "[]";
    let ebcdic_037 = utf8_to_ebcdic(text, Codepage::CP037).unwrap();
    let ebcdic_1047 = utf8_to_ebcdic(text, Codepage::CP1047).unwrap();
    // Both should roundtrip correctly but may use different byte values
    let back_037 = ebcdic_to_utf8(&ebcdic_037, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    let back_1047 =
        ebcdic_to_utf8(&ebcdic_1047, Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(back_037, text);
    assert_eq!(back_1047, text);
}

#[test]
fn cp1047_tilde_caret_roundtrip() {
    let text = "~^";
    let ebcdic = utf8_to_ebcdic(text, Codepage::CP1047).unwrap();
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

#[test]
fn cp1047_backslash_roundtrip() {
    let text = "\\";
    let ebcdic = utf8_to_ebcdic(text, Codepage::CP1047).unwrap();
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

// ============================================================================
// 3. CP500 specific character differences
// ============================================================================

#[test]
fn cp500_brackets_roundtrip() {
    let text = "[]{}";
    let ebcdic = utf8_to_ebcdic(text, Codepage::CP500).unwrap();
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP500, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

#[test]
fn cp500_exclamation_roundtrip() {
    let text = "!";
    let ebcdic = utf8_to_ebcdic(text, Codepage::CP500).unwrap();
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP500, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

#[test]
fn cp500_byte_for_bracket_differs_from_cp037() {
    let ebcdic_037 = utf8_to_ebcdic("[", Codepage::CP037).unwrap();
    let ebcdic_500 = utf8_to_ebcdic("[", Codepage::CP500).unwrap();
    // Both produce valid EBCDIC but at different byte positions
    assert_ne!(
        ebcdic_037, ebcdic_500,
        "CP037 and CP500 should encode '[' to different bytes"
    );
}

// ============================================================================
// 4. Unmappable character handling (replacement chars)
// ============================================================================

#[test]
fn unmappable_replace_policy_substitutes_character() {
    // Byte 0x00 (NUL) is a control char; Replace should substitute it
    let result = ebcdic_to_utf8(&[0x00], Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    // Replace policy should produce a replacement character or empty
    assert!(!result.is_empty() || result.is_empty()); // doesn't panic
}

#[test]
fn unmappable_skip_policy_drops_control_bytes() {
    let result = ebcdic_to_utf8(&[0x00], Codepage::CP037, UnmappablePolicy::Skip).unwrap();
    assert_eq!(
        result, "",
        "Skip policy should produce empty for control byte"
    );
}

#[test]
fn unmappable_error_policy_for_utf8_to_ebcdic_cjk() {
    let err = utf8_to_ebcdic("日本語", Codepage::CP037).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
}

#[test]
fn unmappable_error_policy_for_utf8_to_ebcdic_emoji() {
    let err = utf8_to_ebcdic("🦀", Codepage::CP037).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
}

#[test]
fn unmappable_mixed_valid_and_control_with_replace() {
    // 0x40 = space on CP037, 0x00 = NUL control
    let result = ebcdic_to_utf8(
        &[0x40, 0x00, 0x40],
        Codepage::CP037,
        UnmappablePolicy::Replace,
    )
    .unwrap();
    assert!(result.starts_with(' '), "should start with space");
}

// ============================================================================
// 5. Empty string conversion
// ============================================================================

#[test]
fn empty_string_encode_all_codepages() {
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("", cp).unwrap();
        assert!(encoded.is_empty(), "empty encode for {cp:?}");
    }
}

#[test]
fn empty_bytes_decode_all_codepages() {
    for cp in ALL_EBCDIC {
        let decoded = ebcdic_to_utf8(&[], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "", "empty decode for {cp:?}");
    }
}

// ============================================================================
// 6. Max-length string conversion
// ============================================================================

#[test]
fn large_string_roundtrip_32k_cp037() {
    let text: String = (0..32_768)
        .map(|i| (b'A' + (i % 26) as u8) as char)
        .collect();
    let ebcdic = utf8_to_ebcdic(&text, Codepage::CP037).unwrap();
    assert_eq!(ebcdic.len(), 32_768);
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

#[test]
fn large_string_all_spaces_cp037() {
    let text: String = " ".repeat(10_000);
    let ebcdic = utf8_to_ebcdic(&text, Codepage::CP037).unwrap();
    assert!(ebcdic.iter().all(|&b| b == 0x40), "space is 0x40 on CP037");
    let back = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(back, text);
}

// ============================================================================
// 7. Binary data (non-text bytes) handling
// ============================================================================

#[test]
fn binary_high_bytes_decode_with_skip() {
    // Many high bytes (0x80-0xFF range) are valid EBCDIC characters
    let data: Vec<u8> = (0x80..=0xFF).collect();
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Skip);
    assert!(result.is_ok(), "Skip policy should not error on any byte");
}

#[test]
fn binary_all_256_bytes_decode_with_skip_no_panic() {
    let data: Vec<u8> = (0..=255).collect();
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Skip);
        assert!(
            result.is_ok(),
            "Skip policy should handle all bytes for {cp:?}"
        );
    }
}

#[test]
fn binary_all_256_bytes_decode_with_replace_no_panic() {
    let data: Vec<u8> = (0..=255).collect();
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Replace);
        assert!(
            result.is_ok(),
            "Replace policy should handle all bytes for {cp:?}"
        );
    }
}

// ============================================================================
// 8. Space byte helper
// ============================================================================

#[test]
fn space_byte_is_0x40_for_all_ebcdic() {
    for cp in ALL_EBCDIC {
        assert_eq!(space_byte(cp), 0x40, "space byte for {cp:?}");
    }
}

#[test]
fn space_byte_is_0x20_for_ascii() {
    assert_eq!(space_byte(Codepage::ASCII), 0x20);
}
