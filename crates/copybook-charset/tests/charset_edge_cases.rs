// SPDX-License-Identifier: AGPL-3.0-or-later
//! Edge-case tests for copybook-charset: roundtrip fidelity, non-printable bytes,
//! full 256-byte table coverage, unmappable character handling, and ASCII passthrough.
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
// 1. All printable ASCII roundtrip per codepage
// ============================================================================

#[test]
fn roundtrip_printable_ascii_all_codepages() {
    let printable: String = (0x20u8..=0x7E).map(|b| b as char).collect();
    for cp in ALL_EBCDIC {
        let ebcdic = utf8_to_ebcdic(&printable, cp)
            .unwrap_or_else(|e| panic!("{cp}: encode printable failed: {e}"));
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("{cp}: decode printable failed: {e}"));
        assert_eq!(back, printable, "{cp}: roundtrip mismatch");
    }
}

// ============================================================================
// 2. Individual ASCII character roundtrip (spot checks)
// ============================================================================

#[test]
fn roundtrip_space_character() {
    for cp in ALL_EBCDIC {
        let ebcdic = utf8_to_ebcdic(" ", cp).unwrap();
        assert_eq!(ebcdic.len(), 1);
        assert_eq!(ebcdic[0], space_byte(cp), "{cp}: space byte mismatch");
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, " ", "{cp}: space roundtrip");
    }
}

#[test]
fn roundtrip_digits_0_through_9() {
    for cp in ALL_EBCDIC {
        let text = "0123456789";
        let ebcdic = utf8_to_ebcdic(text, cp).unwrap();
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, text, "{cp}: digits roundtrip");
    }
}

#[test]
fn roundtrip_uppercase_letters() {
    for cp in ALL_EBCDIC {
        let text = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let ebcdic = utf8_to_ebcdic(text, cp).unwrap();
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, text, "{cp}: uppercase roundtrip");
    }
}

#[test]
fn roundtrip_lowercase_letters() {
    for cp in ALL_EBCDIC {
        let text = "abcdefghijklmnopqrstuvwxyz";
        let ebcdic = utf8_to_ebcdic(text, cp).unwrap();
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, text, "{cp}: lowercase roundtrip");
    }
}

#[test]
fn roundtrip_common_punctuation() {
    for cp in ALL_EBCDIC {
        let text = ".,;:!?-+*/=<>()";
        let ebcdic = utf8_to_ebcdic(text, cp).unwrap();
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, text, "{cp}: punctuation roundtrip");
    }
}

// ============================================================================
// 3. Full 256-byte table coverage for CP037
// ============================================================================

#[test]
fn cp037_all_256_bytes_decode_without_panic() {
    let all_bytes: Vec<u8> = (0u8..=255).collect();
    // Replace policy should never panic regardless of byte value
    let result = ebcdic_to_utf8(&all_bytes, Codepage::CP037, UnmappablePolicy::Replace);
    assert!(result.is_ok(), "Replace policy should handle all 256 bytes");
}

#[test]
fn cp037_all_256_bytes_skip_policy_no_panic() {
    let all_bytes: Vec<u8> = (0u8..=255).collect();
    let result = ebcdic_to_utf8(&all_bytes, Codepage::CP037, UnmappablePolicy::Skip);
    assert!(result.is_ok(), "Skip policy should handle all 256 bytes");
}

#[test]
fn cp037_skip_produces_fewer_or_equal_chars_vs_replace() {
    let all_bytes: Vec<u8> = (0u8..=255).collect();
    let replaced = ebcdic_to_utf8(&all_bytes, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    let skipped = ebcdic_to_utf8(&all_bytes, Codepage::CP037, UnmappablePolicy::Skip).unwrap();
    assert!(
        skipped.chars().count() <= replaced.chars().count(),
        "Skip should produce ≤ chars vs Replace"
    );
}

#[test]
fn all_ebcdic_codepages_decode_256_bytes_with_replace() {
    let all_bytes: Vec<u8> = (0u8..=255).collect();
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&all_bytes, cp, UnmappablePolicy::Replace);
        assert!(result.is_ok(), "{cp}: Replace should handle all 256 bytes");
        let decoded = result.unwrap();
        // Every codepage maps 256 bytes; Replace adds U+FFFD for unmappable ones
        assert!(
            !decoded.is_empty(),
            "{cp}: decoded output should not be empty"
        );
    }
}

// ============================================================================
// 4. Non-printable / control character bytes
// ============================================================================

#[test]
fn non_printable_byte_0x00_error_policy_fails_cp037() {
    // Byte 0x00 maps to U+0000 (NUL) which is a control character < 0x20
    let data = [0x00u8];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Error);
    assert!(result.is_err(), "byte 0x00 should fail with Error policy");
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
    );
}

#[test]
fn non_printable_byte_0x00_replace_policy_produces_replacement_char() {
    let data = [0x00u8];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_eq!(
        result, "\u{FFFD}",
        "byte 0x00 with Replace should produce U+FFFD"
    );
}

#[test]
fn non_printable_byte_0x00_skip_policy_produces_empty() {
    let data = [0x00u8];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Skip).unwrap();
    assert_eq!(
        result, "",
        "byte 0x00 with Skip should produce empty string"
    );
}

#[test]
fn multiple_non_printable_bytes_replace_policy() {
    // Bytes 0x01, 0x02, 0x03 all map to control chars
    let data = [0x01u8, 0x02, 0x03];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_eq!(result.chars().filter(|&c| c == '\u{FFFD}').count(), 3);
}

#[test]
fn mixed_printable_and_non_printable_replace_policy() {
    // 0x40 = space in CP037, 0x00 = NUL
    let data = [0x40u8, 0x00, 0x40];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_eq!(result, " \u{FFFD} ");
}

// ============================================================================
// 5. Unmappable character handling in encode direction
// ============================================================================

#[test]
fn utf8_to_ebcdic_unmappable_char_returns_error() {
    // Emoji cannot be mapped to any EBCDIC codepage
    let result = utf8_to_ebcdic("🦀", Codepage::CP037);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
    );
}

#[test]
fn utf8_to_ebcdic_cjk_char_returns_error() {
    let result = utf8_to_ebcdic("中", Codepage::CP037);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
    );
}

#[test]
fn utf8_to_ebcdic_mixed_valid_and_invalid_stops_at_first_error() {
    let result = utf8_to_ebcdic("ABC🦀DEF", Codepage::CP037);
    assert!(result.is_err(), "should fail at emoji");
}

// ============================================================================
// 6. Empty input edge cases
// ============================================================================

#[test]
fn encode_empty_string_produces_empty_vec() {
    for cp in ALL_EBCDIC {
        let result = utf8_to_ebcdic("", cp).unwrap();
        assert!(result.is_empty(), "{cp}: empty encode");
    }
}

#[test]
fn decode_empty_bytes_produces_empty_string() {
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&[], cp, UnmappablePolicy::Error).unwrap();
        assert!(result.is_empty(), "{cp}: empty decode");
    }
}

// ============================================================================
// 7. ASCII passthrough mode
// ============================================================================

#[test]
fn ascii_codepage_decode_is_passthrough() {
    let data: Vec<u8> = (0x20..=0x7E).collect();
    let result = ebcdic_to_utf8(&data, Codepage::ASCII, UnmappablePolicy::Error).unwrap();
    let expected: String = (0x20u8..=0x7E).map(|b| b as char).collect();
    assert_eq!(result, expected);
}

#[test]
fn ascii_codepage_encode_is_passthrough() {
    let text = "Hello, World!";
    let result = utf8_to_ebcdic(text, Codepage::ASCII).unwrap();
    assert_eq!(result, text.as_bytes());
}

#[test]
fn ascii_codepage_roundtrip() {
    let text = "COBOL IS GREAT 12345";
    let encoded = utf8_to_ebcdic(text, Codepage::ASCII).unwrap();
    let decoded = ebcdic_to_utf8(&encoded, Codepage::ASCII, UnmappablePolicy::Error).unwrap();
    assert_eq!(decoded, text);
}

// ============================================================================
// 8. Single byte roundtrip for all printable ASCII per codepage
// ============================================================================

#[test]
fn single_byte_roundtrip_cp037_all_printable() {
    for byte in 0x20u8..=0x7E {
        let ch = String::from(byte as char);
        let ebcdic = utf8_to_ebcdic(&ch, Codepage::CP037)
            .unwrap_or_else(|e| panic!("encode '{ch}' (0x{byte:02X}) failed: {e}"));
        assert_eq!(ebcdic.len(), 1, "'{ch}' should encode to 1 byte");
        let back = ebcdic_to_utf8(&ebcdic, Codepage::CP037, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("decode '{ch}' failed: {e}"));
        assert_eq!(back, ch, "roundtrip mismatch for '{ch}' (0x{byte:02X})");
    }
}

// ============================================================================
// 9. Cross-codepage: same EBCDIC byte decodes differently
// ============================================================================

#[test]
fn byte_0x4a_decodes_differently_across_codepages() {
    let data = [0x4Au8];
    let cp037 = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    let cp273 = ebcdic_to_utf8(&data, Codepage::CP273, UnmappablePolicy::Replace).unwrap();
    // These codepages have different mappings for 0x4A
    // CP037: 0x4A -> '¢' (U+00A2), CP273: different mapping
    assert_ne!(
        cp037, cp273,
        "byte 0x4A should decode differently between CP037 and CP273"
    );
}

// ============================================================================
// 10. EBCDIC digits (0xF0-0xF9) decode to ASCII digits for all codepages
// ============================================================================

#[test]
fn ebcdic_digit_bytes_f0_f9_decode_to_ascii_digits() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            let byte = 0xF0 + digit;
            let decoded = ebcdic_to_utf8(&[byte], cp, UnmappablePolicy::Error).unwrap();
            let expected = format!("{digit}");
            assert_eq!(decoded, expected, "{cp}: EBCDIC 0x{byte:02X} -> '{digit}'");
        }
    }
}

// ============================================================================
// 11. EBCDIC space byte (0x40) roundtrips correctly
// ============================================================================

#[test]
fn ebcdic_space_byte_0x40_decodes_to_space() {
    for cp in ALL_EBCDIC {
        let decoded = ebcdic_to_utf8(&[0x40], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, " ", "{cp}: 0x40 should be space");
    }
}

// ============================================================================
// 12. Encode preserves byte length (1:1 for BMP characters)
// ============================================================================

#[test]
fn encode_preserves_length_for_ascii_text() {
    let text = "HELLO WORLD 12345";
    for cp in ALL_EBCDIC {
        let ebcdic = utf8_to_ebcdic(text, cp).unwrap();
        assert_eq!(
            ebcdic.len(),
            text.len(),
            "{cp}: EBCDIC length should equal UTF-8 length for ASCII"
        );
    }
}

// ============================================================================
// 13. Tab, LF, CR are allowed control characters
// ============================================================================

#[test]
fn tab_lf_cr_are_allowed_through_error_policy() {
    // For CP037:
    // Tab (0x09) comes from EBCDIC byte 0x05 -> U+0009 (tab)
    // LF (0x0A) comes from EBCDIC byte 0x25 -> U+000A (line feed)
    // CR (0x0D) comes from EBCDIC byte 0x0D -> U+000D (carriage return)
    // These should pass through even with Error policy because they are
    // explicitly allowed control characters.
    let tab_byte = [0x05u8]; // Maps to U+0009 (tab) in CP037
    let result = ebcdic_to_utf8(&tab_byte, Codepage::CP037, UnmappablePolicy::Error);
    assert!(result.is_ok(), "tab (U+0009) should be allowed");
    assert_eq!(result.unwrap(), "\t");

    let lf_byte = [0x25u8]; // Maps to U+000A (LF) in CP037
    let result = ebcdic_to_utf8(&lf_byte, Codepage::CP037, UnmappablePolicy::Error);
    assert!(result.is_ok(), "LF (U+000A) should be allowed");
    assert_eq!(result.unwrap(), "\n");

    let cr_byte = [0x0Du8]; // Maps to U+000D (CR) in CP037
    let result = ebcdic_to_utf8(&cr_byte, Codepage::CP037, UnmappablePolicy::Error);
    assert!(result.is_ok(), "CR (U+000D) should be allowed");
    assert_eq!(result.unwrap(), "\r");
}

// ============================================================================
// 14. Large buffer decode/encode
// ============================================================================

#[test]
fn large_buffer_roundtrip() {
    let text: String = "ABCDEFGHIJ".repeat(1000); // 10,000 chars
    for cp in ALL_EBCDIC {
        let ebcdic = utf8_to_ebcdic(&text, cp).unwrap();
        assert_eq!(ebcdic.len(), 10_000);
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, text, "{cp}: large buffer roundtrip");
    }
}

// ============================================================================
// 15. Repeated same-byte decode
// ============================================================================

#[test]
fn repeated_same_byte_decodes_to_repeated_char() {
    // 0xC1 in CP037 = 'A'
    let data = vec![0xC1u8; 5];
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(result, "AAAAA");
}
