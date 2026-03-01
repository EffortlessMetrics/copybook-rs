// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep codepage conversion tests — ASCII↔EBCDIC roundtrip fidelity, boundary
//! bytes, unmappable-character policies, COBOL-common characters, and
//! performance sanity across all five supported codepages.
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, space_byte, utf8_to_ebcdic};
use copybook_error::ErrorCode;
use std::time::Instant;

const ALL_CODEPAGES: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP1047,
    Codepage::CP500,
    Codepage::CP273,
    Codepage::CP1140,
];

// ============================================================================
// 1. Full printable ASCII roundtrip per codepage (individual tests)
// ============================================================================

#[test]
fn cp037_printable_ascii_roundtrip() {
    assert_full_printable_roundtrip(Codepage::CP037);
}

#[test]
fn cp1047_printable_ascii_roundtrip() {
    assert_full_printable_roundtrip(Codepage::CP1047);
}

#[test]
fn cp500_printable_ascii_roundtrip() {
    assert_full_printable_roundtrip(Codepage::CP500);
}

#[test]
fn cp273_printable_ascii_roundtrip() {
    assert_full_printable_roundtrip(Codepage::CP273);
}

#[test]
fn cp1140_printable_ascii_roundtrip() {
    assert_full_printable_roundtrip(Codepage::CP1140);
}

fn assert_full_printable_roundtrip(cp: Codepage) {
    for byte in 0x20u8..=0x7E {
        let original = String::from(byte as char);
        let ebcdic = utf8_to_ebcdic(&original, cp)
            .unwrap_or_else(|e| panic!("{cp}: encode 0x{byte:02X} ('{original}'): {e}"));
        assert_eq!(
            ebcdic.len(),
            1,
            "{cp}: printable ASCII char must map to exactly 1 EBCDIC byte"
        );
        let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("{cp}: decode 0x{:02X}: {e}", ebcdic[0]));
        assert_eq!(
            decoded, original,
            "{cp}: roundtrip failed for 0x{byte:02X} ('{original}')"
        );
    }
}

// ============================================================================
// 2. Digit chars (0-9) roundtrip — all codepages
// ============================================================================

#[test]
fn digit_chars_roundtrip_all_codepages() {
    for cp in ALL_CODEPAGES {
        for digit in b'0'..=b'9' {
            let ch = String::from(digit as char);
            let ebcdic = utf8_to_ebcdic(&ch, cp).unwrap();
            // EBCDIC digits are always 0xF0..=0xF9
            assert_eq!(
                ebcdic[0],
                0xF0 + (digit - b'0'),
                "{cp}: digit '{ch}' should map to 0x{:02X}",
                0xF0 + (digit - b'0')
            );
            let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(back, ch, "{cp}: digit '{ch}' roundtrip failed");
        }
    }
}

// ============================================================================
// 3. Uppercase letter roundtrip — all codepages
// ============================================================================

#[test]
fn uppercase_letter_roundtrip_all_codepages() {
    for cp in ALL_CODEPAGES {
        for letter in b'A'..=b'Z' {
            let ch = String::from(letter as char);
            let ebcdic = utf8_to_ebcdic(&ch, cp).unwrap();
            assert_eq!(ebcdic.len(), 1, "{cp}: uppercase letter must be 1 byte");
            let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(back, ch, "{cp}: uppercase '{ch}' roundtrip failed");
        }
    }
}

// ============================================================================
// 4. Lowercase letter roundtrip — all codepages
// ============================================================================

#[test]
fn lowercase_letter_roundtrip_all_codepages() {
    for cp in ALL_CODEPAGES {
        for letter in b'a'..=b'z' {
            let ch = String::from(letter as char);
            let ebcdic = utf8_to_ebcdic(&ch, cp).unwrap();
            assert_eq!(ebcdic.len(), 1, "{cp}: lowercase letter must be 1 byte");
            let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(back, ch, "{cp}: lowercase '{ch}' roundtrip failed");
        }
    }
}

// ============================================================================
// 5. Special chars (space, period, comma, colon, semicolon, etc.)
// ============================================================================

#[test]
fn special_chars_roundtrip_all_codepages() {
    let specials = [
        ' ', '.', ',', ':', ';', '!', '?', '-', '+', '*', '/', '=', '(', ')', '<', '>', '_', '%',
        '&', '#', '@', '\'', '"',
    ];
    for cp in ALL_CODEPAGES {
        for &ch in &specials {
            let s = String::from(ch);
            let ebcdic =
                utf8_to_ebcdic(&s, cp).unwrap_or_else(|e| panic!("{cp}: encode '{ch}': {e}"));
            let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp}: decode '{ch}': {e}"));
            assert_eq!(back, s, "{cp}: special char '{ch}' roundtrip failed");
        }
    }
}

// ============================================================================
// 6. All 256 byte values through conversion — no panic
// ============================================================================

#[test]
fn all_256_bytes_no_panic_cp037() {
    assert_all_256_no_panic(Codepage::CP037);
}

#[test]
fn all_256_bytes_no_panic_cp1047() {
    assert_all_256_no_panic(Codepage::CP1047);
}

#[test]
fn all_256_bytes_no_panic_cp500() {
    assert_all_256_no_panic(Codepage::CP500);
}

#[test]
fn all_256_bytes_no_panic_cp273() {
    assert_all_256_no_panic(Codepage::CP273);
}

#[test]
fn all_256_bytes_no_panic_cp1140() {
    assert_all_256_no_panic(Codepage::CP1140);
}

fn assert_all_256_no_panic(cp: Codepage) {
    for byte in 0x00u8..=0xFF {
        // With Replace policy, every byte must produce a result without panic
        let result = ebcdic_to_utf8(&[byte], cp, UnmappablePolicy::Replace);
        assert!(
            result.is_ok(),
            "{cp}: byte 0x{byte:02X} must not panic or error with Replace policy"
        );
        let decoded = result.unwrap();
        assert!(
            !decoded.is_empty(),
            "{cp}: byte 0x{byte:02X} must decode to at least one char (possibly U+FFFD)"
        );
    }
}

// ============================================================================
// 7. Null byte (0x00) handling
// ============================================================================

#[test]
fn null_byte_handling_error_policy() {
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Error);
        assert!(
            result.is_err(),
            "{cp}: null byte should error with Error policy"
        );
        let err = result.unwrap_err();
        assert_eq!(
            err.code,
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
            "{cp}: null byte error code"
        );
    }
}

#[test]
fn null_byte_handling_replace_policy() {
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            result, "\u{FFFD}",
            "{cp}: null byte with Replace should yield U+FFFD"
        );
    }
}

#[test]
fn null_byte_handling_skip_policy() {
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Skip).unwrap();
        assert!(
            result.is_empty(),
            "{cp}: null byte with Skip should produce empty string"
        );
    }
}

// ============================================================================
// 8. 0xFF byte handling
// ============================================================================

#[test]
fn byte_0xff_handling_all_codepages() {
    // CP1140 maps 0xFF to € (U+20AC); other codepages map to various chars
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(&[0xFF], cp, UnmappablePolicy::Replace);
        assert!(result.is_ok(), "{cp}: 0xFF must not panic with Replace");
        let decoded = result.unwrap();
        assert!(!decoded.is_empty(), "{cp}: 0xFF should decode to something");
    }
    // CP1140 specifically: 0xFF = €
    let euro = ebcdic_to_utf8(&[0xFF], Codepage::CP1140, UnmappablePolicy::Error).unwrap();
    assert_eq!(euro, "€", "CP1140: 0xFF must be euro sign");
}

// ============================================================================
// 9. Unmappable character policy — Replace vs Error
// ============================================================================

#[test]
fn unmappable_policy_replace_inserts_fffd() {
    // Control byte 0x01 maps to U+0001 on all EBCDIC codepages → unmappable
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(&[0x01], cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            result, "\u{FFFD}",
            "{cp}: unmappable byte with Replace → U+FFFD"
        );
    }
}

#[test]
fn unmappable_policy_error_returns_error() {
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(&[0x01], cp, UnmappablePolicy::Error);
        assert!(result.is_err(), "{cp}: unmappable byte with Error → Err");
        assert_eq!(
            result.unwrap_err().code,
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
        );
    }
}

#[test]
fn unmappable_policy_skip_drops_byte() {
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(&[0x01], cp, UnmappablePolicy::Skip).unwrap();
        assert!(result.is_empty(), "{cp}: unmappable byte with Skip → empty");
    }
}

#[test]
fn unmappable_policy_replace_preserves_valid_neighbors() {
    // Sandwich: valid A (0xC1), unmappable 0x01, valid B (0xC2)
    let data: &[u8] = &[0xC1, 0x01, 0xC2];
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(data, cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            result, "A\u{FFFD}B",
            "{cp}: Replace must keep valid neighbors"
        );
    }
}

// ============================================================================
// 10. Large buffer conversion — performance sanity
// ============================================================================

#[test]
fn large_buffer_encode_decode_1mb() {
    let text: String = "ABCDEFGHIJ0123456789 ".repeat(1_024 * 1_024 / 21);
    let len = text.len();
    for cp in ALL_CODEPAGES {
        let start = Instant::now();
        let ebcdic = utf8_to_ebcdic(&text, cp).unwrap();
        let elapsed_enc = start.elapsed();

        assert_eq!(ebcdic.len(), len, "{cp}: encoded length mismatch");

        let start = Instant::now();
        let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        let elapsed_dec = start.elapsed();

        assert_eq!(decoded, text, "{cp}: large buffer roundtrip mismatch");

        // Generous 2-second ceiling per direction — just a sanity check
        assert!(
            elapsed_enc.as_secs() < 2,
            "{cp}: encode 1 MB took {:?}",
            elapsed_enc
        );
        assert!(
            elapsed_dec.as_secs() < 2,
            "{cp}: decode 1 MB took {:?}",
            elapsed_dec
        );
    }
}

// ============================================================================
// 11. Conversion of COBOL-common chars (spaces, zeros, hyphens)
// ============================================================================

#[test]
fn cobol_common_spaces_roundtrip() {
    let spaces = "          "; // 10 EBCDIC spaces
    for cp in ALL_CODEPAGES {
        let ebcdic = utf8_to_ebcdic(spaces, cp).unwrap();
        // All EBCDIC codepages encode space as 0x40
        assert!(
            ebcdic.iter().all(|&b| b == 0x40),
            "{cp}: spaces must encode to 0x40"
        );
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, spaces, "{cp}: spaces roundtrip");
    }
}

#[test]
fn cobol_common_zeros_roundtrip() {
    let zeros = "0000000000"; // 10 COBOL zeros
    for cp in ALL_CODEPAGES {
        let ebcdic = utf8_to_ebcdic(zeros, cp).unwrap();
        assert!(
            ebcdic.iter().all(|&b| b == 0xF0),
            "{cp}: zeros must encode to 0xF0"
        );
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, zeros, "{cp}: zeros roundtrip");
    }
}

#[test]
fn cobol_common_hyphens_roundtrip() {
    let hyphens = "----------"; // COBOL field separator
    for cp in ALL_CODEPAGES {
        let ebcdic = utf8_to_ebcdic(hyphens, cp).unwrap();
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, hyphens, "{cp}: hyphens roundtrip");
    }
}

#[test]
fn cobol_common_field_name_pattern() {
    // Typical COBOL field name: uppercase, hyphens, digits
    let field = "WS-CUSTOMER-ID-01";
    for cp in ALL_CODEPAGES {
        let ebcdic = utf8_to_ebcdic(field, cp).unwrap();
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, field, "{cp}: COBOL field name roundtrip");
    }
}

#[test]
fn cobol_padded_numeric_field() {
    // Right-justified numeric with leading zeros and trailing spaces
    let value = "00012345  ";
    for cp in ALL_CODEPAGES {
        let ebcdic = utf8_to_ebcdic(value, cp).unwrap();
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, value, "{cp}: padded numeric roundtrip");
    }
}

// ============================================================================
// 12. Space byte helper consistency
// ============================================================================

#[test]
fn space_byte_consistent_with_conversion() {
    for cp in ALL_CODEPAGES {
        let sb = space_byte(cp);
        let decoded = ebcdic_to_utf8(&[sb], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(
            decoded, " ",
            "{cp}: space_byte(0x{sb:02X}) must decode to ' '"
        );
        let encoded = utf8_to_ebcdic(" ", cp).unwrap();
        assert_eq!(encoded, vec![sb], "{cp}: ' ' must encode to space_byte");
    }
}

// ============================================================================
// 13. Bulk string of all printable ASCII — single encode/decode
// ============================================================================

#[test]
fn bulk_printable_string_roundtrip() {
    let printable: String = (0x20u8..=0x7E).map(|b| b as char).collect();
    for cp in ALL_CODEPAGES {
        let ebcdic = utf8_to_ebcdic(&printable, cp).unwrap();
        assert_eq!(
            ebcdic.len(),
            printable.len(),
            "{cp}: 1:1 byte mapping for printable ASCII"
        );
        let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(back, printable, "{cp}: bulk printable roundtrip");
    }
}

// ============================================================================
// 14. Multiple control bytes in sequence
// ============================================================================

#[test]
fn multiple_control_bytes_replace_policy() {
    let data: &[u8] = &[0x00, 0x01, 0x02, 0x03];
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(data, cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            result.chars().filter(|&c| c == '\u{FFFD}').count(),
            4,
            "{cp}: four control bytes → four replacements"
        );
    }
}

#[test]
fn multiple_control_bytes_skip_policy() {
    let data: &[u8] = &[0x00, 0x01, 0x02, 0x03];
    for cp in ALL_CODEPAGES {
        let result = ebcdic_to_utf8(data, cp, UnmappablePolicy::Skip).unwrap();
        assert!(
            result.is_empty(),
            "{cp}: four control bytes skipped → empty"
        );
    }
}

// ============================================================================
// 15. Empty input edge case
// ============================================================================

#[test]
fn empty_input_encode_decode() {
    for cp in ALL_CODEPAGES {
        let enc = utf8_to_ebcdic("", cp).unwrap();
        assert!(enc.is_empty(), "{cp}: encode empty → empty");
        let dec = ebcdic_to_utf8(&[], cp, UnmappablePolicy::Error).unwrap();
        assert!(dec.is_empty(), "{cp}: decode empty → empty");
    }
}
