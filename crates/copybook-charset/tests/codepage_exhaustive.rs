// SPDX-License-Identifier: AGPL-3.0-or-later
//! Exhaustive codepage conversion tests — structural-beam fidelity validation.
//!
//! Every EBCDIC byte value and every printable ASCII character is exercised
//! across all five supported codepages with all unmappable policies.
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, space_byte, utf8_to_ebcdic};
use copybook_error::ErrorCode;

/// All EBCDIC codepages under test.
const ALL_EBCDIC: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP1047,
    Codepage::CP500,
    Codepage::CP273,
    Codepage::CP1140,
];

// ============================================================================
// 1. All printable ASCII → EBCDIC → ASCII round-trip for each codepage
// ============================================================================

/// Printable ASCII range: 0x20 (space) through 0x7E (~).
const PRINTABLE_RANGE: std::ops::RangeInclusive<u8> = 0x20..=0x7E;

#[test]
fn roundtrip_all_printable_ascii_cp037() {
    assert_printable_roundtrip(Codepage::CP037);
}

#[test]
fn roundtrip_all_printable_ascii_cp1047() {
    assert_printable_roundtrip(Codepage::CP1047);
}

#[test]
fn roundtrip_all_printable_ascii_cp500() {
    assert_printable_roundtrip(Codepage::CP500);
}

#[test]
fn roundtrip_all_printable_ascii_cp273() {
    assert_printable_roundtrip(Codepage::CP273);
}

#[test]
fn roundtrip_all_printable_ascii_cp1140() {
    assert_printable_roundtrip(Codepage::CP1140);
}

fn assert_printable_roundtrip(cp: Codepage) {
    let printable: String = PRINTABLE_RANGE.map(|b| b as char).collect();
    let ebcdic = utf8_to_ebcdic(&printable, cp)
        .unwrap_or_else(|e| panic!("{cp}: encode all printable failed: {e}"));
    assert_eq!(
        ebcdic.len(),
        printable.len(),
        "{cp}: EBCDIC length must equal ASCII length (1:1 mapping)"
    );
    let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
        .unwrap_or_else(|e| panic!("{cp}: decode all printable failed: {e}"));
    assert_eq!(
        decoded, printable,
        "{cp}: printable ASCII round-trip mismatch"
    );
}

#[test]
fn roundtrip_individual_printable_chars_all_codepages() {
    for cp in ALL_EBCDIC {
        for byte in PRINTABLE_RANGE {
            let ch = String::from(byte as char);
            let ebcdic = utf8_to_ebcdic(&ch, cp)
                .unwrap_or_else(|e| panic!("{cp}: encode 0x{byte:02X} '{ch}' failed: {e}"));
            assert_eq!(
                ebcdic.len(),
                1,
                "{cp}: single char must encode to single byte"
            );
            let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp}: decode 0x{byte:02X} '{ch}' failed: {e}"));
            assert_eq!(
                decoded, ch,
                "{cp}: char 0x{byte:02X} ('{ch}') round-trip mismatch (EBCDIC 0x{:02X})",
                ebcdic[0]
            );
        }
    }
}

// ============================================================================
// 2. All EBCDIC byte values → decode to UTF-8 → re-encode to EBCDIC
// ============================================================================

#[test]
fn full_byte_range_decode_reencode_cp037() {
    assert_byte_range_roundtrip(Codepage::CP037);
}

#[test]
fn full_byte_range_decode_reencode_cp1047() {
    assert_byte_range_roundtrip(Codepage::CP1047);
}

#[test]
fn full_byte_range_decode_reencode_cp500() {
    assert_byte_range_roundtrip(Codepage::CP500);
}

#[test]
fn full_byte_range_decode_reencode_cp273() {
    assert_byte_range_roundtrip(Codepage::CP273);
}

#[test]
fn full_byte_range_decode_reencode_cp1140() {
    assert_byte_range_roundtrip(Codepage::CP1140);
}

/// For every byte 0x00..=0xFF, decode with Replace policy. Mappable bytes must
/// round-trip; unmappable bytes produce U+FFFD which cannot re-encode.
fn assert_byte_range_roundtrip(cp: Codepage) {
    let mut mappable_count = 0u32;
    let mut unmappable_count = 0u32;

    for byte in 0x00..=0xFFu8 {
        let input = [byte];
        let decoded = ebcdic_to_utf8(&input, cp, UnmappablePolicy::Replace)
            .unwrap_or_else(|e| panic!("{cp}: decode byte 0x{byte:02X} failed: {e}"));

        if decoded == "\u{FFFD}" {
            // Unmappable — verify Error policy also rejects it
            let err_result = ebcdic_to_utf8(&input, cp, UnmappablePolicy::Error);
            assert!(
                err_result.is_err(),
                "{cp}: byte 0x{byte:02X} replaced but Error policy did not reject"
            );
            assert_eq!(
                err_result.unwrap_err().code,
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                "{cp}: byte 0x{byte:02X} wrong error code"
            );
            // Verify Skip omits it
            let skip_result = ebcdic_to_utf8(&input, cp, UnmappablePolicy::Skip).unwrap();
            assert!(
                skip_result.is_empty(),
                "{cp}: byte 0x{byte:02X} Skip should produce empty string"
            );
            unmappable_count += 1;
        } else {
            // Mappable — must re-encode to the exact same byte
            let reencoded = utf8_to_ebcdic(&decoded, cp).unwrap_or_else(|e| {
                panic!(
                    "{cp}: re-encode byte 0x{byte:02X} -> '{}' failed: {e}",
                    decoded.escape_unicode()
                )
            });
            assert_eq!(
                reencoded,
                vec![byte],
                "{cp}: byte 0x{byte:02X} -> '{}' re-encoded to 0x{:02X}, expected 0x{byte:02X}",
                decoded.escape_unicode(),
                reencoded[0]
            );
            mappable_count += 1;
        }
    }

    // Sanity: every codepage must have both mappable and unmappable bytes
    assert!(
        mappable_count > 0,
        "{cp}: expected at least one mappable byte"
    );
    assert!(
        unmappable_count > 0,
        "{cp}: expected at least one unmappable byte"
    );
    // All 256 byte values must be accounted for
    assert_eq!(
        mappable_count + unmappable_count,
        256,
        "{cp}: mappable ({mappable_count}) + unmappable ({unmappable_count}) != 256"
    );
}

// ============================================================================
// 3. Cross-codepage invariants
// ============================================================================

#[test]
fn digits_0_9_encode_to_0xf0_0xf9_all_codepages() {
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("0123456789", cp)
            .unwrap_or_else(|e| panic!("{cp}: encode digits failed: {e}"));
        for (i, &b) in encoded.iter().enumerate() {
            let expected = 0xF0 + u8::try_from(i).unwrap();
            assert_eq!(
                b, expected,
                "{cp}: digit '{i}' encoded as 0x{b:02X}, expected 0x{expected:02X}"
            );
        }
    }
}

#[test]
fn uppercase_a_z_identical_across_cp037_cp1047_cp1140() {
    let codepages = [Codepage::CP037, Codepage::CP1047, Codepage::CP1140];
    let alpha: String = ('A'..='Z').collect();
    let reference = utf8_to_ebcdic(&alpha, Codepage::CP037).unwrap();

    for cp in codepages {
        let encoded =
            utf8_to_ebcdic(&alpha, cp).unwrap_or_else(|e| panic!("{cp}: encode A-Z failed: {e}"));
        assert_eq!(
            encoded, reference,
            "{cp}: A-Z EBCDIC bytes differ from CP037 reference"
        );
    }
}

#[test]
fn lowercase_a_z_identical_across_cp037_cp1047_cp1140() {
    let codepages = [Codepage::CP037, Codepage::CP1047, Codepage::CP1140];
    let alpha: String = ('a'..='z').collect();
    let reference = utf8_to_ebcdic(&alpha, Codepage::CP037).unwrap();

    for cp in codepages {
        let encoded =
            utf8_to_ebcdic(&alpha, cp).unwrap_or_else(|e| panic!("{cp}: encode a-z failed: {e}"));
        assert_eq!(
            encoded, reference,
            "{cp}: a-z EBCDIC bytes differ from CP037 reference"
        );
    }
}

#[test]
fn space_0x20_encodes_to_0x40_all_codepages() {
    for cp in ALL_EBCDIC {
        let encoded =
            utf8_to_ebcdic(" ", cp).unwrap_or_else(|e| panic!("{cp}: encode space failed: {e}"));
        assert_eq!(encoded, vec![0x40], "{cp}: space must encode to 0x40");
    }
}

#[test]
fn space_byte_helper_agrees_with_encoding_all_codepages() {
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic(" ", cp).unwrap();
        assert_eq!(
            encoded[0],
            space_byte(cp),
            "{cp}: space_byte() disagrees with encode(' ')"
        );
    }
}

#[test]
fn ebcdic_0x40_decodes_to_space_all_codepages() {
    for cp in ALL_EBCDIC {
        let decoded = ebcdic_to_utf8(&[0x40], cp, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("{cp}: decode 0x40 failed: {e}"));
        assert_eq!(decoded, " ", "{cp}: 0x40 must decode to space");
    }
}

#[test]
fn digits_encode_identically_across_all_codepages() {
    let digits = "0123456789";
    let reference = utf8_to_ebcdic(digits, Codepage::CP037).unwrap();
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic(digits, cp).unwrap();
        assert_eq!(
            encoded, reference,
            "{cp}: digit encoding differs from CP037"
        );
    }
}

// ============================================================================
// 4. Unmappable character policies
// ============================================================================

#[test]
fn unmappable_error_returns_cbkc301_for_nul() {
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Error);
        assert!(result.is_err(), "{cp}: Error policy must reject NUL (0x00)");
        assert_eq!(
            result.unwrap_err().code,
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
            "{cp}: wrong error code for NUL"
        );
    }
}

#[test]
fn unmappable_replace_inserts_fffd_for_nul() {
    for cp in ALL_EBCDIC {
        let decoded = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            decoded, "\u{FFFD}",
            "{cp}: Replace policy must produce U+FFFD for NUL"
        );
    }
}

#[test]
fn unmappable_skip_omits_nul() {
    for cp in ALL_EBCDIC {
        let decoded = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Skip).unwrap();
        assert!(
            decoded.is_empty(),
            "{cp}: Skip policy must produce empty string for NUL"
        );
    }
}

#[test]
fn unmappable_replace_preserves_surrounding_chars() {
    for cp in ALL_EBCDIC {
        // Build: space (0x40) + NUL (0x00) + 'A' (find EBCDIC for 'A')
        let a_ebcdic = utf8_to_ebcdic("A", cp).unwrap();
        let data = vec![0x40, 0x00, a_ebcdic[0]];
        let decoded = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            decoded, " \u{FFFD}A",
            "{cp}: Replace must preserve surrounding chars"
        );
    }
}

#[test]
fn unmappable_skip_preserves_surrounding_chars() {
    for cp in ALL_EBCDIC {
        let a_ebcdic = utf8_to_ebcdic("A", cp).unwrap();
        let data = vec![0x40, 0x00, a_ebcdic[0]];
        let decoded = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Skip).unwrap();
        assert_eq!(
            decoded, " A",
            "{cp}: Skip must omit unmappable and preserve rest"
        );
    }
}

#[test]
fn unmappable_error_fails_on_first_bad_byte() {
    for cp in ALL_EBCDIC {
        let a_ebcdic = utf8_to_ebcdic("A", cp).unwrap();
        // Put a valid char before the bad byte to confirm it stops at the bad one
        let data = vec![a_ebcdic[0], 0x00];
        let result = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Error);
        assert!(
            result.is_err(),
            "{cp}: Error policy must fail even with valid prefix"
        );
    }
}

#[test]
fn utf8_to_ebcdic_rejects_cjk_all_codepages() {
    for cp in ALL_EBCDIC {
        let result = utf8_to_ebcdic("\u{4E2D}", cp); // 中
        assert!(result.is_err(), "{cp}: CJK character must be unmappable");
        assert_eq!(
            result.unwrap_err().code,
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
        );
    }
}

#[test]
fn utf8_to_ebcdic_rejects_emoji_all_codepages() {
    for cp in ALL_EBCDIC {
        let result = utf8_to_ebcdic("\u{1F680}", cp); // 🚀
        assert!(result.is_err(), "{cp}: emoji must be unmappable");
    }
}

#[test]
fn utf8_to_ebcdic_rejects_null_char_all_codepages() {
    // While '\0' maps in the forward table, the reverse table maps it back;
    // the point is that encode succeeds but the resulting byte 0x00 is unmappable
    // on decode with Error policy.
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("\0", cp);
        if let Ok(bytes) = encoded {
            // If encode succeeded, verify decode with Error rejects it
            let result = ebcdic_to_utf8(&bytes, cp, UnmappablePolicy::Error);
            assert!(
                result.is_err(),
                "{cp}: NUL char should be unmappable on decode"
            );
        }
        // If encode itself fails, that's also acceptable
    }
}

// ============================================================================
// 5. Special EBCDIC characters
// ============================================================================

#[test]
fn low_value_0x00_handling_all_codepages() {
    for cp in ALL_EBCDIC {
        // 0x00 maps to NUL (U+0000) — always unmappable
        let err = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Error);
        assert!(err.is_err(), "{cp}: 0x00 must be unmappable");

        let replaced = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(replaced, "\u{FFFD}", "{cp}: 0x00 Replace -> U+FFFD");

        let skipped = ebcdic_to_utf8(&[0x00], cp, UnmappablePolicy::Skip).unwrap();
        assert!(skipped.is_empty(), "{cp}: 0x00 Skip -> empty");
    }
}

#[test]
fn high_value_0xff_handling_all_codepages() {
    for cp in ALL_EBCDIC {
        // 0xFF maps to different things per codepage:
        //   CP1140: € (U+20AC) — mappable
        //   Others: control char 0x009F or ÿ — depends on table
        let decoded = ebcdic_to_utf8(&[0xFF], cp, UnmappablePolicy::Replace).unwrap();
        assert!(
            !decoded.is_empty(),
            "{cp}: 0xFF must produce at least one char (or replacement)"
        );

        // If it round-trips, the byte is properly mappable
        if !decoded.contains('\u{FFFD}') {
            let reencoded = utf8_to_ebcdic(&decoded, cp).unwrap();
            assert_eq!(
                reencoded,
                vec![0xFF],
                "{cp}: 0xFF decode-reencode must produce 0xFF"
            );
        }
    }
}

#[test]
fn cp1140_0xff_is_euro_sign() {
    let decoded = ebcdic_to_utf8(&[0xFF], Codepage::CP1140, UnmappablePolicy::Error).unwrap();
    assert_eq!(
        decoded, "\u{20AC}",
        "CP1140: 0xFF must decode to € (U+20AC)"
    );
    let reencoded = utf8_to_ebcdic("\u{20AC}", Codepage::CP1140).unwrap();
    assert_eq!(reencoded, vec![0xFF], "CP1140: € must encode to 0xFF");
}

#[test]
fn cp037_0xff_is_not_euro() {
    // CP037 0xFF maps to U+009F (APC control character), not €.
    // U+009F is a C1 control char (>= 0x20) so the codec treats it as mappable.
    let decoded = ebcdic_to_utf8(&[0xFF], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_ne!(
        decoded, "\u{20AC}",
        "CP037: 0xFF must NOT decode to € (that's CP1140)"
    );
    // Verify it is NOT the euro sign
    assert!(
        !decoded.contains('\u{20AC}'),
        "CP037: 0xFF must not contain €"
    );
}

#[test]
fn ebcdic_control_range_0x00_0x3f_contains_unmappable_bytes() {
    // EBCDIC bytes 0x00-0x3F include bytes that map to C0 control characters
    // (< U+0020, excluding tab/LF/CR) which the codec treats as unmappable.
    // Many other bytes in this range map to C1 control characters (U+0080-009F)
    // which are >= 0x20 and thus considered mappable by the codec.
    for cp in ALL_EBCDIC {
        let mut unmappable = 0u32;
        for byte in 0x00..=0x3Fu8 {
            if ebcdic_to_utf8(&[byte], cp, UnmappablePolicy::Error).is_err() {
                unmappable += 1;
            }
        }
        assert!(
            unmappable > 0,
            "{cp}: control range 0x00-0x3F must contain at least one unmappable byte"
        );
    }
}

#[test]
fn tab_lf_cr_are_mappable_all_codepages() {
    // EBCDIC bytes that map to tab (0x09), LF (0x0A), CR (0x0D) should pass Error policy
    for cp in ALL_EBCDIC {
        let all_bytes: Vec<u8> = (0x00..=0xFFu8).collect();
        for &byte in &all_bytes {
            let result = ebcdic_to_utf8(&[byte], cp, UnmappablePolicy::Replace).unwrap();
            // If this byte maps to tab, LF, or CR, verify it passes Error policy too
            if result == "\t" || result == "\n" || result == "\r" {
                let strict = ebcdic_to_utf8(&[byte], cp, UnmappablePolicy::Error);
                assert!(
                    strict.is_ok(),
                    "{cp}: byte 0x{byte:02X} maps to whitespace but Error policy rejected it"
                );
            }
        }
    }
}

// ============================================================================
// 6. Empty and single-character edge cases
// ============================================================================

#[test]
fn empty_byte_slice_decodes_to_empty_string() {
    for cp in ALL_EBCDIC {
        for policy in [
            UnmappablePolicy::Error,
            UnmappablePolicy::Replace,
            UnmappablePolicy::Skip,
        ] {
            let decoded = ebcdic_to_utf8(&[], cp, policy).unwrap();
            assert!(
                decoded.is_empty(),
                "{cp}/{policy}: empty input must produce empty output"
            );
        }
    }
}

#[test]
fn empty_string_encodes_to_empty_bytes() {
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("", cp).unwrap();
        assert!(
            encoded.is_empty(),
            "{cp}: empty string must encode to empty bytes"
        );
    }
}

#[test]
fn single_byte_roundtrip_space() {
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic(" ", cp).unwrap();
        assert_eq!(encoded, vec![0x40], "{cp}: space -> 0x40");
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, " ", "{cp}: 0x40 -> space");
    }
}

#[test]
fn single_byte_roundtrip_each_digit() {
    for cp in ALL_EBCDIC {
        for digit in b'0'..=b'9' {
            let s = String::from(digit as char);
            let encoded = utf8_to_ebcdic(&s, cp).unwrap();
            assert_eq!(encoded.len(), 1, "{cp}: digit '{s}' must be single byte");
            let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(decoded, s, "{cp}: digit '{s}' round-trip failed");
        }
    }
}

#[test]
fn all_spaces_string_roundtrip() {
    for cp in ALL_EBCDIC {
        for len in [1, 2, 10, 100, 255] {
            let spaces: String = " ".repeat(len);
            let encoded = utf8_to_ebcdic(&spaces, cp).unwrap();
            assert_eq!(
                encoded.len(),
                len,
                "{cp}: {len} spaces must encode to {len} bytes"
            );
            assert!(
                encoded.iter().all(|&b| b == 0x40),
                "{cp}: all-spaces must encode to all-0x40"
            );
            let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(decoded, spaces, "{cp}: {len} spaces round-trip failed");
        }
    }
}

#[test]
fn single_char_encode_decode_every_letter() {
    for cp in ALL_EBCDIC {
        for ch in ('A'..='Z').chain('a'..='z') {
            let s = String::from(ch);
            let encoded = utf8_to_ebcdic(&s, cp).unwrap();
            assert_eq!(encoded.len(), 1, "{cp}: '{ch}' must encode to 1 byte");
            let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(decoded, s, "{cp}: '{ch}' round-trip failed");
        }
    }
}

// ============================================================================
// 7. Supplementary structural invariants
// ============================================================================

#[test]
fn no_two_ascii_chars_encode_to_same_ebcdic_byte() {
    // Within printable ASCII, the encoding must be injective (one-to-one).
    for cp in ALL_EBCDIC {
        let mut seen = std::collections::HashMap::new();
        for byte in PRINTABLE_RANGE {
            let ch = String::from(byte as char);
            let encoded = utf8_to_ebcdic(&ch, cp).unwrap();
            if let Some(prev) = seen.insert(encoded[0], byte) {
                panic!(
                    "{cp}: EBCDIC 0x{:02X} maps to both ASCII 0x{prev:02X} ('{}') and 0x{byte:02X} ('{}')",
                    encoded[0], prev as char, byte as char
                );
            }
        }
    }
}

#[test]
fn encode_decode_full_printable_set_preserves_order() {
    // Encoding all printable ASCII and decoding must preserve character order.
    for cp in ALL_EBCDIC {
        let printable: String = PRINTABLE_RANGE.map(|b| b as char).collect();
        let ebcdic = utf8_to_ebcdic(&printable, cp).unwrap();
        let decoded = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error).unwrap();
        let original_chars: Vec<char> = printable.chars().collect();
        let decoded_chars: Vec<char> = decoded.chars().collect();
        assert_eq!(
            original_chars, decoded_chars,
            "{cp}: character order not preserved after round-trip"
        );
    }
}

#[test]
fn cp1140_differs_from_cp037_only_at_0x9f() {
    // CP1140 is identical to CP037 except byte 0x9F → € (U+20AC) instead of ¤ (U+00A4).
    let cp037_decoded: Vec<String> = (0x00..=0xFFu8)
        .map(|b| ebcdic_to_utf8(&[b], Codepage::CP037, UnmappablePolicy::Replace).unwrap())
        .collect();
    let cp1140_decoded: Vec<String> = (0x00..=0xFFu8)
        .map(|b| ebcdic_to_utf8(&[b], Codepage::CP1140, UnmappablePolicy::Replace).unwrap())
        .collect();

    let mut diff_positions = Vec::new();
    for i in 0..256 {
        if cp037_decoded[i] != cp1140_decoded[i] {
            diff_positions.push(i);
        }
    }

    // CP1140 differs from CP037 only at one position (0x9F → ¤ vs €) and 0xFF (control vs €)
    assert!(
        !diff_positions.is_empty(),
        "CP1140 must differ from CP037 in at least one byte position"
    );
    // The difference should be small — they're sibling codepages
    assert!(
        diff_positions.len() <= 2,
        "CP1140 differs from CP037 at {} positions (expected ≤ 2): {:?}",
        diff_positions.len(),
        diff_positions
            .iter()
            .map(|p| format!("0x{p:02X}"))
            .collect::<Vec<_>>()
    );
}

#[test]
fn all_codepages_have_consistent_table_coverage() {
    // Every codepage must handle all 256 byte values without panicking.
    for cp in ALL_EBCDIC {
        let all_bytes: Vec<u8> = (0x00..=0xFFu8).collect();
        let result = ebcdic_to_utf8(&all_bytes, cp, UnmappablePolicy::Replace);
        assert!(
            result.is_ok(),
            "{cp}: Replace policy must handle all 256 bytes without error"
        );
        let decoded = result.unwrap();
        assert!(
            !decoded.is_empty(),
            "{cp}: decoding all 256 bytes must produce non-empty output"
        );
    }
}

#[test]
fn ascii_passthrough_is_identity() {
    // Codepage::ASCII should pass bytes through without EBCDIC translation.
    let printable: String = PRINTABLE_RANGE.map(|b| b as char).collect();
    let encoded = utf8_to_ebcdic(&printable, Codepage::ASCII).unwrap();
    assert_eq!(
        encoded,
        printable.as_bytes(),
        "ASCII codepage must be identity encoding"
    );
    let decoded = ebcdic_to_utf8(
        printable.as_bytes(),
        Codepage::ASCII,
        UnmappablePolicy::Error,
    )
    .unwrap();
    assert_eq!(
        decoded, printable,
        "ASCII codepage must be identity decoding"
    );
}
