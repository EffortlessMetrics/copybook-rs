// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive charset-level string conversion tests.
//!
//! Focuses on multi-character string operations, cross-codepage behavioral
//! differences, COBOL-realistic field patterns, and policy semantics —
//! complementing the single-byte exhaustive tests in `codepage_exhaustive.rs`.
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, utf8_to_ebcdic};
use copybook_error::ErrorCode;

/// All five EBCDIC codepages.
const ALL_EBCDIC: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

// ============================================================================
// 1. String concatenation consistency
// ============================================================================

#[test]
fn encode_concatenation_equals_concat_of_encodings() {
    // Encoding "AB" must produce the same bytes as encoding "A" ++ encoding "B".
    let parts = ["HELLO", " ", "WORLD", " ", "123"];
    for cp in ALL_EBCDIC {
        let full: String = parts.join("");
        let full_encoded = utf8_to_ebcdic(&full, cp).unwrap();

        let mut piecewise = Vec::new();
        for part in &parts {
            piecewise.extend(utf8_to_ebcdic(part, cp).unwrap());
        }
        assert_eq!(
            full_encoded, piecewise,
            "{cp}: concatenation consistency violated"
        );
    }
}

// ============================================================================
// 2. Cross-codepage decode of identical EBCDIC bytes
// ============================================================================

#[test]
fn same_bytes_decode_differently_across_codepages() {
    // Byte 0x4A: CP037 → '¢' (U+00A2), CP273 → 'Ä' (U+00C4), CP500 → '[' (U+005B)
    let byte = [0x4A];
    let cp037 = ebcdic_to_utf8(&byte, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    let cp273 = ebcdic_to_utf8(&byte, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    let cp500 = ebcdic_to_utf8(&byte, Codepage::CP500, UnmappablePolicy::Error).unwrap();

    assert_eq!(cp037, "¢", "CP037 0x4A");
    assert_eq!(cp273, "Ä", "CP273 0x4A");
    assert_eq!(cp500, "[", "CP500 0x4A");
    assert_ne!(cp037, cp273);
    assert_ne!(cp037, cp500);
    assert_ne!(cp273, cp500);
}

#[test]
fn bracket_positions_differ_between_codepages() {
    // '[' and ']' encode to different EBCDIC bytes across codepages.
    let brackets = "[]";
    let cp037_enc = utf8_to_ebcdic(brackets, Codepage::CP037).unwrap();
    let cp273_enc = utf8_to_ebcdic(brackets, Codepage::CP273).unwrap();
    let cp500_enc = utf8_to_ebcdic(brackets, Codepage::CP500).unwrap();
    let cp1047_enc = utf8_to_ebcdic(brackets, Codepage::CP1047).unwrap();

    // All differ from each other in at least one byte position.
    let all = [&cp037_enc, &cp273_enc, &cp500_enc, &cp1047_enc];
    for (i, a) in all.iter().enumerate() {
        for b in all.iter().skip(i + 1) {
            assert_ne!(a, b, "bracket encoding should differ between codepages");
        }
    }

    // But round-trip must still work for each.
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(brackets, cp).unwrap();
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, brackets, "{cp}: bracket round-trip failed");
    }
}

#[test]
fn at_sign_position_differs_between_cp037_and_cp273() {
    // '@': CP037 → 0x7C, CP273 → 0xB5
    let cp037 = utf8_to_ebcdic("@", Codepage::CP037).unwrap();
    let cp273 = utf8_to_ebcdic("@", Codepage::CP273).unwrap();
    assert_eq!(cp037, vec![0x7C], "CP037 @ position");
    assert_eq!(cp273, vec![0xB5], "CP273 @ position");
    assert_ne!(cp037, cp273);
}

// ============================================================================
// 3. Encode then decode with wrong codepage
// ============================================================================

#[test]
fn wrong_codepage_decode_produces_different_text() {
    // Letters A-Z/a-z and digits share positions across codepages, but
    // punctuation and special chars differ. Use brackets to show divergence.
    let original_with_brackets = "A[B]C";
    let encoded_brackets = utf8_to_ebcdic(original_with_brackets, Codepage::CP037).unwrap();
    let decoded_wrong = ebcdic_to_utf8(
        &encoded_brackets,
        Codepage::CP273,
        UnmappablePolicy::Replace,
    )
    .unwrap();
    assert_ne!(
        decoded_wrong, original_with_brackets,
        "Decoding CP037 bytes as CP273 should produce different text for bracket chars"
    );
}

// ============================================================================
// 4. COBOL-realistic field patterns
// ============================================================================

#[test]
fn cobol_numeric_display_field_leading_zeros() {
    // COBOL PIC 9(8) DISPLAY: "00012345"
    let field = "00012345";
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(field, cp).unwrap();
        assert_eq!(enc.len(), 8, "{cp}: numeric field length");
        // All bytes should be in F0-F9 range
        for (i, &b) in enc.iter().enumerate() {
            assert!(
                (0xF0..=0xF9).contains(&b),
                "{cp}: byte {i} = 0x{b:02X} not in digit range"
            );
        }
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, field, "{cp}: numeric field round-trip");
    }
}

#[test]
fn cobol_alphanumeric_field_trailing_spaces() {
    // COBOL PIC X(20): "JOHN DOE            " (padded with spaces)
    let field = "JOHN DOE            ";
    assert_eq!(field.len(), 20);
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(field, cp).unwrap();
        assert_eq!(enc.len(), 20, "{cp}: field length");
        // Last 12 bytes should be 0x40 (EBCDIC space)
        for &b in &enc[8..] {
            assert_eq!(b, 0x40, "{cp}: trailing padding should be EBCDIC space");
        }
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, field, "{cp}: padded field round-trip");
    }
}

#[test]
fn cobol_mixed_content_record() {
    // Typical COBOL record: name(20) + amount(10) + date(8) + filler(2)
    let record = "ACME CORP           0000123456 20230915  ";
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(record, cp).unwrap();
        assert_eq!(enc.len(), record.len(), "{cp}: record length");
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, record, "{cp}: mixed record round-trip");
    }
}

// ============================================================================
// 5. Currency symbols across codepages
// ============================================================================

#[test]
fn cent_sign_available_on_cp037_cp1047_cp1140() {
    // ¢ (U+00A2) is at 0x4A on CP037, CP1047, CP1140
    for cp in [Codepage::CP037, Codepage::CP1047, Codepage::CP1140] {
        let enc = utf8_to_ebcdic("¢", cp).unwrap();
        assert_eq!(enc, vec![0x4A], "{cp}: ¢ should be at 0x4A");
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, "¢", "{cp}: ¢ round-trip");
    }
}

#[test]
fn pound_sign_roundtrip_all_codepages() {
    // £ (U+00A3) exists on all codepages but at different positions.
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic("£", cp).unwrap();
        assert_eq!(enc.len(), 1, "{cp}: £ should be single byte");
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, "£", "{cp}: £ round-trip");
    }
}

#[test]
fn euro_sign_only_available_on_cp1140() {
    // € (U+20AC) — CP1140 has it at 0xFF, other codepages do not have it.
    assert!(utf8_to_ebcdic("€", Codepage::CP1140).is_ok());
    for cp in [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
    ] {
        assert!(
            utf8_to_ebcdic("€", cp).is_err(),
            "{cp}: € should not be encodable"
        );
    }
}

#[test]
fn currency_sign_general_roundtrip_all_codepages() {
    // ¤ (U+00A4 — generic currency sign) exists on all codepages.
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic("¤", cp).unwrap();
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, "¤", "{cp}: ¤ round-trip");
    }
}

// ============================================================================
// 6. Latin-1 extended characters as strings
// ============================================================================

#[test]
fn latin1_extended_chars_string_roundtrip() {
    // Several Latin-1 chars that exist in EBCDIC: £, ¥, §, ©, ®, ±, µ
    let chars = "£¥§©®±µ";
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(chars, cp)
            .unwrap_or_else(|e| panic!("{cp}: encode Latin-1 string failed: {e}"));
        assert_eq!(
            enc.len(),
            chars.chars().count(),
            "{cp}: each Latin-1 char maps to one EBCDIC byte"
        );
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("{cp}: decode Latin-1 string failed: {e}"));
        assert_eq!(dec, chars, "{cp}: Latin-1 string round-trip");
    }
}

// ============================================================================
// 7. UTF-8 multi-byte characters that are single EBCDIC bytes
// ============================================================================

#[test]
fn utf8_multibyte_to_single_ebcdic_byte() {
    // Characters like 'é' (U+00E9, 2 bytes in UTF-8) map to single EBCDIC bytes.
    let accented = "éèêë";
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(accented, cp)
            .unwrap_or_else(|e| panic!("{cp}: encode accented failed: {e}"));
        assert_eq!(enc.len(), 4, "{cp}: 4 accented chars -> 4 EBCDIC bytes");
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, accented, "{cp}: accented round-trip");
    }
}

#[test]
fn euro_sign_is_3_utf8_bytes_but_1_ebcdic_byte() {
    // € is U+20AC = 3 bytes in UTF-8, but 1 byte (0xFF) in CP1140.
    let enc = utf8_to_ebcdic("€", Codepage::CP1140).unwrap();
    assert_eq!(enc.len(), 1, "€ should be single EBCDIC byte");
    assert_eq!("€".len(), 3, "€ is 3 bytes in UTF-8");
}

// ============================================================================
// 8. All-zeros and all-0xFF buffers
// ============================================================================

#[test]
fn all_zeros_buffer_all_policies() {
    let zeros = vec![0x00u8; 16];
    for cp in ALL_EBCDIC {
        // Error: should fail on first byte
        let err = ebcdic_to_utf8(&zeros, cp, UnmappablePolicy::Error);
        assert!(err.is_err(), "{cp}: all-zeros should fail with Error");
        assert_eq!(
            err.unwrap_err().code,
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
        );

        // Replace: 16 replacement chars
        let replaced = ebcdic_to_utf8(&zeros, cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            replaced.chars().count(),
            16,
            "{cp}: all-zeros Replace should produce 16 chars"
        );
        assert!(
            replaced.chars().all(|c| c == '\u{FFFD}'),
            "{cp}: all-zeros Replace should be all U+FFFD"
        );

        // Skip: empty
        let skipped = ebcdic_to_utf8(&zeros, cp, UnmappablePolicy::Skip).unwrap();
        assert!(skipped.is_empty(), "{cp}: all-zeros Skip should be empty");
    }
}

#[test]
fn all_0xff_buffer_decode() {
    let ff_buf = vec![0xFFu8; 8];
    for cp in ALL_EBCDIC {
        // 0xFF always maps to something (€ on CP1140, control on others).
        // Replace should never fail.
        let result = ebcdic_to_utf8(&ff_buf, cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            result.chars().count(),
            8,
            "{cp}: all-0xFF should decode to 8 chars"
        );
    }
    // CP1140: all-0xFF should be 8 euro signs.
    let cp1140_result = ebcdic_to_utf8(&ff_buf, Codepage::CP1140, UnmappablePolicy::Error).unwrap();
    assert_eq!(cp1140_result, "€€€€€€€€");
}

// ============================================================================
// 9. Entirely unmappable input strings
// ============================================================================

#[test]
fn all_control_bytes_skip_produces_empty() {
    // Bytes 0x00-0x04 map to C0 control chars (< 0x20) on all codepages.
    let controls = vec![0x00, 0x01, 0x02, 0x03];
    for cp in ALL_EBCDIC {
        let skipped = ebcdic_to_utf8(&controls, cp, UnmappablePolicy::Skip).unwrap();
        assert!(
            skipped.is_empty(),
            "{cp}: all-control Skip should be empty, got {skipped:?}"
        );
    }
}

#[test]
fn all_control_bytes_replace_all_become_fffd() {
    let controls = vec![0x00, 0x01, 0x02, 0x03];
    for cp in ALL_EBCDIC {
        let replaced = ebcdic_to_utf8(&controls, cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            replaced.chars().count(),
            4,
            "{cp}: 4 control bytes -> 4 replacement chars"
        );
        assert!(
            replaced.chars().all(|c| c == '\u{FFFD}'),
            "{cp}: all should be U+FFFD"
        );
    }
}

// ============================================================================
// 10. Interleaved unmappable/mappable byte patterns
// ============================================================================

#[test]
fn alternating_valid_invalid_bytes_replace() {
    // Pattern: space, NUL, digit-0, NUL, letter-A, NUL
    let data = vec![0x40, 0x00, 0xF0, 0x00, 0xC1, 0x00];
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            result, " \u{FFFD}0\u{FFFD}A\u{FFFD}",
            "{cp}: alternating valid/invalid Replace"
        );
    }
}

#[test]
fn alternating_valid_invalid_bytes_skip() {
    let data = vec![0x40, 0x00, 0xF0, 0x00, 0xC1, 0x00];
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Skip).unwrap();
        assert_eq!(result, " 0A", "{cp}: alternating valid/invalid Skip");
    }
}

#[test]
fn alternating_valid_invalid_bytes_error_stops_at_second() {
    // First byte valid (space 0x40), second byte invalid (0x00).
    let data = vec![0x40, 0x00, 0xF0];
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Error);
        assert!(result.is_err(), "{cp}: Error should fail on second byte");
    }
}

// ============================================================================
// 11. Determinism — repeated calls produce identical output
// ============================================================================

#[test]
fn encode_decode_deterministic_across_calls() {
    let input = "DETERMINISM TEST 2024-01-15 $1,234.56";
    for cp in ALL_EBCDIC {
        let enc1 = utf8_to_ebcdic(input, cp).unwrap();
        let enc2 = utf8_to_ebcdic(input, cp).unwrap();
        assert_eq!(enc1, enc2, "{cp}: encode not deterministic");

        let dec1 = ebcdic_to_utf8(&enc1, cp, UnmappablePolicy::Error).unwrap();
        let dec2 = ebcdic_to_utf8(&enc2, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec1, dec2, "{cp}: decode not deterministic");
        assert_eq!(dec1, input, "{cp}: round-trip fidelity");
    }
}

// ============================================================================
// 12. Boundary-length strings
// ============================================================================

#[test]
fn boundary_length_strings_roundtrip() {
    // Typical COBOL-relevant lengths: 1, 2, 80 (card width), 132 (line printer),
    // 255, 256, 1000.
    let lengths = [1, 2, 80, 132, 255, 256, 1000];
    for cp in ALL_EBCDIC {
        for &len in &lengths {
            let input: String = "ABCDEFGHIJ".chars().cycle().take(len).collect();
            let enc = utf8_to_ebcdic(&input, cp)
                .unwrap_or_else(|e| panic!("{cp}: encode len={len} failed: {e}"));
            assert_eq!(enc.len(), len, "{cp}: encoded length for {len}");
            let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp}: decode len={len} failed: {e}"));
            assert_eq!(dec, input, "{cp}: round-trip len={len}");
        }
    }
}

// ============================================================================
// 13. Repeated single-character strings
// ============================================================================

#[test]
fn repeated_single_char_encodes_to_repeated_byte() {
    for cp in ALL_EBCDIC {
        for ch in ['A', '0', ' ', 'Z'] {
            let input: String = std::iter::repeat_n(ch, 50).collect();
            let enc = utf8_to_ebcdic(&input, cp).unwrap();
            assert_eq!(enc.len(), 50, "{cp}: repeated '{ch}' length");
            // All bytes should be the same
            let expected_byte = enc[0];
            assert!(
                enc.iter().all(|&b| b == expected_byte),
                "{cp}: repeated '{ch}' should encode to uniform bytes"
            );
        }
    }
}

// ============================================================================
// 14. UTF-8 multi-byte unmappable characters in encode direction
// ============================================================================

#[test]
fn utf8_encode_fails_on_2byte_unmappable() {
    // ₿ (U+20BF, Bitcoin sign) — 3 bytes in UTF-8, not in any EBCDIC codepage.
    for cp in ALL_EBCDIC {
        let result = utf8_to_ebcdic("₿", cp);
        assert!(result.is_err(), "{cp}: ₿ should be unmappable");
        assert_eq!(
            result.unwrap_err().code,
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
        );
    }
}

#[test]
fn utf8_encode_fails_on_4byte_emoji() {
    // 🏢 (U+1F3E2, office building) — 4 bytes in UTF-8.
    for cp in ALL_EBCDIC {
        let result = utf8_to_ebcdic("🏢", cp);
        assert!(result.is_err(), "{cp}: emoji should be unmappable");
    }
}

#[test]
fn utf8_encode_partial_failure_stops_at_unmappable() {
    // "Hello🏢World" — encode should fail at the emoji, not produce partial output.
    for cp in ALL_EBCDIC {
        let result = utf8_to_ebcdic("Hello🏢World", cp);
        assert!(result.is_err(), "{cp}: should fail on embedded emoji");
    }
}

// ============================================================================
// 15. CP273 German-specific string patterns
// ============================================================================

#[test]
fn cp273_german_umlauts_in_sentence() {
    let sentence = "Ä Ö Ü ä ö ü";
    let enc = utf8_to_ebcdic(sentence, Codepage::CP273).unwrap();
    let dec = ebcdic_to_utf8(&enc, Codepage::CP273, UnmappablePolicy::Error).unwrap();
    assert_eq!(dec, sentence, "CP273: German umlaut sentence round-trip");
}

#[test]
fn cp273_eszett_roundtrip() {
    // ß (U+00DF, sharp-s) — present on all codepages at 0x59.
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic("ß", cp).unwrap();
        assert_eq!(enc, vec![0x59], "{cp}: ß should be at 0x59");
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, "ß", "{cp}: ß round-trip");
    }
}

// ============================================================================
// 16. CP500 International Latin-1 differences
// ============================================================================

#[test]
fn cp500_exclamation_mark_position_differs() {
    // '!' is at different positions: CP037 → 0x5A, CP500 → 0x4F
    let cp037 = utf8_to_ebcdic("!", Codepage::CP037).unwrap();
    let cp500 = utf8_to_ebcdic("!", Codepage::CP500).unwrap();
    assert_eq!(cp037, vec![0x5A], "CP037: ! at 0x5A");
    assert_eq!(cp500, vec![0x4F], "CP500: ! at 0x4F");
    assert_ne!(cp037, cp500);
}

// ============================================================================
// 17. CP1047 Open Systems differences
// ============================================================================

#[test]
fn cp1047_square_bracket_positions() {
    // '[' and ']' have unique positions on CP1047 vs CP037.
    let cp037_open = utf8_to_ebcdic("[", Codepage::CP037).unwrap();
    let cp1047_open = utf8_to_ebcdic("[", Codepage::CP1047).unwrap();
    assert_eq!(cp037_open, vec![0xBA], "CP037: [ at 0xBA");
    assert_eq!(cp1047_open, vec![0xAD], "CP1047: [ at 0xAD");
    assert_ne!(cp037_open, cp1047_open);
}

// ============================================================================
// 18. Whitespace patterns
// ============================================================================

#[test]
fn leading_trailing_embedded_spaces_roundtrip() {
    let patterns = [
        "   LEADING",
        "TRAILING   ",
        "   BOTH   ",
        "A B C D E",
        "  ",
        "A  B",
    ];
    for cp in ALL_EBCDIC {
        for &pat in &patterns {
            let enc = utf8_to_ebcdic(pat, cp).unwrap();
            let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
            assert_eq!(dec, pat, "{cp}: whitespace pattern '{pat}' round-trip");
        }
    }
}

#[test]
fn nbsp_0x41_roundtrip_all_codepages() {
    // EBCDIC 0x41 is NBSP (U+00A0) on CP037, CP500, CP1047, CP1140.
    // On CP273 0x41 is also NBSP.
    for cp in ALL_EBCDIC {
        let dec = ebcdic_to_utf8(&[0x41], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, "\u{00A0}", "{cp}: 0x41 should be NBSP");
    }
}

// ============================================================================
// 19. Policy consistency across multi-byte strings
// ============================================================================

#[test]
fn replace_policy_count_matches_unmappable_count() {
    // Build a buffer with known unmappable count: 3 NUL + 2 valid.
    let data = vec![0x00, 0x00, 0x40, 0x00, 0xF0]; // NUL, NUL, space, NUL, '0'
    for cp in ALL_EBCDIC {
        let replaced = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Replace).unwrap();
        let fffd_count = replaced.chars().filter(|&c| c == '\u{FFFD}').count();
        assert_eq!(fffd_count, 3, "{cp}: expected 3 replacement chars");
        assert_eq!(
            replaced.chars().count(),
            5,
            "{cp}: total char count should be 5"
        );
    }
}

#[test]
fn skip_policy_length_reflects_mappable_count() {
    let data = vec![0x00, 0x00, 0x40, 0x00, 0xF0]; // 3 unmappable + 2 mappable
    for cp in ALL_EBCDIC {
        let skipped = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Skip).unwrap();
        assert_eq!(
            skipped.chars().count(),
            2,
            "{cp}: Skip should retain only 2 mappable chars"
        );
        assert_eq!(skipped, " 0", "{cp}: Skip output");
    }
}

// ============================================================================
// 20. CP1140 vs CP037 — the only difference at 0xFF
// ============================================================================

#[test]
fn cp1140_cp037_identical_for_printable_ascii_strings() {
    // For pure printable ASCII, both codepages should produce identical EBCDIC bytes.
    let test_strings = [
        "HELLO WORLD",
        "0123456789",
        "abcdefghijklmnopqrstuvwxyz",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "Test String 123 !@#$%",
    ];
    for s in &test_strings {
        let cp037_enc = utf8_to_ebcdic(s, Codepage::CP037).unwrap();
        let cp1140_enc = utf8_to_ebcdic(s, Codepage::CP1140).unwrap();
        assert_eq!(
            cp037_enc, cp1140_enc,
            "CP037 and CP1140 should produce identical bytes for '{s}'"
        );
    }
}

#[test]
fn cp1140_euro_in_mixed_string() {
    // A string with € embedded among ASCII chars — only works on CP1140.
    let input = "Total: €1,234.56";
    let enc = utf8_to_ebcdic(input, Codepage::CP1140).unwrap();
    let dec = ebcdic_to_utf8(&enc, Codepage::CP1140, UnmappablePolicy::Error).unwrap();
    assert_eq!(dec, input, "CP1140: euro in mixed string round-trip");

    // Same string should fail on CP037.
    let result = utf8_to_ebcdic(input, Codepage::CP037);
    assert!(result.is_err(), "CP037 should reject € in string");
}

// ============================================================================
// 21. Single-byte edge case
// ============================================================================

#[test]
fn single_byte_0x40_space_all_policies() {
    for cp in ALL_EBCDIC {
        for policy in [
            UnmappablePolicy::Error,
            UnmappablePolicy::Replace,
            UnmappablePolicy::Skip,
        ] {
            let dec = ebcdic_to_utf8(&[0x40], cp, policy).unwrap();
            assert_eq!(dec, " ", "{cp}/{policy}: 0x40 should always be space");
        }
    }
}

// ============================================================================
// 22. Error message quality
// ============================================================================

#[test]
fn unmappable_utf8_error_contains_character_info() {
    let err = utf8_to_ebcdic("Hello 🌍 World", Codepage::CP037).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
    // Error message should reference the unmappable character.
    let msg = err.to_string();
    assert!(
        msg.contains("🌍") || msg.contains("U+") || msg.contains("cannot be mapped"),
        "Error message should identify the unmappable char: {msg}"
    );
}

#[test]
fn unmappable_ebcdic_error_contains_byte_info() {
    let err = ebcdic_to_utf8(&[0x00], Codepage::CP037, UnmappablePolicy::Error).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
    let msg = err.to_string();
    assert!(
        msg.contains("0x00") || msg.contains("0x0") || msg.contains("U+"),
        "Error message should identify the unmappable byte: {msg}"
    );
}

// ============================================================================
// 23. ASCII passthrough mode with string operations
// ============================================================================

#[test]
fn ascii_passthrough_preserves_all_bytes() {
    let input = "Hello, World! 123 @#$%^&*()";
    let enc = utf8_to_ebcdic(input, Codepage::ASCII).unwrap();
    assert_eq!(enc, input.as_bytes(), "ASCII passthrough encode");
    let dec = ebcdic_to_utf8(&enc, Codepage::ASCII, UnmappablePolicy::Error).unwrap();
    assert_eq!(dec, input, "ASCII passthrough decode");
}

#[test]
fn ascii_passthrough_non_utf8_uses_lossy() {
    // Non-UTF8 bytes in ASCII mode get replaced via from_utf8_lossy.
    let data: &[u8] = &[0x48, 0x65, 0x6C, 0xFF, 0x6F]; // "Hel\xFFo"
    let dec = ebcdic_to_utf8(data, Codepage::ASCII, UnmappablePolicy::Error).unwrap();
    assert!(
        dec.contains('\u{FFFD}'),
        "Non-UTF8 byte should become U+FFFD"
    );
    assert!(dec.starts_with("Hel"), "Valid prefix preserved");
    assert!(dec.ends_with('o'), "Valid suffix preserved");
}

// ============================================================================
// 24. Display format and UnmappablePolicy Display
// ============================================================================

#[test]
fn unmappable_policy_display_format() {
    // Verify the policies have a Display impl (used in error messages).
    let policies = [
        UnmappablePolicy::Error,
        UnmappablePolicy::Replace,
        UnmappablePolicy::Skip,
    ];
    for policy in &policies {
        let display = format!("{policy}");
        assert!(!display.is_empty(), "Policy should have non-empty Display");
    }
}

// ============================================================================
// 25. Large mixed-content buffer
// ============================================================================

#[test]
fn large_mixed_content_roundtrip() {
    // Build a large realistic COBOL-like record buffer.
    let mut record = String::new();
    for i in 0..100 {
        record.push_str(&format!("{i:05} CUSTOMER NAME {i:03}  "));
    }
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(&record, cp)
            .unwrap_or_else(|e| panic!("{cp}: large mixed encode failed: {e}"));
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error)
            .unwrap_or_else(|e| panic!("{cp}: large mixed decode failed: {e}"));
        assert_eq!(dec, record, "{cp}: large mixed round-trip");
    }
}
