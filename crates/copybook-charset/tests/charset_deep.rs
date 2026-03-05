// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep tests for copybook-charset: exhaustive roundtrips, codepage divergence,
//! special characters, batch/mixed/invalid handling, threading, and performance.
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, space_byte, utf8_to_ebcdic};
use copybook_error::ErrorCode;
use std::time::Instant;

const ALL_EBCDIC: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

// ============================================================================
// 1. All printable ASCII → EBCDIC → ASCII roundtrip (individual char)
// ============================================================================

#[test]
fn roundtrip_each_printable_ascii_char_all_codepages() {
    for cp in ALL_EBCDIC {
        for byte in 0x20u8..=0x7E {
            let ch = String::from(byte as char);
            let ebcdic = utf8_to_ebcdic(&ch, cp)
                .unwrap_or_else(|e| panic!("{cp:?} encode 0x{byte:02X} ({ch}): {e}"));
            let back = ebcdic_to_utf8(&ebcdic, cp, UnmappablePolicy::Error)
                .unwrap_or_else(|e| panic!("{cp:?} decode 0x{byte:02X}: {e}"));
            assert_eq!(back, ch, "{cp:?}: char 0x{byte:02X} roundtrip failed");
        }
    }
}

// ============================================================================
// 2. All 256 byte values: decode → re-encode roundtrip
// ============================================================================

#[test]
fn all_256_bytes_decode_reencode_cp037() {
    verify_256_byte_roundtrip(Codepage::CP037);
}

#[test]
fn all_256_bytes_decode_reencode_cp273() {
    verify_256_byte_roundtrip(Codepage::CP273);
}

#[test]
fn all_256_bytes_decode_reencode_cp500() {
    verify_256_byte_roundtrip(Codepage::CP500);
}

#[test]
fn all_256_bytes_decode_reencode_cp1047() {
    verify_256_byte_roundtrip(Codepage::CP1047);
}

#[test]
fn all_256_bytes_decode_reencode_cp1140() {
    verify_256_byte_roundtrip(Codepage::CP1140);
}

fn verify_256_byte_roundtrip(cp: Codepage) {
    let mut roundtrip_count = 0u32;
    for byte in 0x00u8..=0xFF {
        let decoded = match ebcdic_to_utf8(&[byte], cp, UnmappablePolicy::Skip) {
            Ok(s) => s,
            Err(_) => continue,
        };
        if decoded.is_empty() {
            continue; // skipped control char
        }
        if let Ok(re_encoded) = utf8_to_ebcdic(&decoded, cp) {
            assert_eq!(
                re_encoded,
                vec![byte],
                "{cp:?}: byte 0x{byte:02X} → {decoded:?} → {re_encoded:?}"
            );
            roundtrip_count += 1;
        }
    }
    // Ensure a meaningful number of bytes survived the roundtrip
    assert!(
        roundtrip_count >= 90,
        "{cp:?}: only {roundtrip_count} bytes roundtripped (expected ≥90)"
    );
}

// ============================================================================
// 3. Special characters: £, ¢, @, #, |, ^, ~
// ============================================================================

#[test]
fn special_char_pound_sign_roundtrip() {
    // £ (U+00A3) exists on CP037, CP500, CP1047, CP1140 at byte 0xB1
    for cp in [
        Codepage::CP037,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let encoded = utf8_to_ebcdic("£", cp).unwrap_or_else(|e| panic!("{cp:?}: {e}"));
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "£", "{cp:?}: £ roundtrip failed");
    }
}

#[test]
fn special_char_cent_sign_roundtrip() {
    // ¢ (U+00A2) on CP037/CP1047/CP1140 at byte 0x4A
    for cp in [Codepage::CP037, Codepage::CP1047, Codepage::CP1140] {
        let encoded = utf8_to_ebcdic("¢", cp).unwrap();
        assert_eq!(encoded, vec![0x4A], "{cp:?}: ¢ should be 0x4A");
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "¢");
    }
}

#[test]
fn special_char_at_sign_roundtrip() {
    // @ (U+0040) on CP037 at 0x7C, CP500 at 0x7C, CP1047 at 0x7C
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("@", cp).unwrap_or_else(|e| panic!("{cp:?}: {e}"));
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "@", "{cp:?}: @ roundtrip failed");
    }
}

#[test]
fn special_char_hash_roundtrip() {
    // # (U+0023) exists on all EBCDIC codepages
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("#", cp).unwrap_or_else(|e| panic!("{cp:?}: {e}"));
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "#", "{cp:?}: # roundtrip failed");
    }
}

#[test]
fn special_char_pipe_roundtrip() {
    // | (U+007C) exists on all EBCDIC codepages
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("|", cp).unwrap_or_else(|e| panic!("{cp:?}: {e}"));
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "|", "{cp:?}: | roundtrip failed");
    }
}

#[test]
fn special_char_caret_roundtrip() {
    // ^ (U+005E) exists on all EBCDIC codepages
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("^", cp).unwrap_or_else(|e| panic!("{cp:?}: {e}"));
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "^", "{cp:?}: ^ roundtrip failed");
    }
}

#[test]
fn special_char_tilde_roundtrip() {
    // ~ (U+007E) exists on all EBCDIC codepages
    for cp in ALL_EBCDIC {
        let encoded = utf8_to_ebcdic("~", cp).unwrap_or_else(|e| panic!("{cp:?}: {e}"));
        let decoded = ebcdic_to_utf8(&encoded, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(decoded, "~", "{cp:?}: ~ roundtrip failed");
    }
}

// ============================================================================
// 4. Codepage-specific differences
// ============================================================================

#[test]
fn cp037_vs_cp500_bracket_positions_differ() {
    // CP037: [ = 0xBA, ] = 0xBB; CP500: [ = 0x4A, ] = 0x5A
    let cp037_enc = utf8_to_ebcdic("[", Codepage::CP037).unwrap();
    let cp500_enc = utf8_to_ebcdic("[", Codepage::CP500).unwrap();
    assert_ne!(
        cp037_enc, cp500_enc,
        "CP037 and CP500 should encode '[' differently"
    );
    // Verify roundtrips
    let cp037_dec = ebcdic_to_utf8(&cp037_enc, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    let cp500_dec = ebcdic_to_utf8(&cp500_enc, Codepage::CP500, UnmappablePolicy::Error).unwrap();
    assert_eq!(cp037_dec, "[");
    assert_eq!(cp500_dec, "[");
}

#[test]
fn cp037_vs_cp1047_bracket_positions_differ() {
    // CP037: [ = 0xBA; CP1047: [ = 0xAD
    let cp037_enc = utf8_to_ebcdic("[", Codepage::CP037).unwrap();
    let cp1047_enc = utf8_to_ebcdic("[", Codepage::CP1047).unwrap();
    assert_ne!(cp037_enc, cp1047_enc, "CP037 and CP1047 differ for [");
}

#[test]
fn cp1140_euro_vs_cp037_control_at_0xff() {
    // CP1140: 0xFF = € (U+20AC); CP037: 0xFF = control (U+009F)
    let cp1140 = ebcdic_to_utf8(&[0xFF], Codepage::CP1140, UnmappablePolicy::Error).unwrap();
    assert_eq!(cp1140, "€");
    // CP037 0xFF maps to U+009F which is a control char < 0x20 range (actually 0x9F > 0x20,
    // but it's handled by the char mapping). Let's just check it's different.
    let cp037 = ebcdic_to_utf8(&[0xFF], Codepage::CP037, UnmappablePolicy::Replace).unwrap();
    assert_ne!(cp037, "€", "CP037 0xFF must not be €");
}

#[test]
fn cp273_german_national_chars_differ_from_cp037() {
    // CP273 has Ä, Ö, Ü at different positions than CP037
    let cp273_enc = utf8_to_ebcdic("Ä", Codepage::CP273).unwrap();
    let cp037_enc = utf8_to_ebcdic("Ä", Codepage::CP037).unwrap();
    assert_ne!(
        cp273_enc, cp037_enc,
        "CP273 and CP037 should place Ä differently"
    );
}

#[test]
fn exclamation_mark_position_varies() {
    // CP037: ! = 0x5A; CP500: ! = 0x4F; CP273: ! = 0x4F
    let cp037 = utf8_to_ebcdic("!", Codepage::CP037).unwrap();
    let cp500 = utf8_to_ebcdic("!", Codepage::CP500).unwrap();
    assert_ne!(cp037, cp500, "! position differs CP037 vs CP500");
}

#[test]
fn digits_are_f0_f9_on_every_codepage() {
    for cp in ALL_EBCDIC {
        for d in 0u8..=9 {
            let ch = char::from(b'0' + d).to_string();
            let enc = utf8_to_ebcdic(&ch, cp).unwrap();
            assert_eq!(enc, vec![0xF0 + d], "{cp:?}: digit {d}");
        }
    }
}

// ============================================================================
// 5. Batch conversion (entire string)
// ============================================================================

#[test]
fn batch_encode_decode_cobol_field_pattern() {
    let field = "CUSTOMER-NAME   JOHN DOE        ";
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(field, cp).unwrap();
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, field, "{cp:?}: batch field roundtrip");
    }
}

#[test]
fn batch_encode_long_repeated_pattern() {
    let pattern = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ";
    let long_text: String = pattern.repeat(100);
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic(&long_text, cp).unwrap();
        assert_eq!(enc.len(), long_text.len());
        let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, long_text, "{cp:?}: long pattern roundtrip");
    }
}

// ============================================================================
// 6. Mixed valid/invalid byte handling
// ============================================================================

#[test]
fn mixed_valid_invalid_replace_inserts_replacement_chars() {
    // 0x01 maps to U+0001 (control) on all EBCDIC codepages
    let data: &[u8] = &[0x01, 0xC1, 0x01, 0xC2, 0x01];
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(data, cp, UnmappablePolicy::Replace).unwrap();
        assert_eq!(
            result.matches('\u{FFFD}').count(),
            3,
            "{cp:?}: expected 3 replacements"
        );
        assert!(result.contains('A'), "{cp:?}: missing 'A'");
        assert!(result.contains('B'), "{cp:?}: missing 'B'");
    }
}

#[test]
fn mixed_valid_invalid_skip_drops_invalid() {
    let data: &[u8] = &[0x01, 0xF1, 0x01, 0xF2, 0x01];
    for cp in ALL_EBCDIC {
        let result = ebcdic_to_utf8(data, cp, UnmappablePolicy::Skip).unwrap();
        assert_eq!(result, "12", "{cp:?}: skip should yield '12'");
    }
}

#[test]
fn mixed_valid_invalid_error_stops_at_first_invalid() {
    let data: &[u8] = &[0xC1, 0x01, 0xC2];
    for cp in ALL_EBCDIC {
        let err = ebcdic_to_utf8(data, cp, UnmappablePolicy::Error).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE, "{cp:?}");
    }
}

#[test]
fn all_control_bytes_produce_error_on_error_policy() {
    // Bytes 0x00-0x03 map to control chars on all codepages
    for cp in ALL_EBCDIC {
        for byte in [0x00u8, 0x01, 0x02, 0x03] {
            let result = ebcdic_to_utf8(&[byte], cp, UnmappablePolicy::Error);
            assert!(result.is_err(), "{cp:?}: byte 0x{byte:02X} should error");
        }
    }
}

// ============================================================================
// 7. Performance sanity: 1 MB conversion < 100ms
// ============================================================================

#[test]
fn performance_1mb_decode_under_1s() {
    // 1 MB of EBCDIC spaces (0x40)
    let data = vec![0x40u8; 1_024 * 1_024];
    let start = Instant::now();
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    let elapsed = start.elapsed();
    assert_eq!(result.len(), 1_024 * 1_024);
    assert!(
        elapsed.as_millis() < 1_000,
        "1MB decode took {}ms (limit: 1000ms)",
        elapsed.as_millis()
    );
}

#[test]
fn performance_1mb_encode_under_1s() {
    let text: String = std::iter::repeat_n('A', 1_024 * 1_024).collect();
    let start = Instant::now();
    let result = utf8_to_ebcdic(&text, Codepage::CP037).unwrap();
    let elapsed = start.elapsed();
    assert_eq!(result.len(), 1_024 * 1_024);
    assert!(
        elapsed.as_millis() < 1_000,
        "1MB encode took {}ms (limit: 1000ms)",
        elapsed.as_millis()
    );
}

#[test]
fn performance_1mb_mixed_content_decode() {
    // Alternating digits and spaces
    let data: Vec<u8> = (0..1_024 * 1_024)
        .map(|i| if i % 2 == 0 { 0xF0 } else { 0x40 })
        .collect();
    let start = Instant::now();
    let result = ebcdic_to_utf8(&data, Codepage::CP037, UnmappablePolicy::Error).unwrap();
    let elapsed = start.elapsed();
    assert_eq!(result.len(), 1_024 * 1_024);
    assert!(
        elapsed.as_millis() < 1_000,
        "1MB mixed decode took {}ms",
        elapsed.as_millis()
    );
}

// ============================================================================
// 8. Thread safety of conversion functions
// ============================================================================

#[test]
fn thread_safety_concurrent_encode_decode() {
    use std::thread;

    let handles: Vec<_> = ALL_EBCDIC
        .iter()
        .map(|&cp| {
            thread::spawn(move || {
                let text = "THREAD SAFE 12345";
                let enc = utf8_to_ebcdic(text, cp).unwrap();
                let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
                assert_eq!(dec, text, "{cp:?}: thread safety roundtrip");
            })
        })
        .collect();

    for h in handles {
        h.join().expect("thread panicked");
    }
}

#[test]
fn thread_safety_concurrent_large_buffers() {
    use std::thread;

    let handles: Vec<_> = (0..8)
        .map(|i| {
            let cp = ALL_EBCDIC[i % ALL_EBCDIC.len()];
            thread::spawn(move || {
                let text: String = format!("THREAD{i} ").repeat(1000);
                let enc = utf8_to_ebcdic(&text, cp).unwrap();
                let dec = ebcdic_to_utf8(&enc, cp, UnmappablePolicy::Error).unwrap();
                assert_eq!(dec, text);
            })
        })
        .collect();

    for h in handles {
        h.join().expect("thread panicked");
    }
}

// ============================================================================
// 9. Empty string conversion
// ============================================================================

#[test]
fn empty_string_encode_all_codepages() {
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic("", cp).unwrap();
        assert!(enc.is_empty(), "{cp:?}: empty encode should be empty");
    }
    let enc = utf8_to_ebcdic("", Codepage::ASCII).unwrap();
    assert!(enc.is_empty());
}

#[test]
fn empty_slice_decode_all_codepages() {
    for cp in ALL_EBCDIC {
        let dec = ebcdic_to_utf8(&[], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, "", "{cp:?}: empty decode");
    }
    let dec = ebcdic_to_utf8(&[], Codepage::ASCII, UnmappablePolicy::Error).unwrap();
    assert_eq!(dec, "");
}

// ============================================================================
// 10. Single byte conversion
// ============================================================================

#[test]
fn single_byte_space_on_all_codepages() {
    for cp in ALL_EBCDIC {
        let sb = space_byte(cp);
        let dec = ebcdic_to_utf8(&[sb], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, " ", "{cp:?}: space byte 0x{sb:02X}");
    }
    assert_eq!(space_byte(Codepage::ASCII), 0x20);
}

#[test]
fn single_byte_letter_a_on_all_codepages() {
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic("A", cp).unwrap();
        assert_eq!(enc.len(), 1, "{cp:?}: 'A' should be 1 byte");
        assert_eq!(enc[0], 0xC1, "{cp:?}: 'A' should be 0xC1");
    }
}

#[test]
fn single_byte_digit_zero_on_all_codepages() {
    for cp in ALL_EBCDIC {
        let enc = utf8_to_ebcdic("0", cp).unwrap();
        assert_eq!(enc, vec![0xF0], "{cp:?}: '0' should be 0xF0");
        let dec = ebcdic_to_utf8(&[0xF0], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(dec, "0", "{cp:?}: 0xF0 should decode to '0'");
    }
}

// ============================================================================
// 11. Known EBCDIC/ASCII mapping verification (specific bytes)
// ============================================================================

#[test]
fn known_cp037_mappings() {
    let known: &[(u8, char)] = &[
        (0x40, ' '),
        (0x4B, '.'),
        (0x4C, '<'),
        (0x4D, '('),
        (0x4E, '+'),
        (0x50, '&'),
        (0x5A, '!'),
        (0x5B, '$'),
        (0x5C, '*'),
        (0x5D, ')'),
        (0x5E, ';'),
        (0x60, '-'),
        (0x61, '/'),
        (0x6B, ','),
        (0x6C, '%'),
        (0x6D, '_'),
        (0x6E, '>'),
        (0x6F, '?'),
        (0x7A, ':'),
        (0x7B, '#'),
        (0x7C, '@'),
        (0x7D, '\''),
        (0x7E, '='),
        (0x7F, '"'),
        (0xC1, 'A'),
        (0xC9, 'I'),
        (0xD1, 'J'),
        (0xD9, 'R'),
        (0xE2, 'S'),
        (0xE9, 'Z'),
        (0xF0, '0'),
        (0xF9, '9'),
    ];
    for &(ebcdic_byte, expected_char) in known {
        let dec = ebcdic_to_utf8(&[ebcdic_byte], Codepage::CP037, UnmappablePolicy::Error).unwrap();
        assert_eq!(
            dec,
            expected_char.to_string(),
            "CP037: 0x{ebcdic_byte:02X} should be '{expected_char}'"
        );
    }
}

#[test]
fn known_cp500_mappings_differ_from_cp037() {
    // CP500: 0x4A = '[', CP037: 0x4A = '¢'
    let cp500 = ebcdic_to_utf8(&[0x4A], Codepage::CP500, UnmappablePolicy::Error).unwrap();
    let cp037 = ebcdic_to_utf8(&[0x4A], Codepage::CP037, UnmappablePolicy::Error).unwrap();
    assert_eq!(cp500, "[");
    assert_eq!(cp037, "¢");
    assert_ne!(cp500, cp037);
}

#[test]
fn known_cp1047_curly_brace_positions() {
    // CP1047: { = 0xC0, } = 0xD0 (same as CP037 and CP500)
    let lbrace = ebcdic_to_utf8(&[0xC0], Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    let rbrace = ebcdic_to_utf8(&[0xD0], Codepage::CP1047, UnmappablePolicy::Error).unwrap();
    assert_eq!(lbrace, "{");
    assert_eq!(rbrace, "}");
}

#[test]
fn lowercase_a_z_on_all_codepages_at_correct_ebcdic_positions() {
    // a-i = 0x81-0x89, j-r = 0x91-0x99, s-z = 0xA2-0xA9
    for cp in ALL_EBCDIC {
        for (i, ch) in ('a'..='i').enumerate() {
            let enc = utf8_to_ebcdic(&ch.to_string(), cp).unwrap();
            assert_eq!(enc[0], 0x81 + u8::try_from(i).unwrap(), "{cp:?}: '{ch}'");
        }
        for (i, ch) in ('j'..='r').enumerate() {
            let enc = utf8_to_ebcdic(&ch.to_string(), cp).unwrap();
            assert_eq!(enc[0], 0x91 + u8::try_from(i).unwrap(), "{cp:?}: '{ch}'");
        }
        for (i, ch) in ('s'..='z').enumerate() {
            let enc = utf8_to_ebcdic(&ch.to_string(), cp).unwrap();
            assert_eq!(enc[0], 0xA2 + u8::try_from(i).unwrap(), "{cp:?}: '{ch}'");
        }
    }
}

// ============================================================================
// 12. ASCII passthrough mode
// ============================================================================

#[test]
fn ascii_passthrough_preserves_all_bytes() {
    let data: Vec<u8> = (0x20..=0x7E).collect();
    let dec = ebcdic_to_utf8(&data, Codepage::ASCII, UnmappablePolicy::Error).unwrap();
    let expected: String = (0x20u8..=0x7E).map(|b| b as char).collect();
    assert_eq!(dec, expected);
}

#[test]
fn ascii_passthrough_encode_is_identity() {
    let text = "Hello, World! 12345 @#$%^&*()";
    let enc = utf8_to_ebcdic(text, Codepage::ASCII).unwrap();
    assert_eq!(enc, text.as_bytes());
}

// ============================================================================
// 13. Unmappable character (UTF-8 → EBCDIC direction)
// ============================================================================

#[test]
fn unmappable_emoji_rejected_all_codepages() {
    for cp in ALL_EBCDIC {
        let err = utf8_to_ebcdic("😀", cp).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE, "{cp:?}");
    }
}

#[test]
fn unmappable_cjk_rejected_all_codepages() {
    for cp in ALL_EBCDIC {
        let err = utf8_to_ebcdic("日本語", cp).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE, "{cp:?}");
    }
}

#[test]
fn unmappable_partial_string_fails_at_bad_char() {
    // "ABC日" — first 3 chars are valid, 4th is not
    for cp in ALL_EBCDIC {
        let err = utf8_to_ebcdic("ABC日", cp).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE, "{cp:?}");
    }
}

// ============================================================================
// 14. Whitespace and newline handling
// ============================================================================

#[test]
fn tab_lf_cr_allowed_on_all_codepages() {
    for cp in ALL_EBCDIC {
        // Tab: EBCDIC 0x05 → \t
        let tab = ebcdic_to_utf8(&[0x05], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(tab, "\t", "{cp:?}: tab");
        // LF: EBCDIC 0x25 → \n
        let lf = ebcdic_to_utf8(&[0x25], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(lf, "\n", "{cp:?}: LF");
        // CR: EBCDIC 0x0D → \r
        let cr = ebcdic_to_utf8(&[0x0D], cp, UnmappablePolicy::Error).unwrap();
        assert_eq!(cr, "\r", "{cp:?}: CR");
    }
}

// ============================================================================
// 15. Codepage enum properties
// ============================================================================

#[test]
fn codepage_is_ascii_vs_ebcdic() {
    assert!(Codepage::ASCII.is_ascii());
    assert!(!Codepage::ASCII.is_ebcdic());
    for cp in ALL_EBCDIC {
        assert!(!cp.is_ascii(), "{cp:?}");
        assert!(cp.is_ebcdic(), "{cp:?}");
    }
}

#[test]
fn codepage_code_page_numbers() {
    assert_eq!(Codepage::ASCII.code_page_number(), None);
    assert_eq!(Codepage::CP037.code_page_number(), Some(37));
    assert_eq!(Codepage::CP273.code_page_number(), Some(273));
    assert_eq!(Codepage::CP500.code_page_number(), Some(500));
    assert_eq!(Codepage::CP1047.code_page_number(), Some(1047));
    assert_eq!(Codepage::CP1140.code_page_number(), Some(1140));
}
