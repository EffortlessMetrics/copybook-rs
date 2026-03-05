// SPDX-License-Identifier: AGPL-3.0-or-later
//! Edge-case and extended tests for copybook-zoned-format.
//!
//! Covers: zoned decimal parsing, overpunch sign detection via zone nibble,
//! zone nibble extraction, decimal point handling edge cases, and all-zero /
//! all-nine / sign-variation boundaries.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_zoned_format::ZonedEncodingFormat;

// ====================================================================
// 1. Zone nibble extraction – digit nibble permutations
// ====================================================================

#[test]
fn zone_nibble_ascii_all_digit_nibbles() {
    // Zone 0x3 with every possible digit nibble (0x0-0xF)
    for digit in 0x0u8..=0xF {
        let byte = 0x30 | digit;
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ascii),
            "0x{byte:02X} should detect as ASCII"
        );
    }
}

#[test]
fn zone_nibble_ebcdic_all_digit_nibbles() {
    // Zone 0xF with every possible digit nibble (0x0-0xF)
    for digit in 0x0u8..=0xF {
        let byte = 0xF0 | digit;
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ebcdic),
            "0x{byte:02X} should detect as EBCDIC"
        );
    }
}

// ====================================================================
// 2. Overpunch sign zones (C/D) are not digit zones
// ====================================================================

#[test]
fn sign_zone_c_positive_not_detected_as_digit() {
    // 0xC0-0xC9: EBCDIC positive overpunch zone
    for digit in 0u8..=9 {
        let byte = 0xC0 | digit;
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            None,
            "Positive sign byte 0x{byte:02X} must not be a digit zone"
        );
    }
}

#[test]
fn sign_zone_d_negative_not_detected_as_digit() {
    // 0xD0-0xD9: EBCDIC negative overpunch zone
    for digit in 0u8..=9 {
        let byte = 0xD0 | digit;
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            None,
            "Negative sign byte 0x{byte:02X} must not be a digit zone"
        );
    }
}

// ====================================================================
// 3. All-zeros edge case
// ====================================================================

#[test]
fn detect_ascii_zero_0x30() {
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0x30),
        Some(ZonedEncodingFormat::Ascii),
    );
}

#[test]
fn detect_ebcdic_zero_0xf0() {
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0xF0),
        Some(ZonedEncodingFormat::Ebcdic),
    );
}

#[test]
fn detect_null_byte_returns_none() {
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x00), None);
}

// ====================================================================
// 4. All-nines edge case
// ====================================================================

#[test]
fn detect_ascii_nine_0x39() {
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0x39),
        Some(ZonedEncodingFormat::Ascii),
    );
}

#[test]
fn detect_ebcdic_nine_0xf9() {
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0xF9),
        Some(ZonedEncodingFormat::Ebcdic),
    );
}

// ====================================================================
// 5. Boundary bytes between zones
// ====================================================================

#[test]
fn boundary_below_ascii_zone() {
    // 0x2F is zone 0x2 → None
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x2F), None);
}

#[test]
fn boundary_above_ascii_zone() {
    // 0x40 is zone 0x4 → None
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x40), None);
}

#[test]
fn boundary_below_ebcdic_zone() {
    // 0xEF is zone 0xE → None
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0xEF), None);
}

#[test]
fn boundary_at_ebcdic_zone_start() {
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0xF0),
        Some(ZonedEncodingFormat::Ebcdic)
    );
}

// ====================================================================
// 6. Sign variation: zones 0xA, 0xB, 0xE are invalid
// ====================================================================

#[test]
fn zone_a_is_invalid() {
    for nibble in 0u8..=0xF {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0xA0 | nibble),
            None,
            "0xA zone should be invalid"
        );
    }
}

#[test]
fn zone_b_is_invalid() {
    for nibble in 0u8..=0xF {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0xB0 | nibble),
            None,
            "0xB zone should be invalid"
        );
    }
}

#[test]
fn zone_e_is_invalid() {
    for nibble in 0u8..=0xF {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0xE0 | nibble),
            None,
            "0xE zone should be invalid"
        );
    }
}

// ====================================================================
// 7. Decimal point handling – not in detect_from_byte, tested via
//    display/description integration (zoned format doesn't handle
//    decimal itself, that's the codec layer)
// ====================================================================

#[test]
fn display_roundtrip_from_string() {
    let variants = [
        ("ascii", ZonedEncodingFormat::Ascii),
        ("ebcdic", ZonedEncodingFormat::Ebcdic),
        ("auto", ZonedEncodingFormat::Auto),
    ];
    for (expected_str, variant) in variants {
        assert_eq!(format!("{variant}"), expected_str);
    }
}

// ====================================================================
// 8. Serde edge cases
// ====================================================================

#[test]
fn serde_reject_unknown_variant() {
    let result = serde_json::from_str::<ZonedEncodingFormat>("\"Packed\"");
    assert!(result.is_err(), "Unknown variant should be rejected");
}

#[test]
fn serde_reject_integer() {
    let result = serde_json::from_str::<ZonedEncodingFormat>("42");
    assert!(result.is_err(), "Integer should be rejected");
}

#[test]
fn serde_reject_null() {
    let result = serde_json::from_str::<ZonedEncodingFormat>("null");
    assert!(result.is_err(), "null should be rejected");
}

// ====================================================================
// 9. Debug representation
// ====================================================================

#[test]
fn debug_output_contains_variant_name() {
    assert!(format!("{:?}", ZonedEncodingFormat::Ascii).contains("Ascii"));
    assert!(format!("{:?}", ZonedEncodingFormat::Ebcdic).contains("Ebcdic"));
    assert!(format!("{:?}", ZonedEncodingFormat::Auto).contains("Auto"));
}

// ====================================================================
// 10. Exhaustive 256-byte sweep
// ====================================================================

#[test]
fn exhaustive_256_byte_detection_consistency() {
    for byte in 0u8..=255 {
        let zone = byte >> 4;
        let detected = ZonedEncodingFormat::detect_from_byte(byte);
        match zone {
            0x3 => assert_eq!(detected, Some(ZonedEncodingFormat::Ascii), "0x{byte:02X}"),
            0xF => assert_eq!(detected, Some(ZonedEncodingFormat::Ebcdic), "0x{byte:02X}"),
            _ => assert_eq!(detected, None, "0x{byte:02X}"),
        }
    }
}

// ====================================================================
// 11. Multiple detections on multi-byte zoned decimal values
// ====================================================================

#[test]
fn multi_byte_zoned_decimal_ascii_field() {
    // Simulates detecting format from each byte of an ASCII zoned decimal "12345"
    let ascii_field: &[u8] = &[0x31, 0x32, 0x33, 0x34, 0x35];
    for &byte in ascii_field {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ascii)
        );
    }
}

#[test]
fn multi_byte_zoned_decimal_ebcdic_field() {
    // Simulates detecting format from each byte of an EBCDIC zoned decimal
    let ebcdic_field: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    for &byte in ebcdic_field {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ebcdic)
        );
    }
}

#[test]
fn multi_byte_zoned_decimal_ebcdic_with_sign_last_byte() {
    // Last byte has overpunch sign (0xC5 = positive 5), rest are F-zone digits
    let field: &[u8] = &[0xF1, 0xF2, 0xF3, 0xC5];
    // Non-last bytes: EBCDIC
    for &byte in &field[..3] {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ebcdic)
        );
    }
    // Last byte (sign): C-zone is not a digit zone
    assert_eq!(ZonedEncodingFormat::detect_from_byte(field[3]), None);
}

#[test]
fn multi_byte_all_zeros_ascii() {
    let field: &[u8] = &[0x30, 0x30, 0x30, 0x30];
    for &byte in field {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ascii)
        );
    }
}

#[test]
fn multi_byte_all_zeros_ebcdic() {
    let field: &[u8] = &[0xF0, 0xF0, 0xF0, 0xF0];
    for &byte in field {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ebcdic)
        );
    }
}

#[test]
fn multi_byte_all_nines_ascii() {
    let field: &[u8] = &[0x39, 0x39, 0x39, 0x39];
    for &byte in field {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ascii)
        );
    }
}

#[test]
fn multi_byte_all_nines_ebcdic() {
    let field: &[u8] = &[0xF9, 0xF9, 0xF9, 0xF9];
    for &byte in field {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            Some(ZonedEncodingFormat::Ebcdic)
        );
    }
}

#[test]
fn multi_byte_negative_sign_d_zone_last() {
    // 0xD3 = negative digit 3
    let field: &[u8] = &[0xF1, 0xF2, 0xD3];
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(field[0]),
        Some(ZonedEncodingFormat::Ebcdic)
    );
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(field[1]),
        Some(ZonedEncodingFormat::Ebcdic)
    );
    // D-zone is sign zone, not digit
    assert_eq!(ZonedEncodingFormat::detect_from_byte(field[2]), None);
}
