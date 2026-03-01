// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-zoned-format.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_zoned_format::ZonedEncodingFormat;

// ====================================================================
// 1. Zoned decimal detection from bytes
// ====================================================================

#[test]
fn detect_ascii_digit_range_0x30_to_0x39() {
    for byte in 0x30u8..=0x39 {
        let detected = ZonedEncodingFormat::detect_from_byte(byte);
        assert_eq!(
            detected,
            Some(ZonedEncodingFormat::Ascii),
            "byte 0x{byte:02X} should be ASCII"
        );
    }
}

#[test]
fn detect_ascii_non_digit_0x3a_to_0x3f() {
    // These also have zone nibble 0x3, so they detect as ASCII
    for byte in 0x3Au8..=0x3F {
        let detected = ZonedEncodingFormat::detect_from_byte(byte);
        assert_eq!(
            detected,
            Some(ZonedEncodingFormat::Ascii),
            "byte 0x{byte:02X} should detect as ASCII zone"
        );
    }
}

#[test]
fn detect_ebcdic_digit_range_0xf0_to_0xf9() {
    for byte in 0xF0u8..=0xF9 {
        let detected = ZonedEncodingFormat::detect_from_byte(byte);
        assert_eq!(
            detected,
            Some(ZonedEncodingFormat::Ebcdic),
            "byte 0x{byte:02X} should be EBCDIC"
        );
    }
}

#[test]
fn detect_ebcdic_non_digit_0xfa_to_0xff() {
    for byte in 0xFAu8..=0xFF {
        let detected = ZonedEncodingFormat::detect_from_byte(byte);
        assert_eq!(
            detected,
            Some(ZonedEncodingFormat::Ebcdic),
            "byte 0x{byte:02X} should detect as EBCDIC zone"
        );
    }
}

// ====================================================================
// 2. Positive/negative zone nibbles (sign detection)
// ====================================================================

#[test]
fn detect_ebcdic_sign_zones_c_and_d() {
    // 0xC0-0xCF (positive sign zone) and 0xD0-0xDF (negative sign zone)
    // are NOT 0x3 or 0xF zones, so they return None
    for byte in 0xC0u8..=0xCF {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            None,
            "0x{byte:02X}: C-zone is not a digit zone"
        );
    }
    for byte in 0xD0u8..=0xDF {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            None,
            "0x{byte:02X}: D-zone is not a digit zone"
        );
    }
}

// ====================================================================
// 3. Decimal scaling (zone nibble extraction)
// ====================================================================

#[test]
fn zone_nibble_extraction_for_all_256_bytes() {
    for byte in 0u8..=255 {
        let zone = (byte >> 4) & 0x0F;
        let detected = ZonedEncodingFormat::detect_from_byte(byte);
        match zone {
            0x3 => assert_eq!(detected, Some(ZonedEncodingFormat::Ascii)),
            0xF => assert_eq!(detected, Some(ZonedEncodingFormat::Ebcdic)),
            _ => assert_eq!(detected, None, "byte 0x{byte:02X} zone 0x{zone:X}"),
        }
    }
}

// ====================================================================
// 4. Edge cases
// ====================================================================

#[test]
fn detect_zero_byte() {
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x00), None);
}

#[test]
fn detect_max_byte() {
    // 0xFF has zone nibble 0xF → EBCDIC
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0xFF),
        Some(ZonedEncodingFormat::Ebcdic)
    );
}

#[test]
fn detect_boundary_0x2f_and_0x40() {
    // 0x2F has zone 0x2 → None
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x2F), None);
    // 0x40 has zone 0x4 → None
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0x40), None);
}

#[test]
fn detect_boundary_0xef_and_0xf0() {
    // 0xEF has zone 0xE → None
    assert_eq!(ZonedEncodingFormat::detect_from_byte(0xEF), None);
    // 0xF0 has zone 0xF → EBCDIC
    assert_eq!(
        ZonedEncodingFormat::detect_from_byte(0xF0),
        Some(ZonedEncodingFormat::Ebcdic)
    );
}

// ====================================================================
// 5. Predicates, Display, Default
// ====================================================================

#[test]
fn predicates_are_mutually_exclusive() {
    for variant in [
        ZonedEncodingFormat::Ascii,
        ZonedEncodingFormat::Ebcdic,
        ZonedEncodingFormat::Auto,
    ] {
        let count = [variant.is_ascii(), variant.is_ebcdic(), variant.is_auto()]
            .iter()
            .filter(|&&v| v)
            .count();
        assert_eq!(
            count, 1,
            "{variant:?}: exactly one predicate should be true"
        );
    }
}

#[test]
fn default_is_auto() {
    assert_eq!(ZonedEncodingFormat::default(), ZonedEncodingFormat::Auto);
    assert!(ZonedEncodingFormat::default().is_auto());
}

#[test]
fn display_format_all_variants() {
    assert_eq!(format!("{}", ZonedEncodingFormat::Ascii), "ascii");
    assert_eq!(format!("{}", ZonedEncodingFormat::Ebcdic), "ebcdic");
    assert_eq!(format!("{}", ZonedEncodingFormat::Auto), "auto");
}

#[test]
fn description_all_variants_nonempty() {
    for variant in [
        ZonedEncodingFormat::Ascii,
        ZonedEncodingFormat::Ebcdic,
        ZonedEncodingFormat::Auto,
    ] {
        assert!(!variant.description().is_empty(), "{variant:?}");
    }
}

// ====================================================================
// 6. Serde round-trip
// ====================================================================

#[test]
fn serde_roundtrip_all_variants() {
    for variant in [
        ZonedEncodingFormat::Ascii,
        ZonedEncodingFormat::Ebcdic,
        ZonedEncodingFormat::Auto,
    ] {
        let json = serde_json::to_string(&variant).unwrap();
        let deserialized: ZonedEncodingFormat = serde_json::from_str(&json).unwrap();
        assert_eq!(variant, deserialized, "{variant:?} serde round-trip");
    }
}

#[test]
fn serde_deserialize_from_known_strings() {
    let cases: [(&str, ZonedEncodingFormat); 3] = [
        ("\"Ascii\"", ZonedEncodingFormat::Ascii),
        ("\"Ebcdic\"", ZonedEncodingFormat::Ebcdic),
        ("\"Auto\"", ZonedEncodingFormat::Auto),
    ];
    for (json, expected) in cases {
        let deserialized: ZonedEncodingFormat = serde_json::from_str(json).unwrap();
        assert_eq!(deserialized, expected, "from {json}");
    }
}

// ====================================================================
// 7. Clone / Copy / Eq traits
// ====================================================================

#[test]
fn clone_and_copy_semantics() {
    let a = ZonedEncodingFormat::Ebcdic;
    let b = a; // Copy
    let c = a; // Clone (Copy semantics)
    assert_eq!(a, b);
    assert_eq!(a, c);
}

#[test]
fn equality_across_all_variants() {
    let variants = [
        ZonedEncodingFormat::Ascii,
        ZonedEncodingFormat::Ebcdic,
        ZonedEncodingFormat::Auto,
    ];
    for (i, a) in variants.iter().enumerate() {
        for (j, b) in variants.iter().enumerate() {
            if i == j {
                assert_eq!(a, b);
            } else {
                assert_ne!(a, b);
            }
        }
    }
}

// ====================================================================
// 8. Invalid zone nibbles (exhaustive)
// ====================================================================

#[test]
fn all_invalid_zone_nibbles() {
    // Zone nibbles 0x0-0x2, 0x4-0xE should all return None
    for zone in [
        0x0u8, 0x1, 0x2, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE,
    ] {
        let byte = zone << 4; // digit nibble = 0
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(byte),
            None,
            "zone 0x{zone:X} should be invalid"
        );
    }
}
