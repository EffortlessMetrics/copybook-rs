// SPDX-License-Identifier: AGPL-3.0-or-later
//! Zoned-decimal encoding detection helpers used by codec options and framing logic.
#![allow(clippy::missing_inline_in_public_items)]

use serde::{Deserialize, Serialize};
use std::fmt;

/// Zone nibble constants for zoned decimal encoding detection.
mod zone_constants {
    /// ASCII digit zone nibble (0x30-0x39 range).
    pub const ASCII_ZONE: u8 = 0x3;
    /// EBCDIC digit zone nibble (0xF0-0xF9 range).
    pub const EBCDIC_ZONE: u8 = 0xF;
    /// Zone nibble mask for extracting upper 4 bits.
    pub const ZONE_MASK: u8 = 0x0F;
}

/// Zoned decimal encoding format specification for round-trip fidelity.
///
/// This enum controls how zoned decimal fields are encoded and decoded,
/// enabling preservation of the original encoding format during round-trip
/// operations for enterprise data consistency.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum, Default)]
pub enum ZonedEncodingFormat {
    /// ASCII digit zones (0x30-0x39).
    Ascii,
    /// EBCDIC digit zones (0xF0-0xF9).
    Ebcdic,
    /// Automatic detection based on zone nibbles.
    #[default]
    Auto,
}

impl ZonedEncodingFormat {
    /// Check if this is ASCII encoding.
    #[must_use]
    #[inline]
    pub const fn is_ascii(self) -> bool {
        matches!(self, Self::Ascii)
    }

    /// Check if this is EBCDIC encoding.
    #[must_use]
    #[inline]
    pub const fn is_ebcdic(self) -> bool {
        matches!(self, Self::Ebcdic)
    }

    /// Check if this is auto-detection mode.
    #[must_use]
    #[inline]
    pub const fn is_auto(self) -> bool {
        matches!(self, Self::Auto)
    }

    /// Get a human-readable description of the encoding format.
    #[must_use]
    #[inline]
    pub const fn description(self) -> &'static str {
        match self {
            Self::Ascii => "ASCII digit zones (0x30-0x39)",
            Self::Ebcdic => "EBCDIC digit zones (0xF0-0xF9)",
            Self::Auto => "Automatic detection based on zone nibbles",
        }
    }

    /// Detect encoding format from a single byte of zoned decimal data.
    ///
    /// Examines the zone nibble (upper 4 bits) to determine the encoding
    /// format. Returns `None` for invalid zone values.
    #[must_use]
    #[inline]
    pub fn detect_from_byte(byte: u8) -> Option<Self> {
        let zone_nibble = (byte >> 4) & zone_constants::ZONE_MASK;
        match zone_nibble {
            zone_constants::ASCII_ZONE => Some(Self::Ascii),
            zone_constants::EBCDIC_ZONE => Some(Self::Ebcdic),
            _ => None,
        }
    }
}

impl fmt::Display for ZonedEncodingFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ascii => write!(f, "ascii"),
            Self::Ebcdic => write!(f, "ebcdic"),
            Self::Auto => write!(f, "auto"),
        }
    }
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn detect_from_byte_known_bytes() {
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0x35),
            Some(ZonedEncodingFormat::Ascii)
        );
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0xF4),
            Some(ZonedEncodingFormat::Ebcdic)
        );
        assert_eq!(ZonedEncodingFormat::detect_from_byte(0x00), None);
    }

    #[test]
    fn display_and_predicates() {
        assert!(ZonedEncodingFormat::Ascii.is_ascii());
        assert!(!ZonedEncodingFormat::Auto.is_ascii());
        assert_eq!(format!("{}", ZonedEncodingFormat::Ascii), "ascii");
        assert_eq!(
            ZonedEncodingFormat::Auto.description(),
            "Automatic detection based on zone nibbles"
        );
    }

    // --- is_ebcdic ---

    #[test]
    fn test_is_ebcdic() {
        assert!(ZonedEncodingFormat::Ebcdic.is_ebcdic());
        assert!(!ZonedEncodingFormat::Ascii.is_ebcdic());
        assert!(!ZonedEncodingFormat::Auto.is_ebcdic());
    }

    // --- is_auto ---

    #[test]
    fn test_is_auto() {
        assert!(ZonedEncodingFormat::Auto.is_auto());
        assert!(!ZonedEncodingFormat::Ascii.is_auto());
        assert!(!ZonedEncodingFormat::Ebcdic.is_auto());
    }

    // --- Default ---

    #[test]
    fn test_default_is_auto() {
        assert_eq!(ZonedEncodingFormat::default(), ZonedEncodingFormat::Auto);
    }

    // --- Display all variants ---

    #[test]
    fn test_display_all_variants() {
        assert_eq!(format!("{}", ZonedEncodingFormat::Ascii), "ascii");
        assert_eq!(format!("{}", ZonedEncodingFormat::Ebcdic), "ebcdic");
        assert_eq!(format!("{}", ZonedEncodingFormat::Auto), "auto");
    }

    // --- description all variants ---

    #[test]
    fn test_description_all_variants() {
        assert_eq!(
            ZonedEncodingFormat::Ascii.description(),
            "ASCII digit zones (0x30-0x39)"
        );
        assert_eq!(
            ZonedEncodingFormat::Ebcdic.description(),
            "EBCDIC digit zones (0xF0-0xF9)"
        );
        assert_eq!(
            ZonedEncodingFormat::Auto.description(),
            "Automatic detection based on zone nibbles"
        );
    }

    // --- detect_from_byte comprehensive ---

    #[test]
    fn test_detect_from_byte_all_ascii_digits() {
        for byte in 0x30..=0x3F {
            assert_eq!(
                ZonedEncodingFormat::detect_from_byte(byte),
                Some(ZonedEncodingFormat::Ascii),
                "Failed for byte 0x{byte:02X}"
            );
        }
    }

    #[test]
    fn test_detect_from_byte_all_ebcdic_digits() {
        for byte in 0xF0..=0xFF {
            assert_eq!(
                ZonedEncodingFormat::detect_from_byte(byte),
                Some(ZonedEncodingFormat::Ebcdic),
                "Failed for byte 0x{byte:02X}"
            );
        }
    }

    #[test]
    fn test_detect_from_byte_invalid_zones() {
        // Zone nibbles 0x0, 0x1, 0x2, 0x4-0xE should return None
        let invalid_samples: &[u8] = &[
            0x00, 0x10, 0x20, 0x40, 0x50, 0x60, 0x70, 0x80, 0x90, 0xA0, 0xB0, 0xC0, 0xD0, 0xE0,
        ];
        for &byte in invalid_samples {
            assert_eq!(
                ZonedEncodingFormat::detect_from_byte(byte),
                None,
                "Expected None for byte 0x{byte:02X}"
            );
        }
    }

    // --- Serde round-trip ---

    #[test]
    fn test_serde_roundtrip() {
        for variant in [
            ZonedEncodingFormat::Ascii,
            ZonedEncodingFormat::Ebcdic,
            ZonedEncodingFormat::Auto,
        ] {
            let json = serde_json::to_string(&variant).unwrap();
            let deserialized: ZonedEncodingFormat = serde_json::from_str(&json).unwrap();
            assert_eq!(
                variant, deserialized,
                "Serde round-trip failed for {variant:?}"
            );
        }
    }

    // --- Clone / Copy / Eq ---

    #[test]
    fn test_clone_and_eq() {
        let a = ZonedEncodingFormat::Ebcdic;
        let b = a;
        assert_eq!(a, b);
    }
}
