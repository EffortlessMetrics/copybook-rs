// SPDX-License-Identifier: AGPL-3.0-or-later
//! Codepage domain types and helpers.
//!
//! This crate contains codepage-related enums and codepage-specific constants
//! used by charset and numeric handling.

#[cfg(feature = "clap")]
use clap::ValueEnum;
use serde::{Deserialize, Serialize};
use std::str::FromStr;

/// Character encoding specification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "clap", derive(ValueEnum))]
pub enum Codepage {
    /// ASCII encoding
    ASCII,
    /// EBCDIC Code Page 037 (US/Canada)
    #[cfg_attr(feature = "clap", value(name = "cp037"))]
    CP037,
    /// EBCDIC Code Page 273 (Germany/Austria)
    #[cfg_attr(feature = "clap", value(name = "cp273"))]
    CP273,
    /// EBCDIC Code Page 500 (International)
    #[cfg_attr(feature = "clap", value(name = "cp500"))]
    CP500,
    /// EBCDIC Code Page 1047 (Open Systems)
    #[cfg_attr(feature = "clap", value(name = "cp1047"))]
    CP1047,
    /// EBCDIC Code Page 1140 (US/Canada with Euro)
    #[cfg_attr(feature = "clap", value(name = "cp1140"))]
    CP1140,
}

impl Codepage {
    /// Check if this is an ASCII codepage
    #[must_use]
    #[inline]
    pub const fn is_ascii(self) -> bool {
        matches!(self, Self::ASCII)
    }

    /// Check if this is an EBCDIC codepage
    #[must_use]
    #[inline]
    pub const fn is_ebcdic(self) -> bool {
        !self.is_ascii()
    }

    /// Get the numeric code page identifier
    #[must_use]
    #[inline]
    pub const fn code_page_number(self) -> Option<u16> {
        match self {
            Self::ASCII => None,
            Self::CP037 => Some(37),
            Self::CP273 => Some(273),
            Self::CP500 => Some(500),
            Self::CP1047 => Some(1047),
            Self::CP1140 => Some(1140),
        }
    }

    /// Get a human-readable description of the codepage
    #[must_use]
    #[inline]
    pub const fn description(self) -> &'static str {
        match self {
            Self::ASCII => "ASCII encoding",
            Self::CP037 => "EBCDIC Code Page 037 (US/Canada)",
            Self::CP273 => "EBCDIC Code Page 273 (Germany/Austria)",
            Self::CP500 => "EBCDIC Code Page 500 (International)",
            Self::CP1047 => "EBCDIC Code Page 1047 (Open Systems)",
            Self::CP1140 => "EBCDIC Code Page 1140 (US/Canada with Euro)",
        }
    }
}

impl std::fmt::Display for Codepage {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ASCII => write!(f, "ascii"),
            Self::CP037 => write!(f, "cp037"),
            Self::CP273 => write!(f, "cp273"),
            Self::CP500 => write!(f, "cp500"),
            Self::CP1047 => write!(f, "cp1047"),
            Self::CP1140 => write!(f, "cp1140"),
        }
    }
}

impl FromStr for Codepage {
    type Err = std::convert::Infallible;

    #[inline]
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "ascii" => Self::ASCII,
            "cp273" => Self::CP273,
            "cp500" => Self::CP500,
            "cp1047" => Self::CP1047,
            "cp1140" => Self::CP1140,
            // Default to CP037 for backward compatibility
            _ => Self::CP037,
        })
    }
}

/// Policy for handling unmappable characters during decode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "clap", derive(ValueEnum))]
pub enum UnmappablePolicy {
    /// Error on unmappable characters
    #[cfg_attr(feature = "clap", value(name = "error"))]
    Error,
    /// Replace with U+FFFD
    #[cfg_attr(feature = "clap", value(name = "replace"))]
    Replace,
    /// Skip unmappable characters
    #[cfg_attr(feature = "clap", value(name = "skip"))]
    Skip,
}

impl std::fmt::Display for UnmappablePolicy {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Replace => write!(f, "replace"),
            Self::Skip => write!(f, "skip"),
        }
    }
}

impl FromStr for UnmappablePolicy {
    type Err = std::convert::Infallible;

    #[inline]
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "replace" => Self::Replace,
            "skip" => Self::Skip,
            _ => Self::Error, // Default to Error for backward compatibility
        })
    }
}

// Zoned decimal sign tables map the zone nibble (high 4 bits) to sign info.
static EBCDIC_ZONED_SIGNS: [(bool, bool); 16] = [
    (false, false), // 0x0_: unsigned
    (false, false), // 0x1_: unsigned
    (false, false), // 0x2_: unsigned
    (false, false), // 0x3_: unsigned
    (false, false), // 0x4_: unsigned
    (false, false), // 0x5_: unsigned
    (false, false), // 0x6_: unsigned
    (false, false), // 0x7_: unsigned
    (false, false), // 0x8_: unsigned
    (false, false), // 0x9_: unsigned
    (false, false), // 0xA_: unsigned
    (false, false), // 0xB_: unsigned
    (true, false),  // 0xC_: positive
    (true, true),   // 0xD_: negative
    (false, false), // 0xE_: unsigned
    (true, false),  // 0xF_: positive (default)
];

// ASCII overpunch requires byte-level logic, so zoned table is intentionally
// unsigned to avoid accidental misuse in ASCII code paths.
static ASCII_ZONED_SIGNS: [(bool, bool); 16] = [(false, false); 16];

/// Get zoned decimal sign table for a codepage.
#[must_use]
#[inline]
pub fn get_zoned_sign_table(codepage: Codepage) -> &'static [(bool, bool); 16] {
    match codepage {
        Codepage::ASCII => &ASCII_ZONED_SIGNS,
        _ => &EBCDIC_ZONED_SIGNS,
    }
}

/// Get the space byte value for a codepage.
///
/// Returns `0x20` for ASCII, `0x40` for all EBCDIC codepages.
#[must_use]
#[inline]
pub const fn space_byte(codepage: Codepage) -> u8 {
    match codepage {
        Codepage::ASCII => 0x20,
        Codepage::CP037
        | Codepage::CP273
        | Codepage::CP500
        | Codepage::CP1047
        | Codepage::CP1140 => 0x40,
    }
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_space_byte_ascii() {
        assert_eq!(space_byte(Codepage::ASCII), 0x20);
    }

    #[test]
    fn test_space_byte_ebcdic() {
        assert_eq!(space_byte(Codepage::CP037), 0x40);
        assert_eq!(space_byte(Codepage::CP273), 0x40);
        assert_eq!(space_byte(Codepage::CP500), 0x40);
        assert_eq!(space_byte(Codepage::CP1047), 0x40);
        assert_eq!(space_byte(Codepage::CP1140), 0x40);
    }

    #[test]
    fn test_codepage_is_ascii() {
        assert!(Codepage::ASCII.is_ascii());
        assert!(!Codepage::CP037.is_ascii());
    }

    #[test]
    fn test_codepage_is_ebcdic() {
        assert!(!Codepage::ASCII.is_ebcdic());
        assert!(Codepage::CP037.is_ebcdic());
    }

    #[test]
    fn test_codepage_code_page_number() {
        assert_eq!(Codepage::ASCII.code_page_number(), None);
        assert_eq!(Codepage::CP037.code_page_number(), Some(37));
        assert_eq!(Codepage::CP1140.code_page_number(), Some(1140));
    }

    #[test]
    fn test_codepage_from_str_defaults_to_cp037() {
        assert_eq!(
            <Codepage as std::str::FromStr>::from_str("unknown").unwrap(),
            Codepage::CP037
        );
    }

    #[test]
    fn test_unmappable_policy_from_str_defaults_to_error() {
        assert_eq!(
            <UnmappablePolicy as std::str::FromStr>::from_str("unknown").unwrap(),
            UnmappablePolicy::Error
        );
    }

    #[test]
    fn test_get_zoned_sign_table_ascii_is_unsigned() {
        let table = get_zoned_sign_table(Codepage::ASCII);
        assert!(table.iter().all(|entry| *entry == (false, false)));
    }

    #[test]
    fn test_get_zoned_sign_table_ebcdic_has_signed_entries() {
        let table = get_zoned_sign_table(Codepage::CP037);
        assert_eq!(table[0xC], (true, false));
        assert_eq!(table[0xD], (true, true));
        assert_eq!(table[0xF], (true, false));
    }

    // --- Codepage::description tests ---

    #[test]
    fn test_codepage_description_all_variants() {
        assert_eq!(Codepage::ASCII.description(), "ASCII encoding");
        assert_eq!(
            Codepage::CP037.description(),
            "EBCDIC Code Page 037 (US/Canada)"
        );
        assert_eq!(
            Codepage::CP273.description(),
            "EBCDIC Code Page 273 (Germany/Austria)"
        );
        assert_eq!(
            Codepage::CP500.description(),
            "EBCDIC Code Page 500 (International)"
        );
        assert_eq!(
            Codepage::CP1047.description(),
            "EBCDIC Code Page 1047 (Open Systems)"
        );
        assert_eq!(
            Codepage::CP1140.description(),
            "EBCDIC Code Page 1140 (US/Canada with Euro)"
        );
    }

    // --- Codepage Display tests ---

    #[test]
    fn test_codepage_display_all_variants() {
        assert_eq!(format!("{}", Codepage::ASCII), "ascii");
        assert_eq!(format!("{}", Codepage::CP037), "cp037");
        assert_eq!(format!("{}", Codepage::CP273), "cp273");
        assert_eq!(format!("{}", Codepage::CP500), "cp500");
        assert_eq!(format!("{}", Codepage::CP1047), "cp1047");
        assert_eq!(format!("{}", Codepage::CP1140), "cp1140");
    }

    // --- Codepage FromStr tests ---

    #[test]
    fn test_codepage_from_str_all_valid_variants() {
        assert_eq!(<Codepage as std::str::FromStr>::from_str("ascii").unwrap(), Codepage::ASCII);
        assert_eq!(<Codepage as std::str::FromStr>::from_str("cp273").unwrap(), Codepage::CP273);
        assert_eq!(<Codepage as std::str::FromStr>::from_str("cp500").unwrap(), Codepage::CP500);
        assert_eq!(<Codepage as std::str::FromStr>::from_str("cp1047").unwrap(), Codepage::CP1047);
        assert_eq!(<Codepage as std::str::FromStr>::from_str("cp1140").unwrap(), Codepage::CP1140);
    }

    #[test]
    fn test_codepage_from_str_case_insensitive() {
        assert_eq!(<Codepage as std::str::FromStr>::from_str("ASCII").unwrap(), Codepage::ASCII);
        assert_eq!(<Codepage as std::str::FromStr>::from_str("CP273").unwrap(), Codepage::CP273);
        assert_eq!(<Codepage as std::str::FromStr>::from_str("Cp500").unwrap(), Codepage::CP500);
    }

    #[test]
    fn test_codepage_from_str_empty_string_defaults_to_cp037() {
        assert_eq!(<Codepage as std::str::FromStr>::from_str("").unwrap(), Codepage::CP037);
    }

    // --- Codepage is_ebcdic exhaustive ---

    #[test]
    fn test_codepage_is_ebcdic_all_variants() {
        assert!(!Codepage::ASCII.is_ebcdic());
        assert!(Codepage::CP037.is_ebcdic());
        assert!(Codepage::CP273.is_ebcdic());
        assert!(Codepage::CP500.is_ebcdic());
        assert!(Codepage::CP1047.is_ebcdic());
        assert!(Codepage::CP1140.is_ebcdic());
    }

    // --- Codepage code_page_number exhaustive ---

    #[test]
    fn test_codepage_code_page_number_all_variants() {
        assert_eq!(Codepage::ASCII.code_page_number(), None);
        assert_eq!(Codepage::CP037.code_page_number(), Some(37));
        assert_eq!(Codepage::CP273.code_page_number(), Some(273));
        assert_eq!(Codepage::CP500.code_page_number(), Some(500));
        assert_eq!(Codepage::CP1047.code_page_number(), Some(1047));
        assert_eq!(Codepage::CP1140.code_page_number(), Some(1140));
    }

    // --- UnmappablePolicy Display tests ---

    #[test]
    fn test_unmappable_policy_display_all_variants() {
        assert_eq!(format!("{}", UnmappablePolicy::Error), "error");
        assert_eq!(format!("{}", UnmappablePolicy::Replace), "replace");
        assert_eq!(format!("{}", UnmappablePolicy::Skip), "skip");
    }

    // --- UnmappablePolicy FromStr tests ---

    #[test]
    fn test_unmappable_policy_from_str_all_valid() {
        assert_eq!(
            <UnmappablePolicy as std::str::FromStr>::from_str("replace").unwrap(),
            UnmappablePolicy::Replace
        );
        assert_eq!(
            <UnmappablePolicy as std::str::FromStr>::from_str("skip").unwrap(),
            UnmappablePolicy::Skip
        );
        assert_eq!(
            <UnmappablePolicy as std::str::FromStr>::from_str("error").unwrap(),
            UnmappablePolicy::Error
        );
    }

    #[test]
    fn test_unmappable_policy_from_str_case_insensitive() {
        assert_eq!(
            <UnmappablePolicy as std::str::FromStr>::from_str("REPLACE").unwrap(),
            UnmappablePolicy::Replace
        );
        assert_eq!(
            <UnmappablePolicy as std::str::FromStr>::from_str("SKIP").unwrap(),
            UnmappablePolicy::Skip
        );
    }

    // --- Zoned sign table exhaustive ---

    #[test]
    fn test_get_zoned_sign_table_ebcdic_unsigned_nibbles() {
        let table = get_zoned_sign_table(Codepage::CP037);
        for i in 0..=0xB {
            assert_eq!(table[i], (false, false), "Expected unsigned at nibble 0x{i:X}");
        }
        assert_eq!(table[0xE], (false, false));
    }

    #[test]
    fn test_get_zoned_sign_table_all_ebcdic_codepages_same() {
        let cp037 = get_zoned_sign_table(Codepage::CP037);
        let cp273 = get_zoned_sign_table(Codepage::CP273);
        let cp500 = get_zoned_sign_table(Codepage::CP500);
        let cp1047 = get_zoned_sign_table(Codepage::CP1047);
        let cp1140 = get_zoned_sign_table(Codepage::CP1140);
        assert_eq!(cp037, cp273);
        assert_eq!(cp037, cp500);
        assert_eq!(cp037, cp1047);
        assert_eq!(cp037, cp1140);
    }

    // --- Serde round-trip ---

    #[test]
    fn test_codepage_serde_roundtrip() {
        let cp = Codepage::CP037;
        let json = serde_json::to_string(&cp).unwrap();
        let deserialized: Codepage = serde_json::from_str(&json).unwrap();
        assert_eq!(cp, deserialized);
    }

    #[test]
    fn test_unmappable_policy_serde_roundtrip() {
        let policy = UnmappablePolicy::Replace;
        let json = serde_json::to_string(&policy).unwrap();
        let deserialized: UnmappablePolicy = serde_json::from_str(&json).unwrap();
        assert_eq!(policy, deserialized);
    }
}
