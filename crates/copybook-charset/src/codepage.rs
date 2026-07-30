// SPDX-License-Identifier: AGPL-3.0-or-later
//! Codepage identity and character-conversion policy.
#![allow(clippy::missing_inline_in_public_items)]

use serde::{Deserialize, Serialize};
use std::{fmt, str::FromStr};

/// Character encoding specification supported by copybook-rs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Codepage {
    /// ASCII encoding.
    ASCII,
    /// EBCDIC Code Page 037 (US/Canada).
    CP037,
    /// EBCDIC Code Page 273 (Germany/Austria).
    CP273,
    /// EBCDIC Code Page 500 (International).
    CP500,
    /// EBCDIC Code Page 1047 (Open Systems).
    CP1047,
    /// EBCDIC Code Page 1140 (US/Canada with Euro).
    CP1140,
}

#[derive(Debug, Clone, Copy)]
struct CodepageMetadata {
    display_name: &'static str,
    description: &'static str,
    code_page_number: Option<u16>,
}

impl CodepageMetadata {
    const ASCII: Self = Self {
        display_name: "ascii",
        description: "ASCII encoding",
        code_page_number: None,
    };

    const fn ebcdic(
        display_name: &'static str,
        description: &'static str,
        code_page_number: u16,
    ) -> Self {
        Self {
            display_name,
            description,
            code_page_number: Some(code_page_number),
        }
    }
}

impl Codepage {
    const METADATA: [(Self, CodepageMetadata); 6] = [
        (Self::ASCII, CodepageMetadata::ASCII),
        (
            Self::CP037,
            CodepageMetadata::ebcdic("cp037", "EBCDIC Code Page 037 (US/Canada)", 37),
        ),
        (
            Self::CP273,
            CodepageMetadata::ebcdic("cp273", "EBCDIC Code Page 273 (Germany/Austria)", 273),
        ),
        (
            Self::CP500,
            CodepageMetadata::ebcdic("cp500", "EBCDIC Code Page 500 (International)", 500),
        ),
        (
            Self::CP1047,
            CodepageMetadata::ebcdic("cp1047", "EBCDIC Code Page 1047 (Open Systems)", 1047),
        ),
        (
            Self::CP1140,
            CodepageMetadata::ebcdic(
                "cp1140",
                "EBCDIC Code Page 1140 (US/Canada with Euro)",
                1140,
            ),
        ),
    ];

    /// Return all supported codepages in their canonical display order.
    #[must_use]
    pub const fn variants() -> &'static [Self; 6] {
        &[
            Self::ASCII,
            Self::CP037,
            Self::CP273,
            Self::CP500,
            Self::CP1047,
            Self::CP1140,
        ]
    }

    const fn metadata(self) -> CodepageMetadata {
        match self {
            Self::ASCII => Self::METADATA[0].1,
            Self::CP037 => Self::METADATA[1].1,
            Self::CP273 => Self::METADATA[2].1,
            Self::CP500 => Self::METADATA[3].1,
            Self::CP1047 => Self::METADATA[4].1,
            Self::CP1140 => Self::METADATA[5].1,
        }
    }

    /// Check if this is ASCII.
    #[must_use]
    pub const fn is_ascii(self) -> bool {
        self.metadata().code_page_number.is_none()
    }

    /// Check if this is an EBCDIC codepage.
    #[must_use]
    pub const fn is_ebcdic(self) -> bool {
        !self.is_ascii()
    }

    /// Return the numeric codepage identifier, if this is an EBCDIC page.
    #[must_use]
    pub const fn code_page_number(self) -> Option<u16> {
        self.metadata().code_page_number
    }

    /// Return a human-readable description.
    #[must_use]
    pub const fn description(self) -> &'static str {
        self.metadata().description
    }

    /// Return the canonical lower-case spelling.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        self.metadata().display_name
    }

    /// Parse a codepage and reject unknown values.
    ///
    /// # Errors
    ///
    /// Returns [`ParseCodepageError`] when `input` is not a supported
    /// codepage spelling.
    pub fn parse(input: &str) -> Result<Self, ParseCodepageError> {
        Self::METADATA
            .iter()
            .find_map(|(codepage, metadata)| {
                metadata
                    .display_name
                    .eq_ignore_ascii_case(input)
                    .then_some(*codepage)
            })
            .ok_or_else(|| ParseCodepageError {
                input: input.to_owned(),
            })
    }
}

impl fmt::Display for Codepage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for Codepage {
    type Err = ParseCodepageError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Self::parse(input)
    }
}

/// Error returned by strict codepage parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseCodepageError {
    input: String,
}

impl fmt::Display for ParseCodepageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unsupported codepage `{}`", self.input)
    }
}

impl std::error::Error for ParseCodepageError {}

/// Policy for handling characters that have no mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnmappablePolicy {
    /// Return an error.
    Error,
    /// Replace with U+FFFD during decode.
    Replace,
    /// Skip the character during decode.
    Skip,
}

impl UnmappablePolicy {
    /// Return the canonical lower-case spelling.
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Replace => "replace",
            Self::Skip => "skip",
        }
    }

    /// Parse a policy and reject unknown values.
    ///
    /// # Errors
    ///
    /// Returns [`ParseUnmappablePolicyError`] when `input` is not a supported
    /// policy spelling.
    pub fn parse(input: &str) -> Result<Self, ParseUnmappablePolicyError> {
        match input.to_ascii_lowercase().as_str() {
            "error" => Ok(Self::Error),
            "replace" => Ok(Self::Replace),
            "skip" => Ok(Self::Skip),
            _ => Err(ParseUnmappablePolicyError {
                input: input.to_owned(),
            }),
        }
    }
}

impl fmt::Display for UnmappablePolicy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for UnmappablePolicy {
    type Err = ParseUnmappablePolicyError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Self::parse(input)
    }
}

/// Error returned by strict unmappable-policy parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseUnmappablePolicyError {
    input: String,
}

impl fmt::Display for ParseUnmappablePolicyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "unsupported unmappable-character policy `{}`",
            self.input
        )
    }
}

impl std::error::Error for ParseUnmappablePolicyError {}

/// Return the padding byte for a codepage.
#[must_use]
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
mod tests {
    use super::*;

    #[test]
    fn parsing_codepage_metadata_and_validation() {
        assert_eq!(
            Codepage::CP037.description(),
            "EBCDIC Code Page 037 (US/Canada)"
        );
        assert_eq!(Codepage::parse("CP1140"), Ok(Codepage::CP1140));
        assert!(Codepage::parse("unknown").is_err());
        assert_eq!(
            UnmappablePolicy::parse("REPLACE"),
            Ok(UnmappablePolicy::Replace)
        );
        assert!(UnmappablePolicy::parse("unknown").is_err());
        assert_eq!("cp037".parse::<Codepage>(), Ok(Codepage::CP037));
        assert_eq!(
            "replace".parse::<UnmappablePolicy>(),
            Ok(UnmappablePolicy::Replace)
        );
        assert_eq!(
            "unknown".parse::<Codepage>(),
            Err(ParseCodepageError {
                input: "unknown".to_owned(),
            })
        );
        assert_eq!(
            "unknown".parse::<UnmappablePolicy>(),
            Err(ParseUnmappablePolicyError {
                input: "unknown".to_owned(),
            })
        );
    }

    #[test]
    fn parsing_codepage_padding_bytes() {
        assert_eq!(space_byte(Codepage::ASCII), 0x20);
        assert_eq!(space_byte(Codepage::CP037), 0x40);
    }
}
