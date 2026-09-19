// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::missing_inline_in_public_items)]
//! Versioned interpretation profile: reviewed intent for undecided bytes.
//!
//! A profile records what a developer decided about an unfamiliar copybook
//! and byte stream: which dialect the source follows, which framing and
//! codepage the bytes use, and which bounds govern the run. Doctor proposes
//! a profile (slice 4); commands consume it (slices 3 and 5). This module
//! owns only the domain: parsing, validation, canonical form, and the
//! fingerprint that later slices pin into receipts and manifests.
//!
//! Wire form is lowercase TOML (``kind = "rdw"``, ``codepage = "cp037"``,
//! ``dialect = "normative"``) independent of Rust variant spellings, so the
//! file stays reviewable without reading source. Unknown keys are rejected:
//! a profile that silently ignores a misspelled option is worse than none.
//!
//! # Examples
//!
//! ```rust
//! use copybook_codec::options::profile::InterpretationProfile;
//!
//! let profile = InterpretationProfile::parse(
//!     "schema_version = 1\n[source]\ndialect = \"normative\"\n[framing]\nkind = \"rdw\"\nreserved_bytes = \"lenient\"\n[decode]\ncodepage = \"cp037\"\nunmappable = \"error\"\njson_numbers = \"lossless\"\n[limits]\nmaximum_record_length = 32760\nmaximum_errors = 100\n",
//! )
//! .expect("valid profile");
//! assert_eq!(profile.fingerprint().len(), 64);
//! ```

use std::fmt;
use std::str::FromStr;

use super::{JsonNumberMode, RecordFormat};
use copybook_charset::{Codepage, UnmappablePolicy};
use copybook_core::dialect::Dialect;
use serde::{Deserialize, Deserializer, Serialize, Serializer, de as serde_de};
use sha2::{Digest, Sha256};

/// Profile schema version this crate reads and writes.
pub const PROFILE_SCHEMA_VERSION: u32 = 1;

/// Largest accepted `maximum_record_length`: 16 MiB per record bounds decoder
/// memory independent of file size.
pub const MAX_PROFILE_RECORD_LENGTH: u64 = 16_777_216;

/// Largest accepted `maximum_errors`: runs stop long before error storage
/// matters, but the bound must still be explicit.
pub const MAX_PROFILE_ERRORS: u64 = 1_000_000;

/// Default record-length bound (RDW architectural maximum).
pub const DEFAULT_PROFILE_RECORD_LENGTH: u64 = 32760;

/// Default error budget.
pub const DEFAULT_PROFILE_ERRORS: u64 = 100;

/// Reviewed interpretation intent for one copybook and byte stream.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InterpretationProfile {
    /// Profile schema version; must equal [`PROFILE_SCHEMA_VERSION`].
    pub schema_version: u32,
    /// Copybook source interpretation.
    pub source: SourceSection,
    /// Physical framing interpretation.
    pub framing: FramingSection,
    /// Byte decoding interpretation.
    pub decode: DecodeSection,
    /// Explicit run bounds.
    pub limits: LimitsSection,
}

/// Copybook source interpretation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceSection {
    /// ODO `min_count` interpretation.
    pub dialect: SourceDialect,
}

/// Physical framing interpretation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FramingSection {
    /// Record framing kind.
    pub kind: FramingKind,
    /// Reserved-bytes policy. Lenient warns and continues on non-zero
    /// framing reserved bytes; strict fails with `CBKR211`/`CBKF225`
    /// without enabling full strict record handling.
    pub reserved_bytes: ReservedPolicy,
}

/// Byte decoding interpretation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DecodeSection {
    /// Character encoding, lowercase (`cp037`).
    #[serde(
        deserialize_with = "display_from_str",
        serialize_with = "display_to_string"
    )]
    pub codepage: Codepage,
    /// Unmappable-character policy, lowercase (`error`).
    #[serde(
        deserialize_with = "display_from_str",
        serialize_with = "display_to_string"
    )]
    pub unmappable: UnmappablePolicy,
    /// JSON number representation, lowercase (`lossless`).
    #[serde(
        deserialize_with = "display_from_str",
        serialize_with = "display_to_string"
    )]
    pub json_numbers: JsonNumberMode,
}

/// Explicit run bounds.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LimitsSection {
    /// Largest record accepted, in bytes.
    pub maximum_record_length: u64,
    /// Failures tolerated before the run stops.
    pub maximum_errors: u64,
}

/// ODO `min_count` interpretation in profile spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceDialect {
    /// Enforce the declared minimum.
    Normative,
    /// Ignore the declared minimum.
    ZeroTolerant,
    /// Clamp the effective minimum to at least one.
    OneTolerant,
}

/// Record framing kind in profile spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FramingKind {
    /// Fixed-length records.
    Fixed,
    /// RDW variable-length records.
    Rdw,
    /// Variable-blocked records (beta).
    Vb,
}

/// Reserved-bytes policy in profile spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReservedPolicy {
    /// Reserved bytes must be zero (enforced from slice 3).
    Strict,
    /// Reserved bytes accepted as-is (current decoder behavior).
    Lenient,
}

/// Failure to parse or validate a profile document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProfileError {
    /// TOML syntax or type error, with the underlying message.
    InvalidToml(String),
    /// `schema_version` this crate cannot read.
    UnsupportedVersion {
        /// Version found in the document.
        found: u32,
    },
    /// A limit outside its documented bound.
    LimitOutOfRange {
        /// Dotted field path (`limits.maximum_record_length`).
        field: &'static str,
        /// Value found in the document.
        value: u64,
    },
}

impl fmt::Display for ProfileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidToml(message) => write!(f, "invalid profile: {message}"),
            Self::UnsupportedVersion { found } => write!(
                f,
                "unsupported profile schema_version {found}; this tool reads version {PROFILE_SCHEMA_VERSION}"
            ),
            Self::LimitOutOfRange { field, value } => {
                write!(
                    f,
                    "profile {field} value {value} is outside its documented bound"
                )
            }
        }
    }
}

impl std::error::Error for ProfileError {}

impl InterpretationProfile {
    /// Parse and validate a profile document.
    ///
    /// # Errors
    ///
    /// Returns [`ProfileError`] when the TOML is malformed, carries unknown
    /// keys or values, declares an unreadable schema version, or violates a
    /// documented limit bound.
    pub fn parse(text: &str) -> Result<Self, ProfileError> {
        let profile: Self =
            toml::from_str(text).map_err(|error| ProfileError::InvalidToml(error.to_string()))?;
        profile.validate()?;
        Ok(profile)
    }

    /// Check schema version and limit bounds.
    ///
    /// # Errors
    ///
    /// Returns [`ProfileError::UnsupportedVersion`] or
    /// [`ProfileError::LimitOutOfRange`] on violation.
    pub fn validate(&self) -> Result<(), ProfileError> {
        if self.schema_version != PROFILE_SCHEMA_VERSION {
            return Err(ProfileError::UnsupportedVersion {
                found: self.schema_version,
            });
        }
        if self.limits.maximum_record_length == 0
            || self.limits.maximum_record_length > MAX_PROFILE_RECORD_LENGTH
        {
            return Err(ProfileError::LimitOutOfRange {
                field: "limits.maximum_record_length",
                value: self.limits.maximum_record_length,
            });
        }
        if self.limits.maximum_errors > MAX_PROFILE_ERRORS {
            return Err(ProfileError::LimitOutOfRange {
                field: "limits.maximum_errors",
                value: self.limits.maximum_errors,
            });
        }
        Ok(())
    }

    /// Canonical TOML rendering: field order follows the struct definition,
    /// so identical profiles render byte-identical text.
    ///
    /// # Errors
    ///
    /// Returns [`ProfileError::InvalidToml`] when serialization fails; the
    /// current field types cannot fail, so this is defensive.
    pub fn to_canonical_toml(&self) -> Result<String, ProfileError> {
        toml::to_string(self).map_err(|error| ProfileError::InvalidToml(error.to_string()))
    }

    /// SHA-256 fingerprint (lowercase hex) over the canonical TOML rendering.
    /// Later slices pin this into receipts and manifests.
    #[must_use]
    pub fn fingerprint(&self) -> String {
        let canonical = self.to_canonical_toml().unwrap_or_else(|_| String::new());
        let mut hasher = Sha256::new();
        hasher.update(canonical.as_bytes());
        hex::encode(hasher.finalize())
    }

    /// Product defaults: fixed framing, CP037, lossless numbers,
    /// error-on-unmappable, normative dialect, lenient reserved bytes,
    /// and the documented default limits.
    #[must_use]
    pub fn product_defaults() -> Self {
        Self {
            schema_version: PROFILE_SCHEMA_VERSION,
            source: SourceSection {
                dialect: SourceDialect::Normative,
            },
            framing: FramingSection {
                kind: FramingKind::Fixed,
                reserved_bytes: ReservedPolicy::Lenient,
            },
            decode: DecodeSection {
                codepage: Codepage::CP037,
                unmappable: UnmappablePolicy::Error,
                json_numbers: JsonNumberMode::Lossless,
            },
            limits: LimitsSection {
                maximum_record_length: DEFAULT_PROFILE_RECORD_LENGTH,
                maximum_errors: DEFAULT_PROFILE_ERRORS,
            },
        }
    }
}

impl From<SourceDialect> for Dialect {
    fn from(value: SourceDialect) -> Self {
        match value {
            SourceDialect::Normative => Self::Normative,
            SourceDialect::ZeroTolerant => Self::ZeroTolerant,
            SourceDialect::OneTolerant => Self::OneTolerant,
        }
    }
}

impl From<FramingKind> for RecordFormat {
    fn from(value: FramingKind) -> Self {
        match value {
            FramingKind::Fixed => Self::Fixed,
            FramingKind::Rdw => Self::RDW,
            FramingKind::Vb => Self::Vb,
        }
    }
}

impl From<RecordFormat> for FramingKind {
    fn from(value: RecordFormat) -> Self {
        match value {
            RecordFormat::Fixed => Self::Fixed,
            RecordFormat::RDW => Self::Rdw,
            RecordFormat::Vb => Self::Vb,
        }
    }
}

impl From<Dialect> for SourceDialect {
    fn from(value: Dialect) -> Self {
        match value {
            Dialect::Normative => Self::Normative,
            Dialect::ZeroTolerant => Self::ZeroTolerant,
            Dialect::OneTolerant => Self::OneTolerant,
        }
    }
}

impl fmt::Display for SourceDialect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Normative => write!(f, "normative"),
            Self::ZeroTolerant => write!(f, "zero-tolerant"),
            Self::OneTolerant => write!(f, "one-tolerant"),
        }
    }
}

impl FromStr for SourceDialect {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "normative" => Ok(Self::Normative),
            "zero-tolerant" => Ok(Self::ZeroTolerant),
            "one-tolerant" => Ok(Self::OneTolerant),
            _ => Err(format!(
                "unsupported dialect `{input}`; expected normative, zero-tolerant, or one-tolerant"
            )),
        }
    }
}

impl fmt::Display for FramingKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fixed => write!(f, "fixed"),
            Self::Rdw => write!(f, "rdw"),
            Self::Vb => write!(f, "vb"),
        }
    }
}

impl FromStr for FramingKind {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "fixed" => Ok(Self::Fixed),
            "rdw" => Ok(Self::Rdw),
            "vb" => Ok(Self::Vb),
            _ => Err(format!(
                "unsupported framing kind `{input}`; expected fixed, rdw, or vb"
            )),
        }
    }
}

impl fmt::Display for ReservedPolicy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Strict => write!(f, "strict"),
            Self::Lenient => write!(f, "lenient"),
        }
    }
}

impl FromStr for ReservedPolicy {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "strict" => Ok(Self::Strict),
            "lenient" => Ok(Self::Lenient),
            _ => Err(format!(
                "unsupported reserved_bytes policy `{input}`; expected strict or lenient"
            )),
        }
    }
}

impl<'de> Deserialize<'de> for SourceDialect {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        display_from_str(deserializer)
    }
}

impl Serialize for SourceDialect {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        display_to_string(self, serializer)
    }
}

impl<'de> Deserialize<'de> for FramingKind {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        display_from_str(deserializer)
    }
}

impl Serialize for FramingKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        display_to_string(self, serializer)
    }
}

impl<'de> Deserialize<'de> for ReservedPolicy {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        display_from_str(deserializer)
    }
}

impl Serialize for ReservedPolicy {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        display_to_string(self, serializer)
    }
}

/// Deserialize a string field through [`FromStr`], reporting the value and
/// the parse error instead of a bare type mismatch.
fn display_from_str<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: FromStr,
    T::Err: fmt::Display,
{
    let text = String::deserialize(deserializer)?;
    text.parse::<T>()
        .map_err(|error| serde_de::Error::custom(format!("{error} (in `{text}`)")))
}

/// Serialize through [`fmt::Display`] so the wire form stays lowercase and
/// reviewable independent of Rust variant spellings.
fn display_to_string<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: fmt::Display + ?Sized,
    S: Serializer,
{
    serializer.serialize_str(&value.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    const VALID: &str = "schema_version = 1\n[source]\ndialect = \"normative\"\n[framing]\nkind = \"rdw\"\nreserved_bytes = \"lenient\"\n[decode]\ncodepage = \"cp037\"\nunmappable = \"error\"\njson_numbers = \"lossless\"\n[limits]\nmaximum_record_length = 32760\nmaximum_errors = 100\n";

    #[test]
    fn valid_profile_parses_and_round_trips() {
        let profile = InterpretationProfile::parse(VALID).expect("valid profile");
        assert_eq!(profile.schema_version, PROFILE_SCHEMA_VERSION);
        assert_eq!(profile.framing.kind, FramingKind::Rdw);
        assert_eq!(profile.decode.codepage, Codepage::CP037);
        let canonical = profile.to_canonical_toml().expect("canonical form");
        let again = InterpretationProfile::parse(&canonical).expect("reparse");
        assert_eq!(profile, again);
        assert_eq!(profile.fingerprint(), again.fingerprint());
    }

    #[test]
    fn unknown_keys_are_rejected() {
        let text = VALID.replace("[limits]", "[limits]\nextra_key = 1");
        let error = InterpretationProfile::parse(&text).expect_err("unknown key");
        assert!(matches!(error, ProfileError::InvalidToml(_)));
    }

    #[test]
    fn unknown_values_name_the_value() {
        let text = VALID.replace("kind = \"rdw\"", "kind = \"tape\"");
        let error = InterpretationProfile::parse(&text).expect_err("unknown kind");
        let message = error.to_string();
        assert!(message.contains("tape"), "unexpected message: {message}");
    }

    #[test]
    fn wrong_schema_version_is_rejected() {
        let text = VALID.replace("schema_version = 1", "schema_version = 2");
        let error = InterpretationProfile::parse(&text).expect_err("version 2");
        assert_eq!(error, ProfileError::UnsupportedVersion { found: 2 });
    }

    #[test]
    fn out_of_range_limits_are_rejected() {
        for (field, value) in [
            ("maximum_record_length", 0),
            ("maximum_record_length", MAX_PROFILE_RECORD_LENGTH + 1),
            ("maximum_errors", MAX_PROFILE_ERRORS + 1),
        ] {
            let replaced = replace_limit(VALID, field, value);
            let error = InterpretationProfile::parse(&replaced).expect_err("range");
            assert!(
                matches!(error, ProfileError::LimitOutOfRange { .. }),
                "unexpected error for {field}={value}: {error}"
            );
        }
    }

    #[test]
    fn fingerprint_is_deterministic_and_content_sensitive() {
        let first = InterpretationProfile::parse(VALID).expect("valid");
        let second = InterpretationProfile::parse(VALID).expect("valid");
        assert_eq!(first.fingerprint(), second.fingerprint());
        assert_eq!(first.fingerprint().len(), 64);
        let changed = replace_limit(VALID, "maximum_errors", 99);
        let other = InterpretationProfile::parse(&changed).expect("valid");
        assert_ne!(first.fingerprint(), other.fingerprint());
    }

    #[test]
    fn dialect_mapping_covers_all_variants() {
        assert_eq!(Dialect::from(SourceDialect::Normative), Dialect::Normative);
        assert_eq!(
            Dialect::from(SourceDialect::ZeroTolerant),
            Dialect::ZeroTolerant
        );
        assert_eq!(
            Dialect::from(SourceDialect::OneTolerant),
            Dialect::OneTolerant
        );
    }

    #[test]
    fn framing_mapping_covers_all_kinds() {
        assert_eq!(RecordFormat::from(FramingKind::Fixed), RecordFormat::Fixed);
        assert_eq!(RecordFormat::from(FramingKind::Rdw), RecordFormat::RDW);
        assert_eq!(RecordFormat::from(FramingKind::Vb), RecordFormat::Vb);
    }

    #[test]
    fn product_defaults_match_current_behavior() {
        let defaults = InterpretationProfile::product_defaults();
        assert_eq!(defaults.decode.codepage, Codepage::CP037);
        assert_eq!(defaults.decode.unmappable, UnmappablePolicy::Error);
        assert_eq!(defaults.decode.json_numbers, JsonNumberMode::Lossless);
        assert_eq!(defaults.framing.kind, FramingKind::Fixed);
        assert_eq!(defaults.source.dialect, SourceDialect::Normative);
        defaults.validate().expect("defaults validate");
    }

    fn replace_limit(document: &str, field: &str, value: u64) -> String {
        document
            .lines()
            .map(|line| {
                if line.starts_with(&format!("{field} = ")) {
                    format!("{field} = {value}")
                } else {
                    line.to_owned()
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
            + "\n"
    }
}
