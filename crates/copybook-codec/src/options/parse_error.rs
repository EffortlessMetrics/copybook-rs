// SPDX-License-Identifier: AGPL-3.0-or-later
//! Typed errors for parsing codec option values.

use std::fmt;

const FLOAT_FORMAT_SPELLINGS: &[&str] = &["ieee-be", "ieee", "ieee-big-endian", "ibm-hex", "ibm"];
const RECORD_FORMAT_SPELLINGS: &[&str] = &["fixed", "rdw", "vb", "text"];
const JSON_NUMBER_MODE_SPELLINGS: &[&str] = &["lossless", "native"];
const RAW_MODE_SPELLINGS: &[&str] = &["off", "record", "field", "record+rdw"];
const TEXT_TERMINATOR_SPELLINGS: &[&str] = &["lf", "\\n", "unix", "crlf", "\\r\\n", "windows"];

/// Codec option family whose textual value could not be parsed.
///
/// This discriminator is intentionally separate from the stable `CBK*` runtime
/// error taxonomy: parsing a configuration token happens before a codec
/// operation and does not identify a record-processing failure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum CodecOptionKind {
    /// Floating-point representation selected by [`super::FloatFormat`].
    FloatFormat,
    /// Record framing selected by [`super::RecordFormat`].
    RecordFormat,
    /// Text line terminator selected by [`super::TextTerminator`].
    TextTerminator,
    /// JSON number representation selected by [`super::JsonNumberMode`].
    JsonNumberMode,
    /// Raw-data capture selected by [`super::RawMode`].
    RawMode,
}

impl CodecOptionKind {
    /// Return every accepted spelling for this option family.
    #[must_use]
    #[inline]
    pub const fn accepted_spellings(self) -> &'static [&'static str] {
        match self {
            Self::FloatFormat => FLOAT_FORMAT_SPELLINGS,
            Self::RecordFormat => RECORD_FORMAT_SPELLINGS,
            Self::JsonNumberMode => JSON_NUMBER_MODE_SPELLINGS,
            Self::RawMode => RAW_MODE_SPELLINGS,
            Self::TextTerminator => TEXT_TERMINATOR_SPELLINGS,
        }
    }

    #[inline]
    const fn display_name(self) -> &'static str {
        match self {
            Self::FloatFormat => "float format",
            Self::RecordFormat => "record format",
            Self::JsonNumberMode => "JSON number mode",
            Self::RawMode => "raw mode",
            Self::TextTerminator => "text terminator",
        }
    }
}

/// Error returned when a textual codec option is unsupported.
///
/// The original input is retained verbatim for diagnostics. Use [`Self::kind`]
/// and [`Self::accepted_spellings`] for programmatic recovery. Serde errors
/// remain Serde errors; this type is the `FromStr` contract only.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseCodecOptionError {
    kind: CodecOptionKind,
    input: String,
}

impl ParseCodecOptionError {
    #[inline]
    pub(super) fn new(kind: CodecOptionKind, input: &str) -> Self {
        Self {
            kind,
            input: input.to_owned(),
        }
    }

    /// Return the option family that rejected the input.
    #[must_use]
    #[inline]
    pub const fn kind(&self) -> CodecOptionKind {
        self.kind
    }

    /// Return the original, unnormalized input.
    #[must_use]
    #[inline]
    pub fn input(&self) -> &str {
        &self.input
    }

    /// Return every spelling accepted by the rejected option family.
    #[must_use]
    #[inline]
    pub const fn accepted_spellings(&self) -> &'static [&'static str] {
        self.kind.accepted_spellings()
    }
}

impl fmt::Display for ParseCodecOptionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "unsupported {} `{}`",
            self.kind.display_name(),
            self.input
        )
    }
}

impl std::error::Error for ParseCodecOptionError {}
