# SPDX-License-Identifier: AGPL-3.0-or-later
from pathlib import Path


def replace_once(path: Path, old: str, new: str) -> None:
    text = path.read_text(encoding="utf-8")
    count = text.count(old)
    if count != 1:
        raise SystemExit(
            f"{path}: expected one marker, found {count}: {old[:80]!r}"
        )
    path.write_text(text.replace(old, new, 1), encoding="utf-8")


parse_error = Path("crates/copybook-codec/src/options/parse_error.rs")
parse_error.parent.mkdir(parents=True, exist_ok=True)
parse_error.write_text(
    r'''// SPDX-License-Identifier: AGPL-3.0-or-later
//! Typed errors for parsing codec option values.

use std::fmt;

const FLOAT_FORMAT_SPELLINGS: &[&str] =
    &["ieee-be", "ieee", "ieee-big-endian", "ibm-hex", "ibm"];
const RECORD_FORMAT_SPELLINGS: &[&str] = &["fixed", "rdw"];
const JSON_NUMBER_MODE_SPELLINGS: &[&str] = &["lossless", "native"];
const RAW_MODE_SPELLINGS: &[&str] = &["off", "record", "field", "record+rdw"];

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
        }
    }

    #[inline]
    const fn display_name(self) -> &'static str {
        match self {
            Self::FloatFormat => "float format",
            Self::RecordFormat => "record format",
            Self::JsonNumberMode => "JSON number mode",
            Self::RawMode => "raw mode",
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
''',
    encoding="utf-8",
)

options = Path("crates/copybook-codec/src/options.rs")
replace_once(
    options,
    '#![allow(clippy::missing_inline_in_public_items)]\n\n',
    '#![allow(clippy::missing_inline_in_public_items)]\n\n'
    'mod parse_error;\n'
    'pub use parse_error::{CodecOptionKind, ParseCodecOptionError};\n\n',
)
replace_once(
    options,
    'impl FromStr for FloatFormat {\n    type Err = String;\n',
    'impl FromStr for FloatFormat {\n    type Err = ParseCodecOptionError;\n',
)
replace_once(
    options,
    '            _ => Err(format!("unsupported float format `{input}`")),\n',
    '            _ => Err(ParseCodecOptionError::new(\n'
    '                CodecOptionKind::FloatFormat,\n'
    '                input,\n'
    '            )),\n',
)
replace_once(
    options,
    'impl FromStr for RecordFormat {\n    type Err = String;\n',
    'impl FromStr for RecordFormat {\n    type Err = ParseCodecOptionError;\n',
)
replace_once(
    options,
    '            _ => Err(format!("unsupported record format `{input}`")),\n',
    '            _ => Err(ParseCodecOptionError::new(\n'
    '                CodecOptionKind::RecordFormat,\n'
    '                input,\n'
    '            )),\n',
)
replace_once(
    options,
    'impl FromStr for JsonNumberMode {\n    type Err = String;\n',
    'impl FromStr for JsonNumberMode {\n    type Err = ParseCodecOptionError;\n',
)
replace_once(
    options,
    '            _ => Err(format!("unsupported JSON number mode `{input}`")),\n',
    '            _ => Err(ParseCodecOptionError::new(\n'
    '                CodecOptionKind::JsonNumberMode,\n'
    '                input,\n'
    '            )),\n',
)
replace_once(
    options,
    'impl FromStr for RawMode {\n    type Err = String;\n',
    'impl FromStr for RawMode {\n    type Err = ParseCodecOptionError;\n',
)
replace_once(
    options,
    '            _ => Err(format!("unsupported raw mode `{input}`")),\n',
    '            _ => Err(ParseCodecOptionError::new(\n'
    '                CodecOptionKind::RawMode,\n'
    '                input,\n'
    '            )),\n',
)

lib_rs = Path("crates/copybook-codec/src/lib.rs")
replace_once(
    lib_rs,
    'pub use options::{\n'
    '    Codepage, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,\n'
    '    UnmappablePolicy, ZonedEncodingFormat,\n'
    '};\n',
    'pub use options::{\n'
    '    Codepage, CodecOptionKind, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode,\n'
    '    ParseCodecOptionError, RawMode, RecordFormat, UnmappablePolicy, ZonedEncodingFormat,\n'
    '};\n',
)

Path("crates/copybook-codec/tests/option_parse_errors.rs").write_text(
    r'''// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    CodecOptionKind, FloatFormat, JsonNumberMode, ParseCodecOptionError, RawMode, RecordFormat,
};
use std::{error::Error as _, fmt::Debug, str::FromStr};

fn assert_parse_error<T>(
    input: &str,
    kind: CodecOptionKind,
    expected_message: &str,
) -> ParseCodecOptionError
where
    T: FromStr<Err = ParseCodecOptionError> + Debug,
{
    let error = T::from_str(input).expect_err("input should be rejected");
    assert_eq!(error.kind(), kind);
    assert_eq!(error.input(), input);
    assert_eq!(error.accepted_spellings(), kind.accepted_spellings());
    assert_eq!(error.to_string(), expected_message);
    assert!(error.source().is_none());
    error
}

#[test]
fn invalid_values_return_one_inspectable_typed_error() {
    assert_parse_error::<FloatFormat>(
        "Binary128",
        CodecOptionKind::FloatFormat,
        "unsupported float format `Binary128`",
    );
    assert_parse_error::<RecordFormat>(
        "vb",
        CodecOptionKind::RecordFormat,
        "unsupported record format `vb`",
    );
    assert_parse_error::<JsonNumberMode>(
        "decimal",
        CodecOptionKind::JsonNumberMode,
        "unsupported JSON number mode `decimal`",
    );
    assert_parse_error::<RawMode>(
        "header",
        CodecOptionKind::RawMode,
        "unsupported raw mode `header`",
    );
}

#[test]
fn accepted_spellings_are_complete_and_still_case_insensitive() {
    assert_eq!(
        CodecOptionKind::FloatFormat.accepted_spellings(),
        &["ieee-be", "ieee", "ieee-big-endian", "ibm-hex", "ibm"]
    );
    assert_eq!(
        CodecOptionKind::RecordFormat.accepted_spellings(),
        &["fixed", "rdw"]
    );
    assert_eq!(
        CodecOptionKind::JsonNumberMode.accepted_spellings(),
        &["lossless", "native"]
    );
    assert_eq!(
        CodecOptionKind::RawMode.accepted_spellings(),
        &["off", "record", "field", "record+rdw"]
    );

    for spelling in CodecOptionKind::FloatFormat.accepted_spellings() {
        assert!(FloatFormat::from_str(spelling).is_ok(), "{spelling}");
    }
    for spelling in CodecOptionKind::RecordFormat.accepted_spellings() {
        assert!(RecordFormat::from_str(spelling).is_ok(), "{spelling}");
    }
    for spelling in CodecOptionKind::JsonNumberMode.accepted_spellings() {
        assert!(JsonNumberMode::from_str(spelling).is_ok(), "{spelling}");
    }
    for spelling in CodecOptionKind::RawMode.accepted_spellings() {
        assert!(RawMode::from_str(spelling).is_ok(), "{spelling}");
    }

    assert_eq!(
        FloatFormat::from_str("IEEE-BIG-ENDIAN"),
        Ok(FloatFormat::IeeeBigEndian)
    );
    assert_eq!(RecordFormat::from_str("RDW"), Ok(RecordFormat::RDW));
    assert_eq!(
        JsonNumberMode::from_str("NATIVE"),
        Ok(JsonNumberMode::Native)
    );
    assert_eq!(RawMode::from_str("RECORD+RDW"), Ok(RawMode::RecordRDW));
}
''',
    encoding="utf-8",
)

Path("crates/copybook-options/tests/parse_error_compat.rs").write_text(
    r'''// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_options::{CodecOptionKind, ParseCodecOptionError, RecordFormat};
use std::str::FromStr;

#[test]
fn compatibility_package_forwards_typed_parse_contract() {
    let error: ParseCodecOptionError =
        RecordFormat::from_str("variable").expect_err("unsupported format should fail");

    assert_eq!(error.kind(), CodecOptionKind::RecordFormat);
    assert_eq!(error.input(), "variable");
    assert_eq!(error.accepted_spellings(), &["fixed", "rdw"]);
    assert_eq!(
        error.to_string(),
        "unsupported record format `variable`"
    );
}
''',
    encoding="utf-8",
)

migration = Path("docs/MIGRATION_GUIDE.md")
replace_once(
    migration,
    "Guide for migrating from other COBOL data processing tools to copybook-rs.\n\n",
    """Guide for migrating from other COBOL data processing tools to copybook-rs.

## Upcoming 0.6 library API changes

### Typed codec-option parse errors

`FloatFormat`, `RecordFormat`, `JsonNumberMode`, and `RawMode` now implement
`FromStr<Err = ParseCodecOptionError>` instead of returning an unstructured
`String`. Existing accepted spellings and human-readable messages are unchanged.
Code that only displays the error can continue to call `to_string()`; code that
annotated or matched the old `String` type must accept the typed error instead.

The error exposes `kind()`, `input()`, and `accepted_spellings()` for recovery.
It deliberately has no `CBK*` code because textual option conversion happens
before a codec operation; Serde deserialization continues to report Serde's own
error type. The forwarding `copybook-options` package exports the same contract.

""",
)

changelog = Path("CHANGELOG.md")
replace_once(
    changelog,
    "## [Unreleased]\n\n### Added\n",
    """## [Unreleased]

### Changed
- **codec**: `FloatFormat`, `RecordFormat`, `JsonNumberMode`, and `RawMode` now
  return the inspectable `ParseCodecOptionError` from `FromStr` rather than an
  unstructured `String`. The error retains the original input and exposes the
  option family and accepted spellings; accepted values and display text are
  unchanged. This conversion-layer contract is separate from runtime `CBK*`
  errors and from Serde deserialization failures.

### Added
""",
)
