// SPDX-License-Identifier: AGPL-3.0-or-later
//! Interpretation-profile input resolution for `decode` and `verify`.
//!
//! A reviewed profile (`--profile`) supplies the framing and decode
//! interpretation a run must use. Command flags, the profile, the ambient
//! environment, and product defaults form four layers; exactly one value
//! per field wins, and a flag that disagrees with the profile is a
//! contradiction, not a precedence decision. See
//! [`copybook::codec::options::resolve`] for the layer ordering.

use crate::cli_config::DialectPreference;
use copybook::codec::options::profile::{InterpretationProfile, ReservedPolicy};
use copybook::codec::options::resolve::{ConflictError, resolve_field};
use copybook::codec::{Codepage, JsonNumberMode, RecordFormat, UnmappablePolicy};
use copybook::core::dialect::Dialect;
use std::path::Path;

/// Profile keys a flag conflict names. The corresponding command flag is the
/// same concept in flag spelling (`framing.kind` is `--format`, `decode.codepage`
/// is `--codepage`, `decode.json_numbers` is `--json-number`,
/// `decode.unmappable` is `--on-decode-unmappable`, `source.dialect` is
/// `--dialect`, `limits.maximum_errors` is `--max-errors`); the full mapping
/// lives in `docs/CLI_REFERENCE.md`.
const FORMAT_PROFILE_KEY: &str = "framing.kind";
const CODEPAGE_PROFILE_KEY: &str = "decode.codepage";
const JSON_NUMBERS_PROFILE_KEY: &str = "decode.json_numbers";
const UNMAPPABLE_PROFILE_KEY: &str = "decode.unmappable";
const DIALECT_PROFILE_KEY: &str = "source.dialect";
const MAX_ERRORS_PROFILE_KEY: &str = "limits.maximum_errors";

/// Effective framing and decode inputs shared by `decode` and `verify`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ResolvedCommonInputs {
    /// Effective record framing.
    pub format: RecordFormat,
    /// Effective character encoding.
    pub codepage: Codepage,
    /// Effective ODO `min_count` interpretation.
    pub dialect: Dialect,
    /// Whether framing reserved bytes are a hard error.
    pub strict_reserved_bytes: bool,
    /// Effective error budget (`None` means unlimited).
    pub max_errors: Option<u64>,
}

/// Effective decode-only inputs covered by the profile.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ResolvedDecodeInputs {
    /// Effective JSON number representation.
    pub json_number: JsonNumberMode,
    /// Effective unmappable-character policy.
    pub unmappable: UnmappablePolicy,
}

/// A `--profile` failure: unreadable/invalid document, a flag conflict, or a
/// missing required value. Dispatch renders this as structured diagnostics
/// with an `Encode` (validation) exit, never as an internal error.
#[derive(Debug)]
pub(crate) enum ProfileInputError {
    /// The profile file cannot be read.
    Unreadable { path: String, message: String },
    /// The profile file is not valid TOML or fails validation.
    Invalid { path: String, message: String },
    /// An explicit flag disagrees with the profile value.
    Conflict(ConflictError),
    /// A required value is missing and no layer supplies it.
    Missing { message: String },
}

impl std::fmt::Display for ProfileInputError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unreadable { path, message } => {
                write!(formatter, "cannot read profile {path}: {message}")
            }
            Self::Invalid { path, message } => {
                write!(formatter, "invalid profile {path}: {message}")
            }
            Self::Conflict(conflict) => write!(formatter, "{conflict}"),
            Self::Missing { message, .. } => write!(formatter, "{message}"),
        }
    }
}

impl ProfileInputError {
    /// Machine sub-code for structured diagnostics (`4xx` policy range).
    pub(crate) fn subcode(&self) -> u16 {
        match self {
            Self::Unreadable { .. } | Self::Invalid { .. } | Self::Missing { .. } => {
                crate::subcode::PROFILE_INVALID
            }
            Self::Conflict(_) => crate::subcode::PROFILE_CONFLICT,
        }
    }
}

/// Load an interpretation profile, or `None` when no `--profile` was given.
///
/// # Errors
///
/// Returns [`ProfileInputError`] when the file cannot be read or is invalid.
pub(crate) fn load_profile(
    path: Option<&Path>,
) -> Result<Option<InterpretationProfile>, ProfileInputError> {
    let Some(path) = path else {
        return Ok(None);
    };
    let text = std::fs::read_to_string(path).map_err(|error| ProfileInputError::Unreadable {
        path: path.display().to_string(),
        message: error.to_string(),
    })?;
    InterpretationProfile::parse(&text)
        .map_err(|error| ProfileInputError::Invalid {
            path: path.display().to_string(),
            message: error.to_string(),
        })
        .map(Some)
}

/// Ambient `COPYBOOK_DIALECT` input, if set. An invalid value says nothing
/// (the default applies); changing that silent fallback is out of scope for
/// profile consumption.
fn dialect_env() -> Option<Dialect> {
    let value = std::env::var("COPYBOOK_DIALECT").ok()?;
    match value.trim().to_ascii_lowercase().as_str() {
        "0" => Some(Dialect::ZeroTolerant),
        "1" => Some(Dialect::OneTolerant),
        "n" | "normative" => Some(Dialect::Normative),
        _ => None,
    }
}

/// Resolve the framing inputs shared by `decode` and `verify`.
///
/// `format` has no product default: without a profile it must come from the
/// flag (clap enforces this via `required_unless_present`). `max_errors`
/// likewise has no default on decode (`None` means unlimited).
///
/// # Errors
///
/// Returns [`ProfileInputError::Conflict`] on flag/profile disagreement or
/// [`ProfileInputError::Missing`] when framing is unavailable.
#[allow(clippy::too_many_arguments)]
pub(crate) fn resolve_common(
    format_flag: Option<RecordFormat>,
    codepage_flag: Option<Codepage>,
    dialect_flag: Option<DialectPreference>,
    max_errors_flag: Option<u64>,
    profile: Option<&InterpretationProfile>,
) -> Result<ResolvedCommonInputs, ProfileInputError> {
    let profile_format = profile.map(|profile| RecordFormat::from(profile.framing.kind));
    let format = match (format_flag, profile_format) {
        (Some(flag), Some(_)) if Some(flag) != profile_format => {
            return Err(ProfileInputError::Conflict(ConflictError {
                field: FORMAT_PROFILE_KEY,
                profile_value: profile_format
                    .map(|format| format.to_string())
                    .unwrap_or_default(),
                flag_value: flag.to_string(),
            }));
        }
        (Some(flag), _) => flag,
        (None, Some(from_profile)) => from_profile,
        (None, None) => {
            return Err(ProfileInputError::Missing {
                message: "--format is required when no profile supplies framing".to_string(),
            });
        }
    };

    let codepage = resolve_field(
        CODEPAGE_PROFILE_KEY,
        codepage_flag,
        profile.map(|profile| profile.decode.codepage),
        None,
        Codepage::CP037,
    )
    .map(|resolved| resolved.value)
    .map_err(ProfileInputError::Conflict)?;

    let dialect = resolve_field(
        DIALECT_PROFILE_KEY,
        dialect_flag.map(Dialect::from),
        profile.map(|profile| Dialect::from(profile.source.dialect)),
        dialect_env(),
        Dialect::Normative,
    )
    .map(|resolved| resolved.value)
    .map_err(ProfileInputError::Conflict)?;

    let profile_max_errors = profile.map(|profile| profile.limits.maximum_errors);
    let max_errors = match (max_errors_flag, profile_max_errors) {
        (Some(flag), Some(from_profile)) if flag != from_profile => {
            return Err(ProfileInputError::Conflict(ConflictError {
                field: MAX_ERRORS_PROFILE_KEY,
                profile_value: from_profile.to_string(),
                flag_value: flag.to_string(),
            }));
        }
        (Some(flag), _) => Some(flag),
        (None, Some(from_profile)) => Some(from_profile),
        (None, None) => None,
    };

    Ok(ResolvedCommonInputs {
        format,
        codepage,
        dialect,
        strict_reserved_bytes: profile
            .is_some_and(|profile| profile.framing.reserved_bytes == ReservedPolicy::Strict),
        max_errors,
    })
}

/// Resolve the decode-only inputs covered by the profile.
///
/// # Errors
///
/// Returns [`ProfileInputError::Conflict`] on flag/profile disagreement.
pub(crate) fn resolve_decode(
    json_number_flag: Option<JsonNumberMode>,
    unmappable_flag: Option<UnmappablePolicy>,
    profile: Option<&InterpretationProfile>,
) -> Result<ResolvedDecodeInputs, ProfileInputError> {
    let json_number = resolve_field(
        JSON_NUMBERS_PROFILE_KEY,
        json_number_flag,
        profile.map(|profile| profile.decode.json_numbers),
        None,
        JsonNumberMode::Lossless,
    )
    .map(|resolved| resolved.value)
    .map_err(ProfileInputError::Conflict)?;

    let unmappable = resolve_field(
        UNMAPPABLE_PROFILE_KEY,
        unmappable_flag,
        profile.map(|profile| profile.decode.unmappable),
        None,
        UnmappablePolicy::Error,
    )
    .map(|resolved| resolved.value)
    .map_err(ProfileInputError::Conflict)?;

    Ok(ResolvedDecodeInputs {
        json_number,
        unmappable,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook::codec::options::profile::FramingKind;

    const PROFILE_TOML: &str = "schema_version = 1\n[source]\ndialect = \"zero-tolerant\"\n[framing]\nkind = \"rdw\"\nreserved_bytes = \"strict\"\n[decode]\ncodepage = \"cp037\"\nunmappable = \"error\"\njson_numbers = \"lossless\"\n[limits]\nmaximum_record_length = 32760\nmaximum_errors = 100\n";

    fn profile() -> InterpretationProfile {
        InterpretationProfile::parse(PROFILE_TOML).expect("test profile parses")
    }

    #[test]
    fn profile_supplies_every_layer_when_no_flags_given() {
        let profile = profile();
        let common = resolve_common(None, None, None, None, Some(&profile)).unwrap();
        assert_eq!(common.format, RecordFormat::RDW);
        assert_eq!(common.codepage, Codepage::CP037);
        assert_eq!(common.dialect, Dialect::ZeroTolerant);
        assert!(common.strict_reserved_bytes);
        assert_eq!(common.max_errors, Some(100));

        let decode = resolve_decode(None, None, Some(&profile)).unwrap();
        assert_eq!(decode.json_number, JsonNumberMode::Lossless);
        assert_eq!(decode.unmappable, UnmappablePolicy::Error);
    }

    #[test]
    fn equal_flag_and_profile_values_agree() {
        let profile = profile();
        let common = resolve_common(
            Some(RecordFormat::RDW),
            Some(Codepage::CP037),
            None,
            Some(100),
            Some(&profile),
        )
        .unwrap();
        assert_eq!(common.format, RecordFormat::RDW);
        assert_eq!(common.max_errors, Some(100));
    }

    #[test]
    fn disagreeing_format_flag_conflicts() {
        let profile = profile();
        let error = resolve_common(Some(RecordFormat::Fixed), None, None, None, Some(&profile))
            .expect_err("fixed flag vs rdw profile must conflict");
        assert!(matches!(error, ProfileInputError::Conflict(_)));
        assert_eq!(error.subcode(), crate::subcode::PROFILE_CONFLICT);
        let message = error.to_string();
        assert!(
            message.contains("rdw"),
            "message names profile value: {message}"
        );
        assert!(
            message.contains("fixed"),
            "message names flag value: {message}"
        );
    }

    #[test]
    fn disagreeing_max_errors_flag_conflicts() {
        let profile = profile();
        let error = resolve_common(None, None, None, Some(5), Some(&profile))
            .expect_err("max-errors 5 vs profile 100 must conflict");
        assert!(matches!(error, ProfileInputError::Conflict(_)));
        let message = error.to_string();
        assert!(
            message.contains("100"),
            "message names profile value: {message}"
        );
    }

    #[test]
    fn no_profile_falls_back_to_flags_and_defaults() {
        let common = resolve_common(
            Some(RecordFormat::Fixed),
            None,
            Some(DialectPreference::N),
            None,
            None,
        )
        .unwrap();
        assert_eq!(common.format, RecordFormat::Fixed);
        assert_eq!(common.codepage, Codepage::CP037);
        assert_eq!(common.dialect, Dialect::Normative);
        assert!(!common.strict_reserved_bytes);
        assert_eq!(common.max_errors, None);

        let decode = resolve_decode(Some(JsonNumberMode::Native), None, None).unwrap();
        assert_eq!(decode.json_number, JsonNumberMode::Native);
        assert_eq!(decode.unmappable, UnmappablePolicy::Error);
    }

    #[test]
    fn missing_format_without_profile_is_an_error() {
        let error = resolve_common(None, None, None, None, None)
            .expect_err("no framing from any layer must fail");
        assert!(matches!(error, ProfileInputError::Missing { .. }));
    }

    #[test]
    fn lenient_profile_leaves_reserved_lenient() {
        let text = PROFILE_TOML.replace(
            "reserved_bytes = \"strict\"",
            "reserved_bytes = \"lenient\"",
        );
        let profile = InterpretationProfile::parse(&text).expect("lenient profile parses");
        let common = resolve_common(None, None, None, None, Some(&profile)).unwrap();
        assert!(!common.strict_reserved_bytes);
        assert_eq!(common.format, RecordFormat::RDW);
    }

    #[test]
    fn framing_kind_vb_resolves() {
        let text = PROFILE_TOML.replace("kind = \"rdw\"", "kind = \"vb\"");
        let profile = InterpretationProfile::parse(&text).expect("vb profile parses");
        assert_eq!(profile.framing.kind, FramingKind::Vb);
        let common = resolve_common(None, None, None, None, Some(&profile)).unwrap();
        assert_eq!(common.format, RecordFormat::Vb);
    }

    #[test]
    fn unreadable_profile_path_errors() {
        let error = load_profile(Some(Path::new("/nonexistent/profile.toml")))
            .expect_err("missing file must fail");
        assert!(matches!(error, ProfileInputError::Unreadable { .. }));
        assert_eq!(error.subcode(), crate::subcode::PROFILE_INVALID);
    }

    #[test]
    fn invalid_profile_toml_errors() {
        let dir = std::env::temp_dir().join(format!(
            "copybook-profile-invalid-test-{}.toml",
            std::process::id()
        ));
        std::fs::write(&dir, "schema_version = 1\n[framing]\nkind = \"nonsense\"\n").unwrap();
        let error = load_profile(Some(&dir)).expect_err("unknown framing kind must fail");
        assert!(matches!(error, ProfileInputError::Invalid { .. }));
        let _ = std::fs::remove_file(&dir);
    }
}
