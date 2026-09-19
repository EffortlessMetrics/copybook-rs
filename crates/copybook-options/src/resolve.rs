// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::missing_inline_in_public_items)]
//! Effective-option resolution: one value per field, no silent precedence.
//!
//! Every profile-covered option reaches a command from up to four layers:
//! an explicit CLI flag, the reviewed profile, an ambient environment
//! variable, or the product default. This module resolves exactly one value
//! per field and reports where it came from, so machine results can later
//! show effective options instead of asking operators to reconstruct them.
//!
//! The only hard rule: an explicit flag and a profile value that disagree
//! is a contradiction, not a precedence decision. Resolution fails with
//! [`ConflictError`] naming the field and both values:
//!
//! ```text
//! profile says framing=rdw
//! command says --format fixed
//!
//! error: conflicting framing configuration
//! ```
//!
//! Equal values are not a contradiction: the flag wins and the run
//! proceeds. Environment input sits below the profile (ambient setting
//! loses to reviewed intent) and above the default; that ordering is
//! documented here instead of living in each command.
//!
//! # Examples
//!
//! ```rust
//! use copybook_options::resolve::{OptionSource, resolve_field};
//!
//! let resolved = resolve_field("framing", Some("rdw"), Some("rdw"), None, "fixed")
//!     .expect("equal values agree");
//! assert_eq!(resolved.source, OptionSource::Flag);
//!
//! let conflict = resolve_field("framing", Some("fixed"), Some("rdw"), None, "fixed")
//!     .expect_err("different values contradict");
//! assert_eq!(conflict.field, "framing");
//! ```

use std::fmt;

/// Where one resolved option value came from, weakest to strongest.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OptionSource {
    /// Product default; nobody configured this field.
    Default,
    /// Ambient environment variable.
    Env,
    /// Reviewed interpretation profile.
    Profile,
    /// Explicit command-line flag.
    Flag,
}

impl fmt::Display for OptionSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Default => write!(f, "default"),
            Self::Env => write!(f, "environment"),
            Self::Profile => write!(f, "profile"),
            Self::Flag => write!(f, "command flag"),
        }
    }
}

/// One resolved value plus the layer that supplied it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolved<T> {
    /// The effective value.
    pub value: T,
    /// Where the value came from.
    pub source: OptionSource,
}

/// An explicit flag contradicting a profile value for the same field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConflictError {
    /// Option field path (`framing`, `decode.codepage`).
    pub field: &'static str,
    /// Value the profile carries, rendered for display.
    pub profile_value: String,
    /// Value the command flag carries, rendered for display.
    pub flag_value: String,
}

impl fmt::Display for ConflictError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "conflicting {} configuration: profile says {}={}, command says {}={}",
            self.field, self.field, self.profile_value, self.field, self.flag_value
        )
    }
}

impl std::error::Error for ConflictError {}

/// Resolve one option field across flag, profile, environment, and default.
///
/// Precedence is flag, then profile, then environment, then default, with
/// one exception: a flag and a profile value that are both present but
/// different contradict each other and resolution fails. Equal values agree
/// and the flag wins. Every layer is [`Option`]: `None` means that layer
/// says nothing about the field.
///
/// # Errors
///
/// Returns [`ConflictError`] when the flag and the profile disagree.
pub fn resolve_field<T>(
    field: &'static str,
    flag: Option<T>,
    profile: Option<T>,
    env: Option<T>,
    default: T,
) -> Result<Resolved<T>, ConflictError>
where
    T: Clone + PartialEq + fmt::Display,
{
    match (flag, profile) {
        (Some(flag_value), Some(profile_value)) if flag_value != profile_value => {
            Err(ConflictError {
                field,
                profile_value: profile_value.to_string(),
                flag_value: flag_value.to_string(),
            })
        }
        (Some(value), _) => Ok(Resolved {
            value,
            source: OptionSource::Flag,
        }),
        (None, Some(value)) => Ok(Resolved {
            value,
            source: OptionSource::Profile,
        }),
        (None, None) => match env {
            Some(value) => Ok(Resolved {
                value,
                source: OptionSource::Env,
            }),
            None => Ok(Resolved {
                value: default,
                source: OptionSource::Default,
            }),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct PrecedenceCase {
        flag: Option<&'static str>,
        profile: Option<&'static str>,
        env: Option<&'static str>,
        expected: &'static str,
        source: OptionSource,
    }

    #[test]
    fn precedence_is_flag_then_profile_then_env_then_default() {
        let cases = [
            PrecedenceCase {
                flag: Some("flag"),
                profile: Some("flag"),
                env: Some("env"),
                expected: "flag",
                source: OptionSource::Flag,
            },
            PrecedenceCase {
                flag: None,
                profile: Some("profile"),
                env: Some("env"),
                expected: "profile",
                source: OptionSource::Profile,
            },
            PrecedenceCase {
                flag: None,
                profile: None,
                env: Some("env"),
                expected: "env",
                source: OptionSource::Env,
            },
            PrecedenceCase {
                flag: None,
                profile: None,
                env: None,
                expected: "default",
                source: OptionSource::Default,
            },
        ];
        for case in cases {
            let resolved = resolve_field("field", case.flag, case.profile, case.env, "default")
                .expect("no conflict");
            assert_eq!(resolved.value, case.expected);
            assert_eq!(resolved.source, case.source);
        }
    }

    #[test]
    fn differing_flag_and_profile_contradict() {
        let error = resolve_field("framing", Some("fixed"), Some("rdw"), None, "fixed")
            .expect_err("contradiction");
        assert_eq!(error.field, "framing");
        assert_eq!(error.profile_value, "rdw");
        assert_eq!(error.flag_value, "fixed");
        let message = error.to_string();
        assert!(message.contains("framing"), "unexpected message: {message}");
        assert!(message.contains("rdw"), "unexpected message: {message}");
        assert!(message.contains("fixed"), "unexpected message: {message}");
    }

    #[test]
    fn equal_flag_and_profile_agree_with_flag_source() {
        let resolved = resolve_field("framing", Some("rdw"), Some("rdw"), Some("env"), "fixed")
            .expect("equal values agree");
        assert_eq!(resolved.value, "rdw");
        assert_eq!(resolved.source, OptionSource::Flag);
    }

    #[test]
    fn sources_render_for_humans() {
        assert_eq!(OptionSource::Default.to_string(), "default");
        assert_eq!(OptionSource::Env.to_string(), "environment");
        assert_eq!(OptionSource::Profile.to_string(), "profile");
        assert_eq!(OptionSource::Flag.to_string(), "command flag");
    }
}
