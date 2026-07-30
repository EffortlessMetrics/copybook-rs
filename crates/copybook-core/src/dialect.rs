// SPDX-License-Identifier: AGPL-3.0-or-later
//! Dialect contract for ODO (`OCCURS DEPENDING ON`) `min_count` semantics.
//!
//! `copybook-core` owns this parser-language policy. The separate
//! `copybook-dialect` package remains only as a deprecated compatibility
//! forwarder for users of the 0.5 package layout.

use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

/// Dialect for ODO `min_count` interpretation.
///
/// | Dialect | `min_count` interpretation |
/// | --- | --- |
/// | `Normative` | Enforce the declared minimum. |
/// | `ZeroTolerant` | Ignore the declared minimum. |
/// | `OneTolerant` | Clamp the minimum to at least one. |
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub enum Dialect {
    /// Enforce the declared `min_count`.
    #[default]
    Normative,
    /// Ignore the declared `min_count`.
    ZeroTolerant,
    /// Clamp the effective minimum to at least one.
    OneTolerant,
}

impl FromStr for Dialect {
    type Err = String;

    #[inline]
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value.trim() {
            "n" | "N" => Ok(Self::Normative),
            "0" => Ok(Self::ZeroTolerant),
            "1" => Ok(Self::OneTolerant),
            _ => Err(format!(
                "Invalid dialect '{value}'. Valid values are: 'n' (normative), '0' (zero-tolerant), '1' (one-tolerant)"
            )),
        }
    }
}

impl fmt::Display for Dialect {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Normative => write!(formatter, "n"),
            Self::ZeroTolerant => write!(formatter, "0"),
            Self::OneTolerant => write!(formatter, "1"),
        }
    }
}

/// Computes the effective `min_count` for a dialect.
///
/// # Examples
///
/// ```
/// use copybook_core::dialect::{Dialect, effective_min_count};
///
/// assert_eq!(effective_min_count(Dialect::Normative, 5), 5);
/// assert_eq!(effective_min_count(Dialect::ZeroTolerant, 5), 0);
/// assert_eq!(effective_min_count(Dialect::OneTolerant, 0), 1);
/// ```
#[inline]
#[must_use]
pub fn effective_min_count(dialect: Dialect, declared_min_count: u32) -> u32 {
    match dialect {
        Dialect::Normative => declared_min_count,
        Dialect::ZeroTolerant => 0,
        Dialect::OneTolerant => declared_min_count.max(1),
    }
}
