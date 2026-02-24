// SPDX-License-Identifier: AGPL-3.0-or-later
//! Dialect types for ODO (OCCURS DEPENDING ON) `min_count` behavior.
//!
//! This crate defines the dialect lever that controls how `min_count` is interpreted
//! for ODO arrays. Different dialects provide different levels of strictness in
//! enforcing the minimum count constraint.

use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

/// Dialect for ODO `min_count` interpretation
///
/// The dialect lever controls how `min_count` is interpreted for ODO arrays:
///
/// | Dialect | `min_count` Interpretation | Description |
/// |---------|------------------------|-------------|
/// | `Normative` | `min_count` is enforced | Counter must be ≥ `min_count` (strict) |
/// | `ZeroTolerant` | `min_count` is ignored | Counter can be `0..max_count` (relaxed) |
/// | `OneTolerant` | `min_count` clamped to 1 | Counter must be ≥ `max(1, min_count)` |
///
/// # Examples
///
/// ```rust
/// use copybook_dialect::Dialect;
/// use std::str::FromStr;
///
/// // Default dialect is Normative
/// let default = Dialect::default();
/// assert_eq!(default, Dialect::Normative);
///
/// // Parse from string
/// let normative = Dialect::from_str("n").unwrap();
/// assert_eq!(normative, Dialect::Normative);
///
/// let zero_tolerant = Dialect::from_str("0").unwrap();
/// assert_eq!(zero_tolerant, Dialect::ZeroTolerant);
///
/// let one_tolerant = Dialect::from_str("1").unwrap();
/// assert_eq!(one_tolerant, Dialect::OneTolerant);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub enum Dialect {
    /// Normative dialect - `min_count` is strictly enforced
    ///
    /// Counter must be ≥ `min_count`. This is the default behavior.
    #[default]
    Normative,

    /// Zero-tolerant dialect - `min_count` is ignored
    ///
    /// Counter can be `0..max_count`, regardless of declared `min_count`.
    ZeroTolerant,

    /// One-tolerant dialect - `min_count` is clamped to 1
    ///
    /// Counter must be ≥ `max(1, min_count)`. This allows zero-length arrays
    /// when `min_count` is 0, but enforces at least one element otherwise.
    OneTolerant,
}

impl FromStr for Dialect {
    type Err = String;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "n" | "N" => Ok(Self::Normative),
            "0" => Ok(Self::ZeroTolerant),
            "1" => Ok(Self::OneTolerant),
            _ => Err(format!(
                "Invalid dialect '{s}'. Valid values are: 'n' (normative), '0' (zero-tolerant), '1' (one-tolerant)"
            )),
        }
    }
}

impl fmt::Display for Dialect {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Normative => write!(f, "n"),
            Self::ZeroTolerant => write!(f, "0"),
            Self::OneTolerant => write!(f, "1"),
        }
    }
}

/// Compute the effective `min_count` based on dialect
///
/// This function applies the dialect-specific transformation to the declared
/// `min_count` to produce the effective minimum count used for validation.
///
/// # Arguments
///
/// * `dialect` - The dialect to apply
/// * `declared_min_count` - The `min_count` value declared in the copybook
///
/// # Returns
///
/// The effective `min_count` based on dialect:
///
/// | Dialect | Result |
/// |---------|--------|
/// | `Normative` | Returns `declared_min_count` unchanged |
/// | `ZeroTolerant` | Returns `0` (ignores declared `min_count`) |
/// | `OneTolerant` | Returns `max(1, declared_min_count)` |
///
/// # Examples
///
/// ```rust
/// use copybook_dialect::{Dialect, effective_min_count};
///
/// // Normative: min_count is enforced as-is
/// assert_eq!(effective_min_count(Dialect::Normative, 0), 0);
/// assert_eq!(effective_min_count(Dialect::Normative, 1), 1);
/// assert_eq!(effective_min_count(Dialect::Normative, 5), 5);
///
/// // ZeroTolerant: min_count is always 0
/// assert_eq!(effective_min_count(Dialect::ZeroTolerant, 0), 0);
/// assert_eq!(effective_min_count(Dialect::ZeroTolerant, 1), 0);
/// assert_eq!(effective_min_count(Dialect::ZeroTolerant, 5), 0);
///
/// // OneTolerant: min_count is clamped to 1
/// assert_eq!(effective_min_count(Dialect::OneTolerant, 0), 1);
/// assert_eq!(effective_min_count(Dialect::OneTolerant, 1), 1);
/// assert_eq!(effective_min_count(Dialect::OneTolerant, 5), 5);
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

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_dialect_default() {
        assert_eq!(Dialect::default(), Dialect::Normative);
    }

    #[test]
    fn test_dialect_from_str() {
        // Normative variants
        assert_eq!(Dialect::from_str("n").unwrap(), Dialect::Normative);
        assert_eq!(Dialect::from_str("N").unwrap(), Dialect::Normative);

        // ZeroTolerant
        assert_eq!(Dialect::from_str("0").unwrap(), Dialect::ZeroTolerant);

        // OneTolerant
        assert_eq!(Dialect::from_str("1").unwrap(), Dialect::OneTolerant);

        // Invalid values
        assert!(Dialect::from_str("x").is_err());
        assert!(Dialect::from_str("normative").is_err());
        assert!(Dialect::from_str("").is_err());
    }

    #[test]
    fn test_dialect_display() {
        assert_eq!(Dialect::Normative.to_string(), "n");
        assert_eq!(Dialect::ZeroTolerant.to_string(), "0");
        assert_eq!(Dialect::OneTolerant.to_string(), "1");
    }

    #[test]
    fn test_effective_min_count_normative() {
        // Normative: min_count is enforced as-is
        assert_eq!(effective_min_count(Dialect::Normative, 0), 0);
        assert_eq!(effective_min_count(Dialect::Normative, 1), 1);
        assert_eq!(effective_min_count(Dialect::Normative, 5), 5);
        assert_eq!(effective_min_count(Dialect::Normative, 100), 100);
    }

    #[test]
    fn test_effective_min_count_zero_tolerant() {
        // ZeroTolerant: min_count is always 0
        assert_eq!(effective_min_count(Dialect::ZeroTolerant, 0), 0);
        assert_eq!(effective_min_count(Dialect::ZeroTolerant, 1), 0);
        assert_eq!(effective_min_count(Dialect::ZeroTolerant, 5), 0);
        assert_eq!(effective_min_count(Dialect::ZeroTolerant, 100), 0);
    }

    #[test]
    fn test_effective_min_count_one_tolerant() {
        // OneTolerant: min_count is clamped to 1
        assert_eq!(effective_min_count(Dialect::OneTolerant, 0), 1);
        assert_eq!(effective_min_count(Dialect::OneTolerant, 1), 1);
        assert_eq!(effective_min_count(Dialect::OneTolerant, 5), 5);
        assert_eq!(effective_min_count(Dialect::OneTolerant, 100), 100);
    }

    #[test]
    fn test_dialect_roundtrip() {
        // Test that parsing and displaying are consistent
        let dialects = [
            Dialect::Normative,
            Dialect::ZeroTolerant,
            Dialect::OneTolerant,
        ];
        for dialect in dialects {
            let s = dialect.to_string();
            let parsed = Dialect::from_str(&s).unwrap();
            assert_eq!(parsed, dialect);
        }
    }
}
