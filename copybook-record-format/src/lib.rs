// SPDX-License-Identifier: AGPL-3.0-or-later
//! Record format contracts shared by decode/encode and record I/O layers.
#![allow(clippy::missing_inline_in_public_items)]

use serde::{Deserialize, Serialize};

use std::fmt;

/// Record format specification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum RecordFormat {
    /// Fixed-length records.
    Fixed,
    /// Variable-length records with Record Descriptor Word.
    RDW,
}

impl RecordFormat {
    /// Check if this is a fixed-length record format.
    #[must_use]
    #[inline]
    pub const fn is_fixed(self) -> bool {
        matches!(self, Self::Fixed)
    }

    /// Check if this is a variable-length record format.
    #[must_use]
    #[inline]
    pub const fn is_variable(self) -> bool {
        matches!(self, Self::RDW)
    }

    /// Get a human-readable description of the format.
    #[must_use]
    #[inline]
    pub const fn description(self) -> &'static str {
        match self {
            Self::Fixed => "Fixed-length records",
            Self::RDW => "Variable-length records with Record Descriptor Word",
        }
    }
}

impl fmt::Display for RecordFormat {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fixed => write!(f, "fixed"),
            Self::RDW => write!(f, "rdw"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::RecordFormat;

    #[test]
    fn helpers_report_expected_values() {
        assert!(RecordFormat::Fixed.is_fixed());
        assert!(!RecordFormat::Fixed.is_variable());
        assert_eq!(RecordFormat::Fixed.description(), "Fixed-length records");

        assert!(RecordFormat::RDW.is_variable());
        assert!(!RecordFormat::RDW.is_fixed());
        assert_eq!(
            RecordFormat::RDW.description(),
            "Variable-length records with Record Descriptor Word"
        );
    }
}
