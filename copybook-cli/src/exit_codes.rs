//! Canonical exit codes for the `copybook` CLI.
//!
//! The CLI reports structured exit codes that line up with the error taxonomy:
//! `CBKD*` (data quality), `CBKE*` (encode/validation), `CBKF*` (format/runtime),
//! and `CBKI*` (internal orchestration faults). Centralising the mapping here keeps
//! the documentation, tests, and CI summaries in sync.

use std::fmt;
use std::fmt::Formatter;
use std::str::FromStr;

use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// Operation exit codes surfaced by the `copybook` CLI.
#[repr(i32)]
#[non_exhaustive]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExitCode {
    /// Successful run (no warnings that demand attention).
    Ok = 0,
    /// Non-taxonomy failures (panic, unknown errors, integration hiccups).
    Unknown = 1,
    /// Data quality failure (`CBKD*`).
    Data = 2,
    /// Encode/validation failure (`CBKE*`) or verification mismatch.
    Encode = 3,
    /// Record format / iterator fatal (`CBKF*`).
    Format = 4,
    /// Internal orchestration error (`CBKI*`).
    Internal = 5,
}

/// Lightweight metadata used for documentation and job summaries.
pub struct ExitCodeDetails {
    pub code: ExitCode,
    pub tag: &'static str,
    pub meaning: &'static str,
    pub verification_test: &'static str,
}

impl ExitCode {
    /// Numeric process code.
    #[must_use]
    pub const fn as_i32(self) -> i32 {
        self as i32
    }

    /// Short taxonomy tag associated with the exit code.
    #[must_use]
    pub const fn tag(self) -> &'static str {
        match self {
            ExitCode::Ok => "OK",
            ExitCode::Data => "CBKD",
            ExitCode::Encode => "CBKE",
            ExitCode::Format => "CBKF",
            ExitCode::Internal => "CBKI",
            ExitCode::Unknown => "CBK?",
        }
    }

    /// Family identifier used by observability consumers.
    #[must_use]
    pub const fn family(self) -> &'static str {
        match self {
            ExitCode::Ok => "ok",
            ExitCode::Data => "decode",
            ExitCode::Encode => "policy",
            ExitCode::Format => "io",
            ExitCode::Internal => "internal",
            ExitCode::Unknown => "unknown",
        }
    }

    /// Human readable interpretation used in docs and CI summaries.
    #[must_use]
    pub const fn meaning(self) -> &'static str {
        match self {
            ExitCode::Ok => "Success",
            ExitCode::Unknown => "Unhandled failure",
            ExitCode::Data => "Data quality failure",
            ExitCode::Encode => "Encode/validation failure",
            ExitCode::Format => "Record format/RDW failure",
            ExitCode::Internal => "Internal orchestration error",
        }
    }

    /// Canonical test or harness that asserts the mapping.
    #[must_use]
    pub const fn verification_test(self) -> &'static str {
        match self {
            ExitCode::Ok | ExitCode::Unknown => "n/a",
            ExitCode::Data => "exit_code_mapping::exit_code_cbkd_is_2",
            ExitCode::Encode => "exit_code_mapping::exit_code_cbke_is_3",
            ExitCode::Format => "exit_code_mapping::exit_code_cbkf_is_4",
            ExitCode::Internal => "exit_code_mapping::exit_code_cbki_is_5",
        }
    }

    /// Metadata bundle for documentation consumers.
    #[must_use]
    pub const fn details(self) -> ExitCodeDetails {
        ExitCodeDetails {
            code: self,
            tag: self.tag(),
            meaning: self.meaning(),
            verification_test: self.verification_test(),
        }
    }

    /// Ordered slice used for deterministic precedence (higher wins).
    ///
    /// Precedence rule (highest wins): CBKI > CBKF > CBKE > CBKD > Unknown/OK.
    #[must_use]
    pub const fn precedence(self) -> u8 {
        self.precedence_rank()
    }

    /// Ranking helper for dashboards (higher rank wins).
    #[must_use]
    pub const fn precedence_rank(self) -> u8 {
        match self {
            ExitCode::Internal => 4,
            ExitCode::Format => 3,
            ExitCode::Encode => 2,
            ExitCode::Data => 1,
            ExitCode::Unknown | ExitCode::Ok => 0,
        }
    }

    /// Map a taxonomy prefix (e.g. `CBKD`) onto an exit code.
    #[must_use]
    pub fn from_family_prefix(prefix: &str) -> Option<Self> {
        match prefix {
            "CBKD" => Some(ExitCode::Data),
            "CBKE" => Some(ExitCode::Encode),
            "CBKF" => Some(ExitCode::Format),
            "CBKI" => Some(ExitCode::Internal),
            _ => None,
        }
    }

    /// Codes that participate in reviewer-facing documentation and CI summaries.
    #[must_use]
    pub fn documented_codes() -> &'static [ExitCode] {
        &[
            ExitCode::Data,
            ExitCode::Encode,
            ExitCode::Format,
            ExitCode::Internal,
        ]
    }
}

impl fmt::Display for ExitCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.tag())
    }
}

impl FromStr for ExitCode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "OK" => Ok(ExitCode::Ok),
            "CBK?" => Ok(ExitCode::Unknown),
            "CBKD" => Ok(ExitCode::Data),
            "CBKE" => Ok(ExitCode::Encode),
            "CBKF" => Ok(ExitCode::Format),
            "CBKI" => Ok(ExitCode::Internal),
            _ => Err(()),
        }
    }
}

impl std::convert::TryFrom<i32> for ExitCode {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ExitCode::Ok),
            1 => Ok(ExitCode::Unknown),
            2 => Ok(ExitCode::Data),
            3 => Ok(ExitCode::Encode),
            4 => Ok(ExitCode::Format),
            5 => Ok(ExitCode::Internal),
            _ => Err(()),
        }
    }
}

impl From<ExitCode> for std::process::ExitCode {
    #[inline]
    fn from(value: ExitCode) -> Self {
        std::process::ExitCode::from(value as u8)
    }
}

impl From<ExitCode> for i32 {
    #[inline]
    fn from(value: ExitCode) -> Self {
        value.as_i32()
    }
}

impl Serialize for ExitCode {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.tag())
    }
}

impl<'de> Deserialize<'de> for ExitCode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ExitCodeVisitor;

        impl Visitor<'_> for ExitCodeVisitor {
            type Value = ExitCode;

            fn expecting(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
                formatter.write_str("a copybook CLI exit code tag or integer")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "OK" => Ok(ExitCode::Ok),
                    "CBK?" => Ok(ExitCode::Unknown),
                    "CBKD" => Ok(ExitCode::Data),
                    "CBKE" => Ok(ExitCode::Encode),
                    "CBKF" => Ok(ExitCode::Format),
                    "CBKI" => Ok(ExitCode::Internal),
                    other => Err(E::unknown_variant(
                        other,
                        &["OK", "CBK?", "CBKD", "CBKE", "CBKF", "CBKI"],
                    )),
                }
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    0 => Ok(ExitCode::Ok),
                    1 => Ok(ExitCode::Unknown),
                    2 => Ok(ExitCode::Data),
                    3 => Ok(ExitCode::Encode),
                    4 => Ok(ExitCode::Format),
                    5 => Ok(ExitCode::Internal),
                    other => Err(E::custom(format!("unknown exit code {other}"))),
                }
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                self.visit_i64(
                    i64::try_from(v)
                        .map_err(|_| E::custom(format!("exit code {v} does not fit in i64")))?,
                )
            }
        }

        deserializer.deserialize_any(ExitCodeVisitor)
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use serde_json::{from_str, to_string};
    use std::io;

    type TestResult<T> = Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn readme_exit_code_table_matches_source_of_truth() -> TestResult<()> {
        let readme = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../README.md"));
        let mut rows = readme
            .lines()
            .skip_while(|line| *line != "| Code | Tag  | Meaning (1-liner) | Test |");

        let header = rows
            .next()
            .ok_or_else(|| io::Error::other("exit code table header missing"))?;
        assert_eq!(
            header, "| Code | Tag  | Meaning (1-liner) | Test |",
            "exit code table header missing"
        );

        let separator = rows
            .next()
            .ok_or_else(|| io::Error::other("exit code table separator missing"))?;
        assert_eq!(
            separator, "|----:|:----:|--------------------|------|",
            "exit code table separator missing"
        );

        for code in ExitCode::documented_codes() {
            let details = code.details();
            let readme_row = rows.next().ok_or_else(|| {
                io::Error::other(format!("missing README row for {}", details.tag))
            })?;
            let cells: Vec<_> = readme_row
                .trim()
                .trim_matches('|')
                .split('|')
                .map(str::trim)
                .collect();
            assert_eq!(
                cells.len(),
                4,
                "unexpected number of columns for {}",
                details.tag
            );
            assert_eq!(
                cells[0],
                details.code.as_i32().to_string(),
                "code mismatch for {}",
                details.tag
            );
            assert_eq!(cells[1], details.tag, "tag mismatch for {}", details.tag);
            assert_eq!(
                cells[2], details.meaning,
                "meaning mismatch for {}",
                details.tag
            );
            assert_eq!(
                cells[3], details.verification_test,
                "test reference mismatch for {}",
                details.tag
            );
        }
        Ok(())
    }

    #[test]
    fn exit_code_serializes_to_tag() -> TestResult<()> {
        let serialized = to_string(&ExitCode::Format)?;
        assert_eq!(serialized, "\"CBKF\"");
        Ok(())
    }

    #[test]
    fn exit_code_deserializes_from_tag_or_number() -> TestResult<()> {
        assert_eq!(from_str::<ExitCode>("\"CBKI\"")?, ExitCode::Internal);
        assert_eq!(from_str::<ExitCode>("4")?, ExitCode::Format);
        Ok(())
    }
}
