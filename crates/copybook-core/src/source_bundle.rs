// SPDX-License-Identifier: AGPL-3.0-or-later
//! Path-independent source bundle identity for copybook provenance.
//!
//! [`SourceBundle`] names exactly which copybook source set was interpreted
//! without relying on workstation paths or a single opaque root-file hash.
//! It is the durable input identity for resolved manifests, receipts, and
//! compatibility review ([#1116]).
//!
//! Three identities stay distinct:
//!
//! - raw source identity: SHA-256 over one unit's raw bytes (what operators
//!   diff; reproduced here per unit);
//! - schema identity: the canonical resolved schema plus parse options
//!   ([`Schema::fingerprint`](crate::schema::Schema::fingerprint));
//! - bundle identity: [`SourceBundle::fingerprint`], covering every material
//!   source unit and relationship but excluding local display paths,
//!   timestamps, and non-identity labels.
//!
//! The initial contract carries one root source; include collection is
//! explicitly unsupported (see [`IncludeSupport`]). The multi-unit shape,
//! bounds, and duplicate/oversize failures are validated today so includes
//! extend the bundle without changing its identity rules.
//!
//! [#1116]: https://github.com/EffortlessMetrics/copybook-rs/issues/1116

use crate::dialect::Dialect;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

/// Version of the bundle contract serialized by [`SourceBundle`].
pub const SOURCE_BUNDLE_SCHEMA_VERSION: u32 = 1;

/// Stability class of the v1 bundle contract.
pub const SOURCE_BUNDLE_STABILITY_CLASS: &str = "beta";

/// Fingerprint algorithm: SHA-256 over the canonical identity bytes.
pub const SOURCE_BUNDLE_FINGERPRINT_ALGO: &str = "sha256-canonical-json-v1";

/// Maximum source units per bundle. Includes are unsupported, so only
/// single-source bundles are constructible through the public API today;
/// the bound is validated so the multi-unit shape cannot arrive unchecked.
pub const MAX_BUNDLE_UNITS: usize = 8;

/// Maximum raw bytes per source unit, mirroring the profile 16 MiB limit.
pub const MAX_SOURCE_UNIT_BYTES: u64 = 16 * 1024 * 1024;

/// Maximum raw bytes across all units in one bundle.
pub const MAX_BUNDLE_BYTES: u64 = 16 * 1024 * 1024;

/// Include-collection depth limit. Zero means collection is unsupported:
/// include edges and unresolved includes always validate empty.
pub const BUNDLE_INCLUDE_DEPTH_LIMIT: u32 = 0;

/// One material source unit: raw bytes plus their SHA-256 and length.
///
/// The SHA-256 is over the exact bytes handed to the parser (line endings
/// preserved). Any lexer-level normalization is a separately named parser
/// fact, never a silent rewrite of this identity.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceUnit {
    /// Path-independent logical identity (a display-safe unit name, never
    /// a workstation path).
    pub logical_id: String,
    /// Lowercase hex SHA-256 over the unit's raw bytes.
    pub sha256: String,
    /// Raw byte length.
    pub byte_len: u64,
}

/// How raw source bytes reach the parser.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextPolicy {
    /// Text encoding the bytes were read with. Copybooks are read as UTF-8
    /// text today; any future encoding lands here as a distinct value.
    pub encoding: String,
    /// Line-ending treatment: raw bytes are hashed as read while the lexer
    /// normalizes line structure for parsing.
    pub line_endings: LineEndingHandling,
}

/// Line-ending treatment for source bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LineEndingHandling {
    /// Raw bytes (LF or CRLF) are hashed as read; the lexer accepts both.
    PreserveRaw,
}

/// Include-collection support state. Collection is unsupported: edges and
/// unresolved includes validate empty, and the state is recorded instead
/// of claiming complete provenance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IncludeSupport {
    /// Include collection is unsupported; the relationship graph is empty.
    Unsupported,
}

/// One include/copy relationship between logical units. Unconstructible
/// while [`IncludeSupport`] is [`IncludeSupport::Unsupported`]; reserved
/// for include-era extension.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct IncludeEdge {
    /// Logical identity of the including unit.
    pub from: String,
    /// Logical identity of the included unit.
    pub to: String,
}

/// Provenance of the effective dialect used for interpretation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DialectProvenance {
    /// The source set intrinsically declares this dialect.
    DeclaredIntrinsic,
    /// No intrinsic declaration; the operator-selected profile value applies.
    ProfileSelected,
    /// Neither declares nor selects; the default applies and is recorded.
    DefaultUndeclared,
}

/// Effective dialect plus where it came from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EffectiveDialect {
    /// The dialect interpretation must use.
    pub dialect: Dialect,
    /// Why this dialect was selected.
    pub provenance: DialectProvenance,
}

/// Path-independent source bundle: the durable input identity.
///
/// The bundle [`fingerprint`](SourceBundle::fingerprint) covers the schema
/// version, root, ordered units (identity, SHA-256, length), include state,
/// text policy, and intrinsic dialect declaration. Display paths,
/// timestamps, and [`labels`](SourceBundle::labels) are excluded by
/// construction.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceBundle {
    /// Bundle contract version ([`SOURCE_BUNDLE_SCHEMA_VERSION`]).
    pub schema_version: u32,
    /// Stability class ([`SOURCE_BUNDLE_STABILITY_CLASS`]).
    pub stability_class: String,
    /// Logical identity of the root unit.
    pub root: String,
    /// Ordered material source units. Ordering is semantic (include order
    /// when includes exist); single-source bundles hold the root alone.
    pub units: Vec<SourceUnit>,
    /// Include-collection support state.
    pub include_support: IncludeSupport,
    /// Include/copy edges. Always empty while unsupported.
    pub include_edges: Vec<IncludeEdge>,
    /// Declared but unresolvable includes. Always empty while unsupported.
    pub unresolved_includes: Vec<String>,
    /// Source text policy used for parsing.
    pub text_policy: TextPolicy,
    /// Dialect declaration intrinsic to the source set, if any. The current
    /// parser detects no source directives, so this is always `None`;
    /// `Some` is reserved for intrinsic declarations.
    pub declared_dialect: Option<Dialect>,
    /// Optional non-sensitive provenance labels. Explicitly excluded from
    /// the fingerprint; identity must never depend on them.
    pub labels: BTreeMap<String, String>,
    /// Fingerprint algorithm ([`SOURCE_BUNDLE_FINGERPRINT_ALGO`]).
    pub fingerprint_algo: String,
    /// Bundle fingerprint: SHA-256 over the canonical identity bytes.
    pub fingerprint: String,
}

/// Bundle construction or dialect-resolution failure.
///
/// Variants carry limits and dialect names only: logical identities are
/// caller-supplied and may embed workstation paths, so they never appear
/// in machine-visible errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BundleError {
    /// No source units were supplied.
    NoUnits,
    /// A logical identity is empty.
    EmptyLogicalId,
    /// Two units share a logical identity.
    DuplicateLogicalId,
    /// A unit exceeds [`MAX_SOURCE_UNIT_BYTES`]; carries the limit.
    UnitTooLarge {
        /// Enforced per-unit byte limit.
        limit: u64,
    },
    /// All units exceed [`MAX_BUNDLE_BYTES`]; carries the limit.
    BundleTooLarge {
        /// Enforced total byte limit.
        limit: u64,
    },
    /// More units than [`MAX_BUNDLE_UNITS`]; carries the limit.
    TooManyUnits {
        /// Enforced unit-count limit.
        limit: usize,
    },
    /// Include edges supplied while collection is unsupported.
    IncludesUnsupported,
    /// Unresolved includes supplied while collection is unsupported.
    UnresolvedIncludesUnsupported,
    /// An intrinsic declaration disagrees with the selected profile value.
    DialectConflict {
        /// Intrinsically declared dialect.
        declared: Dialect,
        /// Operator-selected dialect.
        selected: Dialect,
    },
}

impl std::fmt::Display for BundleError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoUnits => write!(formatter, "source bundle has no source units"),
            Self::EmptyLogicalId => write!(formatter, "source unit logical identity is empty"),
            Self::DuplicateLogicalId => {
                write!(formatter, "duplicate source unit logical identity")
            }
            Self::UnitTooLarge { limit } => {
                write!(formatter, "source unit exceeds {limit} raw bytes")
            }
            Self::BundleTooLarge { limit } => {
                write!(formatter, "source bundle exceeds {limit} raw bytes")
            }
            Self::TooManyUnits { limit } => {
                write!(formatter, "source bundle exceeds {limit} source units")
            }
            Self::IncludesUnsupported => write!(
                formatter,
                "include collection is unsupported; include edges must be empty"
            ),
            Self::UnresolvedIncludesUnsupported => write!(
                formatter,
                "include collection is unsupported; unresolved includes must be empty"
            ),
            Self::DialectConflict { declared, selected } => write!(
                formatter,
                "intrinsic dialect declaration {declared:?} disagrees with selected profile value {selected:?}"
            ),
        }
    }
}

impl std::error::Error for BundleError {}

impl SourceBundle {
    /// Build a one-source bundle from raw bytes.
    ///
    /// `logical_id` names the unit without workstation paths (see
    /// [`logical_id_for_path`]); `bytes` are the exact bytes handed to the
    /// parser. Include state records collection as unsupported.
    ///
    /// # Errors
    ///
    /// Returns [`BundleError`] for an empty identity, oversize input, or a
    /// serialization failure (unreachable with current field types).
    #[must_use = "Handle the Result or propagate the error"]
    #[inline]
    pub fn single(logical_id: impl Into<String>, bytes: &[u8]) -> Result<Self, BundleError> {
        Self::from_units(&[(logical_id.into(), bytes.to_vec())])
    }

    /// Build a bundle from ordered `(logical identity, raw bytes)` units.
    ///
    /// The first unit is the root. Ordering is semantic and preserved in
    /// identity: equivalent bundles are order-independent only where the
    /// source/include semantics are.
    ///
    /// # Errors
    ///
    /// Returns [`BundleError`] for no units, empty or duplicate identities,
    /// or any bound violation.
    #[must_use = "Handle the Result or propagate the error"]
    #[inline]
    pub fn from_units(units: &[(String, Vec<u8>)]) -> Result<Self, BundleError> {
        if units.is_empty() {
            return Err(BundleError::NoUnits);
        }
        if units.len() > MAX_BUNDLE_UNITS {
            return Err(BundleError::TooManyUnits {
                limit: MAX_BUNDLE_UNITS,
            });
        }
        let mut seen = std::collections::BTreeSet::new();
        let mut total: u64 = 0;
        let mut material = Vec::with_capacity(units.len());
        for (logical_id, bytes) in units {
            if logical_id.is_empty() {
                return Err(BundleError::EmptyLogicalId);
            }
            if !seen.insert(logical_id.clone()) {
                return Err(BundleError::DuplicateLogicalId);
            }
            let byte_len = u64::try_from(bytes.len()).map_err(|_| BundleError::UnitTooLarge {
                limit: MAX_SOURCE_UNIT_BYTES,
            })?;
            if byte_len > MAX_SOURCE_UNIT_BYTES {
                return Err(BundleError::UnitTooLarge {
                    limit: MAX_SOURCE_UNIT_BYTES,
                });
            }
            total = total.saturating_add(byte_len);
            material.push(SourceUnit {
                logical_id: logical_id.clone(),
                sha256: raw_sha256(bytes),
                byte_len,
            });
        }
        if total > MAX_BUNDLE_BYTES {
            return Err(BundleError::BundleTooLarge {
                limit: MAX_BUNDLE_BYTES,
            });
        }
        let root = material[0].logical_id.clone();
        let mut bundle = Self {
            schema_version: SOURCE_BUNDLE_SCHEMA_VERSION,
            stability_class: SOURCE_BUNDLE_STABILITY_CLASS.to_string(),
            root,
            units: material,
            include_support: IncludeSupport::Unsupported,
            include_edges: Vec::new(),
            unresolved_includes: Vec::new(),
            text_policy: TextPolicy {
                encoding: "utf-8".to_string(),
                line_endings: LineEndingHandling::PreserveRaw,
            },
            declared_dialect: None,
            labels: BTreeMap::new(),
            fingerprint_algo: SOURCE_BUNDLE_FINGERPRINT_ALGO.to_string(),
            fingerprint: String::new(),
        };
        bundle.fingerprint = bundle.identity_fingerprint();
        Ok(bundle)
    }

    /// Raw source fingerprint: SHA-256 over the root unit's raw bytes.
    ///
    /// For one-source bundles this reproduces the existing raw source
    /// fingerprint byte for byte; any source edit (comments, formatting,
    /// line endings) changes it.
    #[must_use]
    #[inline]
    pub fn source_fingerprint(&self) -> &str {
        self.units.first().map_or("", |unit| unit.sha256.as_str())
    }

    /// Bundle fingerprint: SHA-256 over the canonical identity bytes.
    #[must_use]
    #[inline]
    pub fn fingerprint(&self) -> &str {
        self.fingerprint.as_str()
    }

    /// Canonical identity bytes: the exact bytes the fingerprint covers.
    ///
    /// Field order follows declaration order with sorted label keys, so
    /// identical bundles serialize byte-identical bytes across runs and
    /// platforms. Labels are excluded; the fingerprint itself is excluded
    /// because it is derived from these bytes.
    #[must_use]
    #[inline]
    pub fn canonical_identity_bytes(&self) -> Vec<u8> {
        let payload = serde_json::json!({
            "schema_version": self.schema_version,
            "stability_class": self.stability_class,
            "root": self.root,
            "units": self.units,
            "include_support": self.include_support,
            "include_edges": self.include_edges,
            "unresolved_includes": self.unresolved_includes,
            "text_policy": self.text_policy,
            "declared_dialect": self.declared_dialect,
            "fingerprint_algo": self.fingerprint_algo,
        });
        serde_json::to_vec(&payload).unwrap_or_default()
    }

    /// Canonical evidence rendering: pretty JSON of the full bundle
    /// including its fingerprint, suitable for checked-in evidence.
    ///
    /// # Errors
    ///
    /// Returns a serialization error, unreachable with current field types.
    #[must_use = "Handle the Result or propagate the error"]
    #[inline]
    pub fn canonical_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    fn identity_fingerprint(&self) -> String {
        use sha2::{Digest, Sha256};
        let digest = Sha256::digest(self.canonical_identity_bytes());
        crate::schema::sha256_hex(&digest)
    }
}

/// Derive a path-independent logical identity for one source file.
///
/// Uses the file name only, so the same bytes under different local paths
/// produce the same bundle identity. Falls back to `"stdin"` for `-` and
/// `"copybook"` when the path has no file name.
#[must_use]
#[inline]
pub fn logical_id_for_path(path: &Path) -> String {
    if path.as_os_str().is_empty() || path == Path::new("-") {
        return "stdin".to_string();
    }
    path.file_name().map_or_else(
        || "copybook".to_string(),
        |name| name.to_string_lossy().into_owned(),
    )
}

/// Resolve the effective dialect from intrinsic declaration and
/// operator-selected profile value without two drifting authorities.
///
/// - intrinsic declaration wins when present and unopposed or agreeing;
/// - the profile value applies when nothing is declared;
/// - the default applies when neither declares nor selects;
/// - disagreement between declaration and selection fails.
///
/// # Errors
///
/// Returns [`BundleError::DialectConflict`] when an intrinsic declaration
/// disagrees with the selected profile value.
#[must_use = "Handle the Result or propagate the error"]
#[inline]
pub fn resolve_effective_dialect(
    declared: Option<Dialect>,
    selected: Option<Dialect>,
) -> Result<EffectiveDialect, BundleError> {
    match (declared, selected) {
        (Some(declaration), Some(selection)) if declaration != selection => {
            Err(BundleError::DialectConflict {
                declared: declaration,
                selected: selection,
            })
        }
        (Some(declaration), _) => Ok(EffectiveDialect {
            dialect: declaration,
            provenance: DialectProvenance::DeclaredIntrinsic,
        }),
        (None, Some(selection)) => Ok(EffectiveDialect {
            dialect: selection,
            provenance: DialectProvenance::ProfileSelected,
        }),
        (None, None) => Ok(EffectiveDialect {
            dialect: Dialect::default(),
            provenance: DialectProvenance::DefaultUndeclared,
        }),
    }
}

/// SHA-256 over raw bytes, lowercase hex.
fn raw_sha256(bytes: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    format!("{:x}", Sha256::digest(bytes))
}

#[cfg(test)]
mod tests {
    use super::*;

    const SIMPLE: &[u8] = b"       01  REC.\n           05  NAME     PIC X(10).\n";

    fn simple_bundle() -> SourceBundle {
        SourceBundle::single("simple.cpy", SIMPLE).expect("valid bundle")
    }

    #[test]
    fn single_reproduces_raw_source_fingerprint() {
        use sha2::{Digest, Sha256};
        let expected = format!("{:x}", Sha256::digest(SIMPLE));
        let bundle = simple_bundle();
        assert_eq!(bundle.source_fingerprint(), expected);
        assert_eq!(bundle.units.len(), 1);
        assert_eq!(bundle.root, "simple.cpy");
    }

    #[test]
    fn bundle_fingerprint_is_deterministic_and_distinct() {
        let first = simple_bundle();
        let second = simple_bundle();
        assert_eq!(first.fingerprint(), second.fingerprint());
        assert_eq!(
            first.canonical_identity_bytes(),
            second.canonical_identity_bytes()
        );
        assert_ne!(first.fingerprint(), first.source_fingerprint());
        assert!(!first.fingerprint().is_empty());
    }

    #[test]
    fn same_bytes_different_paths_share_identity() {
        let left =
            SourceBundle::single(logical_id_for_path(Path::new("/tmp/a/simple.cpy")), SIMPLE)
                .expect("valid bundle");
        let right = SourceBundle::single(
            logical_id_for_path(Path::new("/other/b/simple.cpy")),
            SIMPLE,
        )
        .expect("valid bundle");
        assert_eq!(left.fingerprint(), right.fingerprint());
    }

    #[test]
    fn changed_byte_changes_identity() {
        let mut altered = SIMPLE.to_vec();
        altered[10] = b'X';
        let other = SourceBundle::single("simple.cpy", &altered).expect("valid bundle");
        assert_ne!(simple_bundle().fingerprint(), other.fingerprint());
        assert_ne!(
            simple_bundle().source_fingerprint(),
            other.source_fingerprint()
        );
    }

    #[test]
    fn line_ending_change_changes_raw_identity() {
        let parts: Vec<&[u8]> = SIMPLE.split(|byte| *byte == b'\n').collect();
        let crlf = parts.join(b"\r\n".as_slice());
        let bundle = SourceBundle::single("simple.cpy", &crlf).expect("valid bundle");
        assert_ne!(
            simple_bundle().source_fingerprint(),
            bundle.source_fingerprint()
        );
        assert_ne!(simple_bundle().fingerprint(), bundle.fingerprint());
    }

    #[test]
    fn labels_do_not_affect_identity() {
        let mut labeled = simple_bundle();
        labeled
            .labels
            .insert("operator".to_string(), "payroll-team".to_string());
        assert_eq!(labeled.fingerprint(), simple_bundle().fingerprint());
    }

    #[test]
    fn structural_failures_are_deterministic() {
        assert_eq!(
            SourceBundle::from_units(&[]).expect_err("no units"),
            BundleError::NoUnits
        );
        assert_eq!(
            SourceBundle::single("", SIMPLE).expect_err("empty identity"),
            BundleError::EmptyLogicalId
        );
        assert_eq!(
            SourceBundle::from_units(&[
                ("a.cpy".to_string(), SIMPLE.to_vec()),
                ("a.cpy".to_string(), SIMPLE.to_vec()),
            ])
            .expect_err("duplicate identity"),
            BundleError::DuplicateLogicalId
        );
        // Error messages carry no caller-supplied identities.
        let message = format!("{}", BundleError::DuplicateLogicalId);
        assert!(!message.contains("a.cpy"));
    }

    #[test]
    fn oversize_unit_fails() {
        let big = vec![b' '; (MAX_SOURCE_UNIT_BYTES + 1) as usize];
        assert_eq!(
            SourceBundle::single("big.cpy", &big).expect_err("oversize"),
            BundleError::UnitTooLarge {
                limit: MAX_SOURCE_UNIT_BYTES
            }
        );
    }

    #[test]
    fn dialect_agreement_matrix() {
        let selected =
            resolve_effective_dialect(None, Some(Dialect::ZeroTolerant)).expect("profile-selected");
        assert_eq!(selected.dialect, Dialect::ZeroTolerant);
        assert_eq!(selected.provenance, DialectProvenance::ProfileSelected);

        let defaulted = resolve_effective_dialect(None, None).expect("default");
        assert_eq!(defaulted.dialect, Dialect::Normative);
        assert_eq!(defaulted.provenance, DialectProvenance::DefaultUndeclared);

        let intrinsic =
            resolve_effective_dialect(Some(Dialect::OneTolerant), None).expect("intrinsic");
        assert_eq!(intrinsic.provenance, DialectProvenance::DeclaredIntrinsic);

        let agreed =
            resolve_effective_dialect(Some(Dialect::ZeroTolerant), Some(Dialect::ZeroTolerant))
                .expect("agreement");
        assert_eq!(agreed.dialect, Dialect::ZeroTolerant);

        let conflict =
            resolve_effective_dialect(Some(Dialect::OneTolerant), Some(Dialect::ZeroTolerant))
                .expect_err("conflict");
        assert_eq!(
            conflict,
            BundleError::DialectConflict {
                declared: Dialect::OneTolerant,
                selected: Dialect::ZeroTolerant,
            }
        );
    }

    #[test]
    fn canonical_json_round_trips_with_required_keys() {
        let rendered = simple_bundle().canonical_json().expect("rendering");
        let parsed: serde_json::Value = serde_json::from_str(&rendered).expect("JSON");
        for key in [
            "schema_version",
            "stability_class",
            "root",
            "units",
            "include_support",
            "text_policy",
            "declared_dialect",
            "fingerprint_algo",
            "fingerprint",
        ] {
            assert!(parsed.get(key).is_some(), "missing key {key}");
        }
        assert_eq!(parsed["schema_version"], SOURCE_BUNDLE_SCHEMA_VERSION);
        let restored: SourceBundle = serde_json::from_str(&rendered).expect("restore");
        assert_eq!(restored, simple_bundle());
    }

    #[test]
    fn include_state_records_unsupported() {
        let bundle = simple_bundle();
        assert_eq!(bundle.include_support, IncludeSupport::Unsupported);
        assert!(bundle.include_edges.is_empty());
        assert!(bundle.unresolved_includes.is_empty());
        assert_eq!(BUNDLE_INCLUDE_DEPTH_LIMIT, 0);
    }

    #[test]
    fn logical_id_for_path_uses_file_name_only() {
        assert_eq!(
            logical_id_for_path(Path::new("/tmp/a/simple.cpy")),
            "simple.cpy"
        );
        assert_eq!(logical_id_for_path(Path::new("-")), "stdin");
    }
}
