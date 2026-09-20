// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::missing_inline_in_public_items)]
//! Resolved-schema manifest: a reviewable snapshot of one resolved schema.
//!
//! The manifest binds the four resolved-schema facts an operator reviews: the
//! input bundle fingerprint, the effective option values with provenance (from
//! [`resolve`](crate::options::resolve) layers), the resolved layout (field
//! paths with offsets, lengths, and physical bounds), and the support
//! classification drawn from the static support matrix. It is the inspectable
//! artifact behind `inspect --emit-manifest` and the `ResolvedManifest`
//! contract of #1117.
//!
//! ## Maturity
//!
//! The contract is evolving:
//! [`RESOLVED_MANIFEST_STABILITY_CLASS`](crate::resolved_manifest::RESOLVED_MANIFEST_STABILITY_CLASS)
//! is `beta` until the profile contract it binds settles. Do not treat a beta
//! manifest as a frozen interchange format.
//!
//! ## Canonical form and tamper evidence
//!
//! The fingerprint covers the canonical body bytes: the manifest body as JSON
//! with object keys sorted recursively (`canonical_bytes`). Key order in a
//! stored document never affects verification. Every body property, including
//! properties a reader does not understand, feeds the fingerprint, so an
//! injected property without a regenerated fingerprint fails verification.
//!
//! ## Evolution rules
//!
//! Readers ignore properties they do not understand (after the fingerprint
//! verifies), so additive changes are new optional properties: old readers
//! verify and read new documents, ignoring what they cannot interpret.
//! Incompatible changes are a schema version bump, a stability-class change,
//! a fingerprint-algorithm change, a removed or retyped property, or a
//! property whose meaning changes. Unknown schema versions, stability
//! classes, and fingerprint algorithms are rejected explicitly, never
//! guessed.
//!
//! ## Fingerprint discipline
//!
//! [`crate::resolved_manifest::ResolvedManifest`] carries no cached digest:
//! [`crate::resolved_manifest::ResolvedManifest::fingerprint`] recomputes it
//! from the current body on every call, so a mutated value can never
//! serialize under a stale digest.

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fmt;

use copybook_core::dialect::Dialect;
use copybook_core::schema::{Field, FieldKind, Occurs, Schema, SignPlacement};
use copybook_core::source_bundle::{
    BundleError, DialectProvenance, SourceBundle, resolve_effective_dialect,
};
use copybook_core::support_matrix::{FeatureId, SupportStatus, find_feature_by_id};

use crate::options::profile::InterpretationProfile;
use crate::options::resolve::{OptionSource, Resolved};

/// Schema version of the resolved-manifest JSON document.
///
/// Version 2 restructures `inputs` for identity: the bundle becomes a
/// versioned fingerprinted object, and profile, tool, and canonical schema
/// identities join the contract. Version 1 documents are rejected explicitly;
/// regenerate them.
pub const RESOLVED_MANIFEST_SCHEMA_VERSION: u32 = 2;
/// Stability class of the resolved-manifest contract: beta while the bound
/// profile contract settles (see the module-level maturity note).
pub const RESOLVED_MANIFEST_STABILITY_CLASS: &str = "beta";
/// Hash algorithm used for [`ResolvedManifest::fingerprint`].
pub const RESOLVED_MANIFEST_FINGERPRINT_ALGO: &str = "sha256-v1";
/// Maximum number of flattened layout fields admitted into a manifest.
///
/// The bound applies to `fields` only, matching `schemas/resolved-manifest.json`;
/// detail vectors stay proportional (at most a fixed few entries per field) and
/// the serialized byte bound caps the total.
pub const MAX_MANIFEST_FIELDS: usize = 4096;
/// Maximum JSON byte length of a serialized manifest.
pub const MAX_MANIFEST_BYTES: usize = 1_048_576;

/// Errors raised while generating or verifying a [`ResolvedManifest`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ManifestError {
    /// The effective dialect could not be resolved from bundle and selection.
    DialectConflict {
        /// Intrinsically declared dialect.
        declared: Dialect,
        /// Operator-selected dialect.
        selected: Dialect,
    },
    /// Dialect resolution failed in a way no known variant describes.
    DialectResolutionFailed,
    /// The reviewed profile could not be canonicalized for its identity
    /// fingerprint.
    ProfileFingerprint {
        /// Canonicalization failure.
        reason: String,
    },
    /// The flattened layout fields exceed [`MAX_MANIFEST_FIELDS`].
    TooManyFields {
        /// Number of fields the resolved layout contains.
        found: usize,
    },
    /// The serialized manifest exceeds [`MAX_MANIFEST_BYTES`].
    ManifestTooLarge {
        /// Serialized byte length that was rejected.
        found: usize,
    },
    /// Stored manifest JSON could not be parsed.
    MalformedManifest {
        /// Human-readable parse failure.
        reason: String,
    },
    /// Stored manifest declares an unsupported schema version.
    UnsupportedManifestVersion {
        /// Version the stored document declares.
        found: u32,
    },
    /// Stored manifest declares an unexpected stability class.
    UnsupportedStabilityClass {
        /// Class the stored document declares.
        found: String,
    },
    /// Stored manifest declares an unexpected fingerprint algorithm.
    UnsupportedFingerprintAlgo {
        /// Algorithm the stored document declares.
        found: String,
    },
    /// Stored manifest fingerprint does not match the recomputed value.
    FingerprintMismatch {
        /// Fingerprint declared by the stored document.
        expected: String,
        /// Fingerprint recomputed from the stored document body.
        actual: String,
    },
}

impl fmt::Display for ManifestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DialectConflict { declared, selected } => write!(
                f,
                "dialect conflict: source declares {declared:?}, profile selects {selected:?}"
            ),
            Self::DialectResolutionFailed => {
                write!(f, "effective dialect resolution failed")
            }
            Self::ProfileFingerprint { reason } => {
                write!(f, "reviewed profile fingerprint failed: {reason}")
            }
            Self::TooManyFields { found } => write!(
                f,
                "resolved layout has {found} fields, exceeding the limit of {MAX_MANIFEST_FIELDS}"
            ),
            Self::ManifestTooLarge { found } => write!(
                f,
                "serialized manifest is {found} bytes, exceeding the limit of {MAX_MANIFEST_BYTES}"
            ),
            Self::MalformedManifest { reason } => {
                write!(f, "malformed resolved manifest: {reason}")
            }
            Self::UnsupportedManifestVersion { found } => write!(
                f,
                "unsupported resolved-manifest version {found}; this build reads version {RESOLVED_MANIFEST_SCHEMA_VERSION}"
            ),
            Self::UnsupportedStabilityClass { found } => write!(
                f,
                "unsupported resolved-manifest stability class {found:?}; this build reads {RESOLVED_MANIFEST_STABILITY_CLASS:?} (regenerate the manifest)"
            ),
            Self::UnsupportedFingerprintAlgo { found } => write!(
                f,
                "unsupported resolved-manifest fingerprint algorithm {found:?}; this build reads {RESOLVED_MANIFEST_FINGERPRINT_ALGO:?} (regenerate the manifest)"
            ),
            Self::FingerprintMismatch { expected, actual } => write!(
                f,
                "resolved-manifest fingerprint mismatch: expected {expected}, recomputed {actual}"
            ),
        }
    }
}

impl std::error::Error for ManifestError {}

/// One resolved value plus the layer that supplied it, as recorded.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestValue<T> {
    /// Resolved value.
    pub value: T,
    /// Resolution layer in [`OptionSource`] display spelling.
    pub source: String,
}

/// Effective dialect with both provenance chains recorded.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestDialect {
    /// Effective dialect in profile spelling.
    pub value: String,
    /// Resolution layer in [`OptionSource`] display spelling.
    pub source: String,
    /// Why this dialect was selected, in canonical spelling.
    pub provenance: String,
}

/// Source-bundle identity the schema was resolved from.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestBundle {
    /// Bundle contract version.
    pub schema_version: u32,
    /// Bundle fingerprint.
    pub fingerprint: String,
    /// Logical id of the bundle's root unit (the resolved layout's root).
    pub root_unit: String,
}

/// Reviewed-profile identity the effective values were resolved from.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestProfile {
    /// Profile contract version.
    pub schema_version: u32,
    /// Profile fingerprint over its canonical rendering.
    pub fingerprint: String,
}

/// Tool identity that generated the manifest.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestTool {
    /// Tool name (e.g. `copybook`).
    pub name: String,
    /// Tool package version that generated the manifest.
    pub version: String,
}

/// Resolved inputs captured for one manifest.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestInputs {
    /// Source-bundle identity the schema was resolved from.
    pub bundle: ManifestBundle,
    /// Reviewed-profile identity (`None` for profile-less direct runs).
    pub profile: Option<ManifestProfile>,
    /// Tool identity that generated the manifest.
    pub tool: ManifestTool,
    /// Encoding actually used plus its source.
    pub encoding: ManifestValue<String>,
    /// Dialect actually used plus both provenance chains.
    pub dialect: ManifestDialect,
    /// Framing actually used plus its source.
    pub framing: ManifestValue<String>,
    /// Record bound actually used plus its source (`None` when the run is
    /// uncapped, e.g. a profile-less direct run).
    pub record_bound: Option<ManifestValue<u64>>,
}

/// One flattened layout field with its physical bounds.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestField {
    /// Dotted field path.
    pub path: String,
    /// COBOL level number.
    pub level: u8,
    /// Physical byte offset of the field start.
    pub offset: u32,
    /// Physical byte length of the field.
    pub len: u32,
    /// Physical end offset (`offset + len`, saturating).
    pub end: u32,
    /// Stable numeric kind tag (e.g. `alphanum`, `packed_decimal`, `group`).
    pub kind: String,
    /// Fully-qualified path of the redefined storage owner when this field
    /// carries REDEFINES (chains chase to the ultimate owner).
    pub redefines: Option<String>,
    /// Whether this field carries a SYNCHRONIZED clause.
    pub synchronized: bool,
    /// Whether this field carries a BLANK WHEN ZERO clause.
    pub blank_when_zero: bool,
    /// OCCURS repetition bound (`None` for scalar fields). Optional and
    /// omitted when absent, so manifests generated before this field
    /// existed re-serialize byte-identically.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub occurs: Option<ManifestOccurs>,
}

/// OCCURS repetition bound for one flattened layout field.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestOccurs {
    /// Repetition kind: `fixed` or `odo`.
    pub kind: String,
    /// Fixed repetition count, or the ODO maximum.
    pub count: u32,
    /// Minimum repetitions (equals `count` for fixed tables).
    pub min_count: u32,
    /// ODO counter field path (`None` for fixed tables).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub counter_path: Option<String>,
}

/// Numeric usage detail for one flattened field.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestNumericDetail {
    /// Dotted field path.
    pub path: String,
    /// Total digits (bits for binary integers, zero for floats).
    pub digits: u32,
    /// Decimal scale.
    pub scale: i32,
    /// Whether the field is signed.
    pub signed: bool,
    /// Canonical encoding tag (e.g. `packed-decimal`, `zoned`, `binary-16`).
    pub encoding: String,
}

/// ODO (OCCURS DEPENDING ON) usage detail for one flattened field.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestOdoDetail {
    /// Dotted field path.
    pub path: String,
    /// Path of the counter field.
    pub counter_path: String,
    /// Minimum occurrence count.
    pub min_count: u32,
    /// Maximum occurrence count.
    pub max_count: u32,
    /// Whether the ODO clause trails the array (tail form).
    pub is_tail: bool,
}

/// Level-88 condition-name usage: which fields carry condition names.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestConditionUsage {
    /// Dotted path of the condition-name field.
    pub path: String,
}

/// Support classification of one feature referenced by the resolved schema.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestSupportEntry {
    /// Feature identifier in kebab-case.
    pub feature: String,
    /// Support status in kebab-case (`supported`, `partial`, ...).
    pub status: String,
}

/// Level-66 RENAMES alias over one byte range.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestRenames {
    /// Dotted path of the level-66 alias field.
    pub path: String,
    /// Byte offset of the aliased range.
    pub offset: u32,
    /// Byte length of the aliased range.
    pub length: u32,
    /// Dotted paths covered by the alias, in layout order.
    pub members: Vec<String>,
}

/// One REDEFINES storage location and every view overlaid on it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestStorageGroup {
    /// Dotted path of the redefined (storage owner) field.
    pub storage: String,
    /// Every field occupying the storage in layout order: the owner first,
    /// then each redefining view.
    pub views: Vec<String>,
}

/// Per-field source-span availability: the schema carries no source spans,
///
/// so the manifest states that explicitly rather than omitting the fact.
pub const MANIFEST_SOURCE_SPANS: &str = "unavailable";

/// Inputs accepted by [`ResolvedManifest::generate`].
pub struct GenerateInputs<'a> {
    /// Source bundle the schema was parsed from.
    pub bundle: &'a SourceBundle,
    /// Reviewed profile the effective values were resolved from (`None` for
    /// profile-less direct runs: the manifest then records no profile
    /// identity).
    pub profile: Option<&'a InterpretationProfile>,
    /// Tool identity generating the manifest (name and package version).
    pub tool: ManifestTool,
    /// Effective encoding after flag/profile/env/default resolution.
    pub encoding: Resolved<String>,
    /// Selected dialect after flag/profile/env/default resolution.
    pub dialect: Resolved<Dialect>,
    /// Effective framing after flag/profile/env/default resolution.
    pub framing: Resolved<String>,
    /// Effective record bound after resolution (`None` when uncapped: no flag
    /// carries it and no profile supplies it).
    pub record_bound: Option<Resolved<u64>>,
    /// Schema after [`resolve_layout`](copybook_core::layout::resolve_layout).
    pub schema: &'a Schema,
}

/// A reviewable snapshot of one resolved schema: inputs, effective values
/// with provenance, flattened layout with physical bounds, and support
/// classification.
///
/// The value carries no cached digest. [`ResolvedManifest::fingerprint`]
/// recomputes the canonical fingerprint from the current body on every call,
/// and [`ResolvedManifest::to_json`] embeds the recomputed digest, so a
/// mutated value can never serialize under a stale fingerprint.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResolvedManifest {
    /// Manifest document schema version.
    pub schema_version: u32,
    /// Stability class of this contract.
    pub stability_class: String,
    /// Fingerprint algorithm (see [`RESOLVED_MANIFEST_FINGERPRINT_ALGO`]).
    pub fingerprint_algo: String,
    /// Resolved inputs with per-value provenance.
    pub inputs: ManifestInputs,
    /// Canonical schema fingerprint (SHA-256 over the schema's canonical
    /// JSON, recomputed at generation: stored schema fingerprints are never
    /// trusted).
    pub schema_fingerprint: String,
    /// Flattened layout fields with physical bounds.
    pub fields: Vec<ManifestField>,
    /// Maximum field end offset across the flattened layout, in bytes.
    pub record_len: u32,
    /// Static minimum record extent in bytes (`None` for variable layouts:
    /// any ODO makes the floor counter-dependent). Fixed layouts report the
    /// maximum: their extent never varies.
    pub record_len_min: Option<u32>,
    /// Declared fixed record length (`lrecl`), if the schema states one.
    pub lrecl: Option<u32>,
    /// Per-field source-span availability (see [`MANIFEST_SOURCE_SPANS`]).
    pub source_spans: String,
    /// Numeric usage details.
    pub numeric_details: Vec<ManifestNumericDetail>,
    /// ODO usage details.
    pub odo_details: Vec<ManifestOdoDetail>,
    /// Level-88 condition-name usages.
    pub condition_usages: Vec<ManifestConditionUsage>,
    /// Level-66 RENAMES aliases, in layout order.
    pub renames: Vec<ManifestRenames>,
    /// REDEFINES storage locations with every overlaid view, in layout order.
    pub redefines_groups: Vec<ManifestStorageGroup>,
    /// Support classifications for features the schema references.
    pub support: Vec<ManifestSupportEntry>,
}

/// Top-level fingerprint property of a stored manifest document.
const FINGERPRINT_PROPERTY: &str = "manifest_fingerprint";

impl ResolvedManifest {
    /// Generate a manifest from resolved inputs, binding all four fact classes.
    ///
    /// # Errors
    ///
    /// Returns [`ManifestError`] when dialect resolution conflicts, the layout
    /// exceeds [`MAX_MANIFEST_FIELDS`], or the serialized form exceeds
    /// [`MAX_MANIFEST_BYTES`].
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn generate(inputs: GenerateInputs<'_>) -> Result<Self, ManifestError> {
        let effective =
            resolve_effective_dialect(inputs.bundle.declared_dialect(), Some(inputs.dialect.value))
                .map_err(|conflict| match conflict {
                    BundleError::DialectConflict { declared, selected } => {
                        ManifestError::DialectConflict { declared, selected }
                    }
                    _ => ManifestError::DialectResolutionFailed,
                })?;

        let mut flat = FlatLayout::default();
        flatten_fields(&inputs.schema.fields, 0, &mut flat)?;
        if let Some(tail) = &inputs.schema.tail_odo {
            flat.odos.push(ManifestOdoDetail {
                path: tail.array_path.clone(),
                counter_path: tail.counter_path.clone(),
                min_count: tail.min_count,
                max_count: tail.max_count,
                is_tail: true,
            });
            flat.feature_ids.push(FeatureId::OccursDepending);
        }
        let support = support_entries(&flat.feature_ids);
        let record_len = flat.fields.iter().map(|field| field.end).max().unwrap_or(0);
        // ODO makes the extent counter-dependent: only fixed layouts (no ODO
        // anywhere, including tail ODO) admit a static minimum.
        let record_len_min = if flat.odos.is_empty() {
            Some(record_len)
        } else {
            None
        };
        let redefines_groups = resolve_redefines(&mut flat.fields);

        let manifest = Self {
            schema_version: RESOLVED_MANIFEST_SCHEMA_VERSION,
            stability_class: RESOLVED_MANIFEST_STABILITY_CLASS.to_owned(),
            fingerprint_algo: RESOLVED_MANIFEST_FINGERPRINT_ALGO.to_owned(),
            inputs: ManifestInputs {
                bundle: ManifestBundle {
                    schema_version: inputs.bundle.schema_version(),
                    fingerprint: inputs.bundle.fingerprint().to_owned(),
                    root_unit: inputs.bundle.root().to_owned(),
                },
                profile: inputs
                    .profile
                    .map(|profile| {
                        profile.fingerprint().map(|fingerprint| ManifestProfile {
                            schema_version: profile.schema_version,
                            fingerprint,
                        })
                    })
                    .transpose()
                    .map_err(|error| ManifestError::ProfileFingerprint {
                        reason: error.to_string(),
                    })?,
                tool: inputs.tool,
                encoding: ManifestValue {
                    value: inputs.encoding.value,
                    source: source_str(inputs.encoding.source),
                },
                dialect: ManifestDialect {
                    value: dialect_str(effective.dialect),
                    source: source_str(inputs.dialect.source),
                    provenance: provenance_str(effective.provenance),
                },
                framing: ManifestValue {
                    value: inputs.framing.value,
                    source: source_str(inputs.framing.source),
                },
                record_bound: inputs.record_bound.map(|bound| ManifestValue {
                    value: bound.value,
                    source: source_str(bound.source),
                }),
            },
            schema_fingerprint: schema_fingerprint(inputs.schema),
            fields: flat.fields,
            record_len,
            record_len_min,
            lrecl: inputs.schema.lrecl_fixed,
            source_spans: MANIFEST_SOURCE_SPANS.to_owned(),
            numeric_details: flat.numerics,
            odo_details: flat.odos,
            condition_usages: flat.conditions,
            renames: flat.renames,
            redefines_groups,
            support,
        };
        // Bound the final emitted representation (pretty JSON with fingerprint),
        // not an intermediate body, so `generate` never returns a manifest
        // that `to_json` would reject.
        let serialized = assemble_document(&manifest)?;
        if serialized.len() > MAX_MANIFEST_BYTES {
            return Err(ManifestError::ManifestTooLarge {
                found: serialized.len(),
            });
        }
        Ok(manifest)
    }

    /// Canonical fingerprint of this manifest body: `sha256-v1` over the
    /// canonical body bytes (object keys sorted recursively).
    ///
    /// The digest is recomputed on every call; the value carries no cached
    /// fingerprint that a later mutation could stale.
    #[must_use]
    pub fn fingerprint(&self) -> String {
        let body = serde_json::to_value(self).unwrap_or(serde_json::Value::Null);
        fingerprint_value(&body)
    }

    /// Serialize this manifest to canonical JSON, enforcing the size bound.
    ///
    /// The embedded fingerprint is recomputed from the current body, so even
    /// a mutated value serializes under a consistent digest.
    ///
    /// # Errors
    ///
    /// Returns [`ManifestError::ManifestTooLarge`] when the serialized form
    /// exceeds [`MAX_MANIFEST_BYTES`].
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn to_json(&self) -> Result<Vec<u8>, ManifestError> {
        let serialized = assemble_document(self)?;
        if serialized.len() > MAX_MANIFEST_BYTES {
            return Err(ManifestError::ManifestTooLarge {
                found: serialized.len(),
            });
        }
        Ok(serialized)
    }

    /// Parse and verify a stored manifest: contract identity, then fingerprint.
    ///
    /// Unknown body properties are verified by the fingerprint and then
    /// ignored, so additive properties do not break older readers (see the
    /// module-level evolution rules).
    ///
    /// # Errors
    ///
    /// Returns [`ManifestError`] when the document is malformed, declares an
    /// unsupported version, stability class, or fingerprint algorithm, or
    /// fails fingerprint verification.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn from_json(bytes: &[u8]) -> Result<Self, ManifestError> {
        if bytes.len() > MAX_MANIFEST_BYTES {
            return Err(ManifestError::ManifestTooLarge { found: bytes.len() });
        }
        let document: serde_json::Value =
            serde_json::from_slice(bytes).map_err(|err| ManifestError::MalformedManifest {
                reason: err.to_string(),
            })?;
        let body = document
            .as_object()
            .ok_or_else(|| ManifestError::MalformedManifest {
                reason: "manifest document is not a JSON object".to_owned(),
            })?;
        let fingerprint = body
            .get(FINGERPRINT_PROPERTY)
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| ManifestError::MalformedManifest {
                reason: "manifest document has no string manifest_fingerprint".to_owned(),
            })?
            .to_owned();
        // Contract identity reads raw values before the typed parse, so a
        // document from another contract generation reports its version
        // rather than a shape error. Missing or mistyped identity fields are
        // malformed wire, not foreign generations: only well-typed values
        // reach the compatibility errors.
        let version = body
            .get("schema_version")
            .ok_or_else(|| ManifestError::MalformedManifest {
                reason: "manifest document has no schema_version".to_owned(),
            })
            .and_then(|value| {
                value
                    .as_u64()
                    .and_then(|version| u32::try_from(version).ok())
                    .ok_or_else(|| ManifestError::MalformedManifest {
                        reason: "manifest schema_version is not an integer".to_owned(),
                    })
            })?;
        if version != RESOLVED_MANIFEST_SCHEMA_VERSION {
            return Err(ManifestError::UnsupportedManifestVersion { found: version });
        }
        let stability = body
            .get("stability_class")
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| ManifestError::MalformedManifest {
                reason: "manifest stability_class is not a string".to_owned(),
            })?;
        if stability != RESOLVED_MANIFEST_STABILITY_CLASS {
            return Err(ManifestError::UnsupportedStabilityClass {
                found: stability.to_owned(),
            });
        }
        let algo = body
            .get("fingerprint_algo")
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| ManifestError::MalformedManifest {
                reason: "manifest fingerprint_algo is not a string".to_owned(),
            })?;
        if algo != RESOLVED_MANIFEST_FINGERPRINT_ALGO {
            return Err(ManifestError::UnsupportedFingerprintAlgo {
                found: algo.to_owned(),
            });
        }
        let mut unsigned = body.clone();
        unsigned.remove(FINGERPRINT_PROPERTY);
        let recomputed = fingerprint_value(&serde_json::Value::Object(unsigned));
        if recomputed != fingerprint {
            return Err(ManifestError::FingerprintMismatch {
                expected: fingerprint,
                actual: recomputed,
            });
        }
        let manifest: Self = serde_json::from_value(serde_json::Value::Object(body.clone()))
            .map_err(|err| ManifestError::MalformedManifest {
                reason: err.to_string(),
            })?;
        manifest.validate_invariants()?;
        Ok(manifest)
    }

    /// Enforce the version 2 shape invariants a fingerprint alone cannot see.
    ///
    /// Identity fingerprints must read as lowercase 64-character hex digests,
    /// RENAMES aliases must cover at least one member, and every REDEFINES
    /// group must overlay at least one view on its storage with the owner
    /// listed first. Generation always emits documents satisfying these, so a
    /// violation means hand-built or corrupt wire, reported as malformed.
    fn validate_invariants(&self) -> Result<(), ManifestError> {
        let malformed = |reason: &str| ManifestError::MalformedManifest {
            reason: reason.to_owned(),
        };
        if !is_hex_digest(&self.schema_fingerprint) {
            return Err(malformed("manifest schema_fingerprint is not a hex digest"));
        }
        if !is_hex_digest(&self.inputs.bundle.fingerprint) {
            return Err(malformed("manifest bundle fingerprint is not a hex digest"));
        }
        if let Some(profile) = &self.inputs.profile
            && !is_hex_digest(&profile.fingerprint)
        {
            return Err(malformed(
                "manifest profile fingerprint is not a hex digest",
            ));
        }
        for alias in &self.renames {
            if alias.members.is_empty() {
                return Err(malformed("manifest renames entry has no members"));
            }
        }
        for group in &self.redefines_groups {
            let owner_first = group
                .views
                .first()
                .is_some_and(|first| first == &group.storage);
            if group.views.len() < 2 || !owner_first {
                return Err(malformed(
                    "manifest redefines group must list its storage first with at least one view",
                ));
            }
        }
        Ok(())
    }
}

/// Serialize one manifest to its stored document form: the body plus the
/// recomputed fingerprint, pretty-printed.
fn assemble_document(manifest: &ResolvedManifest) -> Result<Vec<u8>, ManifestError> {
    let fingerprint = manifest.fingerprint();
    let mut document =
        serde_json::to_value(manifest).map_err(|err| ManifestError::MalformedManifest {
            reason: err.to_string(),
        })?;
    document[FINGERPRINT_PROPERTY] = serde_json::Value::String(fingerprint);
    serde_json::to_vec_pretty(&document).map_err(|err| ManifestError::MalformedManifest {
        reason: err.to_string(),
    })
}

/// Canonical body bytes: JSON with object keys sorted recursively, so stored
/// key order never affects the fingerprint.
fn canonical_bytes(value: &serde_json::Value) -> Vec<u8> {
    serde_json::to_vec(&sorted_value(value)).unwrap_or_default()
}

/// Recursively sort every object in a JSON value by key.
fn sorted_value(value: &serde_json::Value) -> serde_json::Value {
    match value {
        serde_json::Value::Object(object) => {
            let mut sorted = serde_json::Map::with_capacity(object.len());
            let mut keys: Vec<&str> = object.keys().map(String::as_str).collect();
            keys.sort_unstable();
            for key in keys {
                sorted.insert(key.to_owned(), sorted_value(&object[key]));
            }
            serde_json::Value::Object(sorted)
        }
        serde_json::Value::Array(items) => {
            serde_json::Value::Array(items.iter().map(sorted_value).collect())
        }
        _ => value.clone(),
    }
}

/// `sha256-v1` fingerprint of canonical JSON bytes.
fn fingerprint_value(value: &serde_json::Value) -> String {
    fingerprint_bytes(&canonical_bytes(value))
}

/// Flattened layout accumulation during one [`ResolvedManifest::generate`].
#[derive(Debug, Default)]
struct FlatLayout {
    fields: Vec<ManifestField>,
    numerics: Vec<ManifestNumericDetail>,
    odos: Vec<ManifestOdoDetail>,
    conditions: Vec<ManifestConditionUsage>,
    renames: Vec<ManifestRenames>,
    feature_ids: Vec<FeatureId>,
}

impl FlatLayout {
    /// Record one field, enforcing [`MAX_MANIFEST_FIELDS`].
    fn push_field_counted(&self) -> Result<(), ManifestError> {
        if self.fields.len() >= MAX_MANIFEST_FIELDS {
            return Err(ManifestError::TooManyFields {
                found: self.fields.len() + 1,
            });
        }
        Ok(())
    }
}

/// Compute the `sha256-v1` fingerprint of canonical manifest body bytes.
fn fingerprint_bytes(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    format!("sha256-v1:{hex}", hex = hex::encode(digest))
}

/// Whether a value reads as a lowercase 64-character hex digest: the shared
/// spelling of schema, bundle, and profile identity fingerprints.
fn is_hex_digest(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
}

/// [`OptionSource`] in its documented display spelling.
fn source_str(source: OptionSource) -> String {
    source.to_string()
}

/// [`Dialect`] in profile spelling.
fn dialect_str(dialect: Dialect) -> String {
    match dialect {
        Dialect::Normative => "normative",
        Dialect::ZeroTolerant => "zero-tolerant",
        Dialect::OneTolerant => "one-tolerant",
    }
    .to_owned()
}

/// [`DialectProvenance`] in canonical kebab-case spelling.
fn provenance_str(provenance: DialectProvenance) -> String {
    match provenance {
        DialectProvenance::DeclaredIntrinsic => "declared-intrinsic",
        DialectProvenance::ProfileSelected => "profile-selected",
        DialectProvenance::DefaultUndeclared => "default-undeclared",
    }
    .to_owned()
}

/// [`SupportStatus`] in the matrix kebab-case spelling.
fn status_str(status: SupportStatus) -> String {
    serde_plain::to_string(&status).unwrap_or_else(|_| "unknown".to_owned())
}

/// Canonical schema fingerprint: SHA-256 over the schema's canonical JSON.
///
/// Recomputed from the schema value at generation time; a stored schema
/// fingerprint is never trusted. The digest is bare lowercase hex, matching
/// the schema's own fingerprint spelling (unlike the manifest fingerprint,
/// which carries the `sha256-v1:` prefix).
fn schema_fingerprint(schema: &Schema) -> String {
    let digest = Sha256::digest(schema.create_canonical_json().as_bytes());
    hex::encode(digest)
}

/// Qualify REDEFINES targets to ultimate storage owners and group the views.
///
/// The parser records the clause's target spelling (usually unqualified),
/// while manifest relations must join to field paths: each target resolves
/// against the redefiner's parent scope, then chases redefinition chains to
/// the ultimate owner. The rewrite lands on the fields themselves, so
/// [`ManifestField::redefines`] always holds a joinable path. Groups sort by
/// storage path; each group's views list the owner first, then redefining
/// views in layout order. An unresolvable target keeps its raw spelling: such
/// schemas fail layout resolution, so generation never emits them.
fn resolve_redefines(fields: &mut [ManifestField]) -> Vec<ManifestStorageGroup> {
    use std::collections::{BTreeMap, BTreeSet};
    let paths: BTreeSet<&str> = fields.iter().map(|field| field.path.as_str()).collect();
    let qualify = |redefiner: &str, target: &str| -> String {
        if target.contains('.') {
            return target.to_owned();
        }
        match redefiner.rfind('.') {
            Some(dot) => {
                let candidate = format!("{}.{}", &redefiner[..dot], target);
                if paths.contains(candidate.as_str()) {
                    candidate
                } else {
                    target.to_owned()
                }
            }
            None => target.to_owned(),
        }
    };
    let mut owners: BTreeMap<String, String> = BTreeMap::new();
    for field in fields.iter() {
        if let Some(target) = field.redefines.as_deref() {
            owners.insert(field.path.clone(), qualify(&field.path, target));
        }
    }
    // Chase chains (C REDEFINES B, B REDEFINES A) to the ultimate owner,
    // bounded by the field count so a cycle cannot hang generation.
    let ultimate = |start: &str| -> String {
        let mut current = start.to_owned();
        for _ in 0..owners.len().saturating_add(1) {
            match owners.get(&current) {
                Some(next) => current = next.clone(),
                None => break,
            }
        }
        current
    };
    let mut groups: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for field in fields.iter_mut() {
        if field.redefines.is_some() {
            let owner = ultimate(&owners[&field.path]);
            field.redefines = Some(owner.clone());
            groups.entry(owner).or_default().push(field.path.clone());
        }
    }
    groups
        .into_iter()
        .map(|(storage, redefiners)| {
            let mut views = Vec::with_capacity(redefiners.len() + 1);
            views.push(storage.clone());
            views.extend(redefiners);
            ManifestStorageGroup { storage, views }
        })
        .collect()
}

/// Build support entries for referenced features, preserving first-seen order.
fn support_entries(feature_ids: &[FeatureId]) -> Vec<ManifestSupportEntry> {
    let mut seen = Vec::new();
    let mut entries = Vec::new();
    for id in feature_ids {
        if seen.contains(id) {
            continue;
        }
        seen.push(*id);
        let status = find_feature_by_id(*id)
            .map_or("unknown".to_owned(), |feature| status_str(feature.status));
        let feature = serde_plain::to_string(id).unwrap_or_else(|_| "unknown".to_owned());
        entries.push(ManifestSupportEntry { feature, status });
    }
    entries
}

/// Stable numeric kind tag for a [`FieldKind`].
fn kind_tag(kind: &FieldKind) -> &'static str {
    match kind {
        FieldKind::Alphanum { .. } => "alphanum",
        FieldKind::EditedNumeric { .. } => "edited_numeric",
        FieldKind::PackedDecimal { .. } => "packed_decimal",
        FieldKind::ZonedDecimal { .. } => "zoned_decimal",
        FieldKind::BinaryInt { .. } => "binary_int",
        FieldKind::FloatSingle => "float_single",
        FieldKind::FloatDouble => "float_double",
        FieldKind::Group => "group",
        FieldKind::Condition { .. } => "condition",
        FieldKind::Renames { .. } => "renames",
    }
}

/// Canonical numeric encoding detail for a numeric [`FieldKind`].
fn numeric_detail(path: &str, kind: &FieldKind) -> Option<ManifestNumericDetail> {
    let (digits, scale, signed, encoding) = match kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => (
            u32::from(*digits),
            i32::from(*scale),
            *signed,
            "packed-decimal".to_owned(),
        ),
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            sign_separate,
        } => {
            let encoding = match sign_separate {
                None => "zoned",
                Some(info) => match info.placement {
                    SignPlacement::Leading => "zoned-sign-separate-leading",
                    SignPlacement::Trailing => "zoned-sign-separate-trailing",
                },
            }
            .to_owned();
            (u32::from(*digits), i32::from(*scale), *signed, encoding)
        }
        FieldKind::BinaryInt { bits, signed } => {
            (u32::from(*bits), 0, *signed, format!("binary-{bits}"))
        }
        FieldKind::FloatSingle => (0, 0, true, "float-single".to_owned()),
        FieldKind::FloatDouble => (0, 0, true, "float-double".to_owned()),
        FieldKind::Alphanum { .. }
        | FieldKind::EditedNumeric { .. }
        | FieldKind::Group
        | FieldKind::Condition { .. }
        | FieldKind::Renames { .. } => return None,
    };
    Some(ManifestNumericDetail {
        path: path.to_owned(),
        digits,
        scale,
        signed,
        encoding,
    })
}

/// Support-matrix feature referenced by one field kind, if any.
fn kind_feature(kind: &FieldKind) -> Option<FeatureId> {
    match kind {
        FieldKind::Condition { .. } => Some(FeatureId::Level88Conditions),
        FieldKind::Renames { .. } => Some(FeatureId::Level66Renames),
        FieldKind::EditedNumeric { .. } => Some(FeatureId::EditedPic),
        FieldKind::FloatSingle | FieldKind::FloatDouble => Some(FeatureId::Comp1Comp2),
        FieldKind::ZonedDecimal { sign_separate, .. } if sign_separate.is_some() => {
            Some(FeatureId::SignSeparate)
        }
        FieldKind::Alphanum { .. }
        | FieldKind::PackedDecimal { .. }
        | FieldKind::ZonedDecimal { .. }
        | FieldKind::BinaryInt { .. }
        | FieldKind::Group => None,
    }
}

/// Flatten layout fields depth-first into manifest detail vectors.
fn flatten_fields(
    fields: &[Field],
    odo_depth: usize,
    flat: &mut FlatLayout,
) -> Result<(), ManifestError> {
    for field in fields {
        flat.push_field_counted()?;
        flat.fields.push(ManifestField {
            path: field.path.clone(),
            level: field.level,
            offset: field.offset,
            len: field.len,
            end: field.offset.saturating_add(field.len),
            kind: kind_tag(&field.kind).to_owned(),
            redefines: field.redefines_of.clone(),
            synchronized: field.synchronized,
            blank_when_zero: field.blank_when_zero,
            occurs: match &field.occurs {
                Some(Occurs::Fixed { count }) => Some(ManifestOccurs {
                    kind: "fixed".to_owned(),
                    count: *count,
                    min_count: *count,
                    counter_path: None,
                }),
                Some(Occurs::ODO {
                    min,
                    max,
                    counter_path,
                }) => Some(ManifestOccurs {
                    kind: "odo".to_owned(),
                    count: *max,
                    min_count: *min,
                    counter_path: Some(counter_path.clone()),
                }),
                None => None,
            },
        });
        if let Some(detail) = numeric_detail(&field.path, &field.kind) {
            flat.numerics.push(detail);
        }
        let field_odo_depth = if let Some(Occurs::ODO {
            min,
            max,
            counter_path,
        }) = &field.occurs
        {
            flat.odos.push(ManifestOdoDetail {
                path: field.path.clone(),
                counter_path: counter_path.clone(),
                min_count: *min,
                max_count: *max,
                is_tail: false,
            });
            flat.feature_ids.push(FeatureId::OccursDepending);
            if odo_depth > 0 {
                flat.feature_ids.push(FeatureId::NestedOdo);
            }
            odo_depth + 1
        } else {
            odo_depth
        };
        if matches!(field.kind, FieldKind::Condition { .. }) {
            flat.conditions.push(ManifestConditionUsage {
                path: field.path.clone(),
            });
        }
        if let Some(aliased) = &field.resolved_renames {
            flat.renames.push(ManifestRenames {
                path: field.path.clone(),
                offset: aliased.offset,
                length: aliased.length,
                members: aliased.members.clone(),
            });
        }
        if let Some(feature) = kind_feature(&field.kind) {
            flat.feature_ids.push(feature);
        }
        flatten_fields(&field.children, field_odo_depth, flat)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_bytes_ignore_key_order() {
        let first = serde_json::json!({"b": 1, "a": {"d": [1, 2], "c": "x"}});
        let second = serde_json::json!({"a": {"c": "x", "d": [1, 2]}, "b": 1});
        assert_eq!(canonical_bytes(&first), canonical_bytes(&second));
        // Arrays keep their order: only object keys sort.
        let reordered = serde_json::json!({"a": {"c": "x", "d": [2, 1]}, "b": 1});
        assert_ne!(canonical_bytes(&first), canonical_bytes(&reordered));
    }

    /// Minimal version-2 body every wire test builds on.
    fn test_body() -> serde_json::Value {
        serde_json::json!({
            "schema_version": RESOLVED_MANIFEST_SCHEMA_VERSION,
            "stability_class": RESOLVED_MANIFEST_STABILITY_CLASS,
            "fingerprint_algo": RESOLVED_MANIFEST_FINGERPRINT_ALGO,
            "inputs": {
                "bundle": {"schema_version": 1, "fingerprint": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "root_unit": "REC"},
                "profile": {"schema_version": 1, "fingerprint": "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"},
                "tool": {"name": "copybook-test", "version": "0.0.0"},
                "encoding": {"value": "cp037", "source": "profile"},
                "dialect": {"value": "normative", "source": "profile", "provenance": "profile-selected"},
                "framing": {"value": "fixed", "source": "profile"},
                "record_bound": {"value": 32760, "source": "profile"}
            },
            "schema_fingerprint": "0e53dc4531f43574f6b5944646ef2180fb191ade8e087989c0ae321b3a33ba33",
            "fields": [],
            "record_len": 0,
            "record_len_min": 0,
            "lrecl": serde_json::Value::Null,
            "source_spans": MANIFEST_SOURCE_SPANS,
            "numeric_details": [],
            "odo_details": [],
            "condition_usages": [],
            "renames": [],
            "redefines_groups": [],
            "support": []
        })
    }

    #[test]
    fn additive_properties_verify_and_read() {
        // An additive property (unknown to this reader) verifies when the
        // fingerprint covers it, and reads back with known facts intact.
        let mut body = test_body();
        body["future_additive_prop"] = serde_json::json!({"note": "added by a newer writer"});
        let fingerprint = fingerprint_value(&body);
        let mut document = body;
        document["manifest_fingerprint"] = serde_json::Value::String(fingerprint);
        let bytes = serde_json::to_vec(&document).expect("document serializes");
        let manifest = ResolvedManifest::from_json(&bytes).expect("extended doc verifies");
        assert_eq!(manifest.record_len, 0);
        assert!(manifest.fields.is_empty());
    }

    #[test]
    fn unsigned_extra_property_breaks_verification() {
        // A full valid body is fingerprinted, then an extra property is
        // injected without updating the digest: verification must fail.
        let body = test_body();
        let fingerprint = fingerprint_value(&body);
        let mut document = body;
        document["injected"] = serde_json::Value::Bool(true);
        document["manifest_fingerprint"] = serde_json::Value::String(fingerprint);
        let bytes = serde_json::to_vec(&document).expect("document serializes");
        let error = ResolvedManifest::from_json(&bytes).expect_err("injection fails");
        assert!(
            matches!(error, ManifestError::FingerprintMismatch { .. }),
            "got {error}"
        );
    }
}
