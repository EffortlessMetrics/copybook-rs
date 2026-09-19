// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::missing_inline_in_public_items)]
//! Resolved-schema manifest: a stable, reviewable snapshot of one resolved schema.
//!
//! The manifest binds the four resolved-schema facts an operator reviews: the
//! input bundle fingerprint, the effective option values with provenance (from
//! [`resolve`](crate::options::resolve) layers), the resolved layout (field
//! paths with offsets, lengths, and physical bounds), and the support
//! classification drawn from the static support matrix. It is the inspectable
//! artifact behind `copybook parse --emit-manifest` and the `ResolvedManifest`
//! contract of #1117.

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fmt;

use copybook_core::dialect::Dialect;
use copybook_core::schema::{Field, FieldKind, Occurs, Schema, SignPlacement};
use copybook_core::source_bundle::{
    BundleError, DialectProvenance, SourceBundle, resolve_effective_dialect,
};
use copybook_core::support_matrix::{FeatureId, SupportStatus, find_feature_by_id};

use crate::options::resolve::{OptionSource, Resolved};

/// Schema version of the resolved-manifest JSON document.
pub const RESOLVED_MANIFEST_SCHEMA_VERSION: u32 = 1;
/// Stability class of the resolved-manifest contract.
pub const RESOLVED_MANIFEST_STABILITY_CLASS: &str = "stable";
/// Hash algorithm used for [`ResolvedManifest::manifest_fingerprint`].
pub const RESOLVED_MANIFEST_FINGERPRINT_ALGO: &str = "sha256-v1";
/// Maximum number of flattened layout fields admitted into a manifest.
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
    /// The flattened layout exceeds [`MAX_MANIFEST_FIELDS`].
    TooManyFields {
        /// Number of entries the resolved layout contains.
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
            Self::TooManyFields { found } => write!(
                f,
                "resolved layout has {found} entries, exceeding the limit of {MAX_MANIFEST_FIELDS}"
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
            Self::FingerprintMismatch { expected, actual } => write!(
                f,
                "resolved-manifest fingerprint mismatch: expected {expected}, recomputed {actual}"
            ),
        }
    }
}

impl std::error::Error for ManifestError {}

/// One resolved value plus the layer that supplied it, as recorded.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestValue<T> {
    /// Resolved value.
    pub value: T,
    /// Resolution layer in [`OptionSource`] display spelling.
    pub source: String,
}

/// Effective dialect with both provenance chains recorded.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestDialect {
    /// Effective dialect in profile spelling.
    pub value: String,
    /// Resolution layer in [`OptionSource`] display spelling.
    pub source: String,
    /// Why this dialect was selected, in canonical spelling.
    pub provenance: String,
}

/// Resolved inputs captured for one manifest.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestInputs {
    /// Bundle fingerprint the schema was resolved from.
    pub bundle_fingerprint: String,
    /// Encoding actually used plus its source.
    pub encoding: ManifestValue<String>,
    /// Dialect actually used plus both provenance chains.
    pub dialect: ManifestDialect,
    /// Framing actually used plus its source.
    pub framing: ManifestValue<String>,
    /// Record bound actually used plus its source.
    pub record_bound: ManifestValue<u64>,
}

/// One flattened layout field with its physical bounds.
#[derive(Debug, Clone, Serialize, Deserialize)]
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
    /// Path of the redefined target when this field carries REDEFINES.
    pub redefines: Option<String>,
    /// Whether this field carries a SYNCHRONIZED clause.
    pub synchronized: bool,
    /// Whether this field carries a BLANK WHEN ZERO clause.
    pub blank_when_zero: bool,
}

/// Numeric usage detail for one flattened field.
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestConditionUsage {
    /// Dotted path of the condition-name field.
    pub path: String,
}

/// Support classification of one feature referenced by the resolved schema.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ManifestSupportEntry {
    /// Feature identifier in kebab-case.
    pub feature: String,
    /// Support status in kebab-case (`supported`, `partial`, ...).
    pub status: String,
}

/// Inputs accepted by [`ResolvedManifest::generate`].
pub struct GenerateInputs<'a> {
    /// Source bundle the schema was parsed from.
    pub bundle: &'a SourceBundle,
    /// Effective encoding after flag/profile/env/default resolution.
    pub encoding: Resolved<String>,
    /// Selected dialect after flag/profile/env/default resolution.
    pub dialect: Resolved<Dialect>,
    /// Effective framing after flag/profile/env/default resolution.
    pub framing: Resolved<String>,
    /// Effective record bound after flag/profile/env/default resolution.
    pub record_bound: Resolved<u64>,
    /// Schema after [`resolve_layout`](copybook_core::layout::resolve_layout).
    pub schema: &'a Schema,
}

/// A stable, reviewable snapshot of one resolved schema: inputs, effective
/// values with provenance, flattened layout with physical bounds, and support
/// classification, bound together by a fingerprint.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedManifest {
    /// Manifest document schema version.
    pub schema_version: u32,
    /// Stability class of this contract.
    pub stability_class: String,
    /// Fingerprint algorithm (see [`RESOLVED_MANIFEST_FINGERPRINT_ALGO`]).
    pub fingerprint_algo: String,
    /// Resolved inputs with per-value provenance.
    pub inputs: ManifestInputs,
    /// Flattened layout fields with physical bounds.
    pub fields: Vec<ManifestField>,
    /// Maximum field end offset across the flattened layout, in bytes.
    pub record_len: u32,
    /// Declared fixed record length (`lrecl`), if the schema states one.
    pub lrecl: Option<u32>,
    /// Numeric usage details.
    pub numeric_details: Vec<ManifestNumericDetail>,
    /// ODO usage details.
    pub odo_details: Vec<ManifestOdoDetail>,
    /// Level-88 condition-name usages.
    pub condition_usages: Vec<ManifestConditionUsage>,
    /// Support classifications for features the schema references.
    pub support: Vec<ManifestSupportEntry>,
    /// Self-fingerprint binding every field above.
    pub manifest_fingerprint: String,
}

/// Stored manifest document: the serializable form plus its fingerprint.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct StoredManifest {
    #[serde(flatten)]
    body: StoredManifestBody,
    manifest_fingerprint: String,
}

/// Stored manifest body: every [`ResolvedManifest`] field except the fingerprint.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct StoredManifestBody {
    schema_version: u32,
    stability_class: String,
    fingerprint_algo: String,
    inputs: ManifestInputs,
    fields: Vec<ManifestField>,
    record_len: u32,
    lrecl: Option<u32>,
    numeric_details: Vec<ManifestNumericDetail>,
    odo_details: Vec<ManifestOdoDetail>,
    condition_usages: Vec<ManifestConditionUsage>,
    support: Vec<ManifestSupportEntry>,
}

impl ResolvedManifest {
    /// Generate a manifest from resolved inputs, binding all four fact classes.
    ///
    /// # Errors
    ///
    /// Returns [`ManifestError`] when dialect resolution conflicts, the layout
    /// exceeds [`MAX_MANIFEST_FIELDS`], or the serialized form exceeds
    /// [`MAX_MANIFEST_BYTES`].
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

        let mut manifest = Self {
            schema_version: RESOLVED_MANIFEST_SCHEMA_VERSION,
            stability_class: RESOLVED_MANIFEST_STABILITY_CLASS.to_owned(),
            fingerprint_algo: RESOLVED_MANIFEST_FINGERPRINT_ALGO.to_owned(),
            inputs: ManifestInputs {
                bundle_fingerprint: inputs.bundle.fingerprint().to_owned(),
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
                record_bound: ManifestValue {
                    value: inputs.record_bound.value,
                    source: source_str(inputs.record_bound.source),
                },
            },
            fields: flat.fields,
            record_len,
            lrecl: inputs.schema.lrecl_fixed,
            numeric_details: flat.numerics,
            odo_details: flat.odos,
            condition_usages: flat.conditions,
            support,
            manifest_fingerprint: String::new(),
        };
        let body = StoredManifestBody::from_manifest(&manifest);
        let serialized =
            serde_json::to_vec(&body).map_err(|err| ManifestError::MalformedManifest {
                reason: err.to_string(),
            })?;
        if serialized.len() > MAX_MANIFEST_BYTES {
            return Err(ManifestError::ManifestTooLarge {
                found: serialized.len(),
            });
        }
        manifest.manifest_fingerprint = fingerprint_bytes(&serialized);
        Ok(manifest)
    }

    /// Serialize this manifest to canonical JSON, enforcing the size bound.
    ///
    /// # Errors
    ///
    /// Returns [`ManifestError::ManifestTooLarge`] when the serialized form
    /// exceeds [`MAX_MANIFEST_BYTES`].
    pub fn to_json(&self) -> Result<Vec<u8>, ManifestError> {
        let stored = StoredManifest {
            body: StoredManifestBody::from_manifest(self),
            manifest_fingerprint: self.manifest_fingerprint.clone(),
        };
        let serialized =
            serde_json::to_vec_pretty(&stored).map_err(|err| ManifestError::MalformedManifest {
                reason: err.to_string(),
            })?;
        if serialized.len() > MAX_MANIFEST_BYTES {
            return Err(ManifestError::ManifestTooLarge {
                found: serialized.len(),
            });
        }
        Ok(serialized)
    }

    /// Parse and verify a stored manifest: version and fingerprint.
    ///
    /// # Errors
    ///
    /// Returns [`ManifestError`] when the document is malformed, declares an
    /// unsupported version, or fails fingerprint verification.
    pub fn from_json(bytes: &[u8]) -> Result<Self, ManifestError> {
        if bytes.len() > MAX_MANIFEST_BYTES {
            return Err(ManifestError::ManifestTooLarge { found: bytes.len() });
        }
        let stored: StoredManifest =
            serde_json::from_slice(bytes).map_err(|err| ManifestError::MalformedManifest {
                reason: err.to_string(),
            })?;
        if stored.body.schema_version != RESOLVED_MANIFEST_SCHEMA_VERSION {
            return Err(ManifestError::UnsupportedManifestVersion {
                found: stored.body.schema_version,
            });
        }
        let body_bytes =
            serde_json::to_vec(&stored.body).map_err(|err| ManifestError::MalformedManifest {
                reason: err.to_string(),
            })?;
        let recomputed = fingerprint_bytes(&body_bytes);
        if recomputed != stored.manifest_fingerprint {
            return Err(ManifestError::FingerprintMismatch {
                expected: stored.manifest_fingerprint,
                actual: recomputed,
            });
        }
        Ok(stored.body.into_manifest(stored.manifest_fingerprint))
    }
}

impl StoredManifestBody {
    fn from_manifest(manifest: &ResolvedManifest) -> Self {
        Self {
            schema_version: manifest.schema_version,
            stability_class: manifest.stability_class.clone(),
            fingerprint_algo: manifest.fingerprint_algo.clone(),
            inputs: manifest.inputs.clone(),
            fields: manifest.fields.clone(),
            record_len: manifest.record_len,
            lrecl: manifest.lrecl,
            numeric_details: manifest.numeric_details.clone(),
            odo_details: manifest.odo_details.clone(),
            condition_usages: manifest.condition_usages.clone(),
            support: manifest.support.clone(),
        }
    }

    fn into_manifest(self, manifest_fingerprint: String) -> ResolvedManifest {
        ResolvedManifest {
            schema_version: self.schema_version,
            stability_class: self.stability_class,
            fingerprint_algo: self.fingerprint_algo,
            inputs: self.inputs,
            fields: self.fields,
            record_len: self.record_len,
            lrecl: self.lrecl,
            numeric_details: self.numeric_details,
            odo_details: self.odo_details,
            condition_usages: self.condition_usages,
            support: self.support,
            manifest_fingerprint,
        }
    }
}

/// Flattened layout accumulation during one [`ResolvedManifest::generate`].
#[derive(Debug, Default)]
struct FlatLayout {
    fields: Vec<ManifestField>,
    numerics: Vec<ManifestNumericDetail>,
    odos: Vec<ManifestOdoDetail>,
    conditions: Vec<ManifestConditionUsage>,
    feature_ids: Vec<FeatureId>,
}

impl FlatLayout {
    /// Number of manifest entries accumulated so far.
    fn len(&self) -> usize {
        self.fields.len() + self.numerics.len() + self.odos.len() + self.conditions.len()
    }

    /// Record one entry, enforcing [`MAX_MANIFEST_FIELDS`].
    fn push_counted(&self) -> Result<(), ManifestError> {
        if self.len() >= MAX_MANIFEST_FIELDS {
            return Err(ManifestError::TooManyFields {
                found: self.len() + 1,
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
        flat.push_counted()?;
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
        });
        if let Some(detail) = numeric_detail(&field.path, &field.kind) {
            flat.push_counted()?;
            flat.numerics.push(detail);
        }
        let field_odo_depth = if let Some(Occurs::ODO {
            min,
            max,
            counter_path,
        }) = &field.occurs
        {
            flat.push_counted()?;
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
            flat.push_counted()?;
            flat.conditions.push(ManifestConditionUsage {
                path: field.path.clone(),
            });
        }
        if let Some(feature) = kind_feature(&field.kind) {
            flat.feature_ids.push(feature);
        }
        flatten_fields(&field.children, field_odo_depth, flat)?;
    }
    Ok(())
}
