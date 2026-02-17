//! COBOL feature support matrix registry
//!
//! Single source of truth for feature support status, used by CLI, docs, and CI.

use serde::{Deserialize, Serialize};

/// Stable identifier used in CLI flags, docs, and CI.
/// Keep these values stable once released.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum FeatureId {
    #[serde(rename = "level-88")]
    Level88Conditions,

    #[serde(rename = "level-66-renames")]
    Level66Renames,

    #[serde(rename = "occurs-depending")]
    OccursDepending,

    #[serde(rename = "edited-pic")]
    EditedPic,

    #[serde(rename = "comp-1-comp-2")]
    Comp1Comp2,

    #[serde(rename = "sign-separate")]
    SignSeparate,

    #[serde(rename = "nested-odo")]
    NestedOdo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum SupportStatus {
    Supported,
    Partial,
    Planned,
    NotPlanned,
}

#[derive(Debug, Clone, Serialize)]
pub struct FeatureSupport {
    /// Stable ID used in CLI and CI
    pub id: FeatureId,

    /// Human-readable name, e.g. "LEVEL 88 condition names"
    pub name: &'static str,

    /// Short human-readable description
    pub description: &'static str,

    /// Current support status
    pub status: SupportStatus,

    /// Optional pointer into docs (e.g. "COBOL_SUPPORT_MATRIX.md#level-88-condition-names")
    pub doc_ref: Option<&'static str>,
}

/// Returns the current support matrix as a static slice.
///
/// This is your single source of truth for feature support.
pub fn all_features() -> &'static [FeatureSupport] {
    use FeatureId::{
        Comp1Comp2, EditedPic, Level66Renames, Level88Conditions, NestedOdo, OccursDepending,
        SignSeparate,
    };
    use SupportStatus::{Partial, Planned, Supported};

    &[
        FeatureSupport {
            id: Level88Conditions,
            name: "LEVEL 88 condition names",
            description: "Condition-name VALUE clauses (space- and comma-separated).",
            status: Supported,
            doc_ref: Some("docs/reference/COBOL_SUPPORT_MATRIX.md#level-88-condition-names"),
        },
        FeatureSupport {
            id: Level66Renames,
            name: "LEVEL 66 RENAMES",
            description: "Non-storage renaming with same-scope and THRU support.",
            status: Partial,
            doc_ref: Some("docs/reference/COBOL_SUPPORT_MATRIX.md#level-66-renames"),
        },
        FeatureSupport {
            id: OccursDepending,
            name: "OCCURS DEPENDING ON",
            description: "Variable-length OCCURS; tail-only, no nesting.",
            status: Partial,
            doc_ref: Some("docs/reference/COBOL_SUPPORT_MATRIX.md#occurs-depending-on"),
        },
        FeatureSupport {
            id: EditedPic,
            name: "Edited PIC clauses",
            description: "Masks like PIC Z,ZZZ.99; full parse/decode/encode support.",
            status: Supported,
            doc_ref: Some("docs/reference/COBOL_SUPPORT_MATRIX.md#edited-pic"),
        },
        FeatureSupport {
            id: Comp1Comp2,
            name: "COMP-1/COMP-2 floating-point",
            description: "Single/double precision floating-point types (requires `comp_1`/`comp_2` feature flags).",
            status: Partial,
            doc_ref: None,
        },
        FeatureSupport {
            id: SignSeparate,
            name: "SIGN LEADING/TRAILING SEPARATE",
            description: "Separate sign byte directives (requires `sign_separate` feature flag).",
            status: Partial,
            doc_ref: Some("docs/reference/COBOL_SUPPORT_MATRIX.md#sign-handling"),
        },
        FeatureSupport {
            id: NestedOdo,
            name: "Nested OCCURS DEPENDING ON",
            description: "ODO arrays inside ODO arrays (O1-O4 supported, O5/O6 rejected).",
            status: Partial,
            doc_ref: Some("docs/reference/COBOL_SUPPORT_MATRIX.md#nested-odo--occurs-behavior---support-status"),
        },
    ]
}

/// Look up a feature by its stable ID (kebab-case)
pub fn find_feature(id: &str) -> Option<&'static FeatureSupport> {
    all_features()
        .iter()
        .find(|f| serde_plain::to_string(&f.id).ok().as_deref() == Some(id))
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_all_features_nonempty() {
        assert!(!all_features().is_empty());
    }

    #[test]
    fn test_find_feature_level88() {
        let feature = find_feature("level-88");
        assert!(feature.is_some());
        let f = feature.expect("feature should exist");
        assert_eq!(f.id, FeatureId::Level88Conditions);
        assert_eq!(f.status, SupportStatus::Supported);
    }

    #[test]
    fn test_find_feature_unknown() {
        let feature = find_feature("no-such-feature");
        assert!(feature.is_none());
    }

    #[test]
    fn test_feature_id_serde_roundtrip() {
        let id = FeatureId::Level88Conditions;
        let serialized = serde_plain::to_string(&id).expect("serialization should succeed");
        assert_eq!(serialized, "level-88");
    }
}
