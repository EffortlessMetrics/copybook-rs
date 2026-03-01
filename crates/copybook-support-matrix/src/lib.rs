// SPDX-License-Identifier: AGPL-3.0-or-later
//! COBOL feature support matrix registry.

use serde::{Deserialize, Serialize};

/// Identifier for a COBOL feature tracked in the support matrix.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[non_exhaustive]
pub enum FeatureId {
    /// Level-88 condition name VALUE clauses.
    #[serde(rename = "level-88")]
    Level88Conditions,

    /// Level-66 RENAMES non-storage renaming.
    #[serde(rename = "level-66-renames")]
    Level66Renames,

    /// Variable-length OCCURS DEPENDING ON arrays.
    #[serde(rename = "occurs-depending")]
    OccursDepending,

    /// Edited numeric PICTURE clauses (e.g. `PIC Z,ZZZ.99`).
    #[serde(rename = "edited-pic")]
    EditedPic,

    /// COMP-1 / COMP-2 IEEE 754 floating-point types.
    #[serde(rename = "comp-1-comp-2")]
    Comp1Comp2,

    /// SIGN LEADING / TRAILING SEPARATE directives.
    #[serde(rename = "sign-separate")]
    SignSeparate,

    /// Nested OCCURS DEPENDING ON (ODO inside ODO).
    #[serde(rename = "nested-odo")]
    NestedOdo,
}

/// Current implementation status of a COBOL feature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
#[non_exhaustive]
pub enum SupportStatus {
    /// Fully implemented and tested.
    Supported,
    /// Partially implemented with known limitations.
    Partial,
    /// Planned for a future release.
    Planned,
    /// Not planned for implementation.
    NotPlanned,
}

/// Metadata entry for a single COBOL feature in the support matrix.
#[derive(Debug, Clone, Serialize)]
pub struct FeatureSupport {
    /// Unique identifier for this feature.
    pub id: FeatureId,
    /// Human-readable feature name.
    pub name: &'static str,
    /// Brief description of the feature.
    pub description: &'static str,
    /// Current implementation status.
    pub status: SupportStatus,
    /// Path to the relevant documentation section, if any.
    pub doc_ref: Option<&'static str>,
}

/// Returns the complete list of tracked COBOL features and their support status.
#[inline]
#[must_use]
pub fn all_features() -> &'static [FeatureSupport] {
    use FeatureId::{
        Comp1Comp2, EditedPic, Level66Renames, Level88Conditions, NestedOdo, OccursDepending,
        SignSeparate,
    };
    use SupportStatus::{Partial, Supported};

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
            description: "Single/double precision IEEE 754 floating-point types; enabled by default.",
            status: Supported,
            doc_ref: Some("docs/reference/COBOL_SUPPORT_MATRIX.md#data-types"),
        },
        FeatureSupport {
            id: SignSeparate,
            name: "SIGN LEADING/TRAILING SEPARATE",
            description: "Separate sign byte directives; enabled by default.",
            status: Supported,
            doc_ref: Some("docs/reference/COBOL_SUPPORT_MATRIX.md#sign-handling"),
        },
        FeatureSupport {
            id: NestedOdo,
            name: "Nested OCCURS DEPENDING ON",
            description: "ODO arrays inside ODO arrays (O1-O4 supported, O5/O6 rejected).",
            status: Partial,
            doc_ref: Some(
                "docs/reference/COBOL_SUPPORT_MATRIX.md#nested-odo--occurs-behavior---support-status",
            ),
        },
    ]
}

/// Looks up a feature by its [`FeatureId`] enum variant.
#[inline]
#[must_use]
pub fn find_feature_by_id(id: FeatureId) -> Option<&'static FeatureSupport> {
    all_features().iter().find(|f| f.id == id)
}

/// Looks up a feature by its kebab-case string identifier (e.g. `"level-88"`).
#[inline]
#[must_use]
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
    fn test_find_feature_by_id() {
        let feature = find_feature_by_id(FeatureId::SignSeparate);
        assert!(feature.is_some());
    }

    #[test]
    fn test_feature_id_serde_roundtrip() {
        let id = FeatureId::Level88Conditions;
        let serialized = serde_plain::to_string(&id).expect("serialization should succeed");
        assert_eq!(serialized, "level-88");
    }

    #[test]
    fn test_all_features_returns_seven_entries() {
        assert_eq!(all_features().len(), 7);
    }

    #[test]
    fn test_find_feature_by_id_all_variants() {
        let ids = [
            FeatureId::Level88Conditions,
            FeatureId::Level66Renames,
            FeatureId::OccursDepending,
            FeatureId::EditedPic,
            FeatureId::Comp1Comp2,
            FeatureId::SignSeparate,
            FeatureId::NestedOdo,
        ];
        for id in ids {
            assert!(
                find_feature_by_id(id).is_some(),
                "missing feature for {id:?}"
            );
        }
    }

    #[test]
    fn test_find_feature_all_kebab_strings() {
        let names = [
            "level-88",
            "level-66-renames",
            "occurs-depending",
            "edited-pic",
            "comp-1-comp-2",
            "sign-separate",
            "nested-odo",
        ];
        for name in names {
            assert!(
                find_feature(name).is_some(),
                "missing feature for string '{name}'"
            );
        }
    }

    #[test]
    fn test_all_features_have_nonempty_name_and_description() {
        for f in all_features() {
            assert!(!f.name.is_empty(), "feature {:?} has empty name", f.id);
            assert!(
                !f.description.is_empty(),
                "feature {:?} has empty description",
                f.id
            );
        }
    }

    #[test]
    fn test_all_features_have_doc_ref() {
        for f in all_features() {
            assert!(
                f.doc_ref.is_some(),
                "feature {:?} should have a doc_ref",
                f.id
            );
        }
    }

    #[test]
    fn test_supported_status_for_known_features() {
        let f = find_feature_by_id(FeatureId::Level88Conditions).unwrap();
        assert_eq!(f.status, SupportStatus::Supported);

        let f = find_feature_by_id(FeatureId::EditedPic).unwrap();
        assert_eq!(f.status, SupportStatus::Supported);

        let f = find_feature_by_id(FeatureId::Comp1Comp2).unwrap();
        assert_eq!(f.status, SupportStatus::Supported);

        let f = find_feature_by_id(FeatureId::SignSeparate).unwrap();
        assert_eq!(f.status, SupportStatus::Supported);
    }

    #[test]
    fn test_partial_status_for_known_features() {
        let f = find_feature_by_id(FeatureId::Level66Renames).unwrap();
        assert_eq!(f.status, SupportStatus::Partial);

        let f = find_feature_by_id(FeatureId::OccursDepending).unwrap();
        assert_eq!(f.status, SupportStatus::Partial);

        let f = find_feature_by_id(FeatureId::NestedOdo).unwrap();
        assert_eq!(f.status, SupportStatus::Partial);
    }

    #[test]
    fn test_feature_id_serde_all_variants() {
        let expected = [
            (FeatureId::Level88Conditions, "level-88"),
            (FeatureId::Level66Renames, "level-66-renames"),
            (FeatureId::OccursDepending, "occurs-depending"),
            (FeatureId::EditedPic, "edited-pic"),
            (FeatureId::Comp1Comp2, "comp-1-comp-2"),
            (FeatureId::SignSeparate, "sign-separate"),
            (FeatureId::NestedOdo, "nested-odo"),
        ];
        for (id, expected_str) in expected {
            let s = serde_plain::to_string(&id).unwrap();
            assert_eq!(s, expected_str, "serde mismatch for {id:?}");
        }
    }

    #[test]
    fn test_feature_id_deserialize_roundtrip() {
        let id = FeatureId::EditedPic;
        let s = serde_plain::to_string(&id).unwrap();
        let back: FeatureId = serde_plain::from_str(&s).unwrap();
        assert_eq!(back, id);
    }

    #[test]
    fn test_support_status_serialization() {
        let json = serde_json::to_string(&SupportStatus::Supported).unwrap();
        assert_eq!(json, "\"supported\"");
        let json = serde_json::to_string(&SupportStatus::Partial).unwrap();
        assert_eq!(json, "\"partial\"");
        let json = serde_json::to_string(&SupportStatus::Planned).unwrap();
        assert_eq!(json, "\"planned\"");
        let json = serde_json::to_string(&SupportStatus::NotPlanned).unwrap();
        assert_eq!(json, "\"not-planned\"");
    }

    #[test]
    fn test_feature_support_json_serialization() {
        let f = find_feature_by_id(FeatureId::Level88Conditions).unwrap();
        let json = serde_json::to_value(f).unwrap();
        assert_eq!(json["id"], "level-88");
        assert_eq!(json["status"], "supported");
        assert!(json["name"].is_string());
        assert!(json["description"].is_string());
    }

    #[test]
    fn test_no_duplicate_feature_ids() {
        let ids: Vec<FeatureId> = all_features().iter().map(|f| f.id).collect();
        for (i, id) in ids.iter().enumerate() {
            assert!(!ids[i + 1..].contains(id), "duplicate feature id: {id:?}");
        }
    }

    #[test]
    fn test_find_feature_empty_string_returns_none() {
        assert!(find_feature("").is_none());
    }
}
