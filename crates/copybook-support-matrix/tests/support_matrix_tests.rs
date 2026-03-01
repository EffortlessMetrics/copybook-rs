// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for copybook-support-matrix registry.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_support_matrix::{
    FeatureId, FeatureSupport, SupportStatus, all_features, find_feature, find_feature_by_id,
};

// ── Registry completeness ───────────────────────────────────────────────────

#[test]
fn registry_contains_exactly_seven_entries() {
    assert_eq!(all_features().len(), 7);
}

#[test]
fn every_feature_id_variant_is_registered() {
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
        assert!(find_feature_by_id(id).is_some(), "missing: {id:?}");
    }
}

#[test]
fn no_duplicate_ids_in_registry() {
    let features = all_features();
    for (i, a) in features.iter().enumerate() {
        for b in &features[i + 1..] {
            assert_ne!(a.id, b.id, "duplicate id: {:?}", a.id);
        }
    }
}

// ── find_feature (string lookup) ────────────────────────────────────────────

#[test]
fn find_feature_returns_correct_entry_for_all_kebab_ids() {
    let mapping: &[(&str, FeatureId)] = &[
        ("level-88", FeatureId::Level88Conditions),
        ("level-66-renames", FeatureId::Level66Renames),
        ("occurs-depending", FeatureId::OccursDepending),
        ("edited-pic", FeatureId::EditedPic),
        ("comp-1-comp-2", FeatureId::Comp1Comp2),
        ("sign-separate", FeatureId::SignSeparate),
        ("nested-odo", FeatureId::NestedOdo),
    ];
    for (name, expected_id) in mapping {
        let f = find_feature(name).unwrap_or_else(|| panic!("missing: {name}"));
        assert_eq!(f.id, *expected_id);
    }
}

#[test]
fn find_feature_returns_none_for_unknown() {
    assert!(find_feature("nonexistent").is_none());
    assert!(find_feature("").is_none());
    assert!(find_feature("LEVEL-88").is_none()); // case-sensitive kebab
}

// ── find_feature_by_id ──────────────────────────────────────────────────────

#[test]
fn find_feature_by_id_returns_matching_metadata() {
    let f = find_feature_by_id(FeatureId::Level88Conditions).unwrap();
    assert_eq!(f.id, FeatureId::Level88Conditions);
    assert!(!f.name.is_empty());
    assert!(!f.description.is_empty());
    assert!(f.doc_ref.is_some());
}

// ── SupportStatus classification ────────────────────────────────────────────

#[test]
fn supported_features_are_correctly_classified() {
    let supported_ids = [
        FeatureId::Level88Conditions,
        FeatureId::EditedPic,
        FeatureId::Comp1Comp2,
        FeatureId::SignSeparate,
    ];
    for id in supported_ids {
        let f = find_feature_by_id(id).unwrap();
        assert_eq!(
            f.status,
            SupportStatus::Supported,
            "{:?} should be Supported",
            id
        );
    }
}

#[test]
fn partial_features_are_correctly_classified() {
    let partial_ids = [
        FeatureId::Level66Renames,
        FeatureId::OccursDepending,
        FeatureId::NestedOdo,
    ];
    for id in partial_ids {
        let f = find_feature_by_id(id).unwrap();
        assert_eq!(
            f.status,
            SupportStatus::Partial,
            "{:?} should be Partial",
            id
        );
    }
}

// ── FeatureId serde ─────────────────────────────────────────────────────────

#[test]
fn feature_id_serde_plain_roundtrip_all_variants() {
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
        let s = serde_plain::to_string(&id).unwrap();
        let back: FeatureId = serde_plain::from_str(&s).unwrap();
        assert_eq!(back, id, "serde_plain roundtrip failed for {id:?}");
    }
}

#[test]
fn feature_id_json_roundtrip() {
    let id = FeatureId::Comp1Comp2;
    let json = serde_json::to_string(&id).unwrap();
    let back: FeatureId = serde_json::from_str(&json).unwrap();
    assert_eq!(back, id);
}

// ── SupportStatus serde ─────────────────────────────────────────────────────

#[test]
fn support_status_serializes_to_kebab_case() {
    assert_eq!(
        serde_json::to_string(&SupportStatus::Supported).unwrap(),
        "\"supported\""
    );
    assert_eq!(
        serde_json::to_string(&SupportStatus::Partial).unwrap(),
        "\"partial\""
    );
    assert_eq!(
        serde_json::to_string(&SupportStatus::Planned).unwrap(),
        "\"planned\""
    );
    assert_eq!(
        serde_json::to_string(&SupportStatus::NotPlanned).unwrap(),
        "\"not-planned\""
    );
}

// ── FeatureSupport metadata quality ─────────────────────────────────────────

#[test]
fn all_entries_have_nonempty_name_and_description() {
    for f in all_features() {
        assert!(!f.name.is_empty(), "{:?} has empty name", f.id);
        assert!(
            !f.description.is_empty(),
            "{:?} has empty description",
            f.id
        );
    }
}

#[test]
fn all_entries_have_doc_ref() {
    for f in all_features() {
        let doc_ref = f
            .doc_ref
            .unwrap_or_else(|| panic!("{:?} missing doc_ref", f.id));
        assert!(
            doc_ref.starts_with("docs/"),
            "{:?} doc_ref should start with docs/",
            f.id
        );
    }
}

#[test]
fn feature_support_json_has_expected_shape() {
    let f = find_feature_by_id(FeatureId::EditedPic).unwrap();
    let json = serde_json::to_value(f).unwrap();
    assert!(json.get("id").is_some());
    assert!(json.get("name").is_some());
    assert!(json.get("description").is_some());
    assert!(json.get("status").is_some());
    assert!(json.get("doc_ref").is_some());
}

#[test]
fn feature_support_clone_preserves_identity() {
    let f = find_feature_by_id(FeatureId::Level88Conditions).unwrap();
    let cloned: FeatureSupport = f.clone();
    assert_eq!(cloned.id, f.id);
    assert_eq!(cloned.name, f.name);
    assert_eq!(cloned.status, f.status);
}
