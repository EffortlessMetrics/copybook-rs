// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep tests for the support matrix registry.
//!
//! Covers uniqueness invariants, serde edge cases, query consistency,
//! metadata quality, and structural invariants.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::{HashMap, HashSet};

use copybook_support_matrix::{
    FeatureId, FeatureSupport, SupportStatus, all_features, find_feature, find_feature_by_id,
};

// =========================================================================
// 1. All COBOL features have matrix entries
// =========================================================================

#[test]
fn all_seven_feature_id_variants_have_entries() {
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
            "FeatureId::{id:?} has no support matrix entry"
        );
    }
}

#[test]
fn all_features_length_matches_variant_count() {
    assert_eq!(all_features().len(), 7);
}

#[test]
fn all_features_ids_are_unique() {
    let ids: HashSet<FeatureId> = all_features().iter().map(|f| f.id).collect();
    assert_eq!(ids.len(), all_features().len());
}

#[test]
fn all_features_names_are_unique() {
    let names: HashSet<&str> = all_features().iter().map(|f| f.name).collect();
    assert_eq!(names.len(), all_features().len());
}

// =========================================================================
// 2. Status classification correctness
// =========================================================================

#[test]
fn supported_features_match_expected_list() {
    let expected_supported = [
        FeatureId::Level88Conditions,
        FeatureId::EditedPic,
        FeatureId::Comp1Comp2,
        FeatureId::SignSeparate,
    ];
    for id in expected_supported {
        let f = find_feature_by_id(id).unwrap();
        assert_eq!(
            f.status,
            SupportStatus::Supported,
            "{id:?} should be Supported"
        );
    }
}

#[test]
fn partial_features_match_expected_list() {
    let expected_partial = [
        FeatureId::Level66Renames,
        FeatureId::OccursDepending,
        FeatureId::NestedOdo,
    ];
    for id in expected_partial {
        let f = find_feature_by_id(id).unwrap();
        assert_eq!(f.status, SupportStatus::Partial, "{id:?} should be Partial");
    }
}

#[test]
fn no_features_currently_have_planned_or_not_planned_status() {
    for f in all_features() {
        assert!(
            f.status == SupportStatus::Supported || f.status == SupportStatus::Partial,
            "{:?} has unexpected status {:?}",
            f.id,
            f.status
        );
    }
}

// =========================================================================
// 3. Serde roundtrip coverage
// =========================================================================

#[test]
fn feature_id_serde_json_roundtrip_all_variants() {
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
        let json = serde_json::to_string(&id).unwrap();
        let back: FeatureId = serde_json::from_str(&json).unwrap();
        assert_eq!(back, id, "JSON roundtrip failed for {id:?}");
    }
}

#[test]
fn feature_id_serde_plain_values_match_expected_kebab_case() {
    let expected = [
        (FeatureId::Level88Conditions, "level-88"),
        (FeatureId::Level66Renames, "level-66-renames"),
        (FeatureId::OccursDepending, "occurs-depending"),
        (FeatureId::EditedPic, "edited-pic"),
        (FeatureId::Comp1Comp2, "comp-1-comp-2"),
        (FeatureId::SignSeparate, "sign-separate"),
        (FeatureId::NestedOdo, "nested-odo"),
    ];
    for (id, kebab) in expected {
        let serialized = serde_plain::to_string(&id).unwrap();
        assert_eq!(serialized, kebab, "serde_plain mismatch for {id:?}");
    }
}

#[test]
fn feature_id_deserialize_rejects_empty_string() {
    let result = serde_plain::from_str::<FeatureId>("");
    assert!(result.is_err());
}

#[test]
fn feature_id_deserialize_rejects_uppercase() {
    let result = serde_plain::from_str::<FeatureId>("LEVEL-88");
    assert!(result.is_err());
}

#[test]
fn support_status_serializes_to_expected_kebab_case() {
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

#[test]
fn feature_support_json_serialization_has_all_fields() {
    for f in all_features() {
        let json = serde_json::to_value(f).unwrap();
        assert!(json.get("id").is_some(), "{:?} missing id", f.id);
        assert!(json.get("name").is_some(), "{:?} missing name", f.id);
        assert!(
            json.get("description").is_some(),
            "{:?} missing description",
            f.id
        );
        assert!(json.get("status").is_some(), "{:?} missing status", f.id);
        assert!(json.get("doc_ref").is_some(), "{:?} missing doc_ref", f.id);
    }
}

#[test]
fn all_features_array_serializes_correctly() {
    let json = serde_json::to_value(all_features()).unwrap();
    let arr = json.as_array().unwrap();
    assert_eq!(arr.len(), 7);
}

// =========================================================================
// 4. Query consistency
// =========================================================================

#[test]
fn find_feature_string_and_id_lookup_return_identical_entries() {
    for f in all_features() {
        let kebab = serde_plain::to_string(&f.id).unwrap();
        let by_str = find_feature(&kebab).unwrap();
        let by_id = find_feature_by_id(f.id).unwrap();
        assert_eq!(by_str.id, by_id.id);
        assert_eq!(by_str.name, by_id.name);
        assert_eq!(by_str.description, by_id.description);
        assert_eq!(by_str.status, by_id.status);
        assert_eq!(by_str.doc_ref, by_id.doc_ref);
    }
}

#[test]
fn find_feature_returns_none_for_various_invalid_inputs() {
    let invalid_inputs = [
        "",
        " ",
        "Level-88", // wrong case
        "level_88", // underscore instead of kebab
        "level88",  // no separator
        "unknown",
        "level-88-conditions", // too-long variant
    ];
    for input in invalid_inputs {
        assert!(
            find_feature(input).is_none(),
            "find_feature({input:?}) should return None"
        );
    }
}

// =========================================================================
// 5. Metadata quality
// =========================================================================

#[test]
fn all_entries_have_substantive_descriptions() {
    for f in all_features() {
        assert!(
            f.description.len() >= 15,
            "{:?} has too-short description ({}): '{}'",
            f.id,
            f.description.len(),
            f.description
        );
    }
}

#[test]
fn all_entries_have_doc_refs_starting_with_docs() {
    for f in all_features() {
        let doc_ref = f
            .doc_ref
            .unwrap_or_else(|| panic!("{:?} missing doc_ref", f.id));
        assert!(
            doc_ref.starts_with("docs/"),
            "{:?} doc_ref should start with 'docs/': {doc_ref}",
            f.id
        );
    }
}

#[test]
fn no_name_or_description_has_leading_trailing_whitespace() {
    for f in all_features() {
        assert_eq!(f.name, f.name.trim(), "{:?} name has whitespace", f.id);
        assert_eq!(
            f.description,
            f.description.trim(),
            "{:?} description has whitespace",
            f.id
        );
    }
}

#[test]
fn doc_refs_are_unique_across_entries() {
    let refs: HashSet<&str> = all_features().iter().filter_map(|f| f.doc_ref).collect();
    // Each feature should point to a distinct doc section
    assert_eq!(refs.len(), all_features().len());
}

// =========================================================================
// 6. FeatureId as collection key
// =========================================================================

#[test]
fn feature_id_works_as_hashmap_key_with_all_entries() {
    let mut map: HashMap<FeatureId, &FeatureSupport> = HashMap::new();
    for f in all_features() {
        map.insert(f.id, f);
    }
    assert_eq!(map.len(), 7);

    // Verify lookup works
    let edited = map.get(&FeatureId::EditedPic).unwrap();
    assert_eq!(edited.id, FeatureId::EditedPic);
    assert_eq!(edited.status, SupportStatus::Supported);
}

#[test]
fn feature_id_hashset_deduplicates_correctly() {
    let mut set = HashSet::new();
    for f in all_features() {
        assert!(set.insert(f.id), "duplicate id: {:?}", f.id);
    }
    // Insert again - should all be rejected
    for f in all_features() {
        assert!(!set.insert(f.id));
    }
    assert_eq!(set.len(), 7);
}

// =========================================================================
// 7. Debug output quality
// =========================================================================

#[test]
fn feature_id_debug_contains_variant_name_for_all() {
    let expected_names = [
        (FeatureId::Level88Conditions, "Level88"),
        (FeatureId::Level66Renames, "Level66"),
        (FeatureId::OccursDepending, "OccursDepending"),
        (FeatureId::EditedPic, "EditedPic"),
        (FeatureId::Comp1Comp2, "Comp1Comp2"),
        (FeatureId::SignSeparate, "SignSeparate"),
        (FeatureId::NestedOdo, "NestedOdo"),
    ];
    for (id, substr) in expected_names {
        let dbg = format!("{id:?}");
        assert!(
            dbg.contains(substr),
            "{id:?} Debug missing '{substr}': {dbg}"
        );
    }
}

#[test]
fn feature_support_debug_includes_id_and_status() {
    for f in all_features() {
        let dbg = format!("{f:?}");
        assert!(!dbg.is_empty());
        // Debug should contain the variant name
        let id_str = format!("{:?}", f.id);
        assert!(
            dbg.contains(&id_str),
            "FeatureSupport Debug missing id for {:?}: {dbg}",
            f.id
        );
    }
}

// =========================================================================
// 8. Clone semantics
// =========================================================================

#[test]
fn feature_support_clone_preserves_all_fields() {
    for f in all_features() {
        let cloned: FeatureSupport = f.clone();
        assert_eq!(cloned.id, f.id);
        assert_eq!(cloned.name, f.name);
        assert_eq!(cloned.description, f.description);
        assert_eq!(cloned.status, f.status);
        assert_eq!(cloned.doc_ref, f.doc_ref);
    }
}

#[test]
fn feature_id_copy_semantics_verified() {
    for f in all_features() {
        let id = f.id;
        let copied = id; // Copy
        assert_eq!(id, copied);
    }
}

#[test]
fn support_status_copy_semantics_verified() {
    let s = SupportStatus::Supported;
    let copied = s;
    assert_eq!(s, copied);
}
