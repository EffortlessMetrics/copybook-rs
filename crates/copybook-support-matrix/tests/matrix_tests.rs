// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-support-matrix: identifiers, statuses,
//! metadata quality, query consistency, serde edge cases, and Debug output.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::collections::{HashMap, HashSet};

use copybook_support_matrix::{
    FeatureId, SupportStatus, all_features, find_feature, find_feature_by_id,
};

// ── 1. FeatureId – identity, equality, hashing, Copy ───────────────────────

#[test]
fn feature_id_copy_semantics() {
    let a = FeatureId::EditedPic;
    let b = a; // Copy
    assert_eq!(a, b, "FeatureId should implement Copy");
}

#[test]
fn feature_id_clone_equals_original() {
    let a = FeatureId::OccursDepending;
    #[allow(clippy::clone_on_copy)]
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn feature_id_hash_consistency() {
    let mut set = HashSet::new();
    set.insert(FeatureId::Level88Conditions);
    set.insert(FeatureId::Level88Conditions); // duplicate
    assert_eq!(set.len(), 1, "identical FeatureIds must hash the same");
}

#[test]
fn feature_id_all_variants_are_distinct() {
    let ids = [
        FeatureId::Level88Conditions,
        FeatureId::Level66Renames,
        FeatureId::OccursDepending,
        FeatureId::EditedPic,
        FeatureId::Comp1Comp2,
        FeatureId::SignSeparate,
        FeatureId::NestedOdo,
    ];
    let set: HashSet<FeatureId> = ids.iter().copied().collect();
    assert_eq!(
        set.len(),
        ids.len(),
        "all FeatureId variants must be distinct"
    );
}

// ── 2. SupportStatus – variants, equality, Copy ────────────────────────────

#[test]
fn support_status_four_variants_exist() {
    // Ensures all four status values can be constructed.
    let statuses = [
        SupportStatus::Supported,
        SupportStatus::Partial,
        SupportStatus::Planned,
        SupportStatus::NotPlanned,
    ];
    let set: HashSet<&str> = statuses
        .iter()
        .map(|s| serde_json::to_value(s).unwrap())
        .map(|v| match v.as_str().unwrap() {
            "supported" => "supported",
            "partial" => "partial",
            "planned" => "planned",
            "not-planned" => "not-planned",
            other => panic!("unexpected status: {other}"),
        })
        .collect();
    assert_eq!(set.len(), 4);
}

#[test]
fn support_status_copy_and_eq() {
    let a = SupportStatus::Partial;
    let b = a; // Copy
    assert_eq!(a, b);
    assert_ne!(a, SupportStatus::Supported);
}

// ── 3. FeatureSupport – creation, querying, metadata ────────────────────────

#[test]
fn feature_support_fields_are_accessible() {
    let f = find_feature_by_id(FeatureId::Comp1Comp2).unwrap();
    // All public fields must be reachable without accessors.
    let _id = f.id;
    let _name = f.name;
    let _desc = f.description;
    let _status = f.status;
    let _doc = f.doc_ref;
    assert_eq!(f.id, FeatureId::Comp1Comp2);
}

#[test]
fn feature_support_name_does_not_contain_leading_trailing_whitespace() {
    for f in all_features() {
        assert_eq!(
            f.name,
            f.name.trim(),
            "{:?} name has extraneous whitespace",
            f.id
        );
        assert_eq!(
            f.description,
            f.description.trim(),
            "{:?} description has extraneous whitespace",
            f.id
        );
    }
}

// ── 4. Matrix queries – cross-referencing string ↔ ID lookups ───────────────

#[test]
fn string_lookup_and_id_lookup_return_same_entry() {
    for f in all_features() {
        let kebab = serde_plain::to_string(&f.id).unwrap();
        let by_str = find_feature(&kebab).unwrap();
        let by_id = find_feature_by_id(f.id).unwrap();
        assert_eq!(by_str.id, by_id.id);
        assert_eq!(by_str.name, by_id.name);
        assert_eq!(by_str.status, by_id.status);
    }
}

#[test]
fn find_feature_is_case_sensitive() {
    // Uppercase and mixed-case must NOT match.
    assert!(find_feature("LEVEL-88").is_none());
    assert!(find_feature("Level-88").is_none());
    assert!(find_feature("Edited-Pic").is_none());
}

// ── 5. Completeness – every FeatureId variant has a registry entry ──────────

#[test]
fn all_feature_ids_map_to_unique_kebab_strings() {
    let mut kebabs = HashSet::new();
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
        assert!(kebabs.insert(s.clone()), "duplicate kebab string: {s}");
    }
    assert_eq!(kebabs.len(), all_features().len());
}

#[test]
fn registry_count_matches_id_variant_count() {
    // If a new variant is added to FeatureId but not to all_features(),
    // the counts will diverge.
    let variant_count = 7; // Keep in sync with FeatureId variants
    assert_eq!(
        all_features().len(),
        variant_count,
        "all_features() count must match FeatureId variant count"
    );
}

// ── 6. Consistency – status invariants ──────────────────────────────────────

#[test]
fn no_feature_uses_not_planned_or_planned_currently() {
    // All current features are either Supported or Partial.
    // If a Planned or NotPlanned feature is added, this test should be
    // updated to track the expectation explicitly.
    for f in all_features() {
        assert!(
            f.status == SupportStatus::Supported || f.status == SupportStatus::Partial,
            "{:?} has unexpected status {:?}; update test if intentional",
            f.id,
            f.status,
        );
    }
}

#[test]
fn supported_features_have_substantive_descriptions() {
    // Supported features should have descriptions longer than a trivial stub.
    for f in all_features() {
        if f.status == SupportStatus::Supported {
            assert!(
                f.description.len() >= 20,
                "{:?} (Supported) has too-short description: {:?}",
                f.id,
                f.description,
            );
        }
    }
}

#[test]
fn doc_refs_follow_path_convention() {
    for f in all_features() {
        let doc = f.doc_ref.unwrap();
        assert!(
            doc.starts_with("docs/"),
            "{:?} doc_ref must start with 'docs/': {doc}",
            f.id
        );
        assert!(
            doc.contains('#') || doc.ends_with(".md"),
            "{:?} doc_ref should point to a markdown file or section: {doc}",
            f.id
        );
    }
}

// ── 7. Display / Debug – human-readable output ─────────────────────────────

#[test]
fn feature_id_debug_is_human_readable() {
    let dbg = format!("{:?}", FeatureId::Level88Conditions);
    assert!(
        dbg.contains("Level88Conditions"),
        "Debug should contain variant name, got: {dbg}"
    );
}

#[test]
fn support_status_debug_is_human_readable() {
    let dbg = format!("{:?}", SupportStatus::Supported);
    assert!(dbg.contains("Supported"), "got: {dbg}");

    let dbg = format!("{:?}", SupportStatus::NotPlanned);
    assert!(dbg.contains("NotPlanned"), "got: {dbg}");
}

#[test]
fn feature_support_debug_includes_id_and_status() {
    let f = find_feature_by_id(FeatureId::NestedOdo).unwrap();
    let dbg = format!("{f:?}");
    assert!(dbg.contains("NestedOdo"), "Debug missing id: {dbg}");
    assert!(dbg.contains("Partial"), "Debug missing status: {dbg}");
}

// ── 8. Serde edge cases ────────────────────────────────────────────────────

#[test]
fn feature_id_deserialize_rejects_unknown_string() {
    let result = serde_plain::from_str::<FeatureId>("bogus-feature");
    assert!(result.is_err());
}

#[test]
fn all_features_json_serializes_as_array() {
    let json = serde_json::to_value(all_features()).unwrap();
    let arr = json.as_array().unwrap();
    assert_eq!(arr.len(), all_features().len());
    for entry in arr {
        assert!(entry.get("id").is_some());
        assert!(entry.get("status").is_some());
    }
}

#[test]
fn feature_id_can_be_used_as_hashmap_key() {
    let mut map: HashMap<FeatureId, &str> = HashMap::new();
    for f in all_features() {
        map.insert(f.id, f.name);
    }
    assert_eq!(map.len(), all_features().len());
    assert_eq!(
        *map.get(&FeatureId::EditedPic).unwrap(),
        "Edited PIC clauses"
    );
}
