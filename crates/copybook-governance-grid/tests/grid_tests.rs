//! Integration tests for copybook-governance-grid.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_governance_grid::{
    Feature, FeatureFlags, FeatureId, audit_governance, feature_flags_for_support_id,
    governance_bindings, summarize_governance,
};

// ── Binding registry ────────────────────────────────────────────────────────

#[test]
fn governance_bindings_returns_seven_entries() {
    assert_eq!(governance_bindings().len(), 7);
}

#[test]
fn every_binding_has_nonempty_rationale() {
    for binding in governance_bindings() {
        assert!(
            !binding.rationale.is_empty(),
            "binding for {:?} has empty rationale",
            binding.support_id
        );
    }
}

#[test]
fn no_duplicate_support_ids_in_bindings() {
    let bindings = governance_bindings();
    for (i, b) in bindings.iter().enumerate() {
        for other in &bindings[i + 1..] {
            assert_ne!(
                b.support_id, other.support_id,
                "duplicate support_id: {:?}",
                b.support_id
            );
        }
    }
}

#[test]
fn bindings_slice_is_stable_across_calls() {
    let a = governance_bindings();
    let b = governance_bindings();
    assert_eq!(a.len(), b.len());
    for (x, y) in a.iter().zip(b.iter()) {
        assert_eq!(x.support_id, y.support_id);
        assert_eq!(x.feature_flags.len(), y.feature_flags.len());
    }
}

// ── Flag linkage per support ID ─────────────────────────────────────────────

#[test]
fn feature_flags_for_sign_separate() {
    // #656 Phase C: SIGN SEPARATE parses unconditionally; no runtime toggle.
    let flags = feature_flags_for_support_id(FeatureId::SignSeparate).unwrap();
    assert!(flags.is_empty());
}

#[test]
fn feature_flags_for_comp12() {
    // #656 Phase C: COMP-1/COMP-2 parse unconditionally; no runtime toggle.
    let flags = feature_flags_for_support_id(FeatureId::Comp1Comp2).unwrap();
    assert!(flags.is_empty());
}

#[test]
fn feature_flags_for_level66_renames() {
    let flags = feature_flags_for_support_id(FeatureId::Level66Renames).unwrap();
    assert_eq!(flags.len(), 1);
    assert!(flags.contains(&Feature::RenamesR4R6));
}

#[test]
fn level88_has_no_feature_flags() {
    let flags = feature_flags_for_support_id(FeatureId::Level88Conditions).unwrap();
    assert!(flags.is_empty());
}

#[test]
fn occurs_depending_has_no_feature_flags() {
    let flags = feature_flags_for_support_id(FeatureId::OccursDepending).unwrap();
    assert!(flags.is_empty());
}

#[test]
fn edited_pic_has_no_feature_flags() {
    let flags = feature_flags_for_support_id(FeatureId::EditedPic).unwrap();
    assert!(flags.is_empty());
}

#[test]
fn nested_odo_has_no_feature_flags() {
    let flags = feature_flags_for_support_id(FeatureId::NestedOdo).unwrap();
    assert!(flags.is_empty());
}

// ── Completeness: bindings vs support matrix ────────────────────────────────

#[test]
fn all_support_ids_have_bindings() {
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
            feature_flags_for_support_id(id).is_some(),
            "missing binding for {id:?}"
        );
    }
}

#[test]
fn governance_bindings_cover_all_support_matrix_entries() {
    let support_ids: Vec<_> = copybook_governance_grid::support_matrix::all_features()
        .iter()
        .map(|f| f.id)
        .collect();
    let binding_ids: Vec<_> = governance_bindings().iter().map(|b| b.support_id).collect();
    assert_eq!(support_ids.len(), binding_ids.len());
    for id in &support_ids {
        assert!(binding_ids.contains(id), "missing binding for {id:?}");
    }
}

#[test]
fn grid_binding_flags_are_valid_feature_variants() {
    let all = copybook_governance_grid::feature_flags::all_features();
    for binding in governance_bindings() {
        for flag in binding.feature_flags {
            assert!(
                all.contains(flag),
                "binding {:?} references unknown Feature variant {:?}",
                binding.support_id,
                flag
            );
        }
    }
}

#[test]
fn governed_features_have_at_least_one_flag() {
    // #656 Phase C: only Level66Renames carries flag linkage; the stable
    // language entries are bound with empty flag slices.
    let flags = feature_flags_for_support_id(FeatureId::Level66Renames).unwrap();
    assert!(
        !flags.is_empty(),
        "Level66Renames should have at least one feature flag"
    );
    for id in [FeatureId::SignSeparate, FeatureId::Comp1Comp2] {
        let flags = feature_flags_for_support_id(id).unwrap();
        assert!(flags.is_empty(), "{id:?} should have no flag linkage");
    }
}

#[test]
fn ungoverned_features_have_zero_flags() {
    let ungoverned = [
        FeatureId::Level88Conditions,
        FeatureId::OccursDepending,
        FeatureId::EditedPic,
        FeatureId::NestedOdo,
    ];
    for id in ungoverned {
        let flags = feature_flags_for_support_id(id).unwrap();
        assert!(flags.is_empty(), "{id:?} should have zero feature flags");
    }
}

#[test]
fn total_linked_flags_equals_sum_of_binding_flag_counts() {
    let expected: usize = governance_bindings()
        .iter()
        .map(|b| b.feature_flags.len())
        .sum();
    let summary = summarize_governance();
    assert_eq!(summary.total_linked_feature_flags, expected);
}

// ── GovernanceSummary ───────────────────────────────────────────────────────

#[test]
fn summarize_governance_totals() {
    let summary = summarize_governance();
    assert_eq!(summary.total_support_features, 7);
    assert_eq!(summary.mapped_support_features, 7);
    assert!(summary.all_features_known());
}

#[test]
fn summarize_governance_linked_flag_count() {
    let summary = summarize_governance();
    // #656 Phase C: only RenamesR4R6 remains flag-linked.
    assert_eq!(summary.total_linked_feature_flags, 1);
    assert_eq!(summary.explicit_bindings(), 1);
}

#[test]
fn all_features_known_reflects_completeness() {
    let summary = summarize_governance();
    assert!(summary.all_features_known());
    assert_eq!(
        summary.total_support_features,
        summary.mapped_support_features
    );
}

#[test]
fn summarize_governance_is_deterministic() {
    let a = summarize_governance();
    let b = summarize_governance();
    assert_eq!(a.total_support_features, b.total_support_features);
    assert_eq!(a.mapped_support_features, b.mapped_support_features);
    assert_eq!(a.total_linked_feature_flags, b.total_linked_feature_flags);
}

#[test]
fn audit_governance_reports_clean_state() {
    let audit = audit_governance();
    assert!(audit.is_clean());
    assert!(audit.missing_support_ids.is_empty());
    assert!(audit.orphaned_binding_ids.is_empty());
    assert!(audit.duplicate_binding_ids.is_empty());
}

// ── Re-exported types ───────────────────────────────────────────────────────

#[test]
fn re_exported_feature_type_is_usable() {
    let f = Feature::RenamesR4R6;
    assert_eq!(f.to_string(), "renames_r4_r6");
}

#[test]
fn re_exported_feature_id_is_usable() {
    let id = FeatureId::EditedPic;
    let s = serde_plain::to_string(&id).unwrap();
    assert_eq!(s, "edited-pic");
}

#[test]
fn re_exported_feature_flags_builder_works() {
    let flags = FeatureFlags::builder()
        .enable(Feature::RenamesR4R6)
        .disable(Feature::LruCache)
        .build();
    assert!(flags.is_enabled(Feature::RenamesR4R6));
    assert!(!flags.is_enabled(Feature::LruCache));
}

#[test]
fn re_exported_support_matrix_all_features_accessible() {
    let features = copybook_governance_grid::support_matrix::all_features();
    assert_eq!(features.len(), 7);
}

#[test]
fn re_exported_feature_flags_module_accessible() {
    let all = copybook_governance_grid::feature_flags::all_features();
    // #656 Phase C: 18 -> 15 flags.
    assert_eq!(all.len(), 15);
}
