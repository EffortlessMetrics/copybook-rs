//! Integration tests for copybook-governance-grid.

use copybook_governance_grid::{
    Feature, FeatureId, feature_flags_for_support_id, governance_bindings, summarize_governance,
};

// ---------------------------------------------------------------------------
// Grid cell creation and lookup
// ---------------------------------------------------------------------------

#[test]
fn governance_bindings_returns_seven_entries() {
    let bindings = governance_bindings();
    assert_eq!(bindings.len(), 7);
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
fn feature_flags_for_sign_separate() {
    let flags = feature_flags_for_support_id(FeatureId::SignSeparate).unwrap();
    assert_eq!(flags.len(), 1);
    assert!(flags.contains(&Feature::SignSeparate));
}

#[test]
fn feature_flags_for_comp12() {
    let flags = feature_flags_for_support_id(FeatureId::Comp1Comp2).unwrap();
    assert_eq!(flags.len(), 2);
    assert!(flags.contains(&Feature::Comp1));
    assert!(flags.contains(&Feature::Comp2));
}

#[test]
fn feature_flags_for_level66_renames() {
    let flags = feature_flags_for_support_id(FeatureId::Level66Renames).unwrap();
    assert_eq!(flags.len(), 1);
    assert!(flags.contains(&Feature::RenamesR4R6));
}

// ---------------------------------------------------------------------------
// Features with no runtime flags (parser-level governance)
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Feature-to-grid mapping completeness
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Grid completeness / summary checks
// ---------------------------------------------------------------------------

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
    // SignSeparate(1) + RenamesR4R6(1) + Comp1+Comp2(2) = 4
    assert_eq!(summary.total_linked_feature_flags, 4);
    assert_eq!(summary.explicit_bindings(), 4);
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
