// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for copybook-governance-grid static mappings.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_governance_grid::{
    Feature, FeatureId, feature_flags_for_support_id, governance_bindings, summarize_governance,
};

// ── Binding registry ────────────────────────────────────────────────────────

#[test]
fn governance_bindings_returns_seven_entries() {
    assert_eq!(governance_bindings().len(), 7);
}

#[test]
fn governance_bindings_are_unique_by_support_id() {
    let bindings = governance_bindings();
    for (i, a) in bindings.iter().enumerate() {
        for b in &bindings[i + 1..] {
            assert_ne!(
                a.support_id, b.support_id,
                "duplicate support_id {:?}",
                a.support_id
            );
        }
    }
}

#[test]
fn all_bindings_have_nonempty_rationale() {
    for binding in governance_bindings() {
        assert!(
            !binding.rationale.is_empty(),
            "{:?} has empty rationale",
            binding.support_id
        );
    }
}

// ── Flag linkage per support ID ─────────────────────────────────────────────

#[test]
fn level88_has_no_required_flags() {
    let flags = feature_flags_for_support_id(FeatureId::Level88Conditions).unwrap();
    assert!(flags.is_empty());
}

#[test]
fn level66_renames_requires_renames_r4r6() {
    let flags = feature_flags_for_support_id(FeatureId::Level66Renames).unwrap();
    assert_eq!(flags.len(), 1);
    assert!(flags.contains(&Feature::RenamesR4R6));
}

#[test]
fn occurs_depending_has_no_required_flags() {
    let flags = feature_flags_for_support_id(FeatureId::OccursDepending).unwrap();
    assert!(flags.is_empty());
}

#[test]
fn edited_pic_has_no_required_flags() {
    let flags = feature_flags_for_support_id(FeatureId::EditedPic).unwrap();
    assert!(flags.is_empty());
}

#[test]
fn comp1comp2_requires_both_comp_flags() {
    let flags = feature_flags_for_support_id(FeatureId::Comp1Comp2).unwrap();
    assert_eq!(flags.len(), 2);
    assert!(flags.contains(&Feature::Comp1));
    assert!(flags.contains(&Feature::Comp2));
}

#[test]
fn sign_separate_requires_sign_separate_flag() {
    let flags = feature_flags_for_support_id(FeatureId::SignSeparate).unwrap();
    assert_eq!(flags.len(), 1);
    assert!(flags.contains(&Feature::SignSeparate));
}

#[test]
fn nested_odo_has_no_required_flags() {
    let flags = feature_flags_for_support_id(FeatureId::NestedOdo).unwrap();
    assert!(flags.is_empty());
}

// ── GovernanceSummary ───────────────────────────────────────────────────────

#[test]
fn summary_total_support_features_equals_seven() {
    let summary = summarize_governance();
    assert_eq!(summary.total_support_features, 7);
}

#[test]
fn summary_mapped_equals_total() {
    let summary = summarize_governance();
    assert_eq!(
        summary.mapped_support_features,
        summary.total_support_features
    );
    assert!(summary.all_features_known());
}

#[test]
fn summary_explicit_bindings_counts_flags() {
    let summary = summarize_governance();
    // 1 (SignSeparate) + 1 (RenamesR4R6) + 2 (Comp1+Comp2) = 4
    assert_eq!(summary.explicit_bindings(), 4);
    assert_eq!(summary.total_linked_feature_flags, 4);
}

// ── Re-exported types ───────────────────────────────────────────────────────

#[test]
fn re_exported_feature_type_is_usable() {
    let f = Feature::SignSeparate;
    assert_eq!(f.to_string(), "sign_separate");
}

#[test]
fn re_exported_feature_id_is_usable() {
    let id = FeatureId::EditedPic;
    let s = serde_plain::to_string(&id).unwrap();
    assert_eq!(s, "edited-pic");
}
