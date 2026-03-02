// SPDX-License-Identifier: AGPL-3.0-or-later
//! Feature lifecycle integration tests for the full governance stack.
//!
//! Verifies cross-layer consistency, lifecycle transitions, concurrent access,
//! and systematic flag combination coverage.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::HashSet;
use std::sync::Arc;
use std::thread;

use copybook_governance::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureId, FeatureLifecycle,
    GovernanceSummary, SupportStatus, feature_flags_for_support_id, governance_bindings,
    governance_state_for_support_id, governance_states, is_support_runtime_available,
    runtime_summary, summarize_governance, support_states,
};

// =========================================================================
// 1. Feature lifecycle stage transitions
// =========================================================================

#[test]
fn lifecycle_experimental_to_stable_transition_is_representable() {
    // A feature starts as Experimental and can transition to Stable.
    let mut lifecycle = FeatureLifecycle::Experimental;
    assert_eq!(lifecycle.to_string(), "experimental");
    lifecycle = FeatureLifecycle::Stable;
    assert_eq!(lifecycle.to_string(), "stable");
}

#[test]
fn lifecycle_stable_to_deprecated_transition_is_representable() {
    let mut lifecycle = FeatureLifecycle::Stable;
    assert_eq!(lifecycle.to_string(), "stable");
    lifecycle = FeatureLifecycle::Deprecated;
    assert_eq!(lifecycle.to_string(), "deprecated");
}

#[test]
fn lifecycle_all_stages_have_distinct_display_values() {
    let stages = [
        FeatureLifecycle::Experimental,
        FeatureLifecycle::Stable,
        FeatureLifecycle::Deprecated,
    ];
    let display_values: HashSet<String> = stages.iter().map(ToString::to_string).collect();
    assert_eq!(display_values.len(), 3);
}

#[test]
fn lifecycle_serde_roundtrip_preserves_all_stages() {
    for lc in [
        FeatureLifecycle::Experimental,
        FeatureLifecycle::Stable,
        FeatureLifecycle::Deprecated,
    ] {
        let json = serde_json::to_string(&lc).unwrap();
        let back: FeatureLifecycle = serde_json::from_str(&json).unwrap();
        assert_eq!(back, lc);
    }
}

// =========================================================================
// 2. Feature flag consistently enables/disables across the full stack
// =========================================================================

#[test]
fn sign_separate_flag_toggle_propagates_through_all_layers() {
    // Layer 1: FeatureFlags
    let mut flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::SignSeparate));

    // Layer 2: Governance grid reports the binding
    let grid_flags = feature_flags_for_support_id(FeatureId::SignSeparate).unwrap();
    assert!(grid_flags.contains(&Feature::SignSeparate));

    // Layer 3: Runtime says available
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));

    // Disable the flag
    flags.disable(Feature::SignSeparate);
    assert!(!flags.is_enabled(Feature::SignSeparate));

    // Layer 3: Runtime says unavailable
    assert!(!is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));

    // Layer 4: Governance state shows missing flag
    let state = governance_state_for_support_id(FeatureId::SignSeparate, &flags).unwrap();
    assert!(!state.runtime_enabled);
    assert!(state.missing_feature_flags.contains(&Feature::SignSeparate));
}

#[test]
fn comp1comp2_multi_flag_binding_requires_both_flags() {
    // Both enabled: available
    let flags_both = FeatureFlags::builder()
        .enable(Feature::Comp1)
        .enable(Feature::Comp2)
        .build();
    assert!(is_support_runtime_available(
        FeatureId::Comp1Comp2,
        &flags_both
    ));

    // Only Comp1: unavailable
    let flags_comp1_only = FeatureFlags::builder()
        .enable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();
    assert!(!is_support_runtime_available(
        FeatureId::Comp1Comp2,
        &flags_comp1_only
    ));

    // Only Comp2: unavailable
    let flags_comp2_only = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .enable(Feature::Comp2)
        .build();
    assert!(!is_support_runtime_available(
        FeatureId::Comp1Comp2,
        &flags_comp2_only
    ));

    // Neither: unavailable
    let flags_neither = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();
    assert!(!is_support_runtime_available(
        FeatureId::Comp1Comp2,
        &flags_neither
    ));
}

// =========================================================================
// 3. All known features have governance entries across the full stack
// =========================================================================

#[test]
fn every_support_matrix_id_has_governance_binding_and_runtime_state() {
    let flags = FeatureFlags::default();
    let all_support = copybook_governance::support_matrix::all_features();

    for feature in all_support {
        // Binding exists in grid
        let binding_flags = feature_flags_for_support_id(feature.id);
        assert!(
            binding_flags.is_some(),
            "No governance binding for {:?}",
            feature.id
        );

        // Runtime state can be resolved
        let state = governance_state_for_support_id(feature.id, &flags);
        assert!(state.is_some(), "No runtime state for {:?}", feature.id);

        // State metadata matches support matrix
        let state = state.unwrap();
        assert_eq!(state.support_id, feature.id);
        assert_eq!(state.support_name, feature.name);
        assert_eq!(state.support_status, feature.status);
    }
}

#[test]
fn every_feature_flag_variant_belongs_to_exactly_one_category() {
    let all = copybook_governance::feature_flags::all_features();
    for feature in &all {
        let cat = feature.category();
        // Verify round-trip: feature is in its own category's list
        let in_category = FeatureFlags::features_in_category(cat);
        assert!(
            in_category.contains(feature),
            "{feature} not found in {cat:?} category list",
        );
    }
}

#[test]
fn category_member_counts_sum_to_total_feature_count() {
    let categories = [
        FeatureCategory::Experimental,
        FeatureCategory::Enterprise,
        FeatureCategory::Performance,
        FeatureCategory::Debug,
        FeatureCategory::Testing,
    ];
    let total: usize = categories
        .iter()
        .map(|c| FeatureFlags::features_in_category(*c).len())
        .sum();
    let all = copybook_governance::feature_flags::all_features();
    assert_eq!(total, all.len());
}

// =========================================================================
// 4. Support matrix reflects actual crate capabilities
// =========================================================================

#[test]
fn support_matrix_status_distribution_matches_expected() {
    let all = copybook_governance::support_matrix::all_features();
    let supported_count = all
        .iter()
        .filter(|f| f.status == SupportStatus::Supported)
        .count();
    let partial_count = all
        .iter()
        .filter(|f| f.status == SupportStatus::Partial)
        .count();

    assert_eq!(supported_count, 4, "expected 4 Supported features");
    assert_eq!(partial_count, 3, "expected 3 Partial features");
    assert_eq!(supported_count + partial_count, all.len());
}

#[test]
fn governance_bindings_and_support_matrix_are_bijective() {
    let bindings = governance_bindings();
    let support = copybook_governance::support_matrix::all_features();

    let binding_ids: HashSet<FeatureId> = bindings.iter().map(|b| b.support_id).collect();
    let support_ids: HashSet<FeatureId> = support.iter().map(|f| f.id).collect();

    assert_eq!(
        binding_ids, support_ids,
        "bindings and support matrix must cover same IDs"
    );
}

// =========================================================================
// 5. Cross-layer summary consistency
// =========================================================================

#[test]
fn grid_and_runtime_summaries_agree_under_all_enabled_flags() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance::feature_flags::all_features() {
        flags.enable(f);
    }

    let grid: GovernanceSummary = summarize_governance();
    let rt = runtime_summary(&flags);

    assert_eq!(grid.total_support_features, rt.total_support_features);
    assert_eq!(grid.mapped_support_features, rt.mapped_support_features);
    assert_eq!(
        grid.total_linked_feature_flags,
        rt.total_linked_feature_flags
    );
    assert_eq!(rt.runtime_disabled_features, 0);
    assert_eq!(rt.runtime_enabled_features, rt.mapped_support_features);
}

#[test]
fn grid_and_runtime_summaries_agree_under_all_disabled_flags() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance::feature_flags::all_features() {
        flags.disable(f);
    }

    let grid = summarize_governance();
    let rt = runtime_summary(&flags);

    assert_eq!(grid.total_support_features, rt.total_support_features);
    assert_eq!(grid.mapped_support_features, rt.mapped_support_features);
    assert!(rt.runtime_disabled_features >= 3);
}

#[test]
fn support_states_and_governance_states_cover_same_ids() {
    let flags = FeatureFlags::default();
    let sup_ids: HashSet<FeatureId> = support_states().iter().map(|s| s.support_id).collect();
    let gov_ids: HashSet<FeatureId> = governance_states(&flags)
        .iter()
        .map(|s| s.support_id)
        .collect();
    assert_eq!(sup_ids, gov_ids);
}

// =========================================================================
// 6. Thread-safe concurrent governance evaluation
// =========================================================================

#[test]
fn handle_concurrent_enable_disable_is_safe() {
    let handle = Arc::new(FeatureFlagsHandle::new());

    let handles: Vec<_> = (0..4)
        .map(|i| {
            let h = Arc::clone(&handle);
            thread::spawn(move || {
                for _ in 0..100 {
                    if i % 2 == 0 {
                        h.enable(Feature::Profiling);
                    } else {
                        h.disable(Feature::Profiling);
                    }
                    let _ = h.is_enabled(Feature::Profiling);
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    // Just verify no panic or deadlock occurred; final state is non-deterministic
    let _ = handle.is_enabled(Feature::Profiling);
}

#[test]
fn handle_concurrent_snapshot_is_consistent() {
    let handle = Arc::new(FeatureFlagsHandle::new());
    handle.enable(Feature::AuditSystem);

    let handles: Vec<_> = (0..4)
        .map(|_| {
            let h = Arc::clone(&handle);
            thread::spawn(move || {
                let snap = h.snapshot();
                // Snapshot should always be a valid FeatureFlags
                let _ = snap.is_enabled(Feature::AuditSystem);
                let _ = snap.enabled_features().count();
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }
}

// =========================================================================
// 7. Governance state serialization completeness
// =========================================================================

#[test]
fn governance_state_serializes_all_fields_for_governed_feature() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    let json = serde_json::to_value(&state).unwrap();

    assert!(json.get("support_id").is_some());
    assert!(json.get("support_name").is_some());
    assert!(json.get("support_description").is_some());
    assert!(json.get("support_status").is_some());
    assert!(json.get("rationale").is_some());
    assert!(json.get("required_feature_flags").is_some());
    assert!(json.get("missing_feature_flags").is_some());
    assert!(json.get("runtime_enabled").is_some());
}

#[test]
fn governance_state_serializes_correctly_for_disabled_feature() {
    let flags = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    let json = serde_json::to_value(&state).unwrap();

    assert_eq!(json["runtime_enabled"], false);
    let missing = json["missing_feature_flags"].as_array().unwrap();
    assert_eq!(missing.len(), 2);
}

#[test]
fn runtime_summary_serializes_all_fields() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    let json = serde_json::to_value(summary).unwrap();

    assert!(json["total_support_features"].is_number());
    assert!(json["mapped_support_features"].is_number());
    assert!(json["total_linked_feature_flags"].is_number());
    assert!(json["runtime_enabled_features"].is_number());
    assert!(json["runtime_disabled_features"].is_number());
}

// =========================================================================
// 8. Systematic flag combination coverage
// =========================================================================

#[test]
fn each_governed_binding_disables_when_its_flags_are_off() {
    let governed_bindings: Vec<_> = governance_bindings()
        .iter()
        .filter(|b| !b.feature_flags.is_empty())
        .collect();

    for binding in &governed_bindings {
        // Build flags with this binding's flags disabled
        let mut flags = FeatureFlags::default();
        for flag in binding.feature_flags {
            flags.disable(*flag);
        }

        let state = governance_state_for_support_id(binding.support_id, &flags).unwrap();
        assert!(
            !state.runtime_enabled,
            "{:?} should be disabled when its flags are off",
            binding.support_id
        );
        assert_eq!(
            state.missing_feature_flags.len(),
            binding.feature_flags.len(),
            "{:?} missing flag count mismatch",
            binding.support_id
        );
    }
}

#[test]
fn each_governed_binding_enables_when_its_flags_are_on() {
    let governed_bindings: Vec<_> = governance_bindings()
        .iter()
        .filter(|b| !b.feature_flags.is_empty())
        .collect();

    for binding in &governed_bindings {
        let mut flags = FeatureFlags::default();
        for flag in binding.feature_flags {
            flags.enable(*flag);
        }

        let state = governance_state_for_support_id(binding.support_id, &flags).unwrap();
        assert!(
            state.runtime_enabled,
            "{:?} should be enabled when its flags are on",
            binding.support_id
        );
        assert!(
            state.missing_feature_flags.is_empty(),
            "{:?} should have no missing flags",
            binding.support_id
        );
    }
}

#[test]
fn ungoverned_features_always_runtime_enabled_regardless_of_flags() {
    let ungoverned: Vec<_> = governance_bindings()
        .iter()
        .filter(|b| b.feature_flags.is_empty())
        .collect();

    // With all flags off
    let mut flags_off = FeatureFlags::default();
    for f in copybook_governance::feature_flags::all_features() {
        flags_off.disable(f);
    }

    for binding in &ungoverned {
        let state = governance_state_for_support_id(binding.support_id, &flags_off).unwrap();
        assert!(
            state.runtime_enabled,
            "{:?} should always be enabled (ungoverned)",
            binding.support_id
        );
    }
}

#[test]
fn missing_feature_flags_is_always_subset_of_required() {
    let configs: Vec<FeatureFlags> = vec![
        FeatureFlags::default(),
        FeatureFlags::builder()
            .disable(Feature::SignSeparate)
            .build(),
        FeatureFlags::builder().disable(Feature::Comp1).build(),
        FeatureFlags::builder()
            .disable(Feature::Comp1)
            .disable(Feature::Comp2)
            .build(),
        {
            let mut f = FeatureFlags::default();
            for feat in copybook_governance::feature_flags::all_features() {
                f.disable(feat);
            }
            f
        },
    ];

    for flags in &configs {
        for state in governance_states(flags) {
            let required_set: HashSet<Feature> =
                state.required_feature_flags.iter().copied().collect();
            for missing in &state.missing_feature_flags {
                assert!(
                    required_set.contains(missing),
                    "{:?}: missing flag {missing:?} not in required set",
                    state.support_id
                );
            }
        }
    }
}

#[test]
fn runtime_enabled_is_true_iff_missing_flags_is_empty() {
    let configs: Vec<FeatureFlags> = vec![
        FeatureFlags::default(),
        FeatureFlags::builder().enable(Feature::RenamesR4R6).build(),
        FeatureFlags::builder()
            .disable(Feature::SignSeparate)
            .disable(Feature::Comp1)
            .build(),
    ];

    for flags in &configs {
        for state in governance_states(flags) {
            assert_eq!(
                state.runtime_enabled,
                state.missing_feature_flags.is_empty(),
                "{:?}: runtime_enabled should equal missing_flags.is_empty()",
                state.support_id
            );
        }
    }
}
