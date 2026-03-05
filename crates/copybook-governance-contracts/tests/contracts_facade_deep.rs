// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep tests for the governance-contracts façade.
//!
//! Covers Feature trait impls, `FeatureFlags` edge cases, category exhaustiveness,
//! Handle concurrency patterns, and serde completeness.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::HashSet;
use std::str::FromStr;
use std::sync::Arc;
use std::thread;

use copybook_governance_contracts::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureId, SupportStatus,
    find_feature, find_feature_by_id,
};

// =========================================================================
// 1. Feature::from_str roundtrip for all variants
// =========================================================================

#[test]
fn feature_display_from_str_roundtrip_all_variants() {
    for feature in copybook_governance_contracts::feature_flags::all_features() {
        let display = feature.to_string();
        let back = Feature::from_str(&display)
            .unwrap_or_else(|e| panic!("from_str failed for '{display}': {e}"));
        assert_eq!(back, feature, "roundtrip failed for {feature}");
    }
}

#[test]
fn feature_from_str_is_case_insensitive() {
    assert_eq!(
        Feature::from_str("SIGN_SEPARATE").unwrap(),
        Feature::SignSeparate
    );
    assert_eq!(Feature::from_str("Lru_Cache").unwrap(), Feature::LruCache);
    assert_eq!(Feature::from_str("profiling").unwrap(), Feature::Profiling);
}

#[test]
fn feature_from_str_rejects_invalid_input() {
    assert!(Feature::from_str("").is_err());
    assert!(Feature::from_str("not_a_feature").is_err());
    assert!(Feature::from_str("sign-separate").is_err()); // kebab-case not supported
}

// =========================================================================
// 2. Feature env_var_name format validation
// =========================================================================

#[test]
fn feature_env_var_name_has_correct_prefix_for_all_variants() {
    for feature in copybook_governance_contracts::feature_flags::all_features() {
        let env_var = feature.env_var_name();
        assert!(
            env_var.starts_with("COPYBOOK_FF_"),
            "{feature} env var '{env_var}' missing prefix"
        );
        // Must be uppercase
        assert_eq!(
            env_var,
            env_var.to_uppercase(),
            "{feature} env var not uppercase"
        );
    }
}

#[test]
fn feature_env_var_names_are_all_unique() {
    let names: HashSet<String> = copybook_governance_contracts::feature_flags::all_features()
        .into_iter()
        .map(Feature::env_var_name)
        .collect();
    assert_eq!(names.len(), 22);
}

// =========================================================================
// 3. Feature description completeness
// =========================================================================

#[test]
fn feature_description_is_substantive_for_all_variants() {
    for feature in copybook_governance_contracts::feature_flags::all_features() {
        let desc = feature.description();
        assert!(
            desc.len() >= 10,
            "{feature} has too-short description: '{desc}'"
        );
    }
}

#[test]
fn feature_descriptions_are_all_unique() {
    let descs: HashSet<&str> = copybook_governance_contracts::feature_flags::all_features()
        .into_iter()
        .map(Feature::description)
        .collect();
    assert_eq!(descs.len(), 22);
}

// =========================================================================
// 4. FeatureFlags mutation edge cases
// =========================================================================

#[test]
fn feature_flags_all_disabled_then_all_enabled_roundtrip() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance_contracts::feature_flags::all_features() {
        flags.disable(f);
    }
    for f in copybook_governance_contracts::feature_flags::all_features() {
        assert!(!flags.is_enabled(f), "{f} should be disabled");
    }
    for f in copybook_governance_contracts::feature_flags::all_features() {
        flags.enable(f);
    }
    for f in copybook_governance_contracts::feature_flags::all_features() {
        assert!(flags.is_enabled(f), "{f} should be enabled");
    }
}

#[test]
fn feature_flags_toggle_all_features_twice_returns_to_original() {
    let original = FeatureFlags::default();
    let mut flags = original.clone();
    for f in copybook_governance_contracts::feature_flags::all_features() {
        flags.toggle(f);
    }
    for f in copybook_governance_contracts::feature_flags::all_features() {
        flags.toggle(f);
    }
    for f in copybook_governance_contracts::feature_flags::all_features() {
        assert_eq!(
            flags.is_enabled(f),
            original.is_enabled(f),
            "{f}: double toggle should restore original state"
        );
    }
}

#[test]
fn feature_flags_specific_builder_configuration() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Profiling)
        .enable(Feature::AuditSystem)
        .disable(Feature::LruCache)
        .disable(Feature::Comp1)
        .build();
    assert!(flags.is_enabled(Feature::Profiling));
    assert!(flags.is_enabled(Feature::AuditSystem));
    assert!(!flags.is_enabled(Feature::LruCache));
    assert!(!flags.is_enabled(Feature::Comp1));
    // Default-enabled that weren't touched
    assert!(flags.is_enabled(Feature::SignSeparate));
    assert!(flags.is_enabled(Feature::Comp2));
}

// =========================================================================
// 5. Category exhaustiveness
// =========================================================================

#[test]
fn every_feature_has_a_known_category() {
    let known_categories: HashSet<FeatureCategory> = [
        FeatureCategory::Experimental,
        FeatureCategory::Enterprise,
        FeatureCategory::Performance,
        FeatureCategory::Debug,
        FeatureCategory::Testing,
    ]
    .into_iter()
    .collect();

    for feature in copybook_governance_contracts::feature_flags::all_features() {
        assert!(
            known_categories.contains(&feature.category()),
            "{feature} has unknown category {:?}",
            feature.category()
        );
    }
}

#[test]
fn no_feature_appears_in_multiple_categories() {
    let categories = [
        FeatureCategory::Experimental,
        FeatureCategory::Enterprise,
        FeatureCategory::Performance,
        FeatureCategory::Debug,
        FeatureCategory::Testing,
    ];
    let mut seen = HashSet::new();
    for cat in &categories {
        for f in FeatureFlags::features_in_category(*cat) {
            assert!(seen.insert(f), "{f} appears in multiple categories");
        }
    }
}

// =========================================================================
// 6. Feature query API covers all features
// =========================================================================

#[test]
fn find_feature_by_id_covers_all_seven_support_ids() {
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
        let f = find_feature_by_id(id);
        assert!(f.is_some(), "find_feature_by_id failed for {id:?}");
        assert_eq!(f.unwrap().id, id);
    }
}

#[test]
fn find_feature_and_find_feature_by_id_agree_on_all_metadata() {
    let kebab_to_id = [
        ("level-88", FeatureId::Level88Conditions),
        ("level-66-renames", FeatureId::Level66Renames),
        ("occurs-depending", FeatureId::OccursDepending),
        ("edited-pic", FeatureId::EditedPic),
        ("comp-1-comp-2", FeatureId::Comp1Comp2),
        ("sign-separate", FeatureId::SignSeparate),
        ("nested-odo", FeatureId::NestedOdo),
    ];
    for (kebab, id) in kebab_to_id {
        let by_str = find_feature(kebab).unwrap();
        let by_id = find_feature_by_id(id).unwrap();
        assert_eq!(by_str.id, by_id.id);
        assert_eq!(by_str.name, by_id.name);
        assert_eq!(by_str.description, by_id.description);
        assert_eq!(by_str.status, by_id.status);
        assert_eq!(by_str.doc_ref, by_id.doc_ref);
    }
}

// =========================================================================
// 7. SupportStatus enum completeness
// =========================================================================

#[test]
fn support_status_all_four_variants_are_distinct() {
    let statuses = [
        SupportStatus::Supported,
        SupportStatus::Partial,
        SupportStatus::Planned,
        SupportStatus::NotPlanned,
    ];
    // Verify all four are distinct via equality
    for (i, a) in statuses.iter().enumerate() {
        for b in &statuses[i + 1..] {
            assert_ne!(a, b);
        }
    }
}

// =========================================================================
// 8. Thread-safe access patterns
// =========================================================================

#[test]
fn handle_concurrent_toggle_from_multiple_threads() {
    let handle = Arc::new(FeatureFlagsHandle::new());

    let threads: Vec<_> = (0..8)
        .map(|_| {
            let h = Arc::clone(&handle);
            thread::spawn(move || {
                for _ in 0..50 {
                    h.toggle(Feature::MemoryTracking);
                    let _ = h.snapshot();
                }
            })
        })
        .collect();

    for t in threads {
        t.join().unwrap();
    }
    // No panic or deadlock means success
    let _ = handle.is_enabled(Feature::MemoryTracking);
}

#[test]
fn handle_snapshot_during_concurrent_mutations() {
    let handle = Arc::new(FeatureFlagsHandle::new());

    let writer = {
        let h = Arc::clone(&handle);
        thread::spawn(move || {
            for _ in 0..200 {
                h.enable(Feature::VerboseLogging);
                h.disable(Feature::VerboseLogging);
            }
        })
    };

    let reader = {
        let h = Arc::clone(&handle);
        thread::spawn(move || {
            let mut snapshots = Vec::new();
            for _ in 0..200 {
                snapshots.push(h.snapshot());
            }
            // Each snapshot should be internally consistent
            for snap in &snapshots {
                let _ = snap.is_enabled(Feature::VerboseLogging);
                let _ = snap.enabled_features().count();
            }
        })
    };

    writer.join().unwrap();
    reader.join().unwrap();
}

// =========================================================================
// 9. Builder chaining edge cases
// =========================================================================

#[test]
fn builder_enable_disable_same_category_results_in_empty() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Enterprise)
        .disable_category(FeatureCategory::Enterprise)
        .build();
    let enterprise = flags.enabled_in_category(FeatureCategory::Enterprise);
    assert!(enterprise.is_empty());
}

#[test]
fn builder_disable_then_enable_category_results_in_all_enabled() {
    let flags = FeatureFlags::builder()
        .disable_category(FeatureCategory::Performance)
        .enable_category(FeatureCategory::Performance)
        .build();
    let perf = flags.enabled_in_category(FeatureCategory::Performance);
    assert_eq!(perf.len(), 4);
}

#[test]
fn builder_individual_override_after_category() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Debug)
        .disable(Feature::Profiling)
        .build();
    assert!(flags.is_enabled(Feature::VerboseLogging));
    assert!(flags.is_enabled(Feature::DiagnosticOutput));
    assert!(!flags.is_enabled(Feature::Profiling));
    assert!(flags.is_enabled(Feature::MemoryTracking));
}

// =========================================================================
// 10. Default-enabled feature consistency
// =========================================================================

#[test]
fn default_enabled_features_match_feature_default_enabled_method() {
    let flags = FeatureFlags::default();
    for feature in copybook_governance_contracts::feature_flags::all_features() {
        assert_eq!(
            flags.is_enabled(feature),
            feature.default_enabled(),
            "{feature}: FeatureFlags::default() disagrees with Feature::default_enabled()"
        );
    }
}

#[test]
fn exactly_four_features_are_default_enabled() {
    let count = copybook_governance_contracts::feature_flags::all_features()
        .into_iter()
        .filter(|f| f.default_enabled())
        .count();
    assert_eq!(count, 4);
}
