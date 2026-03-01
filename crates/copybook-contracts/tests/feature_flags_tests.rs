// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for copybook-contracts feature flag system.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_contracts::feature_flags::all_features;
use copybook_contracts::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureLifecycle,
};
use std::str::FromStr;

// ── Feature enum ────────────────────────────────────────────────────────────

#[test]
fn feature_display_roundtrips_through_from_str() {
    for feature in all_features() {
        let display = feature.to_string();
        let parsed = Feature::from_str(&display).unwrap();
        assert_eq!(parsed, feature, "roundtrip failed for {feature}");
    }
}

#[test]
fn feature_from_str_is_case_insensitive() {
    assert_eq!(
        Feature::from_str("SIGN_SEPARATE").unwrap(),
        Feature::SignSeparate
    );
    assert_eq!(
        Feature::from_str("Sign_Separate").unwrap(),
        Feature::SignSeparate
    );
    assert_eq!(Feature::from_str("lru_cache").unwrap(), Feature::LruCache);
}

#[test]
fn feature_from_str_rejects_invalid_names() {
    let err = Feature::from_str("not_a_feature").unwrap_err();
    assert!(err.contains("Unknown feature flag"), "error was: {err}");
}

#[test]
fn feature_env_var_name_follows_convention() {
    for feature in all_features() {
        let env_name = feature.env_var_name();
        assert!(
            env_name.starts_with("COPYBOOK_FF_"),
            "{env_name} missing prefix"
        );
        assert_eq!(
            env_name,
            env_name.to_uppercase(),
            "{env_name} not uppercase"
        );
    }
}

#[test]
fn every_feature_belongs_to_exactly_one_category() {
    let categories = [
        FeatureCategory::Experimental,
        FeatureCategory::Enterprise,
        FeatureCategory::Performance,
        FeatureCategory::Debug,
        FeatureCategory::Testing,
    ];

    for feature in all_features() {
        let cat = feature.category();
        assert!(
            categories.contains(&cat),
            "{feature} has unknown category {cat}"
        );
    }
}

#[test]
fn all_features_list_is_exhaustive() {
    let features = all_features();
    // 4 Experimental + 6 Enterprise + 4 Performance + 4 Debug + 4 Testing = 22
    assert_eq!(features.len(), 22);
}

// ── FeatureCategory ─────────────────────────────────────────────────────────

#[test]
fn feature_category_display_all_variants() {
    let cases = [
        (FeatureCategory::Experimental, "experimental"),
        (FeatureCategory::Enterprise, "enterprise"),
        (FeatureCategory::Performance, "performance"),
        (FeatureCategory::Debug, "debug"),
        (FeatureCategory::Testing, "testing"),
    ];
    for (cat, expected) in cases {
        assert_eq!(cat.to_string(), expected);
    }
}

#[test]
fn feature_category_serde_roundtrip_all_variants() {
    let categories = [
        FeatureCategory::Experimental,
        FeatureCategory::Enterprise,
        FeatureCategory::Performance,
        FeatureCategory::Debug,
        FeatureCategory::Testing,
    ];
    for cat in categories {
        let json = serde_json::to_string(&cat).unwrap();
        let back: FeatureCategory = serde_json::from_str(&json).unwrap();
        assert_eq!(back, cat);
    }
}

// ── FeatureLifecycle ────────────────────────────────────────────────────────

#[test]
fn feature_lifecycle_display_all_variants() {
    assert_eq!(FeatureLifecycle::Experimental.to_string(), "experimental");
    assert_eq!(FeatureLifecycle::Stable.to_string(), "stable");
    assert_eq!(FeatureLifecycle::Deprecated.to_string(), "deprecated");
}

#[test]
fn feature_lifecycle_serde_roundtrip() {
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

// ── FeatureFlags ────────────────────────────────────────────────────────────

#[test]
fn default_flags_enable_only_default_enabled_features() {
    let flags = FeatureFlags::default();
    for feature in all_features() {
        assert_eq!(
            flags.is_enabled(feature),
            feature.default_enabled(),
            "mismatch for {feature}: default_enabled={}, is_enabled={}",
            feature.default_enabled(),
            flags.is_enabled(feature),
        );
    }
}

#[test]
fn enable_then_disable_returns_to_disabled() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::AuditSystem);
    assert!(flags.is_enabled(Feature::AuditSystem));
    flags.disable(Feature::AuditSystem);
    assert!(!flags.is_enabled(Feature::AuditSystem));
}

#[test]
fn toggle_flips_state_twice_returns_original() {
    let mut flags = FeatureFlags::default();
    let original = flags.is_enabled(Feature::Profiling);
    flags.toggle(Feature::Profiling);
    assert_ne!(flags.is_enabled(Feature::Profiling), original);
    flags.toggle(Feature::Profiling);
    assert_eq!(flags.is_enabled(Feature::Profiling), original);
}

#[test]
fn enabled_features_count_matches_default_enabled() {
    let flags = FeatureFlags::default();
    let default_count = all_features()
        .iter()
        .filter(|f| f.default_enabled())
        .count();
    assert_eq!(flags.enabled_features().count(), default_count);
}

#[test]
fn features_in_category_static_counts() {
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Experimental).len(),
        4
    );
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Enterprise).len(),
        6
    );
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Performance).len(),
        4
    );
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Debug).len(),
        4
    );
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Testing).len(),
        4
    );
}

#[test]
fn enabled_in_category_respects_mutations() {
    let mut flags = FeatureFlags::default();
    // Debug features are all off by default
    assert!(flags.enabled_in_category(FeatureCategory::Debug).is_empty());
    flags.enable(Feature::VerboseLogging);
    let debug = flags.enabled_in_category(FeatureCategory::Debug);
    assert_eq!(debug.len(), 1);
    assert!(debug.contains(&Feature::VerboseLogging));
}

#[test]
fn feature_flags_serde_preserves_enabled_set() {
    let flags = FeatureFlags::builder()
        .enable(Feature::AuditSystem)
        .enable(Feature::Profiling)
        .disable(Feature::LruCache)
        .build();
    let json = serde_json::to_string(&flags).unwrap();
    let restored: FeatureFlags = serde_json::from_str(&json).unwrap();
    assert!(restored.is_enabled(Feature::AuditSystem));
    assert!(restored.is_enabled(Feature::Profiling));
    assert!(!restored.is_enabled(Feature::LruCache));
}

// ── FeatureFlagsBuilder ─────────────────────────────────────────────────────

#[test]
fn builder_starts_from_defaults() {
    let built = FeatureFlags::builder().build();
    let default = FeatureFlags::default();
    for feature in all_features() {
        assert_eq!(
            built.is_enabled(feature),
            default.is_enabled(feature),
            "builder default mismatch for {feature}"
        );
    }
}

#[test]
fn builder_enable_category_enables_all_in_category() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Enterprise)
        .build();
    for feature in FeatureFlags::features_in_category(FeatureCategory::Enterprise) {
        assert!(flags.is_enabled(feature), "{feature} should be enabled");
    }
}

#[test]
fn builder_disable_category_disables_all_in_category() {
    let flags = FeatureFlags::builder()
        .disable_category(FeatureCategory::Experimental)
        .build();
    for feature in FeatureFlags::features_in_category(FeatureCategory::Experimental) {
        assert!(!flags.is_enabled(feature), "{feature} should be disabled");
    }
}

#[test]
fn builder_chaining_last_wins() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Profiling)
        .disable(Feature::Profiling)
        .build();
    assert!(!flags.is_enabled(Feature::Profiling));

    let flags = FeatureFlags::builder()
        .disable(Feature::Profiling)
        .enable(Feature::Profiling)
        .build();
    assert!(flags.is_enabled(Feature::Profiling));
}

#[test]
fn builder_category_then_individual_override() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Debug)
        .disable(Feature::Profiling)
        .build();
    assert!(flags.is_enabled(Feature::VerboseLogging));
    assert!(flags.is_enabled(Feature::DiagnosticOutput));
    assert!(!flags.is_enabled(Feature::Profiling));
    assert!(flags.is_enabled(Feature::MemoryTracking));
}

// ── FeatureFlagsHandle ──────────────────────────────────────────────────────

#[test]
fn handle_enable_disable_toggle_are_consistent() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::ZeroCopy);
    assert!(handle.is_enabled(Feature::ZeroCopy));
    handle.disable(Feature::ZeroCopy);
    assert!(!handle.is_enabled(Feature::ZeroCopy));
    handle.toggle(Feature::ZeroCopy);
    assert!(handle.is_enabled(Feature::ZeroCopy));
}

#[test]
fn handle_snapshot_is_independent_of_future_mutations() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::AdvancedOptimization);
    let snap = handle.snapshot();
    handle.disable(Feature::AdvancedOptimization);
    assert!(snap.is_enabled(Feature::AdvancedOptimization));
    assert!(!handle.is_enabled(Feature::AdvancedOptimization));
}

#[test]
fn handle_clone_yields_independent_copy() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::MutationTesting);
    let cloned = handle.clone();
    handle.disable(Feature::MutationTesting);
    assert!(cloned.is_enabled(Feature::MutationTesting));
}
