// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-contracts feature-flag governance.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_contracts::feature_flags::all_features;
use copybook_contracts::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureLifecycle,
};
use std::str::FromStr;

// ── Feature: Display ────────────────────────────────────────────────

#[test]
fn feature_display_uses_snake_case() {
    assert_eq!(Feature::SignSeparate.to_string(), "sign_separate");
    assert_eq!(Feature::RenamesR4R6.to_string(), "renames_r4_r6");
    assert_eq!(Feature::Comp1.to_string(), "comp_1");
    assert_eq!(Feature::Comp2.to_string(), "comp_2");
    assert_eq!(Feature::AuditSystem.to_string(), "audit_system");
    assert_eq!(Feature::PciDssCompliance.to_string(), "pci_dss_compliance");
    assert_eq!(
        Feature::AdvancedOptimization.to_string(),
        "advanced_optimization"
    );
    assert_eq!(Feature::ParallelDecode.to_string(), "parallel_decode");
    assert_eq!(Feature::DiagnosticOutput.to_string(), "diagnostic_output");
    assert_eq!(
        Feature::CoverageInstrumentation.to_string(),
        "coverage_instrumentation"
    );
}

// ── Feature: FromStr ────────────────────────────────────────────────

#[test]
fn feature_from_str_roundtrip_all_variants() {
    for f in all_features() {
        let s = f.to_string();
        let parsed = Feature::from_str(&s).unwrap();
        assert_eq!(parsed, f);
    }
}

#[test]
fn feature_from_str_case_insensitive() {
    assert_eq!(
        Feature::from_str("VERBOSE_LOGGING").unwrap(),
        Feature::VerboseLogging
    );
    assert_eq!(Feature::from_str("Profiling").unwrap(), Feature::Profiling);
    assert_eq!(
        Feature::from_str("MEMORY_TRACKING").unwrap(),
        Feature::MemoryTracking
    );
}

#[test]
fn feature_from_str_rejects_unknown() {
    let err = Feature::from_str("nonexistent").unwrap_err();
    assert!(err.contains("Unknown feature flag"));
    assert!(err.contains("nonexistent"));
}

#[test]
fn feature_from_str_rejects_empty_string() {
    assert!(Feature::from_str("").is_err());
}

#[test]
fn feature_from_str_rejects_partial_match() {
    assert!(Feature::from_str("sign").is_err());
    assert!(Feature::from_str("lru").is_err());
}

// ── Feature: category ───────────────────────────────────────────────

#[test]
fn feature_category_experimental_members() {
    let experimental = [
        Feature::SignSeparate,
        Feature::RenamesR4R6,
        Feature::Comp1,
        Feature::Comp2,
    ];
    for f in experimental {
        assert_eq!(f.category(), FeatureCategory::Experimental, "{f}");
    }
}

#[test]
fn feature_category_enterprise_members() {
    let enterprise = [
        Feature::AuditSystem,
        Feature::SoxCompliance,
        Feature::HipaaCompliance,
        Feature::GdprCompliance,
        Feature::PciDssCompliance,
        Feature::SecurityMonitoring,
    ];
    for f in enterprise {
        assert_eq!(f.category(), FeatureCategory::Enterprise, "{f}");
    }
}

#[test]
fn feature_category_performance_members() {
    let perf = [
        Feature::AdvancedOptimization,
        Feature::LruCache,
        Feature::ParallelDecode,
        Feature::ZeroCopy,
    ];
    for f in perf {
        assert_eq!(f.category(), FeatureCategory::Performance, "{f}");
    }
}

#[test]
fn feature_category_debug_members() {
    let debug = [
        Feature::VerboseLogging,
        Feature::DiagnosticOutput,
        Feature::Profiling,
        Feature::MemoryTracking,
    ];
    for f in debug {
        assert_eq!(f.category(), FeatureCategory::Debug, "{f}");
    }
}

#[test]
fn feature_category_testing_members() {
    let testing = [
        Feature::MutationTesting,
        Feature::FuzzingIntegration,
        Feature::CoverageInstrumentation,
        Feature::PropertyBasedTesting,
    ];
    for f in testing {
        assert_eq!(f.category(), FeatureCategory::Testing, "{f}");
    }
}

// ── Feature: default_enabled ────────────────────────────────────────

#[test]
fn feature_default_enabled_set() {
    let enabled_by_default = [
        Feature::SignSeparate,
        Feature::Comp1,
        Feature::Comp2,
        Feature::LruCache,
    ];
    for f in enabled_by_default {
        assert!(f.default_enabled(), "{f} should be default-enabled");
    }
}

#[test]
fn feature_default_disabled_set() {
    let disabled_by_default = [
        Feature::RenamesR4R6,
        Feature::AuditSystem,
        Feature::SoxCompliance,
        Feature::AdvancedOptimization,
        Feature::VerboseLogging,
        Feature::MutationTesting,
    ];
    for f in disabled_by_default {
        assert!(!f.default_enabled(), "{f} should be default-disabled");
    }
}

// ── Feature: env_var_name ───────────────────────────────────────────

#[test]
fn feature_env_var_name_prefix_and_uppercase() {
    for f in all_features() {
        let name = f.env_var_name();
        assert!(name.starts_with("COPYBOOK_FF_"), "bad prefix: {name}");
        assert_eq!(name, name.to_uppercase(), "not uppercase: {name}");
    }
}

#[test]
fn feature_env_var_name_specific_values() {
    assert_eq!(
        Feature::ParallelDecode.env_var_name(),
        "COPYBOOK_FF_PARALLEL_DECODE"
    );
    assert_eq!(
        Feature::GdprCompliance.env_var_name(),
        "COPYBOOK_FF_GDPR_COMPLIANCE"
    );
}

// ── Feature: description ────────────────────────────────────────────

#[test]
fn feature_description_non_empty_for_all() {
    for f in all_features() {
        assert!(!f.description().is_empty(), "{f} has empty description");
    }
}

#[test]
fn feature_description_contains_relevant_keyword() {
    assert!(Feature::AuditSystem.description().contains("audit"));
    assert!(Feature::LruCache.description().contains("LRU"));
    assert!(Feature::Profiling.description().contains("profiling"));
}

// ── Feature: Serde roundtrip ────────────────────────────────────────

#[test]
fn feature_serde_json_roundtrip_all_variants() {
    for f in all_features() {
        let json = serde_json::to_string(&f).unwrap();
        let back: Feature = serde_json::from_str(&json).unwrap();
        assert_eq!(back, f, "serde roundtrip failed for {f}");
    }
}

#[test]
fn feature_serde_json_uses_snake_case() {
    let json = serde_json::to_string(&Feature::PropertyBasedTesting).unwrap();
    assert_eq!(json, "\"property_based_testing\"");
}

#[test]
fn feature_serde_alias_works() {
    // The serde alias should accept snake_case
    let back: Feature = serde_json::from_str("\"sign_separate\"").unwrap();
    assert_eq!(back, Feature::SignSeparate);
}

// ── Feature: Debug ──────────────────────────────────────────────────

#[test]
fn feature_debug_output_is_non_empty() {
    let dbg = format!("{:?}", Feature::ZeroCopy);
    assert!(!dbg.is_empty());
    assert!(dbg.contains("ZeroCopy"));
}

// ── FeatureCategory: Display ────────────────────────────────────────

#[test]
fn feature_category_display_all() {
    assert_eq!(FeatureCategory::Experimental.to_string(), "experimental");
    assert_eq!(FeatureCategory::Enterprise.to_string(), "enterprise");
    assert_eq!(FeatureCategory::Performance.to_string(), "performance");
    assert_eq!(FeatureCategory::Debug.to_string(), "debug");
    assert_eq!(FeatureCategory::Testing.to_string(), "testing");
}

// ── FeatureCategory: Serde roundtrip ────────────────────────────────

#[test]
fn feature_category_serde_roundtrip_all() {
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

// ── FeatureLifecycle: Display ───────────────────────────────────────

#[test]
fn feature_lifecycle_display_all() {
    assert_eq!(FeatureLifecycle::Experimental.to_string(), "experimental");
    assert_eq!(FeatureLifecycle::Stable.to_string(), "stable");
    assert_eq!(FeatureLifecycle::Deprecated.to_string(), "deprecated");
}

// ── FeatureLifecycle: Serde roundtrip ───────────────────────────────

#[test]
fn feature_lifecycle_serde_roundtrip_all() {
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

// ── FeatureLifecycle: Debug ─────────────────────────────────────────

#[test]
fn feature_lifecycle_debug_output() {
    let dbg = format!("{:?}", FeatureLifecycle::Deprecated);
    assert!(dbg.contains("Deprecated"));
}

// ── all_features ────────────────────────────────────────────────────

#[test]
fn all_features_returns_22_variants() {
    assert_eq!(all_features().len(), 22);
}

#[test]
fn all_features_has_no_duplicates() {
    let features = all_features();
    let mut set = std::collections::HashSet::new();
    for f in &features {
        assert!(set.insert(f), "duplicate feature: {f}");
    }
}

// ── FeatureFlags: default ───────────────────────────────────────────

#[test]
fn default_flags_matches_default_enabled_property() {
    let flags = FeatureFlags::default();
    for f in all_features() {
        assert_eq!(flags.is_enabled(f), f.default_enabled(), "mismatch for {f}");
    }
}

#[test]
fn default_flags_enabled_count_is_4() {
    let flags = FeatureFlags::default();
    assert_eq!(flags.enabled_features().count(), 4);
}

// ── FeatureFlags: enable / disable / toggle ─────────────────────────

#[test]
fn enable_then_disable_round_trip() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::SecurityMonitoring);
    assert!(flags.is_enabled(Feature::SecurityMonitoring));
    flags.disable(Feature::SecurityMonitoring);
    assert!(!flags.is_enabled(Feature::SecurityMonitoring));
}

#[test]
fn toggle_flips_state() {
    let mut flags = FeatureFlags::default();
    let before = flags.is_enabled(Feature::LruCache);
    flags.toggle(Feature::LruCache);
    assert_ne!(flags.is_enabled(Feature::LruCache), before);
    flags.toggle(Feature::LruCache);
    assert_eq!(flags.is_enabled(Feature::LruCache), before);
}

#[test]
fn enable_idempotent() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::LruCache);
    flags.enable(Feature::LruCache);
    assert!(flags.is_enabled(Feature::LruCache));
}

#[test]
fn disable_idempotent() {
    let mut flags = FeatureFlags::default();
    flags.disable(Feature::Profiling);
    flags.disable(Feature::Profiling);
    assert!(!flags.is_enabled(Feature::Profiling));
}

#[test]
fn enable_all_then_disable_all() {
    let mut flags = FeatureFlags::default();
    for f in all_features() {
        flags.enable(f);
    }
    assert_eq!(flags.enabled_features().count(), 22);
    for f in all_features() {
        flags.disable(f);
    }
    assert_eq!(flags.enabled_features().count(), 0);
}

// ── FeatureFlags: enabled_in_category ───────────────────────────────

#[test]
fn enabled_in_category_empty_when_none_enabled() {
    let mut flags = FeatureFlags::default();
    // Debug is disabled by default
    assert!(flags.enabled_in_category(FeatureCategory::Debug).is_empty());
    flags.enable(Feature::Profiling);
    let debug = flags.enabled_in_category(FeatureCategory::Debug);
    assert_eq!(debug.len(), 1);
    assert!(debug.contains(&Feature::Profiling));
}

// ── FeatureFlags: features_in_category (static) ─────────────────────

#[test]
fn features_in_category_counts() {
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
fn features_in_category_sum_equals_total() {
    let total: usize = [
        FeatureCategory::Experimental,
        FeatureCategory::Enterprise,
        FeatureCategory::Performance,
        FeatureCategory::Debug,
        FeatureCategory::Testing,
    ]
    .iter()
    .map(|c| FeatureFlags::features_in_category(*c).len())
    .sum();
    assert_eq!(total, 22);
}

// ── FeatureFlags: Serde roundtrip ───────────────────────────────────

#[test]
fn feature_flags_serde_roundtrip_preserves_state() {
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

#[test]
fn feature_flags_serde_empty_set() {
    let mut flags = FeatureFlags::default();
    for f in all_features() {
        flags.disable(f);
    }
    let json = serde_json::to_string(&flags).unwrap();
    let restored: FeatureFlags = serde_json::from_str(&json).unwrap();
    for f in all_features() {
        assert!(!restored.is_enabled(f), "{f} should be disabled");
    }
}

// ── FeatureFlagsBuilder ─────────────────────────────────────────────

#[test]
fn builder_starts_from_defaults() {
    let built = FeatureFlags::builder().build();
    let def = FeatureFlags::default();
    for f in all_features() {
        assert_eq!(built.is_enabled(f), def.is_enabled(f), "{f}");
    }
}

#[test]
fn builder_enable_category_enables_all() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Enterprise)
        .build();
    for f in FeatureFlags::features_in_category(FeatureCategory::Enterprise) {
        assert!(flags.is_enabled(f), "{f} should be enabled");
    }
}

#[test]
fn builder_disable_category_disables_all() {
    let flags = FeatureFlags::builder()
        .disable_category(FeatureCategory::Experimental)
        .build();
    for f in FeatureFlags::features_in_category(FeatureCategory::Experimental) {
        assert!(!flags.is_enabled(f), "{f} should be disabled");
    }
}

#[test]
fn builder_last_operation_wins() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Profiling)
        .disable(Feature::Profiling)
        .build();
    assert!(!flags.is_enabled(Feature::Profiling));

    let flags = FeatureFlags::builder()
        .disable(Feature::LruCache)
        .enable(Feature::LruCache)
        .build();
    assert!(flags.is_enabled(Feature::LruCache));
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

#[test]
fn builder_debug_is_implemented() {
    let builder = FeatureFlags::builder();
    let dbg = format!("{:?}", builder);
    assert!(dbg.contains("FeatureFlagsBuilder"));
}

// ── FeatureFlagsHandle ──────────────────────────────────────────────

#[test]
fn handle_enable_disable_toggle() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::ZeroCopy);
    assert!(handle.is_enabled(Feature::ZeroCopy));
    handle.disable(Feature::ZeroCopy);
    assert!(!handle.is_enabled(Feature::ZeroCopy));
    handle.toggle(Feature::ZeroCopy);
    assert!(handle.is_enabled(Feature::ZeroCopy));
}

#[test]
fn handle_snapshot_is_independent() {
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
    assert!(!handle.is_enabled(Feature::MutationTesting));
}

#[test]
fn handle_default_reflects_defaults() {
    let handle = FeatureFlagsHandle::default();
    assert!(handle.is_enabled(Feature::LruCache));
    assert!(!handle.is_enabled(Feature::Profiling));
}

#[test]
fn handle_debug_is_implemented() {
    let handle = FeatureFlagsHandle::new();
    let dbg = format!("{:?}", handle);
    assert!(dbg.contains("FeatureFlagsHandle"));
}

// ── Thread-safety smoke test ────────────────────────────────────────

#[test]
fn handle_is_send_and_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<FeatureFlagsHandle>();
}

#[test]
fn feature_flags_is_send_and_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<FeatureFlags>();
}
