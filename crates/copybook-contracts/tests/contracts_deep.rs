// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep tests for copybook-contracts: feature flags, categories, lifecycle,
//! builder pattern, handle concurrency, and serde serialization.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_contracts::feature_flags::all_features;
use copybook_contracts::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureLifecycle,
};
use std::str::FromStr;

// ============================================================================
// 1. Feature flag contract enforcement
// ============================================================================

#[test]
fn all_features_returns_all_22_variants() {
    let features = all_features();
    assert_eq!(features.len(), 22, "expected 22 feature flags");
}

#[test]
fn feature_default_enabled_contract() {
    // These should be enabled by default
    assert!(Feature::SignSeparate.default_enabled());
    assert!(Feature::Comp1.default_enabled());
    assert!(Feature::Comp2.default_enabled());
    assert!(Feature::LruCache.default_enabled());
    // These should be disabled by default
    assert!(!Feature::RenamesR4R6.default_enabled());
    assert!(!Feature::AuditSystem.default_enabled());
    assert!(!Feature::ParallelDecode.default_enabled());
    assert!(!Feature::VerboseLogging.default_enabled());
}

#[test]
fn feature_flags_default_respects_default_enabled() {
    let flags = FeatureFlags::default();
    for f in all_features() {
        assert_eq!(
            flags.is_enabled(f),
            f.default_enabled(),
            "default mismatch for {f}"
        );
    }
}

#[test]
fn feature_enable_disable_toggle() {
    let mut flags = FeatureFlags::default();
    // Start disabled
    assert!(!flags.is_enabled(Feature::AuditSystem));
    // Enable
    flags.enable(Feature::AuditSystem);
    assert!(flags.is_enabled(Feature::AuditSystem));
    // Disable
    flags.disable(Feature::AuditSystem);
    assert!(!flags.is_enabled(Feature::AuditSystem));
    // Toggle on
    flags.toggle(Feature::AuditSystem);
    assert!(flags.is_enabled(Feature::AuditSystem));
    // Toggle off
    flags.toggle(Feature::AuditSystem);
    assert!(!flags.is_enabled(Feature::AuditSystem));
}

#[test]
fn feature_enable_idempotent() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::Profiling);
    flags.enable(Feature::Profiling);
    assert!(flags.is_enabled(Feature::Profiling));
}

// ============================================================================
// 2. Support level definitions (FeatureCategory)
// ============================================================================

#[test]
fn category_assignment_experimental() {
    assert_eq!(
        Feature::SignSeparate.category(),
        FeatureCategory::Experimental
    );
    assert_eq!(
        Feature::RenamesR4R6.category(),
        FeatureCategory::Experimental
    );
    assert_eq!(Feature::Comp1.category(), FeatureCategory::Experimental);
    assert_eq!(Feature::Comp2.category(), FeatureCategory::Experimental);
}

#[test]
fn category_assignment_enterprise() {
    assert_eq!(Feature::AuditSystem.category(), FeatureCategory::Enterprise);
    assert_eq!(
        Feature::SoxCompliance.category(),
        FeatureCategory::Enterprise
    );
    assert_eq!(
        Feature::HipaaCompliance.category(),
        FeatureCategory::Enterprise
    );
    assert_eq!(
        Feature::GdprCompliance.category(),
        FeatureCategory::Enterprise
    );
    assert_eq!(
        Feature::PciDssCompliance.category(),
        FeatureCategory::Enterprise
    );
    assert_eq!(
        Feature::SecurityMonitoring.category(),
        FeatureCategory::Enterprise
    );
}

#[test]
fn category_assignment_performance() {
    assert_eq!(
        Feature::AdvancedOptimization.category(),
        FeatureCategory::Performance
    );
    assert_eq!(Feature::LruCache.category(), FeatureCategory::Performance);
    assert_eq!(
        Feature::ParallelDecode.category(),
        FeatureCategory::Performance
    );
    assert_eq!(Feature::ZeroCopy.category(), FeatureCategory::Performance);
}

#[test]
fn category_assignment_debug() {
    assert_eq!(Feature::VerboseLogging.category(), FeatureCategory::Debug);
    assert_eq!(Feature::DiagnosticOutput.category(), FeatureCategory::Debug);
    assert_eq!(Feature::Profiling.category(), FeatureCategory::Debug);
    assert_eq!(Feature::MemoryTracking.category(), FeatureCategory::Debug);
}

#[test]
fn category_assignment_testing() {
    assert_eq!(
        Feature::MutationTesting.category(),
        FeatureCategory::Testing
    );
    assert_eq!(
        Feature::FuzzingIntegration.category(),
        FeatureCategory::Testing
    );
    assert_eq!(
        Feature::CoverageInstrumentation.category(),
        FeatureCategory::Testing
    );
    assert_eq!(
        Feature::PropertyBasedTesting.category(),
        FeatureCategory::Testing
    );
}

#[test]
fn features_in_category_experimental_count() {
    let feats = FeatureFlags::features_in_category(FeatureCategory::Experimental);
    assert_eq!(feats.len(), 4);
}

#[test]
fn features_in_category_enterprise_count() {
    let feats = FeatureFlags::features_in_category(FeatureCategory::Enterprise);
    assert_eq!(feats.len(), 6);
}

// ============================================================================
// 3. Governance binding contracts (builder pattern)
// ============================================================================

#[test]
fn builder_enable_single_feature() {
    let flags = FeatureFlags::builder().enable(Feature::Profiling).build();
    assert!(flags.is_enabled(Feature::Profiling));
}

#[test]
fn builder_disable_default_feature() {
    let flags = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .build();
    assert!(!flags.is_enabled(Feature::SignSeparate));
}

#[test]
fn builder_enable_entire_category() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Debug)
        .build();
    assert!(flags.is_enabled(Feature::VerboseLogging));
    assert!(flags.is_enabled(Feature::DiagnosticOutput));
    assert!(flags.is_enabled(Feature::Profiling));
    assert!(flags.is_enabled(Feature::MemoryTracking));
}

#[test]
fn builder_disable_entire_category() {
    let flags = FeatureFlags::builder()
        .disable_category(FeatureCategory::Experimental)
        .build();
    assert!(!flags.is_enabled(Feature::SignSeparate));
    assert!(!flags.is_enabled(Feature::RenamesR4R6));
    assert!(!flags.is_enabled(Feature::Comp1));
    assert!(!flags.is_enabled(Feature::Comp2));
}

#[test]
fn builder_chained_operations() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Enterprise)
        .disable(Feature::GdprCompliance)
        .enable(Feature::Profiling)
        .build();
    assert!(flags.is_enabled(Feature::AuditSystem));
    assert!(!flags.is_enabled(Feature::GdprCompliance));
    assert!(flags.is_enabled(Feature::Profiling));
}

// ============================================================================
// 4. Contract serialization
// ============================================================================

#[test]
fn feature_serde_roundtrip() {
    for f in all_features() {
        let json = serde_json::to_string(&f).unwrap();
        let back: Feature = serde_json::from_str(&json).unwrap();
        assert_eq!(back, f, "serde roundtrip for {f}");
    }
}

#[test]
fn feature_flags_serde_roundtrip() {
    let flags = FeatureFlags::builder()
        .enable(Feature::AuditSystem)
        .enable(Feature::Profiling)
        .disable(Feature::LruCache)
        .build();
    let json = serde_json::to_string(&flags).unwrap();
    let back: FeatureFlags = serde_json::from_str(&json).unwrap();
    assert_eq!(
        back.is_enabled(Feature::AuditSystem),
        flags.is_enabled(Feature::AuditSystem)
    );
    assert_eq!(
        back.is_enabled(Feature::Profiling),
        flags.is_enabled(Feature::Profiling)
    );
    assert_eq!(
        back.is_enabled(Feature::LruCache),
        flags.is_enabled(Feature::LruCache)
    );
}

#[test]
fn feature_category_serde_roundtrip() {
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
        assert_eq!(back, cat, "category serde roundtrip for {cat}");
    }
}

#[test]
fn feature_lifecycle_serde_roundtrip() {
    let stages = [
        FeatureLifecycle::Experimental,
        FeatureLifecycle::Stable,
        FeatureLifecycle::Deprecated,
    ];
    for lc in stages {
        let json = serde_json::to_string(&lc).unwrap();
        let back: FeatureLifecycle = serde_json::from_str(&json).unwrap();
        assert_eq!(back, lc, "lifecycle serde roundtrip for {lc}");
    }
}

// ============================================================================
// 5. Feature Display/FromStr roundtrip
// ============================================================================

#[test]
fn feature_display_from_str_roundtrip() {
    for f in all_features() {
        let s = f.to_string();
        let parsed = Feature::from_str(&s).unwrap();
        assert_eq!(parsed, f, "Display→FromStr roundtrip for {f}");
    }
}

#[test]
fn feature_from_str_case_insensitive() {
    assert_eq!(
        Feature::from_str("SIGN_SEPARATE").unwrap(),
        Feature::SignSeparate
    );
    assert_eq!(
        Feature::from_str("Verbose_Logging").unwrap(),
        Feature::VerboseLogging
    );
}

#[test]
fn feature_from_str_invalid_returns_error() {
    let result = Feature::from_str("nonexistent_feature");
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Unknown feature flag"));
}

// ============================================================================
// 6. Feature metadata
// ============================================================================

#[test]
fn feature_description_non_empty() {
    for f in all_features() {
        let desc = f.description();
        assert!(!desc.is_empty(), "description for {f} should not be empty");
    }
}

#[test]
fn feature_env_var_name_format() {
    let name = Feature::SignSeparate.env_var_name();
    assert!(
        name.starts_with("COPYBOOK_FF_"),
        "env var should start with COPYBOOK_FF_"
    );
    assert!(
        name.chars().all(|c| c.is_ascii_uppercase() || c == '_'),
        "env var should be all uppercase: {name}"
    );
}

#[test]
fn feature_env_var_names_unique() {
    let names: Vec<String> = all_features().iter().map(|f| f.env_var_name()).collect();
    let unique: std::collections::HashSet<&String> = names.iter().collect();
    assert_eq!(
        names.len(),
        unique.len(),
        "all env var names should be unique"
    );
}

// ============================================================================
// 7. FeatureFlagsHandle thread-safety
// ============================================================================

#[test]
fn handle_enable_disable_toggle() {
    let handle = FeatureFlagsHandle::new();
    assert!(!handle.is_enabled(Feature::Profiling));
    handle.enable(Feature::Profiling);
    assert!(handle.is_enabled(Feature::Profiling));
    handle.disable(Feature::Profiling);
    assert!(!handle.is_enabled(Feature::Profiling));
    handle.toggle(Feature::Profiling);
    assert!(handle.is_enabled(Feature::Profiling));
}

#[test]
fn handle_snapshot_is_independent_copy() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::Profiling);
    let snap = handle.snapshot();
    handle.disable(Feature::Profiling);
    // Snapshot retains the state at time of capture
    assert!(snap.is_enabled(Feature::Profiling));
    assert!(!handle.is_enabled(Feature::Profiling));
}

#[test]
fn handle_clone_creates_independent_copy() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::Profiling);
    let cloned = handle.clone();
    handle.disable(Feature::Profiling);
    assert!(cloned.is_enabled(Feature::Profiling));
}

// ============================================================================
// 8. Category display
// ============================================================================

#[test]
fn category_display_all_variants() {
    assert_eq!(FeatureCategory::Experimental.to_string(), "experimental");
    assert_eq!(FeatureCategory::Enterprise.to_string(), "enterprise");
    assert_eq!(FeatureCategory::Performance.to_string(), "performance");
    assert_eq!(FeatureCategory::Debug.to_string(), "debug");
    assert_eq!(FeatureCategory::Testing.to_string(), "testing");
}

#[test]
fn lifecycle_display_all_variants() {
    assert_eq!(FeatureLifecycle::Experimental.to_string(), "experimental");
    assert_eq!(FeatureLifecycle::Stable.to_string(), "stable");
    assert_eq!(FeatureLifecycle::Deprecated.to_string(), "deprecated");
}

// ============================================================================
// 9. Enabled features iteration
// ============================================================================

#[test]
fn enabled_features_count_matches_defaults() {
    let flags = FeatureFlags::default();
    let count = flags.enabled_features().count();
    let expected = all_features()
        .iter()
        .filter(|f| f.default_enabled())
        .count();
    assert_eq!(count, expected);
}

#[test]
fn enabled_in_category_returns_correct_subset() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Debug)
        .build();
    let debug_enabled = flags.enabled_in_category(FeatureCategory::Debug);
    assert_eq!(
        debug_enabled.len(),
        4,
        "all 4 debug features should be enabled"
    );
    for f in &debug_enabled {
        assert_eq!(f.category(), FeatureCategory::Debug);
    }
}
