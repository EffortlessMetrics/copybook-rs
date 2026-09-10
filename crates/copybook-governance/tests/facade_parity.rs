// SPDX-License-Identifier: AGPL-3.0-or-later

//! Facade parity tests ported from the collapsed governance-contracts crate.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_governance::support_matrix::{all_features, find_feature, find_feature_by_id};
use copybook_governance::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureId, FeatureLifecycle,
    SupportStatus,
};
#[test]
fn test_reexported_feature_flags_builder() {
    let flags = FeatureFlags::builder()
        .enable(Feature::RenamesR4R6)
        .disable(Feature::LruCache)
        .build();
    assert!(flags.is_enabled(Feature::RenamesR4R6));
    assert!(!flags.is_enabled(Feature::LruCache));
}

#[test]
fn test_reexported_feature_category() {
    assert_eq!(
        Feature::RenamesR4R6.category(),
        FeatureCategory::Experimental
    );
}

#[test]
fn test_reexported_feature_lifecycle() {
    // Ensure FeatureLifecycle is accessible through facade
    let _ = FeatureLifecycle::Experimental;
    let _ = FeatureLifecycle::Stable;
    let _ = FeatureLifecycle::Deprecated;
}

#[test]
fn test_reexported_support_matrix_all_features() {
    let features = all_features();
    assert_eq!(features.len(), 7);
}

#[test]
fn test_reexported_find_feature_by_id() {
    let f = find_feature_by_id(FeatureId::EditedPic);
    assert!(f.is_some());
    assert_eq!(f.unwrap().status, SupportStatus::Supported);
}

#[test]
fn test_reexported_find_feature_by_string() {
    let f = find_feature("nested-odo");
    assert!(f.is_some());
    assert_eq!(f.unwrap().id, FeatureId::NestedOdo);
}

#[test]
fn test_feature_flags_default_has_expected_defaults() {
    // #656 Phase C: LruCache is the only default-enabled flag.
    let flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::LruCache));
    assert!(!flags.is_enabled(Feature::RenamesR4R6));
    assert!(!flags.is_enabled(Feature::AuditSystem));
}

#[test]
fn test_feature_flags_handle_via_facade() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::Profiling);
    assert!(handle.is_enabled(Feature::Profiling));
    handle.toggle(Feature::Profiling);
    assert!(!handle.is_enabled(Feature::Profiling));
}

#[test]
fn test_support_status_variants_accessible() {
    let _ = SupportStatus::Supported;
    let _ = SupportStatus::Partial;
    let _ = SupportStatus::Planned;
    let _ = SupportStatus::NotPlanned;
}

#[test]
fn test_find_feature_unknown_returns_none() {
    assert!(find_feature("nonexistent").is_none());
}

#[test]
fn test_cross_module_feature_id_equality() {
    let from_matrix = find_feature_by_id(FeatureId::Level88Conditions).unwrap();
    let from_string = find_feature("level-88").unwrap();
    assert_eq!(from_matrix.id, from_string.id);
}

#[test]
fn test_feature_flags_enable_disable_category_via_facade() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Debug)
        .build();
    assert!(flags.is_enabled(Feature::Profiling));
    assert!(flags.is_enabled(Feature::MemoryTracking));
}
