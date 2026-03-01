#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

//! Contract façade for governance interoperability.
//!
//! This crate keeps the cross-cutting governance contract imports small and stable
//! for downstream crates.

/// Re-exported feature flag contract types.
pub mod feature_flags {
    pub use copybook_contracts::feature_flags::*;
}

/// Re-exported support matrix contract types.
pub mod support_matrix {
    pub use copybook_support_matrix::*;
}

pub use feature_flags::FeatureLifecycle;
pub use feature_flags::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
};
pub use support_matrix::{
    FeatureId, FeatureSupport, SupportStatus, all_features, find_feature, find_feature_by_id,
};

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_reexported_feature_flags_builder() {
        let flags = FeatureFlags::builder()
            .enable(Feature::Comp1)
            .disable(Feature::LruCache)
            .build();
        assert!(flags.is_enabled(Feature::Comp1));
        assert!(!flags.is_enabled(Feature::LruCache));
    }

    #[test]
    fn test_reexported_feature_category() {
        assert_eq!(Feature::Comp1.category(), FeatureCategory::Experimental);
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
        let flags = FeatureFlags::default();
        assert!(flags.is_enabled(Feature::SignSeparate));
        assert!(flags.is_enabled(Feature::Comp1));
        assert!(flags.is_enabled(Feature::Comp2));
        assert!(flags.is_enabled(Feature::LruCache));
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
            .enable_category(FeatureCategory::Testing)
            .build();
        assert!(flags.is_enabled(Feature::MutationTesting));
        assert!(flags.is_enabled(Feature::FuzzingIntegration));
    }
}
