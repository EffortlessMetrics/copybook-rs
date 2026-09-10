// SPDX-License-Identifier: AGPL-3.0-or-later
//! Feature flag system contract for copybook-rs.

use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::env;
use std::fmt;
use std::str::FromStr;
use std::sync::{OnceLock, RwLock};

/// All available feature flags.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum Feature {
    // ========== Experimental Features ==========
    // NOTE (#656 Phase C, v0.6.0): `SignSeparate`, `Comp1`, and `Comp2` were
    // removed. SIGN SEPARATE, COMP-1, and COMP-2 are stable, documented COBOL
    // behavior and are now parsed unconditionally; they are no longer
    // runtime-toggled. The `COPYBOOK_FF_SIGN_SEPARATE`, `COPYBOOK_FF_COMP_1`,
    // and `COPYBOOK_FF_COMP_2` environment variables and the matching
    // `--enable-features`/`--disable-features` names are no longer recognized.
    /// Enable RENAMES R4-R6 advanced scenarios
    #[serde(alias = "renames_r4_r6")]
    RenamesR4R6,

    // ========== Enterprise Features ==========
    /// Enable audit system for compliance tracking
    #[serde(alias = "audit_system")]
    AuditSystem,

    /// Enable SOX compliance validation
    #[serde(alias = "sox_compliance")]
    SoxCompliance,

    /// Enable HIPAA compliance validation
    #[serde(alias = "hipaa_compliance")]
    HipaaCompliance,

    /// Enable GDPR compliance validation
    #[serde(alias = "gdpr_compliance")]
    GdprCompliance,

    /// Enable PCI DSS compliance validation
    #[serde(alias = "pci_dss_compliance")]
    PciDssCompliance,

    /// Enable security monitoring integration
    #[serde(alias = "security_monitoring")]
    SecurityMonitoring,

    // ========== Performance Features ==========
    /// Enable advanced optimization mode (SIMD, vectorization)
    #[serde(alias = "advanced_optimization")]
    AdvancedOptimization,

    /// Enable LRU cache for parsed copybooks
    #[serde(alias = "lru_cache")]
    LruCache,

    /// Enable parallel decoding for large files
    #[serde(alias = "parallel_decode")]
    ParallelDecode,

    /// Enable zero-copy parsing where possible
    #[serde(alias = "zero_copy")]
    ZeroCopy,

    // ========== Debug Features ==========
    /// Enable verbose logging with detailed diagnostics
    #[serde(alias = "verbose_logging")]
    VerboseLogging,

    /// Enable diagnostic output for troubleshooting
    #[serde(alias = "diagnostic_output")]
    DiagnosticOutput,

    /// Enable CPU profiling hooks
    #[serde(alias = "profiling")]
    Profiling,

    /// Enable memory usage tracking
    #[serde(alias = "memory_tracking")]
    MemoryTracking,
}

impl Feature {
    /// Get the toggle-group category this feature belongs to.
    ///
    /// `category()` is a **grouping mechanism** used by `enable_category` /
    /// `enabled_in_category` to toggle related flags together; it is **not** a
    /// stability class — the stability class of a feature is reported by
    /// [`Feature::lifecycle`]. Stable, documented COBOL language behavior
    /// (SIGN SEPARATE, COMP-1, COMP-2) is ordinary parser behavior, not a
    /// runtime flag at all (see `docs/reference/COBOL_SUPPORT_MATRIX.md`).
    #[inline]
    #[must_use]
    pub const fn category(self) -> FeatureCategory {
        match self {
            Feature::RenamesR4R6 => FeatureCategory::Experimental,
            Feature::AuditSystem
            | Feature::SoxCompliance
            | Feature::HipaaCompliance
            | Feature::GdprCompliance
            | Feature::PciDssCompliance
            | Feature::SecurityMonitoring => FeatureCategory::Enterprise,
            Feature::AdvancedOptimization
            | Feature::LruCache
            | Feature::ParallelDecode
            | Feature::ZeroCopy => FeatureCategory::Performance,
            Feature::VerboseLogging
            | Feature::DiagnosticOutput
            | Feature::Profiling
            | Feature::MemoryTracking => FeatureCategory::Debug,
        }
    }

    /// Get the stability lifecycle class of this feature.
    ///
    /// This is the authoritative stability signal (distinct from the
    /// toggle-group [`Feature::category`]). Stable, documented COBOL language
    /// behavior (SIGN SEPARATE, COMP-1, COMP-2) is ordinary parser behavior
    /// and is not represented by a flag at all since #656 Phase C; every
    /// remaining flag (advanced RENAMES R4-R6, enterprise/compliance,
    /// performance, and debug hooks) is opt-in and reports
    /// [`FeatureLifecycle::Experimental`].
    #[inline]
    #[must_use]
    pub const fn lifecycle(self) -> FeatureLifecycle {
        match self {
            Feature::RenamesR4R6
            | Feature::AuditSystem
            | Feature::SoxCompliance
            | Feature::HipaaCompliance
            | Feature::GdprCompliance
            | Feature::PciDssCompliance
            | Feature::SecurityMonitoring
            | Feature::AdvancedOptimization
            | Feature::LruCache
            | Feature::ParallelDecode
            | Feature::ZeroCopy
            | Feature::VerboseLogging
            | Feature::DiagnosticOutput
            | Feature::Profiling
            | Feature::MemoryTracking => FeatureLifecycle::Experimental,
        }
    }

    /// Get the default enabled state for this feature.
    #[inline]
    #[must_use]
    pub const fn default_enabled(self) -> bool {
        match self {
            Feature::LruCache => true,
            Feature::RenamesR4R6
            | Feature::AuditSystem
            | Feature::SoxCompliance
            | Feature::HipaaCompliance
            | Feature::GdprCompliance
            | Feature::PciDssCompliance
            | Feature::SecurityMonitoring
            | Feature::AdvancedOptimization
            | Feature::ParallelDecode
            | Feature::ZeroCopy
            | Feature::VerboseLogging
            | Feature::DiagnosticOutput
            | Feature::Profiling
            | Feature::MemoryTracking => false,
        }
    }

    /// Get the environment variable name for this feature.
    #[inline]
    #[must_use]
    pub fn env_var_name(self) -> String {
        format!("COPYBOOK_FF_{}", self.to_string().to_uppercase())
    }

    /// Get a human-readable description of this feature.
    #[inline]
    #[must_use]
    pub const fn description(self) -> &'static str {
        match self {
            Feature::RenamesR4R6 => "Enable RENAMES R4-R6 advanced scenarios",
            Feature::AuditSystem => "Enable audit system for compliance tracking",
            Feature::SoxCompliance => "Enable SOX compliance validation",
            Feature::HipaaCompliance => "Enable HIPAA compliance validation",
            Feature::GdprCompliance => "Enable GDPR compliance validation",
            Feature::PciDssCompliance => "Enable PCI DSS compliance validation",
            Feature::SecurityMonitoring => "Enable security monitoring integration",
            Feature::AdvancedOptimization => {
                "Enable advanced optimization mode (SIMD, vectorization)"
            }
            Feature::LruCache => "Enable LRU cache for parsed copybooks",
            Feature::ParallelDecode => "Enable parallel decoding for large files",
            Feature::ZeroCopy => "Enable zero-copy parsing where possible",
            Feature::VerboseLogging => "Enable verbose logging with detailed diagnostics",
            Feature::DiagnosticOutput => "Enable diagnostic output for troubleshooting",
            Feature::Profiling => "Enable CPU profiling hooks",
            Feature::MemoryTracking => "Enable memory usage tracking",
        }
    }
}

impl fmt::Display for Feature {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Feature::RenamesR4R6 => "renames_r4_r6",
            Feature::AuditSystem => "audit_system",
            Feature::SoxCompliance => "sox_compliance",
            Feature::HipaaCompliance => "hipaa_compliance",
            Feature::GdprCompliance => "gdpr_compliance",
            Feature::PciDssCompliance => "pci_dss_compliance",
            Feature::SecurityMonitoring => "security_monitoring",
            Feature::AdvancedOptimization => "advanced_optimization",
            Feature::LruCache => "lru_cache",
            Feature::ParallelDecode => "parallel_decode",
            Feature::ZeroCopy => "zero_copy",
            Feature::VerboseLogging => "verbose_logging",
            Feature::DiagnosticOutput => "diagnostic_output",
            Feature::Profiling => "profiling",
            Feature::MemoryTracking => "memory_tracking",
        };
        write!(f, "{s}")
    }
}

impl FromStr for Feature {
    type Err = String;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "renames_r4_r6" => Ok(Self::RenamesR4R6),
            "audit_system" => Ok(Self::AuditSystem),
            "sox_compliance" => Ok(Self::SoxCompliance),
            "hipaa_compliance" => Ok(Self::HipaaCompliance),
            "gdpr_compliance" => Ok(Self::GdprCompliance),
            "pci_dss_compliance" => Ok(Self::PciDssCompliance),
            "security_monitoring" => Ok(Self::SecurityMonitoring),
            "advanced_optimization" => Ok(Self::AdvancedOptimization),
            "lru_cache" => Ok(Self::LruCache),
            "parallel_decode" => Ok(Self::ParallelDecode),
            "zero_copy" => Ok(Self::ZeroCopy),
            "verbose_logging" => Ok(Self::VerboseLogging),
            "diagnostic_output" => Ok(Self::DiagnosticOutput),
            "profiling" => Ok(Self::Profiling),
            "memory_tracking" => Ok(Self::MemoryTracking),
            _ => Err(format!("Unknown feature flag: '{s}'")),
        }
    }
}

/// Feature category for grouping related features.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum FeatureCategory {
    /// Pre-release features under active development.
    Experimental,
    /// Compliance and audit-related features for regulated environments.
    Enterprise,
    /// Optimization features for throughput and memory.
    Performance,
    /// Diagnostic and profiling features for development.
    Debug,
}

impl fmt::Display for FeatureCategory {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeatureCategory::Experimental => write!(f, "experimental"),
            FeatureCategory::Enterprise => write!(f, "enterprise"),
            FeatureCategory::Performance => write!(f, "performance"),
            FeatureCategory::Debug => write!(f, "debug"),
        }
    }
}

/// Feature lifecycle stage.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum FeatureLifecycle {
    /// Feature is under active development and may change.
    Experimental,
    /// Feature is production-ready with stable API.
    Stable,
    /// Feature is scheduled for removal in a future release.
    Deprecated,
}

impl fmt::Display for FeatureLifecycle {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeatureLifecycle::Experimental => write!(f, "experimental"),
            FeatureLifecycle::Stable => write!(f, "stable"),
            FeatureLifecycle::Deprecated => write!(f, "deprecated"),
        }
    }
}

/// Feature flag configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeatureFlags {
    enabled: HashSet<Feature>,
}

impl Default for FeatureFlags {
    #[inline]
    fn default() -> Self {
        let mut flags = Self {
            enabled: HashSet::new(),
        };

        for feature in all_features() {
            if feature.default_enabled() {
                flags.enabled.insert(feature);
            }
        }

        flags
    }
}

impl FeatureFlags {
    /// Get the global feature flags instance.
    #[inline]
    #[must_use]
    pub fn global() -> &'static Self {
        GLOBAL_FLAGS.get_or_init(Self::from_env)
    }

    /// Set the global feature flags.
    #[inline]
    pub fn set_global(flags: Self) {
        let _ = GLOBAL_FLAGS.set(flags);
    }

    /// Create feature flags from environment variables.
    #[inline]
    #[must_use]
    pub fn from_env() -> Self {
        let mut flags = Self::default();

        for (key, value) in env::vars() {
            if let Some(feature_name) = key.strip_prefix("COPYBOOK_FF_")
                && let Ok(feature) = Feature::from_str(feature_name)
            {
                let enabled = matches!(
                    value.to_lowercase().as_str(),
                    "1" | "true" | "yes" | "on" | "enabled"
                );
                if enabled {
                    flags.enabled.insert(feature);
                } else {
                    flags.enabled.remove(&feature);
                }
            }
        }

        flags
    }

    /// Check whether a specific feature is currently enabled.
    #[inline]
    #[must_use]
    pub fn is_enabled(&self, feature: Feature) -> bool {
        self.enabled.contains(&feature)
    }

    /// Enable a feature flag.
    #[inline]
    pub fn enable(&mut self, feature: Feature) {
        self.enabled.insert(feature);
    }

    /// Disable a feature flag.
    #[inline]
    pub fn disable(&mut self, feature: Feature) {
        self.enabled.remove(&feature);
    }

    /// Toggle a feature flag between enabled and disabled.
    #[inline]
    pub fn toggle(&mut self, feature: Feature) {
        if self.enabled.contains(&feature) {
            self.enabled.remove(&feature);
        } else {
            self.enabled.insert(feature);
        }
    }

    /// Iterate over all currently enabled features.
    #[inline]
    pub fn enabled_features(&self) -> impl Iterator<Item = &Feature> {
        self.enabled.iter()
    }

    /// Return all enabled features belonging to a specific category.
    #[inline]
    #[must_use]
    pub fn enabled_in_category(&self, category: FeatureCategory) -> Vec<Feature> {
        self.enabled
            .iter()
            .filter(|f| f.category() == category)
            .copied()
            .collect()
    }

    /// Return all features (enabled or not) that belong to a category.
    #[inline]
    #[must_use]
    pub fn features_in_category(category: FeatureCategory) -> Vec<Feature> {
        all_features()
            .into_iter()
            .filter(|f| f.category() == category)
            .collect()
    }

    /// Create a new [`FeatureFlagsBuilder`] starting from defaults.
    #[inline]
    #[must_use]
    pub fn builder() -> FeatureFlagsBuilder {
        FeatureFlagsBuilder::default()
    }
}

/// Builder for constructing [`FeatureFlags`] with a fluent API.
#[derive(Debug, Clone, Default)]
pub struct FeatureFlagsBuilder {
    flags: FeatureFlags,
}

impl FeatureFlagsBuilder {
    /// Enable a single feature flag.
    #[inline]
    #[must_use]
    pub fn enable(mut self, feature: Feature) -> Self {
        self.flags.enable(feature);
        self
    }

    /// Disable a single feature flag.
    #[inline]
    #[must_use]
    pub fn disable(mut self, feature: Feature) -> Self {
        self.flags.disable(feature);
        self
    }

    /// Enable all features in a category.
    #[inline]
    #[must_use]
    pub fn enable_category(mut self, category: FeatureCategory) -> Self {
        for feature in FeatureFlags::features_in_category(category) {
            self.flags.enable(feature);
        }
        self
    }

    /// Disable all features in a category.
    #[inline]
    #[must_use]
    pub fn disable_category(mut self, category: FeatureCategory) -> Self {
        for feature in FeatureFlags::features_in_category(category) {
            self.flags.disable(feature);
        }
        self
    }

    /// Consume the builder and return the configured [`FeatureFlags`].
    #[inline]
    #[must_use]
    pub fn build(self) -> FeatureFlags {
        self.flags
    }
}

static GLOBAL_FLAGS: OnceLock<FeatureFlags> = OnceLock::new();

/// Thread-safe, mutable handle to [`FeatureFlags`] for runtime toggling.
#[derive(Debug)]
pub struct FeatureFlagsHandle {
    flags: RwLock<FeatureFlags>,
}

impl Default for FeatureFlagsHandle {
    #[inline]
    fn default() -> Self {
        Self {
            flags: RwLock::new(FeatureFlags::from_env()),
        }
    }
}

impl FeatureFlagsHandle {
    /// Create a new handle initialized from environment variables.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Check whether a feature is enabled in this handle.
    #[inline]
    #[must_use]
    pub fn is_enabled(&self, feature: Feature) -> bool {
        self.flags
            .read()
            .is_ok_and(|flags| flags.is_enabled(feature))
    }

    /// Enable a feature flag through the handle.
    #[inline]
    pub fn enable(&self, feature: Feature) {
        if let Ok(mut flags) = self.flags.write() {
            flags.enable(feature);
        }
    }

    /// Disable a feature flag through the handle.
    #[inline]
    pub fn disable(&self, feature: Feature) {
        if let Ok(mut flags) = self.flags.write() {
            flags.disable(feature);
        }
    }

    /// Toggle a feature flag through the handle.
    #[inline]
    pub fn toggle(&self, feature: Feature) {
        if let Ok(mut flags) = self.flags.write() {
            flags.toggle(feature);
        }
    }

    /// Take an immutable snapshot of the current feature flags.
    #[inline]
    #[must_use]
    pub fn snapshot(&self) -> FeatureFlags {
        self.flags
            .read()
            .map(|flags| flags.clone())
            .unwrap_or_default()
    }
}

impl Clone for FeatureFlagsHandle {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            flags: RwLock::new(self.snapshot()),
        }
    }
}

/// Return a list of every defined [`Feature`] variant.
#[inline]
#[must_use]
pub fn all_features() -> Vec<Feature> {
    vec![
        Feature::RenamesR4R6,
        Feature::AuditSystem,
        Feature::SoxCompliance,
        Feature::HipaaCompliance,
        Feature::GdprCompliance,
        Feature::PciDssCompliance,
        Feature::SecurityMonitoring,
        Feature::AdvancedOptimization,
        Feature::LruCache,
        Feature::ParallelDecode,
        Feature::ZeroCopy,
        Feature::VerboseLogging,
        Feature::DiagnosticOutput,
        Feature::Profiling,
        Feature::MemoryTracking,
    ]
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_feature_display() {
        assert_eq!(Feature::RenamesR4R6.to_string(), "renames_r4_r6");
        assert_eq!(Feature::LruCache.to_string(), "lru_cache");
    }

    #[test]
    fn test_feature_from_str() {
        assert_eq!(
            Feature::from_str("renames_r4_r6").unwrap(),
            Feature::RenamesR4R6
        );
        assert_eq!(Feature::from_str("LRU_CACHE").unwrap(), Feature::LruCache);
        assert!(Feature::from_str("unknown_feature").is_err());
    }

    #[test]
    fn test_removed_phase_c_flags_are_unknown() {
        // #656 Phase C (v0.6.0): SIGN SEPARATE, COMP-1, and COMP-2 are stable
        // parser behavior, not runtime flags. Their old names must no longer
        // parse so stale configs fail loudly instead of silently changing
        // meaning.
        for removed in [
            "sign_separate",
            "comp_1",
            "comp_2",
            "SIGN_SEPARATE",
            "COMP_1",
            "COMP_2",
        ] {
            assert!(
                Feature::from_str(removed).is_err(),
                "removed flag '{removed}' should not parse"
            );
        }
    }

    #[test]
    fn test_feature_category() {
        assert_eq!(
            Feature::RenamesR4R6.category(),
            FeatureCategory::Experimental
        );
        assert_eq!(Feature::AuditSystem.category(), FeatureCategory::Enterprise);
        assert_eq!(Feature::LruCache.category(), FeatureCategory::Performance);
        assert_eq!(Feature::VerboseLogging.category(), FeatureCategory::Debug);
    }

    #[test]
    fn test_default_enabled() {
        assert!(Feature::LruCache.default_enabled());
        assert!(!Feature::RenamesR4R6.default_enabled());
        assert!(!Feature::VerboseLogging.default_enabled());
    }

    #[test]
    fn lifecycle_reports_experimental_for_all_remaining_flags() {
        // #656 Phase C: stable COBOL language behavior (SIGN SEPARATE,
        // COMP-1, COMP-2) is ordinary parser behavior, not a flag, so no
        // remaining flag reports a Stable lifecycle. lifecycle() stays the
        // authoritative stability signal, orthogonal to the toggle-group
        // category().
        for feat in all_features() {
            assert_eq!(
                feat.lifecycle(),
                FeatureLifecycle::Experimental,
                "{feat} should report an Experimental lifecycle"
            );
        }

        // category() stays a toggle group, orthogonal to the stability class.
        assert_eq!(
            Feature::RenamesR4R6.category(),
            FeatureCategory::Experimental
        );
    }

    #[test]
    fn test_feature_flags_default() {
        let flags = FeatureFlags::default();
        assert!(flags.is_enabled(Feature::LruCache));
        assert!(!flags.is_enabled(Feature::RenamesR4R6));
    }

    #[test]
    fn test_feature_flags_enable_disable() {
        let mut flags = FeatureFlags::default();
        flags.enable(Feature::RenamesR4R6);
        assert!(flags.is_enabled(Feature::RenamesR4R6));
        flags.disable(Feature::RenamesR4R6);
        assert!(!flags.is_enabled(Feature::RenamesR4R6));
    }

    #[test]
    fn test_feature_flags_toggle() {
        let mut flags = FeatureFlags::default();
        flags.toggle(Feature::LruCache);
        assert!(!flags.is_enabled(Feature::LruCache));
        flags.toggle(Feature::LruCache);
        assert!(flags.is_enabled(Feature::LruCache));
    }

    #[test]
    fn test_feature_flags_builder() {
        let flags = FeatureFlags::builder()
            .enable(Feature::RenamesR4R6)
            .disable(Feature::LruCache)
            .build();
        assert!(flags.is_enabled(Feature::RenamesR4R6));
        assert!(!flags.is_enabled(Feature::LruCache));
    }

    #[test]
    fn test_feature_flags_enable_category() {
        // #656 Phase C: RenamesR4R6 is the only remaining Experimental toggle.
        let flags = FeatureFlags::builder()
            .enable_category(FeatureCategory::Experimental)
            .build();
        assert!(flags.is_enabled(Feature::RenamesR4R6));
        assert_eq!(
            flags.enabled_in_category(FeatureCategory::Experimental),
            vec![Feature::RenamesR4R6]
        );
    }

    #[test]
    fn test_feature_flags_handle() {
        let handle = FeatureFlagsHandle::new();
        handle.enable(Feature::RenamesR4R6);
        assert!(handle.is_enabled(Feature::RenamesR4R6));
        handle.disable(Feature::RenamesR4R6);
        assert!(!handle.is_enabled(Feature::RenamesR4R6));
        handle.enable(Feature::RenamesR4R6);
        assert!(handle.is_enabled(Feature::RenamesR4R6));
    }

    #[test]
    fn test_all_features() {
        let features = all_features();
        // #656 Phase C: 18 -> 15 flags (SignSeparate/Comp1/Comp2 removed).
        assert_eq!(features.len(), 15);
        assert!(features.contains(&Feature::RenamesR4R6));
        assert!(features.contains(&Feature::LruCache));
        assert!(features.contains(&Feature::VerboseLogging));
    }

    #[test]
    fn test_enabled_in_category() {
        let mut flags = FeatureFlags::default();
        flags.enable(Feature::RenamesR4R6);
        let experimental = flags.enabled_in_category(FeatureCategory::Experimental);
        assert_eq!(experimental, vec![Feature::RenamesR4R6]);
    }

    #[test]
    fn test_env_var_name() {
        assert_eq!(
            Feature::RenamesR4R6.env_var_name(),
            "COPYBOOK_FF_RENAMES_R4_R6"
        );
        assert_eq!(Feature::LruCache.env_var_name(), "COPYBOOK_FF_LRU_CACHE");
    }

    #[test]
    fn test_feature_serde_json_roundtrip() {
        let feature = Feature::ParallelDecode;
        let json = serde_json::to_string(&feature).unwrap();
        let back: Feature = serde_json::from_str(&json).unwrap();
        assert_eq!(back, feature);
    }

    #[test]
    fn test_feature_category_serde_roundtrip() {
        let cat = FeatureCategory::Enterprise;
        let json = serde_json::to_string(&cat).unwrap();
        let back: FeatureCategory = serde_json::from_str(&json).unwrap();
        assert_eq!(back, cat);
    }

    #[test]
    fn test_feature_lifecycle_display() {
        assert_eq!(FeatureLifecycle::Experimental.to_string(), "experimental");
        assert_eq!(FeatureLifecycle::Stable.to_string(), "stable");
        assert_eq!(FeatureLifecycle::Deprecated.to_string(), "deprecated");
    }

    #[test]
    fn test_feature_category_display() {
        assert_eq!(FeatureCategory::Experimental.to_string(), "experimental");
        assert_eq!(FeatureCategory::Enterprise.to_string(), "enterprise");
        assert_eq!(FeatureCategory::Performance.to_string(), "performance");
        assert_eq!(FeatureCategory::Debug.to_string(), "debug");
    }

    #[test]
    fn test_feature_flags_all_disabled() {
        let mut flags = FeatureFlags::default();
        for feature in all_features() {
            flags.disable(feature);
        }
        for feature in all_features() {
            assert!(!flags.is_enabled(feature), "{feature} should be disabled");
        }
    }

    #[test]
    fn test_feature_flags_all_enabled() {
        let mut flags = FeatureFlags::default();
        for feature in all_features() {
            flags.enable(feature);
        }
        for feature in all_features() {
            assert!(flags.is_enabled(feature), "{feature} should be enabled");
        }
    }

    #[test]
    fn test_builder_disable_category() {
        let flags = FeatureFlags::builder()
            .enable_category(FeatureCategory::Debug)
            .disable_category(FeatureCategory::Debug)
            .build();
        let debug_features = flags.enabled_in_category(FeatureCategory::Debug);
        assert!(debug_features.is_empty());
    }

    #[test]
    fn test_handle_toggle_and_snapshot() {
        let handle = FeatureFlagsHandle::new();
        let initially_enabled = handle.is_enabled(Feature::LruCache);
        handle.toggle(Feature::LruCache);
        assert_ne!(handle.is_enabled(Feature::LruCache), initially_enabled);
        let snap = handle.snapshot();
        assert_ne!(snap.is_enabled(Feature::LruCache), initially_enabled);
    }

    #[test]
    fn test_handle_clone_is_independent() {
        let handle = FeatureFlagsHandle::new();
        handle.enable(Feature::Profiling);
        let cloned = handle.clone();
        handle.disable(Feature::Profiling);
        assert!(cloned.is_enabled(Feature::Profiling));
        assert!(!handle.is_enabled(Feature::Profiling));
    }

    #[test]
    fn test_features_in_category_counts() {
        // #656 Phase C: Experimental holds only RenamesR4R6 (15 flags total).
        let experimental = FeatureFlags::features_in_category(FeatureCategory::Experimental);
        assert_eq!(experimental, vec![Feature::RenamesR4R6]);
        let enterprise = FeatureFlags::features_in_category(FeatureCategory::Enterprise);
        assert_eq!(enterprise.len(), 6);
        let performance = FeatureFlags::features_in_category(FeatureCategory::Performance);
        assert_eq!(performance.len(), 4);
        let debug = FeatureFlags::features_in_category(FeatureCategory::Debug);
        assert_eq!(debug.len(), 4);
    }

    #[test]
    fn test_enabled_features_iterator_count() {
        let flags = FeatureFlags::default();
        let count = flags.enabled_features().count();
        // Default-enabled: LruCache only (stable language behavior is not flagged).
        assert_eq!(count, 1);
    }

    #[test]
    fn test_description_nonempty_for_all_features() {
        for feature in all_features() {
            assert!(
                !feature.description().is_empty(),
                "{feature} has empty description"
            );
        }
    }

    #[test]
    fn test_from_str_unknown_returns_err() {
        assert!(Feature::from_str("does_not_exist").is_err());
        assert!(Feature::from_str("").is_err());
    }

    #[test]
    fn test_feature_flags_serde_json_roundtrip() {
        let flags = FeatureFlags::builder()
            .enable(Feature::Profiling)
            .disable(Feature::LruCache)
            .build();
        let json = serde_json::to_string(&flags).unwrap();
        let back: FeatureFlags = serde_json::from_str(&json).unwrap();
        assert!(back.is_enabled(Feature::Profiling));
        assert!(!back.is_enabled(Feature::LruCache));
    }
}
