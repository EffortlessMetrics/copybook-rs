//! Feature flag system for copybook-rs
//!
//! This module provides a centralized feature flag system that allows runtime
//! control over experimental features, enterprise features, performance optimizations,
//! debug capabilities, and testing hooks.
//!
//! Feature flags can be configured via:
//! - Environment variables (prefixed with `COPYBOOK_FF_`)
//! - Programmatic configuration
//! - Configuration files (TOML/JSON)
//!
//! # Example
//!
//! ```rust
//! use copybook_core::feature_flags::{FeatureFlags, Feature};
//!
//! // Get global feature flags
//! let flags = FeatureFlags::global();
//!
//! // Check if a feature is enabled
//! if flags.is_enabled(Feature::SignSeparate) {
//!     // Use SIGN SEPARATE clause support
//! }
//!
//! // Enable a feature programmatically
//! let mut flags = FeatureFlags::default();
//! flags.enable(Feature::VerboseLogging);
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::env;
use std::fmt;
use std::str::FromStr;
use std::sync::{OnceLock, RwLock};

/// All available feature flags
///
/// Features are organized into categories:
/// - Experimental: Early-stage features under active development
/// - Enterprise: Business-critical features for enterprise deployments
/// - Performance: Optimization modes and caching strategies
/// - Debug: Diagnostic and profiling capabilities
/// - Testing: Hooks for mutation testing and fuzzing
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum Feature {
    // ========== Experimental Features ==========
    /// Enable SIGN SEPARATE clause support (experimental)
    #[serde(alias = "sign_separate")]
    SignSeparate,

    /// Enable RENAMES R4-R6 advanced scenarios
    #[serde(alias = "renames_r4_r6")]
    RenamesR4R6,

    /// Enable COMP-1 (single precision floating point) support
    #[serde(alias = "comp_1")]
    Comp1,

    /// Enable COMP-2 (double precision floating point) support
    #[serde(alias = "comp_2")]
    Comp2,

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

    // ========== Testing Features ==========
    /// Enable mutation testing hooks
    #[serde(alias = "mutation_testing")]
    MutationTesting,

    /// Enable fuzzing integration points
    #[serde(alias = "fuzzing_integration")]
    FuzzingIntegration,

    /// Enable test coverage instrumentation
    #[serde(alias = "coverage_instrumentation")]
    CoverageInstrumentation,

    /// Enable property-based testing integration
    #[serde(alias = "property_based_testing")]
    PropertyBasedTesting,
}

impl Feature {
    /// Get the category this feature belongs to
    pub fn category(&self) -> FeatureCategory {
        match self {
            // Experimental
            Feature::SignSeparate | Feature::RenamesR4R6 | Feature::Comp1 | Feature::Comp2 => {
                FeatureCategory::Experimental
            }
            // Enterprise
            Feature::AuditSystem
            | Feature::SoxCompliance
            | Feature::HipaaCompliance
            | Feature::GdprCompliance
            | Feature::PciDssCompliance
            | Feature::SecurityMonitoring => FeatureCategory::Enterprise,
            // Performance
            Feature::AdvancedOptimization
            | Feature::LruCache
            | Feature::ParallelDecode
            | Feature::ZeroCopy => FeatureCategory::Performance,
            // Debug
            Feature::VerboseLogging
            | Feature::DiagnosticOutput
            | Feature::Profiling
            | Feature::MemoryTracking => FeatureCategory::Debug,
            // Testing
            Feature::MutationTesting
            | Feature::FuzzingIntegration
            | Feature::CoverageInstrumentation
            | Feature::PropertyBasedTesting => FeatureCategory::Testing,
        }
    }

    /// Get the default enabled state for this feature
    pub fn default_enabled(&self) -> bool {
        match self {
            // Promoted to stable: always enabled by default
            Feature::SignSeparate | Feature::Comp1 | Feature::Comp2 => true,
            // Experimental features are disabled by default
            Feature::RenamesR4R6 => false,
            // Enterprise features are disabled by default
            Feature::AuditSystem
            | Feature::SoxCompliance
            | Feature::HipaaCompliance
            | Feature::GdprCompliance
            | Feature::PciDssCompliance
            | Feature::SecurityMonitoring => false,
            // Performance features: some enabled by default
            Feature::LruCache => true,
            Feature::AdvancedOptimization | Feature::ParallelDecode | Feature::ZeroCopy => false,
            // Debug features are disabled by default
            Feature::VerboseLogging
            | Feature::DiagnosticOutput
            | Feature::Profiling
            | Feature::MemoryTracking => false,
            // Testing features are disabled by default
            Feature::MutationTesting
            | Feature::FuzzingIntegration
            | Feature::CoverageInstrumentation
            | Feature::PropertyBasedTesting => false,
        }
    }

    /// Get the environment variable name for this feature
    pub fn env_var_name(&self) -> String {
        format!("COPYBOOK_FF_{}", self.to_string().to_uppercase())
    }

    /// Get a human-readable description of this feature
    pub fn description(&self) -> &'static str {
        match self {
            Feature::SignSeparate => "Enable SIGN SEPARATE clause support",
            Feature::RenamesR4R6 => "Enable RENAMES R4-R6 advanced scenarios",
            Feature::Comp1 => "Enable COMP-1 (single precision floating point) support",
            Feature::Comp2 => "Enable COMP-2 (double precision floating point) support",
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
            Feature::MutationTesting => "Enable mutation testing hooks",
            Feature::FuzzingIntegration => "Enable fuzzing integration points",
            Feature::CoverageInstrumentation => "Enable test coverage instrumentation",
            Feature::PropertyBasedTesting => "Enable property-based testing integration",
        }
    }
}

impl fmt::Display for Feature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Feature::SignSeparate => "sign_separate",
            Feature::RenamesR4R6 => "renames_r4_r6",
            Feature::Comp1 => "comp_1",
            Feature::Comp2 => "comp_2",
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
            Feature::MutationTesting => "mutation_testing",
            Feature::FuzzingIntegration => "fuzzing_integration",
            Feature::CoverageInstrumentation => "coverage_instrumentation",
            Feature::PropertyBasedTesting => "property_based_testing",
        };
        write!(f, "{s}")
    }
}

impl FromStr for Feature {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "sign_separate" => Ok(Self::SignSeparate),
            "renames_r4_r6" => Ok(Self::RenamesR4R6),
            "comp_1" => Ok(Self::Comp1),
            "comp_2" => Ok(Self::Comp2),
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
            "mutation_testing" => Ok(Self::MutationTesting),
            "fuzzing_integration" => Ok(Self::FuzzingIntegration),
            "coverage_instrumentation" => Ok(Self::CoverageInstrumentation),
            "property_based_testing" => Ok(Self::PropertyBasedTesting),
            _ => Err(format!("Unknown feature flag: '{s}'")),
        }
    }
}

/// Feature category for grouping related features
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FeatureCategory {
    /// Experimental features under active development
    Experimental,
    /// Enterprise features for business-critical deployments
    Enterprise,
    /// Performance optimization features
    Performance,
    /// Debug and diagnostic features
    Debug,
    /// Testing and validation features
    Testing,
}

impl fmt::Display for FeatureCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeatureCategory::Experimental => write!(f, "experimental"),
            FeatureCategory::Enterprise => write!(f, "enterprise"),
            FeatureCategory::Performance => write!(f, "performance"),
            FeatureCategory::Debug => write!(f, "debug"),
            FeatureCategory::Testing => write!(f, "testing"),
        }
    }
}

/// Feature lifecycle stage
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FeatureLifecycle {
    /// Feature is experimental and may change
    Experimental,
    /// Feature is stable and production-ready
    Stable,
    /// Feature is deprecated and will be removed
    Deprecated,
}

impl fmt::Display for FeatureLifecycle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FeatureLifecycle::Experimental => write!(f, "experimental"),
            FeatureLifecycle::Stable => write!(f, "stable"),
            FeatureLifecycle::Deprecated => write!(f, "deprecated"),
        }
    }
}

/// Feature flag configuration
///
/// This structure holds the state of all feature flags and provides
/// methods to query and modify them.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeatureFlags {
    enabled: HashSet<Feature>,
}

impl Default for FeatureFlags {
    fn default() -> Self {
        let mut flags = Self {
            enabled: HashSet::new(),
        };

        // Enable features that are enabled by default
        for feature in all_features() {
            if feature.default_enabled() {
                flags.enabled.insert(feature);
            }
        }

        flags
    }
}

impl FeatureFlags {
    /// Get the global feature flags instance
    ///
    /// This returns a reference to the global feature flags that are
    /// initialized from environment variables.
    pub fn global() -> &'static Self {
        GLOBAL_FLAGS.get_or_init(Self::from_env)
    }

    /// Set the global feature flags
    ///
    /// This allows CLI options to override the default feature flags.
    /// Should be called early in the program before any parsing operations.
    pub fn set_global(flags: Self) {
        let _ = GLOBAL_FLAGS.set(flags);
    }

    /// Create feature flags from environment variables
    ///
    /// Environment variables are read with the prefix `COPYBOOK_FF_`.
    /// For example, `COPYBOOK_FF_VERBOSE_LOGGING=1` enables verbose logging.
    pub fn from_env() -> Self {
        let mut flags = Self::default();

        // Read environment variables
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

    /// Check if a feature is enabled
    pub fn is_enabled(&self, feature: Feature) -> bool {
        self.enabled.contains(&feature)
    }

    /// Enable a feature
    pub fn enable(&mut self, feature: Feature) {
        self.enabled.insert(feature);
    }

    /// Disable a feature
    pub fn disable(&mut self, feature: Feature) {
        self.enabled.remove(&feature);
    }

    /// Toggle a feature
    pub fn toggle(&mut self, feature: Feature) {
        if self.enabled.contains(&feature) {
            self.enabled.remove(&feature);
        } else {
            self.enabled.insert(feature);
        }
    }

    /// Get all enabled features
    pub fn enabled_features(&self) -> impl Iterator<Item = &Feature> {
        self.enabled.iter()
    }

    /// Get all enabled features in a specific category
    pub fn enabled_in_category(&self, category: FeatureCategory) -> Vec<Feature> {
        self.enabled
            .iter()
            .filter(|f| f.category() == category)
            .copied()
            .collect()
    }

    /// Get all features in a specific category
    pub fn features_in_category(category: FeatureCategory) -> Vec<Feature> {
        all_features()
            .into_iter()
            .filter(|f| f.category() == category)
            .collect()
    }

    /// Create a builder for feature flags
    pub fn builder() -> FeatureFlagsBuilder {
        FeatureFlagsBuilder::default()
    }
}

/// Builder for feature flags
///
/// Provides a fluent API for constructing feature flags.
#[derive(Debug, Clone, Default)]
pub struct FeatureFlagsBuilder {
    flags: FeatureFlags,
}

impl FeatureFlagsBuilder {
    /// Enable a feature
    #[must_use]
    pub fn enable(mut self, feature: Feature) -> Self {
        self.flags.enable(feature);
        self
    }

    /// Disable a feature
    #[must_use]
    pub fn disable(mut self, feature: Feature) -> Self {
        self.flags.disable(feature);
        self
    }

    /// Enable all features in a category
    #[must_use]
    pub fn enable_category(mut self, category: FeatureCategory) -> Self {
        for feature in FeatureFlags::features_in_category(category) {
            self.flags.enable(feature);
        }
        self
    }

    /// Disable all features in a category
    #[must_use]
    pub fn disable_category(mut self, category: FeatureCategory) -> Self {
        for feature in FeatureFlags::features_in_category(category) {
            self.flags.disable(feature);
        }
        self
    }

    /// Build the feature flags
    pub fn build(self) -> FeatureFlags {
        self.flags
    }
}

/// Global feature flags instance
static GLOBAL_FLAGS: OnceLock<FeatureFlags> = OnceLock::new();

/// Thread-safe feature flags with runtime modification support
///
/// This type provides interior mutability for feature flags, allowing
/// them to be modified at runtime while still being thread-safe.
#[derive(Debug)]
pub struct FeatureFlagsHandle {
    flags: RwLock<FeatureFlags>,
}

impl Default for FeatureFlagsHandle {
    fn default() -> Self {
        Self {
            flags: RwLock::new(FeatureFlags::from_env()),
        }
    }
}

impl FeatureFlagsHandle {
    /// Create a new feature flags handle
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if a feature is enabled
    pub fn is_enabled(&self, feature: Feature) -> bool {
        self.flags
            .read()
            .map(|flags| flags.is_enabled(feature))
            .unwrap_or(false)
    }

    /// Enable a feature
    pub fn enable(&self, feature: Feature) {
        if let Ok(mut flags) = self.flags.write() {
            flags.enable(feature);
        }
    }

    /// Disable a feature
    pub fn disable(&self, feature: Feature) {
        if let Ok(mut flags) = self.flags.write() {
            flags.disable(feature);
        }
    }

    /// Toggle a feature
    pub fn toggle(&self, feature: Feature) {
        if let Ok(mut flags) = self.flags.write() {
            flags.toggle(feature);
        }
    }

    /// Get a snapshot of the current feature flags
    pub fn snapshot(&self) -> FeatureFlags {
        self.flags
            .read()
            .map(|flags| flags.clone())
            .unwrap_or_default()
    }
}

impl Clone for FeatureFlagsHandle {
    fn clone(&self) -> Self {
        Self {
            flags: RwLock::new(self.snapshot()),
        }
    }
}

/// Get all available features
pub fn all_features() -> Vec<Feature> {
    vec![
        // Experimental
        Feature::SignSeparate,
        Feature::RenamesR4R6,
        Feature::Comp1,
        Feature::Comp2,
        // Enterprise
        Feature::AuditSystem,
        Feature::SoxCompliance,
        Feature::HipaaCompliance,
        Feature::GdprCompliance,
        Feature::PciDssCompliance,
        Feature::SecurityMonitoring,
        // Performance
        Feature::AdvancedOptimization,
        Feature::LruCache,
        Feature::ParallelDecode,
        Feature::ZeroCopy,
        // Debug
        Feature::VerboseLogging,
        Feature::DiagnosticOutput,
        Feature::Profiling,
        Feature::MemoryTracking,
        // Testing
        Feature::MutationTesting,
        Feature::FuzzingIntegration,
        Feature::CoverageInstrumentation,
        Feature::PropertyBasedTesting,
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feature_display() {
        assert_eq!(Feature::SignSeparate.to_string(), "sign_separate");
        assert_eq!(Feature::LruCache.to_string(), "lru_cache");
    }

    #[test]
    fn test_feature_from_str() {
        assert_eq!(
            Feature::from_str("sign_separate").unwrap(),
            Feature::SignSeparate
        );
        assert_eq!(Feature::from_str("LRU_CACHE").unwrap(), Feature::LruCache);
        assert!(Feature::from_str("unknown_feature").is_err());
    }

    #[test]
    fn test_feature_category() {
        assert_eq!(
            Feature::SignSeparate.category(),
            FeatureCategory::Experimental
        );
        assert_eq!(Feature::AuditSystem.category(), FeatureCategory::Enterprise);
        assert_eq!(Feature::LruCache.category(), FeatureCategory::Performance);
        assert_eq!(Feature::VerboseLogging.category(), FeatureCategory::Debug);
        assert_eq!(
            Feature::MutationTesting.category(),
            FeatureCategory::Testing
        );
    }

    #[test]
    fn test_default_enabled() {
        // Promoted to stable: enabled by default
        assert!(Feature::SignSeparate.default_enabled());
        assert!(Feature::Comp1.default_enabled());
        assert!(Feature::Comp2.default_enabled());
        assert!(Feature::LruCache.default_enabled());
        assert!(!Feature::VerboseLogging.default_enabled());
    }

    #[test]
    fn test_feature_flags_default() {
        let flags = FeatureFlags::default();
        assert!(flags.is_enabled(Feature::LruCache));
        // Promoted to stable: now enabled by default
        assert!(flags.is_enabled(Feature::SignSeparate));
        assert!(flags.is_enabled(Feature::Comp1));
        assert!(flags.is_enabled(Feature::Comp2));
    }

    #[test]
    fn test_feature_flags_enable_disable() {
        let mut flags = FeatureFlags::default();
        flags.enable(Feature::SignSeparate);
        assert!(flags.is_enabled(Feature::SignSeparate));
        flags.disable(Feature::SignSeparate);
        assert!(!flags.is_enabled(Feature::SignSeparate));
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
            .enable(Feature::SignSeparate)
            .disable(Feature::LruCache)
            .build();
        assert!(flags.is_enabled(Feature::SignSeparate));
        assert!(!flags.is_enabled(Feature::LruCache));
    }

    #[test]
    fn test_feature_flags_enable_category() {
        let flags = FeatureFlags::builder()
            .enable_category(FeatureCategory::Experimental)
            .build();
        assert!(flags.is_enabled(Feature::SignSeparate));
        assert!(flags.is_enabled(Feature::RenamesR4R6));
        assert!(flags.is_enabled(Feature::Comp1));
        assert!(flags.is_enabled(Feature::Comp2));
    }

    #[test]
    fn test_feature_flags_handle() {
        let handle = FeatureFlagsHandle::new();
        // SignSeparate is now enabled by default (promoted to stable)
        assert!(handle.is_enabled(Feature::SignSeparate));
        handle.disable(Feature::SignSeparate);
        assert!(!handle.is_enabled(Feature::SignSeparate));
        handle.enable(Feature::SignSeparate);
        assert!(handle.is_enabled(Feature::SignSeparate));
    }

    #[test]
    fn test_all_features() {
        let features = all_features();
        assert!(features.contains(&Feature::SignSeparate));
        assert!(features.contains(&Feature::LruCache));
        assert!(features.contains(&Feature::VerboseLogging));
    }

    #[test]
    fn test_enabled_in_category() {
        let mut flags = FeatureFlags::default();
        // SignSeparate, Comp1, Comp2 are enabled by default; enable RenamesR4R6 too
        flags.enable(Feature::RenamesR4R6);
        let experimental = flags.enabled_in_category(FeatureCategory::Experimental);
        assert_eq!(experimental.len(), 4);
        assert!(experimental.contains(&Feature::SignSeparate));
        assert!(experimental.contains(&Feature::Comp1));
        assert!(experimental.contains(&Feature::Comp2));
        assert!(experimental.contains(&Feature::RenamesR4R6));
    }

    #[test]
    fn test_env_var_name() {
        assert_eq!(
            Feature::SignSeparate.env_var_name(),
            "COPYBOOK_FF_SIGN_SEPARATE"
        );
        assert_eq!(Feature::LruCache.env_var_name(), "COPYBOOK_FF_LRU_CACHE");
    }
}
