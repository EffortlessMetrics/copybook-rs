// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI configuration parsing helpers.
//!
//! This module owns runtime feature-flag and dialect preference handling so the
//! binary entrypoint can focus on process lifecycle and command dispatch.

use anyhow::anyhow;
use clap::{Args, ValueEnum};
use copybook_charset::{Codepage, UnmappablePolicy};
use copybook_core::{Feature, FeatureCategory, FeatureFlags};
use std::path::PathBuf;
use std::str::FromStr;

/// Parse a codepage for CLI arguments while keeping validation out of library
/// crates and their public dependency graph.
pub(crate) fn parse_codepage(input: &str) -> Result<Codepage, String> {
    Codepage::parse(input).map_err(|error| {
        let expected = Codepage::variants()
            .iter()
            .map(|codepage| codepage.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        format!("{error}; expected one of: {expected}")
    })
}

/// Parse an unmappable-character policy for CLI arguments.
pub(crate) fn parse_unmappable_policy(input: &str) -> Result<UnmappablePolicy, String> {
    UnmappablePolicy::parse(input).map_err(|error| error.to_string())
}

/// Feature flag options for the CLI
///
/// These options allow runtime control over experimental features,
/// enterprise features, performance optimizations, debug capabilities,
/// and testing hooks.
#[derive(Args, Debug, Clone)]
pub(crate) struct FeatureFlagOpts {
    /// Enable specific feature flags (comma-separated)
    ///
    /// Available flags:
    /// - Experimental: `sign_separate`, `renames_r4_r6`, `comp_1`, `comp_2`
    /// - Enterprise: `audit_system`, `sox_compliance`, `hipaa_compliance`, `gdpr_compliance`, `pci_dss_compliance`, `security_monitoring`
    /// - Performance: `advanced_optimization`, `lru_cache`, `parallel_decode`, `zero_copy`
    /// - Debug: `verbose_logging`, `diagnostic_output`, `profiling`, `memory_tracking`
    /// - Testing: `mutation_testing`, `fuzzing_integration`, `coverage_instrumentation`, `property_based_testing`
    ///
    /// Example: --enable-features `sign_separate,verbose_logging`
    #[arg(long, value_delimiter = ',', value_name = "FEATURE")]
    pub enable_features: Vec<String>,

    /// Disable specific feature flags (comma-separated)
    ///
    /// This takes precedence over --enable-features and environment variables.
    ///
    /// Example: --disable-features `lru_cache`
    #[arg(long, value_delimiter = ',', value_name = "FEATURE")]
    pub disable_features: Vec<String>,

    /// Enable all features in a category
    ///
    /// Available categories: `experimental`, `enterprise`, `performance`, `debug`, `testing`
    ///
    /// Example: --enable-category `debug`
    #[arg(long, value_name = "CATEGORY")]
    pub enable_category: Vec<String>,

    /// Disable all features in a category
    ///
    /// Example: --disable-category `experimental`
    #[arg(long, value_name = "CATEGORY")]
    pub disable_category: Vec<String>,

    /// Load feature flags from a configuration file
    ///
    /// The file can be in TOML or JSON format.
    /// TOML format:
    /// ```toml
    /// [feature_flags]
    /// enabled = ["sign_separate", "verbose_logging"]
    /// disabled = ["lru_cache"]
    /// ```
    ///
    /// JSON format:
    /// ```json
    /// {
    ///   "feature_flags": {
    ///     "enabled": ["sign_separate", "verbose_logging"],
    ///     "disabled": ["lru_cache"]
    ///   }
    /// }
    /// ```
    #[arg(long, value_name = "PATH")]
    pub feature_flags_config: Option<PathBuf>,

    /// List all available feature flags and their status
    #[arg(long)]
    pub list_features: bool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
pub(crate) enum DialectPreference {
    /// Normative dialect - `min_count` is strictly enforced
    #[value(name = "n")]
    N,
    /// Zero-tolerant dialect - `min_count` is ignored
    #[value(name = "0")]
    Zero,
    /// One-tolerant dialect - `min_count` is clamped to 1
    #[value(name = "1")]
    One,
}

impl From<DialectPreference> for copybook_core::dialect::Dialect {
    #[inline]
    fn from(value: DialectPreference) -> Self {
        match value {
            DialectPreference::N => Self::Normative,
            DialectPreference::Zero => Self::ZeroTolerant,
            DialectPreference::One => Self::OneTolerant,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
pub(crate) enum ZonedEncodingPreference {
    /// Prefer default zero policy based on target code page.
    #[value(alias = "preferred-zero")]
    Preferred,
    /// Force ASCII zoned encoding format.
    Ascii,
    /// Force EBCDIC zoned encoding format.
    Ebcdic,
    /// Defer to automatic detection when metadata supplies a format.
    Auto,
}

impl From<ZonedEncodingPreference> for copybook_codec::ZonedEncodingFormat {
    #[inline]
    fn from(value: ZonedEncodingPreference) -> Self {
        match value {
            ZonedEncodingPreference::Preferred | ZonedEncodingPreference::Auto => Self::Auto,
            ZonedEncodingPreference::Ascii => Self::Ascii,
            ZonedEncodingPreference::Ebcdic => Self::Ebcdic,
        }
    }
}

/// Initialize feature flags from CLI options and environment variables
#[allow(clippy::too_many_lines)]
pub(crate) fn initialize_feature_flags(opts: &FeatureFlagOpts) -> anyhow::Result<FeatureFlags> {
    use std::fs;
    use std::io::Read;

    // Start with defaults from environment
    let mut flags = FeatureFlags::from_env();

    // Load from config file if specified
    if let Some(config_path) = &opts.feature_flags_config {
        let mut content = String::new();
        let mut file = fs::File::open(config_path)
            .map_err(|e| anyhow!("Failed to open feature flags config: {e}"))?;
        file.read_to_string(&mut content)
            .map_err(|e| anyhow!("Failed to read feature flags config: {e}"))?;

        // Try JSON format first
        if let Ok(json_config) = serde_json::from_str::<serde_json::Value>(&content) {
            apply_json_feature_flags(&mut flags, &json_config);
        } else if let Ok(toml_str) = content.parse::<toml::Value>() {
            apply_toml_feature_flags(&mut flags, &toml_str);
        } else {
            return Err(anyhow!(
                "Failed to parse feature flags config: expected JSON or TOML format"
            ));
        }
    }

    apply_category_flags(&mut flags, &opts.enable_category, FeatureFlagAction::Enable)?;
    apply_category_flags(
        &mut flags,
        &opts.disable_category,
        FeatureFlagAction::Disable,
    )?;
    apply_named_flags(&mut flags, &opts.enable_features, FeatureFlagAction::Enable)?;
    apply_named_flags(
        &mut flags,
        &opts.disable_features,
        FeatureFlagAction::Disable,
    )?;

    Ok(flags)
}

fn apply_json_feature_flags(flags: &mut FeatureFlags, config: &serde_json::Value) {
    if let Some(feature_flags) = config.get("feature_flags") {
        if let Some(enabled) = feature_flags.get("enabled").and_then(|v| v.as_array()) {
            for feature_name in enabled {
                if let Some(name) = feature_name.as_str()
                    && let Ok(feature) = Feature::from_str(name)
                {
                    flags.enable(feature);
                }
            }
        }
        if let Some(disabled) = feature_flags.get("disabled").and_then(|v| v.as_array()) {
            for feature_name in disabled {
                if let Some(name) = feature_name.as_str()
                    && let Ok(feature) = Feature::from_str(name)
                {
                    flags.disable(feature);
                }
            }
        }
    }
}

fn apply_toml_feature_flags(flags: &mut FeatureFlags, config: &toml::Value) {
    if let Some(feature_flags) = config.get("feature_flags") {
        if let Some(enabled) = feature_flags.get("enabled").and_then(|v| v.as_array()) {
            for feature_name in enabled {
                if let Some(name) = feature_name.as_str()
                    && let Ok(feature) = Feature::from_str(name)
                {
                    flags.enable(feature);
                }
            }
        }
        if let Some(disabled) = feature_flags.get("disabled").and_then(|v| v.as_array()) {
            for feature_name in disabled {
                if let Some(name) = feature_name.as_str()
                    && let Ok(feature) = Feature::from_str(name)
                {
                    flags.disable(feature);
                }
            }
        }
    }
}

#[derive(Copy, Clone)]
enum FeatureFlagAction {
    Enable,
    Disable,
}

impl FeatureFlagAction {
    fn apply(self, flags: &mut FeatureFlags, feature: Feature) {
        match self {
            Self::Enable => flags.enable(feature),
            Self::Disable => flags.disable(feature),
        }
    }
}

fn apply_category_flags(
    flags: &mut FeatureFlags,
    category_names: &[String],
    action: FeatureFlagAction,
) -> anyhow::Result<()> {
    for category_name in category_names {
        let category = parse_feature_category(category_name)?;
        for feature in FeatureFlags::features_in_category(category) {
            action.apply(flags, feature);
        }
    }
    Ok(())
}

fn parse_feature_category(category_name: &str) -> anyhow::Result<FeatureCategory> {
    match category_name.to_lowercase().as_str() {
        "experimental" => Ok(FeatureCategory::Experimental),
        "enterprise" => Ok(FeatureCategory::Enterprise),
        "performance" => Ok(FeatureCategory::Performance),
        "debug" => Ok(FeatureCategory::Debug),
        "testing" => Ok(FeatureCategory::Testing),
        _ => Err(anyhow!(
            "Invalid feature category '{category_name}'. Valid categories: experimental, enterprise, performance, debug, testing"
        )),
    }
}

fn apply_named_flags(
    flags: &mut FeatureFlags,
    feature_names: &[String],
    action: FeatureFlagAction,
) -> anyhow::Result<()> {
    for feature_name in feature_names {
        let feature = Feature::from_str(feature_name)
            .map_err(|_| anyhow!("Invalid feature flag '{feature_name}'"))?;
        action.apply(flags, feature);
    }
    Ok(())
}

/// List all available feature flags and their status
#[allow(clippy::unwrap_used)]
pub(crate) fn list_all_features(flags: &FeatureFlags) {
    use std::io::Write;

    let stdout = std::io::stdout();
    let mut stdout = stdout.lock();

    writeln!(stdout, "Available Feature Flags:").unwrap();
    writeln!(stdout).unwrap();

    for category in [
        FeatureCategory::Experimental,
        FeatureCategory::Enterprise,
        FeatureCategory::Performance,
        FeatureCategory::Debug,
        FeatureCategory::Testing,
    ] {
        writeln!(stdout, "{}:", category.to_string().to_uppercase()).unwrap();
        for feature in FeatureFlags::features_in_category(category) {
            let status = if flags.is_enabled(feature) {
                "enabled"
            } else {
                "disabled"
            };
            writeln!(
                stdout,
                "  {:20} ({:8}) - {}",
                feature.to_string(),
                status,
                feature.description()
            )
            .unwrap();
        }
        writeln!(stdout).unwrap();
    }

    writeln!(
        stdout,
        "Environment variables: COPYBOOK_FF_<FEATURE_NAME>=1 to enable"
    )
    .unwrap();
}

/// Get effective dialect from CLI flag or environment variable
///
/// Precedence: CLI flag > `COPYBOOK_DIALECT` env var > default (Normative)
pub(crate) fn effective_dialect(cli_dialect: Option<DialectPreference>) -> DialectPreference {
    if let Some(dialect) = cli_dialect {
        return dialect;
    }
    if let Ok(env_val) = std::env::var("COPYBOOK_DIALECT") {
        match env_val.trim().to_ascii_lowercase().as_str() {
            "0" => DialectPreference::Zero,
            "1" => DialectPreference::One,
            _ => DialectPreference::N, // Default to normative on invalid value
        }
    } else {
        DialectPreference::N // Default to normative
    }
}
