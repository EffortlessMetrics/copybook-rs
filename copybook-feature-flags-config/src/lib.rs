#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_governance_contracts::feature_flags::{Feature, FeatureCategory, FeatureFlags};
use std::fmt;
use std::path::Path;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct FeatureFlagOverrides {
    pub enable_category: Vec<String>,
    pub disable_category: Vec<String>,
    pub enable_features: Vec<String>,
    pub disable_features: Vec<String>,
}

#[derive(Debug)]
pub enum FeatureFlagsConfigError {
    Io(std::io::Error),
    Parse,
    InvalidCategory(String),
    InvalidFeature(String),
}

impl fmt::Display for FeatureFlagsConfigError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(err) => write!(f, "{err}"),
            Self::Parse => write!(
                f,
                "Failed to parse feature flags config: expected JSON or TOML format"
            ),
            Self::InvalidCategory(category) => write!(
                f,
                "Invalid feature category '{category}'. Valid categories: experimental, enterprise, performance, debug, testing"
            ),
            Self::InvalidFeature(feature) => write!(f, "Invalid feature flag '{feature}'"),
        }
    }
}

impl std::error::Error for FeatureFlagsConfigError {}

impl From<std::io::Error> for FeatureFlagsConfigError {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

#[must_use]
pub fn from_env() -> FeatureFlags {
    FeatureFlags::from_env()
}

pub fn apply_config_file(
    flags: &mut FeatureFlags,
    config_path: &Path,
) -> Result<(), FeatureFlagsConfigError> {
    let content = std::fs::read_to_string(config_path)?;

    if let Ok(json_config) = serde_json::from_str::<serde_json::Value>(&content) {
        apply_json_config(flags, &json_config);
        Ok(())
    } else if let Ok(toml_value) = content.parse::<toml::Value>() {
        apply_toml_config(flags, &toml_value);
        Ok(())
    } else {
        Err(FeatureFlagsConfigError::Parse)
    }
}

pub fn apply_overrides(
    flags: &mut FeatureFlags,
    overrides: &FeatureFlagOverrides,
) -> Result<(), FeatureFlagsConfigError> {
    for category_name in &overrides.enable_category {
        let category = parse_category(category_name)?;
        for feature in FeatureFlags::features_in_category(category) {
            flags.enable(feature);
        }
    }

    for category_name in &overrides.disable_category {
        let category = parse_category(category_name)?;
        for feature in FeatureFlags::features_in_category(category) {
            flags.disable(feature);
        }
    }

    for feature_name in &overrides.enable_features {
        let feature = Feature::from_str(feature_name)
            .map_err(|_| FeatureFlagsConfigError::InvalidFeature(feature_name.clone()))?;
        flags.enable(feature);
    }

    for feature_name in &overrides.disable_features {
        let feature = Feature::from_str(feature_name)
            .map_err(|_| FeatureFlagsConfigError::InvalidFeature(feature_name.clone()))?;
        flags.disable(feature);
    }

    Ok(())
}

fn parse_category(category_name: &str) -> Result<FeatureCategory, FeatureFlagsConfigError> {
    match category_name.to_lowercase().as_str() {
        "experimental" => Ok(FeatureCategory::Experimental),
        "enterprise" => Ok(FeatureCategory::Enterprise),
        "performance" => Ok(FeatureCategory::Performance),
        "debug" => Ok(FeatureCategory::Debug),
        "testing" => Ok(FeatureCategory::Testing),
        _ => Err(FeatureFlagsConfigError::InvalidCategory(
            category_name.to_owned(),
        )),
    }
}

fn apply_json_config(flags: &mut FeatureFlags, json_config: &serde_json::Value) {
    if let Some(feature_flags) = json_config.get("feature_flags") {
        apply_arrays(flags, feature_flags.get("enabled"), true);
        apply_arrays(flags, feature_flags.get("disabled"), false);
    }
}

fn apply_toml_config(flags: &mut FeatureFlags, toml_config: &toml::Value) {
    if let Some(feature_flags) = toml_config.get("feature_flags") {
        apply_arrays(flags, feature_flags.get("enabled"), true);
        apply_arrays(flags, feature_flags.get("disabled"), false);
    }
}

fn apply_arrays(flags: &mut FeatureFlags, values: Option<&impl ArrayLike>, enable: bool) {
    if let Some(values) = values {
        for value in values.as_array() {
            if let Ok(feature) = Feature::from_str(value) {
                if enable {
                    flags.enable(feature);
                } else {
                    flags.disable(feature);
                }
            }
        }
    }
}

trait ArrayLike {
    fn as_array(&self) -> Vec<&str>;
}

impl ArrayLike for serde_json::Value {
    fn as_array(&self) -> Vec<&str> {
        self.as_array()
            .map(|array| array.iter().filter_map(serde_json::Value::as_str).collect())
            .unwrap_or_default()
    }
}

impl ArrayLike for toml::Value {
    fn as_array(&self) -> Vec<&str> {
        self.as_array()
            .map(|array| array.iter().filter_map(toml::Value::as_str).collect())
            .unwrap_or_default()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn parses_json_config() {
        let mut flags = FeatureFlags::default();
        let json = serde_json::json!({
            "feature_flags": {
                "enabled": ["verbose_logging"],
                "disabled": ["lru_cache"]
            }
        });
        apply_json_config(&mut flags, &json);
        assert!(flags.is_enabled(Feature::VerboseLogging));
        assert!(!flags.is_enabled(Feature::LruCache));
    }

    #[test]
    fn invalid_category_returns_error() {
        let mut flags = FeatureFlags::default();
        let overrides = FeatureFlagOverrides {
            enable_category: vec!["oops".into()],
            disable_category: vec![],
            enable_features: vec![],
            disable_features: vec![],
        };
        let err = apply_overrides(&mut flags, &overrides).expect_err("must fail");
        assert!(err.to_string().contains("Invalid feature category"));
    }
}
