// SPDX-License-Identifier: AGPL-3.0-or-later
//! Support command for COBOL feature matrix
//!
//! Provides CLI access to the feature support matrix defined in `copybook-core::support_matrix`.

use crate::exit_codes::ExitCode;
use copybook_core::support_matrix::{self, SupportStatus};

#[derive(clap::Args)]
pub struct SupportArgs {
    /// Output format
    #[arg(long, value_enum, default_value = "table")]
    pub format: OutputFormat,

    /// Check feature support by ID
    #[arg(long)]
    pub check: Option<String>,

    /// Filter by support status
    #[arg(long, value_enum)]
    pub status: Option<StatusFilter>,
}

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
pub enum OutputFormat {
    Table,
    Json,
}

#[derive(Clone, Copy, Debug, clap::ValueEnum, PartialEq)]
pub enum StatusFilter {
    Supported,
    Partial,
    Planned,
    NotPlanned,
}

pub fn run(args: &SupportArgs) -> anyhow::Result<ExitCode> {
    if let Some(feature_id) = &args.check {
        // Check specific feature with exit code based on status
        if let Some(feature) = support_matrix::find_feature(feature_id) {
            // Simple rule: only `supported` is success; everything else is non-zero exit.
            match feature.status {
                SupportStatus::Supported => {
                    println!("Feature: {}", feature.name);
                    println!("Status: {:?}", feature.status);
                    println!("Description: {}", feature.description);
                    if let Some(doc_ref) = feature.doc_ref {
                        println!("Documentation: {doc_ref}");
                    }
                    Ok(ExitCode::Ok)
                }
                _status => {
                    eprintln!(
                        "❌ Feature '{}' not fully supported (status: {:?}). See {}",
                        feature_id,
                        feature.status,
                        feature.doc_ref.unwrap_or("project documentation"),
                    );
                    Ok(ExitCode::Encode) // Use non-zero exit code (policy/validation failure)
                }
            }
        } else {
            eprintln!("Error: Unknown feature ID: {feature_id}");
            Ok(ExitCode::Unknown)
        }
    } else {
        // Display full matrix
        let features = support_matrix::all_features();

        let filtered: Vec<_> = if let Some(status_filter) = args.status {
            features
                .iter()
                .filter(|f| matches_status_filter(f.status, status_filter))
                .cloned()
                .collect()
        } else {
            features.to_vec()
        };

        match args.format {
            OutputFormat::Table => {
                println!("COBOL Feature Support Matrix");
                println!();
                println!("{:<25} {:<15} Description", "Feature", "Status");
                println!("{}", "-".repeat(80));

                for feature in &filtered {
                    let status_str = format!("{:?}", feature.status);
                    println!(
                        "{:<25} {:<15} {}",
                        feature.name, status_str, feature.description
                    );
                }

                println!();
                println!(
                    "Use 'copybook support --check <feature-id>' to check a specific feature."
                );
                println!("Use 'copybook support --format json' for machine-readable output.");
            }
            OutputFormat::Json => {
                let json = serde_json::to_string_pretty(&filtered)?;
                println!("{json}");
            }
        }

        Ok(ExitCode::Ok)
    }
}

fn matches_status_filter(status: support_matrix::SupportStatus, filter: StatusFilter) -> bool {
    use support_matrix::SupportStatus;
    matches!(
        (status, filter),
        (SupportStatus::Supported, StatusFilter::Supported)
            | (SupportStatus::Partial, StatusFilter::Partial)
            | (SupportStatus::Planned, StatusFilter::Planned)
            | (SupportStatus::NotPlanned, StatusFilter::NotPlanned)
    )
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_find_feature_level88() {
        let feature = support_matrix::find_feature("level-88");
        assert!(feature.is_some());
        let f = feature.expect("Feature should exist");
        assert_eq!(f.name, "LEVEL 88 condition names");
    }

    #[test]
    fn test_find_feature_unknown() {
        let feature = support_matrix::find_feature("no-such-feature");
        assert!(feature.is_none());
    }

    #[test]
    fn test_all_features_nonempty() {
        let features = support_matrix::all_features();
        assert!(!features.is_empty());
    }

    #[test]
    fn test_json_feature_set_equality() {
        // This test ensures that the JSON output contains exactly the same
        // features as the registry, preventing drift between CLI and core
        use std::collections::HashSet;

        let features = support_matrix::all_features();

        // Serialize to JSON and parse back
        let json = serde_json::to_string(&features).expect("Failed to serialize");
        let parsed: Vec<serde_json::Value> =
            serde_json::from_str(&json).expect("Failed to parse JSON");

        // Extract IDs from JSON
        let json_ids: HashSet<String> = parsed
            .iter()
            .filter_map(|v| v.get("id").and_then(|id| id.as_str()).map(String::from))
            .collect();

        // Extract IDs from registry
        let registry_ids: HashSet<String> = features
            .iter()
            .filter_map(|f| serde_plain::to_string(&f.id).ok())
            .collect();

        // Assert equality
        assert_eq!(
            json_ids, registry_ids,
            "JSON feature IDs must match registry exactly"
        );
        assert_eq!(
            json_ids.len(),
            features.len(),
            "All features must be represented"
        );
    }
}
