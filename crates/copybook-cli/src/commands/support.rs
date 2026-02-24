// SPDX-License-Identifier: AGPL-3.0-or-later
//! Support command for COBOL feature matrix.
//!
//! Provides CLI access to:
//! - The canonical support matrix (`copybook-support-matrix`).
//! - Runtime governance linkage to feature flags (`copybook-governance-grid`).

use crate::exit_codes::ExitCode;
use copybook_governance as governance;
use governance::FeatureFlags;

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

    /// Include governance + feature-flag linkage metadata.
    #[arg(long)]
    pub with_governance: bool,
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
    let feature_flags = FeatureFlags::global();
    let support_features = if args.with_governance {
        governance::governance_states(feature_flags)
    } else {
        governance::support_states()
    };

    if let Some(feature_id) = &args.check {
        return Ok(run_check(
            feature_id,
            args.with_governance,
            &support_features,
            feature_flags,
        ));
    }

    run_matrix_view(
        args.format,
        args.status,
        args.with_governance,
        &support_features,
    )
}

fn run_check(
    feature_id: &str,
    with_governance: bool,
    support_features: &[governance::FeatureGovernanceState],
    feature_flags: &FeatureFlags,
) -> ExitCode {
    let Some(support) = governance::support_matrix::find_feature(feature_id) else {
        eprintln!("Error: Unknown feature ID: {feature_id}");
        return ExitCode::Unknown;
    };

    let Some(state) = support_features
        .iter()
        .find(|state| state.support_id == support.id)
    else {
        eprintln!("Error: Governance state not found for feature: {feature_id}");
        return ExitCode::Unknown;
    };

    // Simple rule: only `supported` is success; everything else is non-zero exit.
    match state.support_status {
        governance::SupportStatus::Supported => {
            println!("Feature: {}", state.support_name);
            println!("Status: {:?}", state.support_status);
            println!("Description: {}", state.support_description);
            if let Some(doc_ref) = state.doc_ref {
                println!("Documentation: {doc_ref}");
            }

            if with_governance {
                println!("Runtime-Available: {}", state.runtime_enabled);
                println!(
                    "Required Feature Flags: {}",
                    format_flags(state.required_feature_flags)
                );
                println!(
                    "Missing Feature Flags: {}",
                    format_flags(&state.missing_feature_flags)
                );
                println!("Rationale: {}", state.rationale);

                if let Some(state) =
                    governance::governance_state_for_support_id(state.support_id, feature_flags)
                {
                    if state.missing_feature_flags.is_empty() {
                        println!("Runtime gating status: enabled by feature flags");
                    } else {
                        println!("Runtime gating status: disabled by feature flags");
                    }
                }
            }

            ExitCode::Ok
        }
        _status => {
            eprintln!(
                "Feature '{}' not fully supported (status: {:?}). See {}",
                feature_id,
                state.support_status,
                state.doc_ref.unwrap_or("project documentation"),
            );
            if with_governance {
                println!("Runtime-Available: {}", state.runtime_enabled);
                println!(
                    "Missing Feature Flags: {}",
                    format_flags(&state.missing_feature_flags)
                );
            }
            ExitCode::Encode // Non-zero exit for policy/validation failure.
        }
    }
}

fn run_matrix_view(
    format: OutputFormat,
    status_filter: Option<StatusFilter>,
    with_governance: bool,
    features: &[governance::FeatureGovernanceState],
) -> anyhow::Result<ExitCode> {
    let filtered: Vec<_> = match status_filter {
        Some(status_filter) => features
            .iter()
            .filter(|f| matches_status_filter(f.support_status, status_filter))
            .cloned()
            .collect(),
        None => features.to_vec(),
    };

    match format {
        OutputFormat::Table => {
            if with_governance {
                println!("COBOL Feature Support + Governance");
                println!();
                println!(
                    "{:<25} {:<15} {:<20} {:<16} Description",
                    "Feature", "Status", "Feature Flags", "Runtime",
                );
                println!("{}", "-".repeat(100));
                for feature in &filtered {
                    let status_str = format!("{:?}", feature.support_status);
                    println!(
                        "{:<25} {:<15} {:<20} {:<16} {}",
                        feature.support_name,
                        status_str,
                        format_flags(feature.required_feature_flags),
                        if feature.runtime_enabled {
                            "enabled"
                        } else {
                            "disabled-by-flags"
                        },
                        feature.support_description,
                    );
                }
            } else {
                println!("COBOL Feature Support Matrix");
                println!();
                println!("{:<25} {:<15} Description", "Feature", "Status");
                println!("{}", "-".repeat(80));
                for feature in &filtered {
                    println!(
                        "{:<25} {:<15} {}",
                        feature.support_name,
                        format!("{:?}", feature.support_status),
                        feature.support_description,
                    );
                }
            }

            println!();
            println!("Use 'copybook support --check <feature-id>' to check a specific feature.");
            println!("Use 'copybook support --format json' for machine-readable output.");
            if with_governance {
                println!(
                    "Use 'copybook support --with-governance' to include runtime flag linkage."
                );
            }
        }
        OutputFormat::Json => {
            let json = if with_governance {
                serde_json::to_string_pretty(&filtered)?
            } else {
                let basic: Vec<_> = filtered
                    .iter()
                    .filter_map(|feature| {
                        governance::support_matrix::find_feature_by_id(feature.support_id)
                    })
                    .collect();
                serde_json::to_string_pretty(&basic)?
            };
            println!("{json}");
        }
    }

    Ok(ExitCode::Ok)
}

fn format_flags(flags: &[governance::Feature]) -> String {
    if flags.is_empty() {
        "none".to_string()
    } else {
        flags
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(",")
    }
}

fn matches_status_filter(status: governance::SupportStatus, filter: StatusFilter) -> bool {
    use governance::SupportStatus;
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
        let feature = governance::support_matrix::find_feature("level-88");
        assert!(feature.is_some());
        let f = feature.expect("Feature should exist");
        assert_eq!(f.name, "LEVEL 88 condition names");
    }

    #[test]
    fn test_find_feature_unknown() {
        let feature = governance::support_matrix::find_feature("no-such-feature");
        assert!(feature.is_none());
    }

    #[test]
    fn test_all_features_nonempty() {
        let features = governance::support_matrix::all_features();
        assert!(!features.is_empty());
    }

    #[test]
    fn test_json_feature_set_equality() {
        // This test ensures that the JSON output contains exactly the same
        // features as the registry, preventing drift between CLI and core
        use std::collections::HashSet;

        let features = governance::support_matrix::all_features();

        let json = serde_json::to_string(&features).expect("Failed to serialize");
        let parsed: Vec<serde_json::Value> =
            serde_json::from_str(&json).expect("Failed to parse JSON");

        let json_ids: HashSet<String> = parsed
            .iter()
            .filter_map(|v| v.get("id").and_then(|id| id.as_str()).map(String::from))
            .collect();
        let registry_ids: HashSet<String> = features
            .iter()
            .filter_map(|f| serde_plain::to_string(&f.id).ok())
            .collect();

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

    #[test]
    fn test_format_flags_none() {
        assert_eq!(format_flags(&[]), "none");
    }

    #[test]
    fn test_format_flags_values() {
        let flags = vec![governance::Feature::SignSeparate];
        assert_eq!(format_flags(&flags), "sign_separate");
    }
}
