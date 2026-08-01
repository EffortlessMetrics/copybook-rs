// SPDX-License-Identifier: AGPL-3.0-or-later
//! Support command for COBOL feature matrix.
//!
//! Provides CLI access to:
//! - The canonical support matrix (`copybook-support-matrix`).
//! - Runtime governance linkage to feature flags (`copybook-governance-grid`).

use crate::exit_codes::ExitCode;
use copybook_governance as governance;
use governance::FeatureFlags;
use std::fmt::Write as _;

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
        eprintln!("Error: unknown feature ID: {feature_id}");
        eprintln!("Known feature IDs: {}", known_feature_ids().join(", "));
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
    let mut out = String::new();
    match state.support_status {
        governance::SupportStatus::Supported => {
            writeln!(out, "Feature: {}", state.support_name).ok();
            writeln!(out, "ID: {}", feature_id_str(state.support_id)).ok();
            writeln!(out, "Status: {}", status_str(state.support_status)).ok();
            writeln!(out, "Description: {}", state.support_description).ok();
            if let Some(doc_ref) = state.doc_ref {
                writeln!(out, "Documentation: {doc_ref}").ok();
            }

            if with_governance {
                writeln!(out, "Runtime-Available: {}", state.runtime_enabled).ok();
                writeln!(
                    out,
                    "Required Feature Flags: {}",
                    format_flags(state.required_feature_flags)
                )
                .ok();
                writeln!(
                    out,
                    "Missing Feature Flags: {}",
                    format_flags(&state.missing_feature_flags)
                )
                .ok();
                writeln!(out, "Rationale: {}", state.rationale).ok();

                if let Some(state) =
                    governance::governance_state_for_support_id(state.support_id, feature_flags)
                {
                    if state.missing_feature_flags.is_empty() {
                        writeln!(out, "Runtime gating status: enabled by feature flags").ok();
                    } else {
                        writeln!(out, "Runtime gating status: disabled by feature flags").ok();
                    }
                }
            }

            write_stdout(&out);
            ExitCode::Ok
        }
        _status => {
            eprintln!(
                "Feature '{}' is not fully supported (status: {}). See {}",
                feature_id,
                status_str(state.support_status),
                state.doc_ref.unwrap_or("project documentation"),
            );
            if with_governance {
                writeln!(out, "Runtime-Available: {}", state.runtime_enabled).ok();
                writeln!(
                    out,
                    "Missing Feature Flags: {}",
                    format_flags(&state.missing_feature_flags)
                )
                .ok();
                write_stdout(&out);
            }
            ExitCode::Encode // Non-zero exit for policy/validation failure.
        }
    }
}

/// Write to stdout through the CLI's pipe-safe writer.
///
/// `println!` panics when the consumer closes the pipe, so `copybook support |
/// head` printed a panic backtrace before the process exited.
fn write_stdout(text: &str) {
    let _ = crate::write_stdout_all(text.as_bytes());
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

    let mut out = String::new();
    match format {
        OutputFormat::Table => {
            // The `ID` column is what `--check` consumes; without it the footer
            // points at identifiers the table never shows.
            let mut rows: Vec<Vec<String>> = Vec::with_capacity(filtered.len());
            let headers: Vec<&str> = if with_governance {
                writeln!(out, "COBOL Feature Support + Governance").ok();
                vec![
                    "ID",
                    "Feature",
                    "Status",
                    "Feature Flags",
                    "Runtime",
                    "Description",
                ]
            } else {
                writeln!(out, "COBOL Feature Support Matrix").ok();
                vec!["ID", "Feature", "Status", "Description"]
            };
            out.push('\n');

            for feature in &filtered {
                let mut row = vec![
                    feature_id_str(feature.support_id),
                    feature.support_name.to_string(),
                    status_str(feature.support_status).to_string(),
                ];
                if with_governance {
                    row.push(format_flags(feature.required_feature_flags));
                    row.push(
                        if feature.runtime_enabled {
                            "enabled"
                        } else {
                            "disabled-by-flags"
                        }
                        .to_string(),
                    );
                }
                row.push(feature.support_description.to_string());
                rows.push(row);
            }

            render_table(&mut out, &headers, &rows);

            out.push('\n');
            writeln!(
                out,
                "Use 'copybook support --check <ID>' to check a specific feature."
            )
            .ok();
            writeln!(
                out,
                "Use 'copybook support --format json' for machine-readable output."
            )
            .ok();
            if !with_governance {
                writeln!(
                    out,
                    "Use 'copybook support --with-governance' to include runtime flag linkage."
                )
                .ok();
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
            writeln!(out, "{json}").ok();
        }
    }

    write_stdout(&out);
    Ok(ExitCode::Ok)
}

/// Canonical kebab-case identifier for a feature, matching `--check` and the
/// `id` field emitted by `--format json`.
fn feature_id_str(id: governance::FeatureId) -> String {
    serde_plain::to_string(&id).unwrap_or_else(|_| format!("{id:?}"))
}

/// Canonical status spelling, matching `--status` values and `--format json`.
fn status_str(status: governance::SupportStatus) -> &'static str {
    match status {
        governance::SupportStatus::Supported => "supported",
        governance::SupportStatus::Partial => "partial",
        governance::SupportStatus::Planned => "planned",
        governance::SupportStatus::NotPlanned => "not-planned",
        // `SupportStatus` is non-exhaustive upstream.
        _ => "unknown",
    }
}

/// Every identifier `--check` accepts, in matrix order.
fn known_feature_ids() -> Vec<String> {
    governance::support_matrix::all_features()
        .iter()
        .map(|feature| feature_id_str(feature.id))
        .collect()
}

/// Render a table whose columns fit their contents.
///
/// The final column is not padded, so a long description never drags trailing
/// whitespace across the line.
fn render_table(out: &mut String, headers: &[&str], rows: &[Vec<String>]) {
    let widths: Vec<usize> = headers
        .iter()
        .enumerate()
        .map(|(column, header)| {
            rows.iter()
                .filter_map(|row| row.get(column))
                .map(String::len)
                .chain(std::iter::once(header.len()))
                .max()
                .unwrap_or(0)
        })
        .collect();

    let render = |cells: &[&str]| -> String {
        let last = cells.len().saturating_sub(1);
        let mut line = String::new();
        for (column, cell) in cells.iter().enumerate() {
            if column == last {
                line.push_str(cell);
            } else {
                write!(line, "{cell:<width$} ", width = widths[column]).ok();
            }
        }
        line
    };

    let header_line = render(headers);
    writeln!(out, "{header_line}").ok();
    writeln!(out, "{}", "-".repeat(header_line.len())).ok();
    for row in rows {
        let cells: Vec<&str> = row.iter().map(String::as_str).collect();
        writeln!(out, "{}", render(&cells)).ok();
    }
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
