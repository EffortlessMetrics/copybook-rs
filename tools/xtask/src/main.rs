// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Result, bail};
use copybook_core::support_matrix;
use std::{fs, path::Path};
use xtask::publish::{PlanFormat, run_plan};
use xtask::{Counts, architecture, counts, perf};

mod docs_verify;
mod pr_insights;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let arg_refs = args
        .iter()
        .map(std::string::String::as_str)
        .collect::<Vec<_>>();

    match arg_refs.as_slice() {
        ["architecture", "check"] => architecture::run_check(),
        ["architecture", "report"] => architecture::run_report(false),
        ["architecture", "report", "--format", "json"] => architecture::run_report(true),
        ["architecture", "debt-generate"] => architecture::run_debt_generate(),
        ["docs", "sync-tests"] => sync(),
        ["docs", "verify-tests"] => verify(),
        ["docs", "verify-all"] => docs_verify::run(),
        ["docs", "verify-record-pipeline"] => docs_verify::verify_record_pipeline_command(),
        ["docs", "sync-record-pipeline"] => docs_verify::sync_record_pipeline_command(),
        ["docs", "verify-stable-errors"] => docs_verify::verify_stable_error_registry_command(),
        ["docs", "verify-support-matrix"] => verify_support_matrix(),
        ["docs", "freeze", "contracts"] => docs_verify::run_freeze_contract_checks(),
        ["docs", "contracts", "generate"] => docs_verify::run_contracts_command(),
        ["perf"] => perf::run(false, None),
        ["perf", "--enforce"] => perf::run(true, None),
        ["perf", "--out-dir", out_dir] => perf::run(false, Some(out_dir)),
        ["perf", "--enforce", "--out-dir", out_dir] => perf::run(true, Some(out_dir)),
        ["perf", "--summarize-last" | "--summarize"] => perf_summarize_last(),
        ["publish", "plan", rest @ ..] => publish_plan(rest),
        ["pr-insights"] => pr_insights::generate_summary(),
        _ => {
            usage();
            Ok(())
        }
    }
}

fn publish_plan(args: &[&str]) -> Result<()> {
    let mut format = PlanFormat::Plain;
    let mut check_only = false;

    let mut i = 0;
    while i < args.len() {
        match args[i] {
            "--check" => {
                check_only = true;
            }
            "--format" => {
                let Some(value) = args.get(i + 1) else {
                    bail!("Missing value for --format. Expected: plain or json.");
                };
                format = match *value {
                    "plain" => PlanFormat::Plain,
                    "json" => PlanFormat::Json,
                    other => {
                        bail!("Unknown format '{other}'. Expected: plain or json.");
                    }
                };
                i += 1;
            }
            other => {
                bail!(
                    "Unknown publish plan argument '{other}'. Expected: --check and optional --format <plain|json>."
                );
            }
        }
        i += 1;
    }

    run_plan(format, check_only)
}

fn usage() {
    eprintln!(
        "Usage: cargo run -p xtask -- [architecture|docs|perf|publish|pr-insights] <subcommand>\n\
         \n\
         architecture check                  Validate package roles, dependency direction, registry fields, and exact debt\n\
         architecture report                 Print the package ownership and dependency report\n\
         architecture report --format json   Print the report as JSON\n\
         architecture debt-generate          Explicitly regenerate the exact debt baseline\n\
         docs sync-tests                     Sync test status from junit.xml\n\
         docs verify-tests                   Verify test status is in sync\n\
         docs verify-all                     Verify all source-of-truth documentation invariants\n\
         docs verify-record-pipeline         Verify fixed/RDW evidence registry anchors\n\
         docs sync-record-pipeline           Refresh the fixed/RDW evidence content digest\n\
         docs verify-stable-errors            Verify stable error taxonomy registry\n\
         docs freeze contracts               Verify freeze-sensitive contracts (strict, API surface contract guard)\n\
         docs contracts generate             Regenerate stable contract manifest baseline\n\
         docs verify-support-matrix          Verify support matrix registry -> docs\n\
         perf                                Run perf benchmark runner\n\
         perf --enforce                      Run perf with SLO enforcement\n\
         perf --out-dir <path>               Run perf with custom output directory\n\
         perf --summarize-last               Summarize latest perf.json with SLO comparison\n\
         publish plan [--format <plain|json>] [--check]  Print and validate the role-aware publish plan\n\
         pr-insights                         Generate PR insights report (nextest + perf)"
    );
}

fn block(c: &Counts) -> String {
    let p = c.passed;
    let s = c.skipped;
    // Emit only fields the junit receipts actually prove. Roundtrip/negative
    // counts and leak absence have no receipt source (leak absence is
    // explicitly NOT_PROVEN, see #776), so they must not appear here.
    format!(
        "**conformance:** {p}/{p}  \u{2022} **skipped:** {s}<br>\n\
         _Source: CI receipts (nextest/junit). This block is updated automatically._"
    )
}

const TEST_STATUS_PATHS: [&str; 2] = ["README.md", "docs/REPORT.md"];

fn replace_in_file(path: &str, new_block: &str) -> Result<()> {
    let content = fs::read_to_string(path)?;

    // Find the TEST_STATUS section and replace it
    let re = regex::Regex::new(r"(?s)<!-- TEST_STATUS:BEGIN -->.*?<!-- TEST_STATUS:END -->")?;

    let replacement = format!("<!-- TEST_STATUS:BEGIN -->\n{new_block}\n<!-- TEST_STATUS:END -->");

    let new_content = re.replace(&content, replacement.as_str());
    fs::write(path, new_content.as_ref())?;

    Ok(())
}

fn sync() -> Result<()> {
    let c = counts()?;
    let b = block(&c);

    // Only update files that carry the marker: the block was removed from the
    // README front page, and sync must not reintroduce it.
    for path in TEST_STATUS_PATHS {
        let content = fs::read_to_string(path)?;
        if content.contains("<!-- TEST_STATUS:BEGIN -->") {
            replace_in_file(path, &b)?;
        }
    }

    println!("\u{2713} Synced test status where the marker is present");
    Ok(())
}

fn verify() -> Result<()> {
    let c = counts()?;
    let expected = block(&c);

    for path in TEST_STATUS_PATHS {
        let content = fs::read_to_string(path)?;
        if !content.contains("<!-- TEST_STATUS:BEGIN -->") {
            continue;
        }
        if !content.contains(&expected) {
            bail!("{path} test-status out of sync");
        }
    }

    println!("\u{2713} Test status is in sync");
    Ok(())
}

/// Expected status marker for a registry [`support_matrix::SupportStatus`].
///
/// The registry is authoritative for feature status (#656 Phase G); each
/// governed doc row renders one of these markers in its Status column.
fn status_marker(status: support_matrix::SupportStatus) -> &'static str {
    match status {
        support_matrix::SupportStatus::Supported => "✅",
        support_matrix::SupportStatus::Partial => "⚠️",
        support_matrix::SupportStatus::Planned => "🔄",
        support_matrix::SupportStatus::NotPlanned => "❌",
        // SupportStatus is non_exhaustive: unknown future variants map to a
        // marker no row carries, forcing an explicit verifier update.
        _ => "❓",
    }
}

/// True for Markdown table separator rows (`|---|---|`, with optional colons).
fn is_separator_line(line: &str) -> bool {
    let trimmed = line.trim();
    if !trimmed.starts_with('|') || !trimmed.ends_with('|') {
        return false;
    }
    let cells: Vec<&str> = trimmed
        .split('|')
        .map(str::trim)
        .filter(|cell| !cell.is_empty())
        .collect();
    !cells.is_empty()
        && cells.iter().all(|cell| {
            let inner = cell.trim_matches(':');
            inner.len() >= 3 && inner.chars().all(|c| c == '-')
        })
}

/// Split a Markdown table line into trimmed cells.
///
/// Returns `None` for non-table lines and separator rows (`|---|---|`).
/// Interior empty cells are dropped, so callers must only use the result on
/// dense tables (no `||` gaps); every governed support-matrix table is dense.
fn table_cells(line: &str) -> Option<Vec<String>> {
    let trimmed = line.trim();
    if !trimmed.starts_with('|') || !trimmed.ends_with('|') {
        return None;
    }
    let cells: Vec<String> = trimmed
        .split('|')
        .map(str::trim)
        .filter(|cell| !cell.is_empty())
        .map(str::to_owned)
        .collect();
    if cells.is_empty() {
        return None;
    }
    let is_separator = cells.iter().all(|cell| {
        let inner = cell.trim_matches(':');
        inner.len() >= 3 && inner.chars().all(|c| c == '-')
    });
    if is_separator { None } else { Some(cells) }
}

/// Verify registry features against the governed `Feature|Status` doc tables.
///
/// A table is governed when its header row has `Feature` and `Status` cells;
/// the Status column index is derived from that header. Each registry feature
/// must have exactly one governed body row carrying its `` (`id`) `` marker,
/// and that row's Status cell must contain the registry status marker.
///
/// Returns one error line per drifted feature (`missing:`, `duplicate:`, or
/// `status:` prefixed). Pure over the doc text so checker changes carry
/// accept/reject fixtures (unit tests at the bottom of this file).
fn verify_support_matrix_content(
    doc: &str,
    features: &[support_matrix::FeatureSupport],
) -> Vec<String> {
    // Collect (status column index, body rows) for every governed table.
    let mut governed: Vec<(usize, Vec<Vec<String>>)> = Vec::new();
    let mut current: Option<(usize, Vec<Vec<String>>)> = None;
    let flush = |current: &mut Option<(usize, Vec<Vec<String>>)>,
                 governed: &mut Vec<(usize, Vec<Vec<String>>)>| {
        if let Some(table) = current.take() {
            governed.push(table);
        }
    };
    for line in doc.lines() {
        // Separator rows (`|---|---|`) belong to the current table: skip them
        // without flushing, or the header's own separator would end the table
        // before any body row is collected.
        if is_separator_line(line) {
            continue;
        }
        let Some(cells) = table_cells(line) else {
            flush(&mut current, &mut governed);
            continue;
        };
        let header_status = cells
            .iter()
            .position(|cell| cell == "Status")
            .filter(|_| cells.iter().any(|cell| cell == "Feature"));
        if let Some(status_idx) = header_status {
            flush(&mut current, &mut governed);
            current = Some((status_idx, Vec::new()));
        } else if let Some((_, rows)) = current.as_mut() {
            rows.push(cells);
        }
        // Cells before any governed header are ignored.
    }
    flush(&mut current, &mut governed);

    let mut errors = Vec::new();
    for feature in features {
        let id =
            serde_plain::to_string(&feature.id).unwrap_or_else(|_| format!("{:?}", feature.id));
        let row_marker = format!("(`{id}`)");
        let mut matches: Vec<(usize, &[String])> = Vec::new();
        for (status_idx, rows) in &governed {
            for row in rows {
                if row.iter().any(|cell| cell.contains(&row_marker)) {
                    matches.push((*status_idx, row));
                }
            }
        }
        if matches.is_empty() {
            errors.push(format!(
                "missing: `{id}` is in the registry but has no governed row"
            ));
        } else if matches.len() > 1 {
            errors.push(format!(
                "duplicate: `{id}` has {} governed rows; keep exactly one",
                matches.len()
            ));
        } else {
            let (status_idx, row) = matches[0];
            let marker = status_marker(feature.status);
            let agrees = row
                .get(status_idx)
                .is_some_and(|cell| cell.contains(marker));
            if !agrees {
                errors.push(format!(
                    "status: `{id}`: registry says {:?} (expected marker {marker} in the Status column)",
                    feature.status
                ));
            }
        }
    }
    errors
}

fn verify_support_matrix() -> Result<()> {
    let doc_path = "docs/reference/COBOL_SUPPORT_MATRIX.md";
    let doc_content = fs::read_to_string(doc_path)?;

    let errors = verify_support_matrix_content(&doc_content, support_matrix::all_features());
    if !errors.is_empty() {
        bail!(
            "Support matrix drift detected!\n\
             The registry is authoritative for feature status (#656 Phase G).\n\
             These {doc_path} rows disagree:\n  - {}\n\n\
             Align the governed rows with the registry: exactly one row per \
             feature, with the Status column carrying the registry marker.",
            errors.join("\n  - ")
        );
    }

    println!(
        "\u{2713} Support matrix registry \u{2194} docs in sync ({} features verified)",
        support_matrix::all_features().len()
    );
    Ok(())
}

fn perf_summarize_last() -> Result<()> {
    // Try to find the latest perf.json, preferring scripts/bench/perf.json (canonical)
    let canonical = Path::new("scripts/bench/perf.json");
    let perf_path = if canonical.exists() {
        canonical.to_path_buf()
    } else {
        // Try to find the latest in target/benchmarks/
        let benchmarks_dir = Path::new("target/benchmarks");
        if !benchmarks_dir.exists() {
            bail!("No perf.json found. Run benchmarks first:\n  bash scripts/bench.sh");
        }

        // Find the most recent timestamp directory
        let mut dirs: Vec<_> = fs::read_dir(benchmarks_dir)?
            .filter_map(Result::ok)
            .filter(|e| e.path().is_dir())
            .collect();

        if dirs.is_empty() {
            bail!("No benchmark runs found in target/benchmarks/");
        }

        // Sort by name (which should be timestamps)
        dirs.sort_by_key(std::fs::DirEntry::path);

        // Try to find perf.json in the latest directory
        let Some(latest_entry) = dirs.last() else {
            anyhow::bail!(
                "No benchmark receipt directories found under {}",
                benchmarks_dir.display()
            )
        };
        let latest_dir = &latest_entry.path();
        let latest_perf = latest_dir.join("perf.json");

        if !latest_perf.exists() {
            bail!(
                "No perf.json found in latest benchmark run: {}",
                latest_dir.display()
            );
        }

        latest_perf
    };

    // Parse the JSON using pure function
    let json_content = fs::read_to_string(perf_path)?;
    let snapshot = perf::parse_perf_receipt(&json_content)?;

    // Evaluate SLO compliance
    let status = perf::evaluate_slo(&snapshot);

    // Emit formatted summary
    let summary = perf::format_slo_summary(&snapshot, &status);
    println!("{summary}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::support_matrix::{FeatureId, FeatureSupport, SupportStatus};

    const HEADER: &str = "| Feature | Status | Test Evidence | Notes |\n|---------|--------|---------------|-------|\n";

    fn feature(id: FeatureId, status: SupportStatus) -> FeatureSupport {
        FeatureSupport {
            id,
            name: "fixture",
            description: "fixture",
            status,
            doc_ref: None,
        }
    }

    fn check(doc: &str, id: FeatureId, status: SupportStatus) -> Vec<String> {
        verify_support_matrix_content(doc, std::slice::from_ref(&feature(id, status)))
    }

    #[test]
    fn status_marker_mapping() {
        assert_eq!(status_marker(SupportStatus::Supported), "✅");
        assert_eq!(status_marker(SupportStatus::Partial), "⚠️");
        assert_eq!(status_marker(SupportStatus::Planned), "🔄");
        assert_eq!(status_marker(SupportStatus::NotPlanned), "❌");
    }

    #[test]
    fn accept_supported_row() {
        let doc =
            format!("{HEADER}| Edited PIC (`edited-pic`) | ✅ Fully Supported | `e1.rs` | masks |");
        assert!(
            check(&doc, FeatureId::EditedPic, SupportStatus::Supported).is_empty(),
            "matching Status marker must verify"
        );
    }

    #[test]
    fn accept_partial_row_with_emoji_in_notes() {
        // Notes-column emoji must not disturb the Status-column agreement.
        let doc = format!(
            "{HEADER}| Nested ODO (`nested-odo`) | ⚠️ Partially Supported (O1-O4) | `o1.rs` | O1-O4✅ supported; O5-O6🚫 rejected |"
        );
        assert!(
            check(&doc, FeatureId::NestedOdo, SupportStatus::Partial).is_empty(),
            "notes emoji must not break Partial agreement"
        );
    }

    #[test]
    fn reject_supported_registry_change_masked_by_notes_emoji() {
        // Devin review on #896: flipping the registry to Supported while the
        // row still renders ⚠️ must fail even though the notes contain ✅.
        let doc = format!(
            "{HEADER}| Nested ODO (`nested-odo`) | ⚠️ Partially Supported (O1-O4) | `o1.rs` | O1-O4✅ supported; O5-O6🚫 rejected |"
        );
        let errors = check(&doc, FeatureId::NestedOdo, SupportStatus::Supported);
        assert_eq!(errors.len(), 1, "expected one status error, got {errors:?}");
        assert!(
            errors[0].starts_with("status:"),
            "expected a status error, got {:?}",
            errors[0]
        );
    }

    #[test]
    fn reject_marker_only_in_evidence_column() {
        // The expected marker in Evidence (not Status) is still drift.
        let doc = format!(
            "{HEADER}| ODO (`occurs-depending`) | ✅ Fully Supported | ⚠️ tail-only run `odo.rs` | driver checks |"
        );
        let errors = check(&doc, FeatureId::OccursDepending, SupportStatus::Partial);
        assert_eq!(
            errors.len(),
            1,
            "evidence-column marker must not satisfy the Status check, got {errors:?}"
        );
        assert!(errors[0].starts_with("status:"));
    }

    #[test]
    fn reject_mismatched_marker() {
        let doc = format!(
            "{HEADER}| SIGN SEPARATE (`sign-separate`) | ✅ Fully Supported | `s.rs` | unconditional |"
        );
        let errors = check(&doc, FeatureId::SignSeparate, SupportStatus::Partial);
        assert_eq!(errors.len(), 1, "got {errors:?}");
        assert!(errors[0].starts_with("status:"));
    }

    #[test]
    fn reject_missing_row() {
        let doc = format!("{HEADER}| Unrelated | ✅ Supported | `u.rs` | - |");
        let errors = check(&doc, FeatureId::Level88Conditions, SupportStatus::Supported);
        assert_eq!(errors.len(), 1, "got {errors:?}");
        assert!(errors[0].starts_with("missing:"));
    }

    #[test]
    fn reject_duplicate_rows() {
        let doc = format!(
            "{HEADER}| Level-88 (`level-88`) | ✅ Fully Supported | `a.rs` | - |\n| Level-88 bis (`level-88`) | ✅ Fully Supported | `b.rs` | - |"
        );
        let errors = check(&doc, FeatureId::Level88Conditions, SupportStatus::Supported);
        assert_eq!(errors.len(), 1, "got {errors:?}");
        assert!(errors[0].starts_with("duplicate:"));
    }

    #[test]
    fn ignores_ungoverned_tables() {
        // An id row in a table without a Feature|Status header is not governed.
        let doc = "| Scenario | Detail |\n|---|---|\n| Nested (`nested-odo`) | ✅ |\n";
        let errors = check(doc, FeatureId::NestedOdo, SupportStatus::Supported);
        assert_eq!(errors.len(), 1, "got {errors:?}");
        assert!(errors[0].starts_with("missing:"));
    }

    #[test]
    fn duplicate_across_governed_tables_is_rejected() {
        let row = "| Nested ODO (`nested-odo`) | ⚠️ Partial | `o.rs` | - |";
        let doc = format!("{HEADER}{row}\n\nSome prose.\n\n{HEADER}{row}");
        let errors = check(&doc, FeatureId::NestedOdo, SupportStatus::Partial);
        assert_eq!(errors.len(), 1, "got {errors:?}");
        assert!(errors[0].starts_with("duplicate:"));
    }
}
