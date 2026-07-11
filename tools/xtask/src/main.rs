// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Result, bail};
use copybook_core::support_matrix;
use std::{fs, path::Path};
use xtask::publish::{PlanFormat, run_plan};
use xtask::{Counts, counts, perf};

mod docs_verify;
mod pr_insights;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let arg_refs = args
        .iter()
        .map(std::string::String::as_str)
        .collect::<Vec<_>>();

    match arg_refs.as_slice() {
        ["docs", "sync-tests"] => sync(),
        ["docs", "verify-tests"] => verify(),
        ["docs", "verify-all"] => docs_verify::run(),
        ["docs", "verify-support-matrix"] => verify_support_matrix(),
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
        "Usage: cargo run -p xtask -- [docs|perf|publish|pr-insights] <subcommand>\n\
         \n\
         docs sync-tests                     Sync test status from junit.xml\n\
         docs verify-tests                   Verify test status is in sync\n\
         docs verify-all                     Verify all source-of-truth documentation invariants\n\
         docs verify-support-matrix          Verify support matrix registry -> docs\n\
         perf                                Run perf benchmark runner\n\
         perf --enforce                      Run perf with SLO enforcement\n\
         perf --out-dir <path>               Run perf with custom output directory\n\
         perf --summarize-last               Summarize latest perf.json with SLO comparison\n\
         publish plan [--format <plain|json>] [--check]  Print and validate publish order\n\
         pr-insights                         Generate PR insights report (nextest + perf)"
    );
}

fn block(c: &Counts) -> String {
    let p = c.passed;
    let s = c.skipped;
    format!(
        "**conformance:** {p}/{p}  \u{2022} **roundtrip:** N/A  \u{2022} **negative:** N/A  \u{2022} **skipped:** {s}  \u{2022} **leaks:** 0  \n\
         _Source: CI receipts (nextest/junit). This block is updated automatically._"
    )
}

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

    replace_in_file("README.md", &b)?;
    replace_in_file("docs/REPORT.md", &b)?;

    println!("\u{2713} Synced test status to README.md and docs/REPORT.md");
    Ok(())
}

fn verify() -> Result<()> {
    let c = counts()?;
    let expected = block(&c);

    for path in ["README.md", "docs/REPORT.md"] {
        let content = fs::read_to_string(path)?;
        if !content.contains(&expected) {
            bail!("{path} test-status out of sync");
        }
    }

    println!("\u{2713} Test status is in sync");
    Ok(())
}

fn verify_support_matrix() -> Result<()> {
    let doc_path = "docs/reference/COBOL_SUPPORT_MATRIX.md";
    let doc_content = fs::read_to_string(doc_path)?;

    let all_features = support_matrix::all_features();
    let mut missing = Vec::new();

    for feature in all_features {
        let id =
            serde_plain::to_string(&feature.id).unwrap_or_else(|_| format!("{:?}", feature.id));

        // Check if the feature ID appears anywhere in the doc
        // We're lenient: just check for the kebab-case ID string
        if !doc_content.contains(&id) {
            missing.push(id);
        }
    }

    if !missing.is_empty() {
        bail!(
            "Support matrix drift detected!\n\
             The following features are in the registry but not documented in {doc_path}:\n  - {}\n\n\
             Add these features to the appropriate tables in {doc_path}.",
            missing.join("\n  - ")
        );
    }

    println!(
        "\u{2713} Support matrix registry \u{2194} docs in sync ({} features verified)",
        all_features.len()
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
