// SPDX-License-Identifier: AGPL-3.0-or-later
//! Local benchmark reporting CLI tool for Issue #52
//!
//! Provides local development tools for baseline management and performance
//! reporting without requiring full CI/CD infrastructure.

use anyhow::{Context, Result};
use copybook_bench::{
    GateMetric,
    baseline::BaselineStore,
    evaluate_metric,
    reporting::PerformanceReport,
    slo::{COMP3_CI_FLOOR_MIBPS, COMP3_FLOOR_MIBPS, DISPLAY_FLOOR_MIBPS, REGRESSION_THRESHOLD_PCT},
};
use serde_json::Value;
use std::env;
use std::path::PathBuf;
use std::process::ExitCode;

fn main() -> Result<ExitCode> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage(&args[0]);
        return Ok(ExitCode::SUCCESS);
    }

    match args[1].as_str() {
        "validate" => validate_report(&args)?,
        "baseline" => manage_baseline(&args)?,
        "compare" => compare_performance(&args)?,
        "gate" => return run_gate(&args),
        "summary" => show_summary(&args),
        "help" | "--help" => print_usage(&args[0]),
        _ => {
            eprintln!("Unknown command: {}", args[1]);
            print_usage(&args[0]);
        }
    }

    Ok(ExitCode::SUCCESS)
}

fn print_usage(program: &str) {
    println!("copybook-rs benchmark reporting tool for Issue #52");
    println!();
    println!("USAGE:");
    println!("    {program} <COMMAND> [OPTIONS]");
    println!();
    println!("COMMANDS:");
    println!("    validate <perf.json>           Validate performance report JSON");
    println!("    baseline promote <perf.json>   Promote report to main baseline");
    println!("    baseline show                  Show current baseline");
    println!("    compare <perf.json>            Compare against baseline");
    println!("    gate <perf.json> [opts]        Enforce perf gates (non-zero exit on failure)");
    println!("    summary                        Show baseline and SLO status");
    println!("    help                          Show this help message");
    println!();
    println!("EXAMPLES:");
    println!("    {program} validate perf.json");
    println!("    {program} baseline promote perf.json");
    println!("    {program} compare perf.json");
    println!("    {program} gate perf.json --baseline baseline.json");
}

fn validate_report(args: &[String]) -> Result<()> {
    if args.len() < 3 {
        eprintln!("Usage: {} validate <perf.json>", args[0]);
        print_usage(&args[0]);
        return Ok(());
    }

    let report_path = &args[2];
    let content = std::fs::read_to_string(report_path)
        .with_context(|| format!("Failed to read {report_path}"))?;

    let value: Value = serde_json::from_str(&content)
        .with_context(|| format!("Failed to parse {report_path} as JSON"))?;
    let mut report =
        parse_report_from_value(report_path, &value).context("parsing performance report")?;

    // Validate against SLOs
    report.validate_slos(DISPLAY_FLOOR_MIBPS / 1024.0, COMP3_FLOOR_MIBPS);

    println!("✅ Valid performance report");
    println!("   Status: {}", report.status);
    if let Some(display) = report.display_gibs {
        println!("   DISPLAY: {display:.2} GiB/s");
    }
    if let Some(comp3) = report.comp3_mibs {
        println!("   COMP-3: {comp3:.0} MiB/s");
    }

    if !report.warnings.is_empty() {
        println!("⚠️  Warnings:");
        for warning in &report.warnings {
            println!("   {warning}");
        }
    }

    if !report.errors.is_empty() {
        println!("❌ Errors:");
        for error in &report.errors {
            println!("   {error}");
        }
    }

    Ok(())
}

fn manage_baseline(args: &[String]) -> Result<()> {
    if args.len() < 3 {
        eprintln!("Usage: {} baseline <promote|show> [perf.json]", args[0]);
        print_usage(&args[0]);
        return Ok(());
    }

    // Diagnostic logging for test failure investigation
    let current_dir = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let is_temp_dir = current_dir.to_string_lossy().contains("/tmp");
    let cargo_manifest_dir = env::var("CARGO_MANIFEST_DIR");

    eprintln!("[DEBUG] Current directory: {}", current_dir.display());
    eprintln!("[DEBUG] Is temp dir (contains /tmp): {is_temp_dir}");
    eprintln!("[DEBUG] CARGO_MANIFEST_DIR: {cargo_manifest_dir:?}");

    let baseline_path = get_baseline_path();

    eprintln!("[DEBUG] Baseline path: {}", baseline_path.display());

    match args[2].as_str() {
        "promote" => {
            if args.len() < 4 {
                eprintln!("Usage: {} baseline promote <perf.json>", args[0]);
                print_usage(&args[0]);
                return Ok(());
            }

            let report_path = &args[3];
            let content = std::fs::read_to_string(report_path)
                .with_context(|| format!("Failed to read {report_path}"))?;

            let value: Value = serde_json::from_str(&content)
                .with_context(|| format!("Failed to parse {report_path} as JSON"))?;
            let report = parse_report_from_value(report_path, &value)?;

            // Load store with error handling
            let mut store = BaselineStore::load_or_create(&baseline_path)?;
            let commit = report.commit.clone();
            store.promote_baseline(&report, "main", &commit);
            store.save(&baseline_path)?;

            println!("✅ Promoted baseline: {}", store.summary());
        }
        "show" => {
            // Validate no extra arguments
            if args.len() > 3 {
                eprintln!("Error: 'baseline show' does not accept extra arguments");
                print_usage(&args[0]);
                return Ok(());
            }

            // Handle gracefully when baseline doesn't exist
            if let Ok(store) = BaselineStore::load_or_create(&baseline_path) {
                println!("📊 {}", store.summary());
                println!("   Baseline file: {}", baseline_path.display());
                println!("   History entries: {}", store.history.len());
            } else {
                println!("📊 No baseline established");
                println!("   Baseline file: {} (not found)", baseline_path.display());
            }
        }
        _ => {
            eprintln!("Unknown baseline command: {}", args[2]);
            print_usage(&args[0]);
        }
    }

    Ok(())
}

fn compare_performance(args: &[String]) -> Result<()> {
    if args.len() < 3 {
        eprintln!("Usage: {} compare <perf.json>", args[0]);
        print_usage(&args[0]);
        return Ok(());
    }

    let report_path = &args[2];
    let content = std::fs::read_to_string(report_path)
        .with_context(|| format!("Failed to read {report_path}"))?;

    let value: Value = serde_json::from_str(&content)
        .with_context(|| format!("Failed to parse {report_path} as JSON"))?;
    let report = parse_report_from_value(report_path, &value)?;

    let baseline_path = get_baseline_path();

    // Handle gracefully when baseline doesn't exist
    if let Ok(store) = BaselineStore::load_or_create(&baseline_path) {
        let regressions = store.check_regression(&report, 5.0); // 5% threshold

        println!("📊 Performance Comparison");
        println!("   {}", store.summary());
        println!("   Current: {}", report.format_pr_summary());

        if regressions.is_empty() {
            println!("✅ No performance regressions detected");
        } else {
            println!("❌ Performance regressions detected:");
            for regression in regressions {
                println!("   {regression}");
            }
        }
    } else {
        println!("📊 Performance Comparison");
        println!("   No baseline established");
        println!("   Current: {}", report.format_pr_summary());
        println!("⚠️  Cannot detect regressions without baseline");
    }

    Ok(())
}

fn show_summary(args: &[String]) {
    // Validate no extra arguments
    if args.len() > 2 {
        eprintln!("Error: 'summary' does not accept arguments");
        print_usage(&args[0]);
        return;
    }

    let baseline_path = get_baseline_path();

    println!("copybook-rs Performance Summary");
    println!("==============================");
    println!();

    // Handle gracefully when baseline doesn't exist
    if let Ok(store) = BaselineStore::load_or_create(&baseline_path) {
        println!("📊 {}", store.summary());
        println!();
        println!("🎯 SLO Targets:");
        println!("   DISPLAY: ≥{DISPLAY_FLOOR_MIBPS:.0} MiB/s");
        println!("   COMP-3:  ≥{COMP3_CI_FLOOR_MIBPS:.0} MiB/s (CI)");
        println!();
        println!("📈 Performance History: {} entries", store.history.len());
        println!("   Baseline file: {}", baseline_path.display());
    } else {
        println!("📊 No baseline established");
        println!();
        println!("🎯 SLO Targets:");
        println!("   DISPLAY: ≥{DISPLAY_FLOOR_MIBPS:.0} MiB/s");
        println!("   COMP-3:  ≥{COMP3_CI_FLOOR_MIBPS:.0} MiB/s (CI)");
        println!();
        println!("📈 Performance History: 0 entries");
        println!("   Baseline file: {} (not found)", baseline_path.display());
    }
}

/// Extract a throughput value in MiB/s from a JSON object, looking in common
/// locations across the receipt (`display_mibps`, `.summary.display_mibps`)
/// and baseline (`display_mibps`) shapes. Returns `None` if absent.
fn mibps_from(value: &Value, key: &str) -> Option<f64> {
    value.get(key).and_then(Value::as_f64).or_else(|| {
        value
            .get("summary")
            .and_then(|s| s.get(key))
            .and_then(Value::as_f64)
    })
}

/// `gate` subcommand: enforce perf floors + relative regression.
///
/// Exits non-zero when an absolute floor is breached (DISPLAY ≥80 MiB/s,
/// COMP-3 ≥8 MiB/s) or when either metric regresses beyond
/// `--regression-threshold` percent versus the baseline.
#[allow(clippy::too_many_lines)]
fn run_gate(args: &[String]) -> Result<ExitCode> {
    if args.len() < 3 {
        eprintln!(
            "Usage: {} gate <perf.json> [--baseline <baseline.json>] [--regression-threshold <pct>]",
            args[0]
        );
        return Ok(ExitCode::from(2));
    }

    let receipt_path = &args[2];
    let mut baseline_path: Option<String> = None;
    let mut threshold = REGRESSION_THRESHOLD_PCT;
    let mut i = 3;
    while i < args.len() {
        match args[i].as_str() {
            "--baseline" => {
                i += 1;
                baseline_path = args.get(i).map(String::as_str).map(str::to_owned);
            }
            "--regression-threshold" => {
                i += 1;
                if let Some(v) = args.get(i) {
                    threshold = v
                        .parse::<f64>()
                        .with_context(|| format!("invalid --regression-threshold value: {v}"))?;
                }
            }
            other => {
                eprintln!("gate: unknown option '{other}'");
                return Ok(ExitCode::from(2));
            }
        }
        i += 1;
    }
    if threshold < 0.0 {
        eprintln!("gate: --regression-threshold must be non-negative");
        return Ok(ExitCode::from(2));
    }

    // Parse receipt (raw MiB/s — avoids the lossy /1024 GiB/s conversion).
    let receipt_value: Value = serde_json::from_str(
        &std::fs::read_to_string(receipt_path)
            .with_context(|| format!("Failed to read receipt {receipt_path}"))?,
    )
    .with_context(|| format!("Failed to parse {receipt_path} as JSON"))?;
    let receipt_display = mibps_from(&receipt_value, "display_mibps");
    let receipt_comp3 = mibps_from(&receipt_value, "comp3_mibps");

    let (display_current, comp3_current) = match (receipt_display, receipt_comp3) {
        (Some(d), Some(c)) => (d, c),
        (None, _) => {
            eprintln!("gate: receipt {receipt_path} is missing display_mibps");
            return Ok(ExitCode::FAILURE);
        }
        (_, None) => {
            eprintln!("gate: receipt {receipt_path} is missing comp3_mibps");
            return Ok(ExitCode::FAILURE);
        }
    };

    // Parse optional baseline.
    let mut baseline_display = None;
    let mut baseline_comp3 = None;
    if let Some(path) = &baseline_path {
        let base_value: Value = serde_json::from_str(
            &std::fs::read_to_string(path)
                .with_context(|| format!("Failed to read baseline {path}"))?,
        )
        .with_context(|| format!("Failed to parse baseline {path} as JSON"))?;
        baseline_display = mibps_from(&base_value, "display_mibps");
        baseline_comp3 = mibps_from(&base_value, "comp3_mibps");
    }

    let display = GateMetric {
        label: "DISPLAY",
        current: display_current,
        baseline: baseline_display,
    };
    let comp3 = GateMetric {
        label: "COMP-3",
        current: comp3_current,
        baseline: baseline_comp3,
    };

    // Both metrics enforce their absolute floor plus the relative regression gate.
    let outcomes = [
        evaluate_metric(&display, true, DISPLAY_FLOOR_MIBPS, threshold),
        evaluate_metric(&comp3, true, COMP3_CI_FLOOR_MIBPS, threshold),
    ];

    let any_failed = print_gate_table(&outcomes);

    let summary = if baseline_path.is_some() {
        format!(
            "Perf gate: {} (DISPLAY floor ≥{:.0} MiB/s, COMP-3 floor ≥{:.0} MiB/s, regression threshold -{:.0}% vs baseline)",
            if any_failed { "FAILED" } else { "passed" },
            DISPLAY_FLOOR_MIBPS,
            COMP3_CI_FLOOR_MIBPS,
            threshold
        )
    } else {
        format!(
            "Perf gate: {} (DISPLAY floor ≥{:.0} MiB/s, COMP-3 floor ≥{:.0} MiB/s; no baseline — relative gate skipped)",
            if any_failed { "FAILED" } else { "passed" },
            DISPLAY_FLOOR_MIBPS,
            COMP3_CI_FLOOR_MIBPS,
        )
    };
    println!("{summary}");
    if let Ok(s) = env::var("GITHUB_STEP_SUMMARY")
        && !s.is_empty()
    {
        std::fs::write(s, format!("### {summary}\n"))?;
    }

    if any_failed {
        Ok(ExitCode::FAILURE)
    } else {
        Ok(ExitCode::SUCCESS)
    }
}

/// Print the per-metric gate results as a markdown table, emit GitHub Actions
/// `::error::` annotations for failures, and return whether any metric failed.
fn print_gate_table(outcomes: &[copybook_bench::GateOutcome]) -> bool {
    println!();
    println!("| Metric | Current | Floor | Baseline | Delta | Status |");
    println!("|--------|---------|-------|----------|-------|--------|");
    let mut any_failed = false;
    for o in outcomes {
        if o.failed {
            any_failed = true;
        }
        let floor = o
            .floor_enforced
            .map_or("—".to_string(), |f| format!("{f:.0}"));
        let base = o.baseline.map_or("—".to_string(), |b| format!("{b:.1}"));
        let delta = o.delta_pct.map_or("—".to_string(), |d| format!("{d:.2}%"));
        let status = if o.failed { "❌ FAIL" } else { "✅ pass" };
        println!(
            "| {} | {:.1} MiB/s | {} | {} MiB/s | {} | {} |",
            o.label, o.current, floor, base, delta, status
        );
        for reason in &o.reasons {
            eprintln!("::error::{reason}");
        }
    }
    println!();
    any_failed
}

fn parse_report_from_value(report_path: &str, value: &Value) -> Result<PerformanceReport> {
    if looks_like_perf_receipt(value) {
        Ok(perf_receipt_to_report(value))
    } else {
        serde_json::from_value(value.clone())
            .with_context(|| format!("Failed to parse {report_path} as a performance report"))
    }
}

fn looks_like_perf_receipt(value: &Value) -> bool {
    value.get("display_gibps").is_some()
        || value.get("display_mibps").is_some()
        || value.get("comp3_mibps").is_some()
        || value.get("summary").is_some()
}

fn perf_receipt_to_report(value: &Value) -> PerformanceReport {
    let mut report = PerformanceReport::new();

    if let Some(timestamp) = value.get("timestamp").and_then(Value::as_str) {
        report.timestamp = timestamp.to_string();
    }
    if let Some(commit) = value.get("commit").and_then(Value::as_str) {
        report.commit = commit.to_string();
    }
    if let Some(status) = value.get("status").and_then(Value::as_str) {
        report.status = match status {
            "pass" => "success",
            "fail" => "failure",
            "warn" => "warning",
            other => other,
        }
        .to_string();
    }

    report.display_gibs = value
        .get("display_gibps")
        .and_then(Value::as_f64)
        .or_else(|| {
            value
                .get("display_mibps")
                .and_then(Value::as_f64)
                .map(|mibps| mibps / 1024.0)
        })
        .or_else(|| {
            value
                .get("summary")
                .and_then(|summary| summary.get("display_mibps"))
                .and_then(Value::as_f64)
                .map(|mibps| mibps / 1024.0)
        });

    report.comp3_mibs = value
        .get("comp3_mibps")
        .and_then(Value::as_f64)
        .or_else(|| {
            value
                .get("summary")
                .and_then(|summary| summary.get("comp3_mibps"))
                .and_then(Value::as_f64)
        });

    report
}

fn get_baseline_path() -> PathBuf {
    let current_dir = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    // Check for temp directory using multiple indicators
    let current_dir_str = current_dir.to_string_lossy();
    let is_temp_dir = current_dir_str.contains("/tmp")
        || current_dir_str.contains("tmp.")
        || current_dir_str.contains("/var/folders")  // macOS temp
        || current_dir_str.contains("/Temp")  // Windows temp (forward slash)
        || current_dir_str.contains("\\Temp")  // Windows temp (backslash)
        || env::var("COPYBOOK_TEST_TEMP").is_ok(); // Explicit test flag

    if is_temp_dir {
        return PathBuf::from("baseline.json");
    }

    // Production scenario: use workspace target directory
    let workspace_root = env::var("CARGO_MANIFEST_DIR")
        .map_or_else(|_| PathBuf::from("."), PathBuf::from)
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .to_path_buf();

    workspace_root
        .join("target")
        .join("baselines")
        .join("performance.json")
}
