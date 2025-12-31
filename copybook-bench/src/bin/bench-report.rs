//! Local benchmark reporting CLI tool for Issue #52
//!
//! Provides local development tools for baseline management and performance
//! reporting without requiring full CI/CD infrastructure.

use anyhow::{Context, Result};
use copybook_bench::{baseline::BaselineStore, reporting::PerformanceReport};
use serde_json::Value;
use std::env;
use std::path::PathBuf;

const DISPLAY_FLOOR_MIBPS: f64 = 80.0;
const COMP3_FLOOR_MIBPS: f64 = 40.0;
const DISPLAY_FLOOR_GIBPS: f64 = DISPLAY_FLOOR_MIBPS / 1024.0;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage(&args[0]);
        return Ok(());
    }

    match args[1].as_str() {
        "validate" => validate_report(&args)?,
        "baseline" => manage_baseline(&args)?,
        "compare" => compare_performance(&args)?,
        "summary" => show_summary(&args),
        "help" | "--help" => print_usage(&args[0]),
        _ => {
            eprintln!("Unknown command: {}", args[1]);
            print_usage(&args[0]);
        }
    }

    Ok(())
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
    println!("    summary                        Show baseline and SLO status");
    println!("    help                          Show this help message");
    println!();
    println!("EXAMPLES:");
    println!("    {program} validate perf.json");
    println!("    {program} baseline promote perf.json");
    println!("    {program} compare perf.json");
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
    report.validate_slos(DISPLAY_FLOOR_GIBPS, COMP3_FLOOR_MIBPS);

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
        println!("   COMP-3:  ≥{COMP3_FLOOR_MIBPS:.0} MiB/s");
        println!();
        println!("📈 Performance History: {} entries", store.history.len());
        println!("   Baseline file: {}", baseline_path.display());
    } else {
        println!("📊 No baseline established");
        println!();
        println!("🎯 SLO Targets:");
        println!("   DISPLAY: ≥{DISPLAY_FLOOR_MIBPS:.0} MiB/s");
        println!("   COMP-3:  ≥{COMP3_FLOOR_MIBPS:.0} MiB/s");
        println!();
        println!("📈 Performance History: 0 entries");
        println!("   Baseline file: {} (not found)", baseline_path.display());
    }
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
    let is_temp_dir = current_dir.to_string_lossy().contains("/tmp")
        || current_dir.to_string_lossy().contains("tmp.")
        || current_dir.to_string_lossy().contains("/var/folders")  // macOS temp
        || current_dir.to_string_lossy().contains("/Temp")  // Windows temp
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
