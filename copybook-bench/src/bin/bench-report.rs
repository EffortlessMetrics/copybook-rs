//! Local benchmark reporting CLI tool for Issue #52
//!
//! Provides local development tools for baseline management and performance
//! reporting without requiring full CI/CD infrastructure.

use anyhow::{Context, Result};
use copybook_bench::{baseline::BaselineStore, reporting::PerformanceReport};
use std::env;
use std::path::PathBuf;

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
        "summary" => show_summary(&args)?,
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
        return Ok(());
    }

    let report_path = &args[2];
    let content = std::fs::read_to_string(report_path)
        .with_context(|| format!("Failed to read {report_path}"))?;

    let mut report: PerformanceReport = serde_json::from_str(&content).with_context(|| {
        format!("Failed to parse {report_path} as valid performance report")
    })?;

    // Validate against SLOs
    report.validate_slos(4.1, 560.0);

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
        return Ok(());
    }

    let baseline_path = get_baseline_path();
    let mut store = BaselineStore::load_or_create(&baseline_path)?;

    match args[2].as_str() {
        "promote" => {
            if args.len() < 4 {
                eprintln!("Usage: {} baseline promote <perf.json>", args[0]);
                return Ok(());
            }

            let report_path = &args[3];
            let content = std::fs::read_to_string(report_path)
                .with_context(|| format!("Failed to read {report_path}"))?;

            let report: PerformanceReport = serde_json::from_str(&content)
                .with_context(|| format!("Failed to parse {report_path}"))?;

            let commit = report.commit.clone();
            store.promote_baseline(&report, "main", &commit);
            store.save(&baseline_path)?;

            println!("✅ Promoted baseline: {}", store.summary());
        }
        "show" => {
            println!("📊 {}", store.summary());
            println!("   Baseline file: {}", baseline_path.display());
            println!("   History entries: {}", store.history.len());
        }
        _ => {
            eprintln!("Unknown baseline command: {}", args[2]);
        }
    }

    Ok(())
}

fn compare_performance(args: &[String]) -> Result<()> {
    if args.len() < 3 {
        eprintln!("Usage: {} compare <perf.json>", args[0]);
        return Ok(());
    }

    let report_path = &args[2];
    let content = std::fs::read_to_string(report_path)
        .with_context(|| format!("Failed to read {report_path}"))?;

    let report: PerformanceReport = serde_json::from_str(&content)
        .with_context(|| format!("Failed to parse {report_path}"))?;

    let baseline_path = get_baseline_path();
    let store = BaselineStore::load_or_create(&baseline_path)?;

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

    Ok(())
}

fn show_summary(_args: &[String]) -> Result<()> {
    let baseline_path = get_baseline_path();
    let store = BaselineStore::load_or_create(&baseline_path)?;

    println!("copybook-rs Performance Summary");
    println!("==============================");
    println!();
    println!("📊 {}", store.summary());
    println!();
    println!("🎯 SLO Targets:");
    println!("   DISPLAY: ≥4.1 GiB/s");
    println!("   COMP-3:  ≥560 MiB/s");
    println!();
    println!("📈 Performance History: {} entries", store.history.len());
    println!("   Baseline file: {}", baseline_path.display());

    Ok(())
}

fn get_baseline_path() -> PathBuf {
    // Store baseline in project target directory
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
