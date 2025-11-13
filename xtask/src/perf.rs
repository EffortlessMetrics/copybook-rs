//! Performance benchmarks CI automation for Issue #66
//!
//! Orchestrates `copybook-bench --features perf` tests and emits machine-readable
//! JSON receipts to `target/benchmarks/` for CI integration and historical tracking.

use anyhow::{Context, Result};
use copybook_bench::PerformanceReport;
use std::path::PathBuf;
use std::process::Command;
use std::{env, fs};

/// Run performance benchmarks and emit JSON receipts
///
/// # Arguments
/// * `enforce` - If true, exit with error on SLO violations
/// * `out_dir` - Optional output directory (defaults to `target/benchmarks/<timestamp>`)
pub fn run(enforce: bool, out_dir: Option<&str>) -> Result<()> {
    eprintln!("🚀 Running performance benchmarks...");

    // Determine output directory
    let timestamp = chrono::Utc::now().format("%Y-%m-%dT%H-%M-%S").to_string();
    let out_path = if let Some(dir) = out_dir {
        PathBuf::from(dir)
    } else {
        PathBuf::from("target/benchmarks").join(&timestamp)
    };
    fs::create_dir_all(&out_path).context("creating benchmark output directory")?;

    // Collect metadata
    let git_sha = get_git_sha()?;
    let rustc_version = get_rustc_version()?;

    // Run perf tests
    eprintln!("📊 Running copybook-bench --features perf tests...");
    let test_output = Command::new("cargo")
        .args([
            "test",
            "-p",
            "copybook-bench",
            "--features",
            "perf",
            "--",
            "--nocapture",
        ])
        .output()
        .context("running copybook-bench perf tests")?;

    let stdout = String::from_utf8_lossy(&test_output.stdout);
    let _stderr = String::from_utf8_lossy(&test_output.stderr);

    // Parse throughput metrics from test output
    let display_throughput = parse_throughput(&stdout, "DISPLAY throughput:");
    let comp3_throughput = parse_throughput(&stdout, "COMP-3 throughput:");

    // Create performance report
    let mut report = PerformanceReport::new();
    report.display_gibs = display_throughput;
    report.comp3_mibs = comp3_throughput;
    report.commit = git_sha;
    report.timestamp = chrono::Utc::now().to_rfc3339();

    // Validate against SLOs (using current baseline targets from CLAUDE.md)
    // NOTE: These are advisory-only SLOs based on established baseline
    // (205 MiB/s DISPLAY, 58 MiB/s COMP-3 from commit 1fa63633)
    // CI enforces realistic floors: DISPLAY ≥80 MiB/s, COMP-3 ≥40 MiB/s
    report.validate_slos(4.1, 560.0);

    // Write report to output directory
    let report_path = out_path.join("perf.json");
    let report_json = serde_json::to_string_pretty(&report)?;
    fs::write(&report_path, report_json).context("writing performance report")?;

    // Write metadata
    let meta = serde_json::json!({
        "git_sha": report.commit,
        "rustc_version": rustc_version,
        "timestamp_utc": report.timestamp,
        "bench_crate": "copybook-bench",
        "runner_os": env::consts::OS,
        "cpu_count": num_cpus::get(),
    });
    let meta_path = out_path.join("meta.json");
    fs::write(&meta_path, serde_json::to_string_pretty(&meta)?)?;

    // Print summary
    eprintln!("\n✅ Benchmark receipts written:");
    eprintln!("   {}", report_path.display());
    eprintln!("   {}", meta_path.display());
    eprintln!();
    eprintln!("📊 {}", report.format_pr_summary());

    if !report.warnings.is_empty() {
        eprintln!("\n⚠️  Warnings:");
        for warning in &report.warnings {
            eprintln!("   {warning}");
        }
    }

    if !report.errors.is_empty() {
        eprintln!("\n❌ Errors:");
        for error in &report.errors {
            eprintln!("   {error}");
        }

        if enforce {
            anyhow::bail!("Performance SLO violations detected");
        }
    }

    Ok(())
}

/// Parse throughput value from test output
///
/// Looks for patterns like "DISPLAY throughput: 4.22 GiB/s" or "COMP-3 throughput: 571 MiB/s"
fn parse_throughput(output: &str, prefix: &str) -> Option<f64> {
    output
        .lines()
        .find(|line| line.contains(prefix))
        .and_then(|line| {
            // Extract the numeric value between the prefix and the unit
            let value_part = line.split(prefix).nth(1)?;
            let value_str = value_part.split_whitespace().next()?;
            value_str.parse::<f64>().ok()
        })
}

/// Get current git SHA (short form)
fn get_git_sha() -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .context("getting git SHA")?;

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Get rustc version
fn get_rustc_version() -> Result<String> {
    let output = Command::new("rustc")
        .args(["--version"])
        .output()
        .context("getting rustc version")?;

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_throughput() {
        let output = "DISPLAY throughput: 4.22 GiB/s (target: ≥4.1 GiB/s)";
        assert_eq!(parse_throughput(output, "DISPLAY throughput:"), Some(4.22));

        let output = "COMP-3 throughput: 571.50 MiB/s (target: ≥560 MiB/s)";
        assert_eq!(parse_throughput(output, "COMP-3 throughput:"), Some(571.50));
    }
}
