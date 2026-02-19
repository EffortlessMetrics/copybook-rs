// SPDX-License-Identifier: AGPL-3.0-or-later
//! Performance benchmarks CI automation and SLO evaluation
//!
//! Provides:
//! - Benchmark orchestration (`run`) for generating perf.json receipts
//! - Receipt parsing (`parse_perf_receipt`) for extracting metrics
//! - SLO evaluation (`evaluate_slo`) for validating performance targets

use anyhow::{Context, Result, bail};
use copybook_bench::PerformanceReport;
use serde_json::Value;
use std::path::PathBuf;
use std::process::Command;
use std::{env, fs};

/// Performance snapshot extracted from perf.json
#[derive(Debug, Clone, PartialEq)]
pub struct PerfSnapshot {
    pub display_mibps: f64,
    pub comp3_mibps: f64,
}

/// SLO evaluation result
#[derive(Debug, Clone, PartialEq)]
pub enum SloStatus {
    Pass,
    Fail {
        display_delta_pct: f64,
        comp3_delta_pct: f64,
    },
}

/// SLO thresholds (must match .github/workflows/perf.yml)
pub const DISPLAY_SLO_MIBPS: f64 = 80.0;
pub const COMP3_SLO_MIBPS: f64 = 40.0;

/// Run performance benchmarks and emit JSON receipts
///
/// # Arguments
/// * `enforce` - If true, exit with error on SLO violations
/// * `out_dir` - Optional output directory (defaults to `target/benchmarks/<timestamp>`)
///
/// # Errors
/// Returns error if benchmark execution fails, output directory creation fails,
/// or if `enforce` is true and SLO violations are detected
#[inline]
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

    // Validate against realistic floors (advisory by default; `--enforce` promotes to a hard gate)
    report.validate_slos(DISPLAY_SLO_MIBPS / 1024.0, COMP3_SLO_MIBPS);

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

/// Parse perf.json receipt into a snapshot
///
/// Handles both flat and nested summary structures:
/// - `{"display_mibps": 205.0, "comp3_mibps": 58.0}`
/// - `{"summary": {"display_mibps": 205.0, "comp3_mibps": 58.0}}`
///
/// # Errors
///
/// Returns error if JSON is malformed, missing required fields, or contains invalid throughput values.
#[inline]
#[allow(clippy::missing_errors_doc)]
pub fn parse_perf_receipt(json_content: &str) -> Result<PerfSnapshot> {
    let data: Value = serde_json::from_str(json_content)?;

    let display_mibps = data["display_mibps"]
        .as_f64()
        .or_else(|| data["summary"]["display_mibps"].as_f64())
        .ok_or_else(|| anyhow::anyhow!("Missing display_mibps in receipt"))?;

    let comp3_mibps = data["comp3_mibps"]
        .as_f64()
        .or_else(|| data["summary"]["comp3_mibps"].as_f64())
        .ok_or_else(|| anyhow::anyhow!("Missing comp3_mibps in receipt"))?;

    // Sanity check: throughput must be non-negative
    if display_mibps < 0.0 || comp3_mibps < 0.0 {
        bail!("Invalid throughput: display={display_mibps}, comp3={comp3_mibps}");
    }

    Ok(PerfSnapshot {
        display_mibps,
        comp3_mibps,
    })
}

/// Evaluate SLO compliance
///
/// Returns Pass if both metrics meet or exceed SLO thresholds,
/// otherwise Fail with percentage deltas.
#[inline]
#[must_use]
pub fn evaluate_slo(snapshot: &PerfSnapshot) -> SloStatus {
    let display_delta_pct =
        ((snapshot.display_mibps - DISPLAY_SLO_MIBPS) / DISPLAY_SLO_MIBPS) * 100.0;
    let comp3_delta_pct = ((snapshot.comp3_mibps - COMP3_SLO_MIBPS) / COMP3_SLO_MIBPS) * 100.0;

    if snapshot.display_mibps >= DISPLAY_SLO_MIBPS && snapshot.comp3_mibps >= COMP3_SLO_MIBPS {
        SloStatus::Pass
    } else {
        SloStatus::Fail {
            display_delta_pct,
            comp3_delta_pct,
        }
    }
}

/// Format SLO status for human consumption
#[inline]
#[must_use]
pub fn format_slo_summary(snapshot: &PerfSnapshot, status: &SloStatus) -> String {
    let display_delta_pct =
        ((snapshot.display_mibps - DISPLAY_SLO_MIBPS) / DISPLAY_SLO_MIBPS) * 100.0;
    let comp3_delta_pct = ((snapshot.comp3_mibps - COMP3_SLO_MIBPS) / COMP3_SLO_MIBPS) * 100.0;

    let display_delta_str = if display_delta_pct >= 0.0 {
        format!("+{display_delta_pct:.1}%")
    } else {
        format!("{display_delta_pct:.1}%")
    };

    let comp3_delta_str = if comp3_delta_pct >= 0.0 {
        format!("+{comp3_delta_pct:.1}%")
    } else {
        format!("{comp3_delta_pct:.1}%")
    };

    let mut lines = vec![format!(
        "DISPLAY: {:.1} MiB/s (SLO {} MiB/s, {}) | COMP-3: {:.1} MiB/s (SLO {} MiB/s, {})",
        snapshot.display_mibps,
        DISPLAY_SLO_MIBPS,
        display_delta_str,
        snapshot.comp3_mibps,
        COMP3_SLO_MIBPS,
        comp3_delta_str,
    )];

    match status {
        SloStatus::Pass => lines.push("✓ All SLOs met".to_string()),
        SloStatus::Fail { .. } => lines.push("⚠ SLOs not met".to_string()),
    }

    lines.join("\n")
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    // Tests from PR #152 (benchmark runner)
    #[test]
    fn test_parse_throughput() {
        let output = "DISPLAY throughput: 4.22 GiB/s (target: ≥4.1 GiB/s)";
        assert_eq!(parse_throughput(output, "DISPLAY throughput:"), Some(4.22));

        let output = "COMP-3 throughput: 571.50 MiB/s (target: ≥560 MiB/s)";
        assert_eq!(parse_throughput(output, "COMP-3 throughput:"), Some(571.50));
    }

    // Tests from PR #155 (SLO evaluation)
    #[test]
    fn test_parse_flat_receipt() {
        let json = r#"{"display_mibps": 205.0, "comp3_mibps": 58.0}"#;
        let snapshot = parse_perf_receipt(json).unwrap();
        assert_eq!(snapshot.display_mibps, 205.0);
        assert_eq!(snapshot.comp3_mibps, 58.0);
    }

    #[test]
    fn test_parse_nested_receipt() {
        let json = r#"{"summary": {"display_mibps": 205.0, "comp3_mibps": 58.0}}"#;
        let snapshot = parse_perf_receipt(json).unwrap();
        assert_eq!(snapshot.display_mibps, 205.0);
        assert_eq!(snapshot.comp3_mibps, 58.0);
    }

    #[test]
    fn test_parse_missing_display() {
        let json = r#"{"comp3_mibps": 58.0}"#;
        assert!(parse_perf_receipt(json).is_err());
    }

    #[test]
    fn test_parse_missing_comp3() {
        let json = r#"{"display_mibps": 205.0}"#;
        assert!(parse_perf_receipt(json).is_err());
    }

    #[test]
    fn test_parse_malformed_json() {
        let json = r#"{"invalid"#;
        assert!(parse_perf_receipt(json).is_err());
    }

    #[test]
    fn test_parse_negative_throughput() {
        let json = r#"{"display_mibps": -1.0, "comp3_mibps": 58.0}"#;
        assert!(parse_perf_receipt(json).is_err());
    }

    #[test]
    fn test_evaluate_slo_clearly_good() {
        let snapshot = PerfSnapshot {
            display_mibps: 200.0,
            comp3_mibps: 100.0,
        };
        assert_eq!(evaluate_slo(&snapshot), SloStatus::Pass);
    }

    #[test]
    fn test_evaluate_slo_exactly_on_floor() {
        let snapshot = PerfSnapshot {
            display_mibps: DISPLAY_SLO_MIBPS,
            comp3_mibps: COMP3_SLO_MIBPS,
        };
        assert_eq!(evaluate_slo(&snapshot), SloStatus::Pass);
    }

    #[test]
    fn test_evaluate_slo_display_just_under() {
        let snapshot = PerfSnapshot {
            display_mibps: DISPLAY_SLO_MIBPS - 0.1,
            comp3_mibps: COMP3_SLO_MIBPS + 10.0,
        };
        assert!(matches!(evaluate_slo(&snapshot), SloStatus::Fail { .. }));
    }

    #[test]
    fn test_evaluate_slo_comp3_just_under() {
        let snapshot = PerfSnapshot {
            display_mibps: DISPLAY_SLO_MIBPS + 10.0,
            comp3_mibps: COMP3_SLO_MIBPS - 0.1,
        };
        assert!(matches!(evaluate_slo(&snapshot), SloStatus::Fail { .. }));
    }

    #[test]
    fn test_evaluate_slo_both_under() {
        let snapshot = PerfSnapshot {
            display_mibps: DISPLAY_SLO_MIBPS - 10.0,
            comp3_mibps: COMP3_SLO_MIBPS - 5.0,
        };
        assert!(matches!(evaluate_slo(&snapshot), SloStatus::Fail { .. }));
    }

    #[test]
    fn test_delta_percentage_calculation() {
        let snapshot = PerfSnapshot {
            display_mibps: 88.0, // +10% over 80
            comp3_mibps: 36.0,   // -10% under 40
        };

        let status = evaluate_slo(&snapshot);
        if let SloStatus::Fail {
            display_delta_pct,
            comp3_delta_pct,
        } = status
        {
            assert!((display_delta_pct - 10.0).abs() < 0.01);
            assert!((comp3_delta_pct + 10.0).abs() < 0.01);
        } else {
            panic!("Expected Fail status");
        }
    }

    #[test]
    fn test_format_slo_summary_pass() {
        let snapshot = PerfSnapshot {
            display_mibps: 200.0,
            comp3_mibps: 100.0,
        };
        let status = evaluate_slo(&snapshot);
        let summary = format_slo_summary(&snapshot, &status);

        assert!(summary.contains("200.0 MiB/s"));
        assert!(summary.contains("100.0 MiB/s"));
        assert!(summary.contains("✓ All SLOs met"));
    }

    #[test]
    fn test_format_slo_summary_fail() {
        let snapshot = PerfSnapshot {
            display_mibps: 70.0,
            comp3_mibps: 30.0,
        };
        let status = evaluate_slo(&snapshot);
        let summary = format_slo_summary(&snapshot, &status);

        assert!(summary.contains("70.0 MiB/s"));
        assert!(summary.contains("30.0 MiB/s"));
        assert!(summary.contains("⚠ SLOs not met"));
    }
}
