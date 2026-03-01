// SPDX-License-Identifier: AGPL-3.0-or-later
//! Tests for xtask automation tool: task discovery, argument parsing,
//! help text, SLO evaluation, and perf receipt handling.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::float_cmp)]

use xtask::perf::{
    COMP3_SLO_MIBPS, DISPLAY_SLO_MIBPS, PerfSnapshot, SloStatus, evaluate_slo, format_slo_summary,
    parse_perf_receipt,
};

/// Helper to get the xtask binary path via the non-deprecated macro.
fn xtask_bin() -> std::path::PathBuf {
    assert_cmd::cargo::cargo_bin!("xtask").to_path_buf()
}

// ====================================================================
// Task discovery and listing
// ====================================================================

/// The xtask binary exists and can be located via cargo.
#[test]
fn xtask_binary_exists() {
    let bin = xtask_bin();
    assert!(bin.exists(), "xtask binary not found at {}", bin.display());
}

/// Running xtask with no arguments prints usage and exits successfully.
#[test]
fn no_args_prints_usage_and_succeeds() {
    let output = std::process::Command::new(xtask_bin())
        .output()
        .expect("failed to run xtask");

    // No-args should succeed (exit 0) and print usage to stderr
    assert!(output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Usage:"),
        "expected usage text, got: {stderr}"
    );
}

/// Usage text lists all known subcommands.
#[test]
fn usage_lists_docs_sync_tests() {
    let output = std::process::Command::new(xtask_bin())
        .output()
        .expect("failed to run xtask");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("docs sync-tests"));
}

#[test]
fn usage_lists_docs_verify_tests() {
    let output = std::process::Command::new(xtask_bin())
        .output()
        .expect("failed to run xtask");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("docs verify-tests"));
}

#[test]
fn usage_lists_docs_verify_support_matrix() {
    let output = std::process::Command::new(xtask_bin())
        .output()
        .expect("failed to run xtask");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("docs verify-support-matrix"));
}

#[test]
fn usage_lists_perf() {
    let output = std::process::Command::new(xtask_bin())
        .output()
        .expect("failed to run xtask");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("perf "));
}

#[test]
fn usage_lists_perf_enforce() {
    let output = std::process::Command::new(xtask_bin())
        .output()
        .expect("failed to run xtask");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("perf --enforce"));
}

#[test]
fn usage_lists_perf_summarize_last() {
    let output = std::process::Command::new(xtask_bin())
        .output()
        .expect("failed to run xtask");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("perf --summarize-last"));
}

// ====================================================================
// Valid task name parsing (via binary invocation)
// ====================================================================

/// Unknown subcommand does not panic — exits cleanly with usage.
#[test]
fn unknown_subcommand_no_panic() {
    let output = std::process::Command::new(xtask_bin())
        .args(["not-a-real-subcommand"])
        .output()
        .expect("failed to run xtask");

    // Should succeed (prints usage) — not crash
    assert!(output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Usage:"));
}

/// Unknown nested subcommand does not panic.
#[test]
fn unknown_nested_subcommand_no_panic() {
    let output = std::process::Command::new(xtask_bin())
        .args(["docs", "not-a-real-subcommand"])
        .output()
        .expect("failed to run xtask");

    assert!(output.status.success());
}

/// Empty string argument does not panic.
#[test]
fn empty_arg_no_panic() {
    let output = std::process::Command::new(xtask_bin())
        .args([""])
        .output()
        .expect("failed to run xtask");

    assert!(output.status.success());
}

/// Multiple unknown arguments do not panic.
#[test]
fn multiple_unknown_args_no_panic() {
    let output = std::process::Command::new(xtask_bin())
        .args(["foo", "bar", "baz", "--quux"])
        .output()
        .expect("failed to run xtask");

    assert!(output.status.success());
}

// ====================================================================
// Perf receipt parsing — flat format
// ====================================================================

#[test]
fn parse_flat_receipt_ok() {
    let json = r#"{"display_mibps": 205.0, "comp3_mibps": 58.0}"#;
    let snap = parse_perf_receipt(json).unwrap();
    assert_eq!(snap.display_mibps, 205.0);
    assert_eq!(snap.comp3_mibps, 58.0);
}

#[test]
fn parse_nested_receipt_ok() {
    let json = r#"{"summary": {"display_mibps": 100.0, "comp3_mibps": 50.0}}"#;
    let snap = parse_perf_receipt(json).unwrap();
    assert_eq!(snap.display_mibps, 100.0);
    assert_eq!(snap.comp3_mibps, 50.0);
}

/// Flat keys win over nested (flat is tried first).
#[test]
fn parse_receipt_flat_takes_precedence() {
    let json = r#"{"display_mibps": 300.0, "comp3_mibps": 70.0, "summary": {"display_mibps": 100.0, "comp3_mibps": 20.0}}"#;
    let snap = parse_perf_receipt(json).unwrap();
    assert_eq!(snap.display_mibps, 300.0);
    assert_eq!(snap.comp3_mibps, 70.0);
}

#[test]
fn parse_receipt_missing_display_errs() {
    let json = r#"{"comp3_mibps": 58.0}"#;
    assert!(parse_perf_receipt(json).is_err());
}

#[test]
fn parse_receipt_missing_comp3_errs() {
    let json = r#"{"display_mibps": 200.0}"#;
    assert!(parse_perf_receipt(json).is_err());
}

#[test]
fn parse_receipt_malformed_json_errs() {
    assert!(parse_perf_receipt("{invalid").is_err());
}

#[test]
fn parse_receipt_empty_string_errs() {
    assert!(parse_perf_receipt("").is_err());
}

#[test]
fn parse_receipt_negative_display_errs() {
    let json = r#"{"display_mibps": -1.0, "comp3_mibps": 50.0}"#;
    assert!(parse_perf_receipt(json).is_err());
}

#[test]
fn parse_receipt_negative_comp3_errs() {
    let json = r#"{"display_mibps": 100.0, "comp3_mibps": -1.0}"#;
    assert!(parse_perf_receipt(json).is_err());
}

#[test]
fn parse_receipt_zero_values_ok() {
    let json = r#"{"display_mibps": 0.0, "comp3_mibps": 0.0}"#;
    let snap = parse_perf_receipt(json).unwrap();
    assert_eq!(snap.display_mibps, 0.0);
    assert_eq!(snap.comp3_mibps, 0.0);
}

// ====================================================================
// SLO evaluation
// ====================================================================

#[test]
fn slo_pass_when_both_above_threshold() {
    let snap = PerfSnapshot {
        display_mibps: DISPLAY_SLO_MIBPS + 10.0,
        comp3_mibps: COMP3_SLO_MIBPS + 10.0,
    };
    assert_eq!(evaluate_slo(&snap), SloStatus::Pass);
}

#[test]
fn slo_pass_at_exact_threshold() {
    let snap = PerfSnapshot {
        display_mibps: DISPLAY_SLO_MIBPS,
        comp3_mibps: COMP3_SLO_MIBPS,
    };
    assert_eq!(evaluate_slo(&snap), SloStatus::Pass);
}

#[test]
fn slo_fail_when_display_below() {
    let snap = PerfSnapshot {
        display_mibps: DISPLAY_SLO_MIBPS - 1.0,
        comp3_mibps: COMP3_SLO_MIBPS + 10.0,
    };
    assert!(matches!(evaluate_slo(&snap), SloStatus::Fail { .. }));
}

#[test]
fn slo_fail_when_comp3_below() {
    let snap = PerfSnapshot {
        display_mibps: DISPLAY_SLO_MIBPS + 10.0,
        comp3_mibps: COMP3_SLO_MIBPS - 1.0,
    };
    assert!(matches!(evaluate_slo(&snap), SloStatus::Fail { .. }));
}

#[test]
fn slo_fail_when_both_below() {
    let snap = PerfSnapshot {
        display_mibps: 10.0,
        comp3_mibps: 5.0,
    };
    assert!(matches!(evaluate_slo(&snap), SloStatus::Fail { .. }));
}

#[test]
fn slo_fail_returns_negative_deltas() {
    let snap = PerfSnapshot {
        display_mibps: DISPLAY_SLO_MIBPS / 2.0,
        comp3_mibps: COMP3_SLO_MIBPS / 2.0,
    };
    match evaluate_slo(&snap) {
        SloStatus::Fail {
            display_delta_pct,
            comp3_delta_pct,
        } => {
            assert!(display_delta_pct < 0.0);
            assert!(comp3_delta_pct < 0.0);
        }
        SloStatus::Pass => panic!("expected Fail"),
    }
}

// ====================================================================
// SLO summary formatting
// ====================================================================

#[test]
fn format_summary_pass_contains_checkmark() {
    let snap = PerfSnapshot {
        display_mibps: 200.0,
        comp3_mibps: 60.0,
    };
    let status = evaluate_slo(&snap);
    let summary = format_slo_summary(&snap, &status);
    assert!(summary.contains("✓"));
}

#[test]
fn format_summary_fail_contains_warning() {
    let snap = PerfSnapshot {
        display_mibps: 10.0,
        comp3_mibps: 5.0,
    };
    let status = evaluate_slo(&snap);
    let summary = format_slo_summary(&snap, &status);
    assert!(summary.contains("⚠"));
}

#[test]
fn format_summary_contains_slo_thresholds() {
    let snap = PerfSnapshot {
        display_mibps: 100.0,
        comp3_mibps: 50.0,
    };
    let status = evaluate_slo(&snap);
    let summary = format_slo_summary(&snap, &status);
    assert!(summary.contains(&format!("{}", DISPLAY_SLO_MIBPS)));
    assert!(summary.contains(&format!("{}", COMP3_SLO_MIBPS)));
}

#[test]
fn format_summary_shows_positive_delta_with_plus() {
    let snap = PerfSnapshot {
        display_mibps: 200.0,
        comp3_mibps: 80.0,
    };
    let status = evaluate_slo(&snap);
    let summary = format_slo_summary(&snap, &status);
    assert!(summary.contains('+'));
}

// ====================================================================
// SLO constants are reasonable
// ====================================================================

#[test]
fn display_slo_is_positive() {
    assert!(DISPLAY_SLO_MIBPS > 0.0);
}

#[test]
fn comp3_slo_is_positive() {
    assert!(COMP3_SLO_MIBPS > 0.0);
}

#[test]
fn display_slo_greater_than_comp3() {
    assert!(DISPLAY_SLO_MIBPS > COMP3_SLO_MIBPS);
}

// ====================================================================
// PerfSnapshot equality
// ====================================================================

#[test]
fn perf_snapshot_eq() {
    let a = PerfSnapshot {
        display_mibps: 100.0,
        comp3_mibps: 50.0,
    };
    let b = PerfSnapshot {
        display_mibps: 100.0,
        comp3_mibps: 50.0,
    };
    assert_eq!(a, b);
}

#[test]
fn perf_snapshot_ne() {
    let a = PerfSnapshot {
        display_mibps: 100.0,
        comp3_mibps: 50.0,
    };
    let b = PerfSnapshot {
        display_mibps: 100.0,
        comp3_mibps: 60.0,
    };
    assert_ne!(a, b);
}

#[test]
fn perf_snapshot_clone() {
    let a = PerfSnapshot {
        display_mibps: 42.0,
        comp3_mibps: 24.0,
    };
    let b = a.clone();
    assert_eq!(a, b);
}

#[test]
fn perf_snapshot_debug() {
    let snap = PerfSnapshot {
        display_mibps: 100.0,
        comp3_mibps: 50.0,
    };
    let dbg = format!("{snap:?}");
    assert!(dbg.contains("PerfSnapshot"));
    assert!(dbg.contains("100"));
}

#[test]
fn slo_status_debug() {
    let pass = SloStatus::Pass;
    assert!(format!("{pass:?}").contains("Pass"));

    let fail = SloStatus::Fail {
        display_delta_pct: -10.0,
        comp3_delta_pct: -5.0,
    };
    assert!(format!("{fail:?}").contains("Fail"));
}

// ====================================================================
// perf --summarize-last with missing receipt
// ====================================================================

#[test]
fn summarize_last_fails_when_no_receipt_exists() {
    let temp = tempfile::TempDir::new().unwrap();
    let output = std::process::Command::new(xtask_bin())
        .args(["perf", "--summarize-last"])
        .current_dir(temp.path())
        .output()
        .expect("failed to run xtask");

    // Should fail because no perf.json exists
    assert!(!output.status.success());
}
