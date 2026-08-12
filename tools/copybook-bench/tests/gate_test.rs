// SPDX-License-Identifier: AGPL-3.0-or-later
//! Tests for the blocking perf gate logic (`bench-report gate`).
//!
//! The gate's pure evaluation logic lives in `copybook_bench::slo` and is
//! exercised here directly. The binary's IO/exit-code wiring is thin and
//! mirrors these cases one-to-one.

#![allow(clippy::expect_used, clippy::unwrap_used, clippy::float_cmp)]

use copybook_bench::GateMetric;
use copybook_bench::evaluate_metric;
use copybook_bench::slo::{
    COMP3_CI_FLOOR_MIBPS, COMP3_FLOOR_MIBPS, DISPLAY_FLOOR_MIBPS, REGRESSION_THRESHOLD_PCT,
    evaluate_metric as eval,
};
use std::fs;
use std::process::Command;
use tempfile::tempdir;

const THRESHOLD: f64 = REGRESSION_THRESHOLD_PCT;

// ---------------------------------------------------------------------------
// (a) Passing gate: both metrics above floor and within tolerance of baseline
// ---------------------------------------------------------------------------

#[test]
fn gate_passes_when_above_floor_and_within_regression_tolerance() {
    let display = GateMetric {
        label: "DISPLAY",
        current: 200.0,
        baseline: Some(200.0),
    };
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 60.0,
        baseline: Some(60.0),
    };

    let d = evaluate_metric(&display, true, DISPLAY_FLOOR_MIBPS, THRESHOLD);
    let c = evaluate_metric(&comp3, true, COMP3_CI_FLOOR_MIBPS, THRESHOLD);

    assert!(!d.failed, "DISPLAY should pass: {}", d.reasons.join("; "));
    assert!(!c.failed, "COMP-3 should pass: {}", c.reasons.join("; "));
    // Delta should be ~0% (no regression).
    assert_eq!(d.delta_pct, Some(0.0));
    assert_eq!(c.delta_pct, Some(0.0));
}

// ---------------------------------------------------------------------------
// (b) DISPLAY absolute floor breach (no baseline needed)
// ---------------------------------------------------------------------------

#[test]
fn display_floor_breach_fails_without_baseline() {
    let display = GateMetric {
        label: "DISPLAY",
        current: 70.0,
        baseline: None,
    };
    let d = evaluate_metric(&display, true, DISPLAY_FLOOR_MIBPS, THRESHOLD);

    assert!(d.failed);
    assert!(d.reasons.iter().any(|r| r.contains("below absolute floor")));
    assert_eq!(d.floor_enforced, Some(DISPLAY_FLOOR_MIBPS));
}

// ---------------------------------------------------------------------------
// (c) DISPLAY regression > threshold vs baseline
// ---------------------------------------------------------------------------

#[test]
fn display_regression_beyond_threshold_fails() {
    // 10% slower than baseline 200 -> current 180, delta -10% < -5%.
    let display = GateMetric {
        label: "DISPLAY",
        current: 180.0,
        baseline: Some(200.0),
    };
    let d = evaluate_metric(&display, true, DISPLAY_FLOOR_MIBPS, THRESHOLD);

    assert!(d.failed);
    assert!(d.reasons.iter().any(|r| r.contains("regression")));
    assert_eq!(d.delta_pct, Some(-10.0));
}

#[test]
fn display_regression_within_threshold_passes() {
    // 4% slower than baseline 200 -> current 192, delta -4% > -5% threshold.
    let display = GateMetric {
        label: "DISPLAY",
        current: 192.0,
        baseline: Some(200.0),
    };
    let d = evaluate_metric(&display, true, DISPLAY_FLOOR_MIBPS, THRESHOLD);

    assert!(
        !d.failed,
        "within-threshold regression should not fail: {}",
        d.reasons.join("; ")
    );
}

// ---------------------------------------------------------------------------
// (d) COMP-3 absolute floor breach (CI-grounded floor of 8 MiB/s is enforced)
// ---------------------------------------------------------------------------

#[test]
fn comp3_floor_breach_fails_without_baseline() {
    // 5 MiB/s is below the CI floor of 8. No baseline needed to fail.
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 5.0,
        baseline: None,
    };
    let c = evaluate_metric(&comp3, true, COMP3_CI_FLOOR_MIBPS, THRESHOLD);

    assert!(c.failed);
    assert!(c.reasons.iter().any(|r| r.contains("below absolute floor")));
    assert_eq!(c.floor_enforced, Some(COMP3_CI_FLOOR_MIBPS));
}

// ---------------------------------------------------------------------------
// (e) COMP-3 regression > threshold vs baseline (floor enforced too)
// ---------------------------------------------------------------------------

#[test]
fn comp3_regression_beyond_threshold_fails() {
    // Current 10 vs baseline 20 = -50% regression (fails relative gate).
    // 10 is above the 8 floor, so the floor does not fire; only regression.
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 10.0,
        baseline: Some(20.0),
    };
    let c = evaluate_metric(&comp3, true, COMP3_CI_FLOOR_MIBPS, THRESHOLD);

    assert!(c.failed);
    assert!(c.reasons.iter().any(|r| r.contains("regression")));
    assert!(
        !c.reasons.iter().any(|r| r.contains("absolute floor")),
        "above-floor COMP-3 should not trip the absolute check"
    );
    assert_eq!(c.floor_enforced, Some(COMP3_CI_FLOOR_MIBPS));
}

// ---------------------------------------------------------------------------
// (f) Missing baseline: relative gate skipped, but absolute floor still holds
// ---------------------------------------------------------------------------

#[test]
fn missing_baseline_does_not_fail_when_above_floor() {
    // COMP-3 with no baseline and an above-floor value passes entirely
    // (floor satisfied, no baseline to compare for regression).
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 15.0,
        baseline: None,
    };
    let c = evaluate_metric(&comp3, true, COMP3_CI_FLOOR_MIBPS, THRESHOLD);

    assert!(
        !c.failed,
        "COMP-3 above floor with no baseline must pass: {}",
        c.reasons.join("; ")
    );
    assert!(c.reasons.is_empty());
    assert_eq!(c.delta_pct, None);
}

#[test]
fn missing_baseline_still_fails_on_floor_breach() {
    // No baseline, but below floor -> absolute gate still fires.
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 3.0,
        baseline: None,
    };
    let c = evaluate_metric(&comp3, true, COMP3_CI_FLOOR_MIBPS, THRESHOLD);
    assert!(c.failed);
    assert_eq!(c.delta_pct, None, "no baseline => no delta");
}

#[test]
fn zero_baseline_is_treated_as_missing() {
    // A zero/missing baseline value is skipped (graceful), not a failure —
    // provided the absolute floor is satisfied.
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 15.0,
        baseline: Some(0.0),
    };
    let c = evaluate_metric(&comp3, true, COMP3_CI_FLOOR_MIBPS, THRESHOLD);
    assert!(!c.failed);
}

// ---------------------------------------------------------------------------
// Boundary: regression exactly at the threshold should pass (strict <)
// ---------------------------------------------------------------------------

#[test]
fn regression_exactly_at_threshold_passes() {
    // 5% regression exactly -> delta == -threshold, not < -threshold.
    // current = baseline * 0.95. Use baseline 200, current 190.
    let display = GateMetric {
        label: "DISPLAY",
        current: 190.0,
        baseline: Some(200.0),
    };
    let d = evaluate_metric(&display, true, DISPLAY_FLOOR_MIBPS, 5.0);
    assert!(
        !d.failed,
        "regression exactly at threshold should pass: {}",
        d.reasons.join("; ")
    );
}

#[test]
fn absolute_floor_boundaries_pass() {
    let display = GateMetric {
        label: "DISPLAY",
        current: DISPLAY_FLOOR_MIBPS,
        baseline: None,
    };
    let comp3 = GateMetric {
        label: "COMP-3",
        current: COMP3_CI_FLOOR_MIBPS,
        baseline: None,
    };

    assert!(!evaluate_metric(&display, true, DISPLAY_FLOOR_MIBPS, THRESHOLD).failed);
    assert!(!evaluate_metric(&comp3, true, COMP3_CI_FLOOR_MIBPS, THRESHOLD).failed);
}

#[test]
fn gate_cli_fails_closed_for_missing_and_malformed_receipts() {
    let temp = tempdir().expect("create gate fixture directory");
    let missing = temp.path().join("missing.json");
    let malformed = temp.path().join("malformed.json");
    fs::write(&malformed, "{not-json").expect("write malformed gate fixture");

    for receipt in [&missing, &malformed] {
        let status = Command::new(env!("CARGO_BIN_EXE_bench-report"))
            .args(["gate", receipt.to_str().expect("fixture path is UTF-8")])
            .status()
            .expect("run bench-report gate");
        assert!(!status.success(), "invalid receipt must fail closed");
    }
}

#[test]
fn gate_cli_exit_status_matches_absolute_floor_decision() {
    let temp = tempdir().expect("create gate fixture directory");
    let passing = temp.path().join("passing.json");
    let failing = temp.path().join("failing.json");
    fs::write(&passing, r#"{"display_mibps":80.0,"comp3_mibps":8.0}"#)
        .expect("write passing gate fixture");
    fs::write(&failing, r#"{"display_mibps":79.9,"comp3_mibps":7.9}"#)
        .expect("write failing gate fixture");

    let pass_status = Command::new(env!("CARGO_BIN_EXE_bench-report"))
        .args(["gate", passing.to_str().expect("fixture path is UTF-8")])
        .status()
        .expect("run passing bench-report gate fixture");
    let fail_status = Command::new(env!("CARGO_BIN_EXE_bench-report"))
        .args(["gate", failing.to_str().expect("fixture path is UTF-8")])
        .status()
        .expect("run failing bench-report gate fixture");

    assert!(pass_status.success(), "exact floor boundary must pass");
    assert!(!fail_status.success(), "below-floor receipt must fail");
}

// ---------------------------------------------------------------------------
// Improvement is never a failure
// ---------------------------------------------------------------------------

#[test]
fn improvement_does_not_fail() {
    let display = GateMetric {
        label: "DISPLAY",
        current: 300.0,
        baseline: Some(200.0),
    };
    let d = evaluate_metric(&display, true, DISPLAY_FLOOR_MIBPS, THRESHOLD);
    assert!(!d.failed);
    assert_eq!(d.delta_pct, Some(50.0));
}

// ---------------------------------------------------------------------------
// Constants sanity (guard against accidental drift)
// ---------------------------------------------------------------------------

#[test]
fn slo_constants_match_documented_values() {
    assert_eq!(DISPLAY_FLOOR_MIBPS, 80.0);
    // Reference-hardware value, retained for documentation (not enforced).
    assert_eq!(COMP3_FLOOR_MIBPS, 40.0);
    // CI-grounded enforced floor.
    assert_eq!(COMP3_CI_FLOOR_MIBPS, 8.0);
    assert_eq!(REGRESSION_THRESHOLD_PCT, 5.0);
}

// ---------------------------------------------------------------------------
// Ensure the aliased re-export and direct path agree (regression guard
// against a future refactor that moves one but not the other).
// ---------------------------------------------------------------------------

#[test]
fn reexported_evaluate_metric_matches_direct_path() {
    let m = GateMetric {
        label: "DISPLAY",
        current: 100.0,
        baseline: Some(100.0),
    };
    let via_reexport = evaluate_metric(&m, true, DISPLAY_FLOOR_MIBPS, THRESHOLD);
    let via_path = eval(&m, true, DISPLAY_FLOOR_MIBPS, THRESHOLD);
    assert_eq!(via_reexport.failed, via_path.failed);
    assert_eq!(via_reexport.delta_pct, via_path.delta_pct);
}
