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
    COMP3_FLOOR_MIBPS, DISPLAY_FLOOR_MIBPS, REGRESSION_THRESHOLD_PCT, evaluate_metric as eval,
};

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
    let c = evaluate_metric(&comp3, false, COMP3_FLOOR_MIBPS, THRESHOLD);

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
    // Floor is only enforced for DISPLAY.
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
// (d) COMP-3 regression > threshold vs baseline (relative gate still applies)
// ---------------------------------------------------------------------------

#[test]
fn comp3_regression_beyond_threshold_fails_even_though_below_floor() {
    // Current 10 MiB/s is below the (un-enforced) COMP-3 floor of 40, but that
    // floor is not enforced for COMP-3. The relative gate still catches a
    // 50% regression from baseline 20 -> current 10.
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 10.0,
        baseline: Some(20.0),
    };
    let c = evaluate_metric(&comp3, false, COMP3_FLOOR_MIBPS, THRESHOLD);

    assert!(c.failed);
    assert!(c.reasons.iter().any(|r| r.contains("regression")));
    assert!(
        !c.reasons.iter().any(|r| r.contains("absolute floor")),
        "COMP-3 must not enforce absolute floor"
    );
    assert_eq!(c.floor_enforced, None, "COMP-3 floor not enforced");
}

// ---------------------------------------------------------------------------
// (e) Missing baseline: relative gate skipped, never fails on absence
// ---------------------------------------------------------------------------

#[test]
fn missing_baseline_does_not_fail_relative_gate() {
    // DISPLAY below floor still fails the absolute check, but the *relative*
    // gate contributes no reason. COMP-3 with no baseline and below-floor
    // value passes entirely (floor unenforced, no baseline to compare).
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 5.0,
        baseline: None,
    };
    let c = evaluate_metric(&comp3, false, COMP3_FLOOR_MIBPS, THRESHOLD);

    assert!(
        !c.failed,
        "COMP-3 with no baseline must pass: {}",
        c.reasons.join("; ")
    );
    assert!(c.reasons.is_empty());
    assert_eq!(c.delta_pct, None);
}

#[test]
fn zero_baseline_is_treated_as_missing() {
    // A zero/missing baseline value is skipped (graceful), not a failure.
    let comp3 = GateMetric {
        label: "COMP-3",
        current: 5.0,
        baseline: Some(0.0),
    };
    let c = evaluate_metric(&comp3, false, COMP3_FLOOR_MIBPS, THRESHOLD);
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
    assert_eq!(COMP3_FLOOR_MIBPS, 40.0);
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
