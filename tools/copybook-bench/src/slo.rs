// SPDX-License-Identifier: AGPL-3.0-or-later
//! Performance SLO thresholds — the single canonical source of truth.
//!
//! The floor and regression values used across the bench harness, the
//! `bench-report` CLI, and the `perf-gate` CI workflow all derive from these
//! constants. Workflow/Python duplications exist historically; this module is
//! the canonical home and should be consulted when reconciling them.

/// Minimum DISPLAY-heavy decode throughput enforced by the absolute floor gate.
///
/// Measured in MiB/s. The CI SLO bench comfortably exceeds this (~1.5 GiB/s on
/// `ubuntu-latest`), so the floor has wide headroom. The `bench-report gate`
/// subcommand fails the build when a receipt reports below this value.
pub const DISPLAY_FLOOR_MIBPS: f64 = 80.0;

/// Minimum COMP-3-heavy decode throughput enforced by the absolute floor gate
/// on CI.
///
/// Measured in MiB/s. COMP-3 packed-decimal decode is throughput-bound at far
/// lower rates than DISPLAY: the SLO bench measures 12–14 MiB/s on
/// `ubuntu-latest` (and the per-record cost is fundamental, not a fixture-size
/// artifact — throughput *decreases* with larger payloads). This CI-grounded
/// floor is set ~35% below the worst observed CI measurement to absorb runner
/// variance while still catching a real (halving) regression.
pub const COMP3_CI_FLOOR_MIBPS: f64 = 8.0;

/// Reference-hardware COMP-3 baseline (58 MiB/s on WSL2 / Ryzen 9 9950X3D).
///
/// **Not enforced** as a gate. Retained for documentation and the
/// baseline/budget table in `ROADMAP.md`. The enforced floor is
/// [`COMP3_CI_FLOOR_MIBPS`], which reflects the canonical CI environment.
pub const COMP3_FLOOR_MIBPS: f64 = 40.0;

/// Maximum acceptable slowdown (percent) versus the committed baseline before
/// the relative-regression gate fails the build. Applies to both metrics.
///
/// Chosen to absorb normal CI runner variance while catching real regressions;
/// matches the historical threshold used in `perf.yml`'s comparison step.
pub const REGRESSION_THRESHOLD_PCT: f64 = 5.0;

/// A single throughput metric (MiB/s) under gate evaluation.
///
/// `current` is the measured PR run; `baseline` is the committed reference
/// (`None` when no baseline is available, e.g. a first run or nightly fallback).
#[derive(Clone, Copy, Debug)]
pub struct GateMetric {
    /// Human-readable label, e.g. `"DISPLAY"` / `"COMP-3"`.
    pub label: &'static str,
    /// Measured throughput in MiB/s.
    pub current: f64,
    /// Committed baseline throughput in MiB/s, if any.
    pub baseline: Option<f64>,
}

/// Outcome of evaluating one metric.
#[derive(Clone, Debug)]
pub struct GateOutcome {
    /// Label of the evaluated metric.
    pub label: &'static str,
    /// Measured throughput (MiB/s).
    pub current: f64,
    /// Absolute floor enforced, if any (both metrics enforce one today).
    pub floor_enforced: Option<f64>,
    /// Baseline throughput compared against, if any.
    pub baseline: Option<f64>,
    /// Percentage change vs baseline (`None` if no baseline).
    pub delta_pct: Option<f64>,
    /// Whether this metric failed any enforced check.
    pub failed: bool,
    /// Human-readable failure reasons (empty on pass).
    pub reasons: Vec<String>,
}

/// Evaluate one metric against an (optional) absolute floor and an (optional)
/// committed baseline.
///
/// - When `enforce_floor` is `true`, `current < floor` fails the gate. Both
///   DISPLAY and COMP-3 enforce their respective floors today.
/// - When a baseline value is present and positive, a regression worse than
///   `threshold` percent fails the gate. A missing/zero baseline is skipped
///   (graceful for first runs) and never fails on its own.
#[must_use]
pub fn evaluate_metric(
    metric: &GateMetric,
    enforce_floor: bool,
    floor: f64,
    threshold: f64,
) -> GateOutcome {
    let mut outcome = GateOutcome {
        label: metric.label,
        current: metric.current,
        floor_enforced: enforce_floor.then_some(floor),
        baseline: metric.baseline,
        delta_pct: None,
        failed: false,
        reasons: Vec::new(),
    };

    // Absolute floor.
    if enforce_floor && metric.current < floor {
        outcome.failed = true;
        outcome.reasons.push(format!(
            "{label} {current:.1} MiB/s below absolute floor {floor:.0} MiB/s",
            label = metric.label,
            current = metric.current,
            floor = floor
        ));
    }

    // Relative regression vs committed baseline.
    if let Some(base) = metric.baseline
        && base > 0.0
    {
        let delta_pct = (metric.current - base) / base * 100.0;
        outcome.delta_pct = Some(delta_pct);
        if delta_pct < -threshold {
            outcome.failed = true;
            outcome.reasons.push(format!(
                "{label} regression {delta:.2}% vs baseline {base:.1} MiB/s (threshold -{threshold:.0}%)",
                label = metric.label,
                delta = delta_pct,
                base = base,
                threshold = threshold
            ));
        }
    }

    outcome
}
