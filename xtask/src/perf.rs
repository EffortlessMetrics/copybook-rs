//! Performance receipt parsing and SLO evaluation
//!
//! Pure functions for parsing perf.json receipts and evaluating SLO compliance.

use anyhow::{Result, bail};
use serde_json::Value;

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

/// Parse perf.json receipt into a snapshot
///
/// Handles both flat and nested summary structures:
/// - `{"display_mibps": 205.0, "comp3_mibps": 58.0}`
/// - `{"summary": {"display_mibps": 205.0, "comp3_mibps": 58.0}}`
///
/// # Errors
///
/// Returns error if JSON is malformed, missing required fields, or contains invalid throughput values.
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
mod tests {
    use super::*;

    #[test]
    fn test_parse_flat_receipt() {
        let json = r#"{"display_mibps": 205.4, "comp3_mibps": 58.2}"#;
        let snapshot = parse_perf_receipt(json).unwrap();
        assert!((snapshot.display_mibps - 205.4).abs() < 0.01);
        assert!((snapshot.comp3_mibps - 58.2).abs() < 0.01);
    }

    #[test]
    fn test_parse_nested_receipt() {
        let json = r#"{"summary": {"display_mibps": 205.4, "comp3_mibps": 58.2}}"#;
        let snapshot = parse_perf_receipt(json).unwrap();
        assert!((snapshot.display_mibps - 205.4).abs() < 0.01);
        assert!((snapshot.comp3_mibps - 58.2).abs() < 0.01);
    }

    #[test]
    fn test_parse_missing_display() {
        let json = r#"{"comp3_mibps": 58.2}"#;
        let result = parse_perf_receipt(json);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("display_mibps"));
    }

    #[test]
    fn test_parse_missing_comp3() {
        let json = r#"{"display_mibps": 205.4}"#;
        let result = parse_perf_receipt(json);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("comp3_mibps"));
    }

    #[test]
    fn test_parse_negative_throughput() {
        let json = r#"{"display_mibps": -1.0, "comp3_mibps": 58.2}"#;
        let result = parse_perf_receipt(json);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Invalid throughput")
        );
    }

    #[test]
    fn test_parse_malformed_json() {
        let json = r#"{"display_mibps": "not a number"}"#;
        let result = parse_perf_receipt(json);
        assert!(result.is_err());
    }

    #[test]
    fn test_evaluate_slo_clearly_good() {
        let snapshot = PerfSnapshot {
            display_mibps: 205.0,
            comp3_mibps: 58.0,
        };
        let status = evaluate_slo(&snapshot);
        assert!(matches!(status, SloStatus::Pass));
    }

    #[test]
    fn test_evaluate_slo_exactly_on_floor() {
        let snapshot = PerfSnapshot {
            display_mibps: 80.0,
            comp3_mibps: 40.0,
        };
        let status = evaluate_slo(&snapshot);
        assert!(matches!(status, SloStatus::Pass));
    }

    #[test]
    fn test_evaluate_slo_display_just_under() {
        let snapshot = PerfSnapshot {
            display_mibps: 79.9,
            comp3_mibps: 40.0,
        };
        let status = evaluate_slo(&snapshot);
        assert!(matches!(status, SloStatus::Fail { .. }));
    }

    #[test]
    fn test_evaluate_slo_comp3_just_under() {
        let snapshot = PerfSnapshot {
            display_mibps: 80.0,
            comp3_mibps: 39.9,
        };
        let status = evaluate_slo(&snapshot);
        assert!(matches!(status, SloStatus::Fail { .. }));
    }

    #[test]
    fn test_evaluate_slo_both_under() {
        let snapshot = PerfSnapshot {
            display_mibps: 60.0,
            comp3_mibps: 30.0,
        };
        let status = evaluate_slo(&snapshot);
        match status {
            SloStatus::Fail {
                display_delta_pct,
                comp3_delta_pct,
            } => {
                // 60 vs 80 = -25%
                assert!((display_delta_pct + 25.0).abs() < 0.1);
                // 30 vs 40 = -25%
                assert!((comp3_delta_pct + 25.0).abs() < 0.1);
            }
            SloStatus::Pass => panic!("Expected Fail status"),
        }
    }

    #[test]
    fn test_format_slo_summary_pass() {
        let snapshot = PerfSnapshot {
            display_mibps: 205.0,
            comp3_mibps: 58.0,
        };
        let status = evaluate_slo(&snapshot);
        let summary = format_slo_summary(&snapshot, &status);

        assert!(summary.contains("DISPLAY: 205.0 MiB/s"));
        assert!(summary.contains("COMP-3: 58.0 MiB/s"));
        assert!(summary.contains("SLO 80 MiB/s"));
        assert!(summary.contains("SLO 40 MiB/s"));
        assert!(summary.contains("✓ All SLOs met"));
    }

    #[test]
    fn test_format_slo_summary_fail() {
        let snapshot = PerfSnapshot {
            display_mibps: 60.0,
            comp3_mibps: 30.0,
        };
        let status = evaluate_slo(&snapshot);
        let summary = format_slo_summary(&snapshot, &status);

        assert!(summary.contains("DISPLAY: 60.0 MiB/s"));
        assert!(summary.contains("COMP-3: 30.0 MiB/s"));
        assert!(summary.contains("⚠ SLOs not met"));
    }

    #[test]
    fn test_delta_percentage_calculation() {
        // Verify delta percentage math is correct
        let snapshot = PerfSnapshot {
            display_mibps: 100.0,
            comp3_mibps: 50.0,
        };
        let status = evaluate_slo(&snapshot);

        // 100 vs 80 = +25%
        // 50 vs 40 = +25%
        assert!(matches!(status, SloStatus::Pass));

        let summary = format_slo_summary(&snapshot, &status);
        assert!(summary.contains("+25.0%") || summary.contains("+25%"));
    }
}
