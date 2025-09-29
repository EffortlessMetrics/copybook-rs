//! Baseline management for Issue #52 AC5
//!
//! Handles baseline promotion workflows for main branch merges and
//! performance regression detection with 90-day retention policy.

use crate::reporting::PerformanceReport;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Baseline performance data for regression detection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceBaseline {
    /// Branch name (typically "main")
    pub branch: String,
    /// Git commit hash
    pub commit: String,
    /// Baseline creation timestamp
    pub timestamp: String,
    /// DISPLAY throughput baseline in GiB/s
    pub display_gibs: Option<f64>,
    /// COMP-3 throughput baseline in MiB/s
    pub comp3_mibs: Option<f64>,
    /// Number of measurements averaged
    pub sample_count: u32,
}

/// Baseline store with retention management
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaselineStore {
    /// Current active baseline
    pub current: Option<PerformanceBaseline>,
    /// Historical baselines for regression analysis
    pub history: Vec<PerformanceBaseline>,
    /// Last updated timestamp
    pub updated: String,
}

impl BaselineStore {
    /// Create new empty baseline store
    #[must_use]
    pub fn new() -> Self {
        Self {
            current: None,
            history: Vec::new(),
            updated: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Load baseline store from file or create new if missing
    ///
    /// # Errors
    ///
    /// Returns an error if the file exists but cannot be read or parsed as valid JSON.
    pub fn load_or_create<P: AsRef<Path>>(path: P) -> anyhow::Result<Self> {
        let path = path.as_ref();
        if path.exists() {
            let content = std::fs::read_to_string(path)?;
            let store: Self = serde_json::from_str(&content)?;
            Ok(store)
        } else {
            Ok(Self::new())
        }
    }

    /// Save baseline store to file
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be created or written to.
    pub fn save<P: AsRef<Path>>(&self, path: P) -> anyhow::Result<()> {
        let content = serde_json::to_string_pretty(self)?;
        if let Some(parent) = path.as_ref().parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Promote report to baseline (AC5 - main branch promotion)
    pub fn promote_baseline(&mut self, report: &PerformanceReport, branch: &str, commit: &str) {
        // Archive current baseline to history
        if let Some(current) = self.current.take() {
            self.history.push(current);
        }

        // Create new baseline
        let baseline = PerformanceBaseline {
            branch: branch.to_string(),
            commit: commit.to_string(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            display_gibs: report.display_gibs,
            comp3_mibs: report.comp3_mibs,
            sample_count: 1,
        };

        self.current = Some(baseline);
        self.updated = chrono::Utc::now().to_rfc3339();

        // Apply 90-day retention policy
        self.apply_retention_policy(90);
    }

    /// Check for performance regression against baseline
    #[must_use]
    pub fn check_regression(&self, report: &PerformanceReport, threshold: f64) -> Vec<String> {
        let mut regressions = Vec::new();

        if let Some(baseline) = &self.current {
            if let (Some(current_display), Some(baseline_display)) =
                (report.display_gibs, baseline.display_gibs)
            {
                let regression_pct =
                    (baseline_display - current_display) / baseline_display * 100.0;
                if regression_pct > threshold {
                    regressions.push(format!(
                        "DISPLAY regression: {regression_pct:.2}% slower than baseline ({current_display:.2} vs {baseline_display:.2} GiB/s)"
                    ));
                }
            }

            if let (Some(current_comp3), Some(baseline_comp3)) =
                (report.comp3_mibs, baseline.comp3_mibs)
            {
                let regression_pct = (baseline_comp3 - current_comp3) / baseline_comp3 * 100.0;
                if regression_pct > threshold {
                    regressions.push(format!(
                        "COMP-3 regression: {regression_pct:.2}% slower than baseline ({current_comp3:.0} vs {baseline_comp3:.0} MiB/s)"
                    ));
                }
            }
        }

        regressions
    }

    /// Apply retention policy (remove baselines older than days)
    fn apply_retention_policy(&mut self, retention_days: i64) {
        let cutoff = chrono::Utc::now() - chrono::Duration::days(retention_days);

        self.history.retain(|baseline| {
            if let Ok(timestamp) = chrono::DateTime::parse_from_rfc3339(&baseline.timestamp) {
                timestamp.with_timezone(&chrono::Utc) > cutoff
            } else {
                // Keep baselines with invalid timestamps for safety
                true
            }
        });
    }

    /// Get baseline summary for reporting
    #[must_use]
    pub fn summary(&self) -> String {
        match &self.current {
            Some(baseline) => {
                let display = baseline
                    .display_gibs
                    .map_or_else(|| "N/A".to_string(), |v| format!("{v:.2} GiB/s"));
                let comp3 = baseline
                    .comp3_mibs
                    .map_or_else(|| "N/A".to_string(), |v| format!("{v:.0} MiB/s"));

                let short_commit = baseline.commit.chars().take(8).collect::<String>();
                format!(
                    "Baseline ({short_commit}): DISPLAY {display} COMP-3 {comp3} [{}]",
                    baseline.branch
                )
            }
            None => "No baseline established".to_string(),
        }
    }
}

impl Default for BaselineStore {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_baseline_store_creation() {
        let store = BaselineStore::new();
        assert!(store.current.is_none());
        assert!(store.history.is_empty());
    }

    #[test]
    #[allow(clippy::unwrap_used)]
    fn test_baseline_promotion() {
        let mut store = BaselineStore::new();
        let mut report = PerformanceReport::new();
        report.display_gibs = Some(4.22);
        report.comp3_mibs = Some(571.0);

        store.promote_baseline(&report, "main", "abc123");

        let baseline = store.current.as_ref().unwrap();
        assert_eq!(baseline.branch, "main");
        assert_eq!(baseline.commit, "abc123");
        assert_eq!(baseline.display_gibs, Some(4.22));
        assert_eq!(baseline.comp3_mibs, Some(571.0));
    }

    #[test]
    fn test_regression_detection() {
        let mut store = BaselineStore::new();
        let mut baseline_report = PerformanceReport::new();
        baseline_report.display_gibs = Some(4.0);
        baseline_report.comp3_mibs = Some(600.0);
        store.promote_baseline(&baseline_report, "main", "baseline");

        // Test no regression
        let mut good_report = PerformanceReport::new();
        good_report.display_gibs = Some(4.0);
        good_report.comp3_mibs = Some(600.0);
        let regressions = store.check_regression(&good_report, 5.0);
        assert!(regressions.is_empty());

        // Test regression
        let mut bad_report = PerformanceReport::new();
        bad_report.display_gibs = Some(3.0); // 25% slower
        bad_report.comp3_mibs = Some(540.0); // 10% slower
        let regressions = store.check_regression(&bad_report, 5.0);
        assert_eq!(regressions.len(), 2);
    }
}
