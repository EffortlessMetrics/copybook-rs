// SPDX-License-Identifier: AGPL-3.0-or-later
//! Performance Audit Subsystem
//!
//! Tracks processing performance metrics, baseline management, and regression
//! detection for copybook-rs enterprise mainframe data processing operations.

use crate::{Error, ErrorCode, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

/// Performance audit system for tracking and validating processing metrics
pub struct PerformanceAuditor {
    baseline_manager: BaselineManager,
    regression_detector: RegressionDetector,
}

impl PerformanceAuditor {
    /// Creates a new PerformanceAuditor.
    ///
    /// # Arguments
    ///
    /// * `baseline_path` - The path to the performance baseline file.
    /// * `regression_threshold` - The percentage threshold for detecting a performance regression.
    pub fn new(baseline_path: impl AsRef<Path>, regression_threshold: f64) -> Self {
        Self {
            baseline_manager: BaselineManager::new(baseline_path),
            regression_detector: RegressionDetector::new().with_threshold(regression_threshold),
        }
    }

    /// Audits the current performance metrics against the baseline.
    ///
    /// # Arguments
    ///
    /// * `current_metrics` - The performance metrics of the current run.
    ///
    /// # Errors
    /// Returns an error if the baseline file cannot be loaded or parsed.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn audit(&self, current_metrics: &ThroughputMetrics) -> Result<Vec<String>> {
        let baseline = self.baseline_manager.load_baseline()?;
        let regressions = self
            .regression_detector
            .check_regression(current_metrics, &baseline.throughput);
        Ok(regressions)
    }
}

/// Performance baseline management
#[derive(Debug, Clone)]
pub struct BaselineManager {
    baseline_path: PathBuf,
}

impl BaselineManager {
    /// Creates a new BaselineManager.
    pub fn new(baseline_path: impl AsRef<Path>) -> Self {
        Self {
            baseline_path: baseline_path.as_ref().to_path_buf(),
        }
    }

    /// Loads the performance baseline from the specified file.
    ///
    /// # Errors
    /// Returns an error if the baseline file cannot be read or parsed.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn load_baseline(&self) -> Result<PerformanceBaseline> {
        let data = fs::read_to_string(&self.baseline_path).map_err(|e| {
            Error::new(
                ErrorCode::CBKA001_BASELINE_ERROR,
                format!("Failed to read baseline file: {}", e),
            )
        })?;
        let baseline = serde_json::from_str(&data).map_err(|e| {
            Error::new(
                ErrorCode::CBKA001_BASELINE_ERROR,
                format!("Failed to parse baseline file: {}", e),
            )
        })?;
        Ok(baseline)
    }

    /// Saves a performance baseline to the specified file.
    ///
    /// # Errors
    /// Returns an error if the baseline file cannot be serialized or written.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn save_baseline(&self, baseline: &PerformanceBaseline) -> Result<()> {
        let data = serde_json::to_string_pretty(baseline).map_err(|e| {
            Error::new(
                ErrorCode::CBKA001_BASELINE_ERROR,
                format!("Failed to serialize baseline: {}", e),
            )
        })?;
        fs::write(&self.baseline_path, data).map_err(|e| {
            Error::new(
                ErrorCode::CBKA001_BASELINE_ERROR,
                format!("Failed to write baseline file: {}", e),
            )
        })
    }
}

/// Performance baseline data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceBaseline {
    pub baseline_id: String,
    pub throughput: ThroughputMetrics,
    pub resources: ResourceMetrics,
    pub created_at: String,
}

/// Throughput performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThroughputMetrics {
    pub display_throughput: u64, // bytes/sec for DISPLAY fields
    pub comp3_throughput: u64,   // bytes/sec for COMP-3 fields
    pub record_rate: u64,        // records/sec
    pub peak_memory_mb: u64,     // peak memory usage
}

/// System resource utilization metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceMetrics {
    pub cpu_usage_percent: f64,
    pub memory_usage_mb: u64,
    pub io_operations: u64,
    pub network_bytes: u64,
}

/// Performance regression detection
#[derive(Debug, Clone)]
pub struct RegressionDetector {
    threshold_percent: f64,
}

impl RegressionDetector {
    /// Creates a new RegressionDetector with a default threshold.
    pub fn new() -> Self {
        Self {
            threshold_percent: 5.0, // 5% degradation threshold
        }
    }

    /// Sets a custom threshold for the detector.
    #[must_use]
    pub fn with_threshold(mut self, threshold_percent: f64) -> Self {
        self.threshold_percent = threshold_percent;
        self
    }

    /// Compares current metrics against a baseline to detect regressions.
    #[allow(clippy::cast_precision_loss)]
    pub fn check_regression(
        &self,
        current: &ThroughputMetrics,
        baseline: &ThroughputMetrics,
    ) -> Vec<String> {
        let mut regressions = Vec::new();
        let threshold_multiplier = 1.0 - (self.threshold_percent / 100.0);

        if (current.record_rate as f64) < (baseline.record_rate as f64) * threshold_multiplier {
            regressions.push(format!(
                "Record rate regression: current {} recs/s < baseline {} recs/s",
                current.record_rate, baseline.record_rate
            ));
        }

        if (current.display_throughput as f64)
            < (baseline.display_throughput as f64) * threshold_multiplier
        {
            regressions.push(format!(
                "Display throughput regression: current {} bytes/s < baseline {} bytes/s",
                current.display_throughput, baseline.display_throughput
            ));
        }

        if (current.comp3_throughput as f64)
            < (baseline.comp3_throughput as f64) * threshold_multiplier
        {
            regressions.push(format!(
                "COMP-3 throughput regression: current {} bytes/s < baseline {} bytes/s",
                current.comp3_throughput, baseline.comp3_throughput
            ));
        }

        // For memory, a higher value is a regression
        let memory_threshold_multiplier = 1.0 + (self.threshold_percent / 100.0);
        if (current.peak_memory_mb as f64)
            > (baseline.peak_memory_mb as f64) * memory_threshold_multiplier
        {
            regressions.push(format!(
                "Peak memory regression: current {} MB > baseline {} MB",
                current.peak_memory_mb, baseline.peak_memory_mb
            ));
        }

        regressions
    }
}

impl Default for RegressionDetector {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_auditor_creation() {
        let auditor = PerformanceAuditor::new("baseline.json", 10.0);
        assert_eq!(auditor.regression_detector.threshold_percent, 10.0);
    }

    #[test]
    fn test_regression_detector_default() {
        let detector = RegressionDetector::new();
        assert_eq!(detector.threshold_percent, 5.0);
    }

    #[test]
    fn test_regression_detector_custom_threshold() {
        let detector = RegressionDetector::new().with_threshold(15.0);
        assert_eq!(detector.threshold_percent, 15.0);
    }

    #[test]
    fn test_regression_detector_no_regression() {
        let detector = RegressionDetector::new().with_threshold(10.0);

        let current = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let baseline = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let regressions = detector.check_regression(&current, &baseline);
        assert!(regressions.is_empty());
    }

    #[test]
    fn test_regression_detector_record_rate_regression() {
        let detector = RegressionDetector::new().with_threshold(10.0);

        let current = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 80, // 20% below baseline
            peak_memory_mb: 100,
        };

        let baseline = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let regressions = detector.check_regression(&current, &baseline);
        assert_eq!(regressions.len(), 1);
        assert!(regressions[0].contains("Record rate regression"));
    }

    #[test]
    fn test_regression_detector_display_throughput_regression() {
        let detector = RegressionDetector::new().with_threshold(10.0);

        let current = ThroughputMetrics {
            display_throughput: 800, // 20% below baseline
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let baseline = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let regressions = detector.check_regression(&current, &baseline);
        assert_eq!(regressions.len(), 1);
        assert!(regressions[0].contains("Display throughput regression"));
    }

    #[test]
    fn test_regression_detector_comp3_throughput_regression() {
        let detector = RegressionDetector::new().with_threshold(10.0);

        let current = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 400, // 20% below baseline
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let baseline = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let regressions = detector.check_regression(&current, &baseline);
        assert_eq!(regressions.len(), 1);
        assert!(regressions[0].contains("COMP-3 throughput regression"));
    }

    #[test]
    fn test_regression_detector_memory_regression() {
        let detector = RegressionDetector::new().with_threshold(10.0);

        let current = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 120, // 20% above baseline
        };

        let baseline = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let regressions = detector.check_regression(&current, &baseline);
        assert_eq!(regressions.len(), 1);
        assert!(regressions[0].contains("Peak memory regression"));
    }

    #[test]
    fn test_regression_detector_multiple_regressions() {
        let detector = RegressionDetector::new().with_threshold(10.0);

        let current = ThroughputMetrics {
            display_throughput: 800, // 20% below
            comp3_throughput: 400,   // 20% below
            record_rate: 80,         // 20% below
            peak_memory_mb: 120,     // 20% above
        };

        let baseline = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        let regressions = detector.check_regression(&current, &baseline);
        assert_eq!(regressions.len(), 4);
    }

    #[test]
    fn test_regression_detector_default_impl() {
        let detector = RegressionDetector::default();
        assert_eq!(detector.threshold_percent, 5.0);
    }

    #[test]
    fn test_baseline_manager_creation() {
        let manager = BaselineManager::new("test_baseline.json");
        assert_eq!(manager.baseline_path, PathBuf::from("test_baseline.json"));
    }

    #[test]
    fn test_performance_baseline_serialization() {
        let baseline = PerformanceBaseline {
            baseline_id: "test-baseline".to_string(),
            throughput: ThroughputMetrics {
                display_throughput: 1000,
                comp3_throughput: 500,
                record_rate: 100,
                peak_memory_mb: 100,
            },
            resources: ResourceMetrics {
                cpu_usage_percent: 50.0,
                memory_usage_mb: 100,
                io_operations: 1000,
                network_bytes: 5000,
            },
            created_at: "2024-01-01T00:00:00Z".to_string(),
        };

        let json = serde_json::to_string(&baseline).expect("Failed to serialize");
        assert!(json.contains("test-baseline"));
        assert!(json.contains("display_throughput"));
    }

    #[test]
    fn test_throughput_metrics_creation() {
        let metrics = ThroughputMetrics {
            display_throughput: 1000,
            comp3_throughput: 500,
            record_rate: 100,
            peak_memory_mb: 100,
        };

        assert_eq!(metrics.display_throughput, 1000);
        assert_eq!(metrics.comp3_throughput, 500);
        assert_eq!(metrics.record_rate, 100);
        assert_eq!(metrics.peak_memory_mb, 100);
    }

    #[test]
    fn test_resource_metrics_creation() {
        let metrics = ResourceMetrics {
            cpu_usage_percent: 50.0,
            memory_usage_mb: 100,
            io_operations: 1000,
            network_bytes: 5000,
        };

        assert_eq!(metrics.cpu_usage_percent, 50.0);
        assert_eq!(metrics.memory_usage_mb, 100);
        assert_eq!(metrics.io_operations, 1000);
        assert_eq!(metrics.network_bytes, 5000);
    }
}
