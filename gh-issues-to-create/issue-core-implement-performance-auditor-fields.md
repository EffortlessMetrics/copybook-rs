# [Task]: Implement or Remove Unused Fields in `PerformanceAuditor`

**Issue Description**

The `PerformanceAuditor` struct in `copybook-core/src/audit/performance.rs` is a stubbed-out feature. Its fields, `baseline_manager` and `regression_detector`, are marked with `#[allow(dead_code)]` and are never used. A search of the codebase confirms the `PerformanceAuditor` itself is never used.

This represents an incomplete feature. The surrounding data structures (`PerformanceBaseline`, `RegressionDetector`, etc.) suggest a clear design for a performance regression testing system. This issue should be resolved by either fully implementing this system or removing the stubbed code to reduce technical debt.

**File and Location:**

`copybook-core/src/audit/performance.rs:12-15`

**Code Context:**

```rust
// copybook-core/src/audit/performance.rs

// Performance audit functionality - implementation placeholder

pub struct PerformanceAuditor {
    #[allow(dead_code)]
    baseline_manager: BaselineManager,
    #[allow(dead_code)]
    regression_detector: RegressionDetector,
}

impl PerformanceAuditor {
    pub fn new() -> Self {
        Self {
            baseline_manager: BaselineManager::new(),
            regression_detector: RegressionDetector::new(),
        }
    }
}
```

**Proposed Fix: Full Implementation**

Instead of removing the code, the recommended path is to implement the feature as it was likely intended. This involves adding logic to the `PerformanceAuditor` and its components.

### Step 1: Implement `BaselineManager`

Add methods to load and save a `PerformanceBaseline` from a file (e.g., JSON).

```rust
// copybook-core/src/audit/performance.rs

use std::path::Path;
use std::fs;
use crate::Result; // Assuming a standard Result type

#[derive(Debug, Clone)]
pub struct BaselineManager {
    baseline_path: PathBuf, // Add a path to the baseline file
}

impl BaselineManager {
    pub fn new(baseline_path: impl AsRef<Path>) -> Self {
        Self { baseline_path: baseline_path.as_ref().to_path_buf() }
    }

    pub fn load_baseline(&self) -> Result<PerformanceBaseline> {
        let data = fs::read_to_string(&self.baseline_path)?;
        let baseline = serde_json::from_str(&data)?;
        Ok(baseline)
    }

    // Add a save method for creating baselines
    pub fn save_baseline(&self, baseline: &PerformanceBaseline) -> Result<()> {
        let data = serde_json::to_string_pretty(baseline)?;
        fs::write(&self.baseline_path, data)?;
        Ok(())
    }
}
```

### Step 2: Implement `RegressionDetector`

Add a method to compare new metrics against a baseline.

```rust
// copybook-core/src/audit/performance.rs

#[derive(Debug, Clone)]
pub struct RegressionDetector {
    threshold_percent: f64,
}

impl RegressionDetector {
    // ... new() and with_threshold() ...

    pub fn check_regression(&self, current: &ThroughputMetrics, baseline: &ThroughputMetrics) -> Vec<String> {
        let mut regressions = Vec::new();

        let threshold_multiplier = 1.0 - (self.threshold_percent / 100.0);

        if (current.record_rate as f64) < (baseline.record_rate as f64) * threshold_multiplier {
            regressions.push(format!(
                "Record rate regression: current {} recs/s < baseline {} recs/s",
                current.record_rate,
                baseline.record_rate
            ));
        }

        // ... add similar checks for other metrics like throughput and memory ...

        regressions
    }
}
```

### Step 3: Implement `PerformanceAuditor`

Tie everything together in the `PerformanceAuditor`.

```rust
// copybook-core/src/audit/performance.rs

pub struct PerformanceAuditor {
    baseline_manager: BaselineManager,
    regression_detector: RegressionDetector,
}

impl PerformanceAuditor {
    pub fn new(baseline_path: impl AsRef<Path>, regression_threshold: f64) -> Self {
        Self {
            baseline_manager: BaselineManager::new(baseline_path),
            regression_detector: RegressionDetector::new().with_threshold(regression_threshold),
        }
    }

    pub fn audit(&self, current_metrics: &ThroughputMetrics) -> Result<Vec<String>> {
        let baseline = self.baseline_manager.load_baseline()?;
        let regressions = self.regression_detector.check_regression(current_metrics, &baseline.throughput);
        Ok(regressions)
    }
}
```

By implementing the feature this way, the `PerformanceAuditor` becomes a useful tool for CI/CD pipelines to automatically catch performance regressions, fulfilling its intended purpose.