---
title: Integrate BaselineManager and RegressionDetector in `PerformanceAuditor`
labels: ["feature", "incomplete", "code-quality"]
assignees: []
---

## Issue Description

The `PerformanceAuditor` struct in `copybook-core/src/audit/performance.rs` declares `baseline_manager` (line 12) and `regression_detector` (line 14) fields, but both are marked with `#[allow(dead_code)]`. This indicates that these components, crucial for performance auditing (managing baselines and detecting regressions), are currently not being utilized within the `PerformanceAuditor`'s implementation. As a result, the `PerformanceAuditor` is likely not performing its full intended function of comprehensive performance analysis.

## Locations

*   `copybook-core/src/audit/performance.rs:12` (`baseline_manager` field)
*   `copybook-core/src/audit/performance.rs:14` (`regression_detector` field)

## Proposed Fix

1.  **Implement Core Auditing Logic:** Develop and integrate methods within the `PerformanceAuditor` struct that actively use the `baseline_manager` and `regression_detector` fields. This would typically involve:
    *   Methods to capture and store current performance metrics.
    *   Methods to retrieve and manage performance baselines.
    *   Methods to compare current metrics against baselines using the `regression_detector` to identify performance regressions.
2.  **Remove `#[allow(dead_code)]`:** Once the `baseline_manager` and `regression_detector` fields are actively used within the `PerformanceAuditor`'s logic, remove the `#[allow(dead_code)]` attributes from their declarations.
3.  **Add Tests:** Implement unit and integration tests for the new performance auditing logic to ensure its correctness and effectiveness.

## Example (Illustrative - adding a method to utilize the fields)

```rust
// Before (simplified):
// pub struct PerformanceAuditor {
//     #[allow(dead_code)]
//     baseline_manager: BaselineManager,
//     #[allow(dead_code)]
//     regression_detector: RegressionDetector,
// }
//
// impl PerformanceAuditor {
//     pub fn new() -> Self { /* ... */ }
// }

// After (conceptual):
// pub struct PerformanceAuditor {
//     baseline_manager: BaselineManager,
//     regression_detector: RegressionDetector,
// }
//
// impl PerformanceAuditor {
//     pub fn new() -> Self { /* ... */ }
//
//     /// Audits the given performance metrics against the established baseline.
//     /// Returns true if a regression is detected.
//     pub fn audit_metrics(&mut self, current_metrics: &PerformanceMetrics) -> bool {
//         // Example: Record current metrics, update baseline if needed
//         self.baseline_manager.update_with_metrics(current_metrics);
//
//         // Example: Detect regression
//         let baseline = self.baseline_manager.get_current_baseline();
//         self.regression_detector.detect_regression(&baseline, current_metrics)
//     }
//
//     // ... other methods that use baseline_manager and regression_detector
// }

// Note: `PerformanceMetrics`, `BaselineManager`, `RegressionDetector` would need to be defined/implemented.
```

This will bring the `PerformanceAuditor` to its full intended functionality and remove misleading `#[allow(dead_code)]` attributes.
