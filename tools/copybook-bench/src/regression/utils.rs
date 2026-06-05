use super::*;

/// Create standard regression detector with default configuration
pub fn create_standard_detector() -> PerformanceRegressionDetector {
    PerformanceRegressionDetector::new()
}

/// Create detector for CI environment with strict thresholds
pub fn create_ci_detector() -> PerformanceRegressionDetector {
    let mut detector = PerformanceRegressionDetector::new();
    // Configure stricter thresholds for CI
    detector.statistical_analyzer.variance_tolerance = 0.015; // 1.5%
    detector
}

/// Create detector for development environment with relaxed thresholds
pub fn create_dev_detector() -> PerformanceRegressionDetector {
    let mut detector = PerformanceRegressionDetector::new();
    // Configure more relaxed thresholds for development
    detector.statistical_analyzer.variance_tolerance = 0.05; // 5%
    detector
}
