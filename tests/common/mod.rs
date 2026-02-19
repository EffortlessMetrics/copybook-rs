// SPDX-License-Identifier: AGPL-3.0-or-later
//! Common testing infrastructure for copybook-rs
//!
//! Provides shared utilities and infrastructure for comprehensive testing
//! across all test suites, including scale testing, performance validation,
//! and enterprise-grade testing capabilities.

pub mod scale_testing;

// Re-export key types for convenience
pub use scale_testing::{
    ScaleTestingEngine, StressTestConfig, ScaleTestResult, ScaleTestStatus,
    MemoryConstraintMonitor, ErrorTaxonomyValidator, PerformanceTracker,
    ScalePerformanceMetrics, MemoryComplianceResult, ErrorAnalysisResult,
    DeterministicValidationResult, ConcurrentValidationResult, utils as scale_utils
};