//! Performance benchmarks for copybook-rs
//!
//! This crate contains criterion benchmarks for measuring the performance
//! of copybook parsing and data conversion operations.

pub mod regression;

// Re-export key types for performance regression detection
pub use regression::{
    AlertSystem, BaselineMetadata, BaselineRepository, CiCheckResult, CiIntegrator,
    EnvironmentInfo, PerformanceMetrics, PerformanceRegressionDetector, RegressionAnalysis,
    RegressionStatus, StatisticalRegressionAnalyzer, utils,
};
