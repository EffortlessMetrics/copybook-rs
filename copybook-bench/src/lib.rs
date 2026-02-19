// SPDX-License-Identifier: AGPL-3.0-or-later
//! Performance benchmarks for copybook-rs
//!
//! This crate contains criterion benchmarks for measuring the performance
//! of copybook parsing and data conversion operations, with machine-readable
//! reporting infrastructure for Issue #52.

#![allow(clippy::missing_inline_in_public_items)]

pub mod baseline;
pub mod health;
pub mod memory;
pub mod regression;
pub mod reporting;

// Re-export key types for Issue #52 machine-readable reporting
pub use baseline::{BaselineStore, PerformanceBaseline};
pub use reporting::PerformanceReport;

// Re-export legacy types for performance regression detection
pub use regression::{
    AlertSystem, BaselineMetadata, BaselineRepository, CiCheckResult, CiIntegrator,
    EnvironmentInfo, PerformanceMetrics, PerformanceRegressionDetector, RegressionAnalysis,
    RegressionStatus, StatisticalRegressionAnalyzer, utils,
};
