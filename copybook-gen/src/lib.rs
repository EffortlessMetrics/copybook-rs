//! Test fixture and synthetic data generation for copybook-rs
//!
//! This crate provides utilities for generating synthetic COBOL copybooks
//! and test data for comprehensive testing and validation.

pub mod copybook;
pub mod data;
pub mod golden;

use copybook_core::Schema;
use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;

/// Configuration for synthetic data generation
#[derive(Debug, Clone)]
pub struct GeneratorConfig {
    /// Random seed for deterministic generation
    pub seed: u64,
    /// Number of records to generate
    pub record_count: usize,
    /// Whether to include edge cases
    pub include_edge_cases: bool,
    /// Whether to include invalid data for negative testing
    pub include_invalid_data: bool,
}

impl Default for GeneratorConfig {
    fn default() -> Self {
        Self {
            seed: 42,
            record_count: 1000,
            include_edge_cases: true,
            include_invalid_data: false,
        }
    }
}

/// Generate synthetic copybook text
pub fn generate_copybook(config: &GeneratorConfig) -> String {
    copybook::generate_synthetic_copybook(config)
}

/// Generate synthetic data for a schema
pub fn generate_data(schema: &Schema, config: &GeneratorConfig) -> Vec<Vec<u8>> {
    data::generate_synthetic_data(schema, config)
}

/// Create golden test with SHA-256 validation
pub fn create_golden_test(
    name: &str,
    copybook: &str,
    data: &[u8],
) -> golden::GoldenTest {
    golden::GoldenTest::new(name, copybook, data)
}