//! Test fixture and synthetic data generation for copybook-rs
//!
//! This crate provides utilities for generating synthetic COBOL copybooks
//! and test data for comprehensive testing and validation.

pub mod copybook;
pub mod data;
pub mod golden;
pub mod test_generation;

use copybook_core::Schema;

// Re-export key types
pub use copybook::{CopybookTemplate, FieldType};
pub use data::{DataStrategy, CorruptionType};
pub use golden::{GoldenTest, GoldenTestSuite, TestConfig, ValidationResult};
pub use test_generation::{TestSuiteStats, generate_complete_test_suite};

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

/// Generate copybook with specific template
pub fn generate_copybook_with_template(config: &GeneratorConfig, template: CopybookTemplate) -> String {
    copybook::generate_copybook_with_template(config, template)
}

/// Generate synthetic data for a schema
pub fn generate_data(schema: &Schema, config: &GeneratorConfig) -> Vec<Vec<u8>> {
    data::generate_synthetic_data(schema, config)
}

/// Generate data with specific strategy
pub fn generate_data_with_strategy(
    schema: &Schema, 
    config: &GeneratorConfig, 
    strategy: DataStrategy
) -> Vec<Vec<u8>> {
    data::generate_data_with_strategy(schema, config, strategy)
}

/// Create golden test with SHA-256 validation
pub fn create_golden_test(name: &str, copybook: &str, data: &[u8]) -> golden::GoldenTest {
    golden::GoldenTest::new(name, copybook, data)
}

/// Create golden test with specific configuration
pub fn create_golden_test_with_config(
    name: &str, 
    copybook: &str, 
    data: &[u8], 
    config: TestConfig
) -> golden::GoldenTest {
    golden::GoldenTest::new_with_config(name, copybook, data, config)
}

/// Generate comprehensive test suite
pub fn generate_comprehensive_test_suite() -> golden::GoldenTestSuite {
    golden::generate_comprehensive_suite()
}

/// Generate performance test suite
pub fn generate_performance_test_suite() -> golden::GoldenTestSuite {
    golden::generate_performance_suite()
}

/// Generate negative test suite
pub fn generate_negative_test_suite() -> golden::GoldenTestSuite {
    golden::generate_negative_test_suite()
}

/// Generate invalid copybooks for negative testing
pub fn generate_invalid_copybooks(config: &GeneratorConfig) -> Vec<(String, String)> {
    copybook::generate_invalid_copybook(config)
}

/// Generate corrupted data for negative testing
pub fn generate_corrupted_data(clean_data: &[u8], corruption_type: CorruptionType) -> Vec<u8> {
    data::generate_corrupted_data(clean_data, corruption_type)
}

/// Test suite builder for creating custom test suites
pub struct TestSuiteBuilder {
    suite: golden::GoldenTestSuite,
    config: GeneratorConfig,
}

impl TestSuiteBuilder {
    /// Create a new test suite builder
    pub fn new(name: &str, description: &str) -> Self {
        Self {
            suite: golden::GoldenTestSuite::new(name, description),
            config: GeneratorConfig::default(),
        }
    }

    /// Set the generator configuration
    pub fn with_config(mut self, config: GeneratorConfig) -> Self {
        self.config = config;
        self
    }

    /// Add a simple test case
    pub fn add_simple_test(mut self, name: &str) -> Self {
        let copybook = generate_copybook_with_template(&self.config, CopybookTemplate::Simple);
        let test = golden::GoldenTest::new(name, &copybook, &[]);
        self.suite.add_test(test);
        self
    }

    /// Add a REDEFINES test case
    pub fn add_redefines_test(mut self, name: &str) -> Self {
        let copybook = generate_copybook_with_template(&self.config, CopybookTemplate::WithRedefines);
        let test = golden::GoldenTest::new(name, &copybook, &[]);
        self.suite.add_test(test);
        self
    }

    /// Add an ODO test case
    pub fn add_odo_test(mut self, name: &str) -> Self {
        let copybook = generate_copybook_with_template(&self.config, CopybookTemplate::WithODO);
        let test = golden::GoldenTest::new(name, &copybook, &[]);
        self.suite.add_test(test);
        self
    }

    /// Add a performance test case
    pub fn add_performance_test(mut self, name: &str, template: CopybookTemplate) -> Self {
        let copybook = generate_copybook_with_template(&self.config, template);
        let mut test = golden::GoldenTest::new(name, &copybook, &[]);
        test.add_tag("performance");
        self.suite.add_test(test);
        self
    }

    /// Add a negative test case
    pub fn add_negative_test(mut self, name: &str, copybook: String) -> Self {
        let mut test = golden::GoldenTest::new(name, &copybook, &[]);
        test.add_tag("negative");
        test.add_tag("invalid");
        self.suite.add_test(test);
        self
    }

    /// Build the test suite
    pub fn build(self) -> golden::GoldenTestSuite {
        self.suite
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_simple_copybook() {
        let config = GeneratorConfig::default();
        let copybook = generate_copybook(&config);
        
        assert!(copybook.contains("01  RECORD-ROOT"));
        assert!(copybook.contains("PIC"));
    }

    #[test]
    fn test_generate_copybook_templates() {
        let config = GeneratorConfig::default();
        
        let simple = generate_copybook_with_template(&config, CopybookTemplate::Simple);
        assert!(simple.contains("Simple"));
        
        let redefines = generate_copybook_with_template(&config, CopybookTemplate::WithRedefines);
        assert!(redefines.contains("REDEFINES"));
        
        let occurs = generate_copybook_with_template(&config, CopybookTemplate::WithOccurs);
        assert!(occurs.contains("OCCURS"));
        
        let odo = generate_copybook_with_template(&config, CopybookTemplate::WithODO);
        assert!(odo.contains("DEPENDING ON"));
    }

    #[test]
    fn test_generate_invalid_copybooks() {
        let config = GeneratorConfig::default();
        let invalid_cases = generate_invalid_copybooks(&config);
        
        assert!(!invalid_cases.is_empty());
        assert!(invalid_cases.iter().any(|(name, _)| name == "invalid_level"));
        assert!(invalid_cases.iter().any(|(name, _)| name == "redefines_missing"));
    }

    #[test]
    fn test_golden_test_creation() {
        let test = create_golden_test("test1", "01 ROOT PIC X(10).", b"test data");
        
        assert_eq!(test.name, "test1");
        assert!(test.copybook.contains("ROOT"));
        assert!(!test.input_hash.is_empty());
    }

    #[test]
    fn test_test_suite_builder() {
        let suite = TestSuiteBuilder::new("test_suite", "Test suite description")
            .add_simple_test("simple_test")
            .add_redefines_test("redefines_test")
            .add_odo_test("odo_test")
            .build();
        
        assert_eq!(suite.name, "test_suite");
        assert_eq!(suite.tests.len(), 3);
        assert!(suite.find_test("simple_test").is_some());
        assert!(suite.find_test("redefines_test").is_some());
        assert!(suite.find_test("odo_test").is_some());
    }

    #[test]
    fn test_deterministic_generation() {
        let config1 = GeneratorConfig { seed: 123, ..Default::default() };
        let config2 = GeneratorConfig { seed: 123, ..Default::default() };
        
        let copybook1 = generate_copybook(&config1);
        let copybook2 = generate_copybook(&config2);
        
        assert_eq!(copybook1, copybook2);
    }
}
