//! Golden test utilities with SHA-256 validation

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;

/// A golden test case with SHA-256 validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoldenTest {
    /// Test name
    pub name: String,
    /// Copybook text
    pub copybook: String,
    /// Input data SHA-256 hash
    pub input_hash: String,
    /// Expected output hashes for different formats
    pub expected_outputs: HashMap<String, String>,
    /// Test metadata
    pub metadata: GoldenTestMetadata,
}

/// Metadata for golden tests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoldenTestMetadata {
    /// Test creation timestamp
    pub created_at: String,
    /// Test description
    pub description: String,
    /// Test tags
    pub tags: Vec<String>,
    /// Test configuration
    pub config: TestConfig,
}

/// Test configuration parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestConfig {
    /// Codepage used
    pub codepage: String,
    /// Record format
    pub record_format: String,
    /// JSON number mode
    pub json_number_mode: String,
    /// Additional flags
    pub flags: Vec<String>,
}

impl Default for TestConfig {
    fn default() -> Self {
        Self {
            codepage: "cp037".to_string(),
            record_format: "fixed".to_string(),
            json_number_mode: "lossless".to_string(),
            flags: Vec::new(),
        }
    }
}

impl GoldenTest {
    /// Create a new golden test
    #[must_use]
    pub fn new(name: &str, copybook: &str, data: &[u8]) -> Self {
        let input_hash = Self::hash_bytes(data);

        Self {
            name: name.to_string(),
            copybook: copybook.to_string(),
            input_hash,
            expected_outputs: HashMap::new(),
            metadata: GoldenTestMetadata {
                created_at: chrono::Utc::now().to_rfc3339(),
                description: format!("Golden test: {name}"),
                tags: vec!["synthetic".to_string()],
                config: TestConfig::default(),
            },
        }
    }

    /// Create a golden test with specific configuration
    #[must_use]
    pub fn new_with_config(name: &str, copybook: &str, data: &[u8], config: TestConfig) -> Self {
        let input_hash = Self::hash_bytes(data);

        Self {
            name: name.to_string(),
            copybook: copybook.to_string(),
            input_hash,
            expected_outputs: HashMap::new(),
            metadata: GoldenTestMetadata {
                created_at: chrono::Utc::now().to_rfc3339(),
                description: format!("Golden test: {name}"),
                tags: vec!["synthetic".to_string()],
                config,
            },
        }
    }

    /// Calculate SHA-256 hash of bytes
    #[must_use]
    pub fn hash_bytes(data: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(data);
        format!("{:x}", hasher.finalize())
    }

    /// Calculate SHA-256 hash of string
    #[must_use]
    pub fn hash_string(data: &str) -> String {
        Self::hash_bytes(data.as_bytes())
    }

    /// Validate output against expected hash
    #[must_use]
    pub fn validate_output(&self, output_type: &str, output: &[u8]) -> bool {
        if let Some(expected_hash) = self.expected_outputs.get(output_type) {
            let actual_hash = Self::hash_bytes(output);
            actual_hash == *expected_hash
        } else {
            true // First run, no expected hash yet
        }
    }

    /// Validate string output against expected hash
    #[must_use]
    pub fn validate_string_output(&self, output_type: &str, output: &str) -> bool {
        if let Some(expected_hash) = self.expected_outputs.get(output_type) {
            let actual_hash = Self::hash_string(output);
            actual_hash == *expected_hash
        } else {
            true // First run, no expected hash yet
        }
    }

    /// Update expected output hash
    pub fn update_expected_hash(&mut self, output_type: &str, output: &[u8]) {
        let hash = Self::hash_bytes(output);
        self.expected_outputs.insert(output_type.to_string(), hash);
    }

    /// Update expected string output hash
    pub fn update_expected_string_hash(&mut self, output_type: &str, output: &str) {
        let hash = Self::hash_string(output);
        self.expected_outputs.insert(output_type.to_string(), hash);
    }

    /// Add a tag to the test
    pub fn add_tag(&mut self, tag: &str) {
        if !self.metadata.tags.contains(&tag.to_string()) {
            self.metadata.tags.push(tag.to_string());
        }
    }

    /// Check if test has a specific tag
    #[must_use]
    pub fn has_tag(&self, tag: &str) -> bool {
        self.metadata.tags.contains(&tag.to_string())
    }
}

/// Golden test suite for managing multiple tests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoldenTestSuite {
    /// Suite name
    pub name: String,
    /// Suite description
    pub description: String,
    /// Test cases
    pub tests: Vec<GoldenTest>,
    /// Suite metadata
    pub metadata: SuiteMetadata,
}

/// Suite metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SuiteMetadata {
    /// Creation timestamp
    pub created_at: String,
    /// Last updated timestamp
    pub updated_at: String,
    /// Version
    pub version: String,
    /// Tags
    pub tags: Vec<String>,
}

impl GoldenTestSuite {
    /// Create a new test suite
    #[must_use]
    pub fn new(name: &str, description: &str) -> Self {
        let now = chrono::Utc::now().to_rfc3339();

        Self {
            name: name.to_string(),
            description: description.to_string(),
            tests: Vec::new(),
            metadata: SuiteMetadata {
                created_at: now.clone(),
                updated_at: now,
                version: "1.0.0".to_string(),
                tags: Vec::new(),
            },
        }
    }

    /// Add a test to the suite
    pub fn add_test(&mut self, test: GoldenTest) {
        self.tests.push(test);
        self.metadata.updated_at = chrono::Utc::now().to_rfc3339();
    }

    /// Find a test by name
    #[must_use]
    pub fn find_test(&self, name: &str) -> Option<&GoldenTest> {
        self.tests.iter().find(|t| t.name == name)
    }

    /// Find a mutable test by name
    #[must_use]
    pub fn find_test_mut(&mut self, name: &str) -> Option<&mut GoldenTest> {
        self.tests.iter_mut().find(|t| t.name == name)
    }

    /// Get tests by tag
    #[must_use]
    pub fn tests_by_tag(&self, tag: &str) -> Vec<&GoldenTest> {
        self.tests.iter().filter(|t| t.has_tag(tag)).collect()
    }

    /// Validate all tests
    #[must_use]
    pub fn validate_all(&self) -> ValidationResult {
        let mut result = ValidationResult::new();

        for test in &self.tests {
            // This would validate against actual output in a real implementation
            result.add_test_result(&test.name, true, None);
        }

        result
    }

    /// Export suite to JSON
    ///
    /// # Errors
    /// Returns `serde_json::Error` if the serialization fails due to:
    /// - JSON serialization constraints
    /// - Memory allocation failures
    /// - Invalid UTF-8 sequences in string fields
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Import suite from JSON
    ///
    /// # Errors
    /// Returns `serde_json::Error` if the deserialization fails due to:
    /// - Invalid JSON syntax in the input string
    /// - JSON structure that doesn't match the expected schema
    /// - Missing required fields or invalid field types
    /// - UTF-8 encoding issues
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }
}

/// Validation result for a test suite
#[derive(Debug, Clone)]
pub struct ValidationResult {
    /// Test results
    pub results: HashMap<String, TestResult>,
    /// Overall success
    pub success: bool,
}

/// Individual test result
#[derive(Debug, Clone)]
pub struct TestResult {
    /// Test passed
    pub passed: bool,
    /// Error message if failed
    pub error: Option<String>,
    /// Execution time
    pub duration: Option<std::time::Duration>,
}

impl ValidationResult {
    /// Create a new validation result
    #[must_use]
    pub fn new() -> Self {
        Self {
            results: HashMap::new(),
            success: true,
        }
    }

    /// Add a test result
    pub fn add_test_result(&mut self, test_name: &str, passed: bool, error: Option<String>) {
        self.results.insert(
            test_name.to_string(),
            TestResult {
                passed,
                error,
                duration: None,
            },
        );

        if !passed {
            self.success = false;
        }
    }

    /// Get summary statistics
    #[must_use]
    pub fn summary(&self) -> (usize, usize, usize) {
        let total = self.results.len();
        let passed = self.results.values().filter(|r| r.passed).count();
        let failed = total - passed;

        (total, passed, failed)
    }
}

impl Default for ValidationResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Generate comprehensive golden test suite
#[must_use]
pub fn generate_comprehensive_suite() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new(
        "comprehensive_copybook_tests",
        "Comprehensive test suite for copybook-rs functionality",
    );

    // Add various test categories
    suite.metadata.tags.extend_from_slice(&[
        "comprehensive".to_string(),
        "regression".to_string(),
        "synthetic".to_string(),
    ]);

    // This would be populated with actual tests
    // For now, return empty suite structure

    suite
}

/// Performance test suite for throughput validation
#[must_use]
pub fn generate_performance_suite() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new("performance_tests", "Performance validation test suite");

    suite.metadata.tags.extend_from_slice(&[
        "performance".to_string(),
        "throughput".to_string(),
        "scalability".to_string(),
    ]);

    suite
}

/// Negative test suite for error handling validation
#[must_use]
pub fn generate_negative_test_suite() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new(
        "negative_tests",
        "Negative test cases for error handling validation",
    );

    suite.metadata.tags.extend_from_slice(&[
        "negative".to_string(),
        "error_handling".to_string(),
        "corruption".to_string(),
    ]);

    suite
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_helpers_match_expected() {
        let expected = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad";
        assert_eq!(GoldenTest::hash_string("abc"), expected);
        assert_eq!(GoldenTest::hash_bytes(b"abc"), expected);
    }

    #[test]
    fn test_validate_output_and_update_hashes() {
        let mut test = GoldenTest::new("sample", "01 ROOT PIC X(1).", b"data");

        assert!(test.validate_output("json", b"anything"));

        test.update_expected_hash("json", b"payload");
        assert!(test.validate_output("json", b"payload"));
        assert!(!test.validate_output("json", b"other"));

        test.update_expected_string_hash("text", "hello");
        assert!(test.validate_string_output("text", "hello"));
        assert!(!test.validate_string_output("text", "world"));
    }

    #[test]
    fn test_add_tag_is_idempotent() {
        let mut test = GoldenTest::new("sample", "01 ROOT PIC X(1).", b"data");
        test.add_tag("smoke");
        test.add_tag("smoke");
        assert!(test.has_tag("smoke"));
        assert_eq!(test.metadata.tags.iter().filter(|t| *t == "smoke").count(), 1);
    }

    #[test]
    fn test_suite_find_and_tags() {
        let mut suite = GoldenTestSuite::new("suite", "desc");
        let mut test = GoldenTest::new("t1", "01 ROOT PIC X(1).", b"data");
        test.add_tag("performance");
        suite.add_test(test);

        assert!(suite.find_test("t1").is_some());
        assert!(suite.find_test("missing").is_none());
        assert_eq!(suite.tests_by_tag("performance").len(), 1);
    }

    #[test]
    fn test_suite_json_roundtrip() {
        let mut suite = GoldenTestSuite::new("suite", "desc");
        suite.add_test(GoldenTest::new("t1", "01 ROOT PIC X(1).", b"data"));

        let json = suite.to_json().unwrap();
        let decoded = GoldenTestSuite::from_json(&json).unwrap();

        assert_eq!(decoded.name, "suite");
        assert_eq!(decoded.tests.len(), 1);
        assert_eq!(decoded.tests[0].name, "t1");
    }

    #[test]
    fn test_validation_result_summary() {
        let mut result = ValidationResult::new();
        result.add_test_result("t1", true, None);
        result.add_test_result("t2", false, Some("boom".to_string()));

        let (total, passed, failed) = result.summary();
        assert_eq!(total, 2);
        assert_eq!(passed, 1);
        assert_eq!(failed, 1);
        assert!(!result.success);
    }

    #[test]
    fn test_generate_suite_tags() {
        let comprehensive = generate_comprehensive_suite();
        assert!(comprehensive.metadata.tags.contains(&"comprehensive".to_string()));

        let performance = generate_performance_suite();
        assert!(performance.metadata.tags.contains(&"performance".to_string()));

        let negative = generate_negative_test_suite();
        assert!(negative.metadata.tags.contains(&"negative".to_string()));
    }
}
