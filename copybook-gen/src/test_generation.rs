//! Comprehensive test generation utilities
//!
//! This module provides high-level functions for generating complete test suites
//! with all combinations of field types, edge cases, and validation scenarios.

use crate::golden::{GoldenTest, GoldenTestSuite, TestConfig};
use crate::{CopybookTemplate, CorruptionType, GeneratorConfig};
use std::collections::HashMap;

/// Generate a complete test matrix covering all field type combinations
#[must_use] pub fn generate_field_type_matrix() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new(
        "field_type_matrix",
        "Comprehensive test matrix for all field type combinations",
    );

    let config = GeneratorConfig {
        seed: 42,
        record_count: 100,
        include_edge_cases: true,
        include_invalid_data: false,
    };

    // Test each template type
    let templates = vec![
        (CopybookTemplate::Simple, "simple"),
        (CopybookTemplate::WithRedefines, "redefines"),
        (CopybookTemplate::WithOccurs, "occurs"),
        (CopybookTemplate::WithODO, "odo"),
        (CopybookTemplate::WithSync, "sync"),
        (CopybookTemplate::Complex, "complex"),
    ];

    for (template, name) in templates {
        let copybook = crate::copybook::generate_copybook_with_template(&config, template);
        let mut test = GoldenTest::new(&format!("field_matrix_{}", name), &copybook, &[]);
        test.add_tag("field_types");
        test.add_tag(name);
        suite.add_test(test);
    }

    suite
}

/// Generate edge case test suite
#[must_use] pub fn generate_edge_case_suite() -> GoldenTestSuite {
    let mut suite =
        GoldenTestSuite::new("edge_cases", "Edge case test suite for boundary conditions");

    let edge_config = GeneratorConfig {
        seed: 123,
        record_count: 50,
        include_edge_cases: true,
        include_invalid_data: false,
    };

    // Generate edge case tests for each template
    let templates = [CopybookTemplate::Simple,
        CopybookTemplate::WithRedefines,
        CopybookTemplate::WithOccurs,
        CopybookTemplate::WithODO,
        CopybookTemplate::WithSync];

    for (i, template) in templates.iter().enumerate() {
        let copybook = crate::copybook::generate_copybook_with_template(&edge_config, *template);
        let mut test = GoldenTest::new(&format!("edge_case_{}", i + 1), &copybook, &[]);
        test.add_tag("edge_cases");
        test.add_tag("boundary");
        suite.add_test(test);
    }

    suite
}

/// Generate performance test datasets
#[must_use] pub fn generate_performance_datasets() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new(
        "performance_datasets",
        "Performance test datasets for throughput validation",
    );

    // DISPLAY-heavy dataset
    let display_config = GeneratorConfig {
        seed: 456,
        record_count: 10000,
        include_edge_cases: false,
        include_invalid_data: false,
    };

    let display_copybook = crate::copybook::generate_copybook_with_template(
        &display_config,
        CopybookTemplate::DisplayHeavy,
    );
    let mut display_test = GoldenTest::new("display_heavy_10k", &display_copybook, &[]);
    display_test.add_tag("performance");
    display_test.add_tag("display_heavy");
    display_test.add_tag("throughput");
    suite.add_test(display_test);

    // COMP-3-heavy dataset
    let comp3_copybook = crate::copybook::generate_copybook_with_template(
        &display_config,
        CopybookTemplate::Comp3Heavy,
    );
    let mut comp3_test = GoldenTest::new("comp3_heavy_10k", &comp3_copybook, &[]);
    comp3_test.add_tag("performance");
    comp3_test.add_tag("comp3_heavy");
    comp3_test.add_tag("throughput");
    suite.add_test(comp3_test);

    suite
}

/// Generate negative test cases
#[must_use] pub fn generate_negative_test_cases() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new(
        "negative_tests",
        "Negative test cases for error handling validation",
    );

    let config = GeneratorConfig {
        seed: 789,
        record_count: 10,
        include_edge_cases: false,
        include_invalid_data: true,
    };

    // Invalid copybook syntax tests
    let invalid_copybooks = crate::copybook::generate_invalid_copybook(&config);
    for (name, copybook) in invalid_copybooks {
        let mut test = GoldenTest::new(&format!("invalid_{}", name), &copybook, &[]);
        test.add_tag("negative");
        test.add_tag("invalid_syntax");
        test.add_tag("parse_error");
        suite.add_test(test);
    }

    suite
}

/// Generate corruption test cases
#[must_use] pub fn generate_corruption_test_cases() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new("corruption_tests", "Data corruption test cases");

    let config = GeneratorConfig {
        seed: 999,
        record_count: 5,
        include_edge_cases: false,
        include_invalid_data: false,
    };

    // Generate clean data first
    let clean_copybook =
        crate::copybook::generate_copybook_with_template(&config, CopybookTemplate::Simple);
    let clean_data = b"clean test data for corruption testing";

    // Test different corruption types
    let corruption_types = vec![
        (CorruptionType::BitFlip, "bit_flip"),
        (CorruptionType::Truncation, "truncation"),
        (CorruptionType::Padding, "padding"),
        (CorruptionType::AsciiTransfer, "ascii_transfer"),
    ];

    for (corruption_type, name) in corruption_types {
        let corrupted_data = crate::data::generate_corrupted_data(clean_data, corruption_type);
        let mut test = GoldenTest::new(
            &format!("corruption_{}", name),
            &clean_copybook,
            &corrupted_data,
        );
        test.add_tag("corruption");
        test.add_tag("negative");
        test.add_tag(name);
        suite.add_test(test);
    }

    suite
}

/// Generate codepage test matrix
#[must_use] pub fn generate_codepage_test_matrix() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new(
        "codepage_matrix",
        "Test matrix for different codepage configurations",
    );

    let config = GeneratorConfig {
        seed: 111,
        record_count: 100,
        include_edge_cases: true,
        include_invalid_data: false,
    };

    let copybook =
        crate::copybook::generate_copybook_with_template(&config, CopybookTemplate::Simple);

    // Test different codepages
    let codepages = vec!["cp037", "cp273", "cp500", "cp1047", "cp1140", "ascii"];

    for codepage in codepages {
        let test_config = TestConfig {
            codepage: codepage.to_string(),
            record_format: "fixed".to_string(),
            json_number_mode: "lossless".to_string(),
            flags: Vec::new(),
        };

        let mut test = GoldenTest::new_with_config(
            &format!("codepage_{}", codepage),
            &copybook,
            &[],
            test_config,
        );
        test.add_tag("codepage");
        test.add_tag(codepage);
        suite.add_test(test);
    }

    suite
}

/// Generate round-trip fidelity tests
#[must_use] pub fn generate_round_trip_tests() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new(
        "round_trip_fidelity",
        "Round-trip fidelity validation tests",
    );

    let config = GeneratorConfig {
        seed: 222,
        record_count: 50,
        include_edge_cases: true,
        include_invalid_data: false,
    };

    // Test round-trip with different configurations
    let templates = [CopybookTemplate::Simple,
        CopybookTemplate::WithRedefines,
        CopybookTemplate::WithOccurs,
        CopybookTemplate::Complex];

    for (i, template) in templates.iter().enumerate() {
        let copybook = crate::copybook::generate_copybook_with_template(&config, *template);

        // Test with raw capture enabled
        let test_config = TestConfig {
            codepage: "cp037".to_string(),
            record_format: "fixed".to_string(),
            json_number_mode: "lossless".to_string(),
            flags: vec!["--emit-raw=record".to_string(), "--use-raw".to_string()],
        };

        let mut test = GoldenTest::new_with_config(
            &format!("round_trip_{}", i + 1),
            &copybook,
            &[],
            test_config,
        );
        test.add_tag("round_trip");
        test.add_tag("fidelity");
        test.add_tag("raw_capture");
        suite.add_test(test);
    }

    suite
}

/// Generate determinism validation tests
#[must_use] pub fn generate_determinism_tests() -> GoldenTestSuite {
    let mut suite = GoldenTestSuite::new(
        "determinism_validation",
        "Deterministic output validation tests",
    );

    let config = GeneratorConfig {
        seed: 333,
        record_count: 1000,
        include_edge_cases: false,
        include_invalid_data: false,
    };

    let copybook =
        crate::copybook::generate_copybook_with_template(&config, CopybookTemplate::Complex);

    // Test single-threaded vs multi-threaded determinism
    for threads in [1, 2, 4, 8] {
        let test_config = TestConfig {
            codepage: "cp037".to_string(),
            record_format: "fixed".to_string(),
            json_number_mode: "lossless".to_string(),
            flags: vec![format!("--threads={}", threads)],
        };

        let mut test = GoldenTest::new_with_config(
            &format!("determinism_{}threads", threads),
            &copybook,
            &[],
            test_config,
        );
        test.add_tag("determinism");
        test.add_tag("parallel");
        test.add_tag(&format!("threads_{}", threads));
        suite.add_test(test);
    }

    suite
}

/// Generate the complete comprehensive test suite
#[must_use] pub fn generate_complete_test_suite() -> Vec<GoldenTestSuite> {
    vec![
        generate_field_type_matrix(),
        generate_edge_case_suite(),
        generate_performance_datasets(),
        generate_negative_test_cases(),
        generate_corruption_test_cases(),
        generate_codepage_test_matrix(),
        generate_round_trip_tests(),
        generate_determinism_tests(),
    ]
}

/// Test suite statistics
#[derive(Debug, Clone)]
pub struct TestSuiteStats {
    pub total_suites: usize,
    pub total_tests: usize,
    pub tests_by_tag: HashMap<String, usize>,
}

/// Calculate statistics for test suites
#[must_use] pub fn calculate_suite_stats(suites: &[GoldenTestSuite]) -> TestSuiteStats {
    let mut stats = TestSuiteStats {
        total_suites: suites.len(),
        total_tests: 0,
        tests_by_tag: HashMap::new(),
    };

    for suite in suites {
        stats.total_tests += suite.tests.len();

        for test in &suite.tests {
            for tag in &test.metadata.tags {
                *stats.tests_by_tag.entry(tag.clone()).or_insert(0) += 1;
            }
        }
    }

    stats
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_field_type_matrix() {
        let suite = generate_field_type_matrix();
        assert_eq!(suite.name, "field_type_matrix");
        assert!(!suite.tests.is_empty());

        // Should have tests for each template type
        assert!(suite.find_test("field_matrix_simple").is_some());
        assert!(suite.find_test("field_matrix_redefines").is_some());
        assert!(suite.find_test("field_matrix_occurs").is_some());
    }

    #[test]
    fn test_generate_performance_datasets() {
        let suite = generate_performance_datasets();
        assert_eq!(suite.name, "performance_datasets");

        let display_test = suite.find_test("display_heavy_10k");
        assert!(display_test.is_some());
        assert!(display_test.unwrap().has_tag("performance"));
        assert!(display_test.unwrap().has_tag("display_heavy"));

        let comp3_test = suite.find_test("comp3_heavy_10k");
        assert!(comp3_test.is_some());
        assert!(comp3_test.unwrap().has_tag("comp3_heavy"));
    }

    #[test]
    fn test_generate_negative_test_cases() {
        let suite = generate_negative_test_cases();
        assert_eq!(suite.name, "negative_tests");
        assert!(!suite.tests.is_empty());

        // All tests should have negative tag
        for test in &suite.tests {
            assert!(test.has_tag("negative"));
        }
    }

    #[test]
    fn test_generate_codepage_matrix() {
        let suite = generate_codepage_test_matrix();
        assert_eq!(suite.name, "codepage_matrix");

        // Should have tests for each codepage
        assert!(suite.find_test("codepage_cp037").is_some());
        assert!(suite.find_test("codepage_ascii").is_some());
        assert!(suite.find_test("codepage_cp1047").is_some());
    }

    #[test]
    fn test_complete_test_suite_generation() {
        let suites = generate_complete_test_suite();
        assert_eq!(suites.len(), 8); // Should have 8 different suite types

        let stats = calculate_suite_stats(&suites);
        assert!(stats.total_tests > 0);
        assert!(stats.tests_by_tag.contains_key("performance"));
        assert!(stats.tests_by_tag.contains_key("negative"));
        assert!(stats.tests_by_tag.contains_key("determinism"));
    }

    #[test]
    fn test_deterministic_generation() {
        let suite1 = generate_field_type_matrix();
        let suite2 = generate_field_type_matrix();

        // Should generate identical suites
        assert_eq!(suite1.tests.len(), suite2.tests.len());

        for (test1, test2) in suite1.tests.iter().zip(suite2.tests.iter()) {
            assert_eq!(test1.name, test2.name);
            assert_eq!(test1.copybook, test2.copybook);
        }
    }
}
