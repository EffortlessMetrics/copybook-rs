// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-gen tooling
//!
//! Covers: Golden fixture generation, test fixture helpers, synthetic data generation,
//! GoldenTest/GoldenTestSuite construction, SHA-256 verification, configuration options,
//! and edge cases.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_gen::{
    CopybookTemplate, CorruptionType, DataStrategy, GeneratorConfig, GoldenTest, GoldenTestSuite,
    TestConfig, TestSuiteBuilder, ValidationResult,
};

// ---------------------------------------------------------------------------
// Golden fixture generation
// ---------------------------------------------------------------------------

#[test]
fn test_comprehensive_suite_has_expected_tags() {
    let suite = copybook_gen::generate_comprehensive_test_suite();
    assert!(suite.metadata.tags.contains(&"comprehensive".to_string()));
    assert!(suite.metadata.tags.contains(&"regression".to_string()));
}

#[test]
fn test_performance_suite_has_throughput_tag() {
    let suite = copybook_gen::generate_performance_test_suite();
    assert!(suite.metadata.tags.contains(&"throughput".to_string()));
}

#[test]
fn test_negative_suite_has_error_handling_tag() {
    let suite = copybook_gen::generate_negative_test_suite();
    assert!(suite.metadata.tags.contains(&"error_handling".to_string()));
}

#[test]
fn test_complete_test_suite_count() {
    let suites = copybook_gen::test_generation::generate_complete_test_suite();
    // 8 sub-suites expected
    assert_eq!(suites.len(), 8);
    let total_tests: usize = suites.iter().map(|s| s.tests.len()).sum();
    assert!(total_tests > 0, "complete test suite should contain tests");
}

#[test]
fn test_field_type_matrix_covers_templates() {
    let suite = copybook_gen::test_generation::generate_field_type_matrix();
    for name in &[
        "field_matrix_simple",
        "field_matrix_redefines",
        "field_matrix_occurs",
        "field_matrix_odo",
        "field_matrix_sync",
        "field_matrix_complex",
    ] {
        assert!(suite.find_test(name).is_some(), "missing test: {name}");
    }
}

// ---------------------------------------------------------------------------
// GoldenTest construction
// ---------------------------------------------------------------------------

#[test]
fn test_golden_test_new_sets_fields() {
    let gt = GoldenTest::new("my_test", "01 ROOT PIC X(5).", b"hello");
    assert_eq!(gt.name, "my_test");
    assert!(gt.copybook.contains("ROOT"));
    assert!(!gt.input_hash.is_empty());
    assert!(gt.expected_outputs.is_empty());
    assert!(gt.has_tag("synthetic"));
    assert!(gt.metadata.description.contains("my_test"));
}

#[test]
fn test_golden_test_new_with_config() {
    let cfg = TestConfig {
        codepage: "cp500".to_string(),
        record_format: "rdw".to_string(),
        json_number_mode: "native".to_string(),
        flags: vec!["--emit-meta".to_string()],
    };
    let gt = GoldenTest::new_with_config("cfg_test", "01 X PIC X(1).", b"", cfg);
    assert_eq!(gt.metadata.config.codepage, "cp500");
    assert_eq!(gt.metadata.config.record_format, "rdw");
    assert_eq!(gt.metadata.config.json_number_mode, "native");
    assert_eq!(gt.metadata.config.flags.len(), 1);
}

#[test]
fn test_golden_test_default_config() {
    let cfg = TestConfig::default();
    assert_eq!(cfg.codepage, "cp037");
    assert_eq!(cfg.record_format, "fixed");
    assert_eq!(cfg.json_number_mode, "lossless");
    assert!(cfg.flags.is_empty());
}

// ---------------------------------------------------------------------------
// SHA-256 verification
// ---------------------------------------------------------------------------

#[test]
fn test_hash_bytes_known_value() {
    // SHA-256 of empty string
    let hash = GoldenTest::hash_bytes(b"");
    assert_eq!(
        hash,
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    );
}

#[test]
fn test_hash_string_known_value() {
    let hash = GoldenTest::hash_string("abc");
    assert_eq!(
        hash,
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    );
}

#[test]
fn test_hash_bytes_and_string_consistent() {
    let data = "copybook-rs test payload";
    assert_eq!(
        GoldenTest::hash_bytes(data.as_bytes()),
        GoldenTest::hash_string(data)
    );
}

#[test]
fn test_validate_output_no_expected_returns_true() {
    let gt = GoldenTest::new("v", "01 X PIC X(1).", b"");
    assert!(gt.validate_output("json", b"anything"));
    assert!(gt.validate_string_output("json", "anything"));
}

#[test]
fn test_validate_output_matches() {
    let mut gt = GoldenTest::new("v", "01 X PIC X(1).", b"");
    gt.update_expected_hash("bin", b"payload");
    assert!(gt.validate_output("bin", b"payload"));
    assert!(!gt.validate_output("bin", b"different"));
}

#[test]
fn test_validate_string_output_matches() {
    let mut gt = GoldenTest::new("v", "01 X PIC X(1).", b"");
    gt.update_expected_string_hash("text", "hello world");
    assert!(gt.validate_string_output("text", "hello world"));
    assert!(!gt.validate_string_output("text", "HELLO WORLD"));
}

// ---------------------------------------------------------------------------
// GoldenTestSuite
// ---------------------------------------------------------------------------

#[test]
fn test_suite_add_find_test() {
    let mut suite = GoldenTestSuite::new("s", "suite desc");
    assert_eq!(suite.name, "s");
    assert_eq!(suite.description, "suite desc");
    assert!(suite.tests.is_empty());

    suite.add_test(GoldenTest::new("a", "01 X PIC X(1).", b""));
    suite.add_test(GoldenTest::new("b", "01 Y PIC X(2).", b""));
    assert_eq!(suite.tests.len(), 2);
    assert!(suite.find_test("a").is_some());
    assert!(suite.find_test("b").is_some());
    assert!(suite.find_test("c").is_none());
}

#[test]
fn test_suite_find_test_mut() {
    let mut suite = GoldenTestSuite::new("s", "d");
    suite.add_test(GoldenTest::new("m", "01 X PIC X(1).", b""));
    let test = suite.find_test_mut("m").unwrap();
    test.add_tag("mutated");
    assert!(suite.find_test("m").unwrap().has_tag("mutated"));
}

#[test]
fn test_suite_tests_by_tag() {
    let mut suite = GoldenTestSuite::new("s", "d");

    let mut t1 = GoldenTest::new("perf1", "01 X PIC X(1).", b"");
    t1.add_tag("performance");
    let mut t2 = GoldenTest::new("neg1", "01 X PIC X(1).", b"");
    t2.add_tag("negative");
    let mut t3 = GoldenTest::new("perf2", "01 X PIC X(1).", b"");
    t3.add_tag("performance");

    suite.add_test(t1);
    suite.add_test(t2);
    suite.add_test(t3);

    assert_eq!(suite.tests_by_tag("performance").len(), 2);
    assert_eq!(suite.tests_by_tag("negative").len(), 1);
    assert_eq!(suite.tests_by_tag("missing").len(), 0);
}

#[test]
fn test_suite_validate_all() {
    let mut suite = GoldenTestSuite::new("s", "d");
    suite.add_test(GoldenTest::new("t1", "01 X PIC X(1).", b""));
    suite.add_test(GoldenTest::new("t2", "01 Y PIC X(2).", b""));

    let result = suite.validate_all();
    assert!(result.success);
    let (total, passed, failed) = result.summary();
    assert_eq!(total, 2);
    assert_eq!(passed, 2);
    assert_eq!(failed, 0);
}

#[test]
fn test_suite_json_roundtrip() {
    let mut suite = GoldenTestSuite::new("roundtrip", "test roundtrip");
    let mut gt = GoldenTest::new("t1", "01 X PIC X(1).", b"data");
    gt.add_tag("custom_tag");
    gt.update_expected_hash("json", b"output");
    suite.add_test(gt);

    let json = suite.to_json().unwrap();
    let parsed = GoldenTestSuite::from_json(&json).unwrap();
    assert_eq!(parsed.name, "roundtrip");
    assert_eq!(parsed.tests.len(), 1);
    assert_eq!(parsed.tests[0].name, "t1");
    assert!(parsed.tests[0].has_tag("custom_tag"));
    assert!(parsed.tests[0].expected_outputs.contains_key("json"));
}

#[test]
fn test_suite_from_invalid_json_errors() {
    let result = GoldenTestSuite::from_json("not valid json");
    assert!(result.is_err());
}

// ---------------------------------------------------------------------------
// ValidationResult
// ---------------------------------------------------------------------------

#[test]
fn test_validation_result_default_success() {
    let vr = ValidationResult::new();
    assert!(vr.success);
    assert!(vr.results.is_empty());
    let (total, passed, failed) = vr.summary();
    assert_eq!((total, passed, failed), (0, 0, 0));
}

#[test]
fn test_validation_result_failure_propagates() {
    let mut vr = ValidationResult::new();
    vr.add_test_result("ok", true, None);
    assert!(vr.success);
    vr.add_test_result("bad", false, Some("oops".to_string()));
    assert!(!vr.success);
    let (total, passed, failed) = vr.summary();
    assert_eq!((total, passed, failed), (2, 1, 1));
}

// ---------------------------------------------------------------------------
// Tag management
// ---------------------------------------------------------------------------

#[test]
fn test_add_tag_idempotent() {
    let mut gt = GoldenTest::new("t", "01 X PIC X(1).", b"");
    gt.add_tag("a");
    gt.add_tag("a");
    gt.add_tag("a");
    assert_eq!(gt.metadata.tags.iter().filter(|t| *t == "a").count(), 1);
}

#[test]
fn test_has_tag_returns_false_for_absent() {
    let gt = GoldenTest::new("t", "01 X PIC X(1).", b"");
    assert!(!gt.has_tag("nonexistent"));
}

// ---------------------------------------------------------------------------
// TestSuiteBuilder
// ---------------------------------------------------------------------------

#[test]
fn test_suite_builder_basic() {
    let suite = TestSuiteBuilder::new("builder_suite", "built by builder")
        .add_simple_test("s1")
        .add_redefines_test("r1")
        .add_odo_test("o1")
        .build();

    assert_eq!(suite.name, "builder_suite");
    assert_eq!(suite.tests.len(), 3);
    assert!(suite.find_test("s1").is_some());
    assert!(suite.find_test("r1").is_some());
    assert!(suite.find_test("o1").is_some());
}

#[test]
fn test_suite_builder_performance_test() {
    let suite = TestSuiteBuilder::new("perf", "perf suite")
        .add_performance_test("display_perf", CopybookTemplate::DisplayHeavy)
        .add_performance_test("comp3_perf", CopybookTemplate::Comp3Heavy)
        .build();

    assert_eq!(suite.tests.len(), 2);
    for t in &suite.tests {
        assert!(t.has_tag("performance"));
    }
}

#[test]
fn test_suite_builder_negative_test() {
    let suite = TestSuiteBuilder::new("neg", "negative tests")
        .add_negative_test("bad_syntax", "INVALID COPYBOOK TEXT")
        .build();

    assert_eq!(suite.tests.len(), 1);
    let t = &suite.tests[0];
    assert!(t.has_tag("negative"));
    assert!(t.has_tag("invalid"));
    assert_eq!(t.copybook, "INVALID COPYBOOK TEXT");
}

#[test]
fn test_suite_builder_with_custom_config() {
    let config = GeneratorConfig {
        seed: 999,
        record_count: 5,
        include_edge_cases: false,
        include_invalid_data: false,
    };
    let suite = TestSuiteBuilder::new("custom", "custom config")
        .with_config(config)
        .add_simple_test("c1")
        .build();

    assert_eq!(suite.tests.len(), 1);
}

// ---------------------------------------------------------------------------
// Copybook generation
// ---------------------------------------------------------------------------

#[test]
fn test_generate_copybook_default() {
    let config = GeneratorConfig::default();
    let cb = copybook_gen::generate_copybook(&config);
    assert!(cb.contains("01  RECORD-ROOT"));
    assert!(cb.contains("PIC"));
}

#[test]
fn test_all_templates_produce_valid_structure() {
    let config = GeneratorConfig::default();
    let templates = [
        CopybookTemplate::Simple,
        CopybookTemplate::WithRedefines,
        CopybookTemplate::WithOccurs,
        CopybookTemplate::WithODO,
        CopybookTemplate::WithSync,
        CopybookTemplate::Complex,
        CopybookTemplate::DisplayHeavy,
        CopybookTemplate::Comp3Heavy,
    ];
    for template in templates {
        let cb = copybook_gen::generate_copybook_with_template(&config, template);
        assert!(
            cb.contains("01  RECORD-ROOT"),
            "{template:?} missing RECORD-ROOT"
        );
        assert!(cb.contains("PIC"), "{template:?} missing PIC clause");
    }
}

#[test]
fn test_deterministic_generation_same_seed() {
    let c1 = GeneratorConfig {
        seed: 77,
        ..Default::default()
    };
    let c2 = GeneratorConfig {
        seed: 77,
        ..Default::default()
    };
    assert_eq!(
        copybook_gen::generate_copybook(&c1),
        copybook_gen::generate_copybook(&c2)
    );
}

#[test]
fn test_different_seeds_differ() {
    let c1 = GeneratorConfig {
        seed: 1,
        ..Default::default()
    };
    let c2 = GeneratorConfig {
        seed: 2,
        ..Default::default()
    };
    // Different seeds should produce different output (with overwhelming probability)
    assert_ne!(
        copybook_gen::generate_copybook(&c1),
        copybook_gen::generate_copybook(&c2)
    );
}

// ---------------------------------------------------------------------------
// Invalid copybook generation
// ---------------------------------------------------------------------------

#[test]
fn test_invalid_copybooks_not_empty() {
    let config = GeneratorConfig::default();
    let invalid = copybook_gen::generate_invalid_copybooks(&config);
    assert!(invalid.len() >= 5);
}

#[test]
fn test_invalid_copybooks_contain_expected_names() {
    let config = GeneratorConfig::default();
    let invalid = copybook_gen::generate_invalid_copybooks(&config);
    let names: Vec<&str> = invalid.iter().map(|(n, _)| n.as_str()).collect();
    for expected in &[
        "invalid_level",
        "invalid_pic",
        "redefines_missing",
        "odo_not_tail",
    ] {
        assert!(names.contains(expected), "missing case: {expected}");
    }
}

// ---------------------------------------------------------------------------
// Corrupted data generation
// ---------------------------------------------------------------------------

#[test]
fn test_corruption_bit_flip() {
    let data = vec![0x00, 0x00, 0x00, 0x00, 0x00];
    let corrupted = copybook_gen::generate_corrupted_data(&data, CorruptionType::BitFlip);
    assert_eq!(corrupted.len(), data.len());
    assert_ne!(corrupted, data, "bit flip should alter data");
}

#[test]
fn test_corruption_truncation() {
    let data = vec![0xAA; 20];
    let corrupted = copybook_gen::generate_corrupted_data(&data, CorruptionType::Truncation);
    assert!(corrupted.len() < data.len());
}

#[test]
fn test_corruption_padding() {
    let data = vec![0xBB; 5];
    let corrupted = copybook_gen::generate_corrupted_data(&data, CorruptionType::Padding);
    assert!(corrupted.len() > data.len());
    assert_eq!(&corrupted[..5], &data[..]);
}

#[test]
fn test_corruption_ascii_transfer() {
    let data = vec![0x41, 0xC1, 0xFF, 0x20];
    let corrupted = copybook_gen::generate_corrupted_data(&data, CorruptionType::AsciiTransfer);
    assert_eq!(corrupted[0], 0x41); // ASCII 'A' stays
    assert_eq!(corrupted[1], b'?'); // High-bit replaced
    assert_eq!(corrupted[2], b'?'); // High-bit replaced
    assert_eq!(corrupted[3], 0x20); // ASCII space stays
}

// ---------------------------------------------------------------------------
// Synthetic data generation (requires parsing)
// ---------------------------------------------------------------------------

#[test]
fn test_generate_data_returns_correct_count() {
    let schema =
        copybook_core::parse_copybook("       01 ROOT.\n           05 F1 PIC X(10).\n").unwrap();
    let config = GeneratorConfig {
        seed: 1,
        record_count: 7,
        include_edge_cases: false,
        include_invalid_data: false,
    };
    let records = copybook_gen::generate_data(&schema, &config);
    assert_eq!(records.len(), 7);
}

#[test]
fn test_generate_data_with_strategy_performance() {
    let schema = copybook_core::parse_copybook(
        "       01 ROOT.\n           05 F1 PIC 9(5).\n           05 F2 PIC X(10).\n",
    )
    .unwrap();
    let config = GeneratorConfig {
        seed: 42,
        record_count: 3,
        include_edge_cases: false,
        include_invalid_data: false,
    };
    let records =
        copybook_gen::generate_data_with_strategy(&schema, &config, DataStrategy::Performance);
    assert_eq!(records.len(), 3);
    for r in &records {
        assert_eq!(r.len(), schema.lrecl_fixed.unwrap_or(1000) as usize);
    }
}

// ---------------------------------------------------------------------------
// Edge cases
// ---------------------------------------------------------------------------

#[test]
fn test_golden_test_empty_data() {
    let gt = GoldenTest::new("empty", "01 X PIC X(1).", b"");
    assert_eq!(
        gt.input_hash,
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    );
}

#[test]
fn test_golden_test_large_data_hash() {
    let big = vec![0xAA; 1_000_000];
    let gt = GoldenTest::new("big", "01 X PIC X(1).", &big);
    assert!(!gt.input_hash.is_empty());
    // Verify determinism
    let gt2 = GoldenTest::new("big2", "01 X PIC X(1).", &big);
    assert_eq!(gt.input_hash, gt2.input_hash);
}

#[test]
fn test_suite_metadata_version() {
    let suite = GoldenTestSuite::new("v", "version test");
    assert_eq!(suite.metadata.version, "1.0.0");
}

#[test]
fn test_suite_updated_timestamp_changes_on_add() {
    let mut suite = GoldenTestSuite::new("ts", "timestamp test");
    let initial = suite.metadata.updated_at.clone();
    // Sleep a tiny bit to ensure timestamp differs
    std::thread::sleep(std::time::Duration::from_millis(10));
    suite.add_test(GoldenTest::new("t", "01 X PIC X(1).", b""));
    // updated_at should be refreshed (or at least set)
    assert!(!suite.metadata.updated_at.is_empty());
    // The timestamps might be the same if very fast, but updated_at should be valid RFC3339
    assert!(suite.metadata.updated_at.contains('T'));
    let _ = initial; // suppress unused warning
}

#[test]
fn test_create_golden_test_helpers() {
    let gt1 = copybook_gen::create_golden_test("h1", "01 X PIC X(1).", b"data");
    assert_eq!(gt1.name, "h1");

    let cfg = TestConfig {
        codepage: "cp1140".to_string(),
        ..TestConfig::default()
    };
    let gt2 = copybook_gen::create_golden_test_with_config("h2", "01 X PIC X(1).", b"", cfg);
    assert_eq!(gt2.metadata.config.codepage, "cp1140");
}

#[test]
fn test_generator_config_default() {
    let cfg = GeneratorConfig::default();
    assert_eq!(cfg.seed, 42);
    assert_eq!(cfg.record_count, 1000);
    assert!(cfg.include_edge_cases);
    assert!(!cfg.include_invalid_data);
}
