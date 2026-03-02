// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for fixture generation in copybook-gen.
//!
//! Covers: copybook text generation for common patterns, binary data generation,
//! golden fixture creation/verification, SHA-256 hashing, test suite management,
//! fixture file I/O, enterprise scenario generation, and edge-case fixtures.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_gen::{
    CopybookTemplate, CorruptionType, DataStrategy, GeneratorConfig, GoldenTest, GoldenTestSuite,
    TestConfig, TestSuiteBuilder, ValidationResult,
};

// ---------------------------------------------------------------------------
// 1. Copybook text generation for common patterns
// ---------------------------------------------------------------------------

#[test]
fn gen_simple_copybook_contains_pic_clauses() {
    let config = GeneratorConfig::default();
    let cb = copybook_gen::generate_copybook_with_template(&config, CopybookTemplate::Simple);
    // Simple template should produce alphanumeric, zoned, packed, binary and group fields
    assert!(cb.contains("PIC X("));
    assert!(cb.contains("PIC") && (cb.contains("9(") || cb.contains("COMP")));
}

#[test]
fn gen_redefines_template_has_multiple_views() {
    let config = GeneratorConfig::default();
    let cb =
        copybook_gen::generate_copybook_with_template(&config, CopybookTemplate::WithRedefines);
    // Should have at least 2 REDEFINES clauses
    let count = cb.matches("REDEFINES").count();
    assert!(count >= 2, "expected >=2 REDEFINES, got {count}");
}

#[test]
fn gen_odo_template_has_depending_on_clause() {
    let config = GeneratorConfig::default();
    let cb = copybook_gen::generate_copybook_with_template(&config, CopybookTemplate::WithODO);
    assert!(cb.contains("DEPENDING ON"));
    assert!(cb.contains("OCCURS"));
}

#[test]
fn gen_display_heavy_has_at_least_50_fields() {
    let config = GeneratorConfig::default();
    let cb = copybook_gen::generate_copybook_with_template(&config, CopybookTemplate::DisplayHeavy);
    let pic_count = cb.matches("PIC").count();
    assert!(pic_count >= 50, "expected >=50 PIC, got {pic_count}");
}

#[test]
fn gen_comp3_heavy_has_packed_decimal_fields() {
    let config = GeneratorConfig::default();
    let cb = copybook_gen::generate_copybook_with_template(&config, CopybookTemplate::Comp3Heavy);
    let comp3_count = cb.matches("COMP-3").count();
    assert!(comp3_count >= 30, "expected >=30 COMP-3, got {comp3_count}");
}

// ---------------------------------------------------------------------------
// 2. Binary test data generation matching a schema
// ---------------------------------------------------------------------------

#[test]
fn gen_data_normal_strategy_produces_valid_length() {
    let schema =
        copybook_core::parse_copybook("       01 ROOT.\n           05 F1 PIC X(20).\n").unwrap();
    let config = GeneratorConfig {
        seed: 10,
        record_count: 5,
        include_edge_cases: false,
        include_invalid_data: false,
    };
    let records = copybook_gen::generate_data(&schema, &config);
    assert_eq!(records.len(), 5);
    let expected_len = schema.lrecl_fixed.unwrap() as usize;
    for r in &records {
        assert_eq!(r.len(), expected_len);
    }
}

#[test]
fn gen_data_edge_cases_strategy_runs_without_panic() {
    let schema = copybook_core::parse_copybook(
        "       01 ROOT.\n           05 F1 PIC 9(5).\n           05 F2 PIC X(10).\n",
    )
    .unwrap();
    let config = GeneratorConfig {
        seed: 20,
        record_count: 10,
        include_edge_cases: true,
        include_invalid_data: false,
    };
    let records =
        copybook_gen::generate_data_with_strategy(&schema, &config, DataStrategy::EdgeCases);
    assert_eq!(records.len(), 10);
}

#[test]
fn gen_data_invalid_strategy_produces_records() {
    let schema =
        copybook_core::parse_copybook("       01 ROOT.\n           05 NUM PIC S9(7) COMP-3.\n")
            .unwrap();
    let config = GeneratorConfig {
        seed: 30,
        record_count: 3,
        include_edge_cases: false,
        include_invalid_data: true,
    };
    let records =
        copybook_gen::generate_data_with_strategy(&schema, &config, DataStrategy::Invalid);
    assert_eq!(records.len(), 3);
}

#[test]
fn gen_data_deterministic_across_calls() {
    let schema =
        copybook_core::parse_copybook("       01 ROOT.\n           05 F1 PIC X(8).\n").unwrap();
    let config = GeneratorConfig {
        seed: 42,
        record_count: 5,
        include_edge_cases: false,
        include_invalid_data: false,
    };
    let r1 = copybook_gen::generate_data(&schema, &config);
    let r2 = copybook_gen::generate_data(&schema, &config);
    assert_eq!(r1, r2, "same seed should produce identical data");
}

// ---------------------------------------------------------------------------
// 3. Golden fixture creation and verification
// ---------------------------------------------------------------------------

#[test]
fn golden_test_stores_copybook_and_hash() {
    let gt = copybook_gen::create_golden_test("fixture_1", "01 ROOT PIC X(10).", b"binary data");
    assert_eq!(gt.name, "fixture_1");
    assert!(gt.copybook.contains("ROOT"));
    assert!(!gt.input_hash.is_empty());
}

#[test]
fn golden_test_validate_passes_first_run() {
    let gt = GoldenTest::new("first_run", "01 X PIC X(1).", b"");
    // No expected hash stored yet → validation passes
    assert!(gt.validate_output("json", b"anything"));
    assert!(gt.validate_string_output("text", "anything"));
}

#[test]
fn golden_test_validate_fails_on_mismatch() {
    let mut gt = GoldenTest::new("mismatch", "01 X PIC X(1).", b"");
    gt.update_expected_hash("bin", b"expected_output");
    assert!(!gt.validate_output("bin", b"wrong_output"));
}

// ---------------------------------------------------------------------------
// 4. SHA-256 hash computation for fixtures
// ---------------------------------------------------------------------------

#[test]
fn sha256_empty_data() {
    assert_eq!(
        GoldenTest::hash_bytes(b""),
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    );
}

#[test]
fn sha256_known_abc() {
    assert_eq!(
        GoldenTest::hash_string("abc"),
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    );
}

#[test]
fn sha256_bytes_and_string_are_consistent() {
    let payload = "copybook enterprise data";
    assert_eq!(
        GoldenTest::hash_bytes(payload.as_bytes()),
        GoldenTest::hash_string(payload)
    );
}

#[test]
fn sha256_different_inputs_produce_different_hashes() {
    let h1 = GoldenTest::hash_bytes(b"alpha");
    let h2 = GoldenTest::hash_bytes(b"bravo");
    assert_ne!(h1, h2);
}

// ---------------------------------------------------------------------------
// 5. Test suite creation and management
// ---------------------------------------------------------------------------

#[test]
fn suite_add_and_count() {
    let mut suite = GoldenTestSuite::new("mgmt", "management tests");
    for i in 0..5 {
        suite.add_test(GoldenTest::new(&format!("t{i}"), "01 X PIC X(1).", b""));
    }
    assert_eq!(suite.tests.len(), 5);
}

#[test]
fn suite_find_test_by_name() {
    let mut suite = GoldenTestSuite::new("s", "d");
    suite.add_test(GoldenTest::new("alpha", "01 X PIC X(1).", b""));
    suite.add_test(GoldenTest::new("beta", "01 X PIC X(1).", b""));
    assert!(suite.find_test("alpha").is_some());
    assert!(suite.find_test("beta").is_some());
    assert!(suite.find_test("gamma").is_none());
}

#[test]
fn suite_tests_by_tag_filters_correctly() {
    let mut suite = GoldenTestSuite::new("s", "d");
    let mut t1 = GoldenTest::new("p1", "01 X PIC X(1).", b"");
    t1.add_tag("perf");
    let t2 = GoldenTest::new("n1", "01 X PIC X(1).", b"");
    suite.add_test(t1);
    suite.add_test(t2);
    assert_eq!(suite.tests_by_tag("perf").len(), 1);
    assert_eq!(suite.tests_by_tag("nonexistent").len(), 0);
}

#[test]
fn suite_validate_all_returns_summary() {
    let mut suite = GoldenTestSuite::new("s", "d");
    suite.add_test(GoldenTest::new("a", "01 X PIC X(1).", b""));
    suite.add_test(GoldenTest::new("b", "01 X PIC X(1).", b""));
    let result = suite.validate_all();
    let (total, passed, _) = result.summary();
    assert_eq!(total, 2);
    assert_eq!(passed, 2);
    assert!(result.success);
}

#[test]
fn suite_builder_creates_mixed_suite() {
    let suite = TestSuiteBuilder::new("mixed", "mixed suite")
        .add_simple_test("s1")
        .add_redefines_test("r1")
        .add_odo_test("o1")
        .add_performance_test("perf1", CopybookTemplate::DisplayHeavy)
        .add_negative_test("neg1", "INVALID")
        .build();
    assert_eq!(suite.tests.len(), 5);
    assert!(suite.find_test("perf1").unwrap().has_tag("performance"));
    assert!(suite.find_test("neg1").unwrap().has_tag("negative"));
}

// ---------------------------------------------------------------------------
// 6. Fixture file I/O (write and read back via JSON)
// ---------------------------------------------------------------------------

#[test]
fn suite_json_roundtrip_preserves_all_fields() {
    let mut suite = GoldenTestSuite::new("io_test", "I/O roundtrip");
    let mut gt = GoldenTest::new("t1", "01 ROOT PIC X(5).", b"data");
    gt.add_tag("io");
    gt.update_expected_hash("json", b"expected");
    gt.update_expected_string_hash("text", "expected_text");
    suite.add_test(gt);

    let json = suite.to_json().unwrap();
    let loaded = GoldenTestSuite::from_json(&json).unwrap();

    assert_eq!(loaded.name, "io_test");
    assert_eq!(loaded.tests.len(), 1);
    let t = &loaded.tests[0];
    assert_eq!(t.name, "t1");
    assert!(t.has_tag("io"));
    assert!(t.expected_outputs.contains_key("json"));
    assert!(t.expected_outputs.contains_key("text"));
}

#[test]
fn suite_json_invalid_input_returns_error() {
    assert!(GoldenTestSuite::from_json("{invalid").is_err());
}

#[test]
fn suite_json_empty_suite_roundtrip() {
    let suite = GoldenTestSuite::new("empty", "no tests");
    let json = suite.to_json().unwrap();
    let loaded = GoldenTestSuite::from_json(&json).unwrap();
    assert!(loaded.tests.is_empty());
    assert_eq!(loaded.name, "empty");
}

// ---------------------------------------------------------------------------
// 7. Enterprise scenario generation (banking, insurance, retail)
// ---------------------------------------------------------------------------

#[test]
fn enterprise_generator_produces_customer_record() {
    use copybook_gen::{
        CobolDialect, EnterpriseFixtureConfig, EnterprisePattern, EnterprisePatternGenerator,
        MainframeTarget, ScaleFactor, StandardEnterpriseGenerator,
    };

    let generator_config = GeneratorConfig::default();
    let ent_config = EnterpriseFixtureConfig {
        mainframe_targets: vec![MainframeTarget::IbmZ],
        cobol_dialects: vec![CobolDialect::Enterprise],
        scale_factors: vec![ScaleFactor::Small],
        data_patterns: vec![EnterprisePattern::CustomerRecord],
        random_seed: Some(42),
    };
    let generator = StandardEnterpriseGenerator::new(generator_config, ent_config);

    let cb = generator
        .generate_enterprise_copybook(&EnterprisePattern::CustomerRecord)
        .unwrap();
    assert!(cb.contains("CUSTOMER-RECORD"));
    assert!(cb.contains("CUSTOMER-ID"));
    assert!(cb.contains("COMP-3"));
}

#[test]
fn enterprise_generator_produces_banking_record() {
    use copybook_gen::{
        CobolDialect, EnterpriseFixtureConfig, EnterprisePattern, EnterprisePatternGenerator,
        MainframeTarget, ScaleFactor, StandardEnterpriseGenerator,
    };

    let generator = StandardEnterpriseGenerator::new(
        GeneratorConfig::default(),
        EnterpriseFixtureConfig {
            mainframe_targets: vec![MainframeTarget::IbmZ],
            cobol_dialects: vec![CobolDialect::Cobol85],
            scale_factors: vec![ScaleFactor::Medium],
            data_patterns: vec![EnterprisePattern::BankingRecord],
            random_seed: Some(1),
        },
    );

    let cb = generator
        .generate_enterprise_copybook(&EnterprisePattern::BankingRecord)
        .unwrap();
    assert!(cb.contains("BANKING-ACCOUNT"));
    assert!(cb.contains("IBAN"));
    assert!(cb.contains("CURRENT-BALANCE"));
}

#[test]
fn enterprise_generator_produces_insurance_claim() {
    use copybook_gen::{
        CobolDialect, EnterpriseFixtureConfig, EnterprisePattern, EnterprisePatternGenerator,
        MainframeTarget, ScaleFactor, StandardEnterpriseGenerator,
    };

    let generator = StandardEnterpriseGenerator::new(
        GeneratorConfig::default(),
        EnterpriseFixtureConfig {
            mainframe_targets: vec![MainframeTarget::Ibm390],
            cobol_dialects: vec![CobolDialect::Cobol2002],
            scale_factors: vec![ScaleFactor::Small],
            data_patterns: vec![EnterprisePattern::InsuranceClaim],
            random_seed: Some(99),
        },
    );

    let cb = generator
        .generate_enterprise_copybook(&EnterprisePattern::InsuranceClaim)
        .unwrap();
    assert!(cb.contains("INSURANCE-CLAIM"));
    assert!(cb.contains("CLAIMED-AMOUNT"));
}

#[test]
fn enterprise_generator_produces_inventory_data() {
    use copybook_gen::{
        CobolDialect, EnterpriseFixtureConfig, EnterprisePattern, EnterprisePatternGenerator,
        MainframeTarget, ScaleFactor, StandardEnterpriseGenerator,
    };

    let generator = StandardEnterpriseGenerator::new(
        GeneratorConfig::default(),
        EnterpriseFixtureConfig {
            mainframe_targets: vec![MainframeTarget::IbmZ],
            cobol_dialects: vec![CobolDialect::Enterprise],
            scale_factors: vec![ScaleFactor::Large],
            data_patterns: vec![EnterprisePattern::InventoryData],
            random_seed: Some(7),
        },
    );

    let cb = generator
        .generate_enterprise_copybook(&EnterprisePattern::InventoryData)
        .unwrap();
    assert!(cb.contains("INVENTORY-RECORD"));
    assert!(cb.contains("ITEM-SKU"));
}

// ---------------------------------------------------------------------------
// 8. Edge case fixture generation (empty, single-field, max-fields)
// ---------------------------------------------------------------------------

#[test]
fn edge_case_empty_data_golden_test() {
    let gt = GoldenTest::new("empty_data", "01 ROOT PIC X(1).", b"");
    assert_eq!(gt.input_hash, GoldenTest::hash_bytes(b""));
}

#[test]
fn edge_case_single_field_copybook() {
    // Minimal single-field copybook should parse and generate data
    let schema =
        copybook_core::parse_copybook("       01 ROOT.\n           05 ONLY PIC X(1).\n").unwrap();
    let config = GeneratorConfig {
        seed: 1,
        record_count: 1,
        include_edge_cases: false,
        include_invalid_data: false,
    };
    let records = copybook_gen::generate_data(&schema, &config);
    assert_eq!(records.len(), 1);
    assert_eq!(records[0].len(), schema.lrecl_fixed.unwrap() as usize);
}

#[test]
fn edge_case_corruption_on_empty_data() {
    // Bit flip on empty data should be no-op
    let empty: Vec<u8> = vec![];
    let flipped = copybook_gen::generate_corrupted_data(&empty, CorruptionType::BitFlip);
    assert!(flipped.is_empty());
}

#[test]
fn edge_case_corruption_truncation_short_data() {
    // Truncation on short data (<=10 bytes) should not truncate
    let short = vec![0xAA; 5];
    let result = copybook_gen::generate_corrupted_data(&short, CorruptionType::Truncation);
    assert_eq!(
        result.len(),
        5,
        "short data should not be further truncated"
    );
}

#[test]
fn edge_case_validation_result_all_pass() {
    let mut vr = ValidationResult::new();
    for i in 0..10 {
        vr.add_test_result(&format!("test_{i}"), true, None);
    }
    assert!(vr.success);
    let (total, passed, failed) = vr.summary();
    assert_eq!((total, passed, failed), (10, 10, 0));
}

#[test]
fn edge_case_validation_result_all_fail() {
    let mut vr = ValidationResult::new();
    for i in 0..3 {
        vr.add_test_result(&format!("fail_{i}"), false, Some(format!("err {i}")));
    }
    assert!(!vr.success);
    let (total, passed, failed) = vr.summary();
    assert_eq!((total, passed, failed), (3, 0, 3));
}

#[test]
fn edge_case_large_suite_json_roundtrip() {
    let mut suite = GoldenTestSuite::new("large", "large suite");
    for i in 0..100 {
        let mut gt = GoldenTest::new(&format!("t{i}"), "01 X PIC X(1).", b"");
        gt.add_tag(&format!("group_{}", i % 5));
        suite.add_test(gt);
    }
    let json = suite.to_json().unwrap();
    let loaded = GoldenTestSuite::from_json(&json).unwrap();
    assert_eq!(loaded.tests.len(), 100);
}

#[test]
fn edge_case_test_config_custom_flags() {
    let cfg = TestConfig {
        codepage: "ascii".to_string(),
        record_format: "rdw".to_string(),
        json_number_mode: "native".to_string(),
        flags: vec![
            "--emit-meta".to_string(),
            "--threads=8".to_string(),
            "--dialect=0".to_string(),
        ],
    };
    let gt = GoldenTest::new_with_config("flags", "01 X PIC X(1).", b"", cfg);
    assert_eq!(gt.metadata.config.flags.len(), 3);
    assert_eq!(gt.metadata.config.codepage, "ascii");
}

#[test]
fn edge_case_complete_test_suite_stats() {
    let suites = copybook_gen::test_generation::generate_complete_test_suite();
    let stats = copybook_gen::test_generation::calculate_suite_stats(&suites);
    assert_eq!(stats.total_suites, 8);
    assert!(stats.total_tests > 0);
    // Should have entries for common tags
    assert!(stats.tests_by_tag.contains_key("negative"));
    assert!(stats.tests_by_tag.contains_key("performance"));
}
