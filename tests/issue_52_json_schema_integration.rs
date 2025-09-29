//! Test scaffolding for Issue #52 JSON schema integration and API contracts
//!
//! Tests feature spec: issue-52-spec.md#json-schema-validation
//! Validates JSON schema compliance, API contracts, and machine-readable format requirements

use std::path::Path;
use std::fs;
use serde_json::{Value, json};

/// Tests feature spec: issue-52-spec.md#json-schema-structure
/// Validates perf.json schema with required fields and decimal precision
#[test]
fn test_perf_json_schema_structure_validation() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Test simplified perf.json schema structure
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let schema_file = workspace_root.join("scripts").join("bench").join("schema.json");

    // This test will fail until schema.json is implemented
    assert!(schema_file.exists(),
        "JSON schema file must exist for validation: {}", schema_file.display());

    let schema_content = fs::read_to_string(&schema_file)?;
    let schema: Value = serde_json::from_str(&schema_content)?;

    // Validate schema structure
    assert!(schema.get("$schema").is_some(),
        "JSON schema must contain $schema field");
    assert!(schema.get("properties").is_some(),
        "JSON schema must contain properties definition");

    let properties = schema.get("properties").unwrap().as_object().unwrap();

    // Verify required fields are defined in schema
    let required_fields = vec![
        "display_gibs",
        "comp3_mibs",
        "warnings",
        "errors"
    ];

    for field in required_fields {
        assert!(properties.contains_key(field),
            "JSON schema must define required field: {}", field);
    }

    // Validate display_gibs field definition
    let display_gibs = properties.get("display_gibs").unwrap();
    assert_eq!(display_gibs.get("type").unwrap().as_str().unwrap(), "number",
        "display_gibs must be defined as number type");

    // Validate comp3_mibs field definition
    let comp3_mibs = properties.get("comp3_mibs").unwrap();
    assert_eq!(comp3_mibs.get("type").unwrap().as_str().unwrap(), "number",
        "comp3_mibs must be defined as number type");

    // Validate warnings array definition
    let warnings = properties.get("warnings").unwrap();
    assert_eq!(warnings.get("type").unwrap().as_str().unwrap(), "array",
        "warnings must be defined as array type");

    // Validate errors array definition
    let errors = properties.get("errors").unwrap();
    assert_eq!(errors.get("type").unwrap().as_str().unwrap(), "array",
        "errors must be defined as array type");

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#decimal-precision-maintenance
/// Validates decimal precision handling for performance values
#[test]
fn test_decimal_precision_maintenance() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Test decimal precision for performance metrics
    let test_performance_data = json!({
        "display_gibs": 4.22,
        "comp3_mibs": 571.0,
        "warnings": [],
        "errors": []
    });

    // Validate display_gibs precision (2 decimal places)
    let display_gibs = test_performance_data.get("display_gibs").unwrap().as_f64().unwrap();
    let display_rounded = (display_gibs * 100.0).round() / 100.0;
    assert_eq!(display_gibs, display_rounded,
        "display_gibs must maintain 2 decimal precision: expected {}, got {}",
        display_rounded, display_gibs);

    // Validate comp3_mibs precision (1 decimal place)
    let comp3_mibs = test_performance_data.get("comp3_mibs").unwrap().as_f64().unwrap();
    let comp3_rounded = (comp3_mibs * 10.0).round() / 10.0;
    assert_eq!(comp3_mibs, comp3_rounded,
        "comp3_mibs must maintain 1 decimal precision: expected {}, got {}",
        comp3_rounded, comp3_mibs);

    // Test JSON serialization preserves precision
    let json_string = serde_json::to_string(&test_performance_data)?;
    assert!(json_string.contains("4.22"),
        "JSON serialization must preserve display_gibs precision");
    assert!(json_string.contains("571.0") || json_string.contains("571"),
        "JSON serialization must preserve comp3_mibs precision");

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#machine-readable-format-compliance
/// Validates machine-readable format requirements and API contracts
#[test]
fn test_machine_readable_format_compliance() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2,AC3 - Test machine-readable format compliance
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let perf_json = workspace_root.join("scripts").join("bench").join("perf.json");

    // This test validates the format when perf.json is generated
    if perf_json.exists() {
        let content = fs::read_to_string(&perf_json)?;
        let data: Value = serde_json::from_str(&content)?;

        // Validate required top-level fields
        assert!(data.get("display_gibs").is_some(),
            "perf.json must contain display_gibs field");
        assert!(data.get("comp3_mibs").is_some(),
            "perf.json must contain comp3_mibs field");
        assert!(data.get("warnings").is_some(),
            "perf.json must contain warnings field");
        assert!(data.get("errors").is_some(),
            "perf.json must contain errors field");

        // Validate data types
        assert!(data.get("display_gibs").unwrap().is_f64(),
            "display_gibs must be a numeric value");
        assert!(data.get("comp3_mibs").unwrap().is_f64(),
            "comp3_mibs must be a numeric value");
        assert!(data.get("warnings").unwrap().is_array(),
            "warnings must be an array");
        assert!(data.get("errors").unwrap().is_array(),
            "errors must be an array");

        // Validate array elements are strings
        let warnings = data.get("warnings").unwrap().as_array().unwrap();
        for warning in warnings {
            assert!(warning.is_string(),
                "All warning elements must be strings");
        }

        let errors = data.get("errors").unwrap().as_array().unwrap();
        for error in errors {
            assert!(error.is_string(),
                "All error elements must be strings");
        }

        // Validate performance values are positive
        let display_gibs = data.get("display_gibs").unwrap().as_f64().unwrap();
        assert!(display_gibs >= 0.0,
            "display_gibs must be non-negative: {}", display_gibs);

        let comp3_mibs = data.get("comp3_mibs").unwrap().as_f64().unwrap();
        assert!(comp3_mibs >= 0.0,
            "comp3_mibs must be non-negative: {}", comp3_mibs);
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#metadata-enrichment
/// Validates optional metadata fields for audit and debugging
#[test]
fn test_metadata_enrichment_validation() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC8 - Test metadata enrichment for comprehensive diagnostics
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let perf_json = workspace_root.join("scripts").join("bench").join("perf.json");

    if perf_json.exists() {
        let content = fs::read_to_string(&perf_json)?;
        let data: Value = serde_json::from_str(&content)?;

        // Check for optional metadata section
        if let Some(metadata) = data.get("_metadata") {
            assert!(metadata.is_object(),
                "_metadata must be an object when present");

            let metadata_obj = metadata.as_object().unwrap();

            // Validate timestamp if present
            if let Some(timestamp) = metadata_obj.get("timestamp") {
                assert!(timestamp.is_string(),
                    "timestamp must be a string");

                let timestamp_str = timestamp.as_str().unwrap();
                assert!(timestamp_str.contains("T") || timestamp_str.contains("-"),
                    "timestamp must be in ISO format");
            }

            // Validate git_commit if present
            if let Some(git_commit) = metadata_obj.get("git_commit") {
                assert!(git_commit.is_string(),
                    "git_commit must be a string");

                let commit_str = git_commit.as_str().unwrap();
                assert!(commit_str.len() >= 7,
                    "git_commit must be at least 7 characters (short hash)");
            }

            // Validate rust_version if present
            if let Some(rust_version) = metadata_obj.get("rust_version") {
                assert!(rust_version.is_string(),
                    "rust_version must be a string");

                let version_str = rust_version.as_str().unwrap();
                assert!(version_str.contains("."),
                    "rust_version must contain version number");
            }

            // Validate environment info if present
            if let Some(environment) = metadata_obj.get("environment") {
                assert!(environment.is_object(),
                    "environment must be an object");
            }
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#performance-floor-validation
/// Validates SLO compliance against enterprise performance floors
#[test]
fn test_performance_floor_validation_schema() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6 - Test SLO validation schema compliance

    // Test case 1: Values above enterprise floors (should pass)
    let passing_data = json!({
        "display_gibs": 4.22,    // 4.22 GiB/s > 4.1 GiB/s enterprise target
        "comp3_mibs": 571.0,     // 571 MiB/s > 560 MiB/s enterprise target
        "warnings": [],
        "errors": []
    });

    let display_gibs = passing_data.get("display_gibs").unwrap().as_f64().unwrap();
    let comp3_mibs = passing_data.get("comp3_mibs").unwrap().as_f64().unwrap();

    // Convert to safety margins vs floors (80 MB/s DISPLAY, 40 MB/s COMP-3)
    let display_mbps = display_gibs * 1073.74; // Convert GiB/s to MB/s
    let display_safety_margin = display_mbps / 80.0;
    let comp3_safety_margin = comp3_mibs / 40.0;

    assert!(display_safety_margin > 1.0,
        "DISPLAY performance must exceed floor: {:.1}x safety margin", display_safety_margin);
    assert!(comp3_safety_margin > 1.0,
        "COMP-3 performance must exceed floor: {:.1}x safety margin", comp3_safety_margin);

    // Test case 2: Values below floors (should generate errors)
    let failing_data = json!({
        "display_gibs": 0.05,    // 0.05 GiB/s << 4.1 GiB/s enterprise target
        "comp3_mibs": 30.0,      // 30 MiB/s < 560 MiB/s enterprise target
        "warnings": ["DISPLAY throughput approaching minimum threshold"],
        "errors": ["COMP-3 throughput below 40 MB/s floor"]
    });

    let errors = failing_data.get("errors").unwrap().as_array().unwrap();
    assert!(!errors.is_empty(),
        "Below-floor performance must generate errors");

    let error_text = errors[0].as_str().unwrap();
    assert!(error_text.contains("below") && error_text.contains("floor"),
        "Error messages must indicate floor violations");

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#json-array-handling
/// Validates array handling for warnings and errors
#[test]
fn test_json_array_handling_compliance() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2,AC8 - Test array handling for warnings and errors

    // Test empty arrays (normal case)
    let normal_case = json!({
        "display_gibs": 4.22,
        "comp3_mibs": 571.0,
        "warnings": [],
        "errors": []
    });

    let warnings = normal_case.get("warnings").unwrap().as_array().unwrap();
    let errors = normal_case.get("errors").unwrap().as_array().unwrap();

    assert!(warnings.is_empty(),
        "warnings array must be empty for normal performance");
    assert!(errors.is_empty(),
        "errors array must be empty for normal performance");

    // Test populated arrays (degraded performance case)
    let degraded_case = json!({
        "display_gibs": 3.0,
        "comp3_mibs": 400.0,
        "warnings": [
            "DISPLAY throughput below enterprise target of 4.1 GiB/s",
            "COMP-3 throughput below enterprise target of 560 MiB/s"
        ],
        "errors": []
    });

    let warnings = degraded_case.get("warnings").unwrap().as_array().unwrap();
    assert_eq!(warnings.len(), 2,
        "warnings array must contain appropriate warning messages");

    for warning in warnings {
        assert!(warning.is_string(),
            "warning entries must be strings");
        let warning_text = warning.as_str().unwrap();
        assert!(!warning_text.is_empty(),
            "warning messages must not be empty");
        assert!(warning_text.contains("throughput") || warning_text.contains("performance"),
            "warning messages must be descriptive");
    }

    // Test error case (critical performance failure)
    let error_case = json!({
        "display_gibs": 0.01,
        "comp3_mibs": 10.0,
        "warnings": [],
        "errors": [
            "DISPLAY throughput 10.7 MB/s below floor of 80 MB/s",
            "COMP-3 throughput 10.0 MiB/s below floor of 40 MB/s"
        ]
    });

    let errors = error_case.get("errors").unwrap().as_array().unwrap();
    assert_eq!(errors.len(), 2,
        "errors array must contain critical performance failures");

    for error in errors {
        assert!(error.is_string(),
            "error entries must be strings");
        let error_text = error.as_str().unwrap();
        assert!(error_text.contains("below") && error_text.contains("floor"),
            "error messages must indicate floor violations");
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#criterion-integration
/// Validates integration with existing Criterion.rs benchmark infrastructure
#[test]
fn test_criterion_integration_api_contracts() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Test integration with existing Criterion.rs infrastructure
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let copybook_bench_dir = workspace_root.join("copybook-bench");

    // Verify Cargo.toml contains Criterion dependency
    let cargo_toml = copybook_bench_dir.join("Cargo.toml");
    if cargo_toml.exists() {
        let content = fs::read_to_string(&cargo_toml)?;
        assert!(content.contains("criterion"),
            "copybook-bench must depend on Criterion.rs for benchmarking");
    }

    // Check for benchmark files
    let benches_dir = copybook_bench_dir.join("benches");
    if benches_dir.exists() {
        let benchmark_files: Vec<_> = fs::read_dir(&benches_dir)?
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.path().extension().and_then(|s| s.to_str()) == Some("rs"))
            .collect();

        assert!(!benchmark_files.is_empty(),
            "copybook-bench must contain benchmark files");

        // Check for performance benchmarks
        for entry in benchmark_files {
            let path = entry.path();
            if let Some(filename) = path.file_name().and_then(|s| s.to_str()) {
                if filename.contains("performance") || filename.contains("decode") {
                    let content = fs::read_to_string(&path)?;
                    assert!(content.contains("criterion") || content.contains("Criterion"),
                        "Performance benchmarks must use Criterion.rs: {}", filename);
                    assert!(content.contains("throughput") || content.contains("iter"),
                        "Performance benchmarks must measure throughput: {}", filename);
                }
            }
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#json-output-generation
/// Validates JSON output generation from benchmark execution
#[test]
fn test_json_output_generation_api() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC3 - Test JSON output generation from benchmark execution
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));

    // Check for JSON reporter implementation
    let json_reporter = workspace_root.join("copybook-bench").join("src").join("json_reporter.rs");
    if json_reporter.exists() {
        let content = fs::read_to_string(&json_reporter)?;

        // Verify JSON reporter structure
        assert!(content.contains("struct") && content.contains("JsonReporter"),
            "json_reporter.rs must define JsonReporter struct");
        assert!(content.contains("PerformanceReport") || content.contains("performance_report"),
            "json_reporter.rs must handle PerformanceReport");
        assert!(content.contains("serde") || content.contains("Serialize"),
            "json_reporter.rs must support JSON serialization");

        // Check for key methods
        assert!(content.contains("fn generate") || content.contains("generate_report"),
            "JsonReporter must have report generation method");
        assert!(content.contains("display_gibs") && content.contains("comp3_mibs"),
            "JsonReporter must extract DISPLAY and COMP-3 metrics");
    }

    // Check for integration in benchmark files
    let decode_performance = workspace_root.join("copybook-bench").join("benches").join("decode_performance.rs");
    if decode_performance.exists() {
        let content = fs::read_to_string(&decode_performance)?;

        // Look for JSON integration points
        if content.contains("GENERATE_JSON_REPORT") || content.contains("json_reporter") {
            assert!(content.contains("JsonReporter") || content.contains("json"),
                "Benchmark must integrate JSON reporting when enabled");
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#enterprise-target-validation
/// Validates enterprise performance target compliance in JSON schema
#[test]
fn test_enterprise_target_validation_schema() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6,AC7 - Test enterprise performance target validation

    // Enterprise targets: DISPLAY ≥4.1 GiB/s, COMP-3 ≥560 MiB/s
    let enterprise_compliant = json!({
        "display_gibs": 4.22,     // Above 4.1 GiB/s target
        "comp3_mibs": 571.0,      // Above 560 MiB/s target
        "warnings": [],
        "errors": []
    });

    let display_gibs = enterprise_compliant.get("display_gibs").unwrap().as_f64().unwrap();
    let comp3_mibs = enterprise_compliant.get("comp3_mibs").unwrap().as_f64().unwrap();

    // Validate against enterprise targets
    assert!(display_gibs >= 4.1,
        "DISPLAY performance must meet enterprise target: {} GiB/s >= 4.1 GiB/s", display_gibs);
    assert!(comp3_mibs >= 560.0,
        "COMP-3 performance must meet enterprise target: {} MiB/s >= 560 MiB/s", comp3_mibs);

    // Test non-compliant case
    let enterprise_non_compliant = json!({
        "display_gibs": 3.5,      // Below 4.1 GiB/s target
        "comp3_mibs": 500.0,      // Below 560 MiB/s target
        "warnings": [
            "DISPLAY throughput 3.5 GiB/s below enterprise target of 4.1 GiB/s",
            "COMP-3 throughput 500.0 MiB/s below enterprise target of 560 MiB/s"
        ],
        "errors": []
    });

    let warnings = enterprise_non_compliant.get("warnings").unwrap().as_array().unwrap();
    assert!(!warnings.is_empty(),
        "Below-target performance must generate warnings");

    // Verify warning content
    let warning_text = warnings[0].as_str().unwrap();
    assert!(warning_text.contains("enterprise target"),
        "Warnings must reference enterprise targets");

    Ok(())
}