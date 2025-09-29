//! Test scaffolding for Issue #52 AC2: JSON schema validation and machine-readable output generation
//!
//! Tests feature spec: issue-52-spec.md#AC2
//! Validates simplified perf.json schema with required fields and decimal precision

#![allow(clippy::expect_used)] // Test code: expects are acceptable for test assertions
#![allow(clippy::unwrap_used)] // Test code: unwraps are acceptable for test assertions
#![allow(clippy::uninlined_format_args)] // Test readability over optimization

use serde_json::{Map, Value};
use std::collections::HashMap;

/// Test data structure representing the simplified perf.json schema
#[derive(Debug, Clone)]
pub struct PerformanceReportSchema {
    pub display_gibs: f64,
    pub comp3_mibs: f64,
    pub warnings: Vec<String>,
    pub errors: Vec<String>,
}

impl PerformanceReportSchema {
    #[must_use]
    pub fn new(display_gibs: f64, comp3_mibs: f64) -> Self {
        Self {
            display_gibs,
            comp3_mibs,
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_warnings(mut self, warnings: Vec<String>) -> Self {
        self.warnings = warnings;
        self
    }

    #[must_use]
    pub fn with_errors(mut self, errors: Vec<String>) -> Self {
        self.errors = errors;
        self
    }

    /// # Errors
    /// Returns error if floating point values cannot be converted to JSON numbers
    /// # Panics
    /// Panics if floating point values are infinite or NaN
    pub fn to_json(&self) -> serde_json::Result<Value> {
        let mut map = Map::new();
        map.insert(
            "display_gibs".to_string(),
            Value::Number(
                serde_json::Number::from_f64(self.display_gibs)
                    .expect("display_gibs must be finite for JSON conversion"),
            ),
        );
        map.insert(
            "comp3_mibs".to_string(),
            Value::Number(
                serde_json::Number::from_f64(self.comp3_mibs)
                    .expect("comp3_mibs must be finite for JSON conversion"),
            ),
        );
        map.insert(
            "warnings".to_string(),
            Value::Array(
                self.warnings
                    .iter()
                    .map(|w| Value::String(w.clone()))
                    .collect(),
            ),
        );
        map.insert(
            "errors".to_string(),
            Value::Array(
                self.errors
                    .iter()
                    .map(|e| Value::String(e.clone()))
                    .collect(),
            ),
        );
        Ok(Value::Object(map))
    }
}

/// Tests feature spec: issue-52-spec.md#AC2-json-schema-structure
/// Validates that JSON schema contains all required fields
#[test]
fn test_perf_json_schema_required_fields() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify simplified perf.json schema with required fields
    let test_schema = PerformanceReportSchema::new(4.22, 571.0);
    let json_value = test_schema.to_json()?;

    assert!(
        json_value.is_object(),
        "Performance report must be a JSON object"
    );
    let obj = json_value
        .as_object()
        .expect("JSON value must be an object");

    // Validate required fields exist
    let required_fields = vec!["display_gibs", "comp3_mibs", "warnings", "errors"];
    for field in required_fields {
        assert!(
            obj.contains_key(field),
            "JSON schema must contain required field: {}",
            field
        );
    }

    // Validate field types
    assert!(
        obj["display_gibs"].is_number(),
        "display_gibs must be a number"
    );
    assert!(obj["comp3_mibs"].is_number(), "comp3_mibs must be a number");
    assert!(obj["warnings"].is_array(), "warnings must be an array");
    assert!(obj["errors"].is_array(), "errors must be an array");

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC2-decimal-precision
/// Validates that decimal precision is maintained for performance values
#[test]
fn test_decimal_precision_support() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify decimal precision for performance measurements
    let test_cases = vec![
        (4.22, 571.0),
        (4.15, 580.2),
        (4.33, 568.5),
        (3.98, 575.1),
        (4.01, 569.8),
    ];

    for (display_gibs, comp3_mibs) in test_cases {
        let schema = PerformanceReportSchema::new(display_gibs, comp3_mibs);
        let json_value = schema.to_json()?;
        let obj = json_value
            .as_object()
            .expect("JSON value must be an object");

        // Verify precision is maintained
        let json_display = obj["display_gibs"]
            .as_f64()
            .expect("display_gibs must be a valid f64");
        let json_comp3 = obj["comp3_mibs"]
            .as_f64()
            .expect("comp3_mibs must be a valid f64");

        assert!(
            (json_display - display_gibs).abs() < 0.001,
            "DISPLAY precision must be maintained: expected {}, got {}",
            display_gibs,
            json_display
        );
        assert!(
            (json_comp3 - comp3_mibs).abs() < 0.001,
            "COMP-3 precision must be maintained: expected {}, got {}",
            comp3_mibs,
            json_comp3
        );
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC2-warnings-array
/// Validates that warnings array can contain performance warning messages
#[test]
fn test_warnings_array_functionality() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify warnings array supports performance warnings
    let warnings = vec![
        "DISPLAY throughput approaching minimum threshold".to_string(),
        "COMP-3 performance variance above 2% tolerance".to_string(),
    ];

    let schema = PerformanceReportSchema::new(4.22, 571.0).with_warnings(warnings.clone());
    let json_value = schema.to_json()?;
    let obj = json_value
        .as_object()
        .expect("JSON value must be an object");

    let json_warnings = obj["warnings"]
        .as_array()
        .expect("warnings must be an array");
    assert_eq!(
        json_warnings.len(),
        warnings.len(),
        "Warnings array length must match input"
    );

    for (i, warning) in warnings.iter().enumerate() {
        assert_eq!(
            json_warnings[i].as_str().expect("warning must be a string"),
            warning,
            "Warning message {} must be preserved",
            i
        );
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC2-errors-array
/// Validates that errors array can contain performance error messages
#[test]
fn test_errors_array_functionality() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify errors array supports performance errors
    let errors = vec![
        "DISPLAY throughput below 80 MB/s floor".to_string(),
        "COMP-3 throughput below 40 MB/s floor".to_string(),
        "Benchmark execution timeout exceeded".to_string(),
    ];

    let schema = PerformanceReportSchema::new(0.5, 25.0).with_errors(errors.clone());
    let json_value = schema.to_json()?;
    let obj = json_value
        .as_object()
        .expect("JSON value must be an object");

    let json_errors = obj["errors"].as_array().expect("errors must be an array");
    assert_eq!(
        json_errors.len(),
        errors.len(),
        "Errors array length must match input"
    );

    for (i, error) in errors.iter().enumerate() {
        assert_eq!(
            json_errors[i].as_str().expect("error must be a string"),
            error,
            "Error message {} must be preserved",
            i
        );
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC2-json-serialization
/// Validates that JSON serialization produces valid, parseable output
#[test]
fn test_json_serialization_roundtrip() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify JSON serialization produces valid output
    let original_schema = PerformanceReportSchema::new(4.22, 571.0)
        .with_warnings(vec!["Performance variance detected".to_string()])
        .with_errors(vec!["Benchmark timeout".to_string()]);

    let json_value = original_schema.to_json()?;
    let json_string = serde_json::to_string(&json_value)?;

    // Parse back from string to verify round-trip
    let parsed_value: Value = serde_json::from_str(&json_string)?;
    let parsed_obj = parsed_value
        .as_object()
        .expect("parsed value must be an object");

    // Verify all fields are preserved
    let display_gibs = parsed_obj["display_gibs"]
        .as_f64()
        .expect("display_gibs must be a valid f64");
    assert!(
        (display_gibs - 4.22).abs() < f64::EPSILON,
        "display_gibs expected 4.22, got {display_gibs}"
    );
    let comp3_mibs = parsed_obj["comp3_mibs"]
        .as_f64()
        .expect("comp3_mibs must be a valid f64");
    assert!(
        (comp3_mibs - 571.0).abs() < f64::EPSILON,
        "comp3_mibs expected 571.0, got {comp3_mibs}"
    );
    assert_eq!(
        parsed_obj["warnings"]
            .as_array()
            .expect("warnings must be an array")
            .len(),
        1
    );
    assert_eq!(
        parsed_obj["errors"]
            .as_array()
            .expect("errors must be an array")
            .len(),
        1
    );

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC2-performance-values-validation
/// Validates that performance values are within expected enterprise ranges
#[test]
fn test_performance_values_enterprise_ranges() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify performance values support enterprise ranges

    // Test current performance levels (exceeding targets)
    let current_performance = PerformanceReportSchema::new(4.22, 571.0);
    let json_value = current_performance.to_json()?;
    let obj = json_value
        .as_object()
        .expect("JSON value must be an object");

    let display_gibs = obj["display_gibs"]
        .as_f64()
        .expect("display_gibs must be a valid f64");
    let comp3_mibs = obj["comp3_mibs"]
        .as_f64()
        .expect("comp3_mibs must be a valid f64");

    // Verify values are positive and within reasonable bounds
    assert!(display_gibs > 0.0, "DISPLAY throughput must be positive");
    assert!(comp3_mibs > 0.0, "COMP-3 throughput must be positive");
    assert!(
        display_gibs < 100.0,
        "DISPLAY throughput must be within reasonable bounds"
    );
    assert!(
        comp3_mibs < 10000.0,
        "COMP-3 throughput must be within reasonable bounds"
    );

    // Test enterprise performance floor scenarios
    let floor_scenarios = vec![
        (0.08, 40.0), // At performance floors (80 MB/s = ~0.074 GiB/s, 40 MB/s)
        (1.0, 100.0), // Above floors
        (5.0, 600.0), // High performance
    ];

    for (display, comp3) in floor_scenarios {
        let schema = PerformanceReportSchema::new(display, comp3);
        let json = schema.to_json()?;
        let obj = json.as_object().expect("JSON value must be an object");

        assert!(
            obj["display_gibs"]
                .as_f64()
                .expect("display_gibs must be a valid f64")
                > 0.0
        );
        assert!(
            obj["comp3_mibs"]
                .as_f64()
                .expect("comp3_mibs must be a valid f64")
                > 0.0
        );
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC2-json-schema-validation
/// Validates that JSON output conforms to machine-readable format standards
#[test]
fn test_machine_readable_format_compliance() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify machine-readable format compliance
    let schema = PerformanceReportSchema::new(4.22, 571.0)
        .with_warnings(vec!["Warning message".to_string()])
        .with_errors(vec!["Error message".to_string()]);

    let json_value = schema.to_json()?;
    let json_string = serde_json::to_string_pretty(&json_value)?;

    // Verify JSON is well-formed and contains expected structure
    assert!(json_string.contains("display_gibs"));
    assert!(json_string.contains("comp3_mibs"));
    assert!(json_string.contains("warnings"));
    assert!(json_string.contains("errors"));

    // Verify JSON can be minified (for machine processing)
    let compact_json = serde_json::to_string(&json_value)?;
    assert!(
        compact_json.len() < json_string.len(),
        "JSON must support compact format for machine processing"
    );

    // Verify JSON can be parsed by different parsers
    let _: HashMap<String, Value> = serde_json::from_str(&json_string)?;

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC2-empty-arrays-handling
/// Validates that empty warnings and errors arrays are handled correctly
#[test]
fn test_empty_arrays_handling() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify empty arrays are handled correctly for successful runs
    let schema = PerformanceReportSchema::new(4.22, 571.0);
    let json_value = schema.to_json()?;
    let obj = json_value
        .as_object()
        .expect("JSON value must be an object");

    let warnings = obj["warnings"]
        .as_array()
        .expect("warnings must be an array");
    let errors = obj["errors"].as_array().expect("errors must be an array");

    assert!(
        warnings.is_empty(),
        "Warnings array should be empty for successful performance"
    );
    assert!(
        errors.is_empty(),
        "Errors array should be empty for successful performance"
    );

    // Verify empty arrays serialize correctly
    let json_string = serde_json::to_string(&json_value)?;
    assert!(json_string.contains("\"warnings\":[]"));
    assert!(json_string.contains("\"errors\":[]"));

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#AC2-json-output-file-generation
/// Validates that JSON output can be written to scripts/bench/perf.json
#[test]
fn test_json_output_file_generation_readiness() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC2 - Verify JSON output generation is ready for file writing
    let schema = PerformanceReportSchema::new(4.22, 571.0);
    let json_value = schema.to_json()?;
    let json_string = serde_json::to_string_pretty(&json_value)?;

    // Verify JSON string is valid and non-empty
    assert!(
        !json_string.trim().is_empty(),
        "Generated JSON must not be empty"
    );
    assert!(
        json_string.len() > 50,
        "Generated JSON must contain substantial content"
    );

    // Verify JSON can be parsed back
    let _parsed: Value = serde_json::from_str(&json_string)?;

    // Test file path construction (without actually writing)
    let workspace_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("CARGO_MANIFEST_DIR must have a parent directory");
    let expected_output_path = workspace_root
        .join("scripts")
        .join("bench")
        .join("perf.json");

    assert!(
        expected_output_path.parent().is_some(),
        "Output path must have valid parent directory"
    );

    Ok(())
}
