//! Integration tests for ODO and REDEFINES error handling
//!
//! This test suite validates the comprehensive error handling implementation
//! for ODO (OCCURS DEPENDING ON) and REDEFINES clauses according to the
//! normative behavior specified in the design document.

use copybook_codec::{
    DecodeOptions, EncodeOptions, DecodeProcessor, EncodeProcessor,
    validate_odo_counter, validate_redefines_encoding, build_redefines_context,
    handle_missing_counter_field, create_comprehensive_error_context,
};
use copybook_core::{Schema, Field, FieldKind, Occurs, ErrorCode};
use serde_json::json;
use std::io::Cursor;

/// Create a test schema with ODO array for testing
fn create_odo_test_schema() -> Schema {
    let mut schema = Schema::new();
    
    // Counter field
    let counter = Field {
        path: "ROOT.COUNTER".to_string(),
        name: "COUNTER".to_string(),
        level: 5,
        kind: FieldKind::ZonedDecimal { digits: 3, scale: 0, signed: false },
        offset: 0,
        len: 3,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        children: vec![],
    };
    
    // ODO array field
    let array_field = Field {
        path: "ROOT.ITEMS".to_string(),
        name: "ITEMS".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 3,
        len: 10,
        redefines_of: None,
        occurs: Some(Occurs::ODO { 
            min: 0, 
            max: 5, 
            counter_path: "ROOT.COUNTER".to_string() 
        }),
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        children: vec![],
    };
    
    schema.fields = vec![counter, array_field];
    schema.tail_odo = Some(copybook_core::TailODO {
        counter_path: "ROOT.COUNTER".to_string(),
        min_count: 0,
        max_count: 5,
        array_path: "ROOT.ITEMS".to_string(),
    });
    
    // Set LRECL for fixed record processing: 3 (counter) + 5*10 (max array) = 53
    schema.lrecl_fixed = Some(53);
    
    schema
}

/// Create a test schema with REDEFINES for testing
fn create_redefines_test_schema() -> Schema {
    let mut schema = Schema::new();
    
    // Original field
    let field_a = Field {
        path: "ROOT.FIELD_A".to_string(),
        name: "FIELD_A".to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len: 10 },
        offset: 0,
        len: 10,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        children: vec![],
    };
    
    // REDEFINES field B
    let field_b = Field {
        path: "ROOT.FIELD_B".to_string(),
        name: "FIELD_B".to_string(),
        level: 5,
        kind: FieldKind::ZonedDecimal { digits: 5, scale: 0, signed: false },
        offset: 0,
        len: 5,
        redefines_of: Some("ROOT.FIELD_A".to_string()),
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        children: vec![],
    };
    
    // REDEFINES field C
    let field_c = Field {
        path: "ROOT.FIELD_C".to_string(),
        name: "FIELD_C".to_string(),
        level: 5,
        kind: FieldKind::BinaryInt { bits: 32, signed: false },
        offset: 0,
        len: 4,
        redefines_of: Some("ROOT.FIELD_A".to_string()),
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        children: vec![],
    };
    
    schema.fields = vec![field_a, field_b, field_c];
    schema.lrecl_fixed = Some(10); // Based on the largest field (FIELD_A = 10 bytes)
    schema
}

#[test]
fn test_odo_strict_mode_fatal_error() {
    // Test that ODO out-of-bounds is fatal in strict mode (NORMATIVE)
    let result = validate_odo_counter(
        10, // counter value exceeds max
        0,  // min
        5,  // max
        "ROOT.ITEMS",
        "ROOT.COUNTER",
        1,  // record index
        3,  // byte offset
        true, // strict mode
    );
    
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS301_ODO_CLIPPED);
    
    // Verify comprehensive error context
    let context = error.context.unwrap();
    assert_eq!(context.record_index, Some(1));
    assert_eq!(context.field_path, Some("ROOT.ITEMS".to_string()));
    assert_eq!(context.byte_offset, Some(3));
    assert!(context.details.is_some());
    assert!(context.details.unwrap().contains("counter_field=ROOT.COUNTER"));
}

#[test]
fn test_odo_lenient_mode_clamp_and_warn() {
    // Test that ODO out-of-bounds is clamped with warning in lenient mode (NORMATIVE)
    let result = validate_odo_counter(
        10, // counter value exceeds max
        0,  // min
        5,  // max
        "ROOT.ITEMS",
        "ROOT.COUNTER",
        1,  // record index
        3,  // byte offset
        false, // lenient mode
    ).unwrap();
    
    assert_eq!(result.actual_count, 5); // Clamped to max
    assert!(result.was_clamped);
    assert!(result.warning.is_some());
    
    let warning = result.warning.unwrap();
    assert_eq!(warning.code, ErrorCode::CBKS301_ODO_CLIPPED);
    
    // Verify comprehensive error context in warning
    let context = warning.context.unwrap();
    assert_eq!(context.record_index, Some(1));
    assert_eq!(context.field_path, Some("ROOT.ITEMS".to_string()));
    assert_eq!(context.byte_offset, Some(3));
}

#[test]
fn test_odo_lenient_mode_raise_to_minimum() {
    // Test that ODO below minimum is raised with warning in lenient mode (NORMATIVE)
    let result = validate_odo_counter(
        0, // counter value below min
        1, // min
        5, // max
        "ROOT.ITEMS",
        "ROOT.COUNTER",
        1, // record index
        3, // byte offset
        false, // lenient mode
    ).unwrap();
    
    assert_eq!(result.actual_count, 1); // Raised to min
    assert!(result.was_clamped);
    assert!(result.warning.is_some());
    
    let warning = result.warning.unwrap();
    assert_eq!(warning.code, ErrorCode::CBKS302_ODO_RAISED);
}

#[test]
fn test_redefines_ambiguity_detection() {
    let schema = create_redefines_test_schema();
    
    // JSON with multiple non-null REDEFINES views (ambiguous)
    let json_data = json!({
        "FIELD_A": "Hello",
        "FIELD_B": "12345",
        "FIELD_C": null
    });
    
    let context = build_redefines_context(&schema, &json_data);
    
    // Should detect ambiguity for FIELD_A cluster
    let result = validate_redefines_encoding(
        &context,
        "ROOT.FIELD_A", // cluster path
        "ROOT.FIELD_B", // field path
        &json_data,
        false, // use_raw
        1,     // record index
        0,     // byte offset
    );
    
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    assert!(error.message.contains("Ambiguous REDEFINES write"));
    
    // Verify comprehensive error context
    let context = error.context.unwrap();
    assert_eq!(context.record_index, Some(1));
    assert_eq!(context.field_path, Some("ROOT.FIELD_B".to_string()));
    assert_eq!(context.byte_offset, Some(0));
    assert!(context.details.unwrap().contains("cluster_path=ROOT.FIELD_A"));
}

#[test]
fn test_redefines_single_view_allowed() {
    let schema = create_redefines_test_schema();
    
    // JSON with single non-null REDEFINES view (allowed)
    let json_data = json!({
        "FIELD_A": "Hello",
        "FIELD_B": null,
        "FIELD_C": null
    });
    
    let context = build_redefines_context(&schema, &json_data);
    
    // Should allow single non-null view
    let result = validate_redefines_encoding(
        &context,
        "ROOT.FIELD_A", // cluster path
        "ROOT.FIELD_A", // field path
        &json_data,
        false, // use_raw
        1,     // record index
        0,     // byte offset
    );
    
    assert!(result.is_ok());
}

#[test]
fn test_redefines_raw_data_precedence() {
    let schema = create_redefines_test_schema();
    
    // JSON with raw data and multiple views (raw takes precedence)
    let json_data = json!({
        "__raw_b64": "SGVsbG8gV29ybGQ=", // Base64 encoded data
        "FIELD_A": "Hello",
        "FIELD_B": "12345",
        "FIELD_C": null
    });
    
    let context = build_redefines_context(&schema, &json_data);
    
    // Should allow raw data precedence (NORMATIVE step 1)
    let result = validate_redefines_encoding(
        &context,
        "ROOT.FIELD_A", // cluster path
        "ROOT.FIELD_B", // field path
        &json_data,
        true, // use_raw
        1,    // record index
        0,    // byte offset
    );
    
    assert!(result.is_ok());
}

#[test]
fn test_missing_counter_field_comprehensive_error() {
    let schema = Schema::new(); // Empty schema
    
    let error = handle_missing_counter_field(
        "NONEXISTENT_COUNTER",
        "ROOT.ITEMS",
        &schema,
        42, // record index
        100, // byte offset
    );
    
    assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND);
    assert!(error.message.contains("ODO counter field 'NONEXISTENT_COUNTER' not found"));
    
    // Verify comprehensive error context
    let context = error.context.unwrap();
    assert_eq!(context.record_index, Some(42));
    assert_eq!(context.field_path, Some("ROOT.ITEMS".to_string()));
    assert_eq!(context.byte_offset, Some(100));
    assert!(context.details.is_some());
}

#[test]
fn test_comprehensive_error_context_creation() {
    let context = create_comprehensive_error_context(
        123,                    // record index
        "ROOT.TEST_FIELD",     // field path
        456,                   // byte offset
        Some("test details".to_string()), // additional details
    );
    
    assert_eq!(context.record_index, Some(123));
    assert_eq!(context.field_path, Some("ROOT.TEST_FIELD".to_string()));
    assert_eq!(context.byte_offset, Some(456));
    assert_eq!(context.details, Some("test details".to_string()));
    assert!(context.line_number.is_none()); // Not applicable for runtime errors
}

#[test]
fn test_decode_processor_odo_validation() {
    let schema = create_odo_test_schema();
    let options = DecodeOptions {
        strict_mode: false, // Lenient mode
        ..DecodeOptions::default()
    };
    
    // Create test data with counter value exceeding max (should be clamped)
    // Counter field: 3 bytes zoned decimal = "010" (value 10, exceeds max of 5)
    // Array items: 5 * 10 bytes each = 50 bytes total (max allocation)
    let test_data = b"010ITEM1     ITEM2     ITEM3     ITEM4     ITEM5     "; // Counter = 10, exceeds max of 5
    
    let mut processor = DecodeProcessor::new(options);
    let input = Cursor::new(test_data);
    let mut output = Vec::new();
    
    // This should succeed in lenient mode with warnings
    let result = processor.process_file(&schema, input, &mut output);
    
    // The processing should succeed but with warnings
    assert!(result.is_ok());
    let summary = result.unwrap();
    
    // Should have warnings due to ODO clamping
    assert!(summary.has_warnings());
}

#[test]
fn test_encode_processor_redefines_validation() {
    let schema = create_redefines_test_schema();
    let options = EncodeOptions {
        strict_mode: false,
        use_raw: false,
        ..EncodeOptions::default()
    };
    
    // Create JSONL with ambiguous REDEFINES (multiple non-null views)
    let jsonl_data = r#"{"FIELD_A": "Hello", "FIELD_B": "12345", "FIELD_C": null}
"#;
    
    let mut processor = EncodeProcessor::new(options);
    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();
    
    // This should fail due to REDEFINES ambiguity
    let result = processor.process_file(&schema, input, &mut output);
    
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn test_odo_array_length_validation_on_encode() {
    let schema = create_odo_test_schema();
    let options = EncodeOptions {
        strict_mode: true, // Strict mode for encoding
        ..EncodeOptions::default()
    };
    
    // Create JSONL with ODO array exceeding maximum length
    let jsonl_data = r#"{"COUNTER": 3, "ITEMS": ["A", "B", "C", "D", "E", "F"]}
"#; // Array has 6 items but max is 5
    
    let mut processor = EncodeProcessor::new(options);
    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();
    
    // This should fail due to array length exceeding maximum
    let result = processor.process_file(&schema, input, &mut output);
    
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKE521_ARRAY_LEN_OOB);
}

#[test]
fn test_error_context_propagation_in_numeric_decode() {
    // Test that numeric decode errors include proper context
    use copybook_codec::numeric::decode_zoned_decimal;
    use copybook_codec::Codepage;
    
    // Invalid zoned decimal data
    let invalid_data = &[0xFF, 0xFF, 0xFF]; // Invalid zones
    
    let result = decode_zoned_decimal(
        invalid_data,
        3,     // digits
        0,     // scale
        false, // signed
        Codepage::CP037,
        false, // blank_when_zero
    );
    
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
    
    // The error should contain detailed information about the invalid data
    assert!(error.message.contains("Invalid"));
}

#[test]
fn test_normative_behavior_documentation() {
    // This test documents the normative behaviors implemented
    
    // NORMATIVE: ODO strict vs lenient behavior
    // Strict mode: out-of-bounds → fatal error
    let strict_result = validate_odo_counter(10, 0, 5, "ARRAY", "COUNTER", 1, 0, true);
    assert!(strict_result.is_err());
    
    // Lenient mode: out-of-bounds → clamp with warning
    let lenient_result = validate_odo_counter(10, 0, 5, "ARRAY", "COUNTER", 1, 0, false);
    assert!(lenient_result.is_ok());
    assert!(lenient_result.unwrap().was_clamped);
    
    // NORMATIVE: REDEFINES encode precedence
    // 1. Raw data takes precedence
    // 2. Single non-null view allowed
    // 3. Multiple non-null views → error
    
    // NORMATIVE: All CBKD/CBKE errors include comprehensive context
    let context = create_comprehensive_error_context(1, "FIELD", 0, None);
    assert!(context.record_index.is_some());
    assert!(context.field_path.is_some());
    assert!(context.byte_offset.is_some());
}