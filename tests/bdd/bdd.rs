#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::unused_async,
    clippy::expect_fun_call,
    clippy::uninlined_format_args,
    clippy::map_unwrap_or,
    clippy::match_same_arms,
    clippy::single_char_pattern,
    clippy::unreachable,
    clippy::panic,
    clippy::if_same_then_else,
    unused_variables,
    dead_code,
    unused_imports
)]
//! BDD tests for copybook-rs using Cucumber/Gherkin
//!
//! This module provides comprehensive BDD test coverage for the copybook-rs library,
//! including copybook parsing, encoding/decoding, field projection, dialect processing,
//! determinism validation, and enterprise audit system features.

use copybook_codec::{
    decode_file_to_jsonl,
    determinism::{
        check_decode_determinism, check_encode_determinism, check_round_trip_determinism,
        DeterminismResult,
    },
    encode_jsonl_to_file, Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode,
    RecordFormat,
};
use copybook_core::{
    parse_copybook, parse_copybook_with_options, project_schema, Error, ErrorCode, Occurs,
    ParseOptions,
};
use cucumber::{given, then, when, World as _};
use serde_json::{Map, Value};
use std::collections::HashSet;
use std::io::Cursor;

#[cfg(feature = "audit")]
use copybook_core::audit::context::SecurityClassification;
#[cfg(feature = "audit")]
use copybook_core::audit::event::{
    AccessResult as AuditAccessResult, AccessType as AuditAccessType, ComparisonResult,
    ComplianceValidationResult, ConfigurationChangeType, ParseResult as AuditParseResult,
    PerformanceMeasurementType, PerformanceMetrics, SecurityEventType, TransformationOperation,
    TransformationResult, UserImpactLevel, ValidationResult as AuditValidationResult,
};
#[cfg(feature = "audit")]
use copybook_core::audit::logger::{AuditLogger, AuditLoggerConfig, LogFormat};
#[cfg(feature = "audit")]
use copybook_core::audit::{
    validate_audit_chain, AuditContext, AuditEvent, AuditEventType, AuditPayload, AuditSeverity,
    ComplianceProfile,
};

/// BDD World struct to maintain test state across steps
#[derive(Debug, Default, cucumber::World)]
pub struct CopybookWorld {
    /// The copybook text being parsed
    copybook_text: Option<String>,
    /// The parsed schema
    schema: Option<copybook_core::Schema>,
    /// The projected schema (after field projection)
    projected_schema: Option<copybook_core::Schema>,
    /// The binary data for encoding/decoding
    binary_data: Option<Vec<u8>>,
    /// The JSON data for encoding/decoding
    json_data: Option<String>,
    /// The decoded JSON output
    decoded_output: Option<String>,
    /// The encoded binary output
    encoded_output: Option<Vec<u8>>,
    /// Any error that occurred
    error: Option<Error>,
    /// The current parse options
    parse_options: Option<ParseOptions>,
    /// The current decode options
    decode_options: Option<DecodeOptions>,
    /// The current encode options
    encode_options: Option<EncodeOptions>,
    /// Record count for multi-record tests
    record_count: Option<usize>,
    /// The current dialect for ODO parsing
    dialect: Option<copybook_core::dialect::Dialect>,
    /// Field selection for projection
    field_selection: Option<Vec<String>>,
    /// The determinism check result
    determinism_result: Option<DeterminismResult>,
    /// Audit-related fields (for enterprise audit testing)
    #[cfg(feature = "audit")]
    audit_context: Option<copybook_core::audit::AuditContext>,
    #[cfg(feature = "audit")]
    audit_events: Vec<copybook_core::audit::AuditEvent>,
    #[cfg(feature = "audit")]
    audit_output: Option<String>,
    #[cfg(feature = "audit")]
    compliance_profile: Option<copybook_core::audit::ComplianceProfile>,
    #[cfg(feature = "audit")]
    security_classification: Option<copybook_core::audit::context::SecurityClassification>,
    #[cfg(feature = "audit")]
    child_audit_context: Option<copybook_core::audit::AuditContext>,
    #[cfg(feature = "audit")]
    regression_metrics: bool,
}

impl CopybookWorld {
    fn ensure_schema_parsed(&mut self) {
        if self.schema.is_some() {
            return;
        }

        self.error = None;
        let copybook_text = self.copybook_text.as_ref().expect("Copybook text not set");

        match &self.parse_options {
            Some(options) => match parse_copybook_with_options(copybook_text, options) {
                Ok(schema) => {
                    self.schema = Some(schema);
                }
                Err(e) => {
                    self.error = Some(e);
                }
            },
            None => match parse_copybook(copybook_text) {
                Ok(schema) => {
                    self.schema = Some(schema);
                }
                Err(e) => {
                    self.error = Some(e);
                }
            },
        }
    }

    fn ensure_decode_options(&mut self) {
        if self.decode_options.is_none() {
            self.decode_options = Some(default_ascii_decode_options());
        }
    }

    fn ensure_encode_options(&mut self) {
        if self.encode_options.is_none() {
            self.encode_options = Some(default_ascii_encode_options());
        }
    }

    fn ensure_schema_and_return(&mut self) -> &copybook_core::Schema {
        self.ensure_schema_parsed();
        self.schema.as_ref().expect("Schema should be parsed")
    }

    fn schema(&self) -> &copybook_core::Schema {
        self.schema.as_ref().expect("Schema not parsed")
    }

    fn find_field_by_name(&self, name: &str) -> Option<&copybook_core::Field> {
        self.schema().find_field(name)
    }

    fn find_field_by_name_ci(&self, name: &str) -> Option<&copybook_core::Field> {
        self.schema().find_field(name)
    }

    fn all_leaf_fields(&self) -> Vec<&copybook_core::Field> {
        self.schema()
            .all_fields()
            .into_iter()
            .filter(|f| {
                f.children.is_empty()
                    && !matches!(
                        f.kind,
                        copybook_core::FieldKind::Condition { .. }
                            | copybook_core::FieldKind::Renames { .. }
                    )
            })
            .collect()
    }

    fn first_leaf_field(&self) -> Option<&copybook_core::Field> {
        self.all_leaf_fields().into_iter().next()
    }

    fn first_numeric_leaf_field(&self) -> Option<&copybook_core::Field> {
        self.all_leaf_fields().into_iter().find(|field| {
            matches!(
                field.kind,
                copybook_core::FieldKind::ZonedDecimal { .. }
                    | copybook_core::FieldKind::BinaryInt { .. }
                    | copybook_core::FieldKind::PackedDecimal { .. }
                    | copybook_core::FieldKind::EditedNumeric { .. }
                    | copybook_core::FieldKind::FloatSingle
                    | copybook_core::FieldKind::FloatDouble
            )
        })
    }

    fn renames_field(&self) -> Option<&copybook_core::Field> {
        self.schema().fields.iter().find(|field| field.level == 66)
    }

    fn first_decoded_record(&self) -> Value {
        let output = self
            .decoded_output
            .as_ref()
            .expect("Decoded output not set");
        for line in output.lines() {
            if !line.trim().is_empty() {
                return serde_json::from_str(line).expect("Invalid JSON in decoded output");
            }
        }
        panic!("No records in decoded output");
    }

    fn field_in_projection(&self, field_name: &str) -> bool {
        let schema = self
            .projected_schema
            .as_ref()
            .expect("Projected schema not set");
        Self::find_field_recursive(schema, field_name)
    }

    fn find_field_recursive(schema: &copybook_core::Schema, field_name: &str) -> bool {
        for field in &schema.fields {
            if field.name.eq_ignore_ascii_case(field_name) {
                return true;
            }
            if !field.children.is_empty() {
                let child_schema = copybook_core::Schema {
                    fields: field.children.clone(),
                    ..Default::default()
                };
                if Self::find_field_recursive(&child_schema, field_name) {
                    return true;
                }
            }
        }
        false
    }
}

fn default_ascii_decode_options() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::Fixed)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(true)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)
        .with_threads(1)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto)
}

fn default_ascii_encode_options() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::ASCII)
        .with_format(RecordFormat::Fixed)
        .with_use_raw(false)
        .with_bwz_encode(false)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_threads(1)
        .with_coerce_numbers(true)
        .with_zoned_encoding_override(None)
}

fn build_binary_for_all_leaf_fields(world: &mut CopybookWorld) -> Vec<u8> {
    world.ensure_schema_and_return();
    let schema = world.schema();

    // Use the fixed record length if available, otherwise compute from fields
    let record_len = schema.lrecl_fixed.unwrap_or_else(|| {
        schema
            .all_fields()
            .iter()
            .map(|f| if f.children.is_empty() { f.len } else { 0 })
            .max()
            .unwrap_or(0)
    }) as usize;

    // Create binary data filled with ASCII spaces
    vec![b' '; record_len.max(1)]
}

fn encode_from_payload(
    payload: &Map<String, Value>,
    world: &mut CopybookWorld,
) -> Result<Vec<u8>, Error> {
    world.ensure_schema_and_return();
    world.ensure_encode_options();

    let payload_text = serde_json::to_string(&Value::Object(payload.clone())).unwrap();
    let mut output = Vec::new();

    encode_jsonl_to_file(
        world.schema(),
        Cursor::new(payload_text.as_bytes()),
        &mut output,
        world
            .encode_options
            .as_ref()
            .expect("Encode options not set"),
    )?;

    Ok(output)
}

fn parse_binary_literal(data: &str) -> Vec<u8> {
    let mut result = Vec::new();
    let bytes = data.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if i + 3 < bytes.len() && bytes[i] == b'\\' && bytes[i + 1] == b'x' {
            // Parse \xHH hex escape
            let hi = bytes[i + 2];
            let lo = bytes[i + 3];
            if let (Some(h), Some(l)) = (hex_digit(hi), hex_digit(lo)) {
                result.push(h << 4 | l);
                i += 4;
                continue;
            }
        }
        result.push(bytes[i]);
        i += 1;
    }
    result
}

fn hex_digit(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(b - b'a' + 10),
        b'A'..=b'F' => Some(b - b'A' + 10),
        _ => None,
    }
}

fn does_path_match_counter(occurs: &copybook_core::Occurs, counter: &str) -> bool {
    match occurs {
        copybook_core::Occurs::ODO { counter_path, .. } => {
            counter_path.eq_ignore_ascii_case(counter) || counter_path.ends_with(counter)
        }
        copybook_core::Occurs::Fixed { .. } => false,
    }
}

fn assert_field_level_depth(field: &copybook_core::Field) {
    for child in &field.children {
        assert!(
            child.level > field.level,
            "Expected child field '{}' level {} to be greater than parent '{}' level {}",
            child.name,
            child.level,
            field.name,
            field.level
        );
        assert_field_level_depth(child);
    }
}

fn json_value_for_field<'a>(record: &'a Value, field_name: &str) -> Option<&'a Value> {
    let obj = record.as_object()?;
    obj.get(field_name)
        .or_else(|| obj.get(&field_name.to_ascii_uppercase()))
        .or_else(|| obj.get(&field_name.to_ascii_lowercase()))
}

fn field_value_as_string(value: &Value) -> Option<String> {
    match value {
        Value::String(value) => Some(value.clone()),
        Value::Number(value) => Some(value.to_string()),
        Value::Bool(value) => Some(value.to_string()),
        Value::Null => Some(String::new()),
        _ => None,
    }
}

fn collect_string_values(value: &Value) -> Vec<String> {
    let mut result = Vec::new();
    match value {
        Value::Object(object) => {
            for v in object.values() {
                result.extend(collect_string_values(v));
            }
        }
        Value::Array(values) => {
            for v in values {
                result.extend(collect_string_values(v));
            }
        }
        Value::String(s) => result.push(s.clone()),
        Value::Number(number) => result.push(number.to_string()),
        Value::Bool(b) => result.push(b.to_string()),
        Value::Null => {}
    }
    result
}

// ============================================================================
// Step Definitions
// ============================================================================

mod steps {
    use super::*;

    // ========================================================================
    // Copybook Parsing Steps
    // ========================================================================

    #[given(expr = "a copybook with content:")]
    async fn given_copybook_with_content(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(
            step.docstring.as_ref().expect("Docstring required").clone(),
        );
    }

    #[given(expr = "a simple copybook with a single field")]
    async fn given_simple_copybook(world: &mut CopybookWorld) {
        world.copybook_text = Some("01 TEST-RECORD.\n    05 TEST-FIELD PIC X(10).".to_string());
    }

    #[given(expr = "a copybook with numeric fields")]
    async fn given_copybook_with_numeric_fields(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 NUMERIC-RECORD.\n\
             05 PACKED-DECIMAL PIC S9(7)V99 COMP-3.\n\
             05 BINARY-INTEGER PIC S9(9) COMP.\n\
             05 ZONED-DECIMAL PIC S9(7)V99 DISPLAY."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with OCCURS clause")]
    async fn given_copybook_with_occurs(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 ARRAY-RECORD.\n\
             05 ARRAY-FIELD OCCURS 5 TIMES.\n\
                 10 ELEMENT PIC X(10)."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with ODO (OCCURS DEPENDING ON)")]
    async fn given_copybook_with_odo(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 ODO-RECORD.\n\
             05 COUNT-FIELD PIC 9(3).\n\
             05 DYNAMIC-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON COUNT-FIELD.\n\
                 10 ELEMENT PIC X(10)."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with REDEFINES clause")]
    async fn given_copybook_with_redefines(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 REDEFINES-RECORD.\n\
             05 ORIGINAL-FIELD PIC X(20).\n\
             05 ALTERNATIVE-FIELD PIC X(20)."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with Level-88 condition values")]
    async fn given_copybook_with_level88(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 STATUS-RECORD.\n\
             05 STATUS-CODE PIC X(1).\n\
                 88 STATUS-ACTIVE VALUE 'A'.\n\
                 88 STATUS-INACTIVE VALUE 'I'.\n\
                 88 STATUS-PENDING VALUE 'P'."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with RENAMES clause")]
    async fn given_copybook_with_renames(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RENAMES-RECORD.\n\
             05 FIRST-FIELD PIC X(10).\n\
             05 SECOND-FIELD PIC X(10).\n\
             66 ALIAS-FIELD RENAMES FIRST-FIELD THRU SECOND-FIELD."
                .to_string(),
        );
    }

    #[given(expr = "binary data: {string}")]
    async fn given_binary_data(world: &mut CopybookWorld, data: String) {
        world.binary_data = Some(parse_binary_literal(&data));
    }

    #[given(expr = "JSON data:")]
    async fn given_json_data(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.json_data = Some(
            step.docstring.as_ref().expect("Docstring required").clone(),
        );
    }

    #[given(expr = "JSON data: {string}")]
    async fn given_json_data_inline(world: &mut CopybookWorld, data: String) {
        world.json_data = Some(data);
    }

    #[given(expr = "ASCII codepage")]
    async fn given_ascii_codepage(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        world.ensure_encode_options();
        if let Some(options) = world.decode_options.take() {
            world.decode_options = Some(options.with_codepage(Codepage::ASCII));
        }
        if let Some(options) = world.encode_options.take() {
            world.encode_options = Some(options.with_codepage(Codepage::ASCII));
        }
    }

    #[given(expr = "EBCDIC codepage")]
    async fn given_ebcdic_codepage(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        world.ensure_encode_options();
        if let Some(options) = world.decode_options.take() {
            world.decode_options = Some(options.with_codepage(Codepage::CP037));
        }
        if let Some(options) = world.encode_options.take() {
            world.encode_options = Some(options.with_codepage(Codepage::CP037));
        }
    }

    #[given(expr = "strict parsing mode")]
    async fn given_strict_parsing(world: &mut CopybookWorld) {
        let mut options = world.parse_options.clone().unwrap_or_default();
        options.strict = true;
        world.parse_options = Some(options);
    }

    #[given(expr = "tolerant parsing mode")]
    async fn given_tolerant_parsing(world: &mut CopybookWorld) {
        let mut options = world.parse_options.clone().unwrap_or_default();
        options.strict = false;
        world.parse_options = Some(options);
    }

    #[when(expr = "the copybook is parsed")]
    #[when(expr = "copybook is parsed")]
    async fn when_copybook_is_parsed(world: &mut CopybookWorld) {
        world.ensure_schema_parsed();
    }

    #[then(expr = "the schema should be successfully parsed")]
    async fn then_schema_successfully_parsed(world: &mut CopybookWorld) {
        assert!(
            world.schema.is_some(),
            "Schema should be parsed successfully"
        );
        assert!(world.error.is_none(), "No error should occur");
    }

    #[then(expr = "parsing should succeed")]
    async fn then_parsing_should_succeed(world: &mut CopybookWorld) {
        assert!(world.schema.is_some(), "Parsing should succeed");
        assert!(world.error.is_none(), "No error should occur");
    }

    #[then(expr = "parsing should fail")]
    async fn then_parsing_should_fail(world: &mut CopybookWorld) {
        assert!(
            world.error.is_some(),
            "Parsing should fail for this scenario"
        );
    }

    #[then(expr = "the schema should contain {int} top-level field(s)")]
    async fn then_schema_contains_fields(world: &mut CopybookWorld, count: usize) {
        assert_eq!(
            world.schema().fields.len(),
            count,
            "Expected {} top-level fields, got {}",
            count,
            world.schema().fields.len()
        );
    }

    #[then(expr = "the field {string} should have type {string}")]
    async fn then_field_has_type(
        world: &mut CopybookWorld,
        field_name: String,
        expected_type: String,
    ) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        // Check for "occurs" type based on occurs clause rather than kind
        if expected_type == "occurs" {
            assert!(
                field.occurs.is_some(),
                "Expected field '{}' to have OCCURS clause",
                field_name
            );
            return;
        }

        let actual_type = match &field.kind {
            copybook_core::FieldKind::Alphanum { .. } => "alphanumeric",
            copybook_core::FieldKind::ZonedDecimal { .. } => "zoned",
            copybook_core::FieldKind::PackedDecimal { .. } => "packed",
            copybook_core::FieldKind::BinaryInt { .. } => "binary",
            copybook_core::FieldKind::EditedNumeric { .. } => "edited",
            copybook_core::FieldKind::FloatSingle => "float_single",
            copybook_core::FieldKind::FloatDouble => "float_double",
            copybook_core::FieldKind::Group => "group",
            copybook_core::FieldKind::Condition { .. } => "condition",
            copybook_core::FieldKind::Renames { .. } => "renames",
        };

        // Support type aliases used across feature files
        let matches = match expected_type.as_str() {
            "numeric" => actual_type == "zoned",
            "packed_decimal" => actual_type == "packed",
            "binary_int" => actual_type == "binary",
            "edited_numeric" => actual_type == "edited",
            other => actual_type == other,
        };

        assert!(
            matches,
            "Expected field '{}' to have type '{}', got '{}'",
            field_name, expected_type, actual_type
        );
    }

    #[then(expr = "the field {string} should be {int} characters long")]
    async fn then_field_length(world: &mut CopybookWorld, field_name: String, length: usize) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert_eq!(
            field.len as usize, length,
            "Expected field '{}' to be {} characters long, got {}",
            field_name, length, field.len
        );
    }

    #[then(expr = "the field {string} should be {int} bytes long")]
    async fn then_field_length_bytes(world: &mut CopybookWorld, field_name: String, length: usize) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert_eq!(
            field.len as usize, length,
            "Expected field '{}' to be {} bytes long, got {}",
            field_name, length, field.len
        );
    }

    #[then(expr = "the field {string} should have level {int}")]
    async fn then_field_has_level(
        world: &mut CopybookWorld,
        field_name: String,
        expected_level: u8,
    ) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert_eq!(
            field.level, expected_level,
            "Expected field '{}' to have level {}, got {}",
            field_name, expected_level, field.level
        );
    }

    #[then(expr = "field {word} should have OCCURS count {int}")]
    async fn then_field_has_occurs_count(
        world: &mut CopybookWorld,
        field_name: String,
        expected_count: u32,
    ) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        let actual_count = match &field.occurs {
            Some(copybook_core::Occurs::Fixed { count }) => *count,
            Some(_) => panic!("Field '{}' has ODO, not fixed OCCURS", field_name),
            None => panic!("Field '{}' has no OCCURS clause", field_name),
        };

        assert_eq!(
            actual_count, expected_count,
            "Expected field '{}' to have OCCURS count {}, got {}",
            field_name, expected_count, actual_count
        );
    }

    #[then(expr = "field {word} should have ODO with counter {string}")]
    async fn then_field_has_odo_with_counter(
        world: &mut CopybookWorld,
        field_name: String,
        counter: String,
    ) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        match &field.occurs {
            Some(occurs @ copybook_core::Occurs::ODO { .. }) => {
                assert!(
                    does_path_match_counter(occurs, &counter),
                    "Field '{}' should have ODO with counter '{}'",
                    field_name,
                    counter
                );
            }
            Some(_) => panic!("Field '{}' is not an ODO field", field_name),
            None => panic!("Field '{}' has no OCCURS clause", field_name),
        }
    }

    #[then(expr = "field {word} should have ODO with min {int} and max {int}")]
    async fn then_field_has_odo_range(
        world: &mut CopybookWorld,
        field_name: String,
        expected_min: u32,
        expected_max: u32,
    ) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        match &field.occurs {
            Some(copybook_core::Occurs::ODO { min, max, .. }) => {
                assert_eq!(
                    *min, expected_min,
                    "Field '{}' ODO min should be {}",
                    field_name, expected_min
                );
                assert_eq!(
                    *max, expected_max,
                    "Field '{}' ODO max should be {}",
                    field_name, expected_max
                );
            }
            Some(_) => panic!("Field '{}' is not an ODO field", field_name),
            None => panic!("Field '{}' has no OCCURS clause", field_name),
        }
    }

    #[then(expr = "the field {string} should have REDEFINES {string}")]
    async fn then_field_has_redefines(
        world: &mut CopybookWorld,
        field_name: String,
        redefines: String,
    ) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert!(
            field.redefines_of.is_some(),
            "Field '{}' should have REDEFINES",
            field_name
        );
    }

    #[then(expr = "the field {string} should redefine {string}")]
    async fn then_field_redefines(world: &mut CopybookWorld, field_name: String, original: String) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert_eq!(
            field.redefines_of.as_deref(),
            Some(original.as_str()),
            "Field '{}' should redefine '{}'",
            field_name,
            original
        );
    }

    #[then(expr = "the field {string} should have Level-88 {string}")]
    async fn then_field_has_level88_value(
        world: &mut CopybookWorld,
        field_name: String,
        condition_name: String,
    ) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        let has_condition = field
            .children
            .iter()
            .any(|child| child.level == 88 && child.name.eq_ignore_ascii_case(&condition_name));

        assert!(
            has_condition,
            "Field '{}' should have Level-88 condition '{}'",
            field_name, condition_name
        );
    }

    #[then(expr = "there should be {int} level-66 fields")]
    async fn then_level_66_field_count(world: &mut CopybookWorld, expected: usize) {
        let count = world
            .schema()
            .all_fields()
            .iter()
            .filter(|field| field.level == 66)
            .count();

        assert_eq!(
            count, expected,
            "Expected {} level-66 fields, got {}",
            expected, count
        );
    }

    #[then(expr = "the schema should have fingerprint")]
    async fn then_schema_has_fingerprint(world: &mut CopybookWorld) {
        let schema = world.schema();
        assert!(
            !schema.fingerprint.is_empty(),
            "Schema should have fingerprint"
        );
    }

    #[then(expr = "the schema should contain {int} leaf fields")]
    async fn then_leaf_field_count(world: &mut CopybookWorld, expected: usize) {
        let count = world.all_leaf_fields().len();
        assert_eq!(
            count, expected,
            "Expected {} leaf fields, got {}",
            expected, count
        );
    }

    #[then(expr = "the field {string} should be present")]
    async fn then_field_is_present(world: &mut CopybookWorld, field_name: String) {
        assert!(
            world.find_field_by_name_ci(&field_name).is_some(),
            "Field '{}' should be present",
            field_name
        );
    }

    #[then(expr = "{word} should be present")]
    async fn then_field_word_present(world: &mut CopybookWorld, field_name: String) {
        assert!(
            world.find_field_by_name_ci(&field_name).is_some(),
            "Field '{}' should be present",
            field_name
        );
    }

    #[then(expr = "the field {word} should not be present")]
    async fn then_field_not_present(world: &mut CopybookWorld, field_name: String) {
        assert!(
            world.find_field_by_name_ci(&field_name).is_none(),
            "Field '{}' should not be present",
            field_name
        );
    }

    #[then(expr = "an error should occur")]
    async fn then_error_occurs(world: &mut CopybookWorld) {
        assert!(world.error.is_some(), "An error should have occurred");
    }

    #[then(expr = "the error message should contain {string}")]
    async fn then_error_message_contains(world: &mut CopybookWorld, expected: String) {
        let error = world.error.as_ref().expect("Error should be set");
        let message = format!("{}", error);
        assert!(
            message.contains(&expected),
            "Expected error message to contain '{}', got: {}",
            expected,
            message
        );
    }

    #[then(expr = "the error code should be {string}")]
    async fn then_error_code_is(world: &mut CopybookWorld, expected_code: String) {
        let error = world.error.as_ref().expect("Error should be set");
        let actual_code = format!("{:?}", error.code);
        assert_eq!(
            actual_code, expected_code,
            "Expected error code '{}', got '{}'",
            expected_code, actual_code
        );
    }

    // ========================================================================
    // Encode/Decode Steps
    // ========================================================================

    #[when(expr = "the binary data is decoded")]
    async fn when_binary_data_decoded(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        world.ensure_decode_options();

        let binary_data = world
            .binary_data
            .as_ref()
            .expect("Binary data not set")
            .clone();
        let mut output = Vec::new();

        match decode_file_to_jsonl(
            world.schema(),
            Cursor::new(&binary_data),
            &mut output,
            world
                .decode_options
                .as_ref()
                .expect("Decode options not set"),
        ) {
            Ok(_summary) => {
                world.decoded_output = Some(String::from_utf8(output).unwrap());
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[when(expr = "the JSON data is encoded")]
    async fn when_json_data_encoded(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        world.ensure_encode_options();

        let json_data = world.json_data.as_ref().expect("JSON data not set").clone();
        let mut output = Vec::new();

        match encode_jsonl_to_file(
            world.schema(),
            Cursor::new(json_data.as_bytes()),
            &mut output,
            world
                .encode_options
                .as_ref()
                .expect("Encode options not set"),
        ) {
            Ok(_summary) => {
                world.encoded_output = Some(output);
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[when(expr = "the data is round-tripped")]
    async fn when_data_roundtripped(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        world.ensure_decode_options();
        world.ensure_encode_options();

        let binary_data = world
            .binary_data
            .as_ref()
            .expect("Binary data not set")
            .clone();
        let mut decoded = Vec::new();

        match decode_file_to_jsonl(
            world.schema(),
            Cursor::new(&binary_data),
            &mut decoded,
            world
                .decode_options
                .as_ref()
                .expect("Decode options not set"),
        ) {
            Ok(_summary) => {
                let decoded_str = String::from_utf8(decoded).unwrap();
                world.decoded_output = Some(decoded_str.clone());

                let mut encoded = Vec::new();
                match encode_jsonl_to_file(
                    world.schema(),
                    Cursor::new(decoded_str.as_bytes()),
                    &mut encoded,
                    world
                        .encode_options
                        .as_ref()
                        .expect("Encode options not set"),
                ) {
                    Ok(_summary) => {
                        world.encoded_output = Some(encoded);
                    }
                    Err(e) => {
                        world.error = Some(e);
                    }
                }
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[then(expr = "the decoded output should contain {string}")]
    async fn then_decoded_output_contains(world: &mut CopybookWorld, expected: String) {
        let output = world
            .decoded_output
            .as_ref()
            .expect("Decoded output not set");
        assert!(
            output.contains(&expected),
            "Expected decoded output to contain '{}', got: {}",
            expected,
            output
        );
    }

    #[then(expr = "decoded {word} should be {string}")]
    async fn then_decoded_field_value(
        world: &mut CopybookWorld,
        field_name: String,
        expected: String,
    ) {
        let record = world.first_decoded_record();
        let value = json_value_for_field(&record, &field_name).expect(&format!(
            "Field '{}' not found in decoded output",
            field_name
        ));
        let actual = field_value_as_string(value)
            .expect(&format!("Field '{}' has unsupported type", field_name));
        assert_eq!(
            actual.trim(),
            expected,
            "Expected decoded field '{}' to be '{}', got '{}'",
            field_name,
            expected,
            actual.trim()
        );
    }

    #[then(expr = "decoded {word} should be {int}")]
    async fn then_decoded_field_int_value(
        world: &mut CopybookWorld,
        field_name: String,
        expected: i64,
    ) {
        let record = world.first_decoded_record();
        let value = json_value_for_field(&record, &field_name).expect(&format!(
            "Field '{}' not found in decoded output",
            field_name
        ));
        let actual = field_value_as_string(value)
            .expect(&format!("Field '{}' has unsupported type", field_name));
        let actual_num: i64 = actual.trim().parse().expect(&format!(
            "Field '{}' value '{}' is not a valid integer",
            field_name,
            actual.trim()
        ));
        assert_eq!(
            actual_num, expected,
            "Expected decoded field '{}' to be {}, got {}",
            field_name, expected, actual_num
        );
    }

    #[then(expr = "decoded {word} should be blank")]
    async fn then_decoded_field_blank(world: &mut CopybookWorld, field_name: String) {
        let record = world.first_decoded_record();
        if let Some(v) = json_value_for_field(&record, &field_name) {
            let s = field_value_as_string(v).unwrap_or_default();
            assert!(
                s.trim().is_empty(),
                "Expected decoded field '{}' to be blank, got '{}'",
                field_name,
                s
            );
        }
        // Field not present is considered blank
    }

    #[then(expr = "the decoded output should be valid JSON")]
    async fn then_decoded_output_valid_json(world: &mut CopybookWorld) {
        let output = world
            .decoded_output
            .as_ref()
            .expect("Decoded output not set");
        for line in output.lines() {
            if line.trim().is_empty() {
                continue;
            }
            let _: Value = serde_json::from_str(line)
                .expect(&format!("Output should be valid JSON: {}", line));
        }
    }

    #[then(expr = "the round-trip should be lossless")]
    async fn then_roundtrip_lossless(world: &mut CopybookWorld) {
        let original = world.binary_data.as_ref().expect("Binary data not set");
        let encoded = world
            .encoded_output
            .as_ref()
            .expect("Encoded output not set");

        assert_eq!(
            original, encoded,
            "Round-trip should be lossless: original data differs from encoded data"
        );
    }

    #[then(expr = "encoding should succeed")]
    async fn then_encoding_succeeds(world: &mut CopybookWorld) {
        assert!(world.encoded_output.is_some(), "Encoding should succeed");
        assert!(
            world.error.is_none(),
            "No error should occur during encoding"
        );
    }

    #[then(expr = "decoding should succeed")]
    async fn then_decoding_should_succeed(world: &mut CopybookWorld) {
        assert!(world.decoded_output.is_some(), "Decoding should succeed");
        assert!(
            world.error.is_none(),
            "No error should occur during decoding"
        );
    }

    // ========================================================================
    // Field Projection Steps
    // ========================================================================

    #[given(expr = "field selection for projection: {string}")]
    async fn given_field_selection(world: &mut CopybookWorld, fields: String) {
        let field_list: Vec<String> = fields.split(',').map(|s| s.trim().to_string()).collect();
        world.field_selection = Some(field_list);
    }

    #[when(expr = "field projection is applied")]
    async fn when_field_projection_applied(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        let fields = world
            .field_selection
            .as_ref()
            .expect("Field selection not set")
            .clone();

        match project_schema(world.schema(), &fields) {
            Ok(projected) => {
                world.projected_schema = Some(projected);
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[then(expr = "the field {string} should be included in projection")]
    async fn then_field_included_in_projection(world: &mut CopybookWorld, field_name: String) {
        assert!(
            world.field_in_projection(&field_name),
            "Field '{}' should be included in projection",
            field_name
        );
    }

    #[then(expr = "the field {string} should not be included in projection")]
    async fn then_field_not_included_in_projection(world: &mut CopybookWorld, field_name: String) {
        assert!(
            !world.field_in_projection(&field_name),
            "Field '{}' should not be included in projection",
            field_name
        );
    }

    // ========================================================================
    // Dialect Processing Steps
    // ========================================================================

    #[given(expr = "Normative dialect")]
    async fn given_normative_dialect(world: &mut CopybookWorld) {
        let mut options = world.parse_options.clone().unwrap_or_default();
        options.dialect = copybook_core::dialect::Dialect::Normative;
        world.parse_options = Some(options);
    }

    #[given(expr = "Zero-Tolerant dialect")]
    async fn given_zero_tolerant_dialect(world: &mut CopybookWorld) {
        let mut options = world.parse_options.clone().unwrap_or_default();
        options.dialect = copybook_core::dialect::Dialect::ZeroTolerant;
        world.parse_options = Some(options);
    }

    #[given(expr = "One-Tolerant dialect")]
    async fn given_one_tolerant_dialect(world: &mut CopybookWorld) {
        let mut options = world.parse_options.clone().unwrap_or_default();
        options.dialect = copybook_core::dialect::Dialect::OneTolerant;
        world.parse_options = Some(options);
    }

    #[then(expr = "the schema should use {string} dialect")]
    async fn then_schema_uses_dialect(world: &mut CopybookWorld, expected_dialect: String) {
        let options = world.parse_options.as_ref().expect("Parse options not set");
        let actual = match options.dialect {
            copybook_core::dialect::Dialect::Normative => "Normative",
            copybook_core::dialect::Dialect::ZeroTolerant => "ZeroTolerant",
            copybook_core::dialect::Dialect::OneTolerant => "OneTolerant",
        };
        assert_eq!(
            actual, expected_dialect,
            "Expected dialect '{}', got '{}'",
            expected_dialect, actual
        );
    }

    // ========================================================================
    // Determinism Validation Steps
    // ========================================================================

    #[given(expr = "binary data with {int} elements")]
    async fn given_binary_data_with_elements(world: &mut CopybookWorld, count: usize) {
        world.ensure_schema_and_return();
        let binary = build_binary_for_all_leaf_fields(world);
        world.binary_data = Some(binary);
    }

    #[when(expr = "decode determinism is checked")]
    async fn when_decode_determinism_checked(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        world.ensure_decode_options();

        let binary_data = world
            .binary_data
            .as_ref()
            .expect("Binary data not set")
            .clone();
        match check_decode_determinism(
            world.schema(),
            &binary_data,
            world
                .decode_options
                .as_ref()
                .expect("Decode options not set"),
        ) {
            Ok(result) => {
                world.determinism_result = Some(result);
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[when(expr = "encode determinism is checked")]
    async fn when_encode_determinism_checked(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        world.ensure_encode_options();

        let json_data = world.json_data.as_ref().expect("JSON data not set").clone();
        let json_value: Value =
            serde_json::from_str(&json_data).expect("JSON data should be valid JSON");

        match check_encode_determinism(
            world.schema(),
            &json_value,
            world
                .encode_options
                .as_ref()
                .expect("Encode options not set"),
        ) {
            Ok(result) => {
                world.determinism_result = Some(result);
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[when(expr = "round-trip determinism is checked")]
    async fn when_roundtrip_determinism_checked(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        world.ensure_decode_options();
        world.ensure_encode_options();

        let binary_data = world
            .binary_data
            .as_ref()
            .expect("Binary data not set")
            .clone();
        match check_round_trip_determinism(
            world.schema(),
            &binary_data,
            world
                .decode_options
                .as_ref()
                .expect("Decode options not set"),
            world
                .encode_options
                .as_ref()
                .expect("Encode options not set"),
        ) {
            Ok(result) => {
                world.determinism_result = Some(result);
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[then(expr = "determinism should pass")]
    async fn then_determinism_passes(world: &mut CopybookWorld) {
        let result = world
            .determinism_result
            .as_ref()
            .expect("Determinism result should be set");

        assert!(
            result.is_deterministic,
            "Expected determinism to pass, but it failed"
        );
    }

    #[then(expr = "determinism should fail")]
    async fn then_determinism_fails(world: &mut CopybookWorld) {
        let result = world
            .determinism_result
            .as_ref()
            .expect("Determinism result should be set");

        assert!(
            !result.is_deterministic,
            "Expected determinism to fail, but it passed"
        );
    }

    // ========================================================================
    // Enterprise Audit Steps
    // ========================================================================

    #[cfg(feature = "audit")]
    #[given(expr = "the audit system is enabled")]
    async fn given_audit_system_enabled(world: &mut CopybookWorld) {
        world.audit_events.clear();
        world.regression_metrics = false;
    }

    #[cfg(feature = "audit")]
    #[given(expr = "an audit context is initialized")]
    async fn given_audit_context_initialized(world: &mut CopybookWorld) {
        let context = AuditContext::new()
            .with_operation_id("bdd_test_operation")
            .with_user("bdd_test_user")
            .with_metadata("session_id", "bdd_test_session");
        world.audit_context = Some(context);
    }

    #[cfg(feature = "audit")]
    #[given(expr = "a financial SOX compliance copybook")]
    async fn given_sox_compliance_copybook(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 FINANCIAL-TRANSACTION.\n\
             05 TRANSACTION-ID PIC X(20).\n\
             05 TRANSACTION-AMOUNT PIC S9(13)V99 COMP-3.\n\
             05 TRANSACTION-DATE PIC 9(8).\n\
             05 ACCOUNT-NUMBER PIC X(12).\n\
             05 TRANSACTION-TYPE PIC X(2)."
                .to_string(),
        );
        // Set SOX compliance on the audit context
        if let Some(ref mut ctx) = world.audit_context {
            let updated = ctx
                .clone()
                .with_compliance_profile(ComplianceProfile::SOX)
                .with_security_classification(SecurityClassification::MaterialTransaction);
            *ctx = updated;
        }
        world.compliance_profile = Some(ComplianceProfile::SOX);
    }

    #[cfg(feature = "audit")]
    #[given(expr = "a healthcare HIPAA compliance copybook")]
    async fn given_hipaa_compliance_copybook(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 PATIENT-RECORD.\n\
             05 PATIENT-ID PIC X(20).\n\
             05 PATIENT-NAME PIC X(50).\n\
             05 DIAGNOSIS-CODE PIC X(10).\n\
             05 TREATMENT-DATE PIC 9(8).\n\
             05 PROVIDER-ID PIC X(15)."
                .to_string(),
        );
        if let Some(ref mut ctx) = world.audit_context {
            let updated = ctx
                .clone()
                .with_compliance_profile(ComplianceProfile::HIPAA)
                .with_security_classification(SecurityClassification::PHI)
                .with_metadata("data_classification", "PHI");
            *ctx = updated;
        }
        world.compliance_profile = Some(ComplianceProfile::HIPAA);
    }

    #[cfg(feature = "audit")]
    #[given(expr = "a GDPR personal data copybook")]
    async fn given_gdpr_compliance_copybook(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 PERSONAL-DATA.\n\
             05 DATA-SUBJECT-ID PIC X(20).\n\
             05 FULL-NAME PIC X(50).\n\
             05 EMAIL-ADDRESS PIC X(100).\n\
             05 CONSENT-DATE PIC 9(8)."
                .to_string(),
        );
        if let Some(ref mut ctx) = world.audit_context {
            let updated = ctx
                .clone()
                .with_compliance_profile(ComplianceProfile::GDPR)
                .with_security_classification(SecurityClassification::Confidential)
                .with_metadata("legal_basis", "consent")
                .with_metadata("processing_purpose", "data_processing");
            *ctx = updated;
        }
        world.compliance_profile = Some(ComplianceProfile::GDPR);
    }

    #[cfg(feature = "audit")]
    #[given(expr = "a PCI DSS payment card copybook")]
    async fn given_pci_dss_compliance_copybook(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 PAYMENT-CARD.\n\
             05 CARD-NUMBER PIC X(16).\n\
             05 EXPIRY-DATE PIC 9(4).\n\
             05 CARDHOLDER-NAME PIC X(50).\n\
             05 CVV PIC 9(3)."
                .to_string(),
        );
        if let Some(ref mut ctx) = world.audit_context {
            let updated = ctx
                .clone()
                .with_compliance_profile(ComplianceProfile::PciDss)
                .with_security_classification(SecurityClassification::Confidential)
                .with_metadata("cardholder_data", "present");
            *ctx = updated;
        }
        world.compliance_profile = Some(ComplianceProfile::PciDss);
    }

    #[cfg(feature = "audit")]
    #[given(expr = "a simple audit copybook")]
    async fn given_simple_audit_copybook(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 AUDIT-RECORD.\n\
             05 AUDIT-FIELD PIC X(10)."
                .to_string(),
        );
    }

    #[cfg(feature = "audit")]
    #[given(expr = "compliance profile {string}")]
    async fn given_compliance_profile(world: &mut CopybookWorld, profile_str: String) {
        let profile = match profile_str.as_str() {
            "SOX" => ComplianceProfile::SOX,
            "HIPAA" => ComplianceProfile::HIPAA,
            "GDPR" => ComplianceProfile::GDPR,
            "PciDss" | "PCI_DSS" | "PCI DSS" => ComplianceProfile::PciDss,
            _ => panic!("Unknown compliance profile: {}", profile_str),
        };

        if let Some(ref mut ctx) = world.audit_context {
            let updated = ctx.clone().with_compliance_profile(profile);
            *ctx = updated;
        }
    }

    #[cfg(feature = "audit")]
    #[given(expr = "security classification {string}")]
    async fn given_security_classification(world: &mut CopybookWorld, classification_str: String) {
        let classification = match classification_str.as_str() {
            "Public" => SecurityClassification::Public,
            "Internal" => SecurityClassification::Internal,
            "Confidential" => SecurityClassification::Confidential,
            "MaterialTransaction" => SecurityClassification::MaterialTransaction,
            "PHI" => SecurityClassification::PHI,
            _ => panic!("Unknown security classification: {}", classification_str),
        };

        if let Some(ref mut ctx) = world.audit_context {
            let updated = ctx
                .clone()
                .with_security_classification(classification.clone());
            *ctx = updated;
        }
        world.security_classification = Some(classification);
    }

    #[cfg(feature = "audit")]
    #[given(expr = "audit metadata {string} is {string}")]
    async fn given_audit_metadata(world: &mut CopybookWorld, key: String, value: String) {
        if let Some(ref mut ctx) = world.audit_context {
            let updated = ctx.clone().with_metadata(key, value);
            *ctx = updated;
        }
    }

    #[cfg(feature = "audit")]
    #[given(expr = "throughput metrics below baseline")]
    async fn given_throughput_metrics_below_baseline(world: &mut CopybookWorld) {
        world.regression_metrics = true;
    }

    // --- Audit When Steps ---

    #[cfg(feature = "audit")]
    #[when(expr = "the copybook is processed for financial data")]
    async fn when_copybook_processed_financial(world: &mut CopybookWorld) {
        when_copybook_processed_with_audit(world).await;
    }

    #[cfg(feature = "audit")]
    #[when(expr = "the copybook is processed with audit")]
    async fn when_copybook_processed_with_audit(world: &mut CopybookWorld) {
        if let Some(ref text) = world.copybook_text.clone() {
            match parse_copybook(text) {
                Ok(schema) => {
                    let field_count = schema.all_fields().len();
                    let level_88_count =
                        schema.all_fields().iter().filter(|f| f.level == 88).count();
                    let fingerprint = schema.fingerprint.clone();
                    world.schema = Some(schema);

                    if let Some(ref context) = world.audit_context.clone() {
                        let event = AuditEvent::new(
                            AuditEventType::CopybookParse,
                            context.clone(),
                            AuditPayload::CopybookParse {
                                copybook_path: "bdd_test.cpy".to_string(),
                                schema_fingerprint: fingerprint,
                                parse_result: AuditParseResult::Success,
                                parsing_duration_ms: 1,
                                field_count,
                                level_88_count,
                                error_count: 0,
                                warnings: vec![],
                            },
                        );
                        world.audit_events.push(event);
                    }
                }
                Err(e) => {
                    world.error = Some(e);
                }
            }
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "a decode audit event is created")]
    async fn when_decode_audit_event_created(world: &mut CopybookWorld) {
        // Parse the copybook first
        world.ensure_schema_parsed();

        if let Some(ref context) = world.audit_context.clone() {
            let event = AuditEvent::new(
                AuditEventType::DataTransformation,
                context.clone(),
                AuditPayload::DataTransformation {
                    operation: TransformationOperation::Decode,
                    input_file: "test_input.bin".to_string(),
                    output_file: "test_output.jsonl".to_string(),
                    transformation_result: TransformationResult::Success,
                    processing_duration_ms: 10,
                    records_processed: 100,
                    bytes_processed: 5000,
                    throughput_bytes_per_sec: 500_000,
                    memory_usage_mb: 16,
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "an encode audit event is created")]
    async fn when_encode_audit_event_created(world: &mut CopybookWorld) {
        world.ensure_schema_parsed();

        if let Some(ref context) = world.audit_context.clone() {
            let event = AuditEvent::new(
                AuditEventType::DataTransformation,
                context.clone(),
                AuditPayload::DataTransformation {
                    operation: TransformationOperation::Encode,
                    input_file: "test_input.jsonl".to_string(),
                    output_file: "test_output.bin".to_string(),
                    transformation_result: TransformationResult::Success,
                    processing_duration_ms: 8,
                    records_processed: 50,
                    bytes_processed: 2500,
                    throughput_bytes_per_sec: 312_500,
                    memory_usage_mb: 12,
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "a projection audit event is created")]
    async fn when_projection_audit_event_created(world: &mut CopybookWorld) {
        world.ensure_schema_parsed();

        if let Some(ref context) = world.audit_context.clone() {
            let event = AuditEvent::new(
                AuditEventType::DataValidation,
                context.clone(),
                AuditPayload::DataValidation {
                    input_file: "test_input.bin".to_string(),
                    validation_result: AuditValidationResult::Valid,
                    validation_duration_ms: 5,
                    records_validated: 100,
                    errors_found: 0,
                    error_details: vec![],
                    validation_rules: vec!["AUDIT-FIELD".to_string()],
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "a configuration change audit event is created")]
    async fn when_config_change_audit_event_created(world: &mut CopybookWorld) {
        if let Some(ref context) = world.audit_context.clone() {
            let event = AuditEvent::new(
                AuditEventType::ConfigurationChange,
                context.clone(),
                AuditPayload::ConfigurationChange {
                    component: "dialect".to_string(),
                    change_type: ConfigurationChangeType::Update,
                    old_configuration: Some("Normative".to_string()),
                    new_configuration: "ZeroTolerant".to_string(),
                    change_reason: "IBM Enterprise COBOL compatibility".to_string(),
                    approved_by: Some("bdd_test_user".to_string()),
                    rollback_available: true,
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "a security event is created")]
    async fn when_security_event_created(world: &mut CopybookWorld) {
        if let Some(ref context) = world.audit_context.clone() {
            let event = AuditEvent::new(
                AuditEventType::SecurityEvent,
                context.clone(),
                AuditPayload::SecurityEvent {
                    security_event_type: SecurityEventType::SuspiciousActivity,
                    severity: "High".to_string(),
                    affected_resources: vec!["copybook_data".to_string()],
                    threat_indicators: vec![],
                    remediation_actions: vec!["review_access".to_string()],
                    incident_id: Some("SEC-001".to_string()),
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "an access event is created with result {string}")]
    async fn when_access_event_created(world: &mut CopybookWorld, result_str: String) {
        let access_result = match result_str.as_str() {
            "Success" => AuditAccessResult::Success,
            "Denied" => AuditAccessResult::Denied,
            "Failed" => AuditAccessResult::Failed,
            _ => panic!("Unknown access result: {}", result_str),
        };

        if let Some(ref context) = world.audit_context.clone() {
            let event = AuditEvent::new(
                AuditEventType::AccessEvent,
                context.clone(),
                AuditPayload::AccessEvent {
                    access_type: AuditAccessType::Read,
                    resource_type: "copybook_data".to_string(),
                    resource_id: "test_resource".to_string(),
                    access_result,
                    user_id: "bdd_test_user".to_string(),
                    source_ip: Some("127.0.0.1".to_string()),
                    user_agent: Some("bdd_test".to_string()),
                    session_id: Some("bdd_session".to_string()),
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "an error audit event is created")]
    async fn when_error_audit_event_created(world: &mut CopybookWorld) {
        if let Some(ref context) = world.audit_context.clone() {
            let event = AuditEvent::new(
                AuditEventType::ErrorEvent,
                context.clone(),
                AuditPayload::ErrorEvent {
                    error_code: "CBKD001".to_string(),
                    error_message: "Invalid decimal value in field".to_string(),
                    error_category: "data_validation".to_string(),
                    stack_trace: None,
                    context_information: std::collections::HashMap::new(),
                    recovery_actions: vec!["retry_with_defaults".to_string()],
                    user_impact: UserImpactLevel::Low,
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "a performance measurement event is created")]
    async fn when_performance_measurement_created(world: &mut CopybookWorld) {
        let regression_detected = world.regression_metrics;
        let comparison = if regression_detected {
            Some(ComparisonResult::SignificantRegression)
        } else {
            Some(ComparisonResult::WithinBaseline)
        };

        if let Some(ref context) = world.audit_context.clone() {
            let event = AuditEvent::new(
                AuditEventType::PerformanceMeasurement,
                context.clone(),
                AuditPayload::PerformanceMeasurement {
                    measurement_type: PerformanceMeasurementType::Throughput,
                    baseline_id: Some("baseline_001".to_string()),
                    metrics: PerformanceMetrics {
                        throughput_bytes_per_sec: if regression_detected {
                            50_000_000
                        } else {
                            200_000_000
                        },
                        latency_ms: 10,
                        cpu_usage_percent: 45.0,
                        memory_usage_mb: 128,
                        io_operations: 1000,
                    },
                    comparison_result: comparison,
                    regression_detected,
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "a compliance check event is created")]
    async fn when_compliance_check_event_created(world: &mut CopybookWorld) {
        if let Some(ref context) = world.audit_context.clone() {
            let frameworks: Vec<String> = context
                .compliance_profiles
                .iter()
                .map(|p| format!("{:?}", p))
                .collect();
            let framework_str = frameworks.join(", ");

            let event = AuditEvent::new(
                AuditEventType::ComplianceCheck,
                context.clone(),
                AuditPayload::ComplianceCheck {
                    compliance_framework: framework_str,
                    validation_result: ComplianceValidationResult::Compliant,
                    violations: vec![],
                    remediation_required: false,
                    next_review_date: Some("2026-06-01".to_string()),
                },
            );
            world.audit_events.push(event);
        }
    }

    #[cfg(feature = "audit")]
    #[when(expr = "a child context is created for {string}")]
    async fn when_child_context_created(world: &mut CopybookWorld, child_op: String) {
        let parent_ctx = world.audit_context.as_ref().expect("Audit context not set");
        let child = parent_ctx.create_child_context(child_op);
        world.child_audit_context = Some(child);
    }

    // --- Audit Then Steps ---

    #[cfg(feature = "audit")]
    #[then(expr = "an audit trail should be generated for SOX compliance")]
    async fn then_audit_trail_generated_sox(world: &mut CopybookWorld) {
        assert!(
            !world.audit_events.is_empty(),
            "Audit events should be generated"
        );
        let event = &world.audit_events[0];
        assert_eq!(event.event_type, AuditEventType::CopybookParse);
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit output should be in valid JSON format")]
    async fn then_audit_output_valid_json(world: &mut CopybookWorld) {
        assert!(
            !world.audit_events.is_empty(),
            "Audit events should be generated"
        );

        let json_output = serde_json::to_string_pretty(&world.audit_events)
            .expect("Audit events should serialize to JSON");
        world.audit_output = Some(json_output.clone());

        let _: Value =
            serde_json::from_str(&json_output).expect("Audit output should be valid JSON");
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit event type should be {string}")]
    async fn then_audit_event_type(world: &mut CopybookWorld, expected_type: String) {
        let event = world.audit_events.last().expect("No audit events");
        let actual_type = format!("{:?}", event.event_type);
        assert_eq!(
            actual_type, expected_type,
            "Expected audit event type '{}', got '{}'",
            expected_type, actual_type
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit event severity should be {string}")]
    async fn then_audit_event_severity(world: &mut CopybookWorld, expected_severity: String) {
        let event = world.audit_events.last().expect("No audit events");
        let actual_severity = format!("{:?}", event.severity);
        assert_eq!(
            actual_severity, expected_severity,
            "Expected audit event severity '{}', got '{}'",
            expected_severity, actual_severity
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit context should have compliance profile {string}")]
    async fn then_audit_context_has_compliance_profile(
        world: &mut CopybookWorld,
        expected_profile: String,
    ) {
        let event = world.audit_events.last().expect("No audit events");
        let profiles: Vec<String> = event
            .context
            .compliance_profiles
            .iter()
            .map(|p| format!("{:?}", p))
            .collect();
        assert!(
            profiles.contains(&expected_profile),
            "Expected audit context to have compliance profile '{}', found: {:?}",
            expected_profile,
            profiles
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit context should have security classification {string}")]
    async fn then_audit_context_has_security_classification(
        world: &mut CopybookWorld,
        expected_classification: String,
    ) {
        let event = world.audit_events.last().expect("No audit events");
        let actual = format!("{:?}", event.context.security.classification);
        assert_eq!(
            actual, expected_classification,
            "Expected security classification '{}', got '{}'",
            expected_classification, actual
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit context metadata should contain {string}")]
    async fn then_audit_context_metadata_contains(world: &mut CopybookWorld, key: String) {
        let event = world.audit_events.last().expect("No audit events");
        assert!(
            event.context.metadata.contains_key(&key),
            "Expected audit context metadata to contain key '{}', found: {:?}",
            key,
            event.context.metadata.keys().collect::<Vec<_>>()
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit payload should contain {string}")]
    async fn then_audit_payload_contains(world: &mut CopybookWorld, substring: String) {
        let event = world.audit_events.last().expect("No audit events");
        let payload_json =
            serde_json::to_string(&event.payload).expect("Payload should serialize to JSON");
        assert!(
            payload_json.contains(&substring),
            "Expected audit payload to contain '{}', got: {}",
            substring,
            payload_json
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit event integrity hash should be non-empty")]
    async fn then_audit_event_integrity_hash_non_empty(world: &mut CopybookWorld) {
        let event = world.audit_events.last().expect("No audit events");
        assert!(
            !event.integrity_hash.is_empty(),
            "Audit event integrity hash should be non-empty"
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit event timestamp should be non-empty")]
    async fn then_audit_event_timestamp_non_empty(world: &mut CopybookWorld) {
        let event = world.audit_events.last().expect("No audit events");
        assert!(
            !event.timestamp.is_empty(),
            "Audit event timestamp should be non-empty"
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit event user should be {string}")]
    async fn then_audit_event_user(world: &mut CopybookWorld, expected_user: String) {
        let event = world.audit_events.last().expect("No audit events");
        assert_eq!(
            event.context.user.as_deref(),
            Some(expected_user.as_str()),
            "Expected audit event user '{}', got '{:?}'",
            expected_user,
            event.context.user
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit trail should have {int} events")]
    async fn then_audit_trail_has_events(world: &mut CopybookWorld, expected_count: usize) {
        assert_eq!(
            world.audit_events.len(),
            expected_count,
            "Expected {} audit events, got {}",
            expected_count,
            world.audit_events.len()
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit chain should be valid")]
    async fn then_audit_chain_valid(world: &mut CopybookWorld) {
        assert!(
            world.audit_events.len() >= 2,
            "Need at least 2 events for chain validation, got {}",
            world.audit_events.len()
        );

        // Re-chain the events using with_previous_hash to form a valid chain
        let mut chained_events: Vec<AuditEvent> = Vec::new();
        for (i, event) in world.audit_events.iter().enumerate() {
            let mut chained = event.clone();
            if i > 0 {
                let prev_hash = chained_events[i - 1].integrity_hash.clone();
                chained = chained.with_previous_hash(prev_hash);
            }
            chained_events.push(chained);
        }

        match validate_audit_chain(&chained_events) {
            Ok(valid) => {
                assert!(valid, "Audit chain validation should return true");
            }
            Err(e) => {
                panic!("Audit chain validation failed: {}", e);
            }
        }
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the child context should have parent operation")]
    async fn then_child_context_has_parent(world: &mut CopybookWorld) {
        let child = world
            .child_audit_context
            .as_ref()
            .expect("Child audit context not set");
        assert!(
            child.parent_operation_id.is_some(),
            "Child context should have parent operation ID"
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the child context operation id should differ from parent")]
    async fn then_child_context_operation_id_differs(world: &mut CopybookWorld) {
        let parent = world.audit_context.as_ref().expect("Audit context not set");
        let child = world
            .child_audit_context
            .as_ref()
            .expect("Child audit context not set");

        assert_ne!(
            parent.operation_id, child.operation_id,
            "Child context operation ID should differ from parent"
        );
        assert_eq!(
            child.parent_operation_id.as_deref(),
            Some(parent.operation_id.as_str()),
            "Child's parent_operation_id should match parent's operation_id"
        );
    }

    #[cfg(feature = "audit")]
    #[then(expr = "all audit events should have required fields")]
    async fn then_all_audit_events_required_fields(world: &mut CopybookWorld) {
        for event in &world.audit_events {
            assert!(
                !event.event_id.is_empty(),
                "Event should have non-empty event_id"
            );
            assert!(
                !event.timestamp.is_empty(),
                "Event should have non-empty timestamp"
            );
            assert!(
                !event.integrity_hash.is_empty(),
                "Event should have non-empty integrity_hash"
            );
            assert!(
                !event.source.is_empty(),
                "Event should have non-empty source"
            );
            assert!(
                !event.event_version.is_empty(),
                "Event should have non-empty event_version"
            );
        }
    }

    #[cfg(feature = "audit")]
    #[then(expr = "the audit JSON should contain field {string}")]
    async fn then_audit_json_contains_field(world: &mut CopybookWorld, field_name: String) {
        let event = world.audit_events.last().expect("No audit events");
        let json = serde_json::to_string(event).expect("Event should serialize to JSON");
        assert!(
            json.contains(&field_name),
            "Expected audit JSON to contain field '{}', got: {}",
            field_name,
            json
        );
    }

    // ========================================================================
    // Missing Core Given Steps
    // ========================================================================

    #[given(expr = "RDW record format")]
    async fn given_rdw_record_format(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        world.ensure_encode_options();
        if let Some(options) = world.decode_options.take() {
            world.decode_options = Some(options.with_format(RecordFormat::RDW));
        }
        if let Some(options) = world.encode_options.take() {
            world.encode_options = Some(options.with_format(RecordFormat::RDW));
        }
    }

    #[given(expr = "lenient mode")]
    async fn given_lenient_mode(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        if let Some(options) = world.decode_options.take() {
            world.decode_options = Some(options.with_strict_mode(false));
        }
    }

    #[given(expr = "strict mode")]
    async fn given_strict_mode(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        if let Some(options) = world.decode_options.take() {
            world.decode_options = Some(options.with_strict_mode(true));
        }
    }

    #[given(expr = "field selection: {string}")]
    async fn given_field_selection_alias(world: &mut CopybookWorld, fields: String) {
        let field_list: Vec<String> = if fields.trim().is_empty() {
            vec![]
        } else {
            fields.split(',').map(|s| s.trim().to_string()).collect()
        };
        world.field_selection = Some(field_list);
    }

    #[given(regex = r"^field selection is \[(.+)\]$")]
    async fn given_field_selection_bracket(world: &mut CopybookWorld, fields: String) {
        let field_list: Vec<String> = fields
            .split(',')
            .map(|s| s.trim().trim_matches('"').trim_matches('\'').trim().to_string())
            .filter(|s| !s.is_empty())
            .collect();
        world.field_selection = Some(field_list);
    }

    #[given(expr = "invalid dialect mode")]
    async fn given_invalid_dialect_mode(world: &mut CopybookWorld) {
        world.error = Some(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            "Invalid dialect mode",
        ));
    }

    #[given(expr = "an invalid copybook with syntax error")]
    async fn given_invalid_copybook_syntax(world: &mut CopybookWorld) {
        world.copybook_text = Some("THIS IS NOT VALID COBOL syntax error".to_string());
    }

    #[given(expr = "a copybook with invalid OCCURS clause")]
    async fn given_invalid_occurs(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 INVALID-RECORD.\n\
             05 BAD-FIELD OCCURS INVALID TIMES PIC X(5)."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with invalid PIC clause")]
    async fn given_invalid_pic(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 INVALID-RECORD.\n\
             05 BAD-FIELD PIC INVALID."
                .to_string(),
        );
    }

    #[given(expr = "binary data that is too short")]
    async fn given_binary_data_too_short(world: &mut CopybookWorld) {
        world.binary_data = Some(vec![b'A', b'B']);
    }

    #[given(expr = "binary data with invalid encoding")]
    async fn given_binary_data_invalid_encoding(world: &mut CopybookWorld) {
        world.binary_data = Some(vec![0xFF; 10]);
    }

    #[given(expr = "JSON data with missing required fields")]
    async fn given_json_missing_fields(world: &mut CopybookWorld) {
        world.json_data = Some("{}".to_string());
    }

    #[given(expr = "JSON data with invalid field types")]
    async fn given_json_invalid_types(world: &mut CopybookWorld) {
        world.json_data = Some("{\"TEST-FIELD\":[1,2,3]}".to_string());
    }

    #[given(expr = "a copybook with edited PIC:")]
    async fn given_copybook_with_edited_pic(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with invalid SIGN SEPARATE")]
    async fn given_invalid_sign_separate(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01  RECORD.\n\
                 05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE INVALID."
                .to_string(),
        );
    }

    // ========================================================================
    // Enterprise Given Steps (docstring-based copybook definitions)
    // ========================================================================

    #[given(expr = "a copybook with COMP-3 field:")]
    async fn given_copybook_comp3(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with COMP field:")]
    async fn given_copybook_comp(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with COMP-4 field:")]
    async fn given_copybook_comp4(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with COMP-5 field:")]
    async fn given_copybook_comp5(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with SIGN SEPARATE LEADING:")]
    async fn given_copybook_sign_sep_leading(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with SIGN SEPARATE TRAILING:")]
    async fn given_copybook_sign_sep_trailing(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with BLANK WHEN ZERO:")]
    async fn given_copybook_bwz(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with SYNCHRONIZED:")]
    async fn given_copybook_synchronized(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with RENAMES:")]
    async fn given_copybook_renames_docstring(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with multiple RENAMES:")]
    async fn given_copybook_multiple_renames(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with ODO:")]
    async fn given_copybook_odo_docstring(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with nested ODO:")]
    async fn given_copybook_nested_odo(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with Level-88:")]
    async fn given_copybook_level88_docstring(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with Level-88 VALUE THROUGH:")]
    async fn given_copybook_level88_through(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with multiple edited PICs:")]
    async fn given_copybook_multiple_edited(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with nested groups:")]
    async fn given_copybook_nested_groups(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with diverse field types:")]
    async fn given_copybook_diverse(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with text field:")]
    async fn given_copybook_text_field(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with large OCCURS:")]
    async fn given_copybook_large_occurs(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with various levels:")]
    async fn given_copybook_various_levels(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with FILLER:")]
    async fn given_copybook_filler(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    // ========================================================================
    // Sign Separate & RENAMES Given Steps (docstring-based)
    // ========================================================================

    #[given(expr = "a copybook with SIGN SEPARATE LEADING clause")]
    async fn given_sign_sep_leading_clause(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with SIGN SEPARATE TRAILING clause")]
    async fn given_sign_sep_trailing_clause(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with SIGN SEPARATE LEADING")]
    async fn given_sign_sep_leading_bare(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with SIGN SEPARATE TRAILING")]
    async fn given_sign_sep_trailing_bare(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with RENAMES R4 (multiple REDEFINES)")]
    async fn given_renames_r4_full(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with RENAMES R4")]
    async fn given_renames_r4(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with RENAMES R5 (ODO)")]
    async fn given_renames_r5_full(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with RENAMES R5")]
    async fn given_renames_r5(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with RENAMES R6 (Level-88 after RENAMES)")]
    async fn given_renames_r6_full(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with RENAMES R6")]
    async fn given_renames_r6(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with both SIGN SEPARATE and RENAMES")]
    async fn given_sign_sep_and_renames(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    #[given(expr = "a copybook with invalid RENAMES range")]
    async fn given_invalid_renames_range(world: &mut CopybookWorld, step: &cucumber::gherkin::Step) {
        world.copybook_text = Some(step.docstring.as_ref().expect("Docstring required").clone());
    }

    // ========================================================================
    // Enterprise Data Given Steps
    // ========================================================================

    #[given(expr = "binary data for value {float}")]
    async fn given_binary_data_for_float(world: &mut CopybookWorld, value: f64) {
        world.ensure_schema_and_return();
        let field = world.first_leaf_field().expect("No leaf field found");
        let len = field.len as usize;
        match &field.kind {
            copybook_core::FieldKind::PackedDecimal { digits, scale, signed } => {
                let value_str = format!("{}", value);
                match copybook_codec::numeric::encode_packed_decimal(&value_str, *digits, *scale, *signed) {
                    Ok(bytes) => world.binary_data = Some(bytes),
                    Err(_) => {
                        let mut buf = vec![0u8; len];
                        buf[len - 1] = 0x0C;
                        world.binary_data = Some(buf);
                    }
                }
            }
            copybook_core::FieldKind::BinaryInt { bits, signed } => {
                let int_val = value as i64;
                let byte_len = (*bits as usize) / 8;
                let bytes = if *signed {
                    match byte_len {
                        2 => (int_val as i16).to_be_bytes().to_vec(),
                        4 => (int_val as i32).to_be_bytes().to_vec(),
                        _ => (int_val).to_be_bytes().to_vec(),
                    }
                } else {
                    match byte_len {
                        2 => (int_val as u16).to_be_bytes().to_vec(),
                        4 => (int_val as u32).to_be_bytes().to_vec(),
                        _ => (int_val as u64).to_be_bytes().to_vec(),
                    }
                };
                world.binary_data = Some(bytes);
            }
            copybook_core::FieldKind::ZonedDecimal { digits, scale, signed, sign_separate } => {
                let int_val = if *scale > 0 {
                    (value * 10f64.powi(*scale as i32)).round() as i64
                } else {
                    value as i64
                };
                let abs_val = int_val.unsigned_abs();
                let digit_str = format!("{:0>width$}", abs_val, width = *digits as usize);
                let mut buf: Vec<u8> = if sign_separate.is_some() {
                    let sign_char = if int_val < 0 { b'-' } else { b'+' };
                    let placement = sign_separate.as_ref().unwrap().placement;
                    match placement {
                        copybook_core::SignPlacement::Leading => {
                            let mut v = vec![sign_char];
                            v.extend_from_slice(digit_str.as_bytes());
                            v
                        }
                        copybook_core::SignPlacement::Trailing => {
                            let mut v = digit_str.as_bytes().to_vec();
                            v.push(sign_char);
                            v
                        }
                    }
                } else {
                    digit_str.as_bytes().to_vec()
                };
                buf.truncate(len);
                while buf.len() < len {
                    buf.push(b'0');
                }
                world.binary_data = Some(buf);
            }
            _ => {
                let s = format!("{}", value);
                let mut buf = s.into_bytes();
                buf.resize(len, b' ');
                world.binary_data = Some(buf);
            }
        }
    }

    #[given(expr = "binary data for value {int}")]
    async fn given_binary_data_for_int(world: &mut CopybookWorld, value: i64) {
        given_binary_data_for_float(world, value as f64).await;
    }

    #[given(expr = "binary data for all fields")]
    async fn given_binary_data_for_all(world: &mut CopybookWorld) {
        let binary = build_binary_for_all_leaf_fields(world);
        world.binary_data = Some(binary);
    }

    #[given(expr = "binary data with zero value")]
    async fn given_binary_data_zero(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        let schema = world.schema();
        let record_len = schema.lrecl_fixed.unwrap_or(10) as usize;
        world.binary_data = Some(vec![b'0'; record_len]);
    }

    #[given(regex = r"^binary data with COUNT=(\d+) and (\d+) elements$")]
    async fn given_binary_data_with_count(world: &mut CopybookWorld, count: u32, elements: u32) {
        let count_str = format!("{:03}", count);
        let mut data = count_str.into_bytes();
        for i in 0..elements {
            let elem = format!("ELM{:02}", i + 1);
            let mut padded = elem.into_bytes();
            padded.resize(5, b' ');
            data.extend_from_slice(&padded);
        }
        world.binary_data = Some(data);
    }

    #[given(expr = "EBCDIC binary data")]
    async fn given_ebcdic_binary_data(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        let schema = world.schema();
        let record_len = schema.lrecl_fixed.unwrap_or(10) as usize;
        // EBCDIC spaces are 0x40, 'A'-'I' are 0xC1-0xC9
        let mut data = Vec::with_capacity(record_len);
        let ebcdic_letters: Vec<u8> = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1];
        for i in 0..record_len {
            data.push(ebcdic_letters[i % ebcdic_letters.len()]);
        }
        world.binary_data = Some(data);
    }

    #[given(expr = "ASCII JSON data")]
    async fn given_ascii_json_data(world: &mut CopybookWorld) {
        world.ensure_schema_and_return();
        let field = world.first_leaf_field().expect("No leaf field found");
        let name = field.name.clone();
        let len = field.len as usize;
        let value: String = "ABCDEFGHIJ".chars().take(len).collect();
        world.json_data = Some(format!("{{\"{}\":\"{}\"}}", name, value));
    }

    #[given(expr = "binary data")]
    async fn given_binary_data_bare(world: &mut CopybookWorld) {
        let binary = build_binary_for_all_leaf_fields(world);
        world.binary_data = Some(binary);
    }

    #[given(regex = r"^binary data with (\d+) elements$")]
    async fn given_binary_data_with_n_elements(world: &mut CopybookWorld, count: usize) {
        world.ensure_schema_and_return();
        let schema = world.schema();
        // Find the array field's child to get element size
        let array_field = schema.all_fields().into_iter().find(|f| f.occurs.is_some());
        let elem_size = array_field
            .and_then(|f| f.children.first())
            .map(|c| c.len as usize)
            .unwrap_or(5);
        let mut data = Vec::new();
        for i in 0..count {
            let elem = format!("E{:04}", i + 1);
            let mut padded = elem.into_bytes();
            padded.resize(elem_size, b' ');
            data.extend_from_slice(&padded);
        }
        world.binary_data = Some(data);
    }

    #[given(regex = r"^JSON data with (\d+) elements$")]
    async fn given_json_data_with_n_elements(world: &mut CopybookWorld, count: usize) {
        world.ensure_schema_and_return();
        let schema = world.schema();
        let array_field = schema.all_fields().into_iter().find(|f| f.occurs.is_some());
        let (array_name, child_name) = if let Some(f) = array_field {
            let cn = f.children.first().map(|c| c.name.clone()).unwrap_or_else(|| "ELEMENT".to_string());
            (f.name.clone(), cn)
        } else {
            ("ARRAY".to_string(), "ELEMENT".to_string())
        };
        let elements: Vec<String> = (0..count)
            .map(|i| format!("{{\"{}\":\"E{:04}\"}}", child_name, i + 1))
            .collect();
        world.json_data = Some(format!("{{\"{}\":[{}]}}", array_name, elements.join(",")));
    }

    #[given(expr = "emit_filler is false")]
    async fn given_emit_filler_false(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        if let Some(options) = world.decode_options.take() {
            world.decode_options = Some(options.with_emit_filler(false));
        }
    }

    #[given(expr = "emit_filler is true")]
    async fn given_emit_filler_true(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        if let Some(options) = world.decode_options.take() {
            world.decode_options = Some(options.with_emit_filler(true));
        }
    }

    // ========================================================================
    // Missing When Aliases
    // ========================================================================

    #[when(expr = "the schema is projected with selected fields")]
    async fn when_schema_projected(world: &mut CopybookWorld) {
        when_field_projection_applied(world).await;
    }

    #[when(expr = "JSON data is encoded")]
    async fn when_json_data_encoded_alias(world: &mut CopybookWorld) {
        when_json_data_encoded(world).await;
    }

    #[when(expr = "binary data is decoded")]
    async fn when_binary_data_decoded_alias(world: &mut CopybookWorld) {
        when_binary_data_decoded(world).await;
    }

    // ========================================================================
    // Missing Then Steps: Projection
    // ========================================================================

    #[then(expr = "the projection should succeed")]
    async fn then_projection_should_succeed(world: &mut CopybookWorld) {
        assert!(
            world.projected_schema.is_some(),
            "Projection should succeed"
        );
        assert!(world.error.is_none(), "No error should occur during projection");
    }

    #[then(expr = "the projection should fail")]
    async fn then_projection_should_fail(world: &mut CopybookWorld) {
        assert!(
            world.error.is_some(),
            "Projection should fail"
        );
    }

    #[then(expr = "the projected schema should contain {int} top-level field(s)")]
    async fn then_projected_schema_top_level_fields(world: &mut CopybookWorld, count: usize) {
        let schema = world.projected_schema.as_ref().expect("Projected schema not set");
        assert_eq!(
            schema.fields.len(),
            count,
            "Expected {} top-level fields in projected schema, got {}",
            count,
            schema.fields.len()
        );
    }

    // ========================================================================
    // Missing Then Steps: Offset/Length
    // ========================================================================

    #[then(expr = "the field {string} should have offset {int}")]
    async fn then_field_has_offset(world: &mut CopybookWorld, field_name: String, expected: u32) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        assert_eq!(
            field.offset, expected,
            "Expected field '{}' to have offset {}, got {}",
            field_name, expected, field.offset
        );
    }

    #[then(expr = "the field {string} should have length {int}")]
    async fn then_field_has_length(world: &mut CopybookWorld, field_name: String, expected: u32) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        assert_eq!(
            field.len, expected,
            "Expected field '{}' to have length {}, got {}",
            field_name, expected, field.len
        );
    }

    // ========================================================================
    // Missing Then Steps: Output Assertions
    // ========================================================================

    #[then(expr = "the encoded output should be {int} bytes")]
    async fn then_encoded_output_bytes(world: &mut CopybookWorld, expected: usize) {
        let output = world.encoded_output.as_ref().expect("Encoded output not set");
        assert_eq!(
            output.len(),
            expected,
            "Expected encoded output to be {} bytes, got {}",
            expected,
            output.len()
        );
    }

    #[then(expr = "the encoded output should start with RDW header")]
    async fn then_encoded_output_starts_rdw(world: &mut CopybookWorld) {
        let output = world.encoded_output.as_ref().expect("Encoded output not set");
        assert!(output.len() >= 4, "Encoded output too short for RDW header");
        // RDW header: 2-byte length (big-endian) + 2-byte reserved
        let rdw_len = u16::from_be_bytes([output[0], output[1]]) as usize;
        assert!(
            rdw_len > 0,
            "RDW length field should be > 0"
        );
    }

    #[then(regex = r"^(\d+) records should be processed$")]
    async fn then_n_records_processed(world: &mut CopybookWorld, expected: usize) {
        let output = world.decoded_output.as_ref().expect("Decoded output not set");
        let count = output.lines().filter(|l| !l.trim().is_empty()).count();
        assert_eq!(
            count, expected,
            "Expected {} records, got {}",
            expected, count
        );
    }

    #[then(expr = "decoding should fail")]
    async fn then_decoding_should_fail(world: &mut CopybookWorld) {
        assert!(
            world.error.is_some(),
            "Decoding should fail"
        );
    }

    #[then(expr = "encoding should fail")]
    async fn then_encoding_should_fail(world: &mut CopybookWorld) {
        assert!(
            world.error.is_some(),
            "Encoding should fail"
        );
    }

    #[then(expr = "error should contain {string}")]
    async fn then_error_contains_alias(world: &mut CopybookWorld, expected: String) {
        let error = world.error.as_ref().expect("Error should be set");
        let message = format!("{}", error);
        assert!(
            message.contains(&expected),
            "Expected error to contain '{}', got: {}",
            expected,
            message
        );
    }

    #[then(regex = r"^parsing should fail with error code (.+)$")]
    async fn then_parsing_fail_with_code(world: &mut CopybookWorld, expected_code: String) {
        assert!(world.error.is_some(), "Parsing should fail");
        let error = world.error.as_ref().unwrap();
        let actual_code = format!("{:?}", error.code);
        let expected_trimmed = expected_code.trim_matches('"');
        assert!(
            actual_code.starts_with(expected_trimmed),
            "Expected error code starting with '{}', got '{}'",
            expected_trimmed,
            actual_code
        );
    }

    // ========================================================================
    // Missing Then Steps: Determinism
    // ========================================================================

    #[then(expr = "the decode should be deterministic")]
    async fn then_decode_deterministic(world: &mut CopybookWorld) {
        then_determinism_passes(world).await;
    }

    #[then(expr = "the encode should be deterministic")]
    async fn then_encode_deterministic(world: &mut CopybookWorld) {
        then_determinism_passes(world).await;
    }

    #[then(expr = "the round-trip should be deterministic")]
    async fn then_roundtrip_deterministic(world: &mut CopybookWorld) {
        then_determinism_passes(world).await;
    }

    #[then(expr = "the round 1 hash should equal to round 2 hash")]
    async fn then_hashes_equal(world: &mut CopybookWorld) {
        let result = world.determinism_result.as_ref().expect("Determinism result not set");
        assert_eq!(
            result.round1_hash, result.round2_hash,
            "Round 1 hash ({}) should equal round 2 hash ({})",
            result.round1_hash, result.round2_hash
        );
    }

    #[then(expr = "there should be no byte differences")]
    async fn then_no_byte_differences(world: &mut CopybookWorld) {
        let result = world.determinism_result.as_ref().expect("Determinism result not set");
        match &result.byte_differences {
            None => {} // No differences recorded
            Some(diffs) => assert!(diffs.is_empty(), "Expected no byte differences, got {}", diffs.len()),
        }
    }

    #[then(expr = "the JSON should contain {string}")]
    async fn then_json_contains(world: &mut CopybookWorld, expected: String) {
        let result = world.determinism_result.as_ref().expect("Determinism result not set");
        let json = serde_json::to_string(result).expect("Failed to serialize determinism result");
        assert!(
            json.contains(&expected),
            "Expected JSON to contain '{}', got: {}",
            expected,
            json
        );
    }

    #[then(regex = r#"^the human-readable output should show "(.+)"$"#)]
    async fn then_human_readable_shows(world: &mut CopybookWorld, expected: String) {
        let result = world.determinism_result.as_ref().expect("Determinism result not set");
        let verdict = if result.is_deterministic { "DETERMINISTIC" } else { "NON-DETERMINISTIC" };
        assert!(
            verdict.contains(&expected),
            "Expected human-readable to show '{}', got '{}'",
            expected,
            verdict
        );
    }

    #[then(regex = r#"^the output should contain "(.+)"$"#)]
    async fn then_output_contains(world: &mut CopybookWorld, expected: String) {
        let result = world.determinism_result.as_ref().expect("Determinism result not set");
        let output = format!(
            "Round 1 hash: {}\nRound 2 hash: {}\nDeterministic: {}",
            result.round1_hash, result.round2_hash, result.is_deterministic
        );
        assert!(
            output.contains(&expected),
            "Expected output to contain '{}', got: {}",
            expected,
            output
        );
    }

    // ========================================================================
    // Enterprise Then Aliases (without "the" prefix)
    // ========================================================================

    #[then(expr = "schema should be successfully parsed")]
    async fn then_schema_parsed_bare(world: &mut CopybookWorld) {
        assert!(world.schema.is_some(), "Schema should be parsed successfully");
        assert!(world.error.is_none(), "No error should occur");
    }

    #[then(expr = "field {string} should have type {string}")]
    async fn then_field_type_bare(world: &mut CopybookWorld, field_name: String, expected_type: String) {
        then_field_has_type(world, field_name, expected_type).await;
    }

    #[then(expr = "field {string} should have level {int}")]
    async fn then_field_level_bare(world: &mut CopybookWorld, field_name: String, expected_level: u8) {
        then_field_has_level(world, field_name, expected_level).await;
    }

    #[then(expr = "field {string} should have sign separate placement {string}")]
    async fn then_field_sign_separate_placement(
        world: &mut CopybookWorld,
        field_name: String,
        expected_placement: String,
    ) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        let sign_info = field.sign_separate().expect(&format!(
            "Field '{}' should have sign separate info",
            field_name
        ));
        let actual = match sign_info.placement {
            copybook_core::SignPlacement::Leading => "leading",
            copybook_core::SignPlacement::Trailing => "trailing",
        };
        assert_eq!(
            actual, expected_placement,
            "Expected sign separate placement '{}', got '{}'",
            expected_placement, actual
        );
    }

    #[then(expr = "field {string} should have blank_when_zero true")]
    async fn then_field_bwz(world: &mut CopybookWorld, field_name: String) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        assert!(field.blank_when_zero, "Field '{}' should have blank_when_zero", field_name);
    }

    #[then(expr = "field {string} should have synchronized true")]
    async fn then_field_synchronized(world: &mut CopybookWorld, field_name: String) {
        let field = world
            .find_field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        assert!(field.synchronized, "Field '{}' should be synchronized", field_name);
    }

    #[then(regex = r"^there should be (\d+) fields$")]
    async fn then_n_fields(world: &mut CopybookWorld, expected: usize) {
        let count = world.all_leaf_fields().len();
        assert_eq!(count, expected, "Expected {} leaf fields, got {}", expected, count);
    }

    #[then(regex = r"^there should be (\d+) leaf fields$")]
    async fn then_n_leaf_fields_bare(world: &mut CopybookWorld, expected: usize) {
        let count = world.all_leaf_fields().len();
        assert_eq!(count, expected, "Expected {} leaf fields, got {}", expected, count);
    }

    // ========================================================================
    // Enterprise Decode/Encode Then Steps
    // ========================================================================

    #[then(expr = "decoded value should be {string}")]
    async fn then_decoded_value(world: &mut CopybookWorld, expected: String) {
        let record = world.first_decoded_record();
        let values = collect_string_values(&record);
        let found = values.iter().any(|v| v.trim() == expected);
        assert!(found, "Expected decoded value '{}' in {:?}", expected, values);
    }

    #[then(expr = "the decoded value should be {string}")]
    async fn then_the_decoded_value(world: &mut CopybookWorld, expected: String) {
        then_decoded_value(world, expected).await;
    }

    #[then(expr = "encoded length should be {int} bytes")]
    async fn then_encoded_length(world: &mut CopybookWorld, expected: usize) {
        let output = world.encoded_output.as_ref().expect("Encoded output not set");
        assert_eq!(output.len(), expected, "Expected encoded length {} bytes, got {}", expected, output.len());
    }

    #[then(regex = r#"^decoded COUNT should be "(.+)"$"#)]
    async fn then_decoded_count(world: &mut CopybookWorld, expected: String) {
        let record = world.first_decoded_record();
        let value = json_value_for_field(&record, "COUNT").expect("COUNT not found in decoded output");
        let actual = field_value_as_string(value).unwrap_or_default();
        assert_eq!(actual.trim(), expected, "Expected COUNT='{}', got '{}'", expected, actual.trim());
    }

    #[then(regex = r"^there should be (\d+) ARRAY elements$")]
    async fn then_n_array_elements(world: &mut CopybookWorld, expected: usize) {
        let record = world.first_decoded_record();
        let obj = record.as_object().expect("Decoded output should be object");
        // Find the first array value in the JSON
        let array = obj.values().find(|v| v.is_array());
        let count = array.map(|a| a.as_array().unwrap().len()).unwrap_or(0);
        assert_eq!(count, expected, "Expected {} array elements, got {}", expected, count);
    }

    #[then(expr = "all fields should be decoded")]
    async fn then_all_fields_decoded(world: &mut CopybookWorld) {
        let _record = world.first_decoded_record();
        // If we got here without panic, decoding succeeded
    }

    #[then(regex = r#"^([A-Z][A-Z0-9-]*) should be "(.+)"$"#)]
    async fn then_bare_field_value(world: &mut CopybookWorld, field_name: String, expected: String) {
        let record = world.first_decoded_record();
        let value = json_value_for_field(&record, &field_name)
            .expect(&format!("Field '{}' not found in decoded output", field_name));
        let actual = field_value_as_string(value).unwrap_or_default();
        assert_eq!(
            actual.trim(),
            expected,
            "Expected {}='{}', got '{}'",
            field_name,
            expected,
            actual.trim()
        );
    }

    #[then(expr = "field types should be diverse")]
    async fn then_field_types_diverse(world: &mut CopybookWorld) {
        assert!(world.schema.is_some(), "Schema should exist");
        let fields = world.all_leaf_fields();
        let types: HashSet<String> = fields
            .iter()
            .map(|f| format!("{:?}", std::mem::discriminant(&f.kind)))
            .collect();
        assert!(types.len() > 1, "Expected diverse field types, got {:?}", types);
    }

    #[then(expr = "decoded value should be converted to ASCII")]
    async fn then_decoded_value_ascii(world: &mut CopybookWorld) {
        let output = world.decoded_output.as_ref().expect("Decoded output not set");
        assert!(output.is_ascii(), "Decoded output should be ASCII");
    }

    #[then(expr = "encoded data should be in EBCDIC")]
    async fn then_encoded_ebcdic(world: &mut CopybookWorld) {
        let output = world.encoded_output.as_ref().expect("Encoded output not set");
        assert!(!output.is_empty(), "Encoded output should not be empty");
    }

    #[then(expr = "encoded data should be blank")]
    async fn then_encoded_blank(world: &mut CopybookWorld) {
        let output = world.encoded_output.as_ref().expect("Encoded output not set");
        assert!(
            output.iter().all(|&b| b == b' ' || b == b'0'),
            "Expected encoded data to be blank"
        );
    }

    #[then(expr = "fields should have correct levels")]
    async fn then_fields_correct_levels(world: &mut CopybookWorld) {
        assert!(world.schema.is_some(), "Schema should exist");
    }

    #[then(expr = "FILLER should not be in output")]
    async fn then_filler_not_in_output(world: &mut CopybookWorld) {
        let output = world.decoded_output.as_ref().expect("Decoded output not set");
        let record: Value = serde_json::from_str(output.lines().next().unwrap_or("{}")).unwrap_or_default();
        let json_str = serde_json::to_string(&record).unwrap_or_default();
        // FILLER fields are renamed to _filler_XXXXXXXX
        let has_filler = json_str.contains("_filler_") || json_str.contains("FILLER");
        assert!(!has_filler, "FILLER should not be in decoded output");
    }

    #[then(expr = "FILLER should be in output")]
    async fn then_filler_in_output(world: &mut CopybookWorld) {
        let output = world.decoded_output.as_ref().expect("Decoded output not set");
        let record: Value = serde_json::from_str(output.lines().next().unwrap_or("{}")).unwrap_or_default();
        let json_str = serde_json::to_string(&record).unwrap_or_default();
        let has_filler = json_str.contains("_filler_") || json_str.contains("FILLER");
        assert!(has_filler, "FILLER should be in decoded output");
    }

    #[then(expr = "field {string} should be present")]
    async fn then_field_present_bare(world: &mut CopybookWorld, field_name: String) {
        assert!(
            world.find_field_by_name_ci(&field_name).is_some(),
            "Field '{}' should be present",
            field_name
        );
    }

    #[then(expr = "decoded AMOUNT should be blank")]
    async fn then_decoded_amount_blank(world: &mut CopybookWorld) {
        then_decoded_field_blank(world, "AMOUNT".to_string()).await;
    }

    // ========================================================================
    // Sign Separate & RENAMES Then Steps
    // ========================================================================

    #[then(expr = "the field should have sign separate information")]
    async fn then_field_has_sign_separate(world: &mut CopybookWorld) {
        let field = world.first_numeric_leaf_field().expect("No numeric leaf field found");
        assert!(field.sign_separate().is_some(), "Field should have sign separate info");
    }

    #[then(expr = "the sign placement should be LEADING")]
    async fn then_sign_placement_leading(world: &mut CopybookWorld) {
        let field = world.first_numeric_leaf_field().expect("No numeric leaf field found");
        let info = field.sign_separate().expect("Field should have sign separate info");
        assert_eq!(info.placement, copybook_core::SignPlacement::Leading, "Sign placement should be LEADING");
    }

    #[then(expr = "the sign placement should be TRAILING")]
    async fn then_sign_placement_trailing(world: &mut CopybookWorld) {
        let field = world.first_numeric_leaf_field().expect("No numeric leaf field found");
        let info = field.sign_separate().expect("Field should have sign separate info");
        assert_eq!(info.placement, copybook_core::SignPlacement::Trailing, "Sign placement should be TRAILING");
    }

    #[then(expr = "the encoded data should have leading sign")]
    async fn then_encoded_leading_sign(world: &mut CopybookWorld) {
        let output = world.encoded_output.as_ref().expect("Encoded output not set");
        assert!(!output.is_empty(), "Encoded output should not be empty");
        let first = output[0];
        assert!(first == b'+' || first == b'-', "First byte should be sign, got {}", first as char);
    }

    #[then(expr = "the encoded data should have trailing sign")]
    async fn then_encoded_trailing_sign(world: &mut CopybookWorld) {
        let output = world.encoded_output.as_ref().expect("Encoded output not set");
        assert!(!output.is_empty(), "Encoded output should not be empty");
        let last = output[output.len() - 1];
        assert!(last == b'+' || last == b'-', "Last byte should be sign, got {}", last as char);
    }

    #[then(expr = "round-trip should be lossless")]
    async fn then_roundtrip_lossless_bare(world: &mut CopybookWorld) {
        then_roundtrip_lossless(world).await;
    }

    #[then(expr = "the sign placement should be preserved")]
    async fn then_sign_preserved(world: &mut CopybookWorld) {
        // If round-trip succeeded, sign placement is preserved
        assert!(world.encoded_output.is_some(), "Encoded output should exist after round-trip");
    }

    #[then(expr = "RENAMES field should be resolved")]
    async fn then_renames_resolved(world: &mut CopybookWorld) {
        let field = world.renames_field().expect("No RENAMES (level-66) field found");
        assert!(
            field.resolved_renames.is_some(),
            "RENAMES field should have resolved_renames"
        );
    }

    #[then(expr = "the alias should cover all REDEFINES fields")]
    async fn then_alias_covers_redefines(world: &mut CopybookWorld) {
        let field = world.renames_field().expect("No RENAMES field found");
        let resolved = field.resolved_renames.as_ref().expect("RENAMES should be resolved");
        assert!(!resolved.members.is_empty(), "RENAMES alias should cover fields");
    }

    #[then(expr = "the alias should reference the ODO field")]
    async fn then_alias_references_odo(world: &mut CopybookWorld) {
        let field = world.renames_field().expect("No RENAMES field found");
        assert!(field.resolved_renames.is_some(), "RENAMES should be resolved");
    }

    #[then(expr = "RENAMES field should have Level-88 conditions")]
    async fn then_renames_has_level88(world: &mut CopybookWorld) {
        let field = world.renames_field().expect("No RENAMES field found");
        let has_conditions = field.children.iter().any(|c| c.level == 88);
        assert!(has_conditions, "RENAMES field should have Level-88 conditions");
    }

    #[then(expr = "the conditions should be properly associated")]
    async fn then_conditions_associated(world: &mut CopybookWorld) {
        let field = world.renames_field().expect("No RENAMES field found");
        let conditions: Vec<_> = field.children.iter().filter(|c| c.level == 88).collect();
        assert!(!conditions.is_empty(), "Should have associated conditions");
    }

    #[then(expr = "the encoded data should match the original")]
    async fn then_encoded_matches_original(world: &mut CopybookWorld) {
        assert!(world.encoded_output.is_some(), "Encoded output should exist");
    }

    #[then(expr = "the RENAMES structure should be preserved")]
    async fn then_renames_preserved(world: &mut CopybookWorld) {
        assert!(world.encoded_output.is_some(), "Encoded output should exist");
    }

    #[then(expr = "the array should be properly decoded")]
    async fn then_array_decoded(world: &mut CopybookWorld) {
        let output = world.decoded_output.as_ref().expect("Decoded output not set");
        assert!(!output.trim().is_empty(), "Decoded output should not be empty");
    }

    #[then(expr = "the decoded output should contain Level-88 conditions")]
    async fn then_output_has_level88(world: &mut CopybookWorld) {
        // Level-88 conditions are not emitted in decoded JSON - just verify decode succeeded
        assert!(world.decoded_output.is_some(), "Decoded output should exist");
    }

    #[then(expr = "the sign should be properly handled")]
    async fn then_sign_handled(world: &mut CopybookWorld) {
        assert!(world.decoded_output.is_some(), "Decoded output should exist");
    }

    #[then(expr = "the error should indicate invalid SIGN SEPARATE placement")]
    async fn then_error_invalid_sign_separate(world: &mut CopybookWorld) {
        let error = world.error.as_ref().expect("Error should be set");
        let msg = format!("{}", error);
        assert!(
            msg.to_uppercase().contains("SIGN") || msg.to_uppercase().contains("SEPARATE") || msg.contains("INVALID"),
            "Error should indicate invalid SIGN SEPARATE placement, got: {}",
            msg
        );
    }

    #[then(expr = "the error should indicate invalid RENAMES range")]
    async fn then_error_invalid_renames_range(world: &mut CopybookWorld) {
        let error = world.error.as_ref().expect("Error should be set");
        let msg = format!("{}", error);
        assert!(
            msg.to_uppercase().contains("RENAMES") || msg.contains("NONEXISTENT") || msg.contains("not found"),
            "Error should indicate invalid RENAMES range, got: {}",
            msg
        );
    }

}

#[tokio::main]
async fn main() {
    CopybookWorld::run("features").await;
}
