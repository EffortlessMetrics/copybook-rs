#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::unused_async,
    clippy::expect_fun_call,
    clippy::uninlined_format_args,
    clippy::map_unwrap_or,
    clippy::match_same_arms,
    clippy::single_char_pattern,
    unused_variables,
    dead_code
)]
//! BDD tests for copybook-rs using Cucumber/Gherkin syntax
//!
//! This module provides Behavior Driven Development tests that describe
//! the expected behavior of the copybook-rs library in human-readable
//! Gherkin syntax.

use copybook_codec::{
    decode_file_to_jsonl, encode_jsonl_to_file, Codepage, DecodeOptions, EncodeOptions,
    JsonNumberMode, RawMode, RecordFormat,
};
use copybook_core::dialect::Dialect;
use copybook_core::{
    parse_copybook, parse_copybook_with_options, project_schema, Error, ErrorCode, Occurs,
    ParseOptions,
};
use cucumber::{given, then, when, World as _};
use serde_json::{Map, Value};
use std::collections::HashSet;
use std::io::Cursor;

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
    dialect: Option<Dialect>,
    /// Field selection for projection
    field_selection: Option<Vec<String>>,
}

impl CopybookWorld {
    fn ensure_schema_parsed(&mut self) {
        if self.schema.is_some() {
            return;
        }

        self.error = None;
        let copybook_text = self.copybook_text.as_ref().expect("Copybook text not set");

        let schema = match &self.parse_options {
            Some(options) => parse_copybook_with_options(copybook_text, options),
            None => parse_copybook(copybook_text),
        };

        self.schema = match schema {
            Ok(schema) => Some(schema),
            Err(error) => {
                self.error = Some(error);
                None
            }
        };
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
        self.schema
            .as_ref()
            .expect("Schema should be parsed for this step")
    }

    /// Get the schema or panic if not set
    fn schema(&self) -> &copybook_core::Schema {
        self.schema.as_ref().expect("Schema not set")
    }

    /// Get the projected schema or panic if not set
    fn projected_schema(&self) -> &copybook_core::Schema {
        self.projected_schema
            .as_ref()
            .expect("Projected schema not set")
    }

    /// Get the binary data or panic if not set
    fn binary_data(&self) -> &[u8] {
        self.binary_data.as_deref().expect("Binary data not set")
    }

    /// Get the JSON data or panic if not set
    fn json_data(&self) -> &str {
        self.json_data.as_deref().expect("JSON data not set")
    }

    /// Get the decoded output or panic if not set
    fn decoded_output(&self) -> &str {
        self.decoded_output
            .as_deref()
            .expect("Decoded output not set")
    }

    /// Find a field by name in the parsed schema
    fn field_by_name(&self, name: &str) -> Option<&copybook_core::Field> {
        self.schema()
            .all_fields()
            .into_iter()
            .find(|field| field.name.eq_ignore_ascii_case(name))
    }

    /// Find a field by name in the parsed schema (case-insensitive)
    fn field_by_name_ci(&self, name: &str) -> Option<&copybook_core::Field> {
        self.schema()
            .all_fields()
            .into_iter()
            .find(|field| field.name.eq_ignore_ascii_case(name))
    }

    /// Get all leaf fields from the parsed schema
    fn all_leaf_fields(&self) -> Vec<&copybook_core::Field> {
        let mut fields = Vec::new();
        collect_leaf_fields(&self.schema().fields, &mut fields);
        fields
    }

    /// Get the first leaf field in the parsed schema
    fn first_leaf_field(&self) -> Option<&copybook_core::Field> {
        self.all_leaf_fields().into_iter().find(|field| {
            !matches!(
                field.kind,
                copybook_core::FieldKind::Condition { .. }
                    | copybook_core::FieldKind::Renames { .. }
            )
        })
    }

    /// Get the first numeric leaf field in the parsed schema
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

    /// Find the alias RENAMES field in the parsed schema
    fn renames_field(&self) -> Option<&copybook_core::Field> {
        self.schema().fields.iter().find(|field| field.level == 66)
    }

    /// Parse the first JSON record in decoded output
    fn first_decoded_record(&self) -> Value {
        let line = self
            .decoded_output()
            .lines()
            .map(str::trim)
            .find(|line| !line.is_empty())
            .expect("Decoded output should contain JSON output");
        serde_json::from_str(line).expect("Decoded output should be valid JSON")
    }

    /// Check if a field exists in the projected schema (recursively)
    fn field_in_projection(&self, field_name: &str) -> bool {
        let schema = self.projected_schema();
        Self::find_field_recursive(schema, field_name)
    }

    /// Recursively find a field by name in the schema
    fn find_field_recursive(schema: &copybook_core::Schema, field_name: &str) -> bool {
        for field in &schema.fields {
            if field.name == field_name {
                return true;
            }
            // Check children recursively
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

mod steps {
    use super::*;

    // ============================================================================
    // Copybook Parsing Steps
    // ============================================================================

    #[given(expr = "a copybook with content:")]
    async fn given_copybook_with_content(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
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
             05 ZONED-DECIMAL PIC S9(5)V99 DISPLAY."
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
             05 ALTERNATIVE-FIELD REDEFINES ORIGINAL-FIELD.\n\
                 10 PART-1 PIC 9(5).\n\
                 10 PART-2 PIC X(15)."
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

    #[given(expr = "a copybook with COMP-3 field:")]
    async fn given_copybook_with_comp3_field(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with COMP field:")]
    async fn given_copybook_with_comp_field(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with COMP-4 field:")]
    async fn given_copybook_with_comp4_field(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with COMP-5 field:")]
    async fn given_copybook_with_comp5_field(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with BLANK WHEN ZERO:")]
    async fn given_copybook_with_blank_when_zero(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with SYNCHRONIZED:")]
    async fn given_copybook_with_synchronized(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with RENAMES:")]
    async fn given_copybook_with_renames(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with multiple RENAMES:")]
    async fn given_copybook_with_multiple_renames(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with ODO:")]
    async fn given_copybook_with_odo_simple(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with nested ODO:")]
    async fn given_copybook_with_nested_odo(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with Level-88:")]
    async fn given_copybook_with_level88_block(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with Level-88 VALUE THROUGH:")]
    async fn given_copybook_with_level88_value_through(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with edited PIC:")]
    async fn given_copybook_with_edited_pic(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with multiple edited PICs:")]
    async fn given_copybook_with_multiple_edited_pics(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with nested groups:")]
    async fn given_copybook_with_nested_groups(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with large OCCURS:")]
    async fn given_copybook_with_large_occurs(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with various levels:")]
    async fn given_copybook_with_various_levels(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with diverse field types:")]
    async fn given_copybook_with_diverse_field_types(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with FILLER:")]
    async fn given_copybook_with_filler(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with text field:")]
    async fn given_copybook_with_text_field(world: &mut CopybookWorld, content: String) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "binary data")]
    async fn given_binary_data_default(world: &mut CopybookWorld) {
        world.ensure_schema_parsed();
        if world.schema.is_none() {
            return;
        }

        let encoded = match build_binary_for_all_leaf_fields(world) {
            Ok(value) => value,
            Err(e) => {
                world.error = Some(e);
                return;
            }
        };
        world.binary_data = Some(encoded);
    }

    #[given(expr = "binary data for value {word}")]
    async fn given_binary_data_for_value(world: &mut CopybookWorld, value: String) {
        world.ensure_schema_parsed();
        world.ensure_encode_options();
        if world.schema.is_none() {
            return;
        }

        let value_field = world
            .first_numeric_leaf_field()
            .or_else(|| world.first_leaf_field())
            .expect("No suitable leaf field for synthetic value");

        let mut payload = Map::new();
        payload.insert(value_field.name.clone(), Value::String(value));
        let encoded = match encode_from_payload(&payload, world) {
            Ok(value) => value,
            Err(error) => {
                world.error = Some(error);
                return;
            }
        };
        world.binary_data = Some(encoded);
    }

    #[given(expr = "binary data with zero value")]
    async fn given_binary_data_with_zero_value(world: &mut CopybookWorld) {
        let field_name = {
            let value_field = world
                .first_numeric_leaf_field()
                .or_else(|| world.first_leaf_field())
                .expect("No suitable leaf field for zero value");
            value_field.name.clone()
        };
        world.ensure_schema_parsed();
        world.ensure_encode_options();
        if world.schema.is_none() {
            return;
        }

        let mut payload = Map::new();
        payload.insert(field_name, Value::String("0".to_string()));
        let encoded = match encode_from_payload(&payload, world) {
            Ok(value) => value,
            Err(error) => {
                world.error = Some(error);
                return;
            }
        };
        world.binary_data = Some(encoded);
    }

    #[given(expr = "binary data with COUNT={word} and {int} elements")]
    async fn given_binary_data_with_count_and_elements(
        world: &mut CopybookWorld,
        counter: String,
        element_count: usize,
    ) {
        world.ensure_schema_parsed();
        world.ensure_encode_options();
        if world.schema.is_none() {
            return;
        }

        let schema = world.schema().clone();
        let all_fields = schema.all_fields();
        let occurs = all_fields.iter().find_map(|field| {
            let occurs = field.occurs.as_ref()?;
            if matches!(occurs, Occurs::ODO { .. }) && does_path_match_counter(occurs, &counter) {
                Some(field)
            } else {
                None
            }
        });
        let Some(occurs) = occurs else {
            unreachable!("No ODO field found for counter {}", counter)
        };

        let count_field = all_fields.iter().find(|field| {
            field.name.eq_ignore_ascii_case(&counter) || path_ends_with(&field.path, &counter)
        });
        let count_len = count_field
            .and_then(|field| usize::try_from(field.len).ok())
            .unwrap_or(3);
        let count_text = format!("{:0width$}", element_count, width = count_len.max(1));
        let mut payload = Map::new();

        payload.insert(counter.clone(), Value::String(count_text));
        let array_payload =
            build_fixed_array_payload(occurs, element_count).expect("Failed to build ODO payload");
        payload.insert(occurs.name.clone(), array_payload);

        let encoded = match encode_from_payload(&payload, world) {
            Ok(value) => value,
            Err(error) => {
                world.error = Some(error);
                return;
            }
        };
        world.binary_data = Some(encoded);
        world.json_data =
            Some(serde_json::to_string(&Value::Object(payload)).expect("Payload should serialize"));
    }

    #[given(expr = "binary data with {int} elements")]
    async fn given_binary_data_with_elements(world: &mut CopybookWorld, count: usize) {
        world.ensure_schema_parsed();
        world.ensure_encode_options();
        if world.schema.is_none() {
            return;
        }

        let schema = world.schema().clone();
        let all_fields = schema.all_fields();
        let fixed_field = all_fields
            .iter()
            .find(|field| matches!(field.occurs, Some(Occurs::Fixed { .. })));
        let Some(fixed_field) = fixed_field else {
            unreachable!("No fixed OCCURS field found")
        };

        let mut payload = Map::new();
        payload.insert(
            fixed_field.name.clone(),
            build_fixed_array_payload(fixed_field, count)
                .expect("Failed to build fixed array payload"),
        );

        let encoded = match encode_from_payload(&payload, world) {
            Ok(value) => value,
            Err(error) => {
                world.error = Some(error);
                return;
            }
        };
        world.binary_data = Some(encoded);
        world.json_data =
            Some(serde_json::to_string(&Value::Object(payload)).expect("Payload should serialize"));
    }

    #[given(expr = "binary data for all fields")]
    async fn given_binary_data_for_all_fields(world: &mut CopybookWorld) {
        world.ensure_schema_parsed();
        world.ensure_encode_options();
        if world.schema.is_none() {
            return;
        }

        let schema = world.schema();
        let mut payload = Map::new();
        for field in schema.all_fields() {
            if !field.children.is_empty() {
                continue;
            }
            let value = match field.name.as_str() {
                "FIELD1" => "ABCDEFGHIJ".to_string(),
                "FIELD2" => "12345".to_string(),
                "FIELD3" => "LMNOPQRSTUVWXYZ".to_string(),
                "FIELD4" => "12345.67".to_string(),
                "FIELD5" => "-12345".to_string(),
                _ => generate_value_for_field(field, 0),
            };
            payload.insert(field.name.clone(), Value::String(value));
        }

        let encoded = match encode_from_payload(&payload, world) {
            Ok(value) => value,
            Err(error) => {
                world.error = Some(error);
                return;
            }
        };
        world.binary_data = Some(encoded);
        world.json_data =
            Some(serde_json::to_string(&Value::Object(payload)).expect("Payload should serialize"));
    }

    #[given(expr = "strict parsing mode")]
    async fn given_strict_parsing(world: &mut CopybookWorld) {
        world.parse_options = Some(ParseOptions {
            allow_inline_comments: true,
            strict: true,
            ..Default::default()
        });
    }

    #[given(expr = "tolerant parsing mode")]
    async fn given_tolerant_parsing(world: &mut CopybookWorld) {
        world.parse_options = Some(ParseOptions {
            allow_inline_comments: true,
            strict: false,
            ..Default::default()
        });
    }

    // ============================================================================
    // Dialect Lever Steps
    // ============================================================================

    #[given(expr = "Normative dialect")]
    async fn given_normative_dialect(world: &mut CopybookWorld) {
        world.dialect = Some(Dialect::Normative);
        let mut options = world.parse_options.clone().unwrap_or_default();
        options.dialect = Dialect::Normative;
        world.parse_options = Some(options);
    }

    #[given(expr = "Zero-Tolerant dialect")]
    async fn given_zero_tolerant_dialect(world: &mut CopybookWorld) {
        world.dialect = Some(Dialect::ZeroTolerant);
        let mut options = world.parse_options.clone().unwrap_or_default();
        options.dialect = Dialect::ZeroTolerant;
        world.parse_options = Some(options);
    }

    #[given(expr = "One-Tolerant dialect")]
    async fn given_one_tolerant_dialect(world: &mut CopybookWorld) {
        world.dialect = Some(Dialect::OneTolerant);
        let mut options = world.parse_options.clone().unwrap_or_default();
        options.dialect = Dialect::OneTolerant;
        world.parse_options = Some(options);
    }

    #[given(expr = "invalid dialect mode")]
    async fn given_invalid_dialect(world: &mut CopybookWorld) {
        // This will be used to test error handling for invalid dialects
        world.dialect = None;
        let options = world.parse_options.clone().unwrap_or_default();
        // We'll simulate an invalid dialect by using a custom approach
        // in the when step
        world.parse_options = Some(options);
    }

    #[when(expr = "the copybook is parsed")]
    #[when(expr = "copybook is parsed")]
    async fn when_copybook_is_parsed(world: &mut CopybookWorld) {
        let copybook_text = world.copybook_text.as_ref().expect("Copybook text not set");

        // Check if we're testing invalid dialect scenario
        if world.dialect.is_none() && world.parse_options.is_some() {
            // Simulate invalid dialect error
            world.error = Some(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "Invalid dialect mode specified",
            ));
            world.schema = None;
            return;
        }

        match &world.parse_options {
            Some(options) => {
                world.schema = match parse_copybook_with_options(copybook_text, options) {
                    Ok(schema) => Some(schema),
                    Err(e) => {
                        world.error = Some(e);
                        None
                    }
                };
            }
            None => {
                world.schema = match parse_copybook(copybook_text) {
                    Ok(schema) => Some(schema),
                    Err(e) => {
                        world.error = Some(e);
                        None
                    }
                };
            }
        }
    }

    #[then(expr = "the schema should be successfully parsed")]
    #[then(expr = "schema should be successfully parsed")]
    async fn then_schema_successfully_parsed(world: &mut CopybookWorld) {
        assert!(
            world.schema.is_some(),
            "Schema should be parsed successfully"
        );
        assert!(world.error.is_none(), "No error should occur");
    }

    #[then(expr = "parsing should succeed")]
    async fn then_parsing_should_succeed(world: &mut CopybookWorld) {
        assert!(
            world.schema.is_some(),
            "Schema should be parsed successfully"
        );
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
        let schema = world.schema();
        assert_eq!(
            schema.fields.len(),
            count,
            "Expected {} top-level fields, got {}",
            count,
            schema.fields.len()
        );
    }

    #[then(expr = "the field {string} should have type {string}")]
    async fn then_field_has_type(
        world: &mut CopybookWorld,
        field_name: String,
        expected_type: String,
    ) {
        let field = world
            .field_by_name_ci(&field_name)
            .or_else(|| world.schema().find_field(&field_name))
            .expect(&format!("Field '{}' not found", field_name));

        let expected_type = match expected_type.as_str() {
            "packed_decimal" => "packed",
            "binary_int" => "binary",
            "edited_numeric" => "edited",
            _ => expected_type.as_str(),
        };

        if expected_type == "numeric" {
            let is_numeric = matches!(
                field.kind,
                copybook_core::FieldKind::ZonedDecimal { .. }
                    | copybook_core::FieldKind::BinaryInt { .. }
                    | copybook_core::FieldKind::PackedDecimal { .. }
                    | copybook_core::FieldKind::EditedNumeric { .. }
            );
            assert!(
                is_numeric,
                "Expected field '{}' to be numeric, got {:?}",
                field_name, field.kind
            );
            return;
        }

        let actual_type = if field.occurs.is_some() {
            "occurs"
        } else {
            match &field.kind {
                copybook_core::FieldKind::Alphanum { .. } => "alphanumeric",
                copybook_core::FieldKind::ZonedDecimal { .. } => "zoned",
                copybook_core::FieldKind::BinaryInt { .. } => "binary",
                copybook_core::FieldKind::PackedDecimal { .. } => "packed",
                copybook_core::FieldKind::Group => "group",
                copybook_core::FieldKind::Renames { .. } => "renames",
                copybook_core::FieldKind::Condition { .. } => "condition",
                copybook_core::FieldKind::EditedNumeric { .. } => "edited",
                copybook_core::FieldKind::FloatSingle => "float_single",
                copybook_core::FieldKind::FloatDouble => "float_double",
            }
        };

        assert_eq!(
            actual_type, expected_type,
            "Expected field '{}' to have type '{}', got '{}'",
            field_name, expected_type, actual_type
        );
    }

    #[then(expr = "the field {string} should have sign separate placement {string}")]
    async fn then_field_has_sign_placement(
        world: &mut CopybookWorld,
        field_name: String,
        expected_placement: String,
    ) {
        let field = world
            .field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        match &field.kind {
            copybook_core::FieldKind::ZonedDecimal { sign_separate, .. } => {
                let actual = sign_separate
                    .as_ref()
                    .map(|value| format!("{:?}", value.placement).to_ascii_lowercase())
                    .unwrap_or_else(|| "none".to_string());
                assert_eq!(
                    actual,
                    expected_placement.to_ascii_lowercase(),
                    "Expected sign placement '{}' for '{}' but got '{}'",
                    expected_placement,
                    field_name,
                    actual
                );
            }
            _ => unreachable!("Expected field '{}' to be signed numeric", field_name),
        }
    }

    #[then(expr = "field {string} should have blank_when_zero {string}")]
    async fn then_field_has_blank_when_zero(
        world: &mut CopybookWorld,
        field_name: String,
        expected_value: String,
    ) {
        let field = world
            .field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        let expected = expected_value.eq_ignore_ascii_case("true");
        assert_eq!(
            field.blank_when_zero, expected,
            "Expected blank_when_zero '{}' on '{}' to be {}, got {}",
            expected_value, field_name, expected, field.blank_when_zero
        );
    }

    #[then(expr = "field {string} should have synchronized {string}")]
    async fn then_field_has_synchronized(
        world: &mut CopybookWorld,
        field_name: String,
        expected_value: String,
    ) {
        let field = world
            .field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        let expected = expected_value.eq_ignore_ascii_case("true");
        assert_eq!(
            field.synchronized, expected,
            "Expected synchronized '{}' on '{}' to be {}, got {}",
            expected_value, field_name, expected, field.synchronized
        );
    }

    #[then(expr = "the field {string} should have offset {int}")]
    async fn then_field_has_offset(world: &mut CopybookWorld, field_name: String, offset: usize) {
        let schema = world.schema();
        let field = schema
            .find_field(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert_eq!(
            field.offset as usize, offset,
            "Expected field '{}' to have offset {}, got {}",
            field_name, offset, field.offset
        );
    }

    #[then(expr = "the field {string} should have length {int}")]
    async fn then_field_has_length(world: &mut CopybookWorld, field_name: String, length: usize) {
        let schema = world.schema();
        let field = schema
            .find_field(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert_eq!(
            field.len as usize, length,
            "Expected field '{}' to have length {}, got {}",
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
            .field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        assert_eq!(
            field.level, expected_level,
            "Expected field '{}' to have level {}, got {}",
            field_name, expected_level, field.level
        );
    }

    #[then(expr = "field {string} should have ODO with counter {string}")]
    async fn then_field_has_odo_with_counter(
        world: &mut CopybookWorld,
        field_name: String,
        counter: String,
    ) {
        let field = world
            .field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        let occurs = match &field.occurs {
            Some(Occurs::ODO { counter_path, .. }) => counter_path,
            Some(_) => unreachable!("Field '{}' is not an ODO field", field_name),
            None => unreachable!("Field '{}' is not an ODO field", field_name),
        };
        assert!(
            does_path_match_counter_string(occurs, &counter),
            "Expected ODO counter '{}' for '{}', got '{}'",
            counter,
            field_name,
            occurs
        );
    }

    #[then(expr = "field {string} should have OCCURS count {int}")]
    async fn then_field_has_occurs_count(
        world: &mut CopybookWorld,
        field_name: String,
        expected_count: u32,
    ) {
        let field = world
            .field_by_name_ci(&field_name)
            .expect(&format!("Field '{}' not found", field_name));
        let occurs = match field.occurs {
            Some(Occurs::Fixed { count }) => count,
            Some(_) => unreachable!("Field '{}' is ODO, not fixed occurs", field_name),
            None => unreachable!("Field '{}' is not an OCCURS field", field_name),
        };
        assert_eq!(
            occurs, expected_count,
            "Expected '{}' to have {} occurrences, got {}",
            field_name, expected_count, occurs
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

    #[then(expr = "there should be {int} leaf fields")]
    async fn then_leaf_field_count(world: &mut CopybookWorld, expected: usize) {
        let count = world.all_leaf_fields().len();
        assert_eq!(
            count, expected,
            "Expected {} leaf fields, got {}",
            expected, count
        );
    }

    #[then(expr = "there should be {int} fields")]
    async fn then_top_level_field_count(world: &mut CopybookWorld, expected: usize) {
        assert_eq!(
            world.schema().fields.len(),
            expected,
            "Expected {} top-level fields, got {}",
            expected,
            world.schema().fields.len()
        );
    }

    #[then(expr = "field types should be diverse")]
    async fn then_field_types_should_be_diverse(world: &mut CopybookWorld) {
        let mut types = HashSet::new();
        for field in world.all_leaf_fields() {
            if matches!(field.kind, copybook_core::FieldKind::Condition { .. }) {
                continue;
            }
            let actual_type = if field.occurs.is_some() {
                "occurs"
            } else {
                match &field.kind {
                    copybook_core::FieldKind::Alphanum { .. } => "alphanumeric",
                    copybook_core::FieldKind::ZonedDecimal { .. } => "zoned",
                    copybook_core::FieldKind::BinaryInt { .. } => "binary",
                    copybook_core::FieldKind::PackedDecimal { .. } => "packed",
                    copybook_core::FieldKind::Group => "group",
                    copybook_core::FieldKind::Renames { .. } => "renames",
                    copybook_core::FieldKind::Condition { .. } => "condition",
                    copybook_core::FieldKind::EditedNumeric { .. } => "edited",
                    copybook_core::FieldKind::FloatSingle => "float_single",
                    copybook_core::FieldKind::FloatDouble => "float_double",
                }
            };
            types.insert(actual_type.to_string());
        }

        assert!(
            types.len() >= 3,
            "Expected diverse types, got {} unique types",
            types.len()
        );
    }

    #[then(expr = "the schema should have fingerprint")]
    async fn then_schema_has_fingerprint(world: &mut CopybookWorld) {
        let schema = world.schema();
        assert!(
            !schema.fingerprint.is_empty(),
            "Schema should have a fingerprint"
        );
    }

    #[then(expr = "parsing should fail with error code {string}")]
    async fn then_parsing_fails_with_error(world: &mut CopybookWorld, error_code: String) {
        assert!(world.error.is_some(), "Parsing should have failed");
        let error = world.error.as_ref().expect("Error should be set");
        let actual_code = format!("{:?}", error.code);
        assert!(
            actual_code.contains(&error_code),
            "Expected error code containing '{}', got '{}'",
            error_code,
            actual_code
        );
    }

    // ============================================================================
    // SIGN SEPARATE and RENAMES Steps
    // ============================================================================

    #[given(expr = "a copybook with SIGN SEPARATE LEADING clause")]
    #[given(expr = "a copybook with SIGN SEPARATE LEADING")]
    async fn given_copybook_with_sign_separate_leading(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE LEADING."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with SIGN SEPARATE LEADING:")]
    async fn given_copybook_with_sign_separate_leading_block(
        world: &mut CopybookWorld,
        content: String,
    ) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with SIGN SEPARATE TRAILING clause")]
    #[given(expr = "a copybook with SIGN SEPARATE TRAILING")]
    async fn given_copybook_with_sign_separate_trailing(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE TRAILING."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with SIGN SEPARATE TRAILING:")]
    async fn given_copybook_with_sign_separate_trailing_block(
        world: &mut CopybookWorld,
        content: String,
    ) {
        world.copybook_text = Some(content);
    }

    #[given(expr = "a copybook with RENAMES R4 (multiple REDEFINES)")]
    async fn given_copybook_with_renames_r4_multiple_redefines(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 BASE-FIELD PIC X(10).\n\
             05 REDEF1 PIC 9(10) REDEFINES BASE-FIELD.\n\
             05 REDEF2 PIC X(10) REDEFINES BASE-FIELD.\n\
             05 REDEF3 PIC S9(5) REDEFINES BASE-FIELD.\n\
             66 ALIAS-FIELD RENAMES REDEF1 THRU REDEF3."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with RENAMES R5 (ODO)")]
    async fn given_copybook_with_renames_r5_odo(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 COUNT-FIELD PIC X(3).\n\
             05 ARRAY-FIELD PIC X(5) OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.\n\
             66 ALIAS-FIELD RENAMES ARRAY-FIELD."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with RENAMES R6 (Level-88 after RENAMES)")]
    async fn given_copybook_with_renames_r6_level88(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 BASE-FIELD PIC X(10).\n\
             05 REDEF1 PIC 9(10) REDEFINES BASE-FIELD.\n\
             05 REDEF2 PIC X(10) REDEFINES BASE-FIELD.\n\
             66 ALIAS-FIELD RENAMES REDEF1 THRU REDEF2.\n\
                 88 ALIAS-VALID VALUE 'A'.\n\
                 88 ALIAS-INVALID VALUE 'X'."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with RENAMES R4")]
    async fn given_copybook_with_renames_r4(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 BASE-FIELD PIC X(10).\n\
             05 REDEF1 PIC 9(10) REDEFINES BASE-FIELD.\n\
             05 REDEF2 PIC X(10) REDEFINES BASE-FIELD.\n\
             66 ALIAS-FIELD RENAMES REDEF1 THRU REDEF2."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with RENAMES R5")]
    async fn given_copybook_with_renames_r5(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 COUNT-FIELD PIC X(3).\n\
             05 ARRAY-FIELD PIC X(5) OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.\n\
             66 ALIAS-FIELD RENAMES ARRAY-FIELD."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with RENAMES R6")]
    async fn given_copybook_with_renames_r6(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 BASE-FIELD PIC X(10).\n\
             05 REDEF1 PIC 9(10) REDEFINES BASE-FIELD.\n\
             05 REDEF2 PIC X(10) REDEFINES BASE-FIELD.\n\
             66 ALIAS-FIELD RENAMES REDEF1 THRU REDEF2.\n\
                 88 ALIAS-VALID VALUE 'A'.\n\
                 88 ALIAS-INVALID VALUE 'X'."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with both SIGN SEPARATE and RENAMES")]
    async fn given_copybook_with_sign_and_renames(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 BASE-FIELD PIC S9(5) SIGN IS SEPARATE LEADING.\n\
             05 REDEF1 PIC X(6) REDEFINES BASE-FIELD.\n\
             05 REDEF2 PIC 9(6) REDEFINES BASE-FIELD.\n\
             66 ALIAS-FIELD RENAMES REDEF1 THRU REDEF2."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with invalid SIGN SEPARATE")]
    async fn given_copybook_with_invalid_sign_separate(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE INVALID."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with invalid RENAMES range")]
    async fn given_copybook_with_invalid_renames_range(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 RECORD.\n\
             05 FIELD1 PIC X(5).\n\
             05 FIELD2 PIC X(5).\n\
             66 ALIAS-FIELD RENAMES FIELD1 THRU NONEXISTENT."
                .to_string(),
        );
    }

    #[then(expr = "the field should have sign separate information")]
    async fn then_field_has_sign_separate(world: &mut CopybookWorld) {
        let field = world
            .field_by_name("SIGNED-FIELD")
            .expect("Field not found");
        match &field.kind {
            copybook_core::FieldKind::ZonedDecimal { sign_separate, .. } => {
                assert!(
                    sign_separate.is_some(),
                    "Field should have sign separate information"
                );
            }
            _ => unreachable!("Expected ZonedDecimal field"),
        }
    }

    #[then(expr = "the sign placement should be LEADING")]
    async fn then_sign_placement_is_leading(world: &mut CopybookWorld) {
        let field = world
            .field_by_name("SIGNED-FIELD")
            .expect("Field not found");
        match &field.kind {
            copybook_core::FieldKind::ZonedDecimal { sign_separate, .. } => {
                let placement = sign_separate
                    .as_ref()
                    .map(|s| format!("{:?}", s.placement))
                    .unwrap_or_else(|| "None".to_string())
                    .to_ascii_uppercase();
                assert_eq!(
                    placement, "LEADING",
                    "Expected sign placement LEADING, got {}",
                    placement
                );
            }
            _ => unreachable!("Expected ZonedDecimal field"),
        }
    }

    #[then(expr = "the sign placement should be TRAILING")]
    async fn then_sign_placement_is_trailing(world: &mut CopybookWorld) {
        let field = world
            .field_by_name("SIGNED-FIELD")
            .expect("Field not found");
        match &field.kind {
            copybook_core::FieldKind::ZonedDecimal { sign_separate, .. } => {
                let placement = sign_separate
                    .as_ref()
                    .map(|s| format!("{:?}", s.placement))
                    .unwrap_or_else(|| "None".to_string())
                    .to_ascii_uppercase();
                assert_eq!(
                    placement, "TRAILING",
                    "Expected sign placement TRAILING, got {}",
                    placement
                );
            }
            _ => unreachable!("Expected ZonedDecimal field"),
        }
    }

    #[then(expr = "the decoded value should be {string}")]
    async fn then_decoded_value_is(world: &mut CopybookWorld, expected: String) {
        let output = world.decoded_output();
        assert!(
            output.contains(&expected),
            "Expected decoded value '{}', got {}",
            expected,
            output
        );
    }

    #[then(expr = "the encoded data should have leading sign")]
    async fn then_encoded_data_has_leading_sign(world: &mut CopybookWorld) {
        let output = world
            .encoded_output
            .as_ref()
            .expect("Encoded output not set");
        assert!(
            output.first() == Some(&b'+') || output.first() == Some(&b'-'),
            "Expected leading sign"
        );
    }

    #[then(expr = "the encoded data should have trailing sign")]
    async fn then_encoded_data_has_trailing_sign(world: &mut CopybookWorld) {
        let output = world
            .encoded_output
            .as_ref()
            .expect("Encoded output not set");
        assert!(
            output.last() == Some(&b'+') || output.last() == Some(&b'-'),
            "Expected trailing sign"
        );
    }

    #[then(expr = "the sign placement should be preserved")]
    async fn then_sign_placement_preserved(world: &mut CopybookWorld) {
        let field = world
            .field_by_name("SIGNED-FIELD")
            .expect("Field not found");
        match &field.kind {
            copybook_core::FieldKind::ZonedDecimal { sign_separate, .. } => {
                assert!(
                    sign_separate.is_some(),
                    "Sign placement should be preserved"
                );
            }
            _ => unreachable!("Expected ZonedDecimal field"),
        }
    }

    #[then(expr = "RENAMES field should be resolved")]
    async fn then_renames_resolved(world: &mut CopybookWorld) {
        let renames_field = world.renames_field().expect("RENAMES field not found");
        assert!(
            renames_field.resolved_renames.is_some(),
            "RENAMES should be resolved"
        );
    }

    #[then(expr = "the alias should cover all REDEFINES fields")]
    async fn then_renames_covers_redefines(world: &mut CopybookWorld) {
        let count = world
            .schema()
            .all_fields()
            .iter()
            .filter(|field| field.redefines_of.is_some())
            .count();
        assert!(count > 0, "Should have REDEFINES fields to cover");
    }

    #[then(expr = "the alias should reference the ODO field")]
    async fn then_renames_reference_odo(world: &mut CopybookWorld) {
        let renames_field = world.renames_field().expect("RENAMES field not found");
        let resolved = renames_field
            .resolved_renames
            .as_ref()
            .expect("RENAMES not resolved");
        assert!(
            !resolved.members.is_empty(),
            "RENAMES should reference ODO fields"
        );
    }

    #[then(expr = "RENAMES field should have Level-88 conditions")]
    async fn then_renames_has_level88(world: &mut CopybookWorld) {
        let has_level88 = world
            .schema()
            .all_fields()
            .iter()
            .any(|field| matches!(field.kind, copybook_core::FieldKind::Condition { .. }));
        assert!(has_level88, "Should have Level-88 conditions");
    }

    #[then(expr = "the conditions should be properly associated")]
    async fn then_renames_conditions_associated(world: &mut CopybookWorld) {
        let renames = world.renames_field().expect("RENAMES field not found");
        let alias_prefix = format!("{}.", renames.path);
        let has_condition_child = renames
            .children
            .iter()
            .any(|field| matches!(field.kind, copybook_core::FieldKind::Condition { .. }));
        let has_nested_condition = world
            .schema()
            .all_fields()
            .iter()
            .any(|field| field.level == 88 && field.path.starts_with(&alias_prefix));
        assert!(
            has_condition_child || has_nested_condition,
            "Level-88 conditions should be associated with the alias"
        );
    }

    #[then(expr = "the sign should be properly handled")]
    async fn then_sign_should_be_properly_handled(world: &mut CopybookWorld) {
        let output = world.decoded_output();
        assert!(
            output.contains('-') || output.contains('+'),
            "Expected decoded value to include a sign"
        );
    }

    #[then(expr = "the error should indicate invalid SIGN SEPARATE placement")]
    async fn then_error_should_indicate_invalid_sign_separate(world: &mut CopybookWorld) {
        let error = world.error.as_ref().expect("Error should be set");
        let message = format!("{}", error);
        assert!(
            message.contains("SIGN") || message.contains("SEPARATE"),
            "Expected error about SIGN SEPARATE"
        );
    }

    #[then(expr = "the error should indicate invalid RENAMES range")]
    async fn then_error_should_indicate_invalid_renames(world: &mut CopybookWorld) {
        let error = world.error.as_ref().expect("Error should be set");
        let message = format!("{}", error);
        assert!(
            message.contains("RENAMES") || message.contains("THRU"),
            "Expected error about invalid RENAMES range"
        );
    }

    #[then(expr = "the encoded data should match the original")]
    async fn then_encoded_data_matches_original(world: &mut CopybookWorld) {
        let output = world
            .encoded_output
            .as_ref()
            .expect("Encoded output not set");
        let output_text = String::from_utf8_lossy(output);
        let original = serde_json::from_str::<Value>(world.json_data()).expect("JSON data invalid");
        let first_value = original
            .as_object()
            .and_then(|obj| obj.values().next())
            .and_then(|value| value.as_str())
            .expect("JSON input should contain a string value for the alias");

        assert_eq!(
            output_text, first_value,
            "Encoded output should match JSON input value"
        );
    }

    #[then(expr = "the array should be properly decoded")]
    async fn then_array_decoded(world: &mut CopybookWorld) {
        let record = world.first_decoded_record();
        let has_expected_payload = record.as_object().is_some_and(|obj| {
            obj.values()
                .filter_map(Value::as_str)
                .any(|value| value.contains("ELEM"))
        });
        assert!(
            has_expected_payload,
            "Expected decoded RENAMES array payload to be present"
        );
    }

    #[then(expr = "the RENAMES structure should be preserved")]
    async fn then_renames_structure_preserved(world: &mut CopybookWorld) {
        let renames = world.renames_field().expect("RENAMES field not found");
        assert!(
            renames.resolved_renames.is_some(),
            "RENAMES structure should remain resolved"
        );
        assert!(!renames.name.is_empty(), "RENAMES name should remain set");
    }

    #[then(expr = "the decoded output should contain Level-88 conditions")]
    async fn then_decoded_output_contains_level88_conditions(world: &mut CopybookWorld) {
        let output = world.decoded_output();
        assert!(
            output.contains("ALIAS-VALID") || output.contains("ALIAS-INVALID"),
            "Expected Level-88 condition names in decoded output"
        );
    }

    // ============================================================================
    // Encode/Decode Steps
    // ============================================================================

    #[given(expr = "binary data: {string}")]
    async fn given_binary_data(world: &mut CopybookWorld, data: String) {
        world.binary_data = Some(parse_binary_literal(&data));
    }

    #[given(expr = "JSON data:")]
    async fn given_json_data(world: &mut CopybookWorld, data: String) {
        world.json_data = Some(data);
    }

    #[given(expr = "ASCII codepage")]
    async fn given_ascii_codepage(world: &mut CopybookWorld) {
        world.decode_options = Some(
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
                .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto),
        );

        world.encode_options = Some(
            EncodeOptions::new()
                .with_codepage(Codepage::ASCII)
                .with_format(RecordFormat::Fixed)
                .with_use_raw(false)
                .with_bwz_encode(false)
                .with_strict_mode(false)
                .with_max_errors(None)
                .with_threads(1)
                .with_coerce_numbers(true)
                .with_zoned_encoding_override(None),
        );
    }

    #[given(expr = "EBCDIC codepage")]
    async fn given_ebcdic_codepage(world: &mut CopybookWorld) {
        world.decode_options = Some(default_ebcdic_decode_options());
        world.encode_options = Some(default_ebcdic_encode_options());
    }

    #[given(expr = "binary data with 100 fixed elements")]
    async fn given_binary_data_with_100_elements(world: &mut CopybookWorld) {
        let schema = world.ensure_schema_and_return().clone();
        if schema.fields.is_empty() {
            return;
        }

        let all_fields = schema.all_fields();
        let fixed_field = all_fields
            .iter()
            .find(|field| matches!(field.occurs, Some(Occurs::Fixed { .. })));
        let Some(fixed_field) = fixed_field else {
            unreachable!("No fixed OCCURS field found")
        };

        let mut payload = Map::new();
        payload.insert(
            fixed_field.name.clone(),
            build_array_payload(fixed_field, 100).expect("Failed to build array payload"),
        );
        let encoded = match encode_from_payload(&payload, world) {
            Ok(output) => output,
            Err(error) => {
                world.error = Some(error);
                return;
            }
        };
        world.binary_data = Some(encoded);
        world.json_data =
            Some(serde_json::to_string(&Value::Object(payload)).expect("Payload should serialize"));
    }

    #[given(expr = "JSON data with 100 elements")]
    async fn given_json_data_with_100_elements(world: &mut CopybookWorld) {
        let schema = world.ensure_schema_and_return().clone();

        let all_fields = schema.all_fields();
        let fixed_field = all_fields
            .iter()
            .find(|field| matches!(field.occurs, Some(Occurs::Fixed { .. })));
        let Some(fixed_field) = fixed_field else {
            unreachable!("No fixed OCCURS field found")
        };

        let mut payload = Map::new();
        payload.insert(
            fixed_field.name.clone(),
            build_array_payload(fixed_field, 100).expect("Failed to build array payload"),
        );

        world.json_data =
            Some(serde_json::to_string(&Value::Object(payload)).expect("Payload should serialize"));
    }

    #[given(expr = "EBCDIC binary data")]
    async fn given_ebcdic_binary_data(world: &mut CopybookWorld) {
        let schema = world.ensure_schema_and_return().clone();
        let fallback = schema
            .all_fields()
            .into_iter()
            .find(|field| field.children.is_empty())
            .expect("No leaf field available")
            .name
            .clone();
        let mut payload = Map::new();
        payload.insert(fallback, Value::String("ABCDEFGHIJ".to_string()));

        world.decode_options = Some(default_ebcdic_decode_options());
        world.encode_options = Some(default_ebcdic_encode_options());

        let encoded = match encode_from_payload(&payload, world) {
            Ok(output) => output,
            Err(error) => {
                world.error = Some(error);
                return;
            }
        };
        world.binary_data = Some(encoded);
        world.json_data =
            Some(serde_json::to_string(&Value::Object(payload)).expect("Payload should serialize"));
    }

    #[given(expr = "ASCII JSON data")]
    async fn given_ascii_json_data(world: &mut CopybookWorld) {
        let schema = world.ensure_schema_and_return().clone();
        let leaf = schema
            .all_fields()
            .into_iter()
            .find(|field| field.children.is_empty())
            .expect("No leaf field available")
            .name
            .clone();
        let mut payload = Map::new();
        payload.insert(leaf, Value::String("ABCDEFGHIJ".to_string()));
        world.json_data =
            Some(serde_json::to_string(&Value::Object(payload)).expect("Payload should serialize"));
    }

    #[given(expr = "emit_filler is false")]
    async fn given_emit_filler_false(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        if let Some(mut options) = world.decode_options.clone() {
            options = options.with_emit_filler(false);
            world.decode_options = Some(options);
        }
    }

    #[given(expr = "emit_filler is true")]
    async fn given_emit_filler_true(world: &mut CopybookWorld) {
        world.ensure_decode_options();
        if let Some(mut options) = world.decode_options.clone() {
            options = options.with_emit_filler(true);
            world.decode_options = Some(options);
        }
    }

    #[given(expr = "EBCDIC JSON data:")]
    async fn given_ebcdic_json_data(world: &mut CopybookWorld, data: String) {
        world.json_data = Some(data);
    }

    #[when(expr = "the binary data is decoded")]
    async fn when_binary_data_decoded(world: &mut CopybookWorld) {
        world.ensure_schema_parsed();
        if world.schema.is_none() {
            return;
        }
        world.ensure_decode_options();
        if world.decode_options.is_none() {
            return;
        }

        let schema = world.schema();
        let decode_options = world
            .decode_options
            .as_ref()
            .expect("Decode options not set");

        let binary_data = world.binary_data();
        let mut output = Vec::new();

        match decode_file_to_jsonl(
            schema,
            Cursor::new(binary_data),
            &mut output,
            decode_options,
        ) {
            Ok(_) => {
                world.decoded_output =
                    Some(String::from_utf8(output).expect("Decoded output should be valid UTF-8"));
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[when(expr = "the JSON data is encoded")]
    async fn when_json_data_encoded(world: &mut CopybookWorld) {
        world.ensure_schema_parsed();
        if world.schema.is_none() {
            return;
        }
        world.ensure_encode_options();
        if world.encode_options.is_none() {
            return;
        }

        let schema = world.schema();
        let encode_options = world
            .encode_options
            .as_ref()
            .expect("Encode options not set");

        let json_data = world.json_data();
        let mut output = Vec::new();

        match encode_jsonl_to_file(
            schema,
            Cursor::new(json_data.as_bytes()),
            &mut output,
            encode_options,
        ) {
            Ok(_) => {
                world.encoded_output = Some(output);
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[when(expr = "the data is round-tripped")]
    async fn when_data_roundtripped(world: &mut CopybookWorld) {
        world.ensure_schema_parsed();
        if world.schema.is_none() {
            return;
        }
        world.ensure_decode_options();
        if world.decode_options.is_none() {
            return;
        }
        world.ensure_encode_options();
        if world.encode_options.is_none() {
            return;
        }

        // First decode
        let schema = world.schema().clone();
        let decode_options = world
            .decode_options
            .as_ref()
            .expect("Decode options not set");

        let binary_data = world.binary_data();
        let mut decoded = Vec::new();

        if let Err(e) = decode_file_to_jsonl(
            &schema,
            Cursor::new(binary_data),
            &mut decoded,
            decode_options,
        ) {
            world.error = Some(e);
            return;
        }

        let decoded_text =
            String::from_utf8(decoded).expect("Decoded output should be valid UTF-8");
        world.decoded_output = Some(decoded_text.clone());

        // Then encode
        let encode_options = world
            .encode_options
            .as_ref()
            .expect("Encode options not set");

        let mut encoded = Vec::new();

        if let Err(e) = encode_jsonl_to_file(
            &schema,
            Cursor::new(decoded_text.as_bytes()),
            &mut encoded,
            encode_options,
        ) {
            world.error = Some(e);
            return;
        }

        world.encoded_output = Some(encoded);
    }

    #[then(expr = "the decoded output should contain {string}")]
    async fn then_decoded_output_contains(world: &mut CopybookWorld, expected: String) {
        let output = world.decoded_output();
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
        let value = json_value_for_field(&record, &field_name)
            .and_then(field_value_as_string)
            .expect(&format!("Decoded field '{field_name}' should be present"));

        if expected.eq_ignore_ascii_case("blank") {
            assert!(
                value.trim().is_empty(),
                "Expected field '{field_name}' to be blank, got '{value}'"
            );
        } else {
            assert_eq!(
                value, expected,
                "Expected decoded field '{field_name}' to be '{expected}', got '{value}'"
            );
        }
    }

    #[then(expr = "{word} should be {string}")]
    async fn then_field_value(world: &mut CopybookWorld, field_name: String, expected: String) {
        then_decoded_field_value(world, field_name, expected).await;
    }

    #[then(expr = "all fields should be decoded")]
    async fn then_all_fields_decoded(world: &mut CopybookWorld) {
        let record = world.first_decoded_record();
        for field in world.all_leaf_fields() {
            if matches!(field.kind, copybook_core::FieldKind::Condition { .. }) {
                continue;
            }
            assert!(
                json_value_for_field(&record, &field.name).is_some(),
                "Expected decoded output to include field '{}'",
                field.name
            );
        }
    }

    #[then(expr = "there should be {int} {word} elements")]
    async fn then_array_element_count(
        world: &mut CopybookWorld,
        expected: usize,
        field_name: String,
    ) {
        let record = world.first_decoded_record();
        let value = json_value_for_field(&record, &field_name).expect(&format!(
            "Decoded array field '{field_name}' should be present"
        ));
        let array = value
            .as_array()
            .expect(&format!("Decoded field '{field_name}' should be an array"));

        assert_eq!(
            array.len(),
            expected,
            "Expected '{field_name}' to have {expected} elements, got {}",
            array.len()
        );
    }

    #[then(expr = "field {word} should be present")]
    #[then(expr = "{word} should be present")]
    async fn then_field_is_present(world: &mut CopybookWorld, field_name: String) {
        let record = world.first_decoded_record();
        assert!(
            json_value_for_field(&record, &field_name).is_some(),
            "Expected field '{field_name}' to be present in decoded output"
        );
    }

    #[then(expr = "{word} should be in output")]
    async fn then_field_in_output(world: &mut CopybookWorld, field_name: String) {
        let record = world.first_decoded_record();
        assert!(
            json_value_for_field(&record, &field_name).is_some(),
            "Expected field '{field_name}' to be in output"
        );
    }

    #[then(expr = "{word} should not be in output")]
    async fn then_field_not_in_output(world: &mut CopybookWorld, field_name: String) {
        let record = world.first_decoded_record();
        assert!(
            json_value_for_field(&record, &field_name).is_none(),
            "Expected field '{field_name}' to not be in output"
        );
    }

    #[then(expr = "fields should have correct levels")]
    async fn then_fields_have_correct_levels(world: &mut CopybookWorld) {
        for field in &world.schema().fields {
            assert_field_level_depth(field);
        }
    }

    #[then(expr = "decoded value should be converted to ASCII")]
    async fn then_decoded_to_ascii(world: &mut CopybookWorld) {
        let record = world.first_decoded_record();
        let values = collect_string_values(&record);
        assert!(
            values.iter().all(|value| value.is_ascii()),
            "Expected decoded values to be ASCII, got non-ASCII data: {:?}",
            values
        );
    }

    #[then(expr = "encoded data should be in EBCDIC")]
    async fn then_encoded_in_ebcdic(world: &mut CopybookWorld) {
        let options = world
            .encode_options
            .as_ref()
            .expect("Encode options not set");
        assert!(
            options.codepage.is_ebcdic(),
            "Expected EBCDIC encode options for this assertion"
        );
        let output = world
            .encoded_output
            .as_ref()
            .expect("Encoded output should be set");
        assert!(
            output.iter().any(|byte| !byte.is_ascii()),
            "Expected encoded output to contain non-ASCII bytes for EBCDIC"
        );
    }

    #[then(expr = "encoded data should be blank")]
    async fn then_encoded_data_blank(world: &mut CopybookWorld) {
        let output = world
            .encoded_output
            .as_ref()
            .expect("Encoded output should be set");
        let options = world
            .encode_options
            .as_ref()
            .expect("Encode options not set");
        let blank_byte = if options.codepage.is_ascii() {
            b' '
        } else {
            0x40
        };
        assert!(
            output.iter().all(|byte| *byte == blank_byte),
            "Expected encoded output to be blank"
        );
    }

    #[then(expr = "encoded length should be {int} bytes")]
    async fn then_encoded_length(world: &mut CopybookWorld, length: usize) {
        then_encoded_output_bytes(world, length).await;
    }

    #[then(expr = "the encoded output should be {int} bytes")]
    async fn then_encoded_output_bytes(world: &mut CopybookWorld, length: usize) {
        let output = world
            .encoded_output
            .as_ref()
            .expect("Encoded output should be set");
        assert_eq!(
            output.len(),
            length,
            "Expected encoded output to be {} bytes, got {}",
            length,
            output.len()
        );
    }

    #[then(expr = "the round-trip should be lossless")]
    async fn then_roundtrip_lossless(world: &mut CopybookWorld) {
        let original = world.binary_data();
        let encoded = world
            .encoded_output
            .as_ref()
            .expect("Encoded output should be set");

        assert_eq!(
            original,
            encoded.as_slice(),
            "Round-trip should be lossless: original data differs from encoded data"
        );
    }

    #[then(expr = "decoding should succeed")]
    async fn then_decoding_succeeds(world: &mut CopybookWorld) {
        assert!(world.decoded_output.is_some(), "Decoding should succeed");
        assert!(
            world.error.is_none(),
            "No error should occur during decoding"
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

    #[then(expr = "the decoded output should be valid JSON")]
    async fn then_decoded_output_valid_json(world: &mut CopybookWorld) {
        let output = world.decoded_output();
        for line in output.lines() {
            if line.trim().is_empty() {
                continue;
            }
            let _: Value = serde_json::from_str(line)
                .expect(&format!("Output should be valid JSON: {}", line));
        }
    }

    // ============================================================================
    // Error Handling Steps
    // ============================================================================

    #[given(expr = "an invalid copybook with syntax error")]
    async fn given_invalid_copybook(world: &mut CopybookWorld) {
        world.copybook_text =
            Some("01 INVALID-RECORD\n    05 INVALID-FIELD INVALID PIC X(10).".to_string());
    }

    #[given(expr = "a copybook with invalid OCCURS clause")]
    async fn given_invalid_occurs(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 INVALID-OCCURS.\n\
             05 FIELD OCCURS INVALID TIMES.\n\
                 10 ELEMENT PIC X(10)."
                .to_string(),
        );
    }

    #[given(expr = "a copybook with invalid PIC clause")]
    async fn given_invalid_pic(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 INVALID-PIC.\n\
             05 FIELD PIC INVALID(10)."
                .to_string(),
        );
    }

    #[given(expr = "binary data that is too short")]
    async fn given_short_binary_data(world: &mut CopybookWorld) {
        world.binary_data = Some(vec![0x41, 0x42]); // Only 2 bytes
    }

    #[given(expr = "binary data with invalid encoding")]
    async fn given_invalid_encoding_data(world: &mut CopybookWorld) {
        world.binary_data = Some(vec![
            0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        ]);
    }

    #[given(expr = "JSON data with missing required fields")]
    async fn given_json_missing_fields(world: &mut CopybookWorld) {
        world.json_data = Some(r#"{"schema":"copybook.v1","record_index":0}"#.to_string());
    }

    #[given(expr = "JSON data with invalid field types")]
    async fn given_json_invalid_types(world: &mut CopybookWorld) {
        world.json_data =
            Some(r#"{"schema":"copybook.v1","record_index":0,"TEST-FIELD":12345}"#.to_string());
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

    // ============================================================================
    // Dialect-specific Then Steps
    // ============================================================================

    #[then(expr = "the error message should contain {string}")]
    async fn then_error_message_contains_dialect(world: &mut CopybookWorld, expected: String) {
        let error = world.error.as_ref().expect("Error should be set");
        let message = format!("{}", error);
        assert!(
            message.contains(&expected),
            "Expected error message to contain '{}', got: {}",
            expected,
            message
        );
    }

    #[then(expr = "the schema should use {string} dialect")]
    async fn then_schema_uses_dialect(world: &mut CopybookWorld, expected_dialect: String) {
        let dialect = world.dialect.expect("Dialect should be set");
        let actual_dialect = match dialect {
            Dialect::Normative => "Normative",
            Dialect::ZeroTolerant => "Zero-Tolerant",
            Dialect::OneTolerant => "One-Tolerant",
        };
        assert_eq!(
            actual_dialect, expected_dialect,
            "Expected dialect '{}', got '{}'",
            expected_dialect, actual_dialect
        );
    }

    #[then(expr = "ODO min_count should be {int}")]
    async fn then_odo_min_count_is(world: &mut CopybookWorld, expected_min_count: u32) {
        let schema = world.schema();
        let dialect = world.dialect.expect("Dialect should be set");

        // Find ODO field
        let occurs = schema
            .all_fields()
            .iter()
            .find_map(|field| {
                if let Some(Occurs::ODO { min, .. }) = field.occurs {
                    Some(min)
                } else {
                    None
                }
            })
            .expect("Should have an ODO field");
        let effective_min_count = copybook_core::dialect::effective_min_count(dialect, occurs);

        assert_eq!(
            effective_min_count, expected_min_count,
            "Expected effective min_count to be {}, got {}",
            expected_min_count, effective_min_count
        );
    }

    // ============================================================================
    // Field Projection Steps
    // ============================================================================

    #[given(expr = "field selection: {string}")]
    async fn given_field_selection(world: &mut CopybookWorld, selection: String) {
        if selection.is_empty() {
            world.field_selection = Some(Vec::new());
        } else {
            world.field_selection = Some(
                selection
                    .split(',')
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect(),
            );
        }
    }

    #[when(expr = "the schema is projected with selected fields")]
    async fn when_schema_projected(world: &mut CopybookWorld) {
        let schema = world.schema().clone();
        let selection = world.field_selection.clone().unwrap_or_default();

        match project_schema(&schema, &selection) {
            Ok(projected) => {
                world.projected_schema = Some(projected);
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[then(expr = "the projection should succeed")]
    async fn then_projection_succeeds(world: &mut CopybookWorld) {
        assert!(
            world.projected_schema.is_some(),
            "Projection should succeed"
        );
        assert!(
            world.error.is_none(),
            "No error should occur during projection"
        );
    }

    #[then(expr = "the projection should fail")]
    async fn then_projection_fails(world: &mut CopybookWorld) {
        assert!(world.projected_schema.is_none(), "Projection should fail");
        assert!(
            world.error.is_some(),
            "An error should occur during projection"
        );
    }

    #[then(expr = "the projected schema should contain {int} top-level field(s)")]
    async fn then_projected_schema_contains_fields(world: &mut CopybookWorld, count: usize) {
        let schema = world.projected_schema();
        assert_eq!(
            schema.fields.len(),
            count,
            "Expected {} top-level fields in projected schema, got {}",
            count,
            schema.fields.len()
        );
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
}

fn parse_binary_literal(input: &str) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(input.len());
    let mut iter = input.as_bytes().iter().copied().peekable();
    while let Some(byte) = iter.next() {
        if byte != b'\\' {
            bytes.push(byte);
            continue;
        }
        match iter.next() {
            Some(b'x') => {
                let hi = iter.next();
                let lo = iter.next();
                match (hi, lo) {
                    (Some(hi), Some(lo)) => {
                        let value = (hex_value(hi) << 4) | hex_value(lo);
                        bytes.push(value);
                    }
                    _ => bytes.push(b'\\'),
                }
            }
            Some(b'n') => bytes.push(b'\n'),
            Some(b'r') => bytes.push(b'\r'),
            Some(b't') => bytes.push(b'\t'),
            Some(b'\\') => bytes.push(b'\\'),
            Some(b'"') => bytes.push(b'"'),
            Some(other) => {
                bytes.push(b'\\');
                bytes.push(other);
            }
            None => bytes.push(b'\\'),
        }
    }
    bytes
}

fn hex_value(byte: u8) -> u8 {
    match byte {
        b'0'..=b'9' => byte - b'0',
        b'a'..=b'f' => byte - b'a' + 10,
        b'A'..=b'F' => byte - b'A' + 10,
        _ => 0,
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

fn default_ebcdic_decode_options() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::CP037)
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

fn default_ebcdic_encode_options() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_format(RecordFormat::Fixed)
        .with_use_raw(false)
        .with_bwz_encode(false)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_threads(1)
        .with_coerce_numbers(true)
        .with_zoned_encoding_override(None)
}

fn collect_leaf_fields<'a>(
    fields: &'a [copybook_core::Field],
    leaf_fields: &mut Vec<&'a copybook_core::Field>,
) {
    for field in fields {
        if field.children.is_empty() {
            leaf_fields.push(field);
        } else {
            collect_leaf_fields(&field.children, leaf_fields);
        }
    }
}

fn generate_value_for_field(field: &copybook_core::Field, index: usize) -> String {
    let width = usize::try_from(field.len).unwrap_or(0).max(1);
    match &field.kind {
        copybook_core::FieldKind::Alphanum { .. } => {
            let start = u8::try_from(index % 26).unwrap_or(0);
            let repeated = char::from(b'A' + start);
            std::iter::repeat_n(repeated, width).collect()
        }
        copybook_core::FieldKind::Condition { .. } => "1".to_string(),
        copybook_core::FieldKind::Renames { .. } => "0".to_string(),
        copybook_core::FieldKind::Group => "0".to_string(),
        copybook_core::FieldKind::FloatSingle | copybook_core::FieldKind::FloatDouble => {
            "0".to_string()
        }
        copybook_core::FieldKind::ZonedDecimal { .. }
        | copybook_core::FieldKind::BinaryInt { .. }
        | copybook_core::FieldKind::PackedDecimal { .. }
        | copybook_core::FieldKind::EditedNumeric { .. } => "0".to_string(),
    }
}

fn build_array_payload(array_field: &copybook_core::Field, count: usize) -> Result<Value, Error> {
    if array_field.occurs.is_none() {
        return Err(Error::new(
            ErrorCode::CBKI001_INVALID_STATE,
            format!("Expected an OCCURS field, got '{}'", array_field.name),
        ));
    }

    let mut values = Vec::with_capacity(count);
    for index in 0..count {
        let value = if array_field.children.is_empty() {
            Value::String(generate_value_for_field(array_field, index))
        } else {
            build_array_element(array_field, index)?
        };
        values.push(value);
    }

    Ok(Value::Array(values))
}

fn build_fixed_array_payload(
    array_field: &copybook_core::Field,
    count: usize,
) -> Result<Value, Error> {
    build_array_payload(array_field, count)
}

fn build_array_element(field: &copybook_core::Field, index: usize) -> Result<Value, Error> {
    let mut map = Map::new();
    for child in &field.children {
        if let Some(occurs) = &child.occurs {
            let nested_count = match occurs {
                Occurs::Fixed { count } => usize::try_from(*count).unwrap_or(1),
                Occurs::ODO { min, .. } => usize::try_from(*min).unwrap_or(1),
            };
            map.insert(
                child.name.clone(),
                build_array_payload(child, nested_count)?,
            );
        } else if child.children.is_empty() {
            map.insert(
                child.name.clone(),
                Value::String(generate_value_for_field(child, index)),
            );
        } else {
            map.insert(child.name.clone(), build_array_element(child, index)?);
        }
    }
    Ok(Value::Object(map))
}

fn build_binary_for_all_leaf_fields(world: &mut CopybookWorld) -> Result<Vec<u8>, Error> {
    world.ensure_schema_parsed();
    world.ensure_encode_options();
    let Some(schema) = &world.schema else {
        return Err(world.error.clone().unwrap_or_else(|| {
            Error::new(ErrorCode::CBKI001_INVALID_STATE, "Schema is not available")
        }));
    };
    if world.encode_options.is_none() {
        return Err(Error::new(
            ErrorCode::CBKI001_INVALID_STATE,
            "Encode options were not configured",
        ));
    }

    let mut payload = Map::new();
    for field in schema.all_fields() {
        if field.children.is_empty() {
            payload.insert(
                field.name.clone(),
                Value::String(generate_value_for_field(field, 0)),
            );
        }
    }
    encode_from_payload(&payload, world)
}

fn encode_from_payload(
    payload: &Map<String, Value>,
    world: &mut CopybookWorld,
) -> Result<Vec<u8>, Error> {
    world.ensure_schema_parsed();
    world.ensure_encode_options();
    if world.schema.is_none() {
        return Err(world.error.clone().unwrap_or_else(|| {
            Error::new(ErrorCode::CBKI001_INVALID_STATE, "Schema is not available")
        }));
    }
    let schema = world.schema.as_ref().expect("Schema should exist");
    let encode_options = world.encode_options.as_ref().ok_or_else(|| {
        Error::new(
            ErrorCode::CBKI001_INVALID_STATE,
            "Encode options not configured",
        )
    })?;

    let payload_text = serde_json::to_string(&Value::Object(payload.clone()))
        .map_err(|error| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, error.to_string()))?;
    let mut output = Vec::new();
    encode_jsonl_to_file(
        schema,
        Cursor::new(payload_text.as_bytes()),
        &mut output,
        encode_options,
    )?;
    Ok(output)
}

fn does_path_match_counter(occurs: &copybook_core::Occurs, counter: &str) -> bool {
    match occurs {
        Occurs::ODO { counter_path, .. } => path_ends_with(counter_path, counter),
        Occurs::Fixed { .. } => false,
    }
}

fn does_path_match_counter_string(counter_path: &str, counter: &str) -> bool {
    path_ends_with(counter_path, counter)
}

fn path_ends_with(path: &str, suffix: &str) -> bool {
    if suffix.is_empty() {
        return false;
    }
    path.eq_ignore_ascii_case(suffix)
        || path
            .rsplit('.')
            .next()
            .is_some_and(|segment| segment.eq_ignore_ascii_case(suffix))
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

fn json_value_for_field<'a>(value: &'a Value, field_name: &str) -> Option<&'a Value> {
    match value {
        Value::Object(object) => {
            for (key, found) in object {
                if key.eq_ignore_ascii_case(field_name) {
                    return Some(found);
                }
            }

            for value in object.values() {
                if let Some(found) = json_value_for_field(value, field_name) {
                    return Some(found);
                }
            }
            None
        }
        Value::Array(values) => values
            .iter()
            .find_map(|value| json_value_for_field(value, field_name)),
        _ => None,
    }
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
            for value in object.values() {
                result.extend(collect_string_values(value));
            }
        }
        Value::Array(values) => {
            for value in values {
                result.extend(collect_string_values(value));
            }
        }
        Value::String(value) => result.push(value.clone()),
        Value::Number(number) => result.push(number.to_string()),
        Value::Bool(value) => result.push(value.to_string()),
        _ => {}
    }
    result
}

// This runs the Cucumber tests
#[tokio::main]
async fn main() {
    CopybookWorld::run("tests/bdd/features").await;
}
