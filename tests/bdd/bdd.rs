//! BDD tests for copybook-rs using Cucumber/Gherkin syntax
//!
//! This module provides Behavior Driven Development tests that describe
//! the expected behavior of the copybook-rs library in human-readable
//! Gherkin syntax.

use cucumber::{given, then, when, World, WorldInit};
use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions, Error, ErrorCode};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    decode_record, encode_record, decode_file_to_jsonl, encode_jsonl_to_file,
};
use serde_json::Value;
use std::io::Cursor;

/// BDD World struct to maintain test state across steps
#[derive(Debug, Default, World)]
pub struct CopybookWorld {
    /// The copybook text being parsed
    copybook_text: Option<String>,
    /// The parsed schema
    schema: Option<copybook_core::Schema>,
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
}

impl CopybookWorld {
    /// Get the schema or panic if not set
    fn schema(&self) -> &copybook_core::Schema {
        self.schema.as_ref().expect("Schema not set")
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
        self.decoded_output.as_deref().expect("Decoded output not set")
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
             05 ZONED-DECIMAL PIC S9(5)V99 DISPLAY.".to_string(),
        );
    }

    #[given(expr = "a copybook with OCCURS clause")]
    async fn given_copybook_with_occurs(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 ARRAY-RECORD.\n\
             05 ARRAY-FIELD OCCURS 5 TIMES.\n\
                 10 ELEMENT PIC X(10).".to_string(),
        );
    }

    #[given(expr = "a copybook with ODO (OCCURS DEPENDING ON)")]
    async fn given_copybook_with_odo(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 ODO-RECORD.\n\
             05 COUNT-FIELD PIC 9(3).\n\
             05 DYNAMIC-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON COUNT-FIELD.\n\
                 10 ELEMENT PIC X(10).".to_string(),
        );
    }

    #[given(expr = "a copybook with REDEFINES clause")]
    async fn given_copybook_with_redefines(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 REDEFINES-RECORD.\n\
             05 ORIGINAL-FIELD PIC X(20).\n\
             05 ALTERNATIVE-FIELD REDEFINES ORIGINAL-FIELD.\n\
                 10 PART-1 PIC 9(5).\n\
                 10 PART-2 PIC X(15).".to_string(),
        );
    }

    #[given(expr = "a copybook with Level-88 condition values")]
    async fn given_copybook_with_level88(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 STATUS-RECORD.\n\
             05 STATUS-CODE PIC X(1).\n\
                 88 STATUS-ACTIVE VALUE 'A'.\n\
                 88 STATUS-INACTIVE VALUE 'I'.\n\
                 88 STATUS-PENDING VALUE 'P'.".to_string(),
        );
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

    #[when(expr = "the copybook is parsed")]
    async fn when_copybook_is_parsed(world: &mut CopybookWorld) {
        let copybook_text = world.copybook_text.as_ref().expect("Copybook text not set");
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
    async fn then_schema_successfully_parsed(world: &mut CopybookWorld) {
        assert!(world.schema.is_some(), "Schema should be parsed successfully");
        assert!(world.error.is_none(), "No error should occur");
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
    async fn then_field_has_type(world: &mut CopybookWorld, field_name: String, expected_type: String) {
        let schema = world.schema();
        let field = schema.find_field(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        let actual_type = match &field.kind {
            copybook_core::FieldKind::Alphanumeric => "alphanumeric",
            copybook_core::FieldKind::Numeric { .. } => "numeric",
            copybook_core::FieldKind::Packed { .. } => "packed",
            copybook_core::FieldKind::Binary { .. } => "binary",
            copybook_core::FieldKind::Zoned { .. } => "zoned",
            copybook_core::FieldKind::Group => "group",
            copybook_core::FieldKind::Filler => "filler",
            copybook_core::FieldKind::Occurs { .. } => "occurs",
            copybook_core::FieldKind::Renames { .. } => "renames",
            copybook_core::FieldKind::Condition { .. } => "condition",
            copybook_core::FieldKind::Edited { .. } => "edited",
        };

        assert_eq!(
            actual_type, expected_type,
            "Expected field '{}' to have type '{}', got '{}'",
            field_name, expected_type, actual_type
        );
    }

    #[then(expr = "the field {string} should have offset {int}")]
    async fn then_field_has_offset(world: &mut CopybookWorld, field_name: String, offset: usize) {
        let schema = world.schema();
        let field = schema.find_field(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert_eq!(
            field.offset, offset,
            "Expected field '{}' to have offset {}, got {}",
            field_name, offset, field.offset
        );
    }

    #[then(expr = "the field {string} should have length {int}")]
    async fn then_field_has_length(world: &mut CopybookWorld, field_name: String, length: usize) {
        let schema = world.schema();
        let field = schema.find_field(&field_name)
            .expect(&format!("Field '{}' not found", field_name));

        assert_eq!(
            field.len, length,
            "Expected field '{}' to have length {}, got {}",
            field_name, length, field.len
        );
    }

    #[then(expr = "the schema should have fingerprint")]
    async fn then_schema_has_fingerprint(world: &mut CopybookWorld) {
        let schema = world.schema();
        assert!(!schema.fingerprint.is_empty(), "Schema should have a fingerprint");
    }

    #[then(expr = "parsing should fail with error code {string}")]
    async fn then_parsing_fails_with_error(world: &mut CopybookWorld, error_code: String) {
        assert!(world.error.is_some(), "Parsing should have failed");
        let error = world.error.as_ref().expect("Error should be set");
        let actual_code = format!("{:?}", error.code);
        assert!(
            actual_code.contains(&error_code),
            "Expected error code containing '{}', got '{}'",
            error_code, actual_code
        );
    }

    // ============================================================================
    // Encode/Decode Steps
    // ============================================================================

    #[given(expr = "binary data: {string}")]
    async fn given_binary_data(world: &mut CopybookWorld, data: String) {
        world.binary_data = Some(data.into_bytes());
    }

    #[given(expr = "JSON data:")]
    async fn given_json_data(world: &mut CopybookWorld, data: String) {
        world.json_data = Some(data);
    }

    #[given(expr = "ASCII codepage")]
    async fn given_ascii_codepage(world: &mut CopybookWorld) {
        world.decode_options = Some(DecodeOptions::new()
            .with_codepage(Codepage::ASCII)
            .with_format(RecordFormat::Fixed)
            .with_json_number_mode(JsonNumberMode::Lossless)
            .with_emit_filler(false)
            .with_emit_meta(false)
            .with_emit_raw(RawMode::Off)
            .with_strict_mode(false)
            .with_max_errors(None)
            .with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)
            .with_threads(1)
            .with_preserve_zoned_encoding(false)
            .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto));

        world.encode_options = Some(EncodeOptions::new()
            .with_codepage(Codepage::ASCII)
            .with_format(RecordFormat::Fixed)
            .with_use_raw(false)
            .with_bwz_encode(false)
            .with_strict_mode(false)
            .with_max_errors(None)
            .with_threads(1)
            .with_coerce_numbers(true)
            .with_zoned_encoding_override(None));
    }

    #[given(expr = "EBCDIC codepage")]
    async fn given_ebcdic_codepage(world: &mut CopybookWorld) {
        world.decode_options = Some(DecodeOptions::new()
            .with_codepage(Codepage::EBCDIC037)
            .with_format(RecordFormat::Fixed)
            .with_json_number_mode(JsonNumberMode::Lossless)
            .with_emit_filler(false)
            .with_emit_meta(false)
            .with_emit_raw(RawMode::Off)
            .with_strict_mode(false)
            .with_max_errors(None)
            .with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)
            .with_threads(1)
            .with_preserve_zoned_encoding(false)
            .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto));

        world.encode_options = Some(EncodeOptions::new()
            .with_codepage(Codepage::EBCDIC037)
            .with_format(RecordFormat::Fixed)
            .with_use_raw(false)
            .with_bwz_encode(false)
            .with_strict_mode(false)
            .with_max_errors(None)
            .with_threads(1)
            .with_coerce_numbers(true)
            .with_zoned_encoding_override(None));
    }

    #[when(expr = "the binary data is decoded")]
    async fn when_binary_data_decoded(world: &mut CopybookWorld) {
        let schema = world.schema();
        let decode_options = world.decode_options.as_ref()
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
                world.decoded_output = Some(String::from_utf8(output)
                    .expect("Decoded output should be valid UTF-8"));
            }
            Err(e) => {
                world.error = Some(e);
            }
        }
    }

    #[when(expr = "the JSON data is encoded")]
    async fn when_json_data_encoded(world: &mut CopybookWorld) {
        let schema = world.schema();
        let encode_options = world.encode_options.as_ref()
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
        // First decode
        let schema = world.schema();
        let decode_options = world.decode_options.as_ref()
            .expect("Decode options not set");

        let binary_data = world.binary_data();
        let mut decoded = Vec::new();

        if let Err(e) = decode_file_to_jsonl(
            schema,
            Cursor::new(binary_data),
            &mut decoded,
            decode_options,
        ) {
            world.error = Some(e);
            return;
        }

        let decoded_text = String::from_utf8(decoded)
            .expect("Decoded output should be valid UTF-8");
        world.decoded_output = Some(decoded_text.clone());

        // Then encode
        let encode_options = world.encode_options.as_ref()
            .expect("Encode options not set");

        let mut encoded = Vec::new();

        if let Err(e) = encode_jsonl_to_file(
            schema,
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
            expected, output
        );
    }

    #[then(expr = "the encoded output should be {int} bytes")]
    async fn then_encoded_output_bytes(world: &mut CopybookWorld, length: usize) {
        let output = world.encoded_output.as_ref()
            .expect("Encoded output should be set");
        assert_eq!(
            output.len(), length,
            "Expected encoded output to be {} bytes, got {}",
            length, output.len()
        );
    }

    #[then(expr = "the round-trip should be lossless")]
    async fn then_roundtrip_lossless(world: &mut CopybookWorld) {
        let original = world.binary_data();
        let encoded = world.encoded_output.as_ref()
            .expect("Encoded output should be set");

        assert_eq!(
            original, encoded.as_slice(),
            "Round-trip should be lossless: original data differs from encoded data"
        );
    }

    #[then(expr = "decoding should succeed")]
    async fn then_decoding_succeeds(world: &mut CopybookWorld) {
        assert!(world.decoded_output.is_some(), "Decoding should succeed");
        assert!(world.error.is_none(), "No error should occur during decoding");
    }

    #[then(expr = "encoding should succeed")]
    async fn then_encoding_succeeds(world: &mut CopybookWorld) {
        assert!(world.encoded_output.is_some(), "Encoding should succeed");
        assert!(world.error.is_none(), "No error should occur during encoding");
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
        world.copybook_text = Some("01 INVALID-RECORD\n    05 INVALID-FIELD INVALID PIC X(10).".to_string());
    }

    #[given(expr = "a copybook with invalid OCCURS clause")]
    async fn given_invalid_occurs(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 INVALID-OCCURS.\n\
             05 FIELD OCCURS INVALID TIMES.\n\
                 10 ELEMENT PIC X(10).".to_string(),
        );
    }

    #[given(expr = "a copybook with invalid PIC clause")]
    async fn given_invalid_pic(world: &mut CopybookWorld) {
        world.copybook_text = Some(
            "01 INVALID-PIC.\n\
             05 FIELD PIC INVALID(10).".to_string(),
        );
    }

    #[given(expr = "binary data that is too short")]
    async fn given_short_binary_data(world: &mut CopybookWorld) {
        world.binary_data = Some(vec![0x41, 0x42]); // Only 2 bytes
    }

    #[given(expr = "binary data with invalid encoding")]
    async fn given_invalid_encoding_data(world: &mut CopybookWorld) {
        world.binary_data = Some(vec![0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]);
    }

    #[given(expr = "JSON data with missing required fields")]
    async fn given_json_missing_fields(world: &mut CopybookWorld) {
        world.json_data = Some(r#"{"schema":"copybook.v1","record_index":0}"#.to_string());
    }

    #[given(expr = "JSON data with invalid field types")]
    async fn given_json_invalid_types(world: &mut CopybookWorld) {
        world.json_data = Some(r#"{"schema":"copybook.v1","record_index":0,"TEST-FIELD":12345}"#.to_string());
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
            expected, message
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
}

// This runs the Cucumber tests
#[tokio::main]
async fn main() {
    // Initialize the world with steps
    let runner = CopybookWorld::init(&[
        steps::given_copybook_with_content,
        steps::given_simple_copybook,
        steps::given_copybook_with_numeric_fields,
        steps::given_copybook_with_occurs,
        steps::given_copybook_with_odo,
        steps::given_copybook_with_redefines,
        steps::given_copybook_with_level88,
        steps::given_strict_parsing,
        steps::given_tolerant_parsing,
        steps::when_copybook_is_parsed,
        steps::then_schema_successfully_parsed,
        steps::then_schema_contains_fields,
        steps::then_field_has_type,
        steps::then_field_has_offset,
        steps::then_field_has_length,
        steps::then_schema_has_fingerprint,
        steps::then_parsing_fails_with_error,
        steps::given_binary_data,
        steps::given_json_data,
        steps::given_ascii_codepage,
        steps::given_ebcdic_codepage,
        steps::when_binary_data_decoded,
        steps::when_json_data_encoded,
        steps::when_data_roundtripped,
        steps::then_decoded_output_contains,
        steps::then_encoded_output_bytes,
        steps::then_roundtrip_lossless,
        steps::then_decoding_succeeds,
        steps::then_encoding_succeeds,
        steps::then_decoded_output_valid_json,
        steps::given_invalid_copybook,
        steps::given_invalid_occurs,
        steps::given_invalid_pic,
        steps::given_short_binary_data,
        steps::given_invalid_encoding_data,
        steps::given_json_missing_fields,
        steps::given_json_invalid_types,
        steps::then_error_occurs,
        steps::then_error_message_contains,
        steps::then_error_code_is,
    ]);

    // Run the tests with feature files from the "features" directory
    runner
        .run(&["tests/bdd/features"])
        .await;
}
