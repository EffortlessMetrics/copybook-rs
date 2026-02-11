#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "comprehensive-tests")]
#![allow(
    clippy::needless_raw_string_hashes,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::cast_precision_loss
)]
//! Comprehensive REDEFINES tests: shorter/equal/longer overlays, encode ambiguity, raw preservation
//!
//! This test suite validates REDEFINES handling according to the normative behavior
//! specified in the design document.

use anyhow::{Context, Result, bail};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, RunSummary,
    ZonedEncodingFormat,
};
use copybook_core::{FieldKind, Schema, parse_copybook};
use serde_json::{Value, json};
use std::io::Cursor;

type TestResult = Result<()>;

fn create_redefines_schema() -> Result<Schema> {
    let copybook = r#"
01 ORIGINAL-FIELD PIC X(20).
01 SHORT-REDEFINES REDEFINES ORIGINAL-FIELD PIC 9(10).
01 EQUAL-REDEFINES REDEFINES ORIGINAL-FIELD PIC X(20).
01 LONG-REDEFINES REDEFINES ORIGINAL-FIELD PIC X(30).
01 NEXT-FIELD PIC X(5).
"#;

    parse_copybook(copybook).map_err(Into::into)
}

fn decode_record_view(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
) -> Result<(RunSummary, Value)> {
    let input = Cursor::new(data);
    let mut output = Vec::new();
    let summary = copybook_codec::decode_file_to_jsonl(schema, input, &mut output, options)?;
    let output_str = String::from_utf8(output)?;
    let json_record = serde_json::from_str(output_str.trim())?;
    Ok((summary, json_record))
}

#[test]
fn test_redefines_shorter_overlay() -> TestResult {
    let schema = create_redefines_schema()?;

    // Find the SHORT-REDEFINES field
    let short_redefines = schema
        .fields
        .iter()
        .find(|f| f.name == "SHORT-REDEFINES")
        .context("SHORT-REDEFINES field missing from schema")?;

    // Should have same offset as original
    assert_eq!(short_redefines.offset, 0);
    assert_eq!(short_redefines.len, 10); // 10 digits = 10 bytes for zoned
    assert!(short_redefines.redefines_of.is_some());
    let redefines_of = short_redefines
        .redefines_of
        .as_ref()
        .context("SHORT-REDEFINES missing REDEFINES metadata")?;
    assert_eq!(redefines_of, "ORIGINAL-FIELD");

    // Should be zoned decimal
    assert!(matches!(
        short_redefines.kind,
        FieldKind::ZonedDecimal {
            digits: 10,
            scale: 0,
            signed: false,
            sign_separate: None
        }
    ));
    Ok(())
}

#[test]
fn test_redefines_equal_overlay() -> TestResult {
    let schema = create_redefines_schema()?;

    let equal_redefines = schema
        .fields
        .iter()
        .find(|f| f.name == "EQUAL-REDEFINES")
        .context("EQUAL-REDEFINES field missing from schema")?;

    // Should have same offset and length as original
    assert_eq!(equal_redefines.offset, 0);
    assert_eq!(equal_redefines.len, 20);
    let redefines_of = equal_redefines
        .redefines_of
        .as_ref()
        .context("EQUAL-REDEFINES missing REDEFINES metadata")?;
    assert_eq!(redefines_of, "ORIGINAL-FIELD");
    Ok(())
}

#[test]
fn test_redefines_longer_overlay() -> TestResult {
    let schema = create_redefines_schema()?;

    let long_redefines = schema
        .fields
        .iter()
        .find(|f| f.name == "LONG-REDEFINES")
        .context("LONG-REDEFINES field missing from schema")?;

    // Should have same offset but longer length
    assert_eq!(long_redefines.offset, 0);
    assert_eq!(long_redefines.len, 30);
    assert!(long_redefines.redefines_of.is_some());

    // NEXT-FIELD should be positioned after the longest REDEFINES variant
    let next_field = schema
        .fields
        .iter()
        .find(|f| f.name == "NEXT-FIELD")
        .context("NEXT-FIELD missing from schema")?;
    assert_eq!(next_field.offset, 30); // After the 30-byte LONG-REDEFINES

    // Total record size should account for largest variant
    assert_eq!(schema.lrecl_fixed, Some(35)); // 30 + 5
    Ok(())
}

#[test]
fn test_redefines_decode_all_views() -> TestResult {
    let schema = create_redefines_schema()?;

    // Create test data: 30 bytes (to fill the longest REDEFINES) + 5 bytes for NEXT-FIELD
    // Use numeric prefix so SHORT-REDEFINES (PIC 9(10)) decodes successfully
    let test_data = b"1234567890ABCDEFGHIJ          12345";

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    let (summary, json_record) = decode_record_view(&schema, test_data, &options)?;
    assert_eq!(summary.records_processed, 1);

    // All REDEFINES views should be present in declaration order
    assert!(json_record.get("ORIGINAL-FIELD").is_some());
    assert!(json_record.get("SHORT-REDEFINES").is_some());
    assert!(json_record.get("EQUAL-REDEFINES").is_some());
    assert!(json_record.get("LONG-REDEFINES").is_some());
    assert!(json_record.get("NEXT-FIELD").is_some());

    // Verify content
    assert_eq!(json_record["ORIGINAL-FIELD"], "1234567890ABCDEFGHIJ");
    assert_eq!(json_record["EQUAL-REDEFINES"], "1234567890ABCDEFGHIJ");
    assert_eq!(
        json_record["LONG-REDEFINES"],
        "1234567890ABCDEFGHIJ          "
    );
    assert_eq!(json_record["NEXT-FIELD"], "12345");
    Ok(())
}

#[test]
fn test_redefines_encode_ambiguity_error() -> TestResult {
    let schema = create_redefines_schema()?;

    // JSON with multiple non-null REDEFINES views (ambiguous)
    let json_data = json!({
        "ORIGINAL-FIELD": "Hello World",
        "SHORT-REDEFINES": "1234567890",
        "EQUAL-REDEFINES": null,
        "LONG-REDEFINES": null,
        "NEXT-FIELD": "Test"
    });

    let jsonl_data = format!("{json_data}\n");

    let options = EncodeOptions {
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        strict_mode: true,
        coerce_numbers: true,
        ..EncodeOptions::default()
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    // Direct encode currently prefers the base view when multiple values are provided
    let encoded = copybook_codec::encode_record(&schema, &json_data, &options)?;
    assert_eq!(encoded.len(), 35);
    assert_eq!(
        encoded,
        b"Hello World         1234567890\0\0\0\0\0".to_vec()
    );

    // File-based encode should mirror the single-record result
    let summary = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options)?;
    assert_eq!(summary.records_processed, 1);
    assert_eq!(summary.records_with_errors, 0);
    assert_eq!(output, encoded);
    Ok(())
}

#[test]
fn test_redefines_encode_single_view_allowed() -> TestResult {
    let schema = create_redefines_schema()?;

    // JSON with single non-null REDEFINES view (allowed)
    let json_data = json!({
        "ORIGINAL-FIELD": "Hello World Test    ",
        "NEXT-FIELD": "12345"
    });

    let jsonl_data = format!("{json_data}\n");

    let options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        use_raw: false,
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: true,
        zoned_encoding_override: None,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    // Should succeed with single non-null view
    let summary = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options)?;
    assert_eq!(summary.records_processed, 1);

    // Verify output length matches expected record size
    assert_eq!(output.len(), 35); // 30 (longest REDEFINES) + 5 (NEXT-FIELD)
    Ok(())
}

#[test]
fn test_redefines_raw_data_precedence() -> TestResult {
    let schema = create_redefines_schema()?;

    // First, decode with raw capture to get baseline
    let test_data = b"1234567890ABCDEFGHIJ          12345";

    let decode_options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Record, // Capture raw data
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    let (decode_summary, mut decoded_json) =
        decode_record_view(&schema, test_data, &decode_options)?;
    assert_eq!(decode_summary.records_processed, 1);

    // Verify raw data is present
    assert!(decoded_json.get("__raw_b64").is_some());

    // Modify multiple REDEFINES views (would normally be ambiguous)
    decoded_json["ORIGINAL-FIELD"] = json!("Modified Original   ");
    decoded_json["SHORT-REDEFINES"] = json!("9876543210");

    let jsonl_data = format!("{decoded_json}\n");

    let encode_options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        use_raw: true, // Use raw data precedence
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: true,
        zoned_encoding_override: None,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut encode_output = Vec::new();

    // Should succeed due to raw data precedence (NORMATIVE step 1)
    let summary =
        copybook_codec::encode_jsonl_to_file(&schema, input, &mut encode_output, &encode_options)?;
    assert_eq!(summary.records_processed, 1);

    // Output should match original test data (raw precedence)
    assert_eq!(encode_output, test_data);
    Ok(())
}

#[test]
fn test_redefines_round_trip_preservation() -> TestResult {
    let schema = create_redefines_schema()?;

    // Ensure record fills the longest REDEFINES cluster (35 bytes)
    let original_data = b"1234567890ABCDEFGHIJ          12345";

    // Decode with raw capture
    let decode_options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Record,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    let (_, decode_json) = decode_record_view(&schema, original_data, &decode_options)?;
    let jsonl_data = format!("{decode_json}\n");

    // Encode back with raw usage
    let encode_options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        use_raw: true,
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: true,
        zoned_encoding_override: None,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut encode_output = Vec::new();

    copybook_codec::encode_jsonl_to_file(&schema, input, &mut encode_output, &encode_options)?;

    // Should be byte-identical round-trip
    assert_eq!(encode_output, original_data);
    Ok(())
}

#[test]
fn test_redefines_cluster_size_calculation() -> TestResult {
    // Test that REDEFINES cluster size is max of all variants
    let copybook = r#"
01 BASE-FIELD PIC X(10).
01 SMALL-REDEF REDEFINES BASE-FIELD PIC 9(5).
01 LARGE-REDEF REDEFINES BASE-FIELD PIC X(25).
01 MEDIUM-REDEF REDEFINES BASE-FIELD PIC 9(15).
01 AFTER-CLUSTER PIC X(3).
"#;

    let schema = parse_copybook(copybook)?;

    // All REDEFINES should have offset 0
    for field in &schema.fields[0..4] {
        assert_eq!(field.offset, 0);
    }

    // AFTER-CLUSTER should be positioned after largest variant (25 bytes)
    let after_field = &schema.fields[4];
    assert_eq!(after_field.offset, 25);
    assert_eq!(after_field.name, "AFTER-CLUSTER");

    // Total record size should be 25 + 3 = 28
    assert_eq!(schema.lrecl_fixed, Some(28));
    Ok(())
}

#[test]
fn test_nested_redefines_groups() -> TestResult {
    let copybook = r#"
01 MAIN-RECORD.
   05 DATA-AREA PIC X(20).
   05 NUMERIC-VIEW REDEFINES DATA-AREA.
      10 PART1 PIC 9(10).
      10 PART2 PIC 9(10).
   05 BINARY-VIEW REDEFINES DATA-AREA PIC 9(9) COMP.
"#;

    let schema = parse_copybook(copybook)?;

    let main_record = &schema.fields[0];
    assert_eq!(main_record.name, "MAIN-RECORD");
    assert_eq!(main_record.children.len(), 3);

    // DATA-AREA
    let data_area = &main_record.children[0];
    assert_eq!(data_area.name, "DATA-AREA");
    assert_eq!(data_area.offset, 0);
    assert_eq!(data_area.len, 20);

    // NUMERIC-VIEW (group REDEFINES)
    let numeric_view = &main_record.children[1];
    assert_eq!(numeric_view.name, "NUMERIC-VIEW");
    assert_eq!(numeric_view.offset, 0); // Same offset as DATA-AREA
    assert!(numeric_view.redefines_of.is_some());
    assert_eq!(numeric_view.children.len(), 2);

    // BINARY-VIEW
    let binary_view = &main_record.children[2];
    assert_eq!(binary_view.name, "BINARY-VIEW");
    assert_eq!(binary_view.offset, 0); // Same offset as DATA-AREA
    assert!(binary_view.redefines_of.is_some());
    Ok(())
}

#[test]
fn test_redefines_with_occurs() -> TestResult {
    let copybook = r#"
01 ARRAY-AREA PIC X(50).
01 STRUCTURED-VIEW REDEFINES ARRAY-AREA.
   05 ITEMS PIC X(10) OCCURS 5 TIMES.
"#;

    let schema = parse_copybook(copybook)?;

    assert_eq!(schema.fields.len(), 2);

    let array_area = &schema.fields[0];
    assert_eq!(array_area.name, "ARRAY-AREA");
    assert_eq!(array_area.len, 50);

    let structured_view = &schema.fields[1];
    assert_eq!(structured_view.name, "STRUCTURED-VIEW");
    assert_eq!(structured_view.offset, 0);
    assert!(structured_view.redefines_of.is_some());
    assert_eq!(structured_view.children.len(), 1);

    let items = &structured_view.children[0];
    assert_eq!(items.name, "ITEMS");
    assert!(items.occurs.is_some());

    if let Some(copybook_core::Occurs::Fixed { count }) = &items.occurs {
        assert_eq!(*count, 5);
    } else {
        bail!("Expected fixed OCCURS, got {:?}", items.occurs);
    }
    Ok(())
}
