#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::items_after_statements,
    clippy::uninlined_format_args,
    clippy::cast_lossless,
    clippy::no_effect_underscore_binding,
    clippy::ignored_unit_patterns,
    clippy::needless_raw_string_hashes,
    clippy::needless_continue,
    clippy::doc_markdown
)]
//! Example: Decode copybook data and convert to Arrow format
//!
//! This example demonstrates:
//! - Parsing a COBOL copybook
//! - Decoding binary data using copybook-codec
//! - Converting the decoded JSON to Arrow RecordBatch
//! - Using ArrowWriter to collect batches

use copybook_arrow::{ArrowWriter, json_to_record_batch, json_to_schema};
use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, UnmappablePolicy};
use copybook_core::parse_copybook;

fn main() {
    // COBOL copybook for customer records
    let copybook = r#"
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID        PIC 9(9).
          05 CUSTOMER-NAME      PIC X(50).
          05 CUSTOMER-EMAIL     PIC X(100).
          05 ACCOUNT-BALANCE    PIC S9(13)V99 COMP-3.
          05 ACTIVE-FLAG        PIC X(1).
          05 LAST-UPDATED       PIC 9(8).
    "#;

    // Parse the copybook schema
    let schema = match parse_copybook(copybook) {
        Ok(schema) => schema,
        Err(error) => {
            eprintln!("Failed to parse copybook schema: {error}");
            return;
        }
    };

    println!("Schema loaded: {} fields", schema.fields.len());

    // Configure decode options
    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037, // EBCDIC
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: copybook_codec::RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Replace,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Sample binary data (simulated EBCDIC-encoded customer record)
    // In production, this would be actual mainframe data
    let test_data = b"000000001JOHN DOE                                     john.doe@example.com                                00000000000000000000123456Y20250108";

    println!("Input data length: {} bytes", test_data.len());

    // Decode the record
    let json_value = match copybook_codec::decode_record(&schema, test_data, &options) {
        Ok(value) => value,
        Err(error) => {
            eprintln!("Failed to decode record: {error}");
            return;
        }
    };

    println!("Decoded JSON:");
    if let Ok(pretty) = serde_json::to_string_pretty(&json_value) {
        println!("{pretty}");
    }

    // Convert JSON to Arrow schema
    let arrow_schema = match json_to_schema(&json_value) {
        Ok(schema) => schema,
        Err(error) => {
            eprintln!("Failed to create Arrow schema: {error}");
            return;
        }
    };

    println!("\nArrow Schema:");
    for field in arrow_schema.fields() {
        println!("  {}: {:?}", field.name(), field.data_type());
    }

    // Create ArrowWriter
    let mut arrow_writer = ArrowWriter::new(arrow_schema);

    // Convert JSON to RecordBatch and add to writer
    match json_to_record_batch(arrow_writer.schema(), &json_value) {
        Ok(batch) => {
            println!("\nRecordBatch created:");
            println!("  Rows: {}", batch.num_rows());
            println!("  Columns: {}", batch.num_columns());
            arrow_writer.add_batch(batch);
        }
        Err(error) => {
            eprintln!("Failed to create RecordBatch: {error}");
            return;
        }
    }

    println!("\nTotal batches written: {}", arrow_writer.batch_count());
    println!("Example completed successfully!");
}
