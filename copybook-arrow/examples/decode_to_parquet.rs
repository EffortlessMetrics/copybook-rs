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
//! Example: Decode copybook data and write to Parquet file
//!
//! This example demonstrates:
//! - Parsing a COBOL copybook
//! - Decoding binary data using copybook-codec
//! - Converting the decoded JSON to Parquet format
//! - Writing Parquet files with compression

use copybook_arrow::{ParquetFileWriter, json_to_schema};
use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, UnmappablePolicy};
use copybook_core::parse_copybook;
use parquet::basic::Compression;
use parquet::file::properties::{WriterProperties, WriterVersion};

fn main() {
    // COBOL copybook for transaction records
    let copybook = r#"
       01 TRANSACTION-RECORD.
          05 TXN-ID             PIC X(16).
          05 TXN-TYPE           PIC X(4).
          05 TXN-DATE           PIC 9(8).
          05 TXN-TIME           PIC 9(6).
          05 ACCOUNT-NUMBER     PIC 9(12).
          05 AMOUNT             PIC S9(13)V99 COMP-3.
          05 CURRENCY-CODE      PIC X(3).
          05 REFERENCE-NUMBER   PIC X(20).
          05 DESCRIPTION        PIC X(50).
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

    // Sample binary data (simulated EBCDIC-encoded transaction records)
    // Record length: 16+4+8+6+12+8+3+20+50 = 127 bytes
    let test_records: Vec<&[u8]> = vec![
        b"TXN0000000000001SALE202501081234560000000012345USDREF00000000000000000001Test transaction one ",
        b"TXN0000000000002PURC20250108123457000000000098765USDREF00000000000000000002Test transaction two ",
        b"TXN0000000000003REFN20250108123458000000000054321EURREF00000000000000000003Test transaction thre",
    ];

    println!("Processing {} records...", test_records.len());

    // Decode all records
    let mut json_records = Vec::new();
    for (i, test_data) in test_records.iter().enumerate() {
        let test_data: &[u8] = test_data;
        println!("Processing record {} ({} bytes)", i + 1, test_data.len());

        match copybook_codec::decode_record(&schema, test_data, &options) {
            Ok(value) => json_records.push(value),
            Err(error) => {
                eprintln!("Failed to decode record {}: {}", i + 1, error);
                continue;
            }
        }
    }

    println!("Successfully decoded {} records", json_records.len());

    // Create Arrow schema from first record
    if json_records.is_empty() {
        eprintln!("No records decoded successfully");
        return;
    }

    let arrow_schema = match json_to_schema(&json_records[0]) {
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

    // Create Parquet writer with custom properties
    let writer_properties = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .set_writer_version(WriterVersion::PARQUET_2_0)
        .set_created_by("copybook-arrow".to_string())
        .build();

    let parquet_writer =
        ParquetFileWriter::new(arrow_schema).with_writer_properties(writer_properties);

    // Write to Parquet file
    let output_path = "transactions.parquet";
    println!("\nWriting to Parquet file: {}", output_path);

    match parquet_writer.write_json_records(output_path, &json_records) {
        Ok(_) => {
            println!(
                "Successfully wrote {} records to {}",
                json_records.len(),
                output_path
            );
        }
        Err(error) => {
            eprintln!("Failed to write Parquet file: {error}");
            return;
        }
    }

    println!("Example completed successfully!");
}
