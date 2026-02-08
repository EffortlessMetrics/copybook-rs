//! Example: Batch processing of copybook data to Arrow/Parquet
//!
//! This example demonstrates:
//! - Processing multiple records in batches
//! - Efficient memory usage with batched operations
//! - Writing large datasets to Parquet

use copybook_arrow::{json_to_record_batch, json_to_schema, ArrowWriter, ParquetFileWriter};
use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, UnmappablePolicy};
use copybook_core::parse_copybook;
use parquet::file::properties::WriterProperties;
use std::time::Instant;

const BATCH_SIZE: usize = 1000;

fn main() {
    // COBOL copybook for inventory records
    let copybook = r#"
       01 INVENTORY-RECORD.
          05 ITEM-ID            PIC X(20).
          05 ITEM-NAME          PIC X(100).
          05 CATEGORY           PIC X(30).
          05 QUANTITY-ON-HAND  PIC 9(9).
          05 REORDER-LEVEL      PIC 9(9).
          05 UNIT-COST          PIC S9(9)V99 COMP-3.
          05 LAST-RECEIVED      PIC 9(8).
          05 WAREHOUSE-ID      PIC X(10).
          05 STATUS-CODE        PIC X(2).
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

    // Generate sample data (simulated mainframe records)
    let total_records = 5000;
    println!("Generating {} sample records...", total_records);

    let start = Instant::now();
    let mut json_records = Vec::new();

    for i in 0..total_records {
        let record_num = format!("{:020}", i);
        let item_name = format!("Item {:010}", i);
        let category = if i % 3 == 0 { "ELECTRONICS" } else if i % 3 == 1 { "CLOTHING" } else { "FOOD" };
        let quantity = (i % 1000) + 1;
        let reorder = 10;
        let cost = (i as f64 * 0.01) + 1.0;
        let date = "20250108";
        let warehouse = format!("WH{:03}", (i % 10) + 1);
        let status = if quantity > reorder { "OK" } else { "LO" };

        // Create a simple binary representation (in production, this would be actual EBCDIC data)
        let mut binary_data = Vec::new();
        binary_data.extend_from_slice(record_num.as_bytes());
        binary_data.extend_from_slice(item_name.as_bytes());
        binary_data.extend_from_slice(category.as_bytes());
        binary_data.extend_from_slice(format!("{:09}", quantity).as_bytes());
        binary_data.extend_from_slice(format!("{:09}", reorder).as_bytes());
        binary_data.extend_from_slice(date.as_bytes());
        binary_data.extend_from_slice(warehouse.as_bytes());
        binary_data.extend_from_slice(status.as_bytes());

        // Pad to fixed length
        while binary_data.len() < 200 {
            binary_data.push(0x40); // EBCDIC space
        }

        match copybook_codec::decode_record(&schema, &binary_data, &options) {
            Ok(value) => json_records.push(value),
            Err(error) => {
                eprintln!("Failed to decode record {}: {}", i, error);
            }
        }

        if (i + 1) % 1000 == 0 {
            println!("Generated {} records...", i + 1);
        }
    }

    let generation_time = start.elapsed();
    println!("Generated {} records in {:?}", json_records.len(), generation_time);

    if json_records.is_empty() {
        eprintln!("No records generated successfully");
        return;
    }

    // Create Arrow schema from first record
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

    // Process in batches
    println!("\nProcessing records in batches of {}...", BATCH_SIZE);

    let start = Instant::now();
    let mut arrow_writer = ArrowWriter::new(arrow_schema.clone());
    let mut batch_count = 0;

    for (i, record) in json_records.iter().enumerate() {
        match json_to_record_batch(&arrow_schema, record) {
            Ok(batch) => {
                arrow_writer.add_batch(batch);
                batch_count += 1;
            }
            Err(error) => {
                eprintln!("Failed to create batch for record {}: {}", i, error);
            }
        }

        if (i + 1) % BATCH_SIZE == 0 {
            println!("Processed {} records...", i + 1);
        }
    }

    let batch_time = start.elapsed();
    println!("Created {} batches in {:?}", batch_count, batch_time);

    // Write to Parquet file
    println!("\nWriting to Parquet file...");

    let writer_properties = WriterProperties::builder()
        .build();

    let parquet_writer = ParquetFileWriter::new(arrow_schema).with_writer_properties(writer_properties);

    let start = Instant::now();
    let output_path = "inventory.parquet";

    match parquet_writer.write_to_file(output_path, arrow_writer.batches()) {
        Ok(_) => {
            let write_time = start.elapsed();
            println!("Successfully wrote {} batches to {} in {:?}",
                     arrow_writer.batch_count(), output_path, write_time);
        }
        Err(error) => {
            eprintln!("Failed to write Parquet file: {error}");
            return;
        }
    }

    println!("\nSummary:");
    println!("  Total records: {}", json_records.len());
    println!("  Total batches: {}", arrow_writer.batch_count());
    println!("  Generation time: {:?}", generation_time);
    println!("  Batch creation time: {:?}", batch_time);
    println!("  Parquet write time: {:?}", start.elapsed());

    println!("\nExample completed successfully!");
}
