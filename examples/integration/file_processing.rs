// Complete file processing workflow example
// Demonstrates end-to-end copybook parsing and data conversion

use copybook_codec::{decode_file_to_jsonl, Codepage, DecodeOptions, JsonNumberMode, RecordFormat};
use copybook_core::parse_copybook;
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example COBOL copybook for customer records
    let copybook = r#"
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID       PIC 9(6).
          05 CUSTOMER-NAME     PIC X(30).
          05 ACCOUNT-TYPE      PIC X(1).
          05 BALANCE           PIC S9(7)V99 COMP-3.
          05 LAST-ACTIVITY     PIC 9(8).
          05 STATUS-FLAGS.
             10 ACTIVE-FLAG    PIC X(1).
             10 VIP-FLAG       PIC X(1).
             10 FILLER         PIC X(6).
    "#;

    println!("🏗️  Parsing COBOL copybook...");
    let schema = parse_copybook(copybook)?;
    println!("✅ Parsed schema with {} fields", schema.fields.len());
    println!("   Fixed record length: {} bytes", schema.lrecl_fixed.unwrap_or(0));

    // Configure decoding options for enterprise processing
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037) // EBCDIC for mainframe data
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(true)
        .with_threads(4); // Parallel processing for large files

    // Example: process a binary data file
    let input_file = "customer_data.bin";
    let output_file = "customer_records.jsonl";

    // Create sample binary data for demonstration
    create_sample_data(input_file)?;

    println!("🔄 Processing file: {} → {}", input_file, output_file);

    // Decode the entire file to JSONL format
    match decode_file_to_jsonl(&schema, input_file, output_file, &options) {
        Ok(summary) => {
            println!("✅ File processing complete!");
            println!("   Records processed: {}", summary.records_processed);
            println!("   Records successful: {}", summary.records_successful);
            println!("   Errors encountered: {}", summary.errors.len());
            println!("   Processing time: {:?}", summary.elapsed);

            if !summary.errors.is_empty() {
                println!("⚠️  Errors:");
                for (i, error) in summary.errors.iter().take(5).enumerate() {
                    println!("   {}. {}", i + 1, error);
                }
                if summary.errors.len() > 5 {
                    println!("   ... and {} more errors", summary.errors.len() - 5);
                }
            }
        }
        Err(e) => {
            eprintln!("❌ File processing failed: {}", e);
            return Err(e.into());
        }
    }

    // Show sample output
    if Path::new(output_file).exists() {
        println!("\n📄 Sample output:");
        let content = fs::read_to_string(output_file)?;
        for (i, line) in content.lines().take(3).enumerate() {
            println!("   Record {}: {}", i + 1, line);
        }
    }

    // Cleanup demo files
    let _ = fs::remove_file(input_file);
    let _ = fs::remove_file(output_file);

    Ok(())
}

fn create_sample_data(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Create sample binary data that matches our copybook
    // This would typically come from a mainframe system

    // Customer ID (6 digits) + Name (30 chars) + Type (1 char) + COMP-3 balance + Date + Flags
    let record_size = 50; // Approximate size for our copybook
    let mut data = Vec::with_capacity(record_size * 3);

    // Sample record 1: Customer 123456
    data.extend_from_slice(b"123456");
    data.extend_from_slice(b"ACME CORPORATION              ");
    data.extend_from_slice(b"B"); // Business account
    data.extend_from_slice(&[0x12, 0x34, 0x5C]); // COMP-3 packed decimal
    data.extend_from_slice(b"20241201");
    data.extend_from_slice(b"YY      "); // Active VIP with filler

    // Pad to fixed length if needed
    while data.len() < record_size {
        data.push(b' ');
    }

    fs::write(filename, data)?;
    println!("📝 Created sample data file: {}", filename);

    Ok(())
}