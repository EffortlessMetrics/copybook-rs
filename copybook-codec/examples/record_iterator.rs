// RecordIterator usage example
// Demonstrates streaming access to decoded records with bounded memory usage

use copybook_codec::{iter_records, DecodeOptions, Codepage, RecordFormat};
use copybook_core::parse_copybook;
use std::io::Cursor;

fn main() {
    println!("=== RecordIterator Example ===\n");

    // Example 1: Basic fixed-length record iteration
    println!("--- Example 1: Basic Fixed-Length Record Iteration ---");

    let copybook = r#"
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID       PIC 9(6).
          05 CUSTOMER-NAME     PIC X(30).
          05 ACCOUNT-TYPE      PIC X(1).
          05 BALANCE           PIC S9(7)V99 COMP-3.
          05 LAST-ACTIVITY     PIC 9(8).
    "#;

    println!("🏗️  Parsing COBOL copybook...");
    let schema = match parse_copybook(copybook) {
        Ok(schema) => schema,
        Err(e) => {
            eprintln!("❌ Failed to parse copybook: {e}");
            return;
        }
    };
    println!("✅ Parsed schema with {} fields", schema.fields.len());

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    // Create sample data (ASCII for simplicity)
    let data = b"000001ACME CORPORATION       B123456789012345678901234567890120241201\
                 000002ANOTHER COMPANY       C9876543210987654321098765432109820241202\
                 000003THIRD COMPANY         D5555555555555555555555555555555520241203";

    println!("Creating iterator from data...");
    let iterator = iter_records(Cursor::new(data), &schema, &options).unwrap();

    println!("Processing records one at a time:");
    for (index, result) in iterator.enumerate() {
        match result {
            Ok(json_value) => {
                println!("  Record {}: {}", index + 1, json_value);
            }
            Err(error) => {
                eprintln!("  Error in record {}: {}", index + 1, error);
            }
        }
    }

    // Example 2: Error recovery
    println!("\n--- Example 2: Error Recovery ---");

    let copybook2 = r#"
       01 SIMPLE-RECORD.
          05 ID PIC 9(5).
          05 DATA PIC X(10).
    "#;

    let schema2 = match parse_copybook(copybook2) {
        Ok(schema) => schema,
        Err(e) => {
            eprintln!("❌ Failed to parse copybook: {e}");
            return;
        }
    };

    let options2 = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    // Mix of valid and invalid data
    let data2 = b"00001DATA00000\
                  00002DATA00001\
                  INVALIDDATA\
                  00003DATA00002";

    let iterator2 = iter_records(Cursor::new(data2), &schema2, &options2).unwrap();

    let mut processed = 0;
    let mut errors = 0;

    println!("Processing records with error recovery:");
    for (index, result) in iterator2.enumerate() {
        match result {
            Ok(json_value) => {
                processed += 1;
                println!("  Record {}: {}", index + 1, json_value);
            }
            Err(error) => {
                errors += 1;
                eprintln!("  Error in record {}: {}", index + 1, error);
            }
        }
    }

    println!("Summary: {} processed, {} errors", processed, errors);

    // Example 3: Collecting records
    println!("\n--- Example 3: Collecting Records ---");

    let copybook3 = "01 RECORD.\n   05 ID PIC 9(5).";
    let schema3 = parse_copybook(copybook3).unwrap();
    let options3 = DecodeOptions::default();

    let data3 = b"00001000020000300004";
    let iterator3 = iter_records(Cursor::new(data3), &schema3, &options3).unwrap();

    // Collect all successful records
    use serde_json::Value;
    let records: Vec<Value> = iterator3
        .filter_map(Result::ok)  // Skip errors
        .collect();

    println!("Collected {} records:", records.len());
    for (i, record) in records.iter().enumerate() {
        println!("  Record {}: {}", i + 1, record);
    }

    // Example 4: Raw record access
    println!("\n--- Example 4: Raw Record Access ---");

    let copybook4 = "01 RECORD.\n   05 DATA PIC X(10).";
    let schema4 = parse_copybook(copybook4).unwrap();
    let options4 = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    let data4 = b"RECORD0001RECORD0002RECORD0003";
    let mut iterator4 = iter_records(Cursor::new(data4), &schema4, &options4).unwrap();

    println!("Reading raw bytes without JSON decoding:");
    while let Some(raw_bytes) = iterator4.read_raw_record().unwrap() {
        println!("  Raw record {}: {} bytes - {:?}",
                 iterator4.current_record_index(),
                 raw_bytes.len(),
                 String::from_utf8_lossy(&raw_bytes));
    }

    // Example 5: File-based iteration
    println!("\n--- Example 5: File-Based Iteration ---");
    println!("To iterate records from a file:");
    println!();
    println!("```rust");
    println!("use copybook_codec::{{iter_records_from_file, DecodeOptions, Codepage, RecordFormat}};");
    println!("use copybook_core::parse_copybook;");
    println!();
    println!("let copybook = \"01 RECORD.\\n   05 DATA PIC X(10).\";");
    println!("let schema = parse_copybook(copybook)?;");
    println!();
    println!("let options = DecodeOptions::new()");
    println!("    .with_codepage(Codepage::CP037)");
    println!("    .with_format(RecordFormat::Fixed);");
    println!();
    println!("let iterator = iter_records_from_file(\"data.bin\", &schema, &options)?;");
    println!();
    println!("for result in iterator {{");
    println!("    match result {{");
    println!("        Ok(json_value) => println!(\"{{}}\", json_value),");
    println!("        Err(error) => eprintln!(\"Error: {{}}\", error),");
    println!("    }}");
    println!("}}");
    println!("```");

    // Example 6: RDW variable-length records
    println!("\n--- Example 6: RDW Variable-Length Records ---");
    println!("To process RDW (Record Descriptor Word) variable-length records:");
    println!();
    println!("```rust");
    println!("use copybook_codec::{{RecordIterator, DecodeOptions, RecordFormat}};");
    println!("use copybook_core::parse_copybook;");
    println!("use std::fs::File;");
    println!();
    println!("let copybook = \"01 RECORD.\\n   05 DATA PIC X(100).\";");
    println!("let schema = parse_copybook(copybook)?;");
    println!();
    println!("let options = DecodeOptions::new()");
    println!("    .with_format(RecordFormat::RDW);  // RDW variable-length format");
    println!();
    println!("let file = File::open(\"transactions.dat\")?;");
    println!("let mut iterator = RecordIterator::new(file, &schema, &options)?;");
    println!();
    println!("for result in iterator {{");
    println!("    match result {{");
    println!("        Ok(json_value) => println!(\"{{}}\", json_value),");
    println!("        Err(error) => eprintln!(\"Error: {{}}\", error),");
    println!("    }}");
    println!("}}");
    println!("```");

    println!("\n=== Performance Characteristics ===");
    println!("✅ Memory: One record buffer (typically <32 KiB per record)");
    println!("✅ Throughput: Depends on decode complexity (DISPLAY vs COMP-3)");
    println!("✅ Latency: Sequential I/O optimized with BufReader");
    println!("✅ Bounded memory usage: Process multi-GB files with <256 MiB RAM");
}
