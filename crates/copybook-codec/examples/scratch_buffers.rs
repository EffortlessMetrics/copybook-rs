#![allow(
// SPDX-License-Identifier: AGPL-3.0-or-later
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::items_after_statements
)]
// ScratchBuffers usage example
// Demonstrates zero-allocation decoding with reusable scratch buffers

use copybook_codec::memory::ScratchBuffers;
use copybook_codec::{Codepage, DecodeOptions, RecordFormat, decode_record_with_scratch};
use copybook_core::parse_copybook;

fn main() {
    println!("=== ScratchBuffers Example ===\n");

    // Example COBOL copybook for customer records
    let copybook = r"
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID       PIC 9(6).
          05 CUSTOMER-NAME     PIC X(30).
          05 ACCOUNT-TYPE      PIC X(1).
          05 BALANCE           PIC S9(7)V99 COMP-3.
          05 LAST-ACTIVITY     PIC 9(8).
    ";

    println!("🏗️  Parsing COBOL copybook...");
    let schema = match parse_copybook(copybook) {
        Ok(schema) => schema,
        Err(e) => {
            eprintln!("❌ Failed to parse copybook: {e}");
            return;
        }
    };
    println!("✅ Parsed schema with {} fields", schema.fields.len());

    // Configure decoding options
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    // Create scratch buffers once (will be reused)
    let mut scratch = ScratchBuffers::new();
    println!("✅ Created ScratchBuffers with initial capacities:");
    println!("   - digit_buffer: stack-allocated (≤32 bytes)");
    println!("   - byte_buffer: {} bytes", scratch.byte_buffer.capacity());
    println!(
        "   - string_buffer: {} chars",
        scratch.string_buffer.capacity()
    );

    // Example 1: Basic buffer usage
    println!("\n--- Example 1: Basic Buffer Usage ---");
    scratch.digit_buffer.push(5);
    scratch.digit_buffer.push(3);
    scratch.byte_buffer.extend_from_slice(b"HELLO");
    scratch.string_buffer.push_str("WORLD");

    println!("digit_buffer: {:?}", scratch.digit_buffer.as_slice());
    println!(
        "byte_buffer: {:?}",
        String::from_utf8_lossy(&scratch.byte_buffer)
    );
    println!("string_buffer: {}", scratch.string_buffer);

    // Clear for reuse (no deallocation)
    scratch.clear();
    println!("After clear(): all buffers empty, capacities preserved");
    println!(
        "byte_buffer capacity: {} bytes",
        scratch.byte_buffer.capacity()
    );

    // Example 2: Pre-allocate for large records
    println!("\n--- Example 2: Capacity Management ---");
    scratch.ensure_byte_capacity(8192); // 8 KB records
    scratch.ensure_string_capacity(4096); // 4 KB strings
    println!(
        "Pre-allocated byte_buffer: {} bytes",
        scratch.byte_buffer.capacity()
    );
    println!(
        "Pre-allocated string_buffer: {} chars",
        scratch.string_buffer.capacity()
    );

    // Example 3: Zero-allocation decoding loop
    println!("\n--- Example 3: Zero-Allocation Decoding Loop ---");

    // Create sample data (ASCII for simplicity)
    let record1 = b"000001ACME CORPORATION       B123456789012345678901234567890120241201";
    let record2 = b"000002ANOTHER COMPANY       C9876543210987654321098765432109820241202";
    let record3 = b"000003THIRD COMPANY         D5555555555555555555555555555555520241203";

    let records = [record1, record2, record3];

    println!(
        "Processing {} records with scratch buffer reuse...",
        records.len()
    );

    for (i, record_data) in records.iter().enumerate() {
        // Decode using scratch buffers (no allocation after warm-up)
        match decode_record_with_scratch(&schema, *record_data, &options, &mut scratch) {
            Ok(json_value) => {
                println!("Record {}: {}", i + 1, json_value);
            }
            Err(e) => {
                eprintln!("Error decoding record {}: {}", i + 1, e);
            }
        }

        // Clear buffers for next record (O(1), no deallocation)
        scratch.clear();
    }

    println!("\n=== Performance Benefits ===");
    println!("✅ Zero allocations in steady-state processing");
    println!("✅ CPU cache-friendly (buffers stay hot in L1/L2 cache)");
    println!("✅ Reduced GC pressure (minimal heap churn)");
    println!("✅ Thread-local (no synchronization overhead)");
}
