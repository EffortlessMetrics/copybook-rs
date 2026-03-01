// SPDX-License-Identifier: AGPL-3.0-or-later
use std::fs::{self, File};
use std::io::Write;

use copybook_core::parse_copybook;
use copybook_gen::{
    GeneratorConfig,
    data::{DataStrategy, generate_data_with_strategy},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Generating fixture data for simple.cpy...");

    // 1. Read and parse the copybook
    let copybook_text = fs::read_to_string("fixtures/copybooks/simple.cpy")?;
    let schema = parse_copybook(&copybook_text)?;

    // 2. Configure the data generator
    let config = GeneratorConfig {
        seed: 2024, // For deterministic output
        record_count: 10,
        include_edge_cases: false,
        include_invalid_data: false,
    };

    // 3. Generate binary records
    // Using the Normal strategy for more realistic data
    let records = generate_data_with_strategy(&schema, &config, DataStrategy::Normal);

    // 4. Save the binary data to a file
    let output_path = "generated.bin";
    let mut file = File::create(output_path)?;
    for record in records {
        file.write_all(&record)?;
    }

    println!(
        "Successfully generated {} records to {}",
        config.record_count, output_path
    );

    Ok(())
}
