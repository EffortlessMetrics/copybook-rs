//! Decode command implementation

use copybook_core::parse_copybook;
use copybook_codec::{DecodeOptions, Codepage, RecordFormat, JsonNumberMode, RawMode, UnmappablePolicy};
use std::path::PathBuf;
use std::fs;
use tracing::info;

#[allow(clippy::too_many_arguments)]
pub async fn run(
    copybook: PathBuf,
    input: PathBuf,
    output: PathBuf,
    format: RecordFormat,
    codepage: Codepage,
    json_number: JsonNumberMode,
    strict: bool,
    max_errors: Option<u64>,
    emit_filler: bool,
    emit_meta: bool,
    emit_raw: RawMode,
    on_decode_unmappable: UnmappablePolicy,
    threads: usize,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Decoding data file: {:?}", input);
    
    // Read copybook file
    let copybook_text = fs::read_to_string(&copybook)?;
    
    // Parse copybook
    let schema = parse_copybook(&copybook_text)?;
    
    // Configure decode options
    let options = DecodeOptions {
        format,
        codepage,
        json_number_mode: json_number,
        emit_filler,
        emit_meta,
        emit_raw,
        strict_mode: strict,
        max_errors,
        on_decode_unmappable,
    };
    
    // Open input and output files
    let input_file = fs::File::open(&input)?;
    let output_file = fs::File::create(&output)?;
    
    // Decode file
    let summary = copybook_codec::decode_file_to_jsonl(&schema, input_file, output_file, &options)?;
    
    // Print summary
    println!("Decode Summary:");
    println!("  Records processed: {}", summary.records_processed);
    println!("  Records with errors: {}", summary.records_with_errors);
    println!("  Warnings: {}", summary.warnings);
    println!("  Processing time: {}ms", summary.processing_time_ms);
    println!("  Bytes processed: {}", summary.bytes_processed);
    
    info!("Decode completed successfully");
    
    // Return appropriate exit code
    if summary.records_with_errors > 0 {
        Ok(1) // Warnings/errors
    } else {
        Ok(0) // Success
    }
}