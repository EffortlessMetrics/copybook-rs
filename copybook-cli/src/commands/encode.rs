//! Encode command implementation

use copybook_codec::{Codepage, EncodeOptions, RecordFormat};
use copybook_core::parse_copybook;
use std::fs;
use std::path::PathBuf;
use tracing::info;

#[allow(clippy::too_many_arguments)]
pub async fn run(
    copybook: PathBuf,
    input: PathBuf,
    output: PathBuf,
    format: RecordFormat,
    codepage: Codepage,
    use_raw: bool,
    bwz_encode: bool,
    strict: bool,
    max_errors: Option<u64>,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Encoding JSONL file: {:?}", input);

    // Read copybook file
    let copybook_text = fs::read_to_string(&copybook)?;

    // Parse copybook
    let schema = parse_copybook(&copybook_text)?;

    // Configure encode options
    let options = EncodeOptions {
        format,
        codepage,
        use_raw,
        bwz_encode,
        strict_mode: strict,
        max_errors,
    };

    // Open input and output files
    let input_file = fs::File::open(&input)?;
    let output_file = fs::File::create(&output)?;

    // Encode file
    let summary = copybook_codec::encode_jsonl_to_file(&schema, input_file, output_file, &options)?;

    // Print summary
    println!("Encode Summary:");
    println!("  Records processed: {}", summary.records_processed);
    println!("  Records with errors: {}", summary.records_with_errors);
    println!("  Warnings: {}", summary.warnings);
    println!("  Processing time: {}ms", summary.processing_time_ms);
    println!("  Bytes processed: {}", summary.bytes_processed);

    info!("Encode completed successfully");

    // Return appropriate exit code
    if summary.records_with_errors > 0 {
        Ok(1) // Warnings/errors
    } else {
        Ok(0) // Success
    }
}
