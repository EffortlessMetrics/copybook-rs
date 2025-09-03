//! Decode command implementation

use crate::utils::{atomic_write, determine_exit_code};
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;
use std::fs;
use std::path::PathBuf;
use tracing::info;

#[allow(clippy::too_many_arguments)]
pub fn run(
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
        threads,
    };

    // Decode file using atomic write
    let summary = {
        let mut result_summary = None;
        atomic_write(&output, |output_writer| {
            let input_file = fs::File::open(&input).map_err(|e| std::io::Error::other(e))?;
            let summary =
                copybook_codec::decode_file_to_jsonl(&schema, input_file, output_writer, &options)
                    .map_err(|e| std::io::Error::other(e))?;
            result_summary = Some(summary);
            Ok(())
        })?;
        result_summary.unwrap()
    };

    // Print comprehensive summary
    println!("=== Decode Summary ===");
    println!("Records processed: {}", summary.records_processed);
    println!("Records with errors: {}", summary.records_with_errors);
    println!("Warnings: {}", summary.warnings);
    println!("Processing time: {}ms", summary.processing_time_ms);
    println!("Bytes processed: {}", summary.bytes_processed);
    println!("Throughput: {:.2} MB/s", summary.throughput_mbps);

    if summary.has_warnings() {
        println!("Warnings: {}", summary.warnings);
    }

    // Print error summary if available
    if summary.has_errors() {
        println!("Records with errors: {}", summary.records_with_errors);
    }

    info!("Decode completed successfully");

    // Return appropriate exit code based on normative specification
    let exit_code = determine_exit_code(summary.has_warnings(), summary.has_errors());
    Ok(exit_code)
}
