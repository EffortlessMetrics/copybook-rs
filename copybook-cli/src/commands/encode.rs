//! Encode command implementation

#![allow(clippy::pedantic)]

use crate::utils::{atomic_write, determine_exit_code};
use copybook_codec::{Codepage, EncodeOptions, RecordFormat};
use copybook_core::parse_copybook;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use tracing::info;

#[allow(clippy::too_many_arguments, clippy::collapsible_if)]
pub fn run(
    copybook: &PathBuf,
    input: &PathBuf,
    output: &PathBuf,
    format: RecordFormat,
    codepage: Codepage,
    use_raw: bool,
    bwz_encode: bool,
    strict: bool,
    max_errors: Option<u64>,
    fail_fast: bool,
    threads: usize,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Encoding JSONL file: {:?}", input);

    // Read copybook file
    let copybook_text = fs::read_to_string(copybook)?;

    // Parse copybook
    let schema = parse_copybook(&copybook_text)?;

    // Configure encode options - use strict mode when fail_fast is enabled
    let effective_strict_mode = strict || fail_fast;
    let effective_max_errors = if fail_fast { Some(1) } else { max_errors };

    let options = EncodeOptions {
        format,
        codepage,
        use_raw,
        bwz_encode,
        strict_mode: effective_strict_mode,
        max_errors: effective_max_errors,
        threads,
    };

    // Encode file using atomic write with better error handling
    let encode_result: Result<copybook_codec::RunSummary, Box<dyn std::error::Error>> = {
        let mut result_summary = None;
        let atomic_result = atomic_write(output, |output_writer| {
            let input_file = fs::File::open(input).map_err(std::io::Error::other)?;
            let summary =
                copybook_codec::encode_jsonl_to_file(&schema, input_file, output_writer, &options)
                    .map_err(std::io::Error::other)?;
            result_summary = Some(summary);
            Ok(())
        });

        match atomic_result {
            Ok(()) => Ok(result_summary.unwrap()),
            Err(e) => {
                // Check if this is an encoding error we should surface
                if let Some(source) = e.source() {
                    if let Some(encode_error) = source.downcast_ref::<copybook_core::Error>() {
                        // This is a structured copybook error - provide detailed information
                        eprintln!("Encoding failed: {} - {}", encode_error.code, encode_error.message);
                        if let Some(context) = &encode_error.context {
                            if let Some(field_path) = &context.field_path {
                                eprintln!("Field: {}", field_path);
                            }
                            if let Some(record_index) = context.record_index {
                                eprintln!("Record: {}", record_index);
                            }
                            if let Some(byte_offset) = context.byte_offset {
                                eprintln!("Byte offset: {}", byte_offset);
                            }
                        }
                        return Ok(3); // Hard error exit code
                    }
                }
                // Generic I/O error
                return Err(Box::new(e));
            }
        }
    };

    let summary = encode_result?;

    // Print comprehensive summary
    println!("=== Encode Summary ===");
    println!("Records processed: {}", summary.records_processed);
    if summary.records_with_errors > 0 {
        println!("Records with errors: {}", summary.records_with_errors);
    }
    if summary.warnings > 0 {
        println!("Warnings: {}", summary.warnings);
    }
    println!("Processing time: {}ms", summary.processing_time_ms);
    println!("Bytes processed: {}", summary.bytes_processed);
    println!("Throughput: {:.2} MB/s", summary.throughput_mbps);

    // Provide detailed feedback about encode status
    if summary.records_processed == 0 && summary.records_with_errors > 0 {
        eprintln!();
        eprintln!("ERROR: No records were successfully encoded.");
        eprintln!("All {} records failed to encode. Use --fail-fast=false to see details of additional errors.", summary.records_with_errors);
        eprintln!("Consider checking your input data format and copybook compatibility.");
    } else if summary.records_with_errors > 0 && !fail_fast {
        eprintln!();
        eprintln!("WARNING: {} records failed to encode but were skipped in lenient mode.", summary.records_with_errors);
        eprintln!("Use --fail-fast to stop on first error for detailed error information.");
    }

    info!("Encode completed successfully");

    // Return appropriate exit code based on normative specification
    let exit_code = determine_exit_code(summary.has_warnings(), summary.has_errors());
    Ok(exit_code)
}
