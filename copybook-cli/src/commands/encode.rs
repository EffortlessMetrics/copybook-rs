//! Encode command implementation

use crate::utils::{atomic_write, determine_exit_code, read_file_or_stdin};
use copybook_codec::{Codepage, EncodeOptions, RecordFormat};
use copybook_core::{ParseOptions, parse_copybook_with_options};
use std::error::Error;
use std::fs;
use std::path::Path;
use tracing::info;

/// Configuration options for the encode command
#[allow(clippy::struct_excessive_bools)]
pub struct EncodeCliOptions {
    pub format: RecordFormat,
    pub codepage: Codepage,
    pub use_raw: bool,
    pub bwz_encode: bool,
    pub strict: bool,
    pub max_errors: Option<u64>,
    pub fail_fast: bool,
    pub threads: usize,
    pub coerce_numbers: bool,
    pub strict_comments: bool,
}

pub fn run(
    copybook: &Path,
    input: &Path,
    output: &Path,
    options: &EncodeCliOptions,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Encoding JSONL file: {:?}", input);

    if options.strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_file_or_stdin(copybook)?;

    // Parse copybook with options
    let parse_options = ParseOptions { strict_comments: false,
        strict: options.strict,
        codepage: options.codepage.to_string(),
        emit_filler: false,
        allow_inline_comments: !options.strict_comments,
    };
    let schema = parse_copybook_with_options(&copybook_text, &parse_options)?;

    // Configure encode options - use strict mode when fail_fast is enabled
    let effective_strict_mode = options.strict || options.fail_fast;
    let effective_max_errors = if options.fail_fast {
        Some(1)
    } else {
        options.max_errors
    };

    let encode_options = EncodeOptions {
        format: options.format,
        codepage: options.codepage,
        use_raw: options.use_raw,
        bwz_encode: options.bwz_encode,
        strict_mode: effective_strict_mode,
        max_errors: effective_max_errors,
        threads: options.threads,
        coerce_numbers: options.coerce_numbers,
    };

    // Encode file using atomic write with better error handling
    let encode_result: Result<copybook_codec::RunSummary, Box<dyn std::error::Error>> = {
        let mut result_summary = None;
        let atomic_result = atomic_write(output, |output_writer| {
            let input_file = fs::File::open(input).map_err(std::io::Error::other)?;
            let summary = copybook_codec::encode_jsonl_to_file(
                &schema,
                input_file,
                output_writer,
                &encode_options,
            )
            .map_err(std::io::Error::other)?;
            result_summary = Some(summary);
            Ok(())
        });

        match atomic_result {
            Ok(()) => Ok(result_summary.unwrap()),
            Err(e) => {
                // Check if this is an encoding error we should surface
                if let Some(source) = e.source()
                    && let Some(encode_error) = source.downcast_ref::<copybook_core::Error>()
                {
                    // This is a structured copybook error - provide detailed information
                    eprintln!(
                        "Encoding failed: {} - {}",
                        encode_error.code, encode_error.message
                    );
                    if let Some(context) = &encode_error.context {
                        if let Some(field_path) = &context.field_path {
                            eprintln!("Field: {field_path}");
                        }
                        if let Some(record_index) = context.record_index {
                            eprintln!("Record: {record_index}");
                        }
                        if let Some(byte_offset) = context.byte_offset {
                            eprintln!("Byte offset: {byte_offset}");
                        }
                    }
                    return Ok(3); // Hard error exit code
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
        eprintln!(
            "All {} records failed to encode. Use --fail-fast=false to see details of additional errors.",
            summary.records_with_errors
        );
        eprintln!("Consider checking your input data format and copybook compatibility.");
    } else if summary.records_with_errors > 0 && !options.fail_fast {
        eprintln!();
        eprintln!(
            "WARNING: {} records failed to encode but were skipped in lenient mode.",
            summary.records_with_errors
        );
        eprintln!("Use --fail-fast to stop on first error for detailed error information.");
    }

    info!("Encode completed successfully");

    // Return appropriate exit code based on normative specification
    let exit_code = determine_exit_code(summary.has_warnings(), summary.has_errors());
    Ok(exit_code)
}
