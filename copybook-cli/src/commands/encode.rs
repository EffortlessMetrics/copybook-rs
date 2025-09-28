//! Encode command implementation

use crate::utils::{atomic_write, determine_exit_code, emit_fatal, read_file_or_stdin};
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
    pub zoned_encoding_override: Option<copybook_codec::ZonedEncodingFormat>,
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
    let parse_options = ParseOptions {
        strict_comments: options.strict_comments,
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

    let encode_options = EncodeOptions::new()
        .with_format(options.format)
        .with_codepage(options.codepage)
        .with_use_raw(options.use_raw)
        .with_bwz_encode(options.bwz_encode)
        .with_strict_mode(effective_strict_mode)
        .with_max_errors(effective_max_errors)
        .with_threads(options.threads)
        .with_coerce_numbers(options.coerce_numbers)
        .with_zoned_encoding_override(options.zoned_encoding_override);

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
            Ok(()) => result_summary.ok_or_else(|| {
                Box::new(std::io::Error::other(
                    "Internal error: summary not populated after successful processing",
                )) as Box<dyn std::error::Error>
            }),
            Err(e) => {
                // Check if this is an encoding error we should surface
                if let Some(source) = e.source()
                    && let Some(encode_error) = source.downcast_ref::<copybook_core::Error>()
                {
                    // Use emit_fatal for structured copybook errors
                    let exit_code = emit_fatal(encode_error);
                    return Ok(exit_code);
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

    // Check for fatal errors when fail-fast is enabled
    if options.fail_fast && summary.has_errors() {
        let error_msg = format!(
            "Encoding failed with {} error(s) in fail-fast mode",
            summary.records_with_errors
        );
        let exit_code = emit_fatal(&std::io::Error::other(error_msg));
        return Ok(exit_code);
    }

    info!("Encode completed successfully");

    // Return appropriate exit code based on normative specification
    let exit_code = determine_exit_code(summary.has_warnings(), summary.has_errors());
    Ok(exit_code)
}
