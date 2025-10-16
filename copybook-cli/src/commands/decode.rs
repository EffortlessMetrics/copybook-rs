//! Decode command implementation

use crate::utils::{atomic_write, determine_exit_code, read_file_or_stdin};
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::{ParseOptions, parse_copybook_with_options};
use std::fs;
use std::path::PathBuf;
use tracing::info;

#[allow(clippy::struct_excessive_bools)]
pub struct DecodeArgs<'a> {
    pub copybook: &'a PathBuf,
    pub input: &'a PathBuf,
    pub output: &'a PathBuf,
    pub format: RecordFormat,
    pub codepage: Codepage,
    pub json_number: JsonNumberMode,
    pub strict: bool,
    pub max_errors: Option<u64>,
    pub fail_fast: bool,
    pub emit_filler: bool,
    pub emit_meta: bool,
    pub emit_raw: RawMode,
    pub on_decode_unmappable: UnmappablePolicy,
    pub threads: usize,
    pub strict_comments: bool,
    pub preserve_zoned_encoding: bool,
    pub preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat,
}

pub fn run(args: &DecodeArgs) -> anyhow::Result<i32> {
    info!("Decoding data file: {:?}", args.input);

    if args.strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_file_or_stdin(args.copybook)?;

    // Parse copybook with options
    let parse_options = ParseOptions {
        strict_comments: false,
        strict: args.strict,
        codepage: args.codepage.to_string(),
        emit_filler: args.emit_filler,
        allow_inline_comments: !args.strict_comments,
    };
    let schema = parse_copybook_with_options(&copybook_text, &parse_options)?;

    // Configure decode options - use strict mode when fail_fast is enabled
    let effective_strict_mode = args.strict || args.fail_fast;
    let effective_max_errors = if args.fail_fast {
        Some(1)
    } else {
        args.max_errors
    };

    let options = DecodeOptions::new()
        .with_format(args.format)
        .with_codepage(args.codepage)
        .with_json_number_mode(args.json_number)
        .with_emit_filler(args.emit_filler)
        .with_emit_meta(args.emit_meta)
        .with_emit_raw(args.emit_raw)
        .with_strict_mode(effective_strict_mode)
        .with_max_errors(effective_max_errors)
        .with_unmappable_policy(args.on_decode_unmappable)
        .with_threads(args.threads)
        .with_preserve_zoned_encoding(args.preserve_zoned_encoding)
        .with_preferred_zoned_encoding(args.preferred_zoned_encoding);

    // Decode file using atomic write
    let summary = {
        let mut result_summary = None;
        atomic_write(args.output, |output_writer| {
            let input_file = fs::File::open(args.input).map_err(std::io::Error::other)?;
            let summary =
                copybook_codec::decode_file_to_jsonl(&schema, input_file, output_writer, &options)
                    .map_err(std::io::Error::other)?;
            result_summary = Some(summary);
            Ok(())
        })?;
        result_summary.ok_or_else(|| {
            std::io::Error::other(
                "Internal error: summary not populated after successful processing",
            )
        })?
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
