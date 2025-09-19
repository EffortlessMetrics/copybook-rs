//! Decode command implementation

use crate::utils::{atomic_write, determine_exit_code};
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;
use std::fs;
use std::path::PathBuf;
use tracing::info;

pub struct DecodeArgs<'a> {
    pub copybook: &'a PathBuf,
    pub input: &'a PathBuf,
    pub output: &'a PathBuf,
    pub format: RecordFormat,
    pub codepage: Codepage,
    pub json_number: JsonNumberMode,
    pub strict: bool,
    pub max_errors: Option<u64>,
    pub emit_filler: bool,
    pub emit_meta: bool,
    pub emit_raw: RawMode,
    pub on_decode_unmappable: UnmappablePolicy,
    pub threads: usize,
}

pub fn run(args: DecodeArgs) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Decoding data file: {:?}", args.input);

    // Read copybook file
    let copybook_text = fs::read_to_string(args.copybook)?;

    // Parse copybook
    let schema = parse_copybook(&copybook_text)?;

    // Configure decode options
    let options = DecodeOptions {
        format: args.format,
        codepage: args.codepage,
        json_number_mode: args.json_number,
        emit_filler: args.emit_filler,
        emit_meta: args.emit_meta,
        emit_raw: args.emit_raw,
        strict_mode: args.strict,
        max_errors: args.max_errors,
        on_decode_unmappable: args.on_decode_unmappable,
        threads: args.threads,
    };

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
