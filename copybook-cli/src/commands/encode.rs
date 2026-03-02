// SPDX-License-Identifier: AGPL-3.0-or-later
//! Encode command implementation

use crate::exit_codes::ExitCode;
use crate::utils::{
    ParseOptionsConfig, apply_field_projection, build_parse_options, determine_exit_code,
    read_file_or_stdin,
};
use crate::{write_stderr_all, write_stdout_all};
use anyhow::{anyhow, bail};
use copybook_cli_io::atomic_write;
use copybook_codec::{Codepage, EncodeOptions, FloatFormat, RecordFormat};
use copybook_core::parse_copybook_with_options;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;
use tracing::info;

/// Configuration options for the encode command
#[allow(clippy::struct_excessive_bools)]
pub struct EncodeCliOptions<'a> {
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
    pub float_format: FloatFormat,
    pub dialect: copybook_core::dialect::Dialect,
    pub select: &'a [String],
}

#[allow(clippy::too_many_lines)]
pub fn run(
    copybook: &Path,
    input: &Path,
    output: &Path,
    options: &EncodeCliOptions,
) -> anyhow::Result<ExitCode> {
    info!("Encoding JSONL file: {:?}", input);

    if options.strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_file_or_stdin(copybook)?;

    // Parse copybook with options
    let parse_options = build_parse_options(&ParseOptionsConfig {
        strict: options.strict,
        strict_comments: options.strict_comments,
        codepage: &options.codepage.to_string(),
        emit_filler: false,
        dialect: options.dialect,
    });
    let schema = parse_copybook_with_options(&copybook_text, &parse_options)?;

    // Apply field projection if --select is provided
    let working_schema = apply_field_projection(schema, options.select)?;

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
        .with_zoned_encoding_override(options.zoned_encoding_override)
        .with_float_format(options.float_format);

    // Check if output is stdout
    let write_to_stdout = output == Path::new("-");

    // Encode file
    let summary = if write_to_stdout {
        // Write directly to stdout (no atomic write, no summary)
        let input_file = fs::File::open(input)?;
        let mut stdout = std::io::stdout().lock();
        copybook_codec::encode_jsonl_to_file(
            &working_schema,
            input_file,
            &mut stdout,
            &encode_options,
        )?
    } else {
        // Use atomic write for file output
        let mut summary = None;
        atomic_write(output, |output_writer| {
            let input_file = fs::File::open(input).map_err(std::io::Error::other)?;
            let run_summary = copybook_codec::encode_jsonl_to_file(
                &working_schema,
                input_file,
                output_writer,
                &encode_options,
            )
            .map_err(std::io::Error::other)?;
            summary = Some(run_summary);
            Ok(())
        })?;
        summary.ok_or_else(|| {
            anyhow!("Internal error: summary not populated after successful processing")
        })?
    };

    // Print comprehensive summary (only when not writing to stdout)
    if !write_to_stdout {
        let mut summary_output = String::new();
        writeln!(&mut summary_output, "=== Encode Summary ===")?;
        writeln!(
            &mut summary_output,
            "Records processed: {}",
            summary.records_processed
        )?;
        if summary.records_with_errors > 0 {
            writeln!(
                &mut summary_output,
                "Records with errors: {}",
                summary.records_with_errors
            )?;
        }
        if summary.warnings > 0 {
            writeln!(&mut summary_output, "Warnings: {}", summary.warnings)?;
        }
        writeln!(
            &mut summary_output,
            "Processing time: {}ms",
            summary.processing_time_ms
        )?;
        writeln!(
            &mut summary_output,
            "Bytes processed: {}",
            summary.bytes_processed
        )?;
        writeln!(
            &mut summary_output,
            "Throughput: {:.2} MB/s",
            summary.throughput_mbps
        )?;
        write_stdout_all(summary_output.as_bytes())?;
    }

    // Provide detailed feedback about encode status
    if summary.records_processed == 0 && summary.records_with_errors > 0 {
        let mut err_output = String::new();
        err_output.push('\n');
        writeln!(
            &mut err_output,
            "ERROR: No records were successfully encoded."
        )?;
        writeln!(
            &mut err_output,
            "All {} records failed to encode. Use --fail-fast=false to see details of additional errors.",
            summary.records_with_errors
        )?;
        writeln!(
            &mut err_output,
            "Consider checking your input data format and copybook compatibility."
        )?;
        write_stderr_all(err_output.as_bytes())?;
    } else if summary.records_with_errors > 0 && !options.fail_fast {
        let mut err_output = String::new();
        err_output.push('\n');
        writeln!(
            &mut err_output,
            "WARNING: {} records failed to encode but were skipped in lenient mode.",
            summary.records_with_errors
        )?;
        writeln!(
            &mut err_output,
            "Use --fail-fast to stop on first error for detailed error information."
        )?;
        write_stderr_all(err_output.as_bytes())?;
    }

    // Check for fatal errors when fail-fast is enabled
    if options.fail_fast && summary.has_errors() {
        let error_msg = format!(
            "Encoding failed with {} error(s) in fail-fast mode",
            summary.records_with_errors
        );
        bail!("{error_msg}");
    }

    info!("Encode completed successfully");

    // Return appropriate exit code based on normative specification
    let exit_code = determine_exit_code(
        summary.has_warnings(),
        summary.has_errors(),
        ExitCode::Encode,
    );
    Ok(exit_code)
}
