// SPDX-License-Identifier: AGPL-3.0-or-later
//! Encode command implementation

use crate::exit_codes::ExitCode;
use crate::utils::{
    ParseOptionsConfig, SummaryIssueCountStyle, append_processing_summary, determine_exit_code,
    effective_error_policy, log_strict_comments, parse_projected_schema, run_with_output,
};
use crate::{write_stderr_all, write_stdout_all};
use anyhow::bail;
use copybook_codec::{Codepage, EncodeOptions, FloatFormat, RecordFormat};
use std::fmt::Write as _;
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

    log_strict_comments(options.strict_comments);

    let codepage = options.codepage.to_string();
    let working_schema = parse_projected_schema(
        copybook,
        &ParseOptionsConfig {
            strict: options.strict,
            strict_comments: options.strict_comments,
            codepage: &codepage,
            emit_filler: false,
            dialect: options.dialect,
        },
        options.select,
    )?;

    let error_policy =
        effective_error_policy(options.strict, options.fail_fast, options.max_errors);

    let encode_options = EncodeOptions::new()
        .with_format(options.format)
        .with_codepage(options.codepage)
        .with_use_raw(options.use_raw)
        .with_bwz_encode(options.bwz_encode)
        .with_strict_mode(error_policy.strict_mode)
        .with_max_errors(error_policy.max_errors)
        .with_threads(options.threads)
        .with_coerce_numbers(options.coerce_numbers)
        .with_zoned_encoding_override(options.zoned_encoding_override)
        .with_float_format(options.float_format);

    let (summary, write_to_stdout) =
        run_with_output(input, output, |input_file, output_writer| {
            Ok(copybook_codec::encode_jsonl_to_file(
                &working_schema,
                input_file,
                output_writer,
                &encode_options,
            )?)
        })?;

    // Print comprehensive summary (only when not writing to stdout)
    if !write_to_stdout {
        let mut summary_output = String::new();
        append_processing_summary(
            &mut summary_output,
            "Encode",
            &summary,
            SummaryIssueCountStyle {
                show_zero_counts: false,
                repeat_nonzero_counts: false,
            },
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
