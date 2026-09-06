// SPDX-License-Identifier: AGPL-3.0-or-later
//! Verify command implementation
//!
//! The verify command validates data file structure against copybook schema without output generation

use super::verify_report::{VerifyCliEcho, VerifyError, VerifyReport, VerifySample};
use crate::exit_codes::ExitCode;
use crate::utils::{
    InputRole, ParseOptionsConfig, apply_field_projection, atomic_write, build_parse_options,
    read_input_or_stdin,
};
use crate::write_stdout_all;
use copybook_codec::file::fixed as fixed_file;
use copybook_codec::lib_api::decode_record_with_raw_data;
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, RecordIterator,
    UnmappablePolicy,
};
use copybook_core::{Error, parse_copybook_with_options};
use std::fmt::Write as _;
use std::fs::{File, metadata};
use std::io::BufReader;
use std::path::{Path, PathBuf};
use tracing::{error, info, warn};

// Hex display constants for consistent output formatting
const HEX_CTX: usize = 16;
const HEX_FALLBACK: usize = 64;

// Report structures now defined in verify_report.rs

/// Helper function to create hex string from bytes
#[allow(clippy::format_collect)]
fn hex_bytes(bytes: &[u8], max: usize) -> String {
    bytes
        .iter()
        .take(max)
        .map(|b| format!("{b:02X}"))
        .collect::<String>()
}

/// Helper function to create centered hex window around error offset
#[allow(clippy::format_collect)]
fn hex_window(bytes: &[u8], offset: usize, ctx: usize) -> String {
    let start = offset.saturating_sub(ctx);
    let end = (offset + ctx).min(bytes.len());
    bytes[start..end]
        .iter()
        .map(|b| format!("{b:02X}"))
        .collect::<String>()
}

/// Add one verification failure using bytes returned by the codec framing boundary.
fn record_verification_error(
    report: &mut VerifyReport,
    record_index: u64,
    error: &Error,
    record_bytes: Option<&[u8]>,
) {
    let error_offset = error.context.as_ref().and_then(|ctx| ctx.byte_offset);
    let hex_data = record_bytes.map(|bytes| {
        if let Some(off) = error_offset {
            match usize::try_from(off) {
                Ok(offset) => hex_window(bytes, offset, HEX_CTX),
                Err(_) => hex_bytes(bytes, HEX_FALLBACK),
            }
        } else {
            hex_bytes(bytes, HEX_FALLBACK)
        }
    });

    report.add_error(VerifyError {
        index: record_index,
        code: format!("{:?}", error.code),
        field: error
            .context
            .as_ref()
            .and_then(|ctx| ctx.field_path.clone()),
        offset: error_offset,
        msg: error.message.clone(),
        hex: hex_data,
    });

    if let Some(bytes) = record_bytes {
        report.add_sample(VerifySample {
            index: record_index,
            hex: hex_bytes(bytes, 256),
        });
    }

    error!(
        "Record {}: {} - {}",
        record_index, error.code, error.message
    );
}

/// Configuration options for the verify command
pub struct VerifyOptions<'a> {
    pub format: RecordFormat,
    pub codepage: Codepage,
    pub strict: bool,
    pub max_errors: u32,
    pub sample: u32,
    pub strict_comments: bool,
    pub dialect: copybook_core::dialect::Dialect,
    pub select: &'a [String],
}

#[allow(clippy::too_many_lines)]
pub fn run(
    copybook_path: &Path,
    input: &Path,
    report: Option<PathBuf>,
    opts: &VerifyOptions,
) -> anyhow::Result<ExitCode> {
    info!("Verifying data file: {:?}", input);

    if opts.strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_input_or_stdin(InputRole::Copybook, copybook_path)?;

    // Parse copybook with options
    let parse_options = build_parse_options(&ParseOptionsConfig {
        strict: opts.strict,
        strict_comments: opts.strict_comments,
        codepage: &opts.codepage.to_string(),
        emit_filler: false,
        dialect: opts.dialect,
    });
    let schema = parse_copybook_with_options(&copybook_text, &parse_options)?;

    // Apply field projection if --select is provided
    let working_schema = apply_field_projection(schema, opts.select)?;

    // Get file metadata
    let file_metadata = metadata(input)?;
    let file_size = file_metadata.len();

    // Configure decode options for verification
    let decode_options = DecodeOptions::new()
        .with_format(opts.format)
        .with_codepage(opts.codepage)
        .with_json_number_mode(JsonNumberMode::Native) // Simple mode for verification
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(opts.strict)
        .with_max_errors(Some(u64::from(opts.max_errors)))
        .with_unmappable_policy(UnmappablePolicy::Error)
        .with_threads(1) // Single-threaded for deterministic error reporting
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto);

    // Validate record format constraints
    match opts.format {
        RecordFormat::Fixed => {
            let lrecl = fixed_file::lrecl(&working_schema)?;
            // Check file size is multiple of LRECL
            if file_size % u64::from(lrecl) != 0 {
                warn!(
                    "File size {} is not a multiple of LRECL {}",
                    file_size, lrecl
                );
            }
        }
        RecordFormat::RDW => {
            // RDW format validation will happen during record iteration
        }
    }

    // Calculate schema fingerprint (simple hash of copybook content)
    let schema_fingerprint = format!("{:x}", md5::compute(copybook_text.as_bytes()));

    // Create CLI options echo for report
    let cli_opts = VerifyCliEcho {
        codepage: format!("{:?}", opts.codepage),
        strict: opts.strict,
        max_errors: opts.max_errors,
        sample: opts.sample,
        strict_comments: opts.strict_comments,
    };

    // Initialize report
    let mut verify_report = VerifyReport::new(
        schema_fingerprint,
        format!("{:?}", opts.format).to_lowercase(),
        input.to_string_lossy().to_string(),
        file_size,
        cli_opts,
    );

    // Verification state
    let mut records_total = 0u64;

    // Open file and create record iterator
    let file = File::open(input)?;
    let reader = BufReader::new(file);

    // Create record iterator based on format
    let mut record_iter = RecordIterator::new(reader, &working_schema, &decode_options)?;

    // Let the codec own fixed/RDW framing so diagnostics use the exact
    // payload that failed decoding rather than a second schema-based read.
    loop {
        match record_iter.read_raw_record() {
            Ok(Some(record_bytes)) => {
                records_total += 1;
                let codec_record_index = record_iter.current_record_index();
                if let Err(error) = decode_record_with_raw_data(
                    &working_schema,
                    &record_bytes,
                    &decode_options,
                    None,
                    codec_record_index,
                ) {
                    record_verification_error(
                        &mut verify_report,
                        records_total - 1,
                        &error,
                        Some(&record_bytes),
                    );
                }
            }
            Ok(None) => break,
            Err(error) => {
                records_total += 1;
                record_verification_error(&mut verify_report, records_total - 1, &error, None);
                // A framing error leaves no reliable record boundary from which
                // verification can resume.
                break;
            }
        }
    }

    // Update report with totals
    verify_report.set_records_total(records_total);

    // Print summary to stdout
    let mut summary_output = String::new();
    writeln!(&mut summary_output, "Verification Summary:")?;
    writeln!(&mut summary_output, "  File: {}", input.display())?;
    writeln!(&mut summary_output, "  Format: {:?}", opts.format)?;
    writeln!(&mut summary_output, "  Codepage: {:?}", opts.codepage)?;
    writeln!(&mut summary_output, "  File Size: {file_size} bytes")?;
    writeln!(
        &mut summary_output,
        "  Records Total: {}",
        verify_report.records_total
    )?;
    if verify_report.errors_total > 0 {
        writeln!(
            &mut summary_output,
            "  Errors: {} (showing first {})",
            verify_report.errors_total,
            verify_report.errors.len()
        )?;
        if verify_report.truncated {
            writeln!(
                &mut summary_output,
                "  Warning: Error list truncated at {} errors",
                verify_report.cli_opts.max_errors
            )?;
        }
        for error in &verify_report.errors {
            writeln!(
                &mut summary_output,
                "    Record {}: {} - {}",
                error.index, error.code, error.msg
            )?;
        }
    } else {
        writeln!(&mut summary_output, "  Status: PASS - No validation errors")?;
    }

    write_stdout_all(summary_output.as_bytes())?;

    // Write detailed report if requested
    if let Some(report_path) = report {
        let report_content = serde_json::to_string_pretty(&verify_report)?;
        atomic_write(&report_path, |writer| {
            writer.write_all(report_content.as_bytes())
        })?;
        info!("Verification report written to: {:?}", report_path);
    }

    // Determine exit code based on report
    let exit_code = verify_report.exit_code();

    info!("Verify completed with exit code: {}", exit_code);
    Ok(exit_code)
}
