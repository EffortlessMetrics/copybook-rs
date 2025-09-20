//! Verify command implementation
//!
//! The verify command validates data file structure against copybook schema without output generation

use super::verify_report::{VerifyCliEcho, VerifyError, VerifyReport, VerifySample};
use crate::utils::atomic_write;
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, RecordIterator,
    UnmappablePolicy,
};
use copybook_core::parse_copybook;
use std::fs::{File, metadata};
use std::io::{BufReader, Read, Seek, SeekFrom};
use std::path::PathBuf;
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

/// Configuration options for the verify command
pub struct VerifyOptions {
    pub format: RecordFormat,
    pub codepage: Codepage,
    pub strict: bool,
    pub max_errors: u32,
    pub sample: u32,
}

#[allow(clippy::too_many_lines)]
pub fn run(
    copybook_path: &PathBuf,
    input: &PathBuf,
    report: Option<PathBuf>,
    opts: &VerifyOptions,
) -> Result<i32, Box<dyn std::error::Error>> {
    info!("Verifying data file: {:?}", input);

    // Read copybook file
    let copybook_text = std::fs::read_to_string(copybook_path)?;

    // Parse copybook (strict mode for verification)
    let schema = parse_copybook(&copybook_text)?;

    // Get file metadata
    let file_metadata = metadata(input)?;
    let file_size = file_metadata.len();

    // Configure decode options for verification
    let decode_options = DecodeOptions {
        format: opts.format,
        codepage: opts.codepage,
        json_number_mode: JsonNumberMode::Native, // Simple mode for verification
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: opts.strict,
        max_errors: Some(u64::from(opts.max_errors)),
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1, // Single-threaded for deterministic error reporting
    };

    // Validate record format constraints
    match opts.format {
        RecordFormat::Fixed => {
            if let Some(lrecl) = schema.lrecl_fixed {
                // Check file size is multiple of LRECL
                if file_size % u64::from(lrecl) != 0 {
                    warn!(
                        "File size {} is not a multiple of LRECL {}",
                        file_size, lrecl
                    );
                }
            } else {
                return Err(
                    "Fixed format requires LRECL from schema, but schema has no fixed length"
                        .into(),
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

    // Open a separate file handle for efficient hex sampling
    let mut file_raw = File::open(input)?;

    // Create record iterator based on format
    let record_iter = RecordIterator::new(reader, &schema, &decode_options)?;

    // Process each record
    for record_result in record_iter {
        records_total += 1;

        match record_result {
            Ok(_json_value) => {
                // Record decoded successfully - no action needed
            }
            Err(error) => {
                // Record failed to decode - efficiently read the raw record data
                let record_bytes = if let Some(lrecl) = schema.lrecl_fixed {
                    // For fixed format, use seek + read_exact for efficiency
                    let record_offset = (records_total - 1) * u64::from(lrecl);

                    match file_raw.seek(SeekFrom::Start(record_offset)) {
                        Ok(_) => {
                            let mut rec = vec![0u8; lrecl as usize];
                            match file_raw.read_exact(&mut rec) {
                                Ok(()) => Some(rec),
                                Err(_) => None, // Fall back to no hex if partial read
                            }
                        }
                        Err(_) => None,
                    }
                } else {
                    // For RDW format, we'd need to track record boundaries differently
                    // For now, skip hex sampling for RDW records with errors
                    None
                };

                // Extract error details with hex capability
                let error_offset = error.context.as_ref().and_then(|ctx| ctx.byte_offset);
                let hex_data = record_bytes.as_ref().map(|bytes| {
                    if let Some(off) = error_offset {
                        hex_window(bytes, usize::try_from(off).unwrap_or(0), HEX_CTX)
                    } else {
                        hex_bytes(bytes, HEX_FALLBACK)
                    }
                });

                let error_entry = VerifyError {
                    index: records_total - 1, // 0-based index
                    code: format!("{:?}", error.code),
                    field: error
                        .context
                        .as_ref()
                        .and_then(|ctx| ctx.field_path.clone()),
                    offset: error_offset,
                    msg: error.message.clone(),
                    hex: hex_data,
                };

                // Add error to report
                verify_report.add_error(error_entry);

                // Add sample record with actual hex data if available
                if let Some(ref bytes) = record_bytes {
                    let sample = VerifySample {
                        index: records_total - 1,
                        hex: hex_bytes(bytes, 256), // First 256 bytes for samples
                    };
                    verify_report.add_sample(sample);
                }

                // Log error for immediate feedback
                error!(
                    "Record {}: {} - {}",
                    records_total - 1,
                    error.code,
                    error.message
                );
            }
        }
    }

    // Update report with totals
    verify_report.set_records_total(records_total);

    // Print summary to stdout
    println!("Verification Summary:");
    println!("  File: {}", input.display());
    println!("  Format: {:?}", opts.format);
    println!("  Codepage: {:?}", opts.codepage);
    println!("  File Size: {file_size} bytes");
    println!("  Records Total: {}", verify_report.records_total);
    if verify_report.errors_total > 0 {
        println!(
            "  Errors: {} (showing first {})",
            verify_report.errors_total,
            verify_report.errors.len()
        );
        if verify_report.truncated {
            println!(
                "  Warning: Error list truncated at {} errors",
                verify_report.cli_opts.max_errors
            );
        }
        for error in &verify_report.errors {
            println!("    Record {}: {} - {}", error.index, error.code, error.msg);
        }
    } else {
        println!("  Status: PASS - No validation errors");
    }

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
