//! Verify command implementation
//!
//! The verify command validates data file structure against copybook schema without output generation

use crate::utils::atomic_write;
use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy, RecordIterator};
use copybook_core::parse_copybook;
use serde_json::json;
use std::fs::{File, metadata};
use std::io::BufReader;
use std::path::PathBuf;
use tracing::{info, warn, error};

/// Verification error entry for JSON report
#[derive(serde::Serialize)]
struct VerificationError {
    /// Record index (0-based)
    index: u64,
    /// Error code
    code: String,
    /// Field path where error occurred
    field: Option<String>,
    /// Byte offset in record where error occurred
    offset: Option<u64>,
    /// Error message
    message: String,
}

/// Verification report structure
#[derive(serde::Serialize)]
struct VerificationReport {
    /// Schema fingerprint for identity
    schema_fingerprint: String,
    /// File path verified
    file: String,
    /// Record format used
    format: String,
    /// Codepage used
    codepage: String,
    /// Total records processed
    records_total: u64,
    /// Total errors found
    errors_total: u64,
    /// Total warnings found
    warnings_total: u64,
    /// File size in bytes
    file_size_bytes: u64,
    /// First errors (up to `max_errors`)
    errors: Vec<VerificationError>,
    /// Sample of problematic record indexes
    sample: Vec<u64>,
}

#[allow(clippy::while_let_on_iterator, clippy::unnecessary_cast, clippy::too_many_lines)]
pub fn run(
    copybook_path: &PathBuf,
    input: &PathBuf,
    report: Option<PathBuf>,
    format: RecordFormat,
    codepage: Codepage,
    strict: bool,
    max_errors: Option<u64>,
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
        format,
        codepage,
        json_number_mode: JsonNumberMode::Native, // Simple mode for verification
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: strict,
        max_errors,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1, // Single-threaded for deterministic error reporting
    };

    // Validate record format constraints
    match format {
        RecordFormat::Fixed => {
            if let Some(lrecl) = schema.lrecl_fixed {
                // Check file size is multiple of LRECL
                if file_size % u64::from(lrecl) != 0 {
                    warn!("File size {} is not a multiple of LRECL {}", file_size, lrecl);
                }
            } else {
                return Err("Fixed format requires LRECL from schema, but schema has no fixed length".into());
            }
        }
        RecordFormat::RDW => {
            // RDW format validation will happen during record iteration
        }
    }

    // Verification state
    let mut records_total = 0u64;
    let mut errors_total = 0u64;
    let warnings_total = 0u64;
    let mut verification_errors = Vec::new();
    let mut sample_records = Vec::new();
    #[allow(clippy::cast_possible_truncation)]
    let max_errors_report = max_errors.unwrap_or(10) as usize; // Limit errors in report
    let max_samples = 5;  // Limit sample records

    // Open file and create record iterator
    let file = File::open(input)?;
    let reader = BufReader::new(file);

    // Create record iterator based on format
    let mut record_iter = RecordIterator::new(reader, &schema, &decode_options)?;

    // Process each record
    while let Some(record_result) = record_iter.next() {
        match record_result {
            Ok(_json_value) => {
                // Record decoded successfully
                records_total += 1;
            }
            Err(error) => {
                // Record failed to decode
                records_total += 1;
                errors_total += 1;

                // Extract error details
                let error_entry = VerificationError {
                    index: records_total - 1, // 0-based index
                    code: format!("{:?}", error.code),
                    field: error.context.as_ref().and_then(|ctx| ctx.field_path.clone()),
                    offset: error.context.as_ref().and_then(|ctx| ctx.byte_offset.map(|o| o as u64)),
                    message: error.message.clone(),
                };

                // Add to errors list (limited)
                if verification_errors.len() < max_errors_report {
                    verification_errors.push(error_entry);
                }

                // Add to sample records (limited)
                if sample_records.len() < max_samples {
                    sample_records.push(records_total - 1);
                }

                // Log error for immediate feedback
                error!("Record {}: {} - {}", records_total - 1, error.code, error.message);
            }
        }
    }

    // Calculate schema fingerprint (simple hash of copybook content)
    let schema_fingerprint = format!("{:x}", md5::compute(copybook_text.as_bytes()));

    // Create verification report
    let verification_report = VerificationReport {
        schema_fingerprint,
        file: input.display().to_string(),
        format: format!("{format:?}"),
        codepage: format!("{codepage:?}"),
        records_total,
        errors_total,
        warnings_total,
        file_size_bytes: file_size,
        errors: verification_errors,
        sample: sample_records,
    };

    // Print summary to stdout
    println!("Verification Summary:");
    println!("  File: {}", input.display());
    println!("  Format: {format:?}");
    println!("  Codepage: {codepage:?}");
    println!("  File Size: {file_size} bytes");
    println!("  Records Total: {records_total}");
    if errors_total > 0 {
        println!("  Errors: {} (showing first {})", errors_total, verification_report.errors.len());
        for error in &verification_report.errors {
            println!("    Record {}: {} - {}", error.index, error.code, error.message);
        }
    } else {
        println!("  Status: PASS - No validation errors");
    }
    if warnings_total > 0 {
        println!("  Warnings: {warnings_total}");
    }

    // Write detailed report if requested
    if let Some(report_path) = report {
        let report_json = json!(verification_report);
        let report_content = serde_json::to_string_pretty(&report_json)?;
        atomic_write(&report_path, |writer| {
            writer.write_all(report_content.as_bytes())
        })?;
        info!("Verification report written to: {:?}", report_path);
    }

    // Determine exit code
    let exit_code = if errors_total > 0 {
        3 // Hard errors
    } else if warnings_total > 0 {
        2 // Soft warnings
    } else {
        0 // Success
    };

    info!("Verify completed with exit code: {}", exit_code);
    Ok(exit_code)
}
