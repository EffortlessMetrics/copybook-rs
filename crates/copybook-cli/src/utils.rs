// SPDX-License-Identifier: AGPL-3.0-or-later
//! Utility functions for CLI operations

use crate::exit_codes::ExitCode;
use copybook_codec::RunSummary;
use copybook_core::{
    Error as CoreError, ErrorCode, ParseOptions, Schema, parse_copybook_with_options,
};
use std::fmt::Write as FmtWrite;
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
#[cfg(test)]
use std::path::PathBuf;
use tempfile::NamedTempFile;
use tracing::{debug, info};

/// Effective error handling derived from CLI `strict`, `max_errors`, and `fail_fast` flags.
pub struct ErrorPolicy {
    pub strict_mode: bool,
    pub max_errors: Option<u64>,
}

/// Normalize strict/error-limit flags shared by encode and decode commands.
#[must_use]
pub const fn effective_error_policy(
    strict: bool,
    fail_fast: bool,
    max_errors: Option<u64>,
) -> ErrorPolicy {
    ErrorPolicy {
        strict_mode: strict || fail_fast,
        max_errors: if fail_fast { Some(1) } else { max_errors },
    }
}

/// Emit the common strict-comments informational trace used by CLI commands.
pub fn log_strict_comments(strict_comments: bool) {
    if strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }
}

/// Read, parse, and optionally project a copybook schema.
///
/// This consolidates the parse/projection pipeline shared by commands that
/// transform data using a copybook.
///
/// # Errors
///
/// Returns an error when the copybook cannot be read, parsed, or projected.
pub fn parse_projected_schema(
    copybook: &Path,
    config: &ParseOptionsConfig,
    select_args: &[String],
) -> anyhow::Result<Schema> {
    let copybook_text = read_input_or_stdin(InputRole::Copybook, copybook)?;
    let parse_options = build_parse_options(config);
    let schema = parse_copybook_with_options(&copybook_text, &parse_options)?;
    apply_field_projection(schema, select_args)
}

/// Parse --select arguments (supports comma-separated and multiple flags)
///
/// This function handles both comma-separated field names in a single argument
/// and multiple --select flags, returning a deduplicated list of field names.
///
/// # Examples
///
/// ```ignore
/// // From "--select FIELD1,FIELD2"
/// let args = vec!["FIELD1,FIELD2".to_string()];
/// assert_eq!(parse_selectors(&args), vec!["FIELD1", "FIELD2"]);
///
/// // From "--select FIELD1 --select FIELD2"
/// let args = vec!["FIELD1".to_string(), "FIELD2".to_string()];
/// assert_eq!(parse_selectors(&args), vec!["FIELD1", "FIELD2"]);
/// ```
pub fn parse_selectors(select_args: &[String]) -> Vec<String> {
    use std::collections::BTreeSet;
    select_args
        .iter()
        .flat_map(|s| s.split(','))
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

/// Apply field projection to a schema if selectors are provided
///
/// Returns the original schema if no selectors are provided, or a projected
/// schema containing only the selected fields (and their dependencies like
/// ODO counters).
///
/// # Errors
///
/// Returns an error if projection fails (e.g., field not found, invalid ODO
/// dependency).
pub fn apply_field_projection(schema: Schema, select_args: &[String]) -> anyhow::Result<Schema> {
    if select_args.is_empty() {
        return Ok(schema);
    }

    let selectors = parse_selectors(select_args);
    info!(
        "Applying field projection with {} selectors",
        selectors.len()
    );
    copybook_core::project_schema(&schema, &selectors).map_err(|err| {
        anyhow::anyhow!("Failed to apply field projection with selectors {selectors:?}: {err}")
    })
}

/// Configuration for building `ParseOptions` from CLI arguments
pub struct ParseOptionsConfig<'a> {
    pub strict: bool,
    pub strict_comments: bool,
    pub codepage: &'a str,
    pub emit_filler: bool,
    pub dialect: copybook_core::dialect::Dialect,
}

/// Build `ParseOptions` from CLI configuration
///
/// This consolidates the common pattern of building `ParseOptions` across
/// different CLI commands.
pub fn build_parse_options(config: &ParseOptionsConfig) -> ParseOptions {
    ParseOptions {
        strict_comments: config.strict_comments,
        strict: config.strict,
        codepage: config.codepage.to_string(),
        emit_filler: config.emit_filler,
        allow_inline_comments: !config.strict_comments,
        dialect: config.dialect,
    }
}

/// Run a streaming transformation, writing either to stdout (`-`) or atomically to a file.
///
/// # Errors
///
/// Returns an error when input cannot be opened, the transformation fails, or the
/// output cannot be written atomically.
pub fn run_with_output<T, F>(
    input: &Path,
    output: &Path,
    mut process: F,
) -> anyhow::Result<(T, bool)>
where
    F: FnMut(fs::File, &mut dyn Write) -> anyhow::Result<T>,
{
    let write_to_stdout = output == Path::new("-");

    if write_to_stdout {
        let input_file =
            fs::File::open(input).map_err(|e| file_read_error(InputRole::Input, input, &e))?;
        let mut stdout = std::io::stdout().lock();
        return Ok((process(input_file, &mut stdout)?, true));
    }

    let mut summary = None;
    atomic_write(output, |output_writer| {
        let input_file = fs::File::open(input)
            .map_err(|e| std::io::Error::other(file_read_error(InputRole::Input, input, &e)))?;
        let run_summary = process(input_file, output_writer).map_err(std::io::Error::other)?;
        summary = Some(run_summary);
        Ok(())
    })?;

    let summary = summary.ok_or_else(|| {
        anyhow::anyhow!("Internal error: summary not populated after successful processing")
    })?;
    Ok((summary, false))
}

/// Controls how issue counts are printed in a processing summary.
#[derive(Clone, Copy)]
pub struct SummaryIssueCountStyle {
    pub show_zero_counts: bool,
    pub repeat_nonzero_counts: bool,
}

/// Append the standard encode/decode processing summary to `output`.
///
/// # Errors
///
/// Returns a formatting error if writing to the provided string fails.
pub fn append_processing_summary(
    output: &mut String,
    title: &str,
    summary: &RunSummary,
    issue_style: SummaryIssueCountStyle,
) -> std::fmt::Result {
    writeln!(output, "=== {title} Summary ===")?;
    writeln!(output, "Records processed: {}", summary.records_processed)?;
    if issue_style.show_zero_counts || summary.records_with_errors > 0 {
        writeln!(
            output,
            "Records with errors: {}",
            summary.records_with_errors
        )?;
    }
    if issue_style.show_zero_counts || summary.warnings > 0 {
        writeln!(output, "Warnings: {}", summary.warnings)?;
    }
    writeln!(output, "Processing time: {}ms", summary.processing_time_ms)?;
    writeln!(output, "Bytes processed: {}", summary.bytes_processed)?;
    writeln!(output, "Throughput: {:.2} MB/s", summary.throughput_mbps)?;

    if issue_style.repeat_nonzero_counts {
        if summary.has_warnings() {
            writeln!(output, "Warnings: {}", summary.warnings)?;
        }
        if summary.has_errors() {
            writeln!(
                output,
                "Records with errors: {}",
                summary.records_with_errors
            )?;
        }
    }

    Ok(())
}

/// Append the record failures a run captured, so the operator can see which
/// records failed and why instead of only how many.
///
/// Writes nothing when the run recorded no failures. The codec retains the
/// first `MAX_CAPTURED_FAILURES`; any beyond that are reported as a remainder.
pub fn append_record_failures(output: &mut String, summary: &RunSummary) -> std::fmt::Result {
    if summary.failures.is_empty() {
        return Ok(());
    }

    writeln!(output)?;
    writeln!(output, "Failed records:")?;
    for failure in &summary.failures {
        writeln!(output, "  {failure}")?;
    }

    let remaining = summary.undisclosed_failure_count();
    if remaining > 0 {
        writeln!(output, "  ... and {remaining} more")?;
    }

    Ok(())
}

/// Atomically write data to a file using temporary file + rename
///
/// This ensures that the output file is either completely written or not present at all,
/// preventing partial writes from being visible to other processes.
///
/// # Errors
///
/// Returns an error if the temporary file cannot be created, written to, or renamed.
pub fn atomic_write<P: AsRef<Path>, F>(path: P, write_fn: F) -> io::Result<()>
where
    F: FnOnce(&mut dyn Write) -> io::Result<()>,
{
    let path = path.as_ref();

    // Create temporary file in the same directory as the target
    let temp_dir = path.parent().unwrap_or_else(|| Path::new("."));
    let mut temp_file = NamedTempFile::new_in(temp_dir)?;

    debug!("Writing to temporary file: {:?}", temp_file.path());

    // Write data to temporary file
    write_fn(&mut temp_file)?;

    // Ensure all data is written to disk
    temp_file.flush()?;
    temp_file.as_file().sync_all()?;

    // Atomically rename temporary file to target
    debug!("Renaming {:?} to {:?}", temp_file.path(), path);
    temp_file.persist(path)?;

    Ok(())
}

/// Create a temporary file path for atomic operations
///
/// This generates a temporary file name in the same directory as the target file
/// with a .tmp suffix and random component.
#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
fn temp_path_for(target: &Path) -> PathBuf {
    let mut temp_name = target
        .file_name()
        .unwrap_or_else(|| std::ffi::OsStr::new("output"))
        .to_os_string();
    temp_name.push(".tmp");

    if let Some(parent) = target.parent() {
        parent.join(temp_name)
    } else {
        PathBuf::from(temp_name)
    }
}
/// Determine exit code based on processing results.
///
/// Warnings never flip the exit code today; we still pass the flag through so that
/// future summary logic can surface it without changing call sites. When errors are
/// present the provided `failure_code` is returned (ensuring decode uses `CBKD`, encode
/// uses `CBKE`, etc.). Otherwise we return [`ExitCode::Ok`].
pub fn determine_exit_code(
    has_warnings: bool,
    has_errors: bool,
    failure_code: ExitCode,
) -> ExitCode {
    let _ = has_warnings; // Currently informational only.
    if has_errors {
        failure_code
    } else {
        ExitCode::Ok
    }
}

/// Read file content from path or stdin if path is "-"
///
/// This function provides portable stdin support by accepting "-" as a special path.
/// When the path is "-", it reads from stdin instead of a file.
///
/// # Errors
///
/// Returns an error if the file cannot be read or if stdin reading fails.
/// Describe a file the user named on the command line, so a failure to open it can
/// say *which* file and *which* argument it came from.
#[derive(Clone, Copy)]
pub enum InputRole {
    /// The `<COPYBOOK>` positional argument.
    Copybook,
    /// The `<INPUT>` positional argument (data or JSONL).
    Input,
}

impl InputRole {
    const fn label(self) -> &'static str {
        match self {
            InputRole::Copybook => "copybook",
            InputRole::Input => "input file",
        }
    }
}

/// Turn a file-open/read failure into a `CBKF001_FILE_READ_ERROR` naming the path.
///
/// A bare `io::Error` reaching the top level prints as an unattributed
/// "No such file or directory (os error 2)" and, carrying no CBK* family, maps to
/// `ExitCode::Internal` — reporting a mistyped path as a bug in copybook-rs.
pub fn file_read_error(role: InputRole, path: &Path, error: &io::Error) -> CoreError {
    CoreError::new(
        ErrorCode::CBKF001_FILE_READ_ERROR,
        format!(
            "failed to read {} '{}': {error}",
            role.label(),
            path.display()
        ),
    )
}

/// Read a user-named file (or stdin for `-`), attributing any failure to the path.
///
/// # Errors
///
/// Returns [`ErrorCode::CBKF001_FILE_READ_ERROR`] naming the path and the argument
/// it came from when the file cannot be read.
pub fn read_input_or_stdin(role: InputRole, path: &Path) -> Result<String, CoreError> {
    read_file_or_stdin(path).map_err(|error| file_read_error(role, path, &error))
}

pub fn read_file_or_stdin<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let path = path.as_ref();

    if path == Path::new("-") {
        debug!("Reading from stdin");
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        Ok(buffer)
    } else {
        debug!("Reading from file: {:?}", path);
        std::fs::read_to_string(path)
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use anyhow::Result;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_atomic_write_success() -> Result<()> {
        let temp_dir = tempdir()?;
        let target_path = temp_dir.path().join("test.txt");

        let result = atomic_write(&target_path, |writer| writer.write_all(b"Hello, world!"));

        assert!(result.is_ok());
        assert!(target_path.exists());

        let content = fs::read_to_string(&target_path)?;
        assert_eq!(content, "Hello, world!");
        Ok(())
    }

    #[test]
    fn test_atomic_write_failure_leaves_no_file() -> Result<()> {
        let temp_dir = tempdir()?;
        let target_path = temp_dir.path().join("test.txt");

        let result = atomic_write(&target_path, |_writer| {
            Err(io::Error::other("Simulated error"))
        });

        assert!(result.is_err());
        assert!(!target_path.exists());
        Ok(())
    }

    #[test]
    fn test_determine_exit_code() {
        assert_eq!(
            determine_exit_code(false, false, ExitCode::Data),
            ExitCode::Ok
        ); // No warnings, no errors
        assert_eq!(
            determine_exit_code(true, false, ExitCode::Data),
            ExitCode::Ok
        ); // Warnings only
        assert_eq!(
            determine_exit_code(false, true, ExitCode::Data),
            ExitCode::Data
        ); // Errors only
        assert_eq!(
            determine_exit_code(true, true, ExitCode::Encode),
            ExitCode::Encode
        ); // Both warnings and errors adopt failure variant
    }

    #[test]
    fn test_effective_error_policy() {
        let lenient = effective_error_policy(false, false, Some(25));
        assert!(!lenient.strict_mode);
        assert_eq!(lenient.max_errors, Some(25));

        let fail_fast = effective_error_policy(false, true, Some(25));
        assert!(fail_fast.strict_mode);
        assert_eq!(fail_fast.max_errors, Some(1));

        let strict = effective_error_policy(true, false, None);
        assert!(strict.strict_mode);
        assert_eq!(strict.max_errors, None);
    }

    #[test]
    fn test_append_processing_summary_styles() -> Result<()> {
        let summary = RunSummary {
            records_processed: 3,
            records_with_errors: 1,
            warnings: 2,
            processing_time_ms: 42,
            bytes_processed: 2048,
            throughput_mbps: 1.5,
            ..RunSummary::default()
        };

        let mut encode_summary = String::new();
        append_processing_summary(
            &mut encode_summary,
            "Encode",
            &summary,
            SummaryIssueCountStyle {
                show_zero_counts: false,
                repeat_nonzero_counts: false,
            },
        )?;
        assert!(encode_summary.contains("=== Encode Summary ==="));
        assert_eq!(encode_summary.matches("Warnings: 2").count(), 1);
        assert_eq!(encode_summary.matches("Records with errors: 1").count(), 1);

        let mut decode_summary = String::new();
        append_processing_summary(
            &mut decode_summary,
            "Decode",
            &summary,
            SummaryIssueCountStyle {
                show_zero_counts: true,
                repeat_nonzero_counts: true,
            },
        )?;
        assert!(decode_summary.contains("=== Decode Summary ==="));
        assert_eq!(decode_summary.matches("Warnings: 2").count(), 2);
        assert_eq!(decode_summary.matches("Records with errors: 1").count(), 2);
        Ok(())
    }

    #[test]
    fn test_append_processing_summary_zero_counts() -> Result<()> {
        let summary = RunSummary {
            records_processed: 3,
            processing_time_ms: 42,
            bytes_processed: 2048,
            throughput_mbps: 1.5,
            ..RunSummary::default()
        };

        let mut encode_summary = String::new();
        append_processing_summary(
            &mut encode_summary,
            "Encode",
            &summary,
            SummaryIssueCountStyle {
                show_zero_counts: false,
                repeat_nonzero_counts: false,
            },
        )?;
        assert!(!encode_summary.contains("Warnings: 0"));
        assert!(!encode_summary.contains("Records with errors: 0"));

        let mut decode_summary = String::new();
        append_processing_summary(
            &mut decode_summary,
            "Decode",
            &summary,
            SummaryIssueCountStyle {
                show_zero_counts: true,
                repeat_nonzero_counts: true,
            },
        )?;
        assert_eq!(decode_summary.matches("Warnings: 0").count(), 1);
        assert_eq!(decode_summary.matches("Records with errors: 0").count(), 1);
        Ok(())
    }

    #[test]
    fn test_temp_path_for() {
        let target = Path::new("/path/to/output.jsonl");
        let temp = temp_path_for(target);
        assert_eq!(temp, Path::new("/path/to/output.jsonl.tmp"));

        let target = Path::new("output.jsonl");
        let temp = temp_path_for(target);
        assert_eq!(temp, Path::new("output.jsonl.tmp"));
    }
}
