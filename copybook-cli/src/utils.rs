//! Utility functions for CLI operations

use crate::exit_codes::ExitCode;
use copybook_core::{ParseOptions, Schema};
use std::io::{self, Read, Write};
use std::path::Path;
#[cfg(test)]
use std::path::PathBuf;
use tempfile::NamedTempFile;
use tracing::{debug, info};

// Limit copybook file size to prevent memory exhaustion (DoS)
// 16 MiB for production, 1 KiB for testing to verify limits
#[cfg(not(test))]
const MAX_COPYBOOK_SIZE: u64 = 16 * 1024 * 1024;
#[cfg(test)]
const MAX_COPYBOOK_SIZE: u64 = 1024;

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

/// Read file content with size limit
///
/// Enforces `MAX_COPYBOOK_SIZE` limit to prevent memory exhaustion.
///
/// # Errors
///
/// Returns an error if the file cannot be read or exceeds the size limit.
pub fn read_file_with_limit<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let path = path.as_ref();
    let file = std::fs::File::open(path)?;

    // We can't rely solely on metadata().len() for all file types, so we limit the read.
    // Read up to limit + 1 byte to detect truncation.
    let mut buffer = String::new();
    let mut handle = file.take(MAX_COPYBOOK_SIZE + 1);
    handle.read_to_string(&mut buffer)?;

    if buffer.len() as u64 > MAX_COPYBOOK_SIZE {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "File too large: exceeds limit of {} bytes",
                MAX_COPYBOOK_SIZE
            ),
        ));
    }

    Ok(buffer)
}

/// Read file content from path or stdin if path is "-"
///
/// This function provides portable stdin support by accepting "-" as a special path.
/// When the path is "-", it reads from stdin instead of a file.
///
/// # Errors
///
/// Returns an error if the file cannot be read or if stdin reading fails.
pub fn read_file_or_stdin<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let path = path.as_ref();

    if path == Path::new("-") {
        debug!("Reading from stdin");
        let mut buffer = String::new();
        let mut handle = io::stdin().lock().take(MAX_COPYBOOK_SIZE + 1);
        handle.read_to_string(&mut buffer)?;

        if buffer.len() as u64 > MAX_COPYBOOK_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "Input too large: exceeds limit of {} bytes",
                    MAX_COPYBOOK_SIZE
                ),
            ));
        }

        Ok(buffer)
    } else {
        debug!("Reading from file: {:?}", path);
        read_file_with_limit(path)
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
    fn test_temp_path_for() {
        let target = Path::new("/path/to/output.jsonl");
        let temp = temp_path_for(target);
        assert_eq!(temp, Path::new("/path/to/output.jsonl.tmp"));

        let target = Path::new("output.jsonl");
        let temp = temp_path_for(target);
        assert_eq!(temp, Path::new("output.jsonl.tmp"));
    }

    #[test]
    fn test_read_file_with_limit_success() -> Result<()> {
        let temp_dir = tempdir()?;
        let file_path = temp_dir.path().join("small.txt");

        // Create a file smaller than MAX_COPYBOOK_SIZE (1024 in test)
        let content = "a".repeat(100);
        fs::write(&file_path, &content)?;

        let read_content = read_file_with_limit(&file_path)?;
        assert_eq!(read_content, content);
        Ok(())
    }

    #[test]
    fn test_read_file_with_limit_exceeded() -> Result<()> {
        let temp_dir = tempdir()?;
        let file_path = temp_dir.path().join("large.txt");

        // Create a file larger than MAX_COPYBOOK_SIZE (1024 in test)
        let content = "a".repeat(1025);
        fs::write(&file_path, &content)?;

        let result = read_file_with_limit(&file_path);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::InvalidInput);
        Ok(())
    }
}
