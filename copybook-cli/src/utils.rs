//! Utility functions for CLI operations

use crate::exit_codes::ExitCode;
use copybook_core::{ParseOptions, Schema};
use std::io::{self, Read, Write};
use std::path::Path;
#[cfg(test)]
use std::path::PathBuf;
use tempfile::NamedTempFile;
use tracing::{debug, info};

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

/// Maximum allowed size for copybook files (16 MiB) to prevent DoS
pub const MAX_COPYBOOK_SIZE: u64 = 16 * 1024 * 1024;

/// Read file content with size limit enforcement
///
/// # Errors
///
/// Returns an error if:
/// - The file cannot be read
/// - The file size exceeds `limit`
/// - The content is not valid UTF-8
fn read_file_with_limit<P: AsRef<Path>>(path: P, limit: u64) -> io::Result<String> {
    let path = path.as_ref();
    let mut buffer = String::new();

    if path == Path::new("-") {
        debug!("Reading from stdin (limit: {} bytes)", limit);
        // We read limit + 1 to detect truncation
        let bytes_read = io::stdin()
            .take(limit.saturating_add(1))
            .read_to_string(&mut buffer)?;
        if bytes_read as u64 > limit {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!("Input from stdin exceeds size limit of {limit} bytes"),
            ));
        }
    } else {
        debug!("Reading from file: {:?} (limit: {} bytes)", path, limit);
        // Check metadata first for efficient fail-fast
        if let Ok(metadata) = std::fs::metadata(path) {
            if metadata.len() > limit {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!(
                        "File too large: {} bytes (limit: {} bytes)",
                        metadata.len(),
                        limit
                    ),
                ));
            }
        }

        // Read with limit to prevent TOCTOU race conditions (e.g. file growing)
        let file = std::fs::File::open(path)?;
        let bytes_read = file
            .take(limit.saturating_add(1))
            .read_to_string(&mut buffer)?;

        if bytes_read as u64 > limit {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!("File content exceeds size limit of {limit} bytes"),
            ));
        }
    }

    Ok(buffer)
}

/// Read file content from path or stdin if path is "-"
///
/// This function provides portable stdin support by accepting "-" as a special path.
/// When the path is "-", it reads from stdin instead of a file.
///
/// This function enforces a default size limit of 16 MiB to prevent memory exhaustion DoS.
///
/// # Errors
///
/// Returns an error if the file cannot be read, if stdin reading fails, or if the size limit is exceeded.
pub fn read_file_or_stdin<P: AsRef<Path>>(path: P) -> io::Result<String> {
    read_file_with_limit(path, MAX_COPYBOOK_SIZE)
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
    fn test_read_file_with_limit() -> Result<()> {
        let temp_dir = tempdir()?;
        let file_path = temp_dir.path().join("test_limit.txt");

        // Create a file with 100 bytes
        {
            let mut file = std::fs::File::create(&file_path)?;
            file.write_all(&[b'a'; 100])?;
        }

        // 1. Read with limit > size (should succeed)
        let content = read_file_with_limit(&file_path, 200)?;
        assert_eq!(content.len(), 100);

        // 2. Read with limit == size (should succeed)
        let content = read_file_with_limit(&file_path, 100)?;
        assert_eq!(content.len(), 100);

        // 3. Read with limit < size (should fail)
        let result = read_file_with_limit(&file_path, 50);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("File too large"));

        Ok(())
    }
}
