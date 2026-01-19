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

/// Maximum allowed size for copybook files (16 MiB) to prevent memory exhaustion
const MAX_COPYBOOK_SIZE: u64 = 16 * 1024 * 1024;

/// Read file content from path or stdin if path is "-"
///
/// This function provides portable stdin support by accepting "-" as a special path.
/// When the path is "-", it reads from stdin instead of a file.
///
/// It enforces a 16 MiB size limit to prevent Denial of Service (DoS) attacks via memory exhaustion.
///
/// # Errors
///
/// Returns an error if the file cannot be read, if stdin reading fails, or if the content exceeds 16 MiB.
pub fn read_file_or_stdin<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let path = path.as_ref();

    // Helper to check size and read safely
    let check_and_read = |reader: Box<dyn Read>| -> io::Result<String> {
        // Read one byte more than the limit to detect truncation
        let mut handle = reader.take(MAX_COPYBOOK_SIZE + 1);
        let mut buffer = String::new();
        handle.read_to_string(&mut buffer)?;
        if buffer.len() as u64 > MAX_COPYBOOK_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Copybook exceeds maximum size of {MAX_COPYBOOK_SIZE} bytes"),
            ));
        }
        Ok(buffer)
    };

    if path == Path::new("-") {
        debug!("Reading from stdin");
        check_and_read(Box::new(io::stdin()))
    } else {
        debug!("Reading from file: {:?}", path);
        let file = std::fs::File::open(path)?;
        // Optimistic check using metadata (if available) to fail fast
        if let Ok(metadata) = file.metadata() {
            if metadata.len() > MAX_COPYBOOK_SIZE {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!(
                        "Copybook file size {} exceeds maximum size of {} bytes",
                        metadata.len(),
                        MAX_COPYBOOK_SIZE
                    ),
                ));
            }
        }
        // Still use check_and_read to be safe against TOCTOU or file growth
        check_and_read(Box::new(file))
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
    fn test_read_large_file_fails() -> Result<()> {
        let temp_dir = tempdir()?;
        let target_path = temp_dir.path().join("large_file.txt");

        // Create a file slightly larger than 16 MiB
        {
            let file = fs::File::create(&target_path)?;
            let mut writer = std::io::BufWriter::new(file);
            // Write in chunks to avoid large memory allocation during creation
            let chunk = vec![b'a'; 1024 * 1024];
            for _ in 0..16 {
                writer.write_all(&chunk)?;
            }
            writer.write_all(&[b'a'])?;
            writer.flush()?;
        }

        // Try to read it
        let result = read_file_or_stdin(&target_path);

        // Should fail now due to size limit
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
        assert!(err.to_string().contains("exceeds maximum size"));
        Ok(())
    }

    #[test]
    fn test_read_valid_file_succeeds() -> Result<()> {
        let temp_dir = tempdir()?;
        let target_path = temp_dir.path().join("valid_file.txt");
        let content = "01 TEST-RECORD PIC X(10).";

        fs::write(&target_path, content)?;

        let result = read_file_or_stdin(&target_path)?;
        assert_eq!(result, content);
        Ok(())
    }
}
