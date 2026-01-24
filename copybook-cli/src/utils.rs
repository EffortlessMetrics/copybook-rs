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

/// Read file content from path or stdin if path is "-"
///
/// This function provides portable stdin support by accepting "-" as a special path.
/// When the path is "-", it reads from stdin instead of a file.
///
/// # Errors
///
/// Returns an error if the file cannot be read or if stdin reading fails.
pub fn read_file_or_stdin<P: AsRef<Path>>(path: P) -> io::Result<String> {
    const MAX_INPUT_SIZE: u64 = 16 * 1024 * 1024; // 16 MiB
    let path = path.as_ref();

    let reader: Box<dyn Read> = if path == Path::new("-") {
        debug!("Reading from stdin");
        Box::new(io::stdin())
    } else {
        debug!("Reading from file: {:?}", path);
        // Fast fail check using metadata
        if let Ok(metadata) = std::fs::metadata(path) {
            if metadata.len() > MAX_INPUT_SIZE {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Input file exceeds maximum allowed size of {} bytes", MAX_INPUT_SIZE),
                ));
            }
        }
        Box::new(std::fs::File::open(path)?)
    };

    let mut buffer = String::new();
    // Read limit + 1 to detect truncation
    let mut handle = reader.take(MAX_INPUT_SIZE + 1);
    handle.read_to_string(&mut buffer)?;

    if buffer.len() as u64 > MAX_INPUT_SIZE {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Input exceeds maximum allowed size of {} bytes", MAX_INPUT_SIZE),
        ));
    }

    Ok(buffer)
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
    fn test_large_file_read_vulnerability() -> Result<()> {
        let temp_dir = tempdir()?;
        let large_file = temp_dir.path().join("large.txt");
        {
            use std::io::BufWriter;
            let f = std::fs::File::create(&large_file)?;
            let mut writer = BufWriter::new(f);
            // Write 16MB + 1 byte
            let chunk = vec![b'a'; 1024 * 1024];
            for _ in 0..16 {
                writer.write_all(&chunk)?;
            }
            writer.write_all(&[b'a'])?;
        }

        // After fix, this should FAIL
        let result = read_file_or_stdin(&large_file);
        assert!(
            result.is_err(),
            "Security fix verification: Should return error for >16MB file"
        );
        if let Err(e) = result {
            assert_eq!(e.kind(), io::ErrorKind::InvalidData);
        }

        // Test valid file (small)
        let small_file = temp_dir.path().join("small.txt");
        std::fs::write(&small_file, "valid content")?;
        let result = read_file_or_stdin(&small_file);
        assert!(result.is_ok(), "Should successfully read small file");
        assert_eq!(result.unwrap(), "valid content");

        Ok(())
    }
}
