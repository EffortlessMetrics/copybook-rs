//! Utility functions for CLI operations

use std::io::{self, Write};
use std::path::{Path, PathBuf};
use tempfile::NamedTempFile;
use tracing::debug;

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

/// Determine exit code based on processing results
///
/// According to the normative specification:
/// - warnings → 0 (success with warnings)
/// - any errors → 1 (completed with errors)
/// - fatal → 2 (unable to continue)
///
/// This function implements the "warnings → 0; any errors → 1" part.
/// Fatal errors (exit code 2) are handled at the main level when operations fail completely.
pub fn determine_exit_code(has_warnings: bool, has_errors: bool) -> i32 {
    if has_errors {
        1 // Completed with errors
    } else if has_warnings {
        0 // Success with warnings
    } else {
        0 // Success
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_atomic_write_success() {
        let temp_dir = tempdir().unwrap();
        let target_path = temp_dir.path().join("test.txt");

        let result = atomic_write(&target_path, |writer| writer.write_all(b"Hello, world!"));

        assert!(result.is_ok());
        assert!(target_path.exists());

        let content = fs::read_to_string(&target_path).unwrap();
        assert_eq!(content, "Hello, world!");
    }

    #[test]
    fn test_atomic_write_failure_leaves_no_file() {
        let temp_dir = tempdir().unwrap();
        let target_path = temp_dir.path().join("test.txt");

        let result = atomic_write(&target_path, |_writer| {
            Err(io::Error::new(io::ErrorKind::Other, "Simulated error"))
        });

        assert!(result.is_err());
        assert!(!target_path.exists());
    }

    #[test]
    fn test_determine_exit_code() {
        assert_eq!(determine_exit_code(false, false), 0); // No warnings, no errors
        assert_eq!(determine_exit_code(true, false), 0); // Warnings only
        assert_eq!(determine_exit_code(false, true), 1); // Errors only
        assert_eq!(determine_exit_code(true, true), 1); // Both warnings and errors
    }

    fn temp_path_for(target: &Path) -> PathBuf {
        let mut temp_path = target.to_path_buf();
        temp_path.set_file_name(format!(
            "{}.tmp",
            target
                .file_name()
                .unwrap_or_else(|| std::ffi::OsStr::new(""))
                .to_string_lossy()
        ));
        temp_path
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
