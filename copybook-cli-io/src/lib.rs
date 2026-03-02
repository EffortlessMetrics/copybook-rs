// SPDX-License-Identifier: AGPL-3.0-or-later
//! Focused CLI I/O helpers.

use std::io::{self, Read, Write};
use std::path::Path;
use tempfile::NamedTempFile;
use tracing::debug;

/// Atomically write data to a file using temporary file + rename.
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

/// Read file content from path or stdin if path is "-".
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
}
