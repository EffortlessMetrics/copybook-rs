#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Test utilities for finding fixture files

use assert_cmd::Command;
use std::path::PathBuf;

/// Find the workspace root by looking for Cargo.toml
///
/// # Errors
///
/// Returns an error if the current directory cannot be accessed or if the workspace root
/// cannot be found by traversing parent directories.
pub fn find_workspace_root() -> Result<PathBuf, Box<dyn std::error::Error>> {
    let mut current = std::env::current_dir()
        .map_err(|e| format!("Failed to get current directory: {e}"))?;

    loop {
        if current.join("Cargo.toml").exists()
            && current.join("copybook-cli").exists()
            && current.join("copybook-codec").exists()
        {
            return Ok(current);
        }

        if let Some(parent) = current.parent() {
            current = parent.to_path_buf();
        } else {
            return Err("Could not find workspace root".into());
        }
    }
}

/// Get the path to a fixture file relative to workspace root
///
/// # Errors
///
/// Returns an error if the workspace root cannot be found.
pub fn fixture_path(relative_path: &str) -> Result<PathBuf, Box<dyn std::error::Error>> {
    Ok(find_workspace_root()?.join("fixtures").join(relative_path))
}

/// Create a copybook command with standard fixed format and CP037 codepage args
///
/// # Panics
///
/// Panics if the copybook binary cannot be found in the cargo target directory.
#[must_use]
pub fn copybook_cmd(args: &[&str]) -> Command {
    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.args(args)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037");
    cmd
}
