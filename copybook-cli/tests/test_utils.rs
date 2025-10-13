//! Test utilities for finding fixture files

use assert_cmd::Command;
use std::error::Error;
use std::path::{Path, PathBuf};

pub type TestResult<T> = Result<T, Box<dyn Error>>;

/// Find the workspace root by looking for Cargo.toml
///
/// # Errors
///
/// Returns an error if the current directory cannot be accessed or if the workspace root
/// cannot be found by traversing parent directories.
pub fn find_workspace_root() -> TestResult<PathBuf> {
    let mut current =
        std::env::current_dir().map_err(|e| format!("Failed to get current directory: {e}"))?;

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
pub fn fixture_path(relative_path: &str) -> TestResult<PathBuf> {
    Ok(find_workspace_root()?.join("fixtures").join(relative_path))
}

/// Create a copybook command with standard fixed format and CP037 codepage args
///
/// # Errors
///
/// Returns an error if the `copybook` binary cannot be located.
pub fn copybook_cmd(args: &[&str]) -> TestResult<Command> {
    let mut cmd = Command::cargo_bin("copybook")?;
    cmd.args(args)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037");
    Ok(cmd)
}

/// Convert an `Option<T>` into a [`TestResult`] with a helpful error message.
pub fn require_some<T, S>(value: Option<T>, context: S) -> TestResult<T>
where
    S: Into<String>,
{
    value
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::Other, context.into()))
        .map_err(Into::into)
}

/// Convert a path to a UTF-8 string with a descriptive error if conversion fails.
pub fn path_to_str<'a>(path: &'a Path) -> TestResult<&'a str> {
    require_some(
        path.to_str(),
        format!("Path {:?} is not valid UTF-8", path),
    )
}
