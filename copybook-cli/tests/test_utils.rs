// SPDX-License-Identifier: AGPL-3.0-or-later
//! Test utilities for finding fixture files

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use assert_cmd::Command;
use assert_cmd::cargo::cargo_bin_cmd;
use std::error::Error;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

pub type TestResult<T> = Result<T, Box<dyn Error>>;

/// Find the workspace root by looking for Cargo.toml
///
/// # Errors
///
/// Returns an error if the current directory cannot be accessed or if the workspace root
/// cannot be found by traversing parent directories.
pub fn find_workspace_root() -> TestResult<PathBuf> {
    let mut current = std::env::current_dir().map_err(|e| {
        let mut message = String::new();
        let _ = write!(&mut message, "Failed to get current directory: {e}");
        message
    })?;

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

/// Get the path to a test-data file relative to workspace root
#[allow(dead_code)] // shared test helper for dialect fixture tests
pub fn test_data_path(relative_path: &str) -> PathBuf {
    find_workspace_root()
        .expect("Failed to find workspace root")
        .join("test-data")
        .join(relative_path)
}

/// Create a copybook command with standard fixed format and CP037 codepage args
#[must_use]
pub fn copybook_cmd(args: &[&str]) -> Command {
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args(args)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037");
    cmd
}

/// Create a copybook command without any pre-configured arguments
#[must_use]
#[allow(dead_code)] // shared test helper for fixture tests
pub fn bin() -> Command {
    cargo_bin_cmd!("copybook")
}

/// Convert an `Option<T>` into a [`TestResult`] with a helpful error message.
///
/// # Errors
/// Returns an error when `value` is `None`.
pub fn require_some<T, S>(value: Option<T>, context: S) -> TestResult<T>
where
    S: Into<String>,
{
    value
        .ok_or_else(|| std::io::Error::other(context.into()))
        .map_err(Into::into)
}

/// Convert a path to a UTF-8 string with a descriptive error if conversion fails.
///
/// # Errors
/// Returns an error when the provided `path` is not valid UTF-8.
pub fn path_to_str(path: &Path) -> TestResult<&str> {
    let mut message = String::new();
    let _ = write!(&mut message, "Path {} is not valid UTF-8", path.display());
    require_some(path.to_str(), message)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn workspace_root_contains_workspace_manifests() -> TestResult<()> {
        let root = find_workspace_root()?;
        assert!(
            root.join("Cargo.toml").exists(),
            "workspace root missing Cargo.toml"
        );
        assert!(
            root.join("copybook-cli").exists(),
            "workspace root missing copybook-cli crate"
        );
        Ok(())
    }

    #[test]
    fn fixture_path_points_inside_fixtures_directory() -> TestResult<()> {
        let path = fixture_path("copybooks/simple.cpy")?;
        assert!(path.ends_with("fixtures/copybooks/simple.cpy"));
        Ok(())
    }

    #[test]
    #[allow(clippy::unnecessary_wraps)]
    fn copybook_cmd_appends_standard_arguments() -> TestResult<()> {
        let cmd = copybook_cmd(&["inspect", "dummy"]);
        let args: Vec<String> = cmd
            .get_args()
            .map(|arg| arg.to_string_lossy().into_owned())
            .collect();

        assert_eq!(args[0], "inspect");
        assert_eq!(args[1], "dummy");
        assert!(args.contains(&"--format".to_string()));
        assert!(args.contains(&"fixed".to_string()));
        assert!(args.contains(&"--codepage".to_string()));
        assert!(args.contains(&"cp037".to_string()));
        Ok(())
    }

    #[test]
    fn require_some_transforms_missing_value_into_error() -> TestResult<()> {
        let value = require_some(Some(42), "value should exist")?;
        assert_eq!(value, 42);

        match require_some::<i32, _>(None, "missing value") {
            Ok(_) => Err("expected error when value is missing".into()),
            Err(err) => {
                assert!(err.to_string().contains("missing value"));
                Ok(())
            }
        }
    }

    #[test]
    fn path_to_str_converts_utf8_paths() -> TestResult<()> {
        let path = PathBuf::from("fixtures");
        let s = path_to_str(&path)?;
        assert_eq!(s, "fixtures");
        Ok(())
    }
}
