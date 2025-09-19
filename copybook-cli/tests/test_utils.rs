//! Test utilities for finding fixture files

use assert_cmd::Command;
use std::path::PathBuf;

/// Find the workspace root by looking for Cargo.toml
pub fn find_workspace_root() -> PathBuf {
    let mut current = std::env::current_dir().expect("Failed to get current directory");

    loop {
        if current.join("Cargo.toml").exists()
            && current.join("copybook-cli").exists()
            && current.join("copybook-codec").exists()
        {
            return current;
        }

        if let Some(parent) = current.parent() {
            current = parent.to_path_buf();
        } else {
            panic!("Could not find workspace root");
        }
    }
}

/// Get the path to a fixture file relative to workspace root
pub fn fixture_path(relative_path: &str) -> PathBuf {
    find_workspace_root().join("fixtures").join(relative_path)
}

/// Create a copybook command with standard fixed format and CP037 codepage args
pub fn copybook_cmd(args: &[&str]) -> Command {
    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.args(args)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037");
    cmd
}
