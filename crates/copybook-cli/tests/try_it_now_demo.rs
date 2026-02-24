// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration test for README.md "Try It Now" demo
//!
//! This test verifies that the demo command shown in the README works as expected:
//! ```bash
//! ./target/release/copybook decode \
//!   fixtures/copybooks/simple.cpy \
//!   fixtures/data/simple.bin \
//!   --format fixed --codepage cp037 \
//!   --output demo.jsonl
//! ```

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use assert_cmd::cargo::cargo_bin_cmd;
use common::{TestResult, write_file};
use predicates::prelude::*;
use std::fs;
use std::path::PathBuf;
use tempfile::tempdir;

/// Find the workspace root by looking for Cargo.toml
fn find_workspace_root() -> TestResult<PathBuf> {
    let mut current = std::env::current_dir()?;

    loop {
        if current.join("Cargo.toml").exists()
            && current.join("copybook-cli").exists()
            && current.join("fixtures").exists()
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

#[test]
fn try_it_now_demo_decode_succeeds() -> TestResult<()> {
    let workspace_root = find_workspace_root()?;
    let copybook_path = workspace_root.join("fixtures/copybooks/simple.cpy");
    let data_path = workspace_root.join("fixtures/data/simple.bin");

    // Verify fixtures exist
    assert!(
        copybook_path.exists(),
        "simple.cpy fixture not found at {}",
        copybook_path.display()
    );
    assert!(
        data_path.exists(),
        "simple.bin fixture not found at {}",
        data_path.display()
    );

    // Create temp dir for output
    let tmp = tempdir()?;
    let output_path = tmp.path().join("demo.jsonl");

    // Run the exact command from README (minus ./target/release/ since we use cargo_bin_cmd)
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "decode",
        copybook_path.to_str().unwrap(),
        data_path.to_str().unwrap(),
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        "--output",
        output_path.to_str().unwrap(),
    ]);

    // Verify command succeeds (exit code 0)
    cmd.assert().success();

    // Verify output file was created
    assert!(
        output_path.exists(),
        "Output file demo.jsonl was not created"
    );

    // Verify output contains valid JSON with expected fields
    let output_content = fs::read_to_string(&output_path)?;

    // Should have exactly one line (one record in simple.bin)
    let lines: Vec<&str> = output_content.lines().collect();
    assert_eq!(
        lines.len(),
        1,
        "Expected exactly one JSON record, got {}",
        lines.len()
    );

    // Verify JSON is well-formed and contains expected fields from simple.cpy:
    // CUSTOMER-ID, CUSTOMER-NAME, ACCOUNT-BALANCE, LAST-ACTIVITY-DATE, STATUS-CODE
    let json_line = lines[0];

    // Basic JSON structure validation
    assert!(
        json_line.starts_with('{') && json_line.ends_with('}'),
        "Output is not valid JSON object: {}",
        json_line
    );

    // Verify expected fields are present (as shown in README example)
    assert!(
        json_line.contains("\"CUSTOMER-ID\""),
        "Missing CUSTOMER-ID field in output: {}",
        json_line
    );
    assert!(
        json_line.contains("\"CUSTOMER-NAME\""),
        "Missing CUSTOMER-NAME field in output: {}",
        json_line
    );
    assert!(
        json_line.contains("\"ACCOUNT-BALANCE\""),
        "Missing ACCOUNT-BALANCE field in output: {}",
        json_line
    );

    // Parse JSON to verify it's valid
    let parsed: serde_json::Value = serde_json::from_str(json_line)?;

    // Verify fields exist and have correct types
    assert!(
        parsed.get("CUSTOMER-ID").is_some(),
        "CUSTOMER-ID field missing in parsed JSON"
    );
    assert!(
        parsed.get("CUSTOMER-NAME").is_some(),
        "CUSTOMER-NAME field missing in parsed JSON"
    );
    assert!(
        parsed.get("ACCOUNT-BALANCE").is_some(),
        "ACCOUNT-BALANCE field missing in parsed JSON"
    );
    assert!(
        parsed.get("LAST-ACTIVITY-DATE").is_some(),
        "LAST-ACTIVITY-DATE field missing in parsed JSON"
    );
    assert!(
        parsed.get("STATUS-CODE").is_some(),
        "STATUS-CODE field missing in parsed JSON"
    );

    Ok(())
}

#[test]
fn try_it_now_demo_decode_with_output_flag_required() -> TestResult<()> {
    let workspace_root = find_workspace_root()?;
    let copybook_path = workspace_root.join("fixtures/copybooks/simple.cpy");
    let data_path = workspace_root.join("fixtures/data/simple.bin");

    // Verify fixtures exist
    assert!(copybook_path.exists(), "simple.cpy fixture not found");
    assert!(data_path.exists(), "simple.bin fixture not found");

    // Run decode without --output flag (should fail because --output is required)
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "decode",
        copybook_path.to_str().unwrap(),
        data_path.to_str().unwrap(),
        "--format",
        "fixed",
        "--codepage",
        "cp037",
    ]);

    // Should fail with message about missing --output
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("--output"));

    Ok(())
}

#[test]
fn try_it_now_demo_invalid_fixture_path_fails() -> TestResult<()> {
    let tmp = tempdir()?;
    let nonexistent_copybook = tmp.path().join("nonexistent.cpy");
    let nonexistent_data = tmp.path().join("nonexistent.bin");

    let output_path = tmp.path().join("output.jsonl");

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "decode",
        nonexistent_copybook.to_str().unwrap(),
        nonexistent_data.to_str().unwrap(),
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        "--output",
        output_path.to_str().unwrap(),
    ]);

    // Should fail with non-zero exit code
    cmd.assert().failure();

    Ok(())
}

#[test]
fn try_it_now_demo_output_file_is_valid_jsonl() -> TestResult<()> {
    let workspace_root = find_workspace_root()?;
    let copybook_path = workspace_root.join("fixtures/copybooks/simple.cpy");
    let data_path = workspace_root.join("fixtures/data/simple.bin");

    let tmp = tempdir()?;
    let output_path = tmp.path().join("output.jsonl");

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "decode",
        copybook_path.to_str().unwrap(),
        data_path.to_str().unwrap(),
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        "--output",
        output_path.to_str().unwrap(),
    ]);

    cmd.assert().success();

    // Read and validate JSONL format (one JSON object per line)
    let content = fs::read_to_string(&output_path)?;
    for (line_num, line) in content.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }

        // Each line should parse as valid JSON
        serde_json::from_str::<serde_json::Value>(line).map_err(|e| {
            format!(
                "Line {} is not valid JSON: {} (error: {})",
                line_num + 1,
                line,
                e
            )
        })?;
    }

    Ok(())
}

#[test]
fn try_it_now_demo_missing_required_args_fails() -> TestResult<()> {
    let workspace_root = find_workspace_root()?;
    let copybook_path = workspace_root.join("fixtures/copybooks/simple.cpy");
    let data_path = workspace_root.join("fixtures/data/simple.bin");

    // Missing --format flag
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "decode",
        copybook_path.to_str().unwrap(),
        data_path.to_str().unwrap(),
        "--codepage",
        "cp037",
    ]);

    cmd.assert().failure();

    // Missing --codepage flag
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "decode",
        copybook_path.to_str().unwrap(),
        data_path.to_str().unwrap(),
        "--format",
        "fixed",
    ]);

    cmd.assert().failure();

    Ok(())
}

#[test]
fn try_it_now_demo_preserves_output_file_on_success() -> TestResult<()> {
    let workspace_root = find_workspace_root()?;
    let copybook_path = workspace_root.join("fixtures/copybooks/simple.cpy");
    let data_path = workspace_root.join("fixtures/data/simple.bin");

    let tmp = tempdir()?;
    let output_path = tmp.path().join("persistent_output.jsonl");

    // Create output file with initial content
    write_file(&output_path, b"initial content")?;
    assert!(output_path.exists(), "Output file should exist initially");

    // Run decode (should overwrite)
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "decode",
        copybook_path.to_str().unwrap(),
        data_path.to_str().unwrap(),
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        "--output",
        output_path.to_str().unwrap(),
    ]);

    cmd.assert().success();

    // Verify output file still exists and contains JSON
    assert!(
        output_path.exists(),
        "Output file should exist after successful decode"
    );

    let content = fs::read_to_string(&output_path)?;
    assert!(
        content.contains("\"CUSTOMER-ID\""),
        "Output file should contain decoded JSON"
    );
    assert!(
        !content.contains("initial content"),
        "Output file should not contain initial content"
    );

    Ok(())
}

#[test]
fn try_it_now_demo_decode_to_stdout() -> TestResult<()> {
    let workspace_root = find_workspace_root()?;
    let copybook_path = workspace_root.join("fixtures/copybooks/simple.cpy");
    let data_path = workspace_root.join("fixtures/data/simple.bin");

    // Verify fixtures exist
    assert!(
        copybook_path.exists(),
        "simple.cpy fixture not found at {}",
        copybook_path.display()
    );
    assert!(
        data_path.exists(),
        "simple.bin fixture not found at {}",
        data_path.display()
    );

    // Run decode with --output - (stdout)
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "decode",
        copybook_path.to_str().unwrap(),
        data_path.to_str().unwrap(),
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        "--output",
        "-",
    ]);

    // Verify command succeeds and outputs JSON to stdout
    let output = cmd.assert().success().get_output().stdout.clone();
    let stdout_content = String::from_utf8(output)?;

    // Should have JSON output (one record in simple.bin)
    let lines: Vec<&str> = stdout_content.lines().collect();
    assert!(
        !lines.is_empty(),
        "Expected JSON output on stdout, got empty"
    );

    // Find JSON line (should be first non-empty line)
    let json_line = lines
        .iter()
        .find(|line| line.trim().starts_with('{'))
        .expect("No JSON object found in stdout");

    // Verify JSON is well-formed
    assert!(
        json_line.starts_with('{') && json_line.ends_with('}'),
        "Output is not valid JSON object: {}",
        json_line
    );

    // Parse JSON to verify it's valid
    let parsed: serde_json::Value = serde_json::from_str(json_line)?;

    // Verify expected fields are present
    assert!(
        parsed.get("CUSTOMER-ID").is_some(),
        "CUSTOMER-ID field missing in parsed JSON"
    );
    assert!(
        parsed.get("CUSTOMER-NAME").is_some(),
        "CUSTOMER-NAME field missing in parsed JSON"
    );

    // Verify summary is NOT present in stdout when writing to stdout
    assert!(
        !stdout_content.contains("=== Decode Summary ==="),
        "Summary should not be present when outputting to stdout"
    );

    Ok(())
}
