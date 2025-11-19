//! Integration tests for the `copybook support` command
//!
//! Tests the feature support matrix CLI functionality including:
//! - Table output
//! - JSON output
//! - Feature checking with exit codes

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use std::process::{Command, Stdio};

#[test]
fn support_table_prints_known_features() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("support")
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Feature"));
    assert!(stdout.contains("Status"));
    assert!(stdout.contains("Description"));
    assert!(stdout.contains("LEVEL 88"));
    assert!(stdout.contains("Supported")); // At least one feature should be supported
}

#[test]
fn support_json_outputs_valid_json() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--format", "json"])
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());

    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --format json should emit valid JSON");

    assert!(value.is_array(), "JSON output should be an array");
    assert!(
        value
            .as_array()
            .unwrap()
            .iter()
            .any(|v| v["id"] == "level-88"),
        "expected level-88 feature in JSON output",
    );
}

#[test]
fn support_check_supported_feature_exits_zero() {
    let status = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--check", "level-88"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("failed to execute command");
    assert!(status.success(), "checking supported feature should exit 0");
}

#[test]
fn support_check_partial_feature_exits_nonzero() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--check", "level-66-renames"])
        .output()
        .expect("failed to execute command");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("not fully supported"));
}

#[test]
fn support_check_unknown_feature_exits_nonzero() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--check", "no-such-feature"])
        .output()
        .expect("failed to execute command");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Unknown feature ID"));
}

#[test]
fn support_table_includes_usage_hints() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("support")
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("copybook support --check"));
    assert!(stdout.contains("copybook support --format json"));
}

#[test]
fn support_json_includes_all_status_types() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--format", "json"])
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());

    let features: Vec<serde_json::Value> = serde_json::from_slice(&output.stdout)
        .expect("support --format json should emit valid JSON array");

    // Verify we have examples of different status types
    let statuses: Vec<&str> = features
        .iter()
        .filter_map(|f| f["status"].as_str())
        .collect();

    assert!(
        statuses.contains(&"supported"),
        "should have at least one supported feature"
    );
    assert!(
        statuses.contains(&"partial") || statuses.contains(&"planned"),
        "should have at least one partial or planned feature"
    );
}

#[test]
fn support_check_planned_feature_exits_nonzero() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--check", "edited-pic"])
        .output()
        .expect("failed to execute command");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("not fully supported"));
    assert!(stderr.contains("Planned"));
}
