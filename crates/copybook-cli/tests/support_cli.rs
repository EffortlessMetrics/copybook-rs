// SPDX-License-Identifier: AGPL-3.0-or-later
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
    // Status is spelled the way `--status` and the JSON `status` field spell it.
    assert!(stdout.contains("supported")); // At least one feature should be supported
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
    assert!(stderr.contains("unknown feature ID"));
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
fn support_json_with_governance_outputs_runtime_fields() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--format", "json", "--with-governance"])
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());

    let values: Vec<serde_json::Value> = serde_json::from_slice(&output.stdout)
        .expect("support --format json --with-governance should emit valid JSON array");

    assert!(!values.is_empty());
    assert!(
        values
            .iter()
            .all(|value| value.get("runtime_enabled").is_some()),
        "runtime fields must be present when governance is requested",
    );
}

#[test]
fn support_check_with_governance_includes_runtime_flags() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--check", "level-88", "--with-governance"])
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Runtime-Available"));
    assert!(stdout.contains("Required Feature Flags"));
}

#[test]
fn support_check_partial_feature_exits_nonzero_nested_odo() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--check", "nested-odo"])
        .output()
        .expect("failed to execute command");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("not fully supported"));
    assert!(stderr.contains("partial"));
}

fn write_advise_copybook(contents: &str) -> tempfile::NamedTempFile {
    let file = tempfile::NamedTempFile::with_suffix(".cpy").expect("temp copybook");
    std::fs::write(file.path(), contents).expect("write temp copybook");
    file
}

const TAIL_ODO_COPYBOOK: &str = "       01 TAIL.\n           05 CNT PIC 9(2).\n           05 DATA PIC X(4) OCCURS 0 TO 4 DEPENDING ON CNT.\n";
const NON_TAIL_ODO_COPYBOOK: &str = "       01 NON-TAIL.\n           05 A PIC X.\n           05 TBL PIC X OCCURS 0 TO 3 DEPENDING ON N.\n           05 B PIC X.\n           05 N PIC 9(2).\n";

#[test]
fn support_advise_tail_odo_exits_zero() {
    let copybook = write_advise_copybook(TAIL_ODO_COPYBOOK);
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--advise", &copybook.path().to_string_lossy()])
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Advisory verdict: supported"));
    assert!(stdout.contains("struct.odo.tail_fixed"));
}

#[test]
fn support_advise_json_invalid_for_non_tail_odo() {
    // Non-tail ODO never parses (CBKP021), so advise reports invalid input
    // with the actionable parse identity in `next_action`; the domain-level
    // `rejected` mapping stays pinned by domain and BDD tests.
    let copybook = write_advise_copybook(NON_TAIL_ODO_COPYBOOK);
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args([
            "support",
            "--advise",
            &copybook.path().to_string_lossy(),
            "--format",
            "json",
        ])
        .output()
        .expect("failed to execute command");

    assert_eq!(output.status.code(), Some(3));
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    assert_eq!(value["verdict"], "invalid-input");
    assert_eq!(value["schema_version"], "1.0");
    assert!(
        value["scenarios"][0]["next_action"]
            .as_str()
            .unwrap_or_default()
            .contains("CBKP021_ODO_NOT_TAIL"),
        "expected CBKP021 identity in next_action, got: {value}"
    );
}

#[test]
fn support_advise_invalid_copybook_reports_invalid_input() {
    let copybook = write_advise_copybook("THIS DOES NOT LOOK LIKE\n");
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args([
            "support",
            "--advise",
            &copybook.path().to_string_lossy(),
            "--format",
            "json",
        ])
        .output()
        .expect("failed to execute command");

    assert_eq!(output.status.code(), Some(3));
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    assert_eq!(value["verdict"], "invalid-input");
}

#[test]
fn support_advise_missing_file_is_typed_error() {
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--advise", "/nonexistent/path/no-such.cpy"])
        .output()
        .expect("failed to execute command");

    assert_eq!(output.status.code(), Some(4));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("CBKF001"));
}

#[test]
fn support_advise_renames_reports_supported_with_limits() {
    let copybook = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../fixtures/copybooks/renames_r4_redefines.cpy");
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args([
            "support",
            "--advise",
            &copybook.to_string_lossy(),
            "--format",
            "json",
        ])
        .output()
        .expect("failed to execute command");

    assert_eq!(output.status.code(), Some(3));
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    assert_eq!(value["verdict"], "supported-with-limits");
    assert!(
        value["scenarios"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .any(|s| s["scenario_id"] == "struct.renames.r1_r3"),
        "expected renames scenario, got: {value}"
    );
}

#[test]
fn support_advise_vb_format_flows_into_effective_options() {
    let copybook = write_advise_copybook(TAIL_ODO_COPYBOOK);
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args([
            "support",
            "--advise",
            &copybook.path().to_string_lossy(),
            "--record-format",
            "vb",
            "--format",
            "json",
        ])
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    assert_eq!(value["verdict"], "supported");
    assert_eq!(value["effective_options"]["format"], "vb");
    assert!(
        value["scenarios"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .all(|s| s["record_formats"] == serde_json::json!(["vb"])),
        "expected vb record format on every scenario, got: {value}"
    );
}

#[test]
fn support_advise_corpus_nontail_odo_reports_invalid_input() {
    // The governed corpus fixture must stay in agreement with the inline
    // rejection contract: non-tail ODO never parses (CBKP021).
    let copybook = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../fixtures/corpus/nontail_odo.cpy");
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args([
            "support",
            "--advise",
            &copybook.to_string_lossy(),
            "--format",
            "json",
        ])
        .output()
        .expect("failed to execute command");

    assert_eq!(output.status.code(), Some(3));
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: serde_json::Value =
        serde_json::from_str(&stdout).expect("advise JSON must parse");
    assert_eq!(value["verdict"], "invalid-input");
    assert!(
        value["scenarios"][0]["next_action"]
            .as_str()
            .unwrap_or_default()
            .contains("CBKP021_ODO_NOT_TAIL"),
        "expected CBKP021 identity in next_action, got: {value}"
    );
}

#[test]
fn support_advise_json_emits_no_filesystem_paths() {
    // Redaction posture is a boundary invariant: machine output carries
    // ledger IDs, fingerprints, and bounded diagnostics, never the input
    // file path, even when the diagnostic text mentions the failure.
    let copybook = write_advise_copybook("THIS DOES NOT LOOK LIKE\n");
    let path = copybook.path().to_string_lossy().into_owned();
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--advise", &path, "--format", "json"])
        .output()
        .expect("failed to execute command");

    assert_eq!(output.status.code(), Some(3));
    let stdout = String::from_utf8_lossy(&output.stdout);
    let _: serde_json::Value =
        serde_json::from_str(&stdout).expect("advise JSON must parse");
    assert!(
        !stdout.contains(&*path),
        "machine output must not leak the input path: {stdout}"
    );
}
