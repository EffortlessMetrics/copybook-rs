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
    // with the typed identity in the `error_identity` machine field (#979):
    // no consumer extracts codes from prose. The domain-level `rejected`
    // mapping stays pinned by domain and BDD tests.
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
    assert_eq!(
        value["scenarios"][0]["error_identity"],
        "CBKP021_ODO_NOT_TAIL",
    );
}

#[test]
fn support_advise_json_invalid_for_nested_odo() {
    // #979 acceptance: a second real parse failure (CBKP022) proving the
    // identity is carried per error, not special-cased for CBKP021.
    const NESTED_ODO_COPYBOOK: &str = "       01 NESTED.\n           05 CNT-OUTER PIC 9(2).\n           05 OUTER-GRP OCCURS 0 TO 2 DEPENDING ON CNT-OUTER.\n               10 CNT-INNER PIC 9(2).\n               10 INNER-GRP OCCURS 0 TO 3 DEPENDING ON CNT-INNER.\n                   15 VAL PIC X.\n";
    let copybook = write_advise_copybook(NESTED_ODO_COPYBOOK);
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
    assert_eq!(
        value["scenarios"][0]["error_identity"],
        "CBKP022_NESTED_ODO",
    );
}

#[test]
fn support_advise_next_action_names_parser_real_flags() {
    // #979: remediation prose references the real framing flag, checked
    // against the actual CLI parser surface (`support --help`).
    let help = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args(["support", "--help"])
        .output()
        .expect("failed to execute command");
    let help_text = String::from_utf8_lossy(&help.stdout);
    assert!(
        help_text.contains("--record-format <RECORD_FORMAT>"),
        "parser must own --record-format, got: {help_text}"
    );

    let copybook = write_advise_copybook(TAIL_ODO_COPYBOOK);
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args([
            "support",
            "--advise",
            &copybook.path().to_string_lossy(),
            "--record-format",
            "rdw",
            "--format",
            "json",
        ])
        .output()
        .expect("failed to execute command");
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    let next_action = value["scenarios"][0]["next_action"]
        .as_str()
        .unwrap_or_default();
    assert!(
        next_action.contains("--record-format rdw"),
        "remediation must name the evaluated framing flag, got: {next_action}"
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
    // #978: the VB path has no tail-ODO ledger row, so the requested format
    // still echoes into `effective_options`/`record_formats` but the verdict
    // is partial-unknown (exit 3), never fixed-evidence certainty. The old
    // expectation (`supported`) certified the VB path from fixed-only
    // evidence and was the reported defect.
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

    assert_eq!(output.status.code(), Some(3));
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    assert_eq!(value["verdict"], "partial-unknown");
    assert_eq!(value["effective_options"]["format"], "vb");
    let scenarios = value["scenarios"].as_array().cloned().unwrap_or_default();
    // #980: ordinary fields sort before the ODO row; find it by ID.
    let row = scenarios
        .iter()
        .find(|s| s["scenario_id"] == "matrix:occurs-depending")
        .expect("VB ODO row");
    assert_eq!(row["status"], "unknown");
    assert_eq!(row["record_formats"], serde_json::json!(["vb"]));
    assert!(
        row["limitation_or_remediation"]
            .as_str()
            .unwrap_or_default()
            .contains("vb"),
        "VB must name the unevaluated format, got: {value}"
    );
}

#[test]
fn support_advise_rdw_tail_odo_resolves_rdw_row() {
    // #978 acceptance: the same tail-ODO copybook under RDW resolves to the
    // RDW ledger row with real evidence, still exiting 0.
    let copybook = write_advise_copybook(TAIL_ODO_COPYBOOK);
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .args([
            "support",
            "--advise",
            &copybook.path().to_string_lossy(),
            "--record-format",
            "rdw",
            "--format",
            "json",
        ])
        .output()
        .expect("failed to execute command");

    assert!(output.status.success());
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    assert_eq!(value["verdict"], "supported");
    let scenarios = value["scenarios"].as_array().cloned().unwrap_or_default();
    // #980: the copybook's ordinary counter field is accounted for too; find
    // the ODO row by ID instead of assuming it is alone.
    let row = scenarios
        .iter()
        .find(|s| s["scenario_id"] == "struct.odo.tail_rdw_variable")
        .expect("RDW tail-ODO row");
    assert_eq!(row["status"], "supported");
    assert_eq!(row["record_formats"], serde_json::json!(["rdw"]));
    assert!(
        row["affected_layers"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .any(|layer| layer == "decode"),
        "RDW row must carry ledger layers, got: {value}"
    );
    assert!(
        row["evidence_refs"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .any(|reference| reference
                .as_str()
                .unwrap_or_default()
                .contains("odo_variable_length_decodes_through_rdw")),
        "RDW row must carry ledger evidence, got: {value}"
    );
}

#[test]
fn support_advise_ordinary_two_field_copybook_is_supported() {
    // #980 acceptance: the ordinary `PIC 9(5)` plus `PIC X(5)` shape (also
    // the release-smoke shape) yields nonempty, evidence-backed results
    // instead of the historical empty assessment. Pre-fix this returned no
    // scenarios with a partial-unknown verdict.
    const TWO_FIELD_COPYBOOK: &str =
        "       01 REC.\n           05 NUM PIC 9(5).\n           05 NAME PIC X(5).\n";
    let copybook = write_advise_copybook(TWO_FIELD_COPYBOOK);
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

    assert!(output.status.success());
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    assert_eq!(value["verdict"], "supported");
    let scenarios = value["scenarios"].as_array().cloned().unwrap_or_default();
    assert_eq!(scenarios.len(), 2);
    let ids: Vec<&str> = scenarios
        .iter()
        .map(|s| s["scenario_id"].as_str().unwrap_or_default())
        .collect();
    assert!(ids.contains(&"struct.field.alphanumeric"), "got: {ids:?}");
    assert!(
        ids.contains(&"struct.field.display_numeric"),
        "got: {ids:?}"
    );
    for scenario in &scenarios {
        assert_eq!(scenario["status"], "supported");
        assert!(
            !scenario["evidence_refs"]
                .as_array()
                .unwrap_or(&vec![])
                .is_empty(),
            "ordinary rows must carry evidence, got: {scenario}"
        );
        assert!(
            scenario["limitation_or_remediation"]
                .as_str()
                .unwrap_or_default()
                .contains("never validates unseen record payloads"),
            "copybook-only limit must be explicit, got: {scenario}"
        );
    }
}

#[test]
fn support_advise_numeric_fields_are_supported() {
    // #980 second family: COMP binary and COMP-3 packed fields in one
    // copybook resolve to their ledger rows with evidence and explicit
    // copybook-only limits.
    const NUMERIC_COPYBOOK: &str = "       01 REC.\n           05 COUNT PIC 9(4) COMP.\n           05 AMT PIC S9(7)V99 COMP-3.\n";
    let copybook = write_advise_copybook(NUMERIC_COPYBOOK);
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

    assert!(output.status.success());
    let value: serde_json::Value = serde_json::from_slice(&output.stdout)
        .expect("support --advise --format json should emit valid JSON");
    assert_eq!(value["verdict"], "supported");
    let scenarios = value["scenarios"].as_array().cloned().unwrap_or_default();
    assert_eq!(scenarios.len(), 2);
    for (id, marker) in [
        ("struct.field.binary_int", "Big-endian byte order"),
        (
            "struct.field.packed_decimal",
            "never validates unseen record payloads",
        ),
    ] {
        let row = scenarios
            .iter()
            .find(|s| s["scenario_id"] == id)
            .unwrap_or_else(|| panic!("expected row {id}"));
        assert_eq!(row["status"], "supported");
        assert!(
            !row["evidence_refs"]
                .as_array()
                .unwrap_or(&vec![])
                .is_empty(),
            "row {id} must carry evidence"
        );
        assert!(
            row["limitation_or_remediation"]
                .as_str()
                .unwrap_or_default()
                .contains(marker),
            "row {id} must state its limit, got: {row}"
        );
    }
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
    let value: serde_json::Value = serde_json::from_str(&stdout).expect("advise JSON must parse");
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
    let _: serde_json::Value = serde_json::from_str(&stdout).expect("advise JSON must parse");
    assert!(
        !stdout.contains(&*path),
        "machine output must not leak the input path: {stdout}"
    );
}
