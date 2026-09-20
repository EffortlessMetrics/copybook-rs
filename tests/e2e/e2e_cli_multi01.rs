// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests pinning the documented multi-01 concatenation contract.
//!
//! A copybook declaring several 01-level layouts decodes as one
//! concatenated layout (offsets accumulate across 01s); no per-record
//! layout selection exists. See `docs/evidence/differential-breadth/README.md`
//! lane 5 for the evidenced JRecord overlay pole.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use serde_json::Value;
use tempfile::TempDir;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

fn workspace_path(rel: &str) -> std::path::PathBuf {
    // copybook-e2e lives at tests/e2e; fixtures live at the workspace root.
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(rel)
}

/// `multi01.cpy`: `A-REC` with `A-FIELD X(4)`, `B-REC` with `B-FIELD 9(4)`.
#[test]
fn multi01_decodes_as_one_concatenated_layout() {
    let dir = TempDir::new().unwrap();
    let data = dir.path().join("data.bin");
    std::fs::write(&data, b"ABCD0001EFGH0002").unwrap();
    let out = dir.path().join("out.jsonl");
    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&out)
        .arg(workspace_path(
            "docs/evidence/differential-breadth/multi01.cpy",
        ))
        .arg(&data)
        .assert()
        .success();
    let text = std::fs::read_to_string(&out).unwrap();
    let records: Vec<Value> = text
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    assert_eq!(records.len(), 2);
    // Offsets accumulate: B-FIELD reads bytes 4..8, not an overlay at 0.
    assert_eq!(records[0]["A-FIELD"], Value::String("ABCD".to_string()));
    assert_eq!(records[0]["B-FIELD"], Value::String("0001".to_string()));
    assert_eq!(records[1]["A-FIELD"], Value::String("EFGH".to_string()));
    assert_eq!(records[1]["B-FIELD"], Value::String("0002".to_string()));
}

#[test]
fn multi01_inspect_reports_concatenated_lrecl() {
    let assertion = cmd()
        .arg("inspect")
        .arg(workspace_path(
            "docs/evidence/differential-breadth/multi01.cpy",
        ))
        .assert();
    let output = assertion.get_output();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Fixed LRECL: 8 bytes"),
        "inspect reports the concatenated width: {stdout}"
    );
}
