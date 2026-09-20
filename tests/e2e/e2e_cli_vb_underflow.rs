// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for multi-block VB strictness (`vb-multi-block-varied`, #1161).
//!
//! Policy, evidenced against JRecord 0.93.2 `-IFS Mainframe_VB_As_RECFMU`
//! (lane 1 in `docs/evidence/differential-breadth/README.md`): framing
//! holds across blocks, a short payload is a classified per-record
//! `CBKF221` (never silent acceptance), and hostile framing (truncated
//! BDW, escaping RDW) is a classified framing error that never consumes
//! bytes past a block end.

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

fn bdw(total_len: u16) -> Vec<u8> {
    let mut out = total_len.to_be_bytes().to_vec();
    out.extend_from_slice(&[0, 0]);
    out
}

fn rdw(total_len: u16) -> Vec<u8> {
    let mut out = total_len.to_be_bytes().to_vec();
    out.extend_from_slice(&[0, 0]);
    out
}

/// mini.cpy is a 14-byte layout (`REC-ID 9(4)`, `REC-NAME X(10)`).
fn mini_record(id: &[u8; 4], name: &[u8; 10]) -> Vec<u8> {
    let mut out = Vec::with_capacity(14);
    out.extend_from_slice(id);
    out.extend_from_slice(name);
    out
}

/// Three single-record blocks; the middle payload is 4 bytes against a
/// 14-byte schema.
fn varied_blocks() -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(&bdw(22));
    out.extend_from_slice(&rdw(18));
    out.extend_from_slice(&mini_record(b"0001", b"ALPHA     "));
    out.extend_from_slice(&bdw(12));
    out.extend_from_slice(&rdw(8));
    out.extend_from_slice(b"0003");
    out.extend_from_slice(&bdw(22));
    out.extend_from_slice(&rdw(18));
    out.extend_from_slice(&mini_record(b"0002", b"BETA      "));
    out
}

fn decode_vb(dir: &TempDir, data: &[u8]) -> (assert_cmd::assert::Assert, std::path::PathBuf) {
    let input = dir.path().join("in.vb");
    std::fs::write(&input, data).unwrap();
    let out = dir.path().join("out.jsonl");
    let assertion = cmd()
        .args([
            "decode",
            "--format",
            "vb",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&out)
        .arg(workspace_path("fixtures/corpus/mini.cpy"))
        .arg(&input)
        .assert();
    (assertion, out)
}

#[test]
fn varied_blocks_short_middle_record_is_per_record_error() {
    let dir = TempDir::new().unwrap();
    let (assertion, out) = decode_vb(&dir, &varied_blocks());
    let output = assertion.get_output();
    assert_eq!(output.status.code(), Some(2), "data error exits 2");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("CBKF221"),
        "short payload stays classified: {stderr}"
    );
    assert!(
        stderr.contains('4') && stderr.contains("14"),
        "error names actual (4) and schema (14) lengths: {stderr}"
    );
    // Framing holds across blocks: records 1 and 3 still decode.
    let text = std::fs::read_to_string(&out).unwrap();
    let names: Vec<String> = text
        .lines()
        .filter_map(|line| serde_json::from_str::<Value>(line).ok())
        .filter_map(|value| value.get("REC-NAME")?.as_str().map(str::to_string))
        .collect();
    assert_eq!(
        names,
        vec!["ALPHA     ".to_string(), "BETA      ".to_string()]
    );
}

#[test]
fn truncated_bdw_is_classified_framing_error() {
    // BDW declares 22 bytes; only 6 payload bytes follow before EOF.
    let mut data = bdw(22);
    data.extend_from_slice(&rdw(8));
    data.extend_from_slice(b"AB");
    let dir = TempDir::new().unwrap();
    let (assertion, _) = decode_vb(&dir, &data);
    let output = assertion.get_output();
    assert_eq!(output.status.code(), Some(4), "framing error exits 4");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("CBKF223"),
        "truncated block stays classified: {stderr}"
    );
}

#[test]
fn escaping_rdw_never_reads_past_block_end() {
    // BDW spans 12 bytes; the RDW declares 18.
    let mut data = bdw(12);
    data.extend_from_slice(&rdw(18));
    data.extend_from_slice(b"ABCDEFGH");
    let dir = TempDir::new().unwrap();
    let (assertion, _) = decode_vb(&dir, &data);
    let output = assertion.get_output();
    assert_eq!(output.status.code(), Some(4), "framing error exits 4");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("CBKF224"),
        "escaping record stays classified: {stderr}"
    );
    assert!(
        stderr.contains("18") && stderr.contains('8'),
        "error names declared (18) and remaining (8) lengths: {stderr}"
    );
}
