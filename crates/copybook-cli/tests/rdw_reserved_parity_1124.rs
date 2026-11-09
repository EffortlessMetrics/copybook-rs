// SPDX-License-Identifier: AGPL-3.0-or-later
//! Process-level RDW reserved-byte parity lock for #1124.
//!
//! The library-level parity (bulk Decode, `RecordIterator`, Verify loop,
//! occurrence Explain, determinism gate) is locked in
//! `copybook-codec/tests/rdw_reserved_parity_1124.rs`. These tests lock the
//! process boundary for the same single fixture: one non-zero-reserved RDW
//! record must surface the stable `CBKR211_RDW_RESERVED_NONZERO` identity
//! with record index and reserved-byte offset through the `decode`,
//! `verify`, and `explain` subcommands, with the taxonomy-correct exit
//! code on each path, while lenient mode continues on every path.
//!
//! Subprocesses are spawned via the compile-time
//! `env!("CARGO_BIN_EXE_copybook")` binary identity only.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use common::{TestResult, write_file};
use serial_test::serial;
use std::process::Command;
use tempfile::{NamedTempFile, TempDir};

/// Eight-byte layout the fixture payload satisfies exactly.
const COPYBOOK: &str = "01 RECORD.\n 05 FIELD-A PIC X(8).\n";

/// One RDW record: length 8, reserved `0x0001`, payload `RECORD01`.
fn reserved_frame() -> Vec<u8> {
    let mut frame = vec![0x00u8, 0x08, 0x00, 0x01];
    frame.extend_from_slice(b"RECORD01");
    frame
}

fn fixture() -> TestResult<(NamedTempFile, NamedTempFile)> {
    let copybook = NamedTempFile::new()?;
    write_file(copybook.path(), COPYBOOK)?;
    let input = NamedTempFile::new()?;
    write_file(input.path(), reserved_frame())?;
    Ok((copybook, input))
}

#[test]
#[serial]
fn decode_strict_reports_reserved_identity_with_format_exit() -> TestResult<()> {
    let (copybook, input) = fixture()?;
    let output_dir = TempDir::new()?;
    let output_path = output_dir.path().join("out.jsonl");

    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("decode")
        .arg("--format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--strict")
        .arg("--output")
        .arg(&output_path)
        .arg(copybook.path())
        .arg(input.path())
        .output()?;

    let code = output.status.code().unwrap_or(1);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        code, 4,
        "CBKR framing failures must map to exit code 4, got {code} with stderr:\n{stderr}"
    );
    assert!(
        stderr.contains("CBKR211_RDW_RESERVED_NONZERO"),
        "stderr must name the stable reserved identity, got:\n{stderr}"
    );
    assert!(
        stderr.contains("record 1"),
        "stderr must name record 1, got:\n{stderr}"
    );
    assert!(
        stderr.contains("offset 2"),
        "stderr must name reserved-byte offset 2, got:\n{stderr}"
    );
    Ok(())
}

#[test]
#[serial]
fn decode_lenient_continues_and_decodes_record() -> TestResult<()> {
    let (copybook, input) = fixture()?;
    let output_dir = TempDir::new()?;
    let output_path = output_dir.path().join("out.jsonl");

    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("decode")
        .arg("--format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--output")
        .arg(&output_path)
        .arg(copybook.path())
        .arg(input.path())
        .output()?;

    let code = output.status.code().unwrap_or(1);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        code, 0,
        "lenient decode must continue past non-zero reserved bytes, got {code} with stderr:\n{stderr}"
    );
    let decoded = std::fs::read_to_string(&output_path)?;
    assert_eq!(
        decoded.lines().count(),
        1,
        "lenient decode must emit the single record, got:\n{decoded}"
    );
    assert!(
        decoded.contains("RECORD01"),
        "lenient decode must emit the payload, got:\n{decoded}"
    );
    Ok(())
}

#[test]
#[serial]
fn verify_strict_reports_reserved_identity_with_validation_exit() -> TestResult<()> {
    let (copybook, input) = fixture()?;
    let output_dir = TempDir::new()?;
    let report_path = output_dir.path().join("report.json");

    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("verify")
        .arg("--format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--strict")
        .arg("--report")
        .arg(&report_path)
        .arg(copybook.path())
        .arg(input.path())
        .output()?;

    let code = output.status.code().unwrap_or(1);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        code, 3,
        "verify with validation errors must map to exit code 3, got {code} with stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("CBKR211_RDW_RESERVED_NONZERO"),
        "verify output must name the stable reserved identity, got:\n{stdout}"
    );
    assert!(
        stdout.contains("record byte offset 2"),
        "verify output must name reserved-byte offset 2, got:\n{stdout}"
    );
    let report = std::fs::read_to_string(&report_path)?;
    let json: serde_json::Value = serde_json::from_str(&report)?;
    let errors = json
        .get("errors")
        .and_then(|value| value.as_array())
        .ok_or("verify report must carry an errors array")?;
    let first = errors
        .first()
        .ok_or("verify report must list the framing failure")?;
    assert_eq!(
        first.get("code").and_then(serde_json::Value::as_str),
        Some("CBKR211_RDW_RESERVED_NONZERO")
    );
    assert_eq!(
        first.get("offset").and_then(serde_json::Value::as_u64),
        Some(2)
    );
    Ok(())
}

#[test]
#[serial]
fn verify_lenient_passes() -> TestResult<()> {
    let (copybook, input) = fixture()?;

    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("verify")
        .arg("--format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg(copybook.path())
        .arg(input.path())
        .output()?;

    let code = output.status.code().unwrap_or(1);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        code, 0,
        "lenient verify must pass, got {code} with stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("PASS"),
        "lenient verify must report PASS, got:\n{stdout}"
    );
    Ok(())
}

#[test]
#[serial]
fn explain_strict_locates_reserved_identity() -> TestResult<()> {
    let (copybook, input) = fixture()?;

    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("explain")
        .arg("--copybook")
        .arg(copybook.path())
        .arg("--input")
        .arg(input.path())
        .arg("--record-format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--strict")
        .output()?;

    let code = output.status.code().unwrap_or(1);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        code, 0,
        "occurrence explain renders the card with exit 0, got {code} with stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("CBKR211_RDW_RESERVED_NONZERO"),
        "explain must name the stable reserved identity, got:\n{stdout}"
    );
    assert!(
        stdout.contains("Record 1"),
        "explain must locate record 1, got:\n{stdout}"
    );

    // The same identity reached through the explicit code filter.
    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("explain")
        .arg("CBKR211_RDW_RESERVED_NONZERO")
        .arg("--copybook")
        .arg(copybook.path())
        .arg("--input")
        .arg(input.path())
        .arg("--record-format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--strict")
        .output()?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("CBKR211_RDW_RESERVED_NONZERO"),
        "filtered explain must locate the reserved failure, got:\n{stdout}"
    );
    Ok(())
}

#[test]
#[serial]
fn explain_lenient_finds_no_failure() -> TestResult<()> {
    let (copybook, input) = fixture()?;

    let output = Command::new(env!("CARGO_BIN_EXE_copybook"))
        .arg("explain")
        .arg("--copybook")
        .arg(copybook.path())
        .arg("--input")
        .arg(input.path())
        .arg("--record-format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .output()?;

    let code = output.status.code().unwrap_or(1);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        code, 0,
        "lenient explain must exit 0, got {code} with stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("No matching failure"),
        "lenient explain must report a clean scan, got:\n{stdout}"
    );
    Ok(())
}
