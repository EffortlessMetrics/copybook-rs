// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fs;
use std::process::Command;

use anyhow::{Context, Result, ensure};
use serde_json::Value;
use tempfile::tempdir;

fn run_generator(root: &std::path::Path, args: &[&str]) -> Result<std::process::Output> {
    Command::new(env!("CARGO_BIN_EXE_gen-external-input-manifest"))
        .args(args)
        .current_dir(root)
        .output()
        .context("failed to launch manifest generator")
}

fn write_copybook(root: &std::path::Path) -> Result<()> {
    fs::write(root.join("record.cpy"), "01 REC.\n   05 FIELD PIC X(4).\n")?;
    Ok(())
}

#[test]
fn fixed_manifest_contains_deterministic_integrity_and_shape() -> Result<()> {
    let root = tempdir()?;
    write_copybook(root.path())?;
    fs::write(root.path().join("fixed.bin"), b"ABCDWXYZ")?;
    let output = run_generator(
        root.path(),
        &[
            "--copybook",
            "record.cpy",
            "--dataset",
            "fixed.bin",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--workload",
            "display-heavy",
            "--record-length",
            "4",
            "--output",
            "fixed.json",
        ],
    )?;
    ensure!(
        output.status.success(),
        "generator failed: {:?}",
        output.stderr
    );
    let manifest: Value = serde_json::from_slice(&fs::read(root.path().join("fixed.json"))?)?;
    ensure!(manifest["schema_version"] == "1.0.0");
    ensure!(manifest["record_count"] == 2);
    ensure!(manifest["record_length"] == 4);
    ensure!(manifest["record_format"] == "fixed");
    ensure!(manifest["copybook"] == "record.cpy");
    ensure!(manifest["dataset"] == "fixed.bin");
    ensure!(
        manifest["copybook_sha256"]
            .as_str()
            .is_some_and(|value| value.len() == 64)
    );
    ensure!(
        manifest["dataset_sha256"]
            .as_str()
            .is_some_and(|value| value.len() == 64)
    );
    Ok(())
}

#[test]
fn rdw_manifest_counts_payload_records() -> Result<()> {
    let root = tempdir()?;
    write_copybook(root.path())?;
    let mut dataset = Vec::new();
    for payload in [b"ABCD".as_slice(), b"WXYZ".as_slice()] {
        dataset.extend_from_slice(&(payload.len() as u16).to_be_bytes());
        dataset.extend_from_slice(&[0, 0]);
        dataset.extend_from_slice(payload);
    }
    fs::write(root.path().join("rdw.bin"), dataset)?;
    let output = run_generator(
        root.path(),
        &[
            "--copybook",
            "record.cpy",
            "--dataset",
            "rdw.bin",
            "--format",
            "rdw",
            "--codepage",
            "cp037",
            "--workload",
            "mixed",
            "--record-length",
            "4",
            "--output",
            "rdw.json",
        ],
    )?;
    ensure!(
        output.status.success(),
        "generator failed: {:?}",
        output.stderr
    );
    let manifest: Value = serde_json::from_slice(&fs::read(root.path().join("rdw.json"))?)?;
    ensure!(manifest["record_count"] == 2);
    ensure!(manifest["record_format"] == "rdw");
    ensure!(manifest["codepage"] == "cp037");
    Ok(())
}

#[test]
fn malformed_dataset_and_unsafe_input_are_rejected() -> Result<()> {
    let root = tempdir()?;
    write_copybook(root.path())?;
    fs::write(root.path().join("bad.bin"), b"ABCDE")?;
    let malformed = run_generator(
        root.path(),
        &[
            "--copybook",
            "record.cpy",
            "--dataset",
            "bad.bin",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--workload",
            "mixed",
            "--record-length",
            "4",
            "--output",
            "bad.json",
        ],
    )?;
    ensure!(!malformed.status.success());

    fs::write(root.path().join("good.bin"), b"ABCD")?;
    let outside_copybook = root
        .path()
        .parent()
        .context("temporary directory has no parent")?
        .join("outside-record.cpy");
    fs::write(&outside_copybook, "01 OUTSIDE.\n")?;
    let unsafe_input = run_generator(
        root.path(),
        &[
            "--copybook",
            "../outside-record.cpy",
            "--dataset",
            "good.bin",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--workload",
            "mixed",
            "--record-length",
            "4",
            "--output",
            "unsafe.json",
        ],
    )?;
    ensure!(!unsafe_input.status.success());
    fs::remove_file(outside_copybook)?;
    Ok(())
}
