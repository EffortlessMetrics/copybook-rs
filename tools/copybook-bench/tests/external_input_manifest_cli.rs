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
            "fixed-one.json",
        ],
    )?;
    ensure!(
        output.status.success(),
        "generator failed: {:?}",
        output.stderr
    );
    let second = run_generator(
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
            "fixed-two.json",
        ],
    )?;
    ensure!(second.status.success(), "second generator failed");
    ensure!(
        fs::read(root.path().join("fixed-one.json"))?
            == fs::read(root.path().join("fixed-two.json"))?,
        "repeated generation must be byte-identical"
    );
    let manifest: Value = serde_json::from_slice(&fs::read(root.path().join("fixed-one.json"))?)?;
    ensure!(manifest["schema_version"] == "1.0.0");
    ensure!(manifest["record_count"] == 2);
    ensure!(manifest["record_length"] == 4);
    ensure!(manifest["record_format"] == "fixed");
    ensure!(manifest["copybook"] == "record.cpy");
    ensure!(manifest["dataset"] == "fixed.bin");
    ensure!(
        manifest["copybook_sha256"]
            == "f3f801e797c679c0910c4428e5e5144dea3df67133218fb8a9e12d081423d116"
    );
    ensure!(
        manifest["dataset_sha256"]
            == "7bdee4c4987c1b91a0c9d619e16441d2914f2f5582b012e219903f5c84a8e18b"
    );
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
    let mut tampered = manifest.clone();
    tampered["dataset_sha256"] = Value::from("0".repeat(64));
    fs::write(
        root.path().join("tampered.json"),
        serde_json::to_vec_pretty(&tampered)?,
    )?;
    let preflight = Command::new(env!("CARGO_BIN_EXE_external-input-preflight"))
        .args(["tampered.json", "tampered-report.json"])
        .current_dir(root.path())
        .env("GITHUB_SHA", "0123456789abcdef0123456789abcdef01234567")
        .output()?;
    ensure!(
        !preflight.status.success(),
        "tampered digest must fail closed"
    );
    ensure!(!root.path().join("tampered-report.json").exists());
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
    ensure!(
        manifest["dataset_sha256"]
            == "8d8a56b0b5d8183d2b53dc9e903f2ae69584f79df1107a60a1dfaecd711045e2"
    );
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

    fs::write(root.path().join("bad-rdw.bin"), [0, 4, 0, 0, b'A', b'B'])?;
    let malformed_rdw = run_generator(
        root.path(),
        &[
            "--copybook",
            "record.cpy",
            "--dataset",
            "bad-rdw.bin",
            "--format",
            "rdw",
            "--codepage",
            "ascii",
            "--workload",
            "mixed",
            "--record-length",
            "4",
            "--output",
            "bad-rdw.json",
        ],
    )?;
    ensure!(!malformed_rdw.status.success());

    let missing = run_generator(
        root.path(),
        &[
            "--copybook",
            "record.cpy",
            "--dataset",
            "missing.bin",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--workload",
            "mixed",
            "--record-length",
            "4",
            "--output",
            "missing.json",
        ],
    )?;
    ensure!(!missing.status.success());

    let unsupported = run_generator(
        root.path(),
        &[
            "--copybook",
            "record.cpy",
            "--dataset",
            "bad.bin",
            "--format",
            "fixed",
            "--codepage",
            "bogus",
            "--workload",
            "mixed",
            "--record-length",
            "4",
            "--output",
            "unsupported.json",
        ],
    )?;
    ensure!(!unsupported.status.success());

    fs::write(root.path().join("empty.bin"), [])?;
    let zero_count = run_generator(
        root.path(),
        &[
            "--copybook",
            "record.cpy",
            "--dataset",
            "empty.bin",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--workload",
            "mixed",
            "--record-length",
            "4",
            "--output",
            "empty.json",
        ],
    )?;
    ensure!(!zero_count.status.success());

    fs::write(root.path().join("good.bin"), b"ABCD")?;
    let output_alias = run_generator(
        root.path(),
        &[
            "--copybook",
            "record.cpy",
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
            "good.bin",
        ],
    )?;
    ensure!(!output_alias.status.success());
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
