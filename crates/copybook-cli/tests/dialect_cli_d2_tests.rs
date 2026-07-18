#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]

mod common;

use common::{TestResult, bin, write_file};
use predicates::str::contains;
use std::path::Path;
use tempfile::TempDir;

const CBKE: i32 = 3;

fn parse_args<'a>(
    cmd: &'a mut assert_cmd::Command,
    copybook_path: &std::path::Path,
    output_path: &std::path::Path,
) -> &'a mut assert_cmd::Command {
    cmd.arg("parse")
        .arg(copybook_path)
        .arg("--output")
        .arg(output_path)
}

fn inspect_args<'a>(
    cmd: &'a mut assert_cmd::Command,
    copybook_path: &std::path::Path,
) -> &'a mut assert_cmd::Command {
    cmd.arg("inspect").arg(copybook_path)
}

fn decode_args<'a>(
    cmd: &'a mut assert_cmd::Command,
    copybook_path: &std::path::Path,
    data_path: &std::path::Path,
    output_path: &std::path::Path,
) -> &'a mut assert_cmd::Command {
    cmd.args([
        "decode",
        "--format",
        "fixed",
        "--codepage",
        "ascii",
        "--preferred-zoned-encoding",
        "ascii",
    ])
    .arg(copybook_path)
    .arg(data_path)
    .arg("--output")
    .arg(output_path)
}

fn encode_args<'a>(
    cmd: &'a mut assert_cmd::Command,
    copybook_path: &std::path::Path,
    input_path: &std::path::Path,
    output_path: &std::path::Path,
) -> &'a mut assert_cmd::Command {
    cmd.args(["encode", "--codepage", "ascii", "--format", "fixed"])
        .arg(copybook_path)
        .arg(input_path)
        .arg("--output")
        .arg(output_path)
}

fn verify_args<'a>(
    cmd: &'a mut assert_cmd::Command,
    copybook_path: &std::path::Path,
    data_path: &std::path::Path,
) -> &'a mut assert_cmd::Command {
    cmd.args(["verify", "--format", "fixed", "--codepage", "ascii"])
        .arg(copybook_path)
        .arg(data_path)
}

fn parsed_tail_min_count(cmd: &mut assert_cmd::Command, copybook_path: &Path) -> TestResult<u64> {
    let output = cmd.arg("parse").arg(copybook_path).output()?;
    assert!(
        output.status.success(),
        "parse failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let schema: serde_json::Value = serde_json::from_slice(&output.stdout)?;
    schema["tail_odo"]["min_count"]
        .as_u64()
        .ok_or_else(|| "parse output did not contain tail_odo.min_count".into())
}

#[test]
fn parse_command_accepts_dialect_flag() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let output_path = tmp.path().join("output.json");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    parse_args(&mut cmd, &copybook_path, &output_path)
        .arg("--dialect")
        .arg("n")
        .assert()
        .success();

    Ok(())
}

#[test]
fn parse_command_accepts_zero_tolerant_dialect() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let output_path = tmp.path().join("output.json");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    parse_args(&mut cmd, &copybook_path, &output_path)
        .arg("--dialect")
        .arg("0")
        .assert()
        .success();

    Ok(())
}

#[test]
fn parse_command_accepts_one_tolerant_dialect() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let output_path = tmp.path().join("output.json");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    parse_args(&mut cmd, &copybook_path, &output_path)
        .arg("--dialect")
        .arg("1")
        .assert()
        .success();

    Ok(())
}

#[test]
fn parse_command_rejects_invalid_dialect() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let output_path = tmp.path().join("output.json");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    parse_args(&mut cmd, &copybook_path, &output_path)
        .arg("--dialect")
        .arg("invalid")
        .assert()
        .failure()
        .code(CBKE)
        .stderr(contains("error:"))
        .stderr(contains("--dialect"));

    Ok(())
}

#[test]
fn parse_command_default_is_normative() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let output_path = tmp.path().join("output.json");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    parse_args(&mut cmd, &copybook_path, &output_path)
        .assert()
        .success();

    Ok(())
}

#[test]
fn env_var_copybook_dialect() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let output_path = tmp.path().join("output.json");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    cmd.env("COPYBOOK_DIALECT", "0");
    parse_args(&mut cmd, &copybook_path, &output_path)
        .assert()
        .success();

    Ok(())
}

#[test]
fn cli_flag_overrides_env_var() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let output_path = tmp.path().join("output.json");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    cmd.env("COPYBOOK_DIALECT", "0");
    parse_args(&mut cmd, &copybook_path, &output_path)
        .arg("--dialect")
        .arg("1")
        .assert()
        .success();

    Ok(())
}

#[test]
fn dialect_cli_precedence_is_observable() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    write_file(
        &copybook_path,
        "01 RECORD.\n  05 COUNT PIC 9(2).\n  05 ITEMS OCCURS 5 TO 9 TIMES DEPENDING ON COUNT PIC X.\n",
    )?;

    let mut default_cmd = bin();
    assert_eq!(parsed_tail_min_count(&mut default_cmd, &copybook_path)?, 5);

    let mut env_cmd = bin();
    env_cmd.env("COPYBOOK_DIALECT", "0");
    assert_eq!(parsed_tail_min_count(&mut env_cmd, &copybook_path)?, 0);

    let mut invalid_env_cmd = bin();
    invalid_env_cmd.env("COPYBOOK_DIALECT", "unsupported");
    assert_eq!(
        parsed_tail_min_count(&mut invalid_env_cmd, &copybook_path)?,
        5
    );

    let zero_min_copybook = tmp.path().join("zero-min-schema.cpy");
    write_file(
        &zero_min_copybook,
        "01 RECORD.\n  05 COUNT PIC 9(2).\n  05 ITEMS OCCURS 0 TO 9 TIMES DEPENDING ON COUNT PIC X.\n",
    )?;
    let mut cli_cmd = bin();
    cli_cmd.env("COPYBOOK_DIALECT", "0");
    cli_cmd
        .arg("parse")
        .arg(&zero_min_copybook)
        .args(["--dialect", "1"]);
    let output = cli_cmd.output()?;
    assert!(
        output.status.success(),
        "parse failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let schema: serde_json::Value = serde_json::from_slice(&output.stdout)?;
    assert_eq!(schema["tail_odo"]["min_count"].as_u64(), Some(1));

    Ok(())
}

#[test]
fn inspect_command_accepts_dialect_flag() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    inspect_args(&mut cmd, &copybook_path)
        .arg("--dialect")
        .arg("n")
        .assert()
        .success();

    Ok(())
}

#[test]
fn decode_command_accepts_dialect_flag() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("output.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    decode_args(&mut cmd, &copybook_path, &data_path, &output_path)
        .arg("--dialect")
        .arg("n")
        .assert()
        .success();

    Ok(())
}

#[test]
fn encode_command_accepts_dialect_flag() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let input_path = tmp.path().join("input.jsonl");
    let output_path = tmp.path().join("output.bin");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&input_path, r#"{"FIELD":"DATA"}"#)?;

    let mut cmd = bin();
    encode_args(&mut cmd, &copybook_path, &input_path, &output_path)
        .arg("--dialect")
        .arg("n")
        .assert()
        .success();

    Ok(())
}

#[test]
fn verify_command_accepts_dialect_flag() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    verify_args(&mut cmd, &copybook_path, &data_path)
        .arg("--dialect")
        .arg("n")
        .assert()
        .success();

    Ok(())
}
