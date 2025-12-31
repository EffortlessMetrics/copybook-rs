#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use common::{TestResult, bin, write_file};
use predicates::str::contains;
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
