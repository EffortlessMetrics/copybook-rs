mod common;

use common::{TestResult, bin, write_file};
use predicates::prelude::PredicateBooleanExt;
use predicates::str::contains;
use tempfile::TempDir;

const CBKE: i32 = 3;
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

#[test]
fn preferred_without_preserve_warns_by_default() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("output.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    decode_args(&mut cmd, &copybook_path, &data_path, &output_path)
        .assert()
        .success()
        .stderr(contains("code_tag=CBKE"))
        .stderr(contains("subcode=401"))
        .stderr(contains(
            "preferred zoned encoding requested without preservation",
        ));

    Ok(())
}

#[test]
fn strict_flag_overrides_env_off() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("output.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    cmd.env("COPYBOOK_STRICT_POLICY", "0")
        .arg("--strict-policy");
    decode_args(&mut cmd, &copybook_path, &data_path, &output_path)
        .assert()
        .failure()
        .code(CBKE)
        .stderr(contains(
            "requires --preserve-zoned-encoding in strict mode",
        ))
        .stderr(contains("subcode=401"));

    Ok(())
}

#[test]
fn preferred_without_preserve_respects_strict_policy_env() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("output.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    cmd.env("COPYBOOK_STRICT_POLICY", "1");
    decode_args(&mut cmd, &copybook_path, &data_path, &output_path)
        .assert()
        .failure()
        .code(CBKE)
        .stderr(contains(
            "requires --preserve-zoned-encoding in strict mode",
        ))
        .stderr(contains("subcode=401"));

    Ok(())
}

#[test]
fn no_strict_flag_overrides_env_on() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("output.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    cmd.env("COPYBOOK_STRICT_POLICY", "1")
        .arg("--no-strict-policy");
    decode_args(&mut cmd, &copybook_path, &data_path, &output_path)
        .assert()
        .success()
        .stderr(contains("code_tag=CBKE"))
        .stderr(contains("subcode=401"));

    Ok(())
}

#[test]
fn compat_warning_logs_warn_with_subcode_and_effective_exit() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("output.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    decode_args(&mut cmd, &copybook_path, &data_path, &output_path)
        .assert()
        .success()
        .stderr(contains("WARN").or(contains("level=WARN")))
        .stderr(contains("code_tag=CBKE"))
        .stderr(contains("subcode=401"))
        .stderr(contains("effective_exit=0"))
        .stderr(contains("invocation_id="));

    Ok(())
}

#[test]
fn strict_failure_logs_error_with_subcode_and_effective_exit() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("output.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    cmd.arg("--strict-policy");
    decode_args(&mut cmd, &copybook_path, &data_path, &output_path)
        .assert()
        .failure()
        .code(CBKE)
        .stderr(contains("ERROR").or(contains("level=ERROR")))
        .stderr(contains("code_tag=CBKE"))
        .stderr(contains("subcode=401"))
        .stderr(contains("effective_exit=3"))
        .stderr(contains("invocation_id="));

    Ok(())
}
