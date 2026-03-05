#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::while_let_on_iterator)]
#![allow(clippy::panic)]
mod common;

use common::{TestResult, bin, write_file};
use predicates::str::contains;
use tempfile::TempDir;

const FIELDS: &[&str] = &[
    "code_tag=",
    "code=",
    "family=",
    "precedence_rank=",
    "effective_exit=",
    "invocation_id=",
    "log_schema=",
    "op_stage=",
    "severity_tag=",
];

fn strip_ansi(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars();
    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' {
            if let Some('[') = chars.next() {
                while let Some(c) = chars.next() {
                    if ('@'..='~').contains(&c) {
                        break;
                    }
                }
            }
        } else {
            out.push(ch);
        }
    }
    out
}

fn assert_structured(stdout: &str, stderr: &str) {
    let mut combined = String::new();
    combined.push_str(stdout);
    combined.push_str(stderr);
    let cleaned = strip_ansi(&combined);
    let has_error_level = cleaned.contains("severity_tag=ERROR")
        || cleaned.contains("level=ERROR")
        || cleaned.contains(" ERROR ")
        || cleaned.contains(" ERROR:");
    assert!(
        has_error_level,
        "missing ERROR level marker:\nstdout: {}\nstderr: {}",
        strip_ansi(stdout),
        strip_ansi(stderr)
    );
    for field in FIELDS {
        assert!(
            cleaned.contains(field),
            "missing structured field `{field}`:\nstdout: {}\nstderr: {}",
            strip_ansi(stdout),
            strip_ansi(stderr)
        );
    }
    assert!(
        cleaned.contains("severity_tag=ERROR"),
        "missing severity_tag=ERROR field:\nstdout: {}\nstderr: {}",
        strip_ansi(stdout),
        strip_ansi(stderr)
    );
}

#[test]
fn honors_env_invocation_id() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    cmd.env("RUST_LOG", "info")
        .env("COPYBOOK_INVOCATION_ID", "run-123")
        .arg("parse")
        .arg(&copybook_path);
    cmd.assert()
        .success()
        .stderr(contains("invocation_id=run-123"));
    Ok(())
}

#[test]
fn parse_failure_emits_structured_diagnostics() -> TestResult<()> {
    let missing_path = TempDir::new()?.path().join("missing.cpy");
    let mut cmd = bin();
    cmd.arg("parse").arg(&missing_path);
    let assert = cmd.assert().failure();
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_structured(&stdout, &stderr);
    Ok(())
}

#[test]
fn inspect_failure_emits_structured_diagnostics() -> TestResult<()> {
    let missing_path = TempDir::new()?.path().join("missing.cpy");
    let mut cmd = bin();
    cmd.arg("inspect").arg(&missing_path);
    let assert = cmd.assert().failure();
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_structured(&stdout, &stderr);
    Ok(())
}

#[test]
fn decode_failure_emits_structured_diagnostics() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("out.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let mut cmd = bin();
    cmd.arg("--strict-policy")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--preferred-zoned-encoding")
        .arg("ascii")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path);
    let assert = cmd.assert().failure();
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_structured(&stdout, &stderr);
    Ok(())
}

#[test]
fn encode_failure_emits_structured_diagnostics() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let input_path = tmp.path().join("missing.jsonl");
    let output_path = tmp.path().join("out.bin");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    cmd.arg("encode")
        .arg(&copybook_path)
        .arg(&input_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii");
    let assert = cmd.assert().failure();
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_structured(&stdout, &stderr);
    Ok(())
}

#[test]
fn verify_failure_emits_structured_diagnostics() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let input_path = tmp.path().join("missing.bin");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    cmd.arg("verify")
        .arg(&copybook_path)
        .arg(&input_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii");
    let assert = cmd.assert().failure();
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_structured(&stdout, &stderr);
    Ok(())
}

#[test]
fn inspect_success_emits_completion_record() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;

    let mut cmd = bin();
    cmd.env("RUST_LOG", "info");
    cmd.arg("inspect").arg(&copybook_path);

    let assert = cmd.assert().success();
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let clean_stdout = strip_ansi(&stdout);
    let clean_stderr = strip_ansi(&stderr);
    let mut combined = String::new();
    combined.push_str(&clean_stdout);
    combined.push_str(&clean_stderr);

    assert!(
        combined.contains("INFO"),
        "missing INFO completion record:\nstdout: {}\nstderr: {}",
        clean_stdout,
        clean_stderr
    );
    assert!(
        combined.contains("completed"),
        "missing completion message:\nstdout: {}\nstderr: {}",
        clean_stdout,
        clean_stderr
    );
    assert!(
        combined.contains("effective_exit=0"),
        "missing effective_exit=0 in completion record:\nstdout: {}\nstderr: {}",
        clean_stdout,
        clean_stderr
    );
    assert!(
        combined.contains("log_schema=1"),
        "missing log_schema=1 in completion record:\nstdout: {}\nstderr: {}",
        clean_stdout,
        clean_stderr
    );
    assert!(
        combined.contains("severity_tag=INFO"),
        "missing severity_tag=INFO in completion record:\nstdout: {}\nstderr: {}",
        clean_stdout,
        clean_stderr
    );
    assert!(
        combined.contains("op_stage=finalize"),
        "missing op_stage=finalize in completion record:\nstdout: {}\nstderr: {}",
        clean_stdout,
        clean_stderr
    );

    Ok(())
}
