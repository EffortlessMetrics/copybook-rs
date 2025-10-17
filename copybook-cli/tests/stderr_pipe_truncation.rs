mod common;

use assert_cmd::cargo::cargo_bin;
use common::TestResult;
use os_pipe::pipe;
use std::path::PathBuf;
use std::process::{Command, ExitStatus, Stdio};
use tempfile::TempDir;

const OK: i32 = 0;
const CBKE: i32 = 3;

#[test]
fn lenient_warning_with_closed_stderr_pipe_exits_zero() -> TestResult<()> {
    let status = run_decode_with_closed_stderr(false)?;
    assert_eq!(status.code(), Some(OK));
    Ok(())
}

#[test]
fn strict_error_with_closed_stderr_pipe_exits_cbke() -> TestResult<()> {
    let status = run_decode_with_closed_stderr(true)?;
    assert_eq!(status.code(), Some(CBKE));
    Ok(())
}

#[test]
fn inspect_success_with_closed_stderr_pipe_exits_zero() -> TestResult<()> {
    let (reader, writer) = pipe()?;
    drop(reader);

    let copybook = simple_copybook_fixture();
    let bin = cargo_bin("copybook");
    let mut cmd = Command::new(bin);
    cmd.arg("inspect")
        .arg(&copybook)
        .stdout(Stdio::null())
        .stderr(Stdio::from(writer));

    let status = cmd.status()?;
    assert_eq!(status.code(), Some(OK));
    Ok(())
}

fn run_decode_with_closed_stderr(strict_policy: bool) -> TestResult<ExitStatus> {
    let (reader, writer) = pipe()?;
    drop(reader);

    let copybook = simple_copybook_fixture();
    let input = simple_data_fixture();
    let temp_dir = TempDir::new()?;
    let output = temp_dir.path().join("decode-output.jsonl");

    let bin = cargo_bin("copybook");
    let mut cmd = Command::new(bin);
    if strict_policy {
        cmd.arg("--strict-policy");
    }
    cmd.arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--preferred-zoned-encoding")
        .arg("ascii")
        .arg(&copybook)
        .arg(&input)
        .arg("--output")
        .arg(&output)
        .stdout(Stdio::null())
        .stderr(Stdio::from(writer));

    let status = cmd.status()?;
    Ok(status)
}

fn simple_copybook_fixture() -> PathBuf {
    workspace_root().join("fixtures/copybooks/simple.cpy")
}

fn simple_data_fixture() -> PathBuf {
    workspace_root().join("fixtures/data/simple.bin")
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .to_path_buf()
}
