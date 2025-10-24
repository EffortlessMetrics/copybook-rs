#![allow(clippy::expect_used)]
#![allow(clippy::panic)]
mod common;

use assert_cmd::cargo::cargo_bin;
use common::{TestResult, write_file};
use os_pipe::pipe;
use std::io::Read;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use tempfile::TempDir;

const OK: i32 = 0;
const CBKE: i32 = 3;

fn simple_copybook_fixture() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace root")
        .join("fixtures/copybooks/simple.cpy")
}

#[cfg(unix)]
#[test]
fn stdout_broken_pipe_is_ok_unix() -> TestResult<()> {
    stdout_broken_pipe_is_ok()
}

#[cfg(windows)]
#[test]
fn stdout_broken_pipe_is_ok_windows() -> TestResult<()> {
    stdout_broken_pipe_is_ok()
}

#[cfg(any(unix, windows))]
fn stdout_broken_pipe_is_ok() -> TestResult<()> {
    let bin = cargo_bin("copybook");
    let (mut reader, writer) = pipe()?;
    let copybook = simple_copybook_fixture();

    let mut cmd = Command::new(bin);
    cmd.arg("parse")
        .arg(&copybook)
        .stdout(Stdio::from(writer))
        .stderr(Stdio::piped());

    let mut child = cmd.spawn()?;
    let mut stderr_pipe = child.stderr.take();

    let mut buffer = [0u8; 1];
    let _ = reader.read(&mut buffer)?;
    drop(reader);

    let status = child.wait()?;
    let mut stderr_output = String::new();
    if let Some(reader) = stderr_pipe.as_mut() {
        reader.read_to_string(&mut stderr_output)?;
    }
    assert_eq!(status.code(), Some(OK), "stderr output: {stderr_output}");
    Ok(())
}

#[cfg(unix)]
#[test]
fn stderr_broken_pipe_is_ok_unix() -> TestResult<()> {
    stderr_broken_pipe_respects_exit_code()
}

#[cfg(windows)]
#[test]
fn stderr_broken_pipe_is_ok_windows() -> TestResult<()> {
    stderr_broken_pipe_respects_exit_code()
}

#[cfg(any(unix, windows))]
fn stderr_broken_pipe_respects_exit_code() -> TestResult<()> {
    let tmp = TempDir::new()?;
    let copybook_path = tmp.path().join("schema.cpy");
    let data_path = tmp.path().join("input.bin");
    let output_path = tmp.path().join("output.jsonl");

    write_file(&copybook_path, "01 RECORD.\n  05 FIELD PIC X(4).")?;
    write_file(&data_path, b"DATA")?;

    let bin = cargo_bin("copybook");
    let (mut reader, writer) = pipe()?;

    let mut cmd = Command::new(bin);
    cmd.env("COPYBOOK_STRICT_POLICY", "1");
    cmd.arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--preferred-zoned-encoding")
        .arg("ascii")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .stdout(Stdio::null())
        .stderr(Stdio::from(writer));

    let mut child = cmd.spawn()?;

    let mut buffer = [0u8; 1];
    let _ = reader.read(&mut buffer)?;
    drop(reader);

    let status = child.wait()?;
    assert_eq!(status.code(), Some(CBKE));
    Ok(())
}
