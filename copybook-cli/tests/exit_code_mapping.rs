use std::ffi::OsString;
use std::process::{Command, Stdio};

use tempfile::{NamedTempFile, TempDir};

fn run_and_status(args: &[OsString]) -> i32 {
    let bin = env!("CARGO_BIN_EXE_copybook");
    let status = Command::new(bin)
        .args(args)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("failed to spawn copybook CLI");
    status.code().unwrap_or(1)
}

#[test]
fn exit_code_cbkf_is_4() {
    let copybook = NamedTempFile::new().expect("temp copybook");
    std::fs::write(copybook.path(), "01 RECORD.\n   05 FIELD PIC X(4).").expect("write copybook");

    // Malformed RDW payload: header claims 16 bytes but body only has 4.
    let input = NamedTempFile::new().expect("temp input");
    std::fs::write(input.path(), [0x00, 0x10, 0xAA, 0xBB, 0xCC, 0xDD])
        .expect("write malformed rdw");

    let output_dir = TempDir::new().expect("temp output dir");
    let output_path = output_dir.path().join("rdw-out.jsonl");

    let args = vec![
        OsString::from("decode"),
        OsString::from("--format"),
        OsString::from("rdw"),
        OsString::from("--codepage"),
        OsString::from("ascii"),
        OsString::from("--output"),
        output_path.into_os_string(),
        copybook.path().as_os_str().to_owned(),
        input.path().as_os_str().to_owned(),
    ];

    let code = run_and_status(&args);

    assert_eq!(code, 4, "CBKF errors should map to exit code 4");
}
