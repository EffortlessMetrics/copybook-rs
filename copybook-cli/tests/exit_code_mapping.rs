use std::ffi::OsString;
use std::process::{Command, Stdio};

use serial_test::serial;
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
#[serial]
fn exit_code_cbkf_is_4() {
    let copybook = NamedTempFile::new().expect("temp copybook");
    std::fs::write(copybook.path(), "01 RECORD.\n   05 FIELD PIC X(4).").expect("write copybook");

    // Malformed RDW payload: header claims 16 bytes but body only has 4.
    let input = NamedTempFile::new().expect("temp input");
    std::fs::write(input.path(), [0x00, 0x10, 0xAA, 0xBB, 0xCC, 0xDD])
        .expect("write malformed rdw payload");

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

#[test]
#[serial]
fn exit_code_cbkd_is_2() {
    // Data error: invalid nibble for packed decimal → CBKD (2)
    let copybook = NamedTempFile::new().expect("temp copybook");
    std::fs::write(copybook.path(), "01 REC.\n  05 F1 PIC 9(2) COMP-3.\n").expect("write copybook");

    // 0xFF bytes are invalid packed nibbles for a 2-digit COMP-3 field.
    let input = NamedTempFile::new().expect("temp input");
    std::fs::write(input.path(), [0xFFu8, 0xFF]).expect("write invalid packed payload");

    let output = NamedTempFile::new().expect("temp output");

    let args = vec![
        OsString::from("decode"),
        OsString::from("--format"),
        OsString::from("fixed"),
        OsString::from("--codepage"),
        OsString::from("ascii"),
        OsString::from("--fail-fast"),
        OsString::from("--output"),
        output.path().as_os_str().to_owned(),
        copybook.path().as_os_str().to_owned(),
        input.path().as_os_str().to_owned(),
    ];

    let code = run_and_status(&args);

    assert_eq!(code, 2, "CBKD errors should map to exit code 2");
}

#[test]
#[serial]
fn exit_code_cbke_is_3() {
    // JSON/type mismatch on encode → CBKE (3)
    let copybook = NamedTempFile::new().expect("temp copybook");
    std::fs::write(copybook.path(), "01 REC.\n  05 F1 PIC 9(2).\n").expect("write copybook");

    let payload = NamedTempFile::new().expect("temp json payload");
    // Malformed JSON to trigger the parser/type error path immediately.
    std::fs::write(payload.path(), br#"{"F1":"unterminated"#).expect("write malformed json");

    let output = NamedTempFile::new().expect("temp output");

    let args = vec![
        OsString::from("encode"),
        OsString::from("--format"),
        OsString::from("fixed"),
        OsString::from("--codepage"),
        OsString::from("ascii"),
        OsString::from("--output"),
        output.path().as_os_str().to_owned(),
        copybook.path().as_os_str().to_owned(),
        payload.path().as_os_str().to_owned(),
    ];

    let code = run_and_status(&args);

    assert_eq!(code, 3, "CBKE errors should map to exit code 3");
}

#[test]
#[serial]
fn exit_code_cbki_is_5() {
    // Iterator/CLI invalid state: force --format fixed on an ODO layout → CBKI (5)
    let copybook = NamedTempFile::new().expect("temp copybook");
    std::fs::write(
        copybook.path(),
        "01 REC.\n  05 CNT PIC 9(1).\n  05 ARR OCCURS 1 TO 3 TIMES DEPENDING ON CNT.\n    10 A PIC X(1).\n",
    )
    .expect("write copybook");

    let input = NamedTempFile::new().expect("temp input");
    std::fs::write(input.path(), b"").expect("write empty payload");

    let output = NamedTempFile::new().expect("temp output");

    let args = vec![
        OsString::from("decode"),
        OsString::from("--format"),
        OsString::from("fixed"),
        OsString::from("--codepage"),
        OsString::from("ascii"),
        OsString::from("--output"),
        output.path().as_os_str().to_owned(),
        copybook.path().as_os_str().to_owned(),
        input.path().as_os_str().to_owned(),
    ];

    let code = run_and_status(&args);

    assert_eq!(code, 5, "CBKI errors should map to exit code 5");
}

#[test]
#[serial]
fn panic_is_mapped_to_internal_exit_code() {
    let bin = env!("CARGO_BIN_EXE_copybook");
    let status = Command::new(bin)
        .env("COPYBOOK_TEST_PANIC", "1")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("failed to spawn copybook CLI");

    assert_eq!(
        status.code(),
        Some(5),
        "Panics should map to internal (CBKI) exit code"
    );
}

#[test]
#[serial]
fn broken_pipe_is_treated_as_success() {
    let bin = env!("CARGO_BIN_EXE_copybook");
    let copybook = NamedTempFile::new().expect("temp copybook");
    std::fs::write(copybook.path(), "01 RECORD.\n   05 FIELD PIC X(4).").expect("write copybook");

    let mut child = Command::new(bin)
        .arg("inspect")
        .arg(copybook.path())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("failed to spawn copybook CLI");

    // Immediately drop the read end of stdout to simulate a downstream consumer exiting early.
    drop(child.stdout.take());

    let status = child.wait().expect("failed to wait for copybook CLI");

    assert_eq!(
        status.code(),
        Some(0),
        "Broken pipe scenarios should resolve to success exit code"
    );
}
