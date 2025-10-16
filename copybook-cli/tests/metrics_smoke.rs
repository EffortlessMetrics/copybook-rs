#![cfg(feature = "metrics")]
// Integration smoke test for the /metrics exporter in a short failing run.
// It asserts that HELP/TYPE descriptors are present and that the CBKF family
// counter increments for a malformed RDW header.

use std::net::TcpListener;
use std::process::{Command, Stdio};
use std::{thread, time::Duration};

fn pick_free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .expect("bind ephemeral")
        .local_addr()
        .expect("addr")
        .port()
}

#[test]
#[ignore] // run only when explicitly requested or in the metrics smoke workflow
fn metrics_shows_descriptors_and_cbkf_on_short_error() {
    // --- Prepare a small malformed RDW: len=0x0010 but only 4 body bytes ---
    let bad = tempfile::NamedTempFile::new().expect("temp");
    std::fs::write(bad.path(), [0x00, 0x10, 0xAA, 0xBB, 0xCC, 0xDD]).expect("write bad rdw");

    let copybook = tempfile::NamedTempFile::new().expect("temp copybook");
    std::fs::write(
        copybook.path(),
        "       01 TEST-REC.\n           05 FIELD PIC X(04).\n",
    )
    .expect("write copybook");

    // --- Pick a free port for /metrics ---
    let port = pick_free_port();
    let addr = format!("127.0.0.1:{port}");

    // --- Spawn CLI with exporter and a brief grace window for scrapes ---
    // CARGO_BIN_EXE_copybook is set by Cargo for integration tests targeting the bin.
    let bin = env!("CARGO_BIN_EXE_copybook");
    let output_dir = tempfile::tempdir().expect("temp dir");
    let output_path = output_dir.path().join("out.jsonl");
    let mut child = Command::new(bin)
        .args([
            "--metrics-listen",
            &addr,
            "--metrics-grace-ms",
            "1500",
            "decode",
            "--format",
            "rdw",
            "--codepage",
            "ascii",
            "--output",
            output_path.to_str().expect("output path"),
            copybook.path().to_str().unwrap(),
            bad.path().to_str().unwrap(),
        ])
        .stdout(Stdio::null())
        .stderr(Stdio::inherit())
        .spawn()
        .expect("spawn copybook");

    // Give the exporter a moment to bind before the first scrape.
    thread::sleep(Duration::from_millis(300));
    assert!(
        child.try_wait().expect("child status check").is_none(),
        "copybook process exited before metrics scrape"
    );

    // --- Scrape /metrics once during the grace window ---
    let body = reqwest::blocking::get(format!("http://127.0.0.1:{port}/metrics"))
        .expect("scrape")
        .text()
        .expect("metrics body");

    // HELP/TYPE descriptors should always be present after describe_metrics_once().
    assert!(
        body.contains("# HELP copybook_decode_errors_total")
            && body.contains("# TYPE copybook_decode_errors_total counter"),
        "descriptors missing in metrics body:\n{body}"
    );

    // CBKF should increment for malformed RDW underflow.
    assert!(
        body.contains(r#"copybook_decode_errors_total{family="CBKF"}"#),
        "CBKF family counter missing in metrics body:\n{body}"
    );

    // Exit code should map to 4 for CBKF family.
    let status = child.wait().expect("wait");
    assert_eq!(status.code(), Some(4), "exit code should be 4 (CBKF)");
}
