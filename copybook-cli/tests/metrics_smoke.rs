#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "metrics")]
// Integration smoke test for the /metrics exporter in a short failing run.
// It asserts that HELP/TYPE descriptors are present and that the CBKF family
// counter increments for a malformed RDW header.

use std::io::ErrorKind;
use std::net::TcpListener;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

fn pick_free_port() -> std::io::Result<u16> {
    let listener = TcpListener::bind("127.0.0.1:0")?;
    let addr = listener.local_addr()?;
    Ok(addr.port())
}

#[test]
#[ignore = "run only when explicitly requested or in the metrics smoke workflow"]
fn metrics_shows_descriptors_and_cbkf_on_short_error() -> Result<(), Box<dyn std::error::Error>> {
    // --- Prepare a small malformed RDW: len=0x0010 but only 4 body bytes ---
    let bad = tempfile::NamedTempFile::new()?;
    std::fs::write(bad.path(), [0x00, 0x10, 0xAA, 0xBB, 0xCC, 0xDD])?;

    let copybook = tempfile::NamedTempFile::new()?;
    std::fs::write(
        copybook.path(),
        "       01 TEST-REC.\n           05 FIELD PIC X(04).\n",
    )?;

    // --- Pick a free port for /metrics ---
    let port = pick_free_port()?;
    let addr = format!("127.0.0.1:{port}");

    // --- Spawn CLI with exporter and a brief grace window for scrapes ---
    // CARGO_BIN_EXE_copybook is set by Cargo for integration tests targeting the bin.
    let bin = env!("CARGO_BIN_EXE_copybook");
    let output_dir = tempfile::tempdir()?;
    let output_path = output_dir.path().join("out.jsonl");
    let output_arg = output_path
        .to_str()
        .ok_or_else(|| {
            std::io::Error::new(
                ErrorKind::InvalidData,
                format!("output path is not UTF-8: {output_path:?}"),
            )
        })?
        .to_owned();
    let copybook_arg = copybook
        .path()
        .to_str()
        .ok_or_else(|| {
            std::io::Error::new(
                ErrorKind::InvalidData,
                format!("copybook path is not UTF-8: {:?}", copybook.path()),
            )
        })?
        .to_owned();
    let bad_arg = bad
        .path()
        .to_str()
        .ok_or_else(|| {
            std::io::Error::new(
                ErrorKind::InvalidData,
                format!("RDW path is not UTF-8: {:?}", bad.path()),
            )
        })?
        .to_owned();

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
            &output_arg,
            &copybook_arg,
            &bad_arg,
        ])
        .stdout(Stdio::null())
        .stderr(Stdio::inherit())
        .spawn()?;

    // Wait for the exporter to be ready by polling until it responds or timeout
    let start = Instant::now();
    let timeout = Duration::from_secs(5);
    let mut body = String::new();
    let mut server_ready = false;

    while start.elapsed() < timeout {
        // Check if process is still running
        if child.try_wait()?.is_some() {
            return Err("copybook process exited before metrics scrape".into());
        }

        // Try to scrape metrics
        match reqwest::blocking::get(format!("http://127.0.0.1:{port}/metrics")) {
            Ok(response) => {
                body = response.text()?;
                server_ready = true;
                break;
            }
            Err(_) => {
                // Server not ready yet, wait a bit and retry
                thread::sleep(Duration::from_millis(50));
            }
        }
    }

    if !server_ready {
        return Err(format!(
            "metrics server did not become ready within {} seconds",
            timeout.as_secs()
        )
        .into());
    }

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
    let status = child.wait()?;
    assert_eq!(status.code(), Some(4), "exit code should be 4 (CBKF)");

    Ok(())
}
