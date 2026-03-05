// SPDX-License-Identifier: AGPL-3.0-or-later
//! Multi-worker determinism proof tests.
//!
//! Validates that parallel decode/encode produces byte-identical output
//! regardless of thread count, run iteration, or worker completion order.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use sha2::{Digest, Sha256};
use std::process::{Command, Output};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn copybook_cmd() -> Command {
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .expect("parent of tests/e2e")
        .parent()
        .expect("workspace root");
    let bin_name = if cfg!(windows) {
        "copybook.exe"
    } else {
        "copybook"
    };
    let bin_path = workspace_root.join("target").join("debug").join(bin_name);
    assert!(
        bin_path.exists(),
        "copybook binary not found at {bin_path:?}. Run `cargo build --bin copybook` first."
    );
    Command::new(bin_path)
}

fn stdout_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn stderr_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

fn assert_no_panic(stderr: &str) {
    assert!(
        !stderr.contains("panicked at"),
        "CLI panicked! stderr:\n{stderr}"
    );
}

fn sha256_hex(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("{:x}", hasher.finalize())
}

/// A 13-byte copybook: NAME PIC X(10), AGE PIC 9(3).
const MULTI_RECORD_COPYBOOK: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// Build 20 EBCDIC records for multi-thread testing.
/// Each record: NAME (10 bytes CP037) + AGE (3 bytes CP037).
fn build_multi_records() -> Vec<u8> {
    // Pre-built name/age pairs in CP037
    let records: Vec<(&[u8], &[u8])> = vec![
        // "ALICE     " "025"
        (
            &[0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40],
            &[0xF0, 0xF2, 0xF5],
        ),
        // "BOB       " "030"
        (
            &[0xC2, 0xD6, 0xC2, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40],
            &[0xF0, 0xF3, 0xF0],
        ),
        // "CHARLIE   " "045"
        (
            &[0xC3, 0xC8, 0xC1, 0xD9, 0xD3, 0xC9, 0xC5, 0x40, 0x40, 0x40],
            &[0xF0, 0xF4, 0xF5],
        ),
        // "DIANA     " "028"
        (
            &[0xC4, 0xC9, 0xC1, 0xD5, 0xC1, 0x40, 0x40, 0x40, 0x40, 0x40],
            &[0xF0, 0xF2, 0xF8],
        ),
        // "EVE       " "022"
        (
            &[0xC5, 0xE5, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40],
            &[0xF0, 0xF2, 0xF2],
        ),
    ];

    let mut data = Vec::new();
    // Repeat the 5 records 4 times = 20 records
    for _ in 0..4 {
        for (name, age) in &records {
            data.extend_from_slice(name);
            data.extend_from_slice(age);
        }
    }
    data
}

/// Write test fixtures to a temp dir and return it.
fn setup_multi_record_dir() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    std::fs::write(dir.path().join("schema.cpy"), MULTI_RECORD_COPYBOOK).unwrap();
    std::fs::write(dir.path().join("data.bin"), build_multi_records()).unwrap();
    dir
}

/// Run decode with given thread count, return the JSONL output bytes.
fn run_decode(dir: &tempfile::TempDir, threads: usize) -> Vec<u8> {
    let out_path = dir.path().join(format!("output_{threads}t.jsonl"));

    let output = copybook_cmd()
        .arg("decode")
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .arg("--output")
        .arg(&out_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .args(["--threads", &threads.to_string()])
        .output()
        .expect("run decode");

    assert_eq!(
        output.status.code(),
        Some(0),
        "decode with {threads} threads failed. stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));

    std::fs::read(&out_path).expect("read decoded JSONL")
}

/// Run decode to a specific output path.
fn run_decode_to(dir: &tempfile::TempDir, out_name: &str, threads: usize) -> Vec<u8> {
    let out_path = dir.path().join(out_name);

    let output = copybook_cmd()
        .arg("decode")
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .arg("--output")
        .arg(&out_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .args(["--threads", &threads.to_string()])
        .output()
        .expect("run decode");

    assert_eq!(
        output.status.code(),
        Some(0),
        "decode failed. stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));

    std::fs::read(&out_path).expect("read decoded JSONL")
}

// =========================================================================
// 1. Multi-thread decode ordering: 1, 2, 4, 8 threads → identical JSONL
// =========================================================================

#[test]
fn test_decode_deterministic_across_thread_counts() {
    let dir = setup_multi_record_dir();

    let baseline = run_decode(&dir, 1);
    assert!(
        !baseline.is_empty(),
        "baseline decode should produce output"
    );

    // Verify baseline has expected record count
    let baseline_str = String::from_utf8_lossy(&baseline);
    let line_count = baseline_str.lines().count();
    assert_eq!(line_count, 20, "expected 20 records, got {line_count}");

    for threads in [2, 4, 8] {
        let result = run_decode(&dir, threads);
        assert_eq!(
            baseline,
            result,
            "decode with {threads} threads produced different output than 1-thread baseline.\n\
             baseline len={}, result len={}",
            baseline.len(),
            result.len()
        );
    }
}

// =========================================================================
// 2. Multi-run determinism: 3 decode runs → identical SHA-256 hashes
// =========================================================================

#[test]
fn test_decode_multi_run_sha256_stable() {
    let dir = setup_multi_record_dir();

    let mut hashes = Vec::new();
    for i in 0..3 {
        let result = run_decode_to(&dir, &format!("run_{i}.jsonl"), 4);
        hashes.push(sha256_hex(&result));
    }

    assert_eq!(
        hashes[0], hashes[1],
        "SHA-256 mismatch between run 0 and run 1"
    );
    assert_eq!(
        hashes[1], hashes[2],
        "SHA-256 mismatch between run 1 and run 2"
    );
}

// =========================================================================
// 3. Sequence ring ordering: out-of-order submission → in-order output
// =========================================================================

#[test]
fn test_sequence_ring_preserves_input_order() {
    use copybook_sequence_ring::{SequenceRing, SequencedRecord};
    use std::thread;

    let mut ring = SequenceRing::new(64, 32);
    let sender = ring.sender();

    // Simulate 4 workers completing records in non-sequential order.
    // Records are numbered 1..=20, workers process disjoint subsets.
    let senders: Vec<_> = (0..4).map(|_| sender.clone()).collect();
    drop(sender); // drop the original; only worker copies remain

    let handles: Vec<_> = senders
        .into_iter()
        .enumerate()
        .map(|(worker_id, s)| {
            thread::spawn(move || {
                // Each worker handles IDs: worker_id+1, worker_id+5, worker_id+9, ...
                let mut ids: Vec<u64> = (0..5).map(|i| (worker_id as u64) + 1 + i * 4).collect();
                // Reverse to simulate out-of-order completion
                ids.reverse();
                for id in ids {
                    s.send(SequencedRecord::new(id, format!("record_{id}")))
                        .unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    // Receive all 20 records and verify in-order delivery
    let mut received = Vec::new();
    while let Ok(Some(data)) = ring.recv_ordered() {
        received.push(data);
        if received.len() == 20 {
            break;
        }
    }

    assert_eq!(received.len(), 20, "expected 20 records from ring");
    for (i, record) in received.iter().enumerate() {
        let expected = format!("record_{}", i + 1);
        assert_eq!(
            record, &expected,
            "record at position {i} should be '{expected}', got '{record}'"
        );
    }
}

// =========================================================================
// 4. Fingerprint stability: same copybook → same fingerprint across runs
// =========================================================================

#[test]
fn test_schema_fingerprint_stable_across_runs() {
    let schema1 = copybook_core::parse_copybook(MULTI_RECORD_COPYBOOK).expect("parse 1");
    let schema2 = copybook_core::parse_copybook(MULTI_RECORD_COPYBOOK).expect("parse 2");
    let schema3 = copybook_core::parse_copybook(MULTI_RECORD_COPYBOOK).expect("parse 3");

    assert!(
        !schema1.fingerprint.is_empty(),
        "fingerprint should be non-empty"
    );
    assert_eq!(
        schema1.fingerprint, schema2.fingerprint,
        "fingerprint should be identical across parses"
    );
    assert_eq!(
        schema2.fingerprint, schema3.fingerprint,
        "fingerprint should be identical across parses"
    );

    // Fingerprint should be a 64-char hex SHA-256
    assert_eq!(schema1.fingerprint.len(), 64);
    assert!(schema1.fingerprint.chars().all(|c| c.is_ascii_hexdigit()));
}

// =========================================================================
// 5. Determinism subcommand: decode and encode verify exit code 0
// =========================================================================

#[test]
fn test_determinism_subcommand_decode_exit_zero() {
    let dir = setup_multi_record_dir();

    let output = copybook_cmd()
        .args(["determinism", "decode"])
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run determinism decode");

    assert_no_panic(&stderr_str(&output));
    assert_eq!(
        output.status.code(),
        Some(0),
        "determinism decode should exit 0 (deterministic). stderr: {}",
        stderr_str(&output)
    );

    let so = stdout_str(&output);
    assert!(
        so.contains("deterministic")
            || so.contains("Deterministic")
            || so.contains("PASS")
            || so.contains("match")
            || so.contains("✅"),
        "determinism output should indicate success: {so}"
    );
}

#[test]
fn test_determinism_subcommand_encode_exit_zero() {
    let dir = setup_multi_record_dir();

    // First decode to get JSONL for encode input
    let jsonl_path = dir.path().join("for_encode.jsonl");
    let decode_output = copybook_cmd()
        .arg("decode")
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .arg("--output")
        .arg(&jsonl_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("decode for encode setup");
    assert_eq!(
        decode_output.status.code(),
        Some(0),
        "decode setup failed: {}",
        stderr_str(&decode_output)
    );

    let output = copybook_cmd()
        .args(["determinism", "encode"])
        .arg(dir.path().join("schema.cpy"))
        .arg(&jsonl_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run determinism encode");

    assert_no_panic(&stderr_str(&output));
    assert_eq!(
        output.status.code(),
        Some(0),
        "determinism encode should exit 0 (deterministic). stderr: {}",
        stderr_str(&output)
    );
}

// =========================================================================
// 6. Multi-thread encode determinism
// =========================================================================

#[test]
fn test_encode_deterministic_across_thread_counts() {
    let dir = setup_multi_record_dir();

    // Decode once to get JSONL
    let jsonl_path = dir.path().join("source.jsonl");
    let decode_output = copybook_cmd()
        .arg("decode")
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .arg("--output")
        .arg(&jsonl_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("decode for encode setup");
    assert_eq!(
        decode_output.status.code(),
        Some(0),
        "decode setup failed: {}",
        stderr_str(&decode_output)
    );

    // Encode with 1 thread as baseline
    let encode_with = |threads: usize| -> Vec<u8> {
        let out = dir.path().join(format!("encoded_{threads}t.bin"));
        let output = copybook_cmd()
            .arg("encode")
            .arg(dir.path().join("schema.cpy"))
            .arg(&jsonl_path)
            .arg("--output")
            .arg(&out)
            .args(["--format", "fixed", "--codepage", "cp037"])
            .args(["--threads", &threads.to_string()])
            .output()
            .expect("run encode");
        assert_eq!(
            output.status.code(),
            Some(0),
            "encode with {threads} threads failed. stderr: {}",
            stderr_str(&output)
        );
        assert_no_panic(&stderr_str(&output));
        std::fs::read(&out).expect("read encoded binary")
    };

    let baseline = encode_with(1);
    assert!(
        !baseline.is_empty(),
        "baseline encode should produce output"
    );

    for threads in [2, 4, 8] {
        let result = encode_with(threads);
        assert_eq!(
            baseline,
            result,
            "encode with {threads} threads produced different output than 1-thread baseline.\n\
             baseline len={}, result len={}",
            baseline.len(),
            result.len()
        );
    }
}
