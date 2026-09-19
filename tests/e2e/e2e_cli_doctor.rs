// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `doctor` CLI subcommand.
//!
//! Validates the healthy demo pair reports healthy (exit 0) with a next
//! command, a garbage copybook fails with its stable identity and fix
//! (exit 3), a truncated file fails framing, copybook-only mode passes,
//! and JSON output is machine-readable.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;
use std::io::Write as _;

fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

fn workspace_path(rel: &str) -> std::path::PathBuf {
    // copybook-e2e lives at tests/e2e; fixtures live at the workspace root.
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(rel)
}

fn write_temp_file(dir: &tempfile::TempDir, name: &str, contents: &[u8]) -> std::path::PathBuf {
    let path = dir.path().join(name);
    let mut file = std::fs::File::create(&path).expect("create temp file");
    file.write_all(contents).expect("write temp file");
    path
}

#[test]
fn doctor_healthy_demo_pair() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stdout(predicate::str::contains("healthy"))
        .stdout(predicate::str::contains("trial-decode"))
        .stdout(predicate::str::contains("next: copybook decode"));
}

#[test]
fn doctor_garbage_copybook_names_code_and_fix() {
    let dir = tempfile::tempdir().expect("tempdir");
    let bad = write_temp_file(&dir, "bad.cpy", b"THIS IS NOT A COPYBOOK ((( ");
    let data = workspace_path("fixtures/data/simple.bin");
    cmd()
        .args(["doctor"])
        .arg(&bad)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stdout(predicate::str::contains("CBKP"))
        .stdout(predicate::str::contains("fix:"));
}

#[test]
fn doctor_truncated_file_fails_framing() {
    let dir = tempfile::tempdir().expect("tempdir");
    let data = workspace_path("fixtures/data/simple.bin");
    let bytes = std::fs::read(&data).expect("read fixture");
    let truncated = write_temp_file(&dir, "short.bin", &bytes[..30]);
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&truncated)
        .assert()
        .failure()
        .stdout(predicate::str::contains("format-probe"));
}

#[test]
fn doctor_copybook_only_mode_passes() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .assert()
        .success()
        .stdout(predicate::str::contains("copybook-only diagnosis"));
}

#[test]
fn doctor_healthy_run_keeps_probe_noise_off_stderr() {
    // Doctor's speculative probes (losing framings, losing codepages) warn
    // inside the libraries they reuse; rejected hypotheses must not leak
    // onto stderr. The verdict and findings on stdout are the diagnosis.
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stderr(predicate::str::contains("WARN").not())
        .stdout(predicate::str::contains("healthy"));
}

#[test]
fn doctor_verbose_run_shows_probe_internals() {
    // Explicit -v opts back into library internals, including the losing
    // codepage warnings the quiet path suppresses.
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    cmd()
        .arg("-v")
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stderr(predicate::str::contains("WARN"))
        .stdout(predicate::str::contains("healthy"));
}

#[test]
fn doctor_large_file_diagnoses_bounded_scope() {
    // A multi-megabyte extract must not be ingested whole: doctor inspects
    // a bounded leading prefix and says so in the result.
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let record = std::fs::read(&data).expect("read fixture");
    let dir = tempfile::tempdir().expect("tempdir");
    let mut big = Vec::new();
    for _ in 0..30_000 {
        big.extend_from_slice(&record);
    }
    let path = write_temp_file(&dir, "big.bin", &big);
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&path)
        .assert()
        .success()
        .stderr(predicate::str::contains("WARN").not())
        .stdout(predicate::str::contains("healthy"))
        .stdout(predicate::str::contains(format!(
            "inspecting first 1048576 of {} bytes",
            big.len()
        )));
}

#[test]
fn doctor_vb_pair_is_healthy() {
    // VB is a peer framing candidate (BDW plus nested RDW), not a failure.
    let copybook = workspace_path("fixtures/corpus/mini.cpy");
    let data = workspace_path("fixtures/corpus/mini_vb.bin");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stdout(predicate::str::contains("healthy"))
        .stdout(predicate::str::contains("bytes fit VB framing"))
        .stdout(predicate::str::contains("--format vb"));
}

#[test]
fn doctor_ambiguous_framing_stays_inconclusive() {
    // Bytes fitting both fixed (LRECL multiple) and RDW (valid headers)
    // must not secretly continue as one of them: the probe fails with the
    // pin command and no trial decode runs on a guess.
    let copybook = workspace_path("fixtures/corpus/mini.cpy");
    let dir = tempfile::tempdir().expect("tempdir");
    let mut bytes = Vec::new();
    for payload in [b"ABCDEFGHIJ", b"KLMNOPQRST"] {
        bytes.extend_from_slice(&[0, 10, 0, 0]);
        bytes.extend_from_slice(payload);
    }
    let data = write_temp_file(&dir, "ambiguous.bin", &bytes);
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(4)
        .stdout(predicate::str::contains("bytes fit fixed and RDW framing"))
        .stdout(predicate::str::contains(
            "Rerun with --format fixed or --format rdw",
        ));
}

#[test]
fn doctor_low_confidence_codepage_names_candidates() {
    // A tied probe must not silently configure the trial: it warns with
    // the leading candidates, and the trial corroborates without pinning.
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stdout(predicate::str::contains("no reliable winner"))
        .stdout(predicate::str::contains("leading candidates"))
        .stdout(predicate::str::contains(
            "corroborating the leading candidate without pinning it",
        ));
}

#[test]
fn doctor_high_confidence_codepage_resolves() {
    let copybook = workspace_path("fixtures/corpus/mini.cpy");
    let data = workspace_path("fixtures/corpus/mini_rdw.bin");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "ascii wins over 36 bytes with high confidence",
        ));
}

#[test]
fn doctor_emit_profile_drafts_reviewable_file() {
    // A healthy diagnosis drafts a profile: established keys are PINNED,
    // the unpinned leading codepage is REVIEW, and the draft feeds back
    // into `decode --profile` unchanged.
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let dir = tempfile::tempdir().expect("tempdir");
    let draft = dir.path().join("draft.toml");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .arg("--emit-profile")
        .arg(&draft)
        .assert()
        .success()
        .stdout(predicate::str::contains("healthy"))
        .stderr(predicate::str::contains("profile drafted"))
        .stderr(predicate::str::contains("need review"));
    let rendered = std::fs::read_to_string(&draft).expect("draft written");
    assert!(
        rendered.contains("# PINNED framing.kind=fixed"),
        "framing pinned, got:\n{rendered}"
    );
    assert!(
        rendered.contains("# REVIEW decode.codepage="),
        "leading codepage needs review, got:\n{rendered}"
    );
    assert!(rendered.contains("schema_version = 1"), "got:\n{rendered}");
    assert!(rendered.contains("kind = \"fixed\""), "got:\n{rendered}");
    assert!(
        rendered.contains("codepage = \"cp037\""),
        "got:\n{rendered}"
    );
    assert!(
        rendered.contains("maximum_record_length = 50"),
        "got:\n{rendered}"
    );
    let out = dir.path().join("out.jsonl");
    cmd()
        .args(["decode"])
        .arg(&copybook)
        .arg(&data)
        .arg("--profile")
        .arg(&draft)
        .arg("--output")
        .arg(&out)
        .assert()
        .success()
        .stdout(predicate::str::contains("Records with errors: 0"));
}

#[test]
fn doctor_emit_profile_fully_pinned_with_flags() {
    // Explicit --format/--codepage pin every probed key: no REVIEW lines.
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let dir = tempfile::tempdir().expect("tempdir");
    let draft = dir.path().join("pinned.toml");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .arg("--emit-profile")
        .arg(&draft)
        .assert()
        .success()
        .stderr(predicate::str::contains("fully pinned"));
    let rendered = std::fs::read_to_string(&draft).expect("draft written");
    assert!(
        !rendered.lines().any(|line| line.starts_with("# REVIEW")),
        "no review lines expected, got:\n{rendered}"
    );
    assert!(
        rendered.contains("# PINNED decode.codepage=cp037 (explicit --codepage flag)"),
        "got:\n{rendered}"
    );
}

#[test]
fn doctor_emit_profile_refuses_on_failure() {
    // A failing diagnosis emits nothing and keeps its own exit code.
    let dir = tempfile::tempdir().expect("tempdir");
    let bad = write_temp_file(&dir, "bad.cpy", b"THIS IS NOT A COPYBOOK ((( ");
    let data = workspace_path("fixtures/data/simple.bin");
    let draft = dir.path().join("refused.toml");
    cmd()
        .args(["doctor"])
        .arg(&bad)
        .arg(&data)
        .arg("--emit-profile")
        .arg(&draft)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("profile not emitted"));
    assert!(!draft.exists(), "no draft may be written on failure");
}

#[test]
fn doctor_emit_profile_write_failure_escalates() {
    // A healthy diagnosis that cannot write its draft keeps the stderr
    // report but escalates to an orchestration error (exit 5), not a
    // record diagnosis.
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let dir = tempfile::tempdir().expect("tempdir");
    let missing = dir.path().join("no-such-dir").join("draft.toml");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .arg("--emit-profile")
        .arg(&missing)
        .assert()
        .failure()
        .code(5)
        .stderr(predicate::str::contains(
            "profile not emitted: cannot write",
        ));
}

#[test]
fn doctor_json_report_is_machine_readable() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let assert = cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .arg("--json")
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let parsed: serde_json::Value =
        serde_json::from_str(&stdout).expect("doctor --json should produce valid JSON");
    assert_eq!(parsed["verdict"], "healthy");
    let findings = parsed["findings"].as_array().expect("findings array");
    assert!(
        findings.iter().any(|f| f["check"] == "trial-decode"),
        "report must include trial-decode, got: {parsed}"
    );
}
