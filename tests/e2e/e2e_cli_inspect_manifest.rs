// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for `inspect --emit-manifest` (toward #1122).
//!
//! A reviewed profile plus `inspect` produces a resolved-schema manifest file
//! binding the run's effective inputs with provenance, the layout bounds, and
//! the support classification. Emission requires `--profile`, rejects
//! flag/profile conflicts (exit 3), and refuses stdin copybooks (exit 3:
//! stdin has no stable source identity).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use copybook_codec::resolved_manifest::ResolvedManifest;
use std::io::Write as _;

#[allow(deprecated)]
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

/// Fixed/CP037 profile matching `fixtures/copybooks/simple.cpy`.
const FIXED_CP037_PROFILE: &str = "\
schema_version = 1
[source]
dialect = \"normative\"
[framing]
kind = \"fixed\"
reserved_bytes = \"lenient\"
[decode]
codepage = \"cp037\"
unmappable = \"error\"
json_numbers = \"lossless\"
[limits]
maximum_record_length = 32760
maximum_errors = 100
";

#[test]
fn inspect_emits_manifest_with_profile_provenance() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let manifest_path = dir.path().join("simple.manifest.json");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .success();

    let bytes = std::fs::read(&manifest_path).expect("read emitted manifest");
    let manifest = ResolvedManifest::from_json(&bytes).expect("emitted manifest verifies");
    assert_eq!(manifest.inputs.dialect.value, "normative");
    assert_eq!(manifest.inputs.dialect.source, "profile");
    assert_eq!(manifest.inputs.framing.value, "fixed");
    assert_eq!(manifest.inputs.framing.source, "profile");
    assert_eq!(manifest.inputs.encoding.value, "cp037");
    assert_eq!(manifest.inputs.encoding.source, "profile");
    let bound = manifest
        .inputs
        .record_bound
        .as_ref()
        .expect("bound present");
    assert_eq!((bound.value, bound.source.as_str()), (32760, "profile"));
    assert!(!manifest.fields.is_empty(), "layout fields present");
    assert!(
        manifest
            .fields
            .iter()
            .any(|field| field.path.contains("CUSTOMER-ID")),
        "customer fields present"
    );
    assert!(!manifest.inputs.bundle_fingerprint.is_empty());
}

#[test]
fn inspect_emit_manifest_requires_profile() {
    let dir = tempfile::tempdir().expect("tempdir");
    let manifest_path = dir.path().join("out.manifest.json");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .code(3);
    assert!(!manifest_path.exists(), "no manifest on missing profile");
}

#[test]
fn inspect_emit_manifest_flag_conflict_fails() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let manifest_path = dir.path().join("out.manifest.json");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--profile"])
        .arg(&profile)
        .args(["--codepage", "cp273"])
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .code(3);
    assert!(!manifest_path.exists(), "no manifest on conflict");
}

#[test]
fn inspect_legacy_path_needs_no_profile() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .assert()
        .success()
        .stdout(predicates::str::contains("Copybook Layout"));
}

#[test]
fn inspect_emit_manifest_refuses_stdin_copybook() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let manifest_path = dir.path().join("out.manifest.json");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let source = std::fs::read(&copybook).expect("read fixture");

    cmd()
        .args(["inspect", "-"])
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .write_stdin(source)
        .assert()
        .code(3);
    assert!(!manifest_path.exists(), "no manifest for stdin copybook");
}
