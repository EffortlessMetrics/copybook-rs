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
use copybook_codec::options::profile::InterpretationProfile;
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
    assert!(!manifest.inputs.bundle.fingerprint.is_empty());
    assert_eq!(manifest.inputs.bundle.schema_version, 1);
    // The emitted manifest pins the exact reviewed profile the CLI consumed.
    let selected = InterpretationProfile::parse(FIXED_CP037_PROFILE).expect("profile parses");
    let identity = manifest.inputs.profile.as_ref().expect("profile identity");
    assert_eq!(identity.schema_version, selected.schema_version);
    assert_eq!(
        identity.fingerprint,
        selected.fingerprint().expect("profile fingerprints")
    );
    assert_eq!(manifest.inputs.tool.name, "copybook");
    assert!(!manifest.inputs.tool.version.is_empty());
    assert_eq!(manifest.schema_fingerprint.len(), 64);
    assert_eq!(manifest.record_len_min, Some(manifest.record_len));
    assert_eq!(manifest.source_spans, "unavailable");
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

#[test]
fn inspect_emit_manifest_refuses_stdout_target() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest", "-"])
        .assert()
        .code(3)
        .stderr(predicates::str::contains("file path"));
}

#[test]
#[cfg(unix)]
fn inspect_emit_manifest_refuses_dangling_symlink() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let manifest_path = dir.path().join("out.manifest.json");
    std::os::unix::fs::symlink("nowhere.json", &manifest_path).expect("symlink created");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    // exists() misses dangling symlinks; the target guard must not.
    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .code(3)
        .stderr(predicates::str::contains("overwrite-manifest"));
}

#[test]
fn inspect_emit_manifest_no_overwrite_by_default() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let manifest_path = dir.path().join("out.manifest.json");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    // First emission succeeds.
    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .success();
    let first = std::fs::read(&manifest_path).expect("first manifest written");

    // A second emission to the same path refuses and leaves the file intact.
    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .code(3)
        .stderr(predicates::str::contains("overwrite-manifest"));
    let kept = std::fs::read(&manifest_path).expect("manifest still readable");
    assert_eq!(
        kept, first,
        "refused emission leaves the file byte-identical"
    );

    // Explicit opt-in replaces the target.
    let sentinel = dir.path().join("sentinel.manifest.json");
    std::fs::write(&sentinel, b"stale bytes").expect("sentinel written");
    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&sentinel)
        .args(["--overwrite-manifest"])
        .assert()
        .success();
    let replaced = std::fs::read(&sentinel).expect("replaced manifest readable");
    assert_ne!(replaced, b"stale bytes".as_slice());
    let manifest = ResolvedManifest::from_json(&replaced).expect("replaced manifest verifies");
    assert_eq!(manifest.schema_version, 2);
}

#[test]
fn inspect_overwrite_manifest_requires_emit_manifest() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    // Usage errors (including clap's missing-requirement refusal) exit 3.
    cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--overwrite-manifest"])
        .assert()
        .code(3)
        .stderr(predicates::str::contains("emit-manifest"));
}

#[test]
fn inspect_emit_manifest_layout_matches_legacy_inspect() {
    // The artifact never replaces the human report: emission stdout carries
    // the same layout the profile-less legacy path prints.
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let manifest_path = dir.path().join("out.manifest.json");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    let legacy = cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--codepage", "cp037", "--dialect", "n"])
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();

    let emitted = cmd()
        .args(["inspect"])
        .arg(&copybook)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .success()
        .get_output()
        .stdout
        .clone();

    assert_eq!(emitted, legacy, "emission keeps the legacy layout report");
}

#[test]
fn inspect_emit_manifest_field_count_bound_leaves_no_file() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let manifest_path = dir.path().join("out.manifest.json");
    let mut copybook = String::from("       01 BIG-REC.\n");
    for index in 0..5000 {
        copybook.push_str(&format!("           05 F{index:05} PIC X.\n"));
    }
    let copybook_path = write_temp_file(&dir, "big.cpy", copybook.as_bytes());

    cmd()
        .args(["inspect"])
        .arg(&copybook_path)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .failure()
        .stderr(predicates::str::contains("exceeding the limit"));
    assert!(
        !manifest_path.exists(),
        "a bound failure leaves no partial manifest"
    );
}

#[test]
fn inspect_emit_manifest_byte_bound_leaves_no_file() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let manifest_path = dir.path().join("out.manifest.json");
    // Packed fields stay under the field-count bound while their numeric
    // details push the serialized snapshot past its byte bound. Names stay
    // short: fixed-format lines end at column 72.
    let mut copybook = String::from("       01 BIG-REC.\n");
    for index in 0..4090 {
        copybook.push_str(&format!("           05 F{index:05} PIC 9(10) COMP-3.\n"));
    }
    let copybook_path = write_temp_file(&dir, "fat.cpy", copybook.as_bytes());

    cmd()
        .args(["inspect"])
        .arg(&copybook_path)
        .args(["--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .assert()
        .failure()
        .stderr(predicates::str::contains("exceeding the limit"));
    assert!(
        !manifest_path.exists(),
        "a bound failure leaves no partial manifest"
    );
}

// =========================================================================
// Ownership queries (#1122 slice 1)
// =========================================================================
// Static field-to-byte and byte-to-field answers over a manifest document
// or a copybook/profile pair, with identical machine results from both
// inputs and closed refusals for unanswerable paths.

/// Byte query over copybook/profile names the leaf owner and containers.
#[test]
fn inspect_query_byte_names_owner() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .args(["--payload-byte", "2"])
        .arg(&copybook)
        .assert()
        .success()
        .stdout(predicates::str::contains("Ownership query: payload byte 2"))
        .stdout(predicates::str::contains("State: owned"))
        .stdout(predicates::str::contains("PRIMARY"))
        .stdout(predicates::str::contains("CUSTOMER-RECORD.CUSTOMER-ID"))
        .stdout(predicates::str::contains("0..6"))
        .stdout(predicates::str::contains("CONTAINER"))
        .stdout(predicates::str::contains("Profile: sha256:"));
}

/// Manifest-backed and source-backed queries answer byte-identically.
#[test]
fn inspect_query_manifest_matches_source() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let manifest_path = dir.path().join("simple.manifest.json");

    cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .arg(&copybook)
        .assert()
        .success();

    let from_manifest = cmd()
        .args(["inspect", "--manifest"])
        .arg(&manifest_path)
        .args(["--payload-byte", "38"])
        .output()
        .expect("manifest query");
    assert_eq!(from_manifest.status.code(), Some(0));

    let from_source = cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .args(["--payload-byte", "38"])
        .arg(&copybook)
        .output()
        .expect("source query");
    assert_eq!(from_source.status.code(), Some(0));
    assert_eq!(
        from_source.stdout, from_manifest.stdout,
        "both inputs answer byte-identically"
    );
    let stdout = String::from_utf8(from_manifest.stdout).expect("utf8 stdout");
    assert!(stdout.contains("ACCOUNT-BALANCE"));
    assert!(stdout.contains("36..41"));
}

/// Field query with JSON output binds identities and carries no local paths.
#[test]
fn inspect_query_field_json_binds_identities() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    let output = cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .args(["--field", "account-balance", "--output", "json"])
        .arg(&copybook)
        .output()
        .expect("field query json");
    assert_eq!(output.status.code(), Some(0));
    let stdout = String::from_utf8(output.stdout).expect("utf8 stdout");
    let parsed: serde_json::Value = serde_json::from_str(&stdout).expect("answer parses");
    assert_eq!(parsed["query"]["kind"], "field_path");
    assert_eq!(parsed["coordinate_system"], "payload-relative");
    assert!(
        parsed["manifest_fingerprint"]
            .as_str()
            .expect("fingerprint")
            .starts_with("sha256-v1:")
    );
    assert_eq!(parsed["state"], "owned");
    let matches = parsed["matches"].as_array().expect("matches");
    assert_eq!(matches[0]["path"], "CUSTOMER-RECORD.ACCOUNT-BALANCE");
    assert_eq!(matches[0]["role"], "primary");
    assert_eq!(matches[0]["offset"], 36);
    // Machine output carries fingerprints, never local paths.
    fn no_paths(value: &serde_json::Value, needle: &str) {
        match value {
            serde_json::Value::String(text) => assert!(
                !text.contains(needle),
                "machine output leaks a local path: {text}"
            ),
            serde_json::Value::Array(items) => {
                items.iter().for_each(|item| no_paths(item, needle));
            }
            serde_json::Value::Object(fields) => {
                fields.values().for_each(|item| no_paths(item, needle));
            }
            _ => {}
        }
    }
    let dir_str = dir.path().to_str().expect("tempdir utf8");
    no_paths(&parsed, dir_str);
}

/// Unknown field paths fail closed with exit 3.
#[test]
fn inspect_query_unknown_field_fails_closed() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .args(["--field", "NOPE"])
        .arg(&copybook)
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("unknown field path 'NOPE'"));
}

/// Ambiguous short names fail closed naming every candidate.
#[test]
fn inspect_query_ambiguous_short_name_fails_closed() {
    const AMBIGUOUS_COPYBOOK: &str = "       01  REC.\n           05  LEFT.\n               10  CODE PIC X(2).\n           05  RIGHT.\n               10  CODE PIC X(2).\n";
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "ambiguous.cpy", AMBIGUOUS_COPYBOOK.as_bytes());

    cmd()
        .args(["inspect", "--format", "fixed", "--field", "CODE"])
        .arg(&copybook)
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("ambiguous field path 'CODE'"))
        .stderr(predicates::str::contains("REC.LEFT.CODE"))
        .stderr(predicates::str::contains("REC.RIGHT.CODE"));
}

/// Bytes past the record extent report an explicit state with exit 0.
#[test]
fn inspect_query_out_of_range_reports_state() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .args(["--payload-byte", "50"])
        .arg(&copybook)
        .assert()
        .success()
        .stdout(predicates::str::contains("State: out of range"));
}

/// `--manifest` with `COPYBOOK` is a contradiction, never a silent default.
#[test]
fn inspect_query_manifest_and_copybook_conflict() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let manifest_path = dir.path().join("simple.manifest.json");

    cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .arg(&copybook)
        .assert()
        .success();

    cmd()
        .args(["inspect", "--manifest"])
        .arg(&manifest_path)
        .args(["--payload-byte", "2"])
        .arg(&copybook)
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("--manifest reads no copybook"));
}

/// An oversize `--manifest` is refused before allocation, with the same
/// invalid-manifest subcode `from_json` reports after the read.
#[test]
fn inspect_query_oversize_manifest_refused_without_read() {
    let dir = tempfile::tempdir().expect("tempdir");
    let big = vec![b' '; 1_048_577];
    let manifest = write_temp_file(&dir, "big.manifest.json", &big);

    cmd()
        .args(["inspect", "--manifest"])
        .arg(&manifest)
        .args(["--payload-byte", "0"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("exceeds the"))
        .stderr(predicates::str::contains("subcode=410"));
}

/// `--manifest` without a selector enters query validation instead of
/// silently running the legacy layout report that ignores the manifest.
#[test]
fn inspect_query_manifest_without_selector_fails() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let manifest_path = dir.path().join("simple.manifest.json");

    cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .args(["--emit-manifest"])
        .arg(&manifest_path)
        .arg(&copybook)
        .assert()
        .success();

    cmd()
        .args(["inspect", "--manifest"])
        .arg(&manifest_path)
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains(
            "needs --payload-byte <N> or --field <PATH>",
        ))
        .stderr(predicates::str::contains("subcode=407"));
}

/// Non-default `--output` without a query selector is rejected: the layout
/// report has no machine rendering to honor it with.
#[test]
fn inspect_output_json_without_selector_fails() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");

    cmd()
        .args(["inspect", "--output", "json"])
        .arg(&copybook)
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains(
            "--output json needs an ownership query",
        ))
        .stderr(predicates::str::contains("subcode=407"));
}

/// REDEFINES bytes report one storage owner plus views.
#[test]
fn inspect_query_redefines_reports_owner_and_views() {
    const REDEFINES_COPYBOOK: &str = "       01  REC.\n           05  PRIMARY  PIC X(6).\n           05  SECONDARY  REDEFINES PRIMARY PIC 9(6).\n";
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "redefines.cpy", REDEFINES_COPYBOOK.as_bytes());

    cmd()
        .args(["inspect", "--format", "fixed", "--payload-byte", "2"])
        .arg(&copybook)
        .assert()
        .success()
        .stdout(predicates::str::contains("PRIMARY"))
        .stdout(predicates::str::contains("VIEW"))
        .stdout(predicates::str::contains("REC.SECONDARY"))
        .stdout(predicates::str::contains("Profile: direct"));
}
