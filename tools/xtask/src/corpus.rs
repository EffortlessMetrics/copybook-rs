// SPDX-License-Identifier: AGPL-3.0-or-later
//! Private local validator for the governed corpus (#952 Lane B).
//!
//! Produces payload-free redacted receipts for running `copybook` against
//! files that cannot be committed. The receipt carries content hashes,
//! counts, and stable identities only: never record payload, raw field
//! values, filenames, paths, or user identifiers. This tool only shells out
//! to the local `copybook` CLI; it never uploads or transmits inputs.

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

const CORPUS_MANIFEST_PATH: &str = "fixtures/corpus/manifest.toml";
const RECEIPT_SCHEMA_VERSION: &str = "0.7.0";
/// Bound for any failure excerpt copied into a receipt.
const MAX_FAILURE_CHARS: usize = 512;

/// Manifest subset the receipt producer needs. The authoritative manifest
/// contract and fail-closed validation live in `docs_verify::verify_corpus`;
/// this reader is intentionally a forward-compatible subset.
#[derive(Debug, Deserialize)]
struct ReceiptManifest {
    #[serde(default)]
    fixture: Vec<ReceiptFixture>,
}

#[derive(Debug, Deserialize)]
struct ReceiptFixture {
    fixture_id: String,
    copybook: String,
    #[serde(default)]
    records: String,
    record_organization: String,
    codepage: String,
    dialect: String,
}

#[derive(Debug, Serialize)]
struct CorpusReceipt {
    receipt_schema_version: String,
    tool_version: String,
    build_commit: String,
    fixture_id: String,
    copybook_sha256: String,
    copybook_schema_fingerprint: Option<String>,
    records_sha256: Option<String>,
    record_format: String,
    codepage: String,
    dialect: String,
    record_count: Option<u64>,
    physical_bytes: Option<u64>,
    status: String,
    stable_identity: Option<String>,
    first_failure: Option<String>,
    deterministic: Option<bool>,
    round_trip: Option<bool>,
    output_sha256: Option<String>,
    redaction: ReceiptRedaction,
    started_at: String,
    completed_at: String,
}

#[derive(Debug, Serialize)]
struct ReceiptRedaction {
    record_payload_excluded: bool,
    paths_redacted: bool,
}

pub(crate) fn run_receipt(args: &[&str]) -> Result<()> {
    let mut fixture_id: Option<&str> = None;
    let mut out: Option<&str> = None;
    let mut index = 0;
    while index < args.len() {
        match args[index] {
            "--fixture" => {
                fixture_id = Some(args.get(index + 1).context("missing value for --fixture")?);
                index += 1;
            }
            "--out" => {
                out = Some(args.get(index + 1).context("missing value for --out")?);
                index += 1;
            }
            other => bail!(
                "unknown corpus receipt argument `{other}`; expected --fixture <id> --out <path>"
            ),
        }
        index += 1;
    }
    let fixture_id = fixture_id.context("corpus receipt needs --fixture <id>")?;
    let out = out.context("corpus receipt needs --out <path>")?;

    let started_at = chrono::Utc::now().to_rfc3339();
    let root = workspace_root();
    let fixture = load_receipt_fixture(&root, fixture_id)?;
    let tool_version = format!("copybook {}", env!("CARGO_PKG_VERSION"));
    let build_commit = build_commit(&root)?;

    let receipt = if fixture.records.is_empty() {
        receipt_for_parse_rejection(&root, &fixture, &tool_version, &build_commit, &started_at)?
    } else {
        receipt_for_records(&root, &fixture, &tool_version, &build_commit, &started_at)?
    };

    let json = serde_json::to_string_pretty(&receipt).context("serializing corpus receipt")?;
    fs::write(out, format!("{json}\n"))
        .with_context(|| format!("writing corpus receipt to {out}"))?;
    println!("corpus receipt for `{fixture_id}` written to {out}");
    Ok(())
}

fn workspace_root() -> PathBuf {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest
        .parent()
        .and_then(|tools| tools.parent())
        .unwrap_or(manifest)
        .to_path_buf()
}

fn load_receipt_fixture(root: &Path, fixture_id: &str) -> Result<ReceiptFixture> {
    let path = root.join(CORPUS_MANIFEST_PATH);
    let source = fs::read_to_string(&path)
        .with_context(|| format!("loading corpus manifest {}", path.display()))?;
    let manifest: ReceiptManifest =
        toml::from_str(&source).with_context(|| format!("parsing {}", path.display()))?;
    manifest
        .fixture
        .into_iter()
        .find(|entry| entry.fixture_id == fixture_id)
        .with_context(|| format!("unknown corpus fixture `{fixture_id}`"))
}

fn build_commit(root: &Path) -> Result<String> {
    let output = Command::new("git")
        .current_dir(root)
        .args(["rev-parse", "HEAD"])
        .output()
        .context("resolving build commit")?;
    if !output.status.success() {
        bail!("git rev-parse HEAD failed for corpus receipt");
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn sha256_file_hex(path: &Path) -> Result<String> {
    use sha2::Digest as _;
    use std::fmt::Write as _;

    let bytes =
        fs::read(path).with_context(|| format!("reading receipt input {}", path.display()))?;
    let digest = sha2::Sha256::digest(&bytes);
    let mut hex = String::with_capacity(64);
    for byte in digest {
        let _ = write!(hex, "{byte:02x}");
    }
    Ok(hex)
}

/// Run the local CLI from the workspace root with workspace-relative paths
/// so diagnostics never capture absolute local paths.
fn run_cli(root: &Path, args: &[&str]) -> Result<std::process::Output> {
    Command::new("cargo")
        .current_dir(root)
        .arg("run")
        .arg("-q")
        .arg("-p")
        .arg("copybook-cli")
        .arg("--")
        .args(args)
        .output()
        .context("running local copybook CLI")
}

fn stable_identity_in(text: &str) -> Option<String> {
    let pattern = regex::Regex::new(r"CBK[A-Z]+[0-9]{3}_[A-Z0-9_]+").ok()?;
    pattern.find(text).map(|hit| hit.as_str().to_string())
}

/// Bound a failure excerpt and strip any echoed input paths. Keeps the single
/// diagnostic line (the stable-identity line when present) so log timestamps
/// and tracing fields never enter the receipt.
fn bound_failure(text: &str, copybook: &str, records: &str) -> String {
    let line = text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .find(|line| line.contains("CBK"))
        .or_else(|| text.lines().map(str::trim).find(|line| !line.is_empty()))
        .unwrap_or(text.trim());
    let mut scrubbed = line.replace(copybook, "<copybook>");
    if !records.is_empty() {
        scrubbed = scrubbed.replace(records, "<records>");
    }
    scrubbed.chars().take(MAX_FAILURE_CHARS).collect()
}

fn receipt_for_parse_rejection(
    root: &Path,
    fixture: &ReceiptFixture,
    tool_version: &str,
    build_commit: &str,
    started_at: &str,
) -> Result<CorpusReceipt> {
    let copybook_sha256 = sha256_file_hex(&root.join(&fixture.copybook))?;
    let output = run_cli(root, &["parse", &fixture.copybook])?;
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let combined = format!("{stdout}\n{stderr}");
    let stable_identity = stable_identity_in(&combined);
    let status = if output.status.success() || stable_identity.is_none() {
        "tool-failure".to_string()
    } else {
        "rejected".to_string()
    };
    Ok(CorpusReceipt {
        receipt_schema_version: RECEIPT_SCHEMA_VERSION.to_string(),
        tool_version: tool_version.to_string(),
        build_commit: build_commit.to_string(),
        fixture_id: fixture.fixture_id.clone(),
        copybook_sha256,
        copybook_schema_fingerprint: None,
        records_sha256: None,
        record_format: fixture.record_organization.clone(),
        codepage: fixture.codepage.clone(),
        dialect: fixture.dialect.clone(),
        record_count: None,
        physical_bytes: None,
        first_failure: Some(bound_failure(combined.trim(), &fixture.copybook, "")),
        status,
        stable_identity,
        deterministic: None,
        round_trip: None,
        output_sha256: None,
        redaction: ReceiptRedaction {
            record_payload_excluded: true,
            paths_redacted: true,
        },
        started_at: started_at.to_string(),
        completed_at: chrono::Utc::now().to_rfc3339(),
    })
}

fn receipt_for_records(
    root: &Path,
    fixture: &ReceiptFixture,
    tool_version: &str,
    build_commit: &str,
    started_at: &str,
) -> Result<CorpusReceipt> {
    let copybook_sha256 = sha256_file_hex(&root.join(&fixture.copybook))?;
    let records_path = root.join(&fixture.records);
    let records_sha256 = sha256_file_hex(&records_path)?;
    let physical_bytes = records_path.metadata()?.len();

    let workdir = scratch_dir("corpus-receipt")?;
    let decode_a = workdir.0.join("decode_a.jsonl");
    let decode_b = workdir.0.join("decode_b.jsonl");
    let decode_args = |out: &Path| {
        vec![
            "decode".to_string(),
            fixture.copybook.clone(),
            fixture.records.clone(),
            "-o".to_string(),
            out.to_string_lossy().into_owned(),
            "--format".to_string(),
            fixture.record_organization.clone(),
            "--codepage".to_string(),
            fixture.codepage.clone(),
        ]
    };
    let args_a = decode_args(&decode_a);
    let args_b = decode_args(&decode_b);
    let first = run_cli(root, &args_a.iter().map(String::as_str).collect::<Vec<_>>())?;
    if !first.status.success() {
        let combined = format!(
            "{}\n{}",
            String::from_utf8_lossy(&first.stdout),
            String::from_utf8_lossy(&first.stderr)
        );
        return Ok(rejected_receipt(
            fixture,
            tool_version,
            build_commit,
            started_at,
            &copybook_sha256,
            &records_sha256,
            physical_bytes,
            &combined,
        ));
    }
    let second = run_cli(root, &args_b.iter().map(String::as_str).collect::<Vec<_>>())?;
    if !second.status.success() {
        bail!("corpus receipt second decode pass failed; inputs may be nondeterministic");
    }
    let bytes_a = fs::read(&decode_a)?;
    let bytes_b = fs::read(&decode_b)?;
    let deterministic = bytes_a == bytes_b;
    let record_count = u64::try_from(
        std::str::from_utf8(&bytes_a)
            .map(|text| text.lines().count())
            .unwrap_or_default(),
    )
    .unwrap_or(u64::MAX);
    let output_sha256 = sha256_bytes_hex(&bytes_a);

    let round_trip = round_trip_holds(root, fixture, &workdir.0)?;
    let copybook_schema_fingerprint = schema_fingerprint(root, fixture)?;

    Ok(CorpusReceipt {
        receipt_schema_version: RECEIPT_SCHEMA_VERSION.to_string(),
        tool_version: tool_version.to_string(),
        build_commit: build_commit.to_string(),
        fixture_id: fixture.fixture_id.clone(),
        copybook_sha256,
        copybook_schema_fingerprint,
        records_sha256: Some(records_sha256),
        record_format: fixture.record_organization.clone(),
        codepage: fixture.codepage.clone(),
        dialect: fixture.dialect.clone(),
        record_count: Some(record_count),
        physical_bytes: Some(physical_bytes),
        status: "supported".to_string(),
        stable_identity: None,
        first_failure: None,
        deterministic: Some(deterministic),
        round_trip: Some(round_trip),
        output_sha256: Some(output_sha256),
        redaction: ReceiptRedaction {
            record_payload_excluded: true,
            paths_redacted: true,
        },
        started_at: started_at.to_string(),
        completed_at: chrono::Utc::now().to_rfc3339(),
    })
}

#[allow(clippy::too_many_arguments)]
fn rejected_receipt(
    fixture: &ReceiptFixture,
    tool_version: &str,
    build_commit: &str,
    started_at: &str,
    copybook_sha256: &str,
    records_sha256: &str,
    physical_bytes: u64,
    combined: &str,
) -> CorpusReceipt {
    CorpusReceipt {
        receipt_schema_version: RECEIPT_SCHEMA_VERSION.to_string(),
        tool_version: tool_version.to_string(),
        build_commit: build_commit.to_string(),
        fixture_id: fixture.fixture_id.clone(),
        copybook_sha256: copybook_sha256.to_string(),
        copybook_schema_fingerprint: None,
        records_sha256: Some(records_sha256.to_string()),
        record_format: fixture.record_organization.clone(),
        codepage: fixture.codepage.clone(),
        dialect: fixture.dialect.clone(),
        record_count: None,
        physical_bytes: Some(physical_bytes),
        status: "rejected".to_string(),
        stable_identity: stable_identity_in(combined),
        first_failure: Some(bound_failure(
            combined.trim(),
            &fixture.copybook,
            &fixture.records,
        )),
        deterministic: None,
        round_trip: None,
        output_sha256: None,
        redaction: ReceiptRedaction {
            record_payload_excluded: true,
            paths_redacted: true,
        },
        started_at: started_at.to_string(),
        completed_at: chrono::Utc::now().to_rfc3339(),
    }
}

/// Private scratch directory, removed best-effort when the receipt is done.
struct ScratchDir(PathBuf);

impl Drop for ScratchDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn scratch_dir(prefix: &str) -> Result<ScratchDir> {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let unique = COUNTER.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("{prefix}-{}-{}", std::process::id(), unique));
    fs::create_dir_all(&dir)
        .with_context(|| format!("creating receipt scratch dir {}", dir.display()))?;
    Ok(ScratchDir(dir))
}

fn sha256_bytes_hex(bytes: &[u8]) -> String {
    use sha2::Digest as _;
    use std::fmt::Write as _;

    let digest = sha2::Sha256::digest(bytes);
    let mut hex = String::with_capacity(64);
    for byte in digest {
        let _ = write!(hex, "{byte:02x}");
    }
    hex
}

/// Decode with whole-record raw capture, re-encode from the captured bytes,
/// and compare against the original records.
fn round_trip_holds(root: &Path, fixture: &ReceiptFixture, workdir: &Path) -> Result<bool> {
    let raw_decode = workdir.join("roundtrip_raw.jsonl");
    let reencoded = workdir.join("roundtrip.bin");
    let decode = run_cli(
        root,
        &[
            "decode",
            &fixture.copybook,
            &fixture.records,
            "-o",
            &raw_decode.to_string_lossy(),
            "--format",
            &fixture.record_organization,
            "--codepage",
            &fixture.codepage,
            "--emit-raw",
            "record",
        ],
    )?;
    if !decode.status.success() {
        return Ok(false);
    }
    let encode = run_cli(
        root,
        &[
            "encode",
            &fixture.copybook,
            &raw_decode.to_string_lossy(),
            "-o",
            &reencoded.to_string_lossy(),
            "--format",
            &fixture.record_organization,
            "--codepage",
            &fixture.codepage,
            "--use-raw",
        ],
    )?;
    if !encode.status.success() {
        return Ok(false);
    }
    let original = fs::read(root.join(&fixture.records))?;
    let produced = fs::read(&reencoded)?;
    Ok(original == produced)
}

/// Canonical schema fingerprint from the verify report surface.
fn schema_fingerprint(root: &Path, fixture: &ReceiptFixture) -> Result<Option<String>> {
    let workdir = scratch_dir("corpus-fingerprint")?;
    let report = workdir.0.join("verify.json");
    let output = run_cli(
        root,
        &[
            "verify",
            &fixture.copybook,
            &fixture.records,
            "--format",
            &fixture.record_organization,
            "--codepage",
            &fixture.codepage,
            "--report",
            &report.to_string_lossy(),
        ],
    )?;
    if !output.status.success() {
        return Ok(None);
    }
    let text = fs::read_to_string(&report)?;
    let value: serde_json::Value = serde_json::from_str(&text)?;
    Ok(value
        .get("schema_fingerprint")
        .and_then(serde_json::Value::as_str)
        .map(str::to_string))
}
