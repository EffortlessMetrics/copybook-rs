// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use anyhow::{Context, Result, ensure};
use serde_json::Value;
use sha2::{Digest, Sha256};
use tempfile::TempDir;

const COMMIT: &str = "0123456789abcdef0123456789abcdef01234567";

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn fixtures() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("test_fixtures/external_input")
}

fn run_cli(manifest: &Path, output: &Path) -> Result<Output> {
    run_cli_in(&workspace_root(), manifest, output)
}

fn run_cli_in(current_dir: &Path, manifest: &Path, output: &Path) -> Result<Output> {
    Command::new(env!("CARGO_BIN_EXE_external-input-preflight"))
        .arg(manifest)
        .arg(output)
        .current_dir(current_dir)
        .env("GITHUB_SHA", COMMIT)
        .output()
        .context("failed to run external-input-preflight binary")
}

fn copy_fixture(name: &str) -> Result<(TempDir, PathBuf)> {
    let temp = tempfile::tempdir()?;
    for entry in fs::read_dir(fixtures())? {
        let entry = entry?;
        if entry.file_type()?.is_file() {
            fs::copy(entry.path(), temp.path().join(entry.file_name()))?;
        }
    }
    let manifest = temp.path().join(name);
    Ok((temp, manifest))
}

fn has_adjacent_lines(text: &str, first: &str, second: &str) -> bool {
    let mut previous = None;
    for line in text.lines() {
        if previous == Some(first) && line == second {
            return true;
        }
        previous = Some(line);
    }
    false
}

fn has_exact_top_level_block(text: &str, header: &str, expected: &[&str]) -> bool {
    if text.lines().filter(|line| *line == header).count() != 1 {
        return false;
    }
    let mut block = Vec::new();
    let mut collecting = false;
    for line in text.lines() {
        if line == header {
            collecting = true;
            continue;
        }
        if !collecting {
            continue;
        }
        if line.is_empty() {
            continue;
        }
        if !line.starts_with(' ') {
            break;
        }
        block.push(line);
    }
    block == expected
}

fn shell_array_entries<'a>(text: &'a str, declaration: &str) -> Option<Vec<&'a str>> {
    if text
        .lines()
        .filter(|line| line.trim() == declaration)
        .count()
        != 1
    {
        return None;
    }
    let mut entries = Vec::new();
    let mut collecting = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed == declaration {
            collecting = true;
            continue;
        }
        if !collecting {
            continue;
        }
        if trimmed == ")" {
            return Some(entries);
        }
        if trimmed.is_empty() || trimmed.contains(char::is_whitespace) {
            return None;
        }
        entries.push(trimmed);
    }
    None
}

fn has_exact_workspace_manifest_binding(text: &str) -> bool {
    const EXPECTED: &str = "COPYBOOK_EXTERNAL_INPUT_MANIFEST=\"$GITHUB_WORKSPACE/$manifest\" \\";
    let mut bindings = text.lines().filter_map(|line| {
        let normalized = line.trim_start().trim_end_matches('\r');
        normalized
            .starts_with("COPYBOOK_EXTERNAL_INPUT_MANIFEST=")
            .then_some(normalized)
    });
    bindings.next() == Some(EXPECTED) && bindings.next().is_none()
}

#[test]
fn adjacent_lines_are_eol_agnostic_but_layout_sensitive() -> Result<()> {
    let first = "permissions:";
    let second = "  contents: read";
    for accepted in [
        "permissions:\n  contents: read\n",
        "permissions:\r\n  contents: read\r\n",
    ] {
        ensure!(has_adjacent_lines(accepted, first, second));
    }
    for rejected in [
        "permissions:\n contents: read\n",
        "permissions:\n\n  contents: read\n",
        "permissions:\n  unrelated: value\n  contents: read\n",
        "  contents: read\npermissions:\n",
    ] {
        ensure!(!has_adjacent_lines(rejected, first, second));
    }
    Ok(())
}

fn require_success(output: &Output) -> Result<()> {
    ensure!(
        output.status.success(),
        "preflight CLI failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(())
}

#[test]
fn cli_publishes_closed_deterministic_report() -> Result<()> {
    let temp = tempfile::tempdir()?;
    let first = temp.path().join("first.json");
    let second = temp.path().join("second.json");
    let manifest = fixtures().join("rdw-cp037.json");
    require_success(&run_cli(&manifest, &first)?)?;
    require_success(&run_cli(&manifest, &second)?)?;

    let first_bytes = fs::read(&first)?;
    let second_bytes = fs::read(&second)?;
    ensure!(Sha256::digest(&first_bytes) == Sha256::digest(&second_bytes));
    let report: Value = serde_json::from_slice(&first_bytes)?;
    ensure!(report.pointer("/schema_version") == Some(&Value::String("1.0.0".into())));
    ensure!(report.pointer("/status") == Some(&Value::String("decoded".into())));
    ensure!(report.pointer("/commit") == Some(&Value::String(COMMIT.into())));
    ensure!(report.pointer("/record_format") == Some(&Value::String("rdw".into())));
    ensure!(report.pointer("/codepage") == Some(&Value::String("cp037".into())));
    ensure!(report.pointer("/decoded_records") == Some(&Value::from(1)));
    ensure!(report.pointer("/physical_bytes") == Some(&Value::from(9)));
    ensure!(report.pointer("/payload_bytes") == Some(&Value::from(5)));
    ensure!(report.pointer("/framing_bytes") == Some(&Value::from(4)));
    ensure!(report.pointer("/payload_ranges/0/start") == Some(&Value::from(4)));
    ensure!(report.pointer("/payload_ranges/0/end") == Some(&Value::from(9)));
    Ok(())
}

#[test]
fn report_matches_closed_schema_inventory() -> Result<()> {
    let temp = tempfile::tempdir()?;
    let output = temp.path().join("report.json");
    require_success(&run_cli(&fixtures().join("fixed-ascii.json"), &output)?)?;
    let report: Value = serde_json::from_slice(&fs::read(output)?)?;
    let schema: Value = serde_json::from_slice(&fs::read(
        workspace_root().join("schemas/external-input-preflight-report.json"),
    )?)?;
    ensure!(schema.pointer("/additionalProperties") == Some(&Value::Bool(false)));
    ensure!(
        schema.pointer("/properties/schema_version/const") == Some(&Value::String("1.0.0".into()))
    );
    ensure!(schema.pointer("/properties/status/const") == Some(&Value::String("decoded".into())));
    let required = schema
        .pointer("/required")
        .and_then(Value::as_array)
        .context("report schema required inventory is missing")?;
    let required: BTreeSet<&str> = required.iter().filter_map(Value::as_str).collect();
    let actual: BTreeSet<&str> = report
        .as_object()
        .context("preflight report is not an object")?
        .keys()
        .map(String::as_str)
        .collect();
    ensure!(actual == required);
    for field in ["manifest_sha256", "copybook_sha256", "dataset_sha256"] {
        let value = report
            .get(field)
            .and_then(Value::as_str)
            .with_context(|| format!("report field {field} is missing"))?;
        ensure!(value.len() == 64 && value.bytes().all(|byte| byte.is_ascii_hexdigit()));
    }
    for forbidden in ["timestamp", "duration", "throughput", "pass", "slo"] {
        ensure!(!actual.contains(forbidden));
    }
    Ok(())
}

#[test]
fn cli_removes_stale_output_on_validation_and_write_failure() -> Result<()> {
    let temp = tempfile::tempdir()?;
    let stale = temp.path().join("stale.json");
    let (_fixture, manifest) = copy_fixture("fixed-ascii.json")?;
    let mut invalid: Value = serde_json::from_slice(&fs::read(&manifest)?)?;
    invalid["schema_version"] = Value::String("unsupported".into());
    fs::write(&manifest, serde_json::to_vec_pretty(&invalid)?)?;
    fs::write(&stale, b"stale-success")?;
    let validation = run_cli(&manifest, &stale)?;
    ensure!(!validation.status.success());
    ensure!(!stale.exists());

    let missing_parent = temp.path().join("missing-parent");
    let unwritable = missing_parent.join("report.json");
    let write = run_cli(&fixtures().join("fixed-ascii.json"), &unwritable)?;
    ensure!(!write.status.success());
    ensure!(!unwritable.exists());
    Ok(())
}

#[test]
fn cli_preserves_unverifiable_output_for_missing_and_malformed_manifests() -> Result<()> {
    let temp = tempfile::tempdir()?;
    for (manifest, label) in [
        (temp.path().join("missing.json"), "missing"),
        (temp.path().join("malformed.json"), "malformed"),
    ] {
        if label == "malformed" {
            fs::write(&manifest, b"{not-json")?;
        }
        let output = temp.path().join(format!("{label}-output.json"));
        fs::write(&output, b"unverifiable-stale-output")?;
        let result = run_cli(&manifest, &output)?;
        ensure!(!result.status.success());
        ensure!(fs::read(&output)? == b"unverifiable-stale-output");
    }
    Ok(())
}

#[cfg(unix)]
#[test]
fn cli_preserves_lexical_target_when_parent_identity_is_unresolved() -> Result<()> {
    let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
    let stale = temp.path().join("stale-report.json");
    fs::write(&stale, b"stale-success")?;
    let unresolved = temp.path().join("missing-parent/../stale-report.json");

    let result = run_cli(&manifest, &unresolved)?;
    ensure!(!result.status.success());
    ensure!(
        String::from_utf8_lossy(&result.stderr)
            .contains("preflight output directory does not exist")
    );
    ensure!(fs::read(&stale)? == b"stale-success");
    Ok(())
}

#[test]
fn cli_rejects_relative_input_aliases_without_mutating_inputs() -> Result<()> {
    let cases = [
        ("fixed-ascii.json", "manifest"),
        ("./simple.cpy", "copybook"),
        ("fixed-ascii.bin", "dataset"),
    ];
    for (output_name, artifact) in cases {
        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let output = temp.path().join(output_name);
        let before = fs::read(&output)?;
        let result = run_cli_in(
            temp.path(),
            Path::new("fixed-ascii.json"),
            Path::new(output_name),
        )?;
        ensure!(!result.status.success());
        ensure!(fs::read(&output)? == before);
        ensure!(
            String::from_utf8_lossy(&result.stderr)
                .contains(&format!("must not alias the {artifact} input"))
        );
        ensure!(manifest.exists());
    }
    Ok(())
}

#[cfg(unix)]
#[test]
fn cli_rejects_copybook_alias_through_symlinked_parent() -> Result<()> {
    use std::os::unix::fs::symlink;

    let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
    let linked_parent = temp.path().join("linked-parent");
    symlink(temp.path(), &linked_parent)?;
    let copybook = temp.path().join("simple.cpy");
    let before = fs::read(&copybook)?;
    let result = run_cli(&manifest, &linked_parent.join("simple.cpy"))?;
    ensure!(!result.status.success());
    ensure!(fs::read(&copybook)? == before);
    ensure!(String::from_utf8_lossy(&result.stderr).contains("must not alias the copybook input"));
    Ok(())
}

#[cfg(unix)]
#[test]
fn cli_resolves_symlink_parent_components_before_aliasing() -> Result<()> {
    use std::os::unix::fs::symlink;

    let cases = [
        ("fixed-ascii.json", "manifest"),
        ("simple.cpy", "copybook"),
        ("fixed-ascii.bin", "dataset"),
    ];
    for (output_name, artifact) in cases {
        let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
        let nested = temp.path().join("nested/child");
        fs::create_dir_all(&nested)?;
        let linked_child = temp.path().join("linked-child");
        symlink(&nested, &linked_child)?;
        let output = linked_child.join("../..").join(output_name);
        let input_path = temp.path().join(output_name);
        let before = fs::read(&input_path)?;

        let result = run_cli(&manifest, &output)?;
        ensure!(!result.status.success());
        ensure!(fs::read(&input_path)? == before);
        ensure!(
            String::from_utf8_lossy(&result.stderr)
                .contains(&format!("must not alias the {artifact} input"))
        );
    }

    let (temp, manifest) = copy_fixture("fixed-ascii.json")?;
    let nested = temp.path().join("nested/child");
    fs::create_dir_all(&nested)?;
    let linked_child = temp.path().join("linked-child");
    symlink(&nested, &linked_child)?;
    let output = linked_child.join("../..").join("distinct-report.json");
    require_success(&run_cli(&manifest, &output)?)?;
    ensure!(temp.path().join("distinct-report.json").is_file());
    Ok(())
}

#[test]
fn workflow_is_manual_fixed_inventory_telemetry_only() -> Result<()> {
    let workflow = fs::read_to_string(
        workspace_root().join(".github/workflows/external-input-preflight.yml"),
    )?;
    ensure!(workflow.contains("workflow_dispatch: {}"));
    ensure!(!workflow.contains("schedule:"));
    ensure!(!workflow.contains("matrix:"));
    ensure!(!workflow.contains("inputs:"));
    ensure!(has_adjacent_lines(
        &workflow,
        "permissions:",
        "  contents: read"
    ));
    ensure!(workflow.contains("timeout-minutes: 15"));
    ensure!(!workflow.contains("continue-on-error"));

    let manifests = [
        "fixed-ascii.json",
        "fixed-cp037.json",
        "rdw-ascii.json",
        "rdw-cp037.json",
    ];
    let mut prior = 0_usize;
    for manifest in manifests {
        let inventory_entry = format!("external_input/{manifest} ");
        ensure!(workflow.matches(&inventory_entry).count() == 1);
        let position = workflow
            .find(&inventory_entry)
            .with_context(|| format!("workflow omits {manifest}"))?;
        ensure!(position > prior);
        prior = position;
    }
    let upload = workflow
        .find("uses: actions/upload-artifact@v7")
        .context("workflow omits artifact upload")?;
    ensure!(upload > prior);
    ensure!(workflow.contains("name: external-input-preflight-${{ github.sha }}"));
    ensure!(workflow.contains("if-no-files-found: error"));
    for forbidden in ["threshold", "throughput", "perf.json", "soak.yml"] {
        ensure!(!workflow.contains(forbidden));
    }
    Ok(())
}

#[test]
fn criterion_workflow_is_manual_fixed_inventory_telemetry_only() -> Result<()> {
    let workflow = fs::read_to_string(
        workspace_root().join(".github/workflows/external-input-criterion.yml"),
    )?;
    ensure!(has_exact_top_level_block(
        &workflow,
        "on:",
        &["  workflow_dispatch: {}"]
    ));
    for forbidden in [
        "schedule:",
        "matrix:",
        "inputs:",
        "continue-on-error",
        "threshold",
        "SLO",
        "perf.json",
        "receipt",
    ] {
        ensure!(!workflow.contains(forbidden));
    }
    ensure!(has_adjacent_lines(
        &workflow,
        "permissions:",
        "  contents: read"
    ));
    ensure!(workflow.contains("runs-on: ubuntu-latest"));
    ensure!(workflow.contains("timeout-minutes: 20"));
    ensure!(workflow.contains("persist-credentials: false"));
    ensure!(workflow.contains("set -euo pipefail"));
    ensure!(workflow.contains("--features external-input"));
    ensure!(has_exact_workspace_manifest_binding(&workflow));
    ensure!(workflow.contains("--warm-up-time 1 --measurement-time 1 --sample-size 10"));

    let manifests = [
        "tools/copybook-bench/test_fixtures/external_input/fixed-ascii.json",
        "tools/copybook-bench/test_fixtures/external_input/fixed-cp037.json",
        "tools/copybook-bench/test_fixtures/external_input/rdw-ascii.json",
        "tools/copybook-bench/test_fixtures/external_input/rdw-cp037.json",
    ];
    ensure!(shell_array_entries(&workflow, "manifests=(") == Some(manifests.to_vec()));
    let prior = workflow
        .find(manifests[3])
        .context("Criterion workflow omits final canonical manifest")?;
    let upload = workflow
        .find("uses: actions/upload-artifact@v7")
        .context("Criterion workflow omits artifact upload")?;
    ensure!(upload > prior);
    ensure!(workflow.contains("name: external-input-criterion-${{ github.sha }}"));
    ensure!(workflow.contains("path: target/criterion/external_input_decode/**"));
    ensure!(workflow.contains("if-no-files-found: error"));
    Ok(())
}

#[test]
fn workflow_contract_helpers_reject_extra_triggers_and_manifests() -> Result<()> {
    let expected_trigger = ["  workflow_dispatch: {}"];
    for accepted in [
        "on:\n  workflow_dispatch: {}\njobs:\n",
        "on:\r\n  workflow_dispatch: {}\r\njobs:\r\n",
    ] {
        ensure!(has_exact_top_level_block(
            accepted,
            "on:",
            &expected_trigger
        ));
    }
    for rejected in [
        "on:\n  workflow_dispatch: {}\n  push: {}\njobs:\n",
        "on:\n  pull_request: {}\n  workflow_dispatch: {}\njobs:\n",
        "on:\n  workflow_dispatch: {}\n  workflow_dispatch: {}\njobs:\n",
    ] {
        ensure!(!has_exact_top_level_block(
            rejected,
            "on:",
            &expected_trigger
        ));
    }

    let expected_manifests = ["fixed.json", "rdw.json"];
    for accepted in [
        "manifests=(\n  fixed.json\n  rdw.json\n)\n",
        "manifests=(\r\n  fixed.json\r\n  rdw.json\r\n)\r\n",
    ] {
        ensure!(shell_array_entries(accepted, "manifests=(") == Some(expected_manifests.to_vec()));
    }
    for rejected in [
        "manifests=(\n  fixed.json\n  rdw.json\n  extra.json\n)\n",
        "manifests=(\n  rdw.json\n  fixed.json\n)\n",
        "manifests=(\n  fixed.json\n)\n",
        "manifests=(\n  fixed.json\n  fixed.json\n  rdw.json\n)\n",
        "manifests=(\n  fixed.json\n  rdw.json\n",
        "manifests=(\n  fixed.json\n  rdw.json\n)\nmanifests=(\n  fixed.json\n  rdw.json\n)\n",
    ] {
        ensure!(shell_array_entries(rejected, "manifests=(") != Some(expected_manifests.to_vec()));
    }

    let binding = "COPYBOOK_EXTERNAL_INPUT_MANIFEST=\"$GITHUB_WORKSPACE/$manifest\" \\\n";
    ensure!(has_exact_workspace_manifest_binding(binding));
    ensure!(has_exact_workspace_manifest_binding(
        &binding.replace('\n', "\r\n")
    ));
    for rejected in [
        String::new(),
        "COPYBOOK_EXTERNAL_INPUT_MANIFES=\"$GITHUB_WORKSPACE/$manifest\" \\\n".to_string(),
        "COPYBOOK_EXTERNAL_INPUT_MANIFEST=\"$GITHUB_WORKSPACE/$manifest\" \\   \n".to_string(),
        "COPYBOOK_EXTERNAL_INPUT_MANIFEST=\"$manifest\" \\\n".to_string(),
        "COPYBOOK_EXTERNAL_INPUT_MANIFEST=\"$GITHUB_WORKSPACE$manifest\" \\\n".to_string(),
        format!("{binding}{binding}"),
    ] {
        ensure!(!has_exact_workspace_manifest_binding(&rejected));
        ensure!(!has_exact_workspace_manifest_binding(
            &rejected.replace('\n', "\r\n")
        ));
    }
    Ok(())
}
