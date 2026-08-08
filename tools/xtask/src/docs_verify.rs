// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use chrono::Utc;
use copybook_bench::{COMP3_CI_FLOOR_MIBPS, DISPLAY_FLOOR_MIBPS};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{
    collections::{BTreeMap, BTreeSet},
    env, fs,
    path::{Path, PathBuf},
    process::Command,
};

use super::{verify, verify_support_matrix};
use xtask::junit_xml_path;
use xtask::perf;

type Verifier = (&'static str, fn() -> Result<()>);

pub(crate) fn run() -> Result<()> {
    let checks: [Verifier; 16] = [
        (
            "workspace-version-and-msrv",
            verify_workspace_version_and_msrv,
        ),
        ("facade-invariants", verify_facade_invariants),
        (
            "copybook-rs-redirect",
            verify_copybook_rs_redirect_invariant,
        ),
        ("error-code-inventory", verify_error_code_inventory),
        ("record-pipeline-evidence", verify_record_pipeline_evidence),
        ("test-status", verify_test_status_if_present),
        ("support-matrix", verify_support_matrix),
        (
            "performance-floor-and-receipt",
            verify_performance_floor_and_receipt,
        ),
        ("cli-inventory", verify_cli_inventory),
        (
            "publish-workflow-inventory",
            verify_publish_workflow_inventory,
        ),
        ("stability-registry", verify_stability_registry),
        (
            "stable-contract-inventory",
            verify_stable_contract_inventory,
        ),
        ("deprecation-audit", verify_deprecation_audit),
        (
            "surface-deprecation-audit",
            verify_surface_deprecation_audit,
        ),
        ("quick-start-versioning", verify_quick_start_versioning),
        ("status-versioning", verify_status_versioning),
    ];
    run_checks(&checks)
}

pub(crate) fn run_contracts_command() -> Result<()> {
    generate_stable_contract_manifest()
        .and_then(|manifest| persist_stable_contract_manifest(&manifest))
}

pub(crate) fn verify_record_pipeline_command() -> Result<()> {
    verify_record_pipeline_evidence()
}

pub(crate) fn run_freeze_contract_checks() -> Result<()> {
    let checks: [Verifier; 3] = [
        ("error-code-inventory", verify_error_code_inventory),
        ("cli-inventory", verify_cli_inventory),
        (
            "stable-contract-inventory",
            verify_stable_contract_inventory_strict,
        ),
    ];
    run_checks(&checks)
}

fn verify_contracts_with_strictness(
    baseline: &StableSurfaceContractManifest,
    current: &StableSurfaceContractManifest,
    fail_on_additions: bool,
) -> Result<()> {
    let expected_source_paths: BTreeSet<String> = STABLE_CONTRACT_SOURCE_PATHS
        .iter()
        .map(ToString::to_string)
        .collect();
    let baseline_source_paths: BTreeSet<String> = baseline.source_paths.iter().cloned().collect();
    if baseline_source_paths != expected_source_paths {
        bail!(
            "stable-contract manifest source paths are stale: expected {expected_source_paths:?}, found {baseline_source_paths:?} | run `cargo run -p xtask -- docs contracts generate`"
        );
    }

    let current_source_paths: BTreeSet<String> = current.source_paths.iter().cloned().collect();
    if current_source_paths != expected_source_paths {
        bail!(
            "stable-contract collector source paths drifted: expected {expected_source_paths:?}, found {current_source_paths:?}"
        );
    }

    let deltas = diff_contracts(baseline, current);

    if !deltas.removed.is_empty() {
        let mut lines = Vec::new();
        for item in deltas.removed {
            lines.push(format!("- {} {}", item.category, item.item));
        }
        lines.sort_unstable();
        bail!(
            "stable-contract inventory has breaking changes:\n{}",
            lines.join("\n")
        );
    }

    if !fail_on_additions {
        if !deltas.added.is_empty() {
            let mut lines = Vec::new();
            for item in deltas.added {
                lines.push(format!("- {} {}", item.category, item.item));
            }
            lines.sort_unstable();
            println!(
                "stable-contract inventory additions (non-blocking):\n{}",
                lines.join("\n")
            );
        }
        return Ok(());
    }

    if !deltas.added.is_empty() {
        let mut lines = Vec::new();
        for item in deltas.added {
            lines.push(format!("- {} {}", item.category, item.item));
        }
        lines.sort_unstable();
        bail!(
            "stable-contract inventory has incompatible changes:\n{}",
            lines.join("\n")
        );
    }

    Ok(())
}

fn verify_stable_contract_inventory() -> Result<()> {
    let baseline = load_stable_contract_manifest()?;
    if baseline.schema_version != STABLE_CONTRACT_SCHEMA_VERSION {
        bail!(
            "stable contract manifest schema mismatch: expected {STABLE_CONTRACT_SCHEMA_VERSION}, found {}",
            baseline.schema_version
        );
    }

    let current = collect_stable_contract_inventory()?;
    verify_contracts_with_strictness(&baseline, &current, false)
}

fn verify_stable_contract_inventory_strict() -> Result<()> {
    let baseline = load_stable_contract_manifest()?;
    if baseline.schema_version != STABLE_CONTRACT_SCHEMA_VERSION {
        bail!(
            "stable contract manifest schema mismatch: expected {STABLE_CONTRACT_SCHEMA_VERSION}, found {}",
            baseline.schema_version
        );
    }

    let current = collect_stable_contract_inventory()?;
    verify_contracts_with_strictness(&baseline, &current, true)
}

fn run_checks(checks: &[Verifier]) -> Result<()> {
    for (name, check) in checks {
        check().map_err(|err| anyhow::anyhow!("{name} failed: {err}"))?;
    }

    println!("docs verify-all completed");
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

const STABILITY_SCHEMA_VERSION: &str = "1.0.0";
const STABILITY_REGISTRY_PATH: &str = "docs/stability/surface-registry.json";
const RECORD_PIPELINE_EVIDENCE_PATH: &str = "docs/evidence/fixed-rdw-pipeline.toml";
const RECORD_PIPELINE_SOURCE_PATHS: [&str; 6] = [
    "crates/copybook-codec",
    "crates/copybook-fixed",
    "crates/copybook-rdw",
    "crates/copybook-cli",
    "crates/copybook-record-io",
    "tests/e2e",
];
const STABLE_CONTRACT_SCHEMA_VERSION: &str = "1.0.0";
const STABLE_CONTRACT_MANIFEST_PATH: &str = "docs/contracts/stable-surface-contract.json";
const STABLE_CONTRACT_SOURCE_PATHS: [&str; 6] = [
    "crates/copybook-cli/src/main.rs",
    "crates/copybook-cli/src/exit_codes.rs",
    "crates/copybook-error/src/lib.rs",
    "crates/copybook-codec/src/options.rs",
    "schemas/record-format.json",
    "docs/CLI_REFERENCE.md",
];
const DEPRECATION_AUDIT_PATH: &str = "docs/reports/deprecation-audit.json";
const SURFACE_DEPRECATION_AUDIT_PATH: &str = "docs/reports/surface-deprecation-audit.json";
const SURFACE_DEPRECATION_SURFACES: [&str; 5] = ["cli", "schema", "error", "config", "output"];
const SURFACE_DEPRECATION_STATES: [&str; 2] = ["deprecated", "none"];
const STABILITY_MANUAL_REVIEW_PLACEHOLDERS: [&str; 2] = ["tbd", "set during manual review"];

#[derive(Debug, Serialize, Deserialize)]
struct StableSurfaceContractManifest {
    schema_version: String,
    generated_at: String,
    generated_by: String,
    source_revision: String,
    source_paths: Vec<String>,
    cli: ContractCliInventory,
    error: ContractErrorInventory,
    exit_code: ContractExitCodeInventory,
    jsonl: ContractJsonlInventory,
    raw_capture: ContractRawCaptureInventory,
}

#[derive(Debug, Serialize, Deserialize)]
struct ContractCliInventory {
    commands: Vec<String>,
    options: Vec<String>,
    env_vars: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ContractErrorInventory {
    variants: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RecordPipelineEvidence {
    schema_version: u32,
    scope: String,
    verified_against: String,
    scenarios: Vec<RecordPipelineScenario>,
}

#[derive(Debug, Deserialize)]
struct RecordPipelineScenario {
    id: String,
    record_formats: Vec<String>,
    api_tests: Vec<String>,
    cli_tests: Vec<String>,
    error_codes: Vec<String>,
    cli_commands: Vec<String>,
    known_limitation: String,
}

fn verify_record_pipeline_evidence() -> Result<()> {
    let root = workspace_root();
    let registry_path = root.join(RECORD_PIPELINE_EVIDENCE_PATH);
    let source = fs::read_to_string(&registry_path).with_context(|| {
        format!(
            "loading fixed/RDW evidence registry {}",
            registry_path.display()
        )
    })?;
    let registry: RecordPipelineEvidence =
        toml::from_str(&source).with_context(|| format!("parsing {}", registry_path.display()))?;

    if registry.schema_version != 1 {
        bail!(
            "unsupported fixed/RDW evidence registry schema version {}",
            registry.schema_version
        );
    }
    if registry.scope != "fixed-rdw-pipeline" {
        bail!(
            "unexpected fixed/RDW evidence registry scope `{}`",
            registry.scope
        );
    }
    if registry.scenarios.is_empty() {
        bail!("fixed/RDW evidence registry contains no scenarios");
    }

    let sha = registry.verified_against.trim();
    if sha.len() != 40 || !sha.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        bail!("verified_against must be a 40-character commit SHA, found `{sha}`");
    }

    let commit_check = Command::new("git")
        .current_dir(&root)
        .args(["cat-file", "-e", &format!("{sha}^{{commit}}")])
        .output()
        .context("checking fixed/RDW evidence registry commit")?;
    match commit_check.status.code() {
        Some(0) => verify_record_pipeline_commit_ancestry(&root, sha)?,
        Some(1) if is_shallow_repository(&root)? => {
            println!(
                "fixed/RDW evidence commit `{sha}` unavailable in shallow checkout; ancestry and drift checks skipped"
            );
        }
        Some(1) => bail!("fixed/RDW evidence registry commit `{sha}` is not available"),
        other => bail!(
            "git cat-file failed while checking fixed/RDW evidence registry commit (exit {other:?}): {}",
            String::from_utf8_lossy(&commit_check.stderr).trim()
        ),
    }

    let error_code_source = fs::read_to_string(root.join("crates/copybook-error/src/lib.rs"))
        .context("loading crates/copybook-error/src/lib.rs")?;
    let error_codes = parse_error_code_variants(&error_code_source)?;
    verify_record_pipeline_scenarios(&root, &registry.scenarios, &error_codes)?;

    println!(
        "fixed/RDW evidence registry verified: {} scenarios at {}",
        registry.scenarios.len(),
        sha
    );
    Ok(())
}

fn verify_record_pipeline_commit_ancestry(root: &Path, sha: &str) -> Result<()> {
    let ancestor_check = Command::new("git")
        .current_dir(root)
        .args(["merge-base", "--is-ancestor", sha, "HEAD"])
        .output()
        .context("checking fixed/RDW evidence registry commit ancestry")?;
    match ancestor_check.status.code() {
        Some(0) => {}
        Some(1) => {
            bail!(
                "fixed/RDW evidence was verified against `{sha}`, which is not an ancestor of HEAD"
            )
        }
        other => bail!(
            "git merge-base failed while checking fixed/RDW evidence ancestry (exit {other:?}): {}",
            String::from_utf8_lossy(&ancestor_check.stderr).trim()
        ),
    }

    let diff_check = Command::new("git")
        .current_dir(root)
        .args(["diff", "--quiet", &format!("{sha}..HEAD"), "--"])
        .args(RECORD_PIPELINE_SOURCE_PATHS)
        .output()
        .context("checking fixed/RDW evidence source drift")?;
    match diff_check.status.code() {
        Some(0) => {}
        Some(1) => bail!(
            "fixed/RDW evidence source paths changed after verified commit `{sha}`; update the registry"
        ),
        other => bail!(
            "git diff failed while checking fixed/RDW evidence source drift (exit {other:?}): {}",
            String::from_utf8_lossy(&diff_check.stderr).trim()
        ),
    }
    Ok(())
}

fn is_shallow_repository(root: &Path) -> Result<bool> {
    let output = Command::new("git")
        .current_dir(root)
        .args(["rev-parse", "--is-shallow-repository"])
        .output()
        .context("checking repository depth for fixed/RDW evidence")?;
    Ok(output.status.success() && String::from_utf8_lossy(&output.stdout).trim() == "true")
}

fn verify_record_pipeline_scenarios(
    root: &Path,
    scenarios: &[RecordPipelineScenario],
    error_codes: &[String],
) -> Result<()> {
    let mut scenario_ids = BTreeSet::new();
    for scenario in scenarios {
        if !scenario_ids.insert(&scenario.id) {
            bail!("duplicate fixed/RDW evidence scenario `{}`", scenario.id);
        }
        if scenario.record_formats.is_empty() {
            bail!("scenario `{}` has no record format", scenario.id);
        }
        if scenario
            .record_formats
            .iter()
            .any(|format| !matches!(format.as_str(), "fixed" | "rdw"))
        {
            bail!(
                "scenario `{}` contains a record format outside the fixed/RDW scope",
                scenario.id
            );
        }
        if scenario.api_tests.is_empty() && scenario.cli_tests.is_empty() {
            bail!("scenario `{}` has no test anchor", scenario.id);
        }
        if scenario.cli_tests.is_empty() != scenario.cli_commands.is_empty() {
            bail!(
                "scenario `{}` must keep CLI command and CLI test anchors in sync",
                scenario.id
            );
        }
        if scenario.known_limitation.trim().is_empty() {
            bail!("scenario `{}` has no limitation statement", scenario.id);
        }
        for anchor in scenario.api_tests.iter().chain(&scenario.cli_tests) {
            verify_test_anchor(root, anchor, &scenario.id)?;
        }
        for error_code in &scenario.error_codes {
            if !error_codes.contains(error_code) {
                bail!(
                    "scenario `{}` references unknown stable error code `{error_code}`",
                    scenario.id
                );
            }
        }
    }
    Ok(())
}

fn verify_test_anchor(root: &Path, anchor: &str, scenario_id: &str) -> Result<()> {
    let (path, symbol) = anchor.rsplit_once("::").ok_or_else(|| {
        anyhow::anyhow!(
            "scenario `{scenario_id}` has malformed test anchor `{anchor}`; expected path::function"
        )
    })?;
    let source_path = root.join(path);
    let source = fs::read_to_string(&source_path)
        .with_context(|| format!("loading test anchor `{anchor}` for scenario `{scenario_id}`"))?;
    let function_anchor = format!("fn {symbol}");
    let declares_function = source.match_indices(&function_anchor).any(|(index, _)| {
        source[index + function_anchor.len()..]
            .chars()
            .next()
            .is_none_or(|next| !next.is_alphanumeric() && next != '_')
    });
    if !declares_function {
        bail!("scenario `{scenario_id}` anchor `{anchor}` does not name an existing function");
    }
    Ok(())
}

#[derive(Debug, Serialize, Deserialize)]
struct ContractExitCodeInventory {
    variants: Vec<String>,
    tags: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ContractJsonlInventory {
    schema_keys: Vec<String>,
    required_keys: Vec<String>,
    pattern_properties: Vec<String>,
    compatibility_keys: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ContractRawCaptureInventory {
    modes: Vec<String>,
    emitted_keys: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct DeprecatedAuditReport {
    schema_version: String,
    generated_at: String,
    generated_by: String,
    items: Vec<DeprecatedApiItem>,
}

#[derive(Debug, Serialize, Deserialize)]
struct DeprecatedApiItem {
    path: String,
    symbol: String,
    kind: String,
    deprecated_since: String,
    replacement: String,
    migration_example: String,
    planned_removal: String,
    compatibility_impact: String,
    note: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct SurfaceDeprecationAuditReport {
    schema_version: String,
    generated_at: String,
    generated_by: String,
    entries: Vec<SurfaceDeprecationAuditEntry>,
}

#[derive(Debug, Serialize, Deserialize)]
struct SurfaceDeprecationAuditEntry {
    surface: String,
    state: String,
    path: String,
    symbol: String,
    kind: String,
    deprecated_since: Option<String>,
    replacement: Option<String>,
    migration_example: Option<String>,
    planned_removal: Option<String>,
    compatibility_impact: Option<String>,
    note: String,
}

#[derive(Debug)]
struct DeprecatedAttribute {
    since: Option<String>,
}

#[derive(Debug)]
struct DiscoveredDeprecatedItem {
    path: String,
    symbol: String,
    kind: String,
    since: Option<String>,
}

#[derive(Debug)]
struct ContractDelta {
    category: &'static str,
    item: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StabilityClass {
    Stable,
    Beta,
    Experimental,
    InternalDevOnly,
}

impl StabilityClass {
    fn requires_contracts(self) -> bool {
        matches!(self, Self::Beta | Self::Experimental)
    }
}

fn parse_stability_class(value: &str, context: &str) -> Result<StabilityClass> {
    match value {
        "stable" => Ok(StabilityClass::Stable),
        "beta" => Ok(StabilityClass::Beta),
        "experimental" => Ok(StabilityClass::Experimental),
        "internal-dev-only" => Ok(StabilityClass::InternalDevOnly),
        other => bail!("unknown stability class `{other}` in {context}"),
    }
}

#[derive(Debug, Deserialize)]
struct StabilityRegistry {
    schema_version: String,
    packages: Vec<StabilityPackage>,
}

#[derive(Debug, Deserialize)]
struct StabilityPackage {
    name: String,
    publish: bool,
    class: String,
    stability_statement: String,
    limitations: Vec<String>,
    graduation_criteria: Vec<String>,
    source_of_truth: Vec<String>,
    #[serde(default)]
    features: Vec<StabilityFeature>,
}

#[derive(Debug, Deserialize)]
struct StabilityFeature {
    name: String,
    class: String,
    stability_statement: String,
    limitations: Vec<String>,
    graduation_criteria: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadata {
    workspace_members: Vec<String>,
    packages: Vec<CargoMetadataPackage>,
}

#[derive(Debug, Deserialize)]
struct CargoMetadataPackage {
    id: String,
    name: String,
    #[serde(default)]
    features: BTreeMap<String, Value>,
    #[serde(default)]
    publish: Option<Value>,
}

#[derive(Debug, Clone)]
struct WorkspacePackageInventory {
    publish: bool,
    features: BTreeSet<String>,
}

fn verify_stability_registry() -> Result<()> {
    let registry = load_stability_registry()?;
    let metadata = load_cargo_metadata()?;
    verify_stability_registry_against_metadata(&registry, &metadata)
}

fn verify_deprecation_audit() -> Result<()> {
    let audit = load_deprecation_audit_report()?;
    let discovered = collect_deprecated_api_inventory()?;

    if discovered.is_empty() {
        bail!("no deprecated API declarations were discovered in repository sources");
    }

    let mut discovered_by_key = BTreeMap::new();
    for item in discovered {
        let key = format!("{}:{}", item.path, item.symbol);
        discovered_by_key.insert(key, item);
    }

    let mut report_by_key = BTreeMap::new();
    for item in &audit.items {
        let key = format!("{}:{}", item.path, item.symbol);
        report_by_key.insert(key, item);
    }

    let mut missing: Vec<String> = discovered_by_key
        .keys()
        .filter(|key| !report_by_key.contains_key(*key))
        .cloned()
        .collect();

    if !missing.is_empty() {
        missing.sort_unstable();
        bail!(
            "deprecation audit is missing required entries: {} | authoritative-source=crates and docs/reports/deprecation-audit.json",
            missing.join(", ")
        );
    }

    let mut extra: Vec<String> = report_by_key
        .keys()
        .filter(|key| !discovered_by_key.contains_key(*key))
        .cloned()
        .collect();

    if !extra.is_empty() {
        extra.sort_unstable();
        bail!(
            "deprecation audit has stale entries: {} | authoritative-source=crates",
            extra.join(", ")
        );
    }

    for item in report_by_key.values() {
        if item.deprecated_since.trim().is_empty()
            || item.replacement.trim().is_empty()
            || item.migration_example.trim().is_empty()
            || item.planned_removal.trim().is_empty()
            || item.compatibility_impact.trim().is_empty()
            || item.note.trim().is_empty()
        {
            bail!(
                "deprecation audit entry `{}` missing required evidence fields | authoritative-source=docs/reports/deprecation-audit.json",
                item.symbol
            );
        }

        let key = format!("{}:{}", item.path, item.symbol);
        let discovered_item = discovered_by_key.get(&key).ok_or_else(|| {
            anyhow::anyhow!("deprecation audit item `{key}` missing from discovered inventory")
        })?;

        if discovered_item.kind != item.kind {
            bail!(
                "deprecation audit kind mismatch for {}: report={} discovered={}",
                item.symbol,
                item.kind,
                discovered_item.kind
            );
        }

        if item.deprecated_since
            != discovered_item
                .since
                .as_ref()
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "deprecation audit item `{}` is missing deprecated since in source",
                        item.symbol
                    )
                })?
                .as_str()
        {
            bail!(
                "deprecation audit item `{}` stale `deprecated_since`: report={} source={}",
                item.symbol,
                item.deprecated_since,
                discovered_item.since.as_deref().unwrap_or(""),
            );
        }
    }

    println!(
        "\u{2713} Deprecation audit inventory coverage verified ({} entries)",
        audit.items.len()
    );
    Ok(())
}

fn verify_surface_deprecation_audit() -> Result<()> {
    let audit = load_surface_deprecation_audit_report()?;

    if audit.entries.is_empty() {
        bail!("surface deprecation audit is empty");
    }

    let mut by_surface: BTreeMap<&str, Vec<&SurfaceDeprecationAuditEntry>> = BTreeMap::new();
    for entry in &audit.entries {
        validate_surface_deprecation_audit_entry(entry)?;

        by_surface
            .entry(entry.surface.as_str())
            .or_default()
            .push(entry);
    }

    validate_surface_deprecation_audit_output(&by_surface)?;

    println!(
        "\u{2713} Surface deprecation audit coverage verified ({} entries)",
        audit.entries.len()
    );
    Ok(())
}

fn validate_surface_deprecation_audit_entry(entry: &SurfaceDeprecationAuditEntry) -> Result<()> {
    if !SURFACE_DEPRECATION_SURFACES.contains(&entry.surface.as_str()) {
        bail!(
            "surface deprecation audit contains unknown surface `{}` in entry `{}`",
            entry.surface,
            entry.symbol
        );
    }
    if !SURFACE_DEPRECATION_STATES.contains(&entry.state.as_str()) {
        bail!(
            "surface deprecation audit has invalid state `{}` for `{}`",
            entry.state,
            entry.symbol
        );
    }

    let resolved_entry_path = resolve_workspace_file(&entry.path).ok_or_else(|| {
        anyhow::anyhow!(
            "surface deprecation audit entry `{}` references unresolved path `{}`",
            entry.symbol,
            entry.path
        )
    })?;
    if !resolved_entry_path.exists() {
        bail!(
            "surface deprecation audit entry `{}` references missing path `{}`",
            entry.symbol,
            entry.path
        );
    }

    if entry.symbol.trim().is_empty()
        || entry.kind.trim().is_empty()
        || entry.note.trim().is_empty()
    {
        bail!("surface deprecation audit entry has empty `symbol`, `kind`, or `note`");
    }

    if entry.state == "deprecated" {
        validate_surface_deprecated_entry(entry)?;
    } else if entry.deprecated_since.is_some()
        || entry.replacement.is_some()
        || entry.migration_example.is_some()
        || entry.planned_removal.is_some()
        || entry.compatibility_impact.is_some()
    {
        bail!(
            "surface deprecation audit entry `{}` has `state: none` but migration fields",
            entry.symbol
        );
    }

    Ok(())
}

fn validate_surface_deprecated_entry(entry: &SurfaceDeprecationAuditEntry) -> Result<()> {
    let required = [
        (&entry.deprecated_since, "deprecated_since"),
        (&entry.replacement, "replacement"),
        (&entry.migration_example, "migration_example"),
        (&entry.planned_removal, "planned_removal"),
        (&entry.compatibility_impact, "compatibility_impact"),
    ];
    for (value, field_name) in required {
        let Some(value) = value.as_ref().map(|value| value.trim()) else {
            bail!(
                "surface deprecation audit deprecated entry `{}` missing required field `{field_name}`",
                entry.symbol
            );
        };
        if value.is_empty() {
            bail!(
                "surface deprecation audit deprecated entry `{}` has empty required field `{field_name}`",
                entry.symbol
            );
        }
    }

    let known_output_symbols = ["__schema_id", "__raw_b64"];
    if entry.surface == "output" && !known_output_symbols.contains(&entry.symbol.as_str()) {
        bail!(
            "surface deprecation audit has unexpected deprecated output symbol `{}`",
            entry.symbol
        );
    }

    Ok(())
}

fn validate_surface_deprecation_audit_output(
    by_surface: &BTreeMap<&str, Vec<&SurfaceDeprecationAuditEntry>>,
) -> Result<()> {
    for surface in SURFACE_DEPRECATION_SURFACES {
        if !by_surface.contains_key(surface) {
            bail!(
                "surface deprecation audit missing required surface `{surface}` in {SURFACE_DEPRECATION_AUDIT_PATH}"
            );
        }
    }

    let output_entries: &[&SurfaceDeprecationAuditEntry] = by_surface
        .get("output")
        .map_or(&[], |entries| entries.as_slice());
    let has_schema_id = output_entries
        .iter()
        .any(|entry| entry.symbol == "__schema_id" && entry.state == "deprecated");
    let has_raw_b64 = output_entries
        .iter()
        .any(|entry| entry.symbol == "__raw_b64" && entry.state == "deprecated");
    if !has_schema_id || !has_raw_b64 {
        bail!(
            "surface deprecation audit output surface must include deprecated `__schema_id` and `__raw_b64`; found {:?}",
            output_entries
                .iter()
                .map(|entry| &entry.symbol)
                .collect::<Vec<_>>()
        );
    }
    Ok(())
}

fn collect_stable_contract_inventory() -> Result<StableSurfaceContractManifest> {
    let cli_source = fs::read_to_string("crates/copybook-cli/src/main.rs")
        .context("loading crates/copybook-cli/src/main.rs")?;
    let cli_reference =
        fs::read_to_string("docs/CLI_REFERENCE.md").context("loading docs/CLI_REFERENCE.md")?;
    let exit_code_source = fs::read_to_string("crates/copybook-cli/src/exit_codes.rs")
        .context("loading crates/copybook-cli/src/exit_codes.rs")?;
    let error_source = fs::read_to_string("crates/copybook-error/src/lib.rs")
        .context("loading crates/copybook-error/src/lib.rs")?;
    let raw_mode_source = fs::read_to_string("crates/copybook-codec/src/options.rs")
        .context("loading crates/copybook-codec/src/options.rs")?;
    let jsonl_schema_source = fs::read_to_string("schemas/record-format.json")
        .context("loading schemas/record-format.json")?;

    let cli_contract = ContractCliInventory {
        commands: parse_cli_command_variants(&cli_source)?,
        options: parse_cli_reference_option_contracts(&cli_reference)?,
        env_vars: parse_cli_reference_env_contracts(&cli_reference)?,
    };
    let error_contract = ContractErrorInventory {
        variants: parse_error_code_variants(&error_source)?,
    };
    let exit_code_inventory = parse_exit_code_inventory(&exit_code_source)?;
    let (schema_keys, required_keys, pattern_properties) =
        parse_jsonl_schema_inventory(&jsonl_schema_source)?;
    let raw_capture_inventory = ContractRawCaptureInventory {
        modes: parse_raw_mode_variants(&raw_mode_source)?,
        emitted_keys: parse_cli_reference_raw_keys(&cli_reference)?,
    };

    let source_revision = git_head_revision().unwrap_or_else(|_| "unknown".to_string());
    Ok(StableSurfaceContractManifest {
        schema_version: STABLE_CONTRACT_SCHEMA_VERSION.to_string(),
        generated_at: Utc::now().to_rfc3339(),
        generated_by: "cargo run -p xtask -- docs contracts generate".to_string(),
        source_revision,
        source_paths: STABLE_CONTRACT_SOURCE_PATHS
            .iter()
            .map(ToString::to_string)
            .collect(),
        cli: cli_contract,
        error: error_contract,
        exit_code: exit_code_inventory,
        jsonl: ContractJsonlInventory {
            schema_keys,
            required_keys,
            pattern_properties,
            compatibility_keys: vec!["__raw_b64".to_string(), "raw_b64".to_string()],
        },
        raw_capture: raw_capture_inventory,
    })
}

fn generate_stable_contract_manifest() -> Result<StableSurfaceContractManifest> {
    collect_stable_contract_inventory()
}

fn persist_stable_contract_manifest(manifest: &StableSurfaceContractManifest) -> Result<()> {
    let output_path = resolve_workspace_file("docs").map(|docs_dir| {
        if docs_dir.is_file() {
            docs_dir.with_file_name("contracts/stable-surface-contract.json")
        } else {
            docs_dir.join("contracts/stable-surface-contract.json")
        }
    });
    let output_path = output_path.ok_or_else(|| {
        anyhow::anyhow!("unable to resolve docs/contracts/stable-surface-contract.json location")
    })?;
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("creating {}", parent.display()))?;
    }

    let payload =
        serde_json::to_string_pretty(manifest).context("serializing stable contract manifest")?;
    fs::write(&output_path, format!("{payload}\n")).with_context(|| {
        format!(
            "writing stable contract manifest to {}",
            output_path.display()
        )
    })?;
    println!(
        "stable-contract inventory updated at {}",
        output_path.display()
    );
    Ok(())
}

fn load_stable_contract_manifest() -> Result<StableSurfaceContractManifest> {
    let manifest_path = resolve_workspace_file(STABLE_CONTRACT_MANIFEST_PATH)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "unable to resolve stable contract manifest path; run `cargo run -p xtask -- docs contracts generate`"
            )
        })?;
    let source = fs::read_to_string(&manifest_path)
        .with_context(|| format!("loading {}", manifest_path.display()))?;
    serde_json::from_str(&source).context("parsing docs/contracts/stable-surface-contract.json")
}

fn diff_contracts(
    baseline: &StableSurfaceContractManifest,
    current: &StableSurfaceContractManifest,
) -> ContractDiffSet {
    let mut deltas = ContractDiffSet {
        added: Vec::new(),
        removed: Vec::new(),
    };

    let baseline_source_paths: BTreeSet<String> = baseline.source_paths.iter().cloned().collect();
    let current_paths: BTreeSet<String> = current.source_paths.iter().cloned().collect();
    classify_delta_items(
        &baseline_source_paths,
        &current_paths,
        "contract-source-path",
        &mut deltas,
    );

    classify_delta_items(
        &baseline.cli.commands.iter().cloned().collect(),
        &current.cli.commands.iter().cloned().collect(),
        "cli.command",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.cli.options.iter().cloned().collect(),
        &current.cli.options.iter().cloned().collect(),
        "cli.option",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.cli.env_vars.iter().cloned().collect(),
        &current.cli.env_vars.iter().cloned().collect(),
        "cli.env-var",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.error.variants.iter().cloned().collect(),
        &current.error.variants.iter().cloned().collect(),
        "error-code",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.exit_code.variants.iter().cloned().collect(),
        &current.exit_code.variants.iter().cloned().collect(),
        "cli.exit-code-variant",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.exit_code.tags.iter().cloned().collect(),
        &current.exit_code.tags.iter().cloned().collect(),
        "cli.exit-code-tag",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.jsonl.schema_keys.iter().cloned().collect(),
        &current.jsonl.schema_keys.iter().cloned().collect(),
        "jsonl.schema-key",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.jsonl.required_keys.iter().cloned().collect(),
        &current.jsonl.required_keys.iter().cloned().collect(),
        "jsonl.required-key",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.jsonl.pattern_properties.iter().cloned().collect(),
        &current.jsonl.pattern_properties.iter().cloned().collect(),
        "jsonl.pattern-key",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.jsonl.compatibility_keys.iter().cloned().collect(),
        &current.jsonl.compatibility_keys.iter().cloned().collect(),
        "jsonl.compat-key",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.raw_capture.modes.iter().cloned().collect(),
        &current.raw_capture.modes.iter().cloned().collect(),
        "raw.mode",
        &mut deltas,
    );
    classify_delta_items(
        &baseline.raw_capture.emitted_keys.iter().cloned().collect(),
        &current.raw_capture.emitted_keys.iter().cloned().collect(),
        "raw.key",
        &mut deltas,
    );

    deltas
}

#[derive(Debug)]
struct ContractDiffSet {
    added: Vec<ContractDelta>,
    removed: Vec<ContractDelta>,
}

fn classify_delta_items(
    baseline: &BTreeSet<String>,
    current: &BTreeSet<String>,
    category: &'static str,
    deltas: &mut ContractDiffSet,
) {
    for item in baseline.difference(current) {
        deltas.removed.push(ContractDelta {
            category,
            item: item.clone(),
        });
    }
    for item in current.difference(baseline) {
        deltas.added.push(ContractDelta {
            category,
            item: item.clone(),
        });
    }
}

fn parse_cli_reference_option_contracts(source: &str) -> Result<Vec<String>> {
    let option_re = Regex::new(r"--([a-z0-9][a-z0-9_-]*)")?;
    let mut options = BTreeSet::new();
    for capture in option_re.captures_iter(source) {
        options.insert(capture[1].replace('_', "-"));
    }
    if options.is_empty() {
        bail!("CLI reference option inventory is empty");
    }
    Ok(options.into_iter().collect())
}

fn parse_cli_reference_env_contracts(source: &str) -> Result<Vec<String>> {
    let env_re = Regex::new(r"\b(COPYBOOK_[A-Z0-9]+(?:_[A-Z0-9]+)*)\b")?;
    let mut vars = BTreeSet::new();
    for capture in env_re.captures_iter(source) {
        vars.insert(capture[1].to_string());
    }
    if vars.is_empty() {
        bail!("CLI reference environment variable inventory is empty");
    }
    Ok(vars.into_iter().collect())
}

fn parse_exit_code_inventory(source: &str) -> Result<ContractExitCodeInventory> {
    let variants = parse_exit_code_variants(source)?;
    let tags: BTreeSet<String> = variants
        .iter()
        .filter_map(|variant| match variant.as_str() {
            "Data" => Some("CBKD"),
            "Encode" => Some("CBKE"),
            "Format" => Some("CBKF"),
            "Internal" => Some("CBKI"),
            _ => None,
        })
        .map(ToString::to_string)
        .collect();

    if variants.is_empty() {
        bail!("no ExitCode variants parsed from copybook-cli/src/exit_codes.rs");
    }

    Ok(ContractExitCodeInventory {
        variants,
        tags: tags.into_iter().collect(),
    })
}

fn parse_exit_code_variants(source: &str) -> Result<Vec<String>> {
    let start_re = Regex::new(r"(?m)^pub enum ExitCode \{")?;
    let start = start_re
        .find(source)
        .ok_or_else(|| anyhow::anyhow!("could not find ExitCode enum"))?
        .end();

    let mut depth = 1i32;
    let mut end = source.len();
    for (offset, ch) in source[start..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    end = start + offset;
                    break;
                }
            }
            _ => {}
        }
    }

    if end <= start {
        bail!("could not parse ExitCode enum body");
    }

    let remainder = &source[start..end];
    let variant_re =
        Regex::new(r"(?m)^\s*([A-Za-z][A-Za-z0-9_]*)\s*(?:=\s*[^,]+)?\s*,?(?:\s*//.*)?$")?;
    let mut variants = BTreeSet::new();
    for capture in variant_re.captures_iter(remainder) {
        variants.insert(capture[1].to_string());
    }
    if variants.is_empty() {
        bail!("no ExitCode variants parsed");
    }

    Ok(variants.into_iter().collect())
}

fn parse_error_code_variants(source: &str) -> Result<Vec<String>> {
    let start_re = Regex::new(r"(?m)^pub enum ErrorCode \{")?;
    let start = start_re
        .find(source)
        .ok_or_else(|| anyhow::anyhow!("could not find ErrorCode enum"))?
        .end();

    let mut depth = 1i32;
    let mut end = source.len();
    for (offset, ch) in source[start..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    end = start + offset;
                    break;
                }
            }
            _ => {}
        }
    }
    if end <= start {
        bail!("could not parse ErrorCode enum body");
    }

    let remainder = &source[start..end];
    let variant_re = Regex::new(r"(?m)^\s*([A-Z][A-Z0-9_]*)\s*(?:=\s*[^,]+)?\s*,(?:\s*//.*)?$")?;
    let mut variants = BTreeSet::new();
    for capture in variant_re.captures_iter(remainder) {
        variants.insert(capture[1].to_string());
    }
    if variants.is_empty() {
        bail!("no ErrorCode variants parsed");
    }
    Ok(variants.into_iter().collect())
}

fn parse_jsonl_schema_inventory(source: &str) -> Result<(Vec<String>, Vec<String>, Vec<String>)> {
    let value: Value =
        serde_json::from_str(source).context("parsing schemas/record-format.json")?;
    let properties = value
        .get("properties")
        .and_then(|value| value.as_object())
        .ok_or_else(|| anyhow::anyhow!("record-format.json missing object `properties`"))?;
    let required = value
        .get("required")
        .and_then(|value| value.as_array())
        .ok_or_else(|| anyhow::anyhow!("record-format.json missing array `required`"))?
        .iter()
        .filter_map(|entry| entry.as_str())
        .map(ToString::to_string)
        .collect();
    let pattern_properties = value
        .get("patternProperties")
        .and_then(|value| value.as_object())
        .map(|object| object.keys().map(ToString::to_string).collect())
        .unwrap_or_default();

    Ok((
        properties.keys().map(ToString::to_string).collect(),
        required,
        pattern_properties,
    ))
}

fn parse_cli_reference_raw_keys(source: &str) -> Result<Vec<String>> {
    let key_re = Regex::new(r"`([^`]+raw_b64[^`]*)`")?;
    let mut keys = BTreeSet::new();
    for capture in key_re.captures_iter(source) {
        let key = capture[1].trim();
        if key.contains("raw_b64") && !key.contains('`') {
            keys.insert(key.to_string());
        }
    }

    if keys.is_empty() {
        bail!("no raw-capture keys found in docs/CLI_REFERENCE.md");
    }

    Ok(keys.into_iter().collect())
}

fn parse_raw_mode_variants(source: &str) -> Result<Vec<String>> {
    let start_re = Regex::new(r"(?m)^pub enum RawMode \{")?;
    let start = start_re
        .find(source)
        .ok_or_else(|| anyhow::anyhow!("could not find RawMode enum"))?
        .end();

    let mut depth = 1i32;
    let mut end = source.len();
    for (offset, ch) in source[start..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    end = start + offset;
                    break;
                }
            }
            _ => {}
        }
    }
    if end <= start {
        bail!("could not parse RawMode enum body");
    }

    let body = &source[start..end];
    let variant_re = Regex::new(r"(?m)^\s*([A-Z][A-Za-z0-9_]*)\s*(?:,|$)")?;
    let mut variants = BTreeSet::new();
    for capture in variant_re.captures_iter(body) {
        let variant = capture[1].to_string();
        if variant == "Off" || variant == "Record" || variant == "Field" || variant == "RecordRDW" {
            variants.insert(variant);
        }
    }

    if variants.is_empty() {
        bail!("no RawMode variants parsed");
    }

    Ok(variants.into_iter().collect())
}

fn git_head_revision() -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .context("failed to run `git rev-parse HEAD`")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("git rev-parse HEAD failed: {stderr}");
    }
    Ok(String::from_utf8(output.stdout)
        .context("git rev-parse HEAD output was not UTF-8")?
        .trim()
        .to_string())
}

fn load_stability_registry() -> Result<StabilityRegistry> {
    let registry_path = resolve_workspace_file(STABILITY_REGISTRY_PATH)
        .ok_or_else(|| anyhow::anyhow!("loading docs/stability/surface-registry.json"))?;
    let source = fs::read_to_string(&registry_path)
        .with_context(|| format!("loading {}", registry_path.display()))?;
    let registry: StabilityRegistry =
        serde_json::from_str(&source).context("parsing docs/stability/surface-registry.json")?;
    Ok(registry)
}

fn load_cargo_metadata() -> Result<CargoMetadata> {
    let output = Command::new("cargo")
        .args(["metadata", "--format-version", "1", "--no-deps", "--locked"])
        .output()
        .context("failed to execute cargo metadata")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("cargo metadata failed:\n{stderr}");
    }
    let metadata_text =
        String::from_utf8(output.stdout).context("cargo metadata output not valid UTF-8")?;
    let metadata: CargoMetadata =
        serde_json::from_str(&metadata_text).context("failed to parse cargo metadata output")?;
    Ok(metadata)
}

fn verify_stability_registry_against_metadata(
    registry: &StabilityRegistry,
    metadata: &CargoMetadata,
) -> Result<()> {
    if registry.schema_version != STABILITY_SCHEMA_VERSION {
        bail!(
            "stability schema version mismatch: expected {STABILITY_SCHEMA_VERSION}, found {}",
            registry.schema_version
        );
    }

    let workspace = collect_workspace_packages(metadata);
    if workspace.is_empty() {
        bail!("no workspace packages found for stability registry validation");
    }

    let registry_by_name = index_registry_packages(registry)?;
    ensure_package_coverage(&workspace, &registry_by_name)?;

    for (package_name, package_inventory) in workspace {
        let entry = registry_by_name.get(&package_name).context(format!(
            "missing stability entry for package {package_name}"
        ))?;
        verify_stability_package_entry(entry, package_name.as_str(), &package_inventory)?;
    }

    Ok(())
}

fn index_registry_packages(
    registry: &StabilityRegistry,
) -> Result<BTreeMap<String, &StabilityPackage>> {
    let mut registry_by_name = BTreeMap::new();

    for package in &registry.packages {
        if registry_by_name
            .insert(package.name.clone(), package)
            .is_some()
        {
            bail!(
                "stability registry duplicate package entry: {}",
                package.name
            );
        }
    }

    Ok(registry_by_name)
}

fn ensure_package_coverage(
    workspace: &BTreeMap<String, WorkspacePackageInventory>,
    registry_by_name: &BTreeMap<String, &StabilityPackage>,
) -> Result<()> {
    let workspace_names: BTreeSet<String> = workspace.keys().cloned().collect();
    let registry_names: BTreeSet<String> = registry_by_name.keys().cloned().collect();
    let missing_from_registry = workspace_names
        .difference(&registry_names)
        .collect::<Vec<_>>();
    let extra_in_registry = registry_names
        .difference(&workspace_names)
        .collect::<Vec<_>>();

    if !missing_from_registry.is_empty() {
        bail!("stability registry missing packages: {missing_from_registry:?}");
    }
    if !extra_in_registry.is_empty() {
        bail!("stability registry has unknown packages: {extra_in_registry:?}");
    }

    Ok(())
}

fn verify_stability_package_entry(
    entry: &StabilityPackage,
    package_name: &str,
    package_inventory: &WorkspacePackageInventory,
) -> Result<()> {
    let context = format!("package `{package_name}`");

    if entry.publish != package_inventory.publish {
        bail!(
            "stability registry publish mismatch for `{package_name}`: registry={}, metadata={}",
            entry.publish,
            package_inventory.publish
        );
    }

    let class = parse_stability_class(&entry.class, &context)?;
    validate_stability_entry(
        &context,
        class,
        &entry.stability_statement,
        &entry.limitations,
        &entry.graduation_criteria,
    )?;

    if entry.source_of_truth.is_empty() {
        bail!("{context} missing source_of_truth");
    }
    for doc in &entry.source_of_truth {
        if resolve_source_of_truth_path(doc).is_none() {
            bail!("{context} references missing source-of-truth path `{doc}`");
        }
    }

    let mut feature_names = BTreeSet::new();
    for feature in &entry.features {
        if !feature_names.insert(feature.name.clone()) {
            bail!("{context} has duplicate feature entry: {}", feature.name);
        }
        if !package_inventory.features.contains(&feature.name) {
            bail!(
                "{context} has stability entry for unknown feature `{}`",
                feature.name
            );
        }

        let feature_context = format!("{context} feature `{}`", feature.name);
        let feature_class = parse_stability_class(&feature.class, &feature_context)?;
        validate_stability_entry(
            &feature_context,
            feature_class,
            &feature.stability_statement,
            &feature.limitations,
            &feature.graduation_criteria,
        )?;
    }

    verify_feature_coverage(&context, &package_inventory.features, &feature_names)?;
    Ok(())
}

fn verify_feature_coverage(
    context: &str,
    package_features: &BTreeSet<String>,
    registry_features: &BTreeSet<String>,
) -> Result<()> {
    let missing_features = package_features
        .difference(registry_features)
        .collect::<Vec<_>>();
    if !missing_features.is_empty() {
        bail!("{context} missing feature-level registry rows for {missing_features:?}");
    }

    Ok(())
}

fn resolve_workspace_file(doc: &str) -> Option<std::path::PathBuf> {
    let relative = Path::new(doc);
    if relative.exists() {
        return Some(relative.to_path_buf());
    }

    let mut current = workspace_root();
    while let Some(parent) = current.parent() {
        if current.join(relative).exists() {
            return Some(current.join(relative));
        }
        current = parent.to_path_buf();
    }

    None
}

fn resolve_source_of_truth_path(doc: &str) -> Option<std::path::PathBuf> {
    resolve_workspace_file(doc)
}

fn collect_workspace_packages(
    metadata: &CargoMetadata,
) -> BTreeMap<String, WorkspacePackageInventory> {
    let members: BTreeSet<&str> = metadata
        .workspace_members
        .iter()
        .map(String::as_str)
        .collect();
    let mut packages = BTreeMap::new();

    for package in metadata
        .packages
        .iter()
        .filter(|pkg| members.contains(package_id_as_str(pkg)))
    {
        let mut features = BTreeSet::new();
        for feature_name in package.features.keys() {
            if feature_name != "default" {
                features.insert(feature_name.clone());
            }
        }

        packages.insert(
            package.name.clone(),
            WorkspacePackageInventory {
                publish: is_publishable_package(package.publish.as_ref()),
                features,
            },
        );
    }

    packages
}

fn package_id_as_str(package: &CargoMetadataPackage) -> &str {
    package.id.as_str()
}

fn is_publishable_package(publish: Option<&Value>) -> bool {
    match publish {
        Some(Value::Bool(false)) => false,
        Some(Value::Array(values)) => !values.is_empty(),
        Some(_) | None => true,
    }
}

fn validate_stability_entry(
    context: &str,
    class: StabilityClass,
    statement: &str,
    limitations: &[String],
    graduation: &[String],
) -> Result<()> {
    let normalized_statement = statement.trim();
    if normalized_statement.is_empty() || is_placeholder_text(normalized_statement) {
        bail!("{context} has missing stability_statement");
    }

    if class.requires_contracts() {
        if limitations.is_empty() {
            bail!("{context} needs explicit stability limitations (beta/experimental)");
        }
        if graduation.is_empty() {
            bail!("{context} needs explicit graduation criteria (beta/experimental)");
        }
    }

    for limit in limitations {
        let value = limit.trim();
        if value.is_empty() || is_placeholder_text(value) {
            bail!("{context} has invalid stability limitation placeholder");
        }
    }
    for criterion in graduation {
        let value = criterion.trim();
        if value.is_empty() || is_placeholder_text(value) {
            bail!("{context} has invalid graduation criteria placeholder");
        }
    }

    Ok(())
}

fn is_placeholder_text(value: &str) -> bool {
    let normalized = value.trim().to_ascii_lowercase();
    STABILITY_MANUAL_REVIEW_PLACEHOLDERS
        .iter()
        .any(|placeholder| normalized == *placeholder)
}

fn verify_workspace_version_and_msrv() -> Result<()> {
    let workspace = cargo_workspace_toml()?;
    let version = workspace_version(&workspace)?;
    let msrv = workspace_msrv(&workspace)?;

    let readme = fs::read_to_string("README.md").context("loading README.md")?;
    let roadmap = fs::read_to_string("docs/ROADMAP.md").context("loading docs/ROADMAP.md")?;

    let expected_version_token = format!("Engineering Preview (v{version})");
    if !readme.contains(&expected_version_token) {
        bail!(
            "workspace version source-of-truth mismatch: expected {expected_version_token} in README.md | authoritative-source=README.md"
        );
    }

    let msrv_token = format!("MSRV-{msrv}");
    if !readme.contains(&msrv_token) {
        bail!(
            "MSRV source-of-truth mismatch: expected {msrv_token} in README.md | authoritative-source=README.md"
        );
    }

    if !roadmap.contains(&expected_version_token) {
        bail!(
            "workspace version source-of-truth mismatch: expected {expected_version_token} in docs/ROADMAP.md | authoritative-source=docs/ROADMAP.md"
        );
    }

    Ok(())
}

fn verify_facade_invariants() -> Result<()> {
    let lib_module_set = collect_copybook_facade_modules()?;
    let dep_module_set = collect_copybook_dependency_modules()?;
    let readme_module_set = collect_copybook_readme_modules()?;

    // These deprecated facade aliases forward directly to their true owners;
    // they intentionally have no compatibility-crate dependency so the
    // facade does not reintroduce an old ownership edge.
    verify_facade_module_dependency_invariant(
        &lib_module_set,
        &dep_module_set,
        &[
            "codepage",
            "determinism",
            "options",
            "overpunch",
            "record_io",
        ],
    )?;

    let (readme_only, lib_readme_only) = symmetric_diff(&readme_module_set, &lib_module_set);
    if !(readme_only.is_empty() && lib_readme_only.is_empty()) {
        bail!(
            "copybook README module inventory mismatch: readme-only={readme_only:?} lib-only={lib_readme_only:?} | authoritative-source=crates/copybook/README.md and crates/copybook/src/lib.rs"
        );
    }

    Ok(())
}

fn verify_facade_module_dependency_invariant(
    lib_module_set: &BTreeSet<String>,
    dep_module_set: &BTreeSet<String>,
    aliases: &[&str],
) -> Result<()> {
    let mut dependency_modules = lib_module_set.clone();
    for alias in aliases {
        if !dep_module_set.contains(*alias) {
            dependency_modules.remove(*alias);
        }
    }
    let (lib_only, dep_only) = symmetric_diff(&dependency_modules, dep_module_set);
    if !(lib_only.is_empty() && dep_only.is_empty()) {
        bail!(
            "copybook facade modules mismatch dependency list: lib-only={lib_only:?} dep-only={dep_only:?} | authoritative-source=crates/copybook/src/lib.rs and crates/copybook/Cargo.toml"
        );
    }
    Ok(())
}

fn verify_copybook_rs_redirect_invariant() -> Result<()> {
    let content = fs::read_to_string("crates/copybook-rs/src/lib.rs")
        .context("loading crates/copybook-rs/src/lib.rs")?;

    if !content.contains("pub use copybook::*;") {
        bail!(
            "copybook-rs redirect invariant broken: crates/copybook-rs/src/lib.rs must include `pub use copybook::*;` | authoritative-source=crates/copybook-rs/src/lib.rs"
        );
    }

    let module_decl_re = Regex::new(r"(?m)^\s*pub\s+mod\s+")?;
    if module_decl_re.is_match(&content) {
        bail!(
            "copybook-rs should remain a redirect-only crate. Found additional `pub mod` items in crates/copybook-rs/src/lib.rs | authoritative-source=crates/copybook-rs/src/lib.rs"
        );
    }

    Ok(())
}

fn verify_test_status_if_present() -> Result<()> {
    let _junit_path = junit_xml_path()?;
    verify()?;
    Ok(())
}

fn verify_error_code_inventory() -> Result<()> {
    let error_code_source = fs::read_to_string("crates/copybook-error/src/lib.rs")
        .context("loading crates/copybook-error/src/lib.rs")?;
    let docs_source = fs::read_to_string("docs/reference/ERROR_CODES.md")
        .context("loading docs/reference/ERROR_CODES.md")?;

    let source_count = parse_error_code_variant_count(&error_code_source)?;
    let doc_count = parse_error_index_count(&docs_source)?;

    if source_count != doc_count {
        bail!(
            "stable error-code count drift: source={source_count} docs={doc_count}. authoritative-source=crates/copybook-error/src/lib.rs and docs/reference/ERROR_CODES.md"
        );
    }

    Ok(())
}

fn verify_performance_floor_and_receipt() -> Result<()> {
    let policy_text = fs::read_to_string("docs/PERFORMANCE_GOVERNANCE.md")
        .context("loading docs/PERFORMANCE_GOVERNANCE.md")?;
    if !policy_text.contains("scripts/bench/perf.json") {
        bail!("performance policy does not reference canonical scripts/bench/perf.json");
    }

    if !policy_text.contains("DISPLAY absolute floor")
        || !policy_text.contains("COMP-3 absolute floor")
    {
        bail!("performance policy floor declarations are incomplete");
    }

    let display_floor = parse_floor_value(&policy_text, "DISPLAY")?;
    let comp3_floor = parse_floor_value(&policy_text, "COMP-3")?;
    if (display_floor - DISPLAY_FLOOR_MIBPS).abs() > f64::EPSILON {
        bail!(
            "DISPLAY floor drift: docs={display_floor} vs perf gate={DISPLAY_FLOOR_MIBPS} | authoritative-source=docs/PERFORMANCE_GOVERNANCE.md and tools/copybook-bench/src/slo.rs"
        );
    }
    if (comp3_floor - COMP3_CI_FLOOR_MIBPS).abs() > f64::EPSILON {
        bail!(
            "COMP-3 floor drift: docs={comp3_floor} vs perf gate={COMP3_CI_FLOOR_MIBPS} | authoritative-source=docs/PERFORMANCE_GOVERNANCE.md and tools/copybook-bench/src/slo.rs"
        );
    }

    let canonical_path = Path::new("scripts/bench/perf.json");
    if !canonical_path.exists() {
        bail!("missing mandatory canonical perf receipt at scripts/bench/perf.json");
    }

    let canonical =
        fs::read_to_string(canonical_path).context("loading scripts/bench/perf.json")?;
    let snapshot = perf::parse_perf_receipt(&canonical)?;

    if snapshot.display_mibps < DISPLAY_FLOOR_MIBPS {
        let display_mibps = snapshot.display_mibps;
        bail!(
            "canonical performance floor failed: DISPLAY display={display_mibps} MiB/s < enforced floor {DISPLAY_FLOOR_MIBPS} MiB/s"
        );
    }
    if snapshot.comp3_mibps < COMP3_CI_FLOOR_MIBPS {
        let comp3_mibps = snapshot.comp3_mibps;
        bail!(
            "canonical performance floor failed: COMP-3 {comp3_mibps} MiB/s < enforced floor {COMP3_CI_FLOOR_MIBPS} MiB/s"
        );
    }
    let display_delta_pct =
        ((snapshot.display_mibps - DISPLAY_FLOOR_MIBPS) / DISPLAY_FLOOR_MIBPS) * 100.0;
    let comp3_delta_pct =
        ((snapshot.comp3_mibps - COMP3_CI_FLOOR_MIBPS) / COMP3_CI_FLOOR_MIBPS) * 100.0;
    if display_delta_pct.is_nan() || comp3_delta_pct.is_nan() {
        bail!("invalid performance floor delta calculation for scripts/bench/perf.json");
    }

    Ok(())
}

fn verify_cli_inventory() -> Result<()> {
    let cli_source = fs::read_to_string("crates/copybook-cli/src/main.rs")
        .context("loading crates/copybook-cli/src/main.rs")?;
    let cli_ref =
        fs::read_to_string("docs/CLI_REFERENCE.md").context("loading docs/CLI_REFERENCE.md")?;

    for command in parse_cli_command_variants(&cli_source)? {
        let heading = format!("### {command}");
        if !cli_ref.contains(&heading) {
            bail!(
                "CLI command `{command}` missing from docs/CLI_REFERENCE.md | authoritative-source=crates/copybook-cli/src/main.rs"
            );
        }
    }

    let required_doc_snippets = [
        "copybook [GLOBAL OPTIONS]",
        "-v, --verbose",
        "--strict-policy",
        "--no-strict-policy",
        "--enable-features",
        "--disable-features",
        "--enable-category",
        "--disable-category",
        "--feature-flags-config",
        "--list-features",
        "RUST_LOG",
        "COPYBOOK_DIALECT",
        "COPYBOOK_STRICT_POLICY",
        "COPYBOOK_FF_<FEATURE>",
        "COPYBOOK_FF_",
    ];
    for snippet in required_doc_snippets {
        if !cli_ref.contains(snippet) {
            bail!("CLI reference missing `{snippet}` | authoritative-source=docs/CLI_REFERENCE.md");
        }
    }

    Ok(())
}

fn verify_publish_workflow_inventory() -> Result<()> {
    let publish_workflow = fs::read_to_string(".github/workflows/publish.yml")
        .context("loading .github/workflows/publish.yml")?;
    let publish_dry_run = fs::read_to_string(".github/workflows/publish-dry-run.yml")
        .context("loading .github/workflows/publish-dry-run.yml")?;

    for line in [
        "cargo run -p xtask -- publish plan --check",
        "cargo run -p xtask -- publish plan --format json > \"${PLAN_JSON}\"",
    ] {
        if !publish_workflow.contains(line) {
            bail!("publish workflow inventory mismatch: missing `{line}` in publish workflow");
        }
        if !publish_dry_run.contains(line) {
            bail!("publish-dry-run workflow inventory mismatch: missing `{line}`");
        }
    }

    for line in [
        "mapfile -t PUBLISH_CRATES < <(python - \"$PLAN_JSON\" <<'PY'",
        "PLAN_COUNT=$(python - \"${PLAN_JSON}\" <<'PY'",
        "print(crate[\"package\"])",
        "if [ \"${PLAN_COUNT}\" -le 0 ]; then",
    ] {
        if !publish_workflow.contains(line) {
            bail!("publish workflow inventory mismatch: missing `{line}`");
        }
        if !publish_dry_run.contains(line) {
            bail!("publish-dry-run workflow inventory mismatch: missing `{line}`");
        }
    }

    if !publish_workflow.contains("if [ \"${PLAN_COUNT}\" -ne \"${#PUBLISH_CRATES[@]}\" ]; then") {
        bail!("publish workflow inventory mismatch: missing publish plan count verification");
    }

    Ok(())
}

fn verify_quick_start_versioning() -> Result<()> {
    let workspace = cargo_workspace_toml()?;
    let version = workspace_version(&workspace)?;
    let mut version_parts = version.split('.');
    let major = version_parts
        .next()
        .ok_or_else(|| anyhow::anyhow!("invalid version"))?;
    let minor = version_parts
        .next()
        .ok_or_else(|| anyhow::anyhow!("invalid version"))?;
    let expected_prefix = format!("{major}.{minor}");

    let targets = [
        "crates/copybook/README.md",
        "crates/copybook-rs/README.md",
        "docs/reference/LIBRARY_API.md",
    ];
    for target in targets {
        let source = fs::read_to_string(target).context(format!("loading {target}"))?;
        let versions = parse_copybook_dependency_versions(&source)?;
        if versions.is_empty() {
            bail!("no copybook dependency version found in {target}");
        }
        for version in versions {
            if !version.starts_with(&expected_prefix) {
                bail!(
                    "quick-start dependency version drift in {target}: found `{version}`, expected prefix `{expected_prefix}`"
                );
            }
        }
    }

    Ok(())
}

/// Documents that declare the project's current release status.
///
/// Each of these carries a line naming the Engineering Preview version. They
/// drifted to `v0.4.3` while the workspace was on `0.5.0`, and `docs/REPORT.md`
/// contradicted itself in two places within one file.
const STATUS_VERSION_TARGETS: [&str; 7] = [
    "CLAUDE.md",
    "CONTRIBUTING.md",
    "README.md",
    "SECURITY.md",
    "docs/REPORT.md",
    "docs/ROADMAP.md",
    "docs/USER_GUIDE.md",
];

/// Extract version-like tokens (`0.5.0`, `v0.5.0`, `0.5.x`) from one line.
fn version_tokens(line: &str) -> Vec<String> {
    let bytes: Vec<char> = line.chars().collect();
    let mut tokens = Vec::new();
    let mut index = 0;
    while index < bytes.len() {
        if !bytes[index].is_ascii_digit() {
            index += 1;
            continue;
        }
        let start = index;
        while index < bytes.len() && (bytes[index].is_ascii_digit() || bytes[index] == '.') {
            index += 1;
        }
        // Allow a trailing wildcard component such as `0.5.x`.
        if index < bytes.len() && bytes[index] == 'x' && bytes[start..index].contains(&'.') {
            index += 1;
        }
        let token: String = bytes[start..index].iter().collect();
        if token.matches('.').count() >= 1 {
            tokens.push(token);
        }
    }
    tokens
}

/// Fail when a current-status line names a version other than the workspace one.
///
/// Only lines mentioning "Engineering Preview" are considered, so historical
/// statements ("promoted to stable in v0.4.3") stay untouched.
fn verify_status_versioning() -> Result<()> {
    // Resolve from the workspace root rather than the current directory so the
    // check behaves the same under `xtask docs verify-all` and `cargo test`.
    let manifest_path = workspace_root().join("Cargo.toml");
    let manifest = fs::read_to_string(&manifest_path)
        .with_context(|| format!("loading {}", manifest_path.display()))?;
    let workspace: toml::Value = toml::from_str(&manifest)?;
    let version = workspace_version(&workspace)?;
    let mut parts = version.split('.');
    let major = parts
        .next()
        .ok_or_else(|| anyhow::anyhow!("invalid workspace version: {version}"))?;
    let minor = parts
        .next()
        .ok_or_else(|| anyhow::anyhow!("invalid workspace version: {version}"))?;
    let expected_prefix = format!("{major}.{minor}");

    for target in STATUS_VERSION_TARGETS {
        // Resolved from the workspace root, not the current directory: several
        // packages ship their own README.md, which would otherwise shadow the
        // repository one.
        let path = workspace_root().join(target);
        let source =
            fs::read_to_string(&path).with_context(|| format!("loading {}", path.display()))?;
        for (offset, line) in source.lines().enumerate() {
            if !line.contains("Engineering Preview") {
                continue;
            }
            let tokens = version_tokens(line);
            if tokens.is_empty() {
                continue;
            }
            // At least one token must be the current release. Status lines
            // legitimately also mention the v1.0.0 target, so requiring *every*
            // token to match would reject correct prose.
            if !tokens
                .iter()
                .any(|token| token.starts_with(&expected_prefix))
            {
                bail!(
                    "status version drift in {target}:{}: found `{}`, expected one naming `{expected_prefix}`",
                    offset + 1,
                    tokens.join("`, `")
                );
            }
        }
    }

    Ok(())
}

fn load_deprecation_audit_report() -> Result<DeprecatedAuditReport> {
    let path = resolve_workspace_file(DEPRECATION_AUDIT_PATH)
        .ok_or_else(|| anyhow::anyhow!("loading docs/reports/deprecation-audit.json"))?;
    let source = fs::read_to_string(&path)
        .with_context(|| format!("loading deprecation audit {}", path.display()))?;

    let audit: DeprecatedAuditReport =
        serde_json::from_str(&source).context("parsing docs/reports/deprecation-audit.json")?;
    if audit.schema_version != STABILITY_SCHEMA_VERSION {
        bail!(
            "deprecation audit schema version mismatch: expected {STABILITY_SCHEMA_VERSION}, found {}",
            audit.schema_version
        );
    }

    Ok(audit)
}

fn load_surface_deprecation_audit_report() -> Result<SurfaceDeprecationAuditReport> {
    let path = resolve_workspace_file(SURFACE_DEPRECATION_AUDIT_PATH)
        .ok_or_else(|| anyhow::anyhow!("loading docs/reports/surface-deprecation-audit.json"))?;
    let source = fs::read_to_string(&path)
        .with_context(|| format!("loading surface deprecation audit {}", path.display()))?;

    let audit: SurfaceDeprecationAuditReport = serde_json::from_str(&source)
        .context("parsing docs/reports/surface-deprecation-audit.json")?;
    if audit.schema_version != STABILITY_SCHEMA_VERSION {
        bail!(
            "surface deprecation audit schema version mismatch: expected {STABILITY_SCHEMA_VERSION}, found {}",
            audit.schema_version
        );
    }

    Ok(audit)
}

fn collect_deprecated_api_inventory() -> Result<Vec<DiscoveredDeprecatedItem>> {
    let mut files = Vec::new();
    let crates_dir = workspace_root().join("crates");
    collect_rust_source_files(&crates_dir, &mut files)?;

    let mut items = Vec::new();
    let deprecated_re = Regex::new(r"^\s*#\[deprecated")?;
    let attr_since_re = Regex::new(r#"since\s*=\s*"([^"]+)""#)?;
    let attr_kv_re = Regex::new(r#"^\s*[A-Za-z_][A-Za-z0-9_]*\s*="#)?;
    let fn_re = Regex::new(
        r"^\s*(?:pub\s+)?(?:const\s+)?(?:async\s+)?(?:unsafe\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)",
    )?;
    let type_re =
        Regex::new(r"^\s*(?:pub\s+)?(struct|enum|trait|type|mod)\s+([A-Za-z_][A-Za-z0-9_]*)")?;
    let workspace_root = workspace_root();

    for path in files {
        let source =
            fs::read_to_string(&path).with_context(|| format!("loading {}", path.display()))?;
        let mut pending: Option<DeprecatedAttribute> = None;

        for line in source.lines() {
            if deprecated_re.is_match(line) {
                let since = attr_since_re
                    .captures(line)
                    .and_then(|c| c.get(1).map(|m| m.as_str().to_string()));
                pending = Some(DeprecatedAttribute { since });
                continue;
            }

            if pending.is_none() {
                continue;
            }

            let trimmed = line.trim();
            if let Some(current) = pending.as_mut() {
                if let Some(c) = attr_since_re.captures(line) {
                    current.since =
                        Some(c.get(1).map(|m| m.as_str().to_string()).unwrap_or_default());
                }

                if trimmed.is_empty()
                    || trimmed.starts_with('#')
                    || trimmed.starts_with("//")
                    || trimmed.starts_with("/*")
                    || attr_kv_re.is_match(trimmed)
                    || trimmed == ")"
                    || trimmed == ")]"
                    || trimmed == "),"
                    || trimmed.ends_with(")]")
                    || trimmed == ","
                {
                    continue;
                }

                if let Some(cap) = fn_re.captures(line) {
                    let pending_item = pending.take();
                    let symbol = cap.get(1).map_or_else(
                        || {
                            anyhow::bail!(
                                "failed to extract deprecated function symbol from declaration"
                            );
                        },
                        |m| Ok::<_, anyhow::Error>(m.as_str().to_string()),
                    )?;
                    let rel_path = path.strip_prefix(&workspace_root).unwrap_or(&path);
                    items.push(DiscoveredDeprecatedItem {
                        path: rel_path.to_string_lossy().replace('\\', "/"),
                        symbol,
                        kind: "fn".to_string(),
                        since: pending_item.as_ref().and_then(|meta| meta.since.clone()),
                    });
                    continue;
                } else if let Some(cap) = type_re.captures(line) {
                    let pending_item = pending.take();
                    let kind = cap.get(1).map_or_else(
                        || {
                            anyhow::bail!(
                                "failed to extract deprecated item kind from declaration"
                            );
                        },
                        |m| Ok::<_, anyhow::Error>(m.as_str().to_string()),
                    )?;
                    let symbol = cap.get(2).map_or_else(
                        || {
                            anyhow::bail!(
                                "failed to extract deprecated item symbol from declaration"
                            );
                        },
                        |m| Ok::<_, anyhow::Error>(m.as_str().to_string()),
                    )?;
                    let rel_path = path.strip_prefix(&workspace_root).unwrap_or(&path);
                    items.push(DiscoveredDeprecatedItem {
                        path: rel_path.to_string_lossy().replace('\\', "/"),
                        symbol,
                        kind,
                        since: pending_item.as_ref().and_then(|meta| meta.since.clone()),
                    });
                    continue;
                }
                pending = None;
            }
        }
    }

    Ok(items)
}

fn collect_rust_source_files(dir: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir).context(format!("reading directory {}", dir.display()))? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            if path
                .file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name == "target")
            {
                continue;
            }
            collect_rust_source_files(&path, files)?;
            continue;
        }

        if path
            .extension()
            .is_none_or(|ext| !ext.eq_ignore_ascii_case("rs"))
        {
            continue;
        }

        let path_str = path.to_string_lossy().to_lowercase();
        let exclude_dir = path_str.contains("/tests/") || path_str.contains("\\tests\\");
        if exclude_dir {
            continue;
        }

        files.push(path);
    }
    Ok(())
}

fn collect_copybook_facade_modules() -> Result<BTreeSet<String>> {
    let source = fs::read_to_string("crates/copybook/src/lib.rs")
        .context("loading crates/copybook/src/lib.rs")?;
    let re = Regex::new(r"(?m)^pub mod ([a-z_][a-z0-9_]*)")?;
    let mut modules = BTreeSet::new();
    for cap in re.captures_iter(&source) {
        modules.insert(cap[1].to_string());
    }
    if modules.is_empty() {
        bail!("no public modules found in copybook facade lib");
    }
    Ok(modules)
}

fn collect_copybook_dependency_modules() -> Result<BTreeSet<String>> {
    let source = fs::read_to_string("crates/copybook/Cargo.toml")
        .context("loading crates/copybook/Cargo.toml")?;
    let toml: toml::Value = toml::from_str(&source)?;

    let dependencies = toml
        .get("dependencies")
        .and_then(|deps| deps.as_table())
        .ok_or_else(|| anyhow::anyhow!("missing [dependencies] in crates/copybook/Cargo.toml"))?;

    let mut modules = BTreeSet::new();
    for dep in dependencies.keys() {
        if dep.starts_with("copybook-") {
            modules.insert(dep.trim_start_matches("copybook-").replace('-', "_"));
        }
    }

    if modules.is_empty() {
        bail!("no copybook-* dependencies found in crates/copybook/Cargo.toml");
    }
    Ok(modules)
}

fn collect_copybook_readme_modules() -> Result<BTreeSet<String>> {
    let source = fs::read_to_string("crates/copybook/README.md")
        .context("loading crates/copybook/README.md")?;
    let re = Regex::new(r"^\|\s*`([^`]+)`\s*\|\s*`(copybook-[^`]+)`\s*\|")?;
    let mut modules = BTreeSet::new();
    let mut in_table = false;

    for line in source.lines() {
        if line.starts_with("| ---") {
            in_table = true;
            continue;
        }
        if !in_table {
            continue;
        }
        if !line.trim_start().starts_with('|') {
            break;
        }
        if line.starts_with("| Module") {
            continue;
        }
        if let Some(cap) = re.captures(line) {
            modules.insert(cap[1].to_string());
        }
    }

    if modules.is_empty() {
        bail!("no module rows found in crates/copybook/README.md");
    }
    Ok(modules)
}

fn parse_cli_command_variants(source: &str) -> Result<Vec<String>> {
    let mut commands = Vec::new();
    let start_re = Regex::new(r"(?m)^enum\s+Commands\s*\{")?;
    let start = start_re
        .find(source)
        .ok_or_else(|| anyhow::anyhow!("could not find `enum Commands`"))?
        .end();

    let mut depth = 1i32;
    let mut end = source.len();
    for (offset, ch) in source[start..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    end = start + offset;
                    break;
                }
            }
            _ => {}
        }
    }
    if end <= start {
        bail!("could not find end of `enum Commands`");
    }

    let remainder = &source[start..end];
    let variant_re = Regex::new(r"(?m)^\s*([A-Z][A-Za-z0-9_]*)\s*\{$")?;
    for capture in variant_re.captures_iter(remainder) {
        commands.push(snake_case(&capture[1]));
    }

    if commands.is_empty() {
        bail!("no command variants parsed from enum Commands");
    }

    Ok(commands)
}

fn parse_error_code_variant_count(source: &str) -> Result<usize> {
    let start_re = Regex::new(r"(?m)^pub enum ErrorCode \{")?;
    let start = start_re
        .find(source)
        .ok_or_else(|| anyhow::anyhow!("could not find ErrorCode enum"))?
        .end();

    let variant_re = Regex::new(r"(?m)^[A-Z][A-Z0-9_]+\s*(?:=\s*[^,]+)?\s*,(?:\s*//.*)?$")?;
    let mut count = 0usize;
    for line in source[start..].lines() {
        let line = line.trim();
        if line.trim() == "}" {
            break;
        }
        if variant_re.is_match(line) {
            count += 1;
        }
    }

    if count == 0 {
        bail!("no ErrorCode variants found");
    }

    Ok(count)
}

fn parse_error_index_count(source: &str) -> Result<usize> {
    let re = Regex::new(r"All\s+(\d+)\s+stable error codes across")?;
    if let Some(cap) = re.captures(source) {
        let count = cap[1].parse::<usize>()?;
        if count == 0 {
            bail!("error-code count declared as zero");
        }
        return Ok(count);
    }
    bail!("could not find stable error-code count declaration in docs/reference/ERROR_CODES.md");
}

fn parse_floor_value(policy_source: &str, label: &str) -> Result<f64> {
    let target = label.to_ascii_lowercase();
    let floor_re = Regex::new(r"absolute\s+floor[^0-9]*?(\d+(?:\.\d+)?)")?;
    for line in policy_source.lines() {
        let lower = line.to_ascii_lowercase();
        if !lower.contains(&target) || !lower.contains("absolute floor") {
            continue;
        }
        let Some(cap) = floor_re.captures(&lower) else {
            continue;
        };
        return cap[1]
            .parse::<f64>()
            .context("parsing performance floor value from policy");
    }
    bail!("missing {label} absolute floor in policy")
}

fn parse_copybook_dependency_versions(source: &str) -> Result<Vec<String>> {
    let re = Regex::new(r#"(?m)^\s*copybook\s*=\s*"([^"]+)""#)
        .context("building copybook dependency regex")?;
    Ok(re
        .captures_iter(source)
        .map(|capture| capture[1].to_string())
        .collect())
}

fn symmetric_diff(left: &BTreeSet<String>, right: &BTreeSet<String>) -> (Vec<String>, Vec<String>) {
    let mut left_only = Vec::new();
    let mut right_only = Vec::new();

    for item in left {
        if !right.contains(item) {
            left_only.push(item.clone());
        }
    }
    for item in right {
        if !left.contains(item) {
            right_only.push(item.clone());
        }
    }

    (left_only, right_only)
}

fn cargo_workspace_toml() -> Result<toml::Value> {
    let source = fs::read_to_string("Cargo.toml").context("loading Cargo.toml")?;
    Ok(toml::from_str(&source)?)
}

fn workspace_version(workspace: &toml::Value) -> Result<String> {
    let version = workspace
        .get("workspace")
        .and_then(|workspace| workspace.get("package"))
        .and_then(|package| package.get("version"))
        .and_then(|version| version.as_str())
        .ok_or_else(|| anyhow::anyhow!("missing workspace.package.version"))?;
    Ok(version.to_string())
}

fn workspace_msrv(workspace: &toml::Value) -> Result<String> {
    let rust_version = workspace
        .get("workspace")
        .and_then(|workspace| workspace.get("package"))
        .and_then(|package| package.get("rust-version"))
        .and_then(|rust_version| rust_version.as_str())
        .ok_or_else(|| anyhow::anyhow!("missing workspace.package.rust-version"))?;
    Ok(rust_version.to_string())
}

fn snake_case(value: &str) -> String {
    let mut output = String::new();
    for (index, ch) in value.chars().enumerate() {
        if ch.is_uppercase() && index > 0 {
            output.push('_');
        }
        output.push(ch.to_ascii_lowercase());
    }
    output
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};
    use std::sync::{Mutex, OnceLock};

    use super::*;

    #[expect(
        clippy::unnecessary_wraps,
        reason = "Verifier callbacks share the Result-returning function signature"
    )]
    fn ok() -> Result<()> {
        Ok(())
    }

    #[test]
    fn version_tokens_finds_release_shapes() {
        assert_eq!(version_tokens("Engineering Preview v0.5.0"), vec!["0.5.0"]);
        assert_eq!(version_tokens("| 0.5.x | supported |"), vec!["0.5.x"]);
        assert_eq!(
            version_tokens("Preview (v0.5.0) and (v0.4.3)"),
            vec!["0.5.0", "0.4.3"]
        );
        assert!(version_tokens("Engineering Preview, no version here").is_empty());
        // A bare integer is not a version.
        assert!(version_tokens("supports 63 error codes").is_empty());
    }

    #[test]
    fn status_versioning_matches_the_workspace_version() {
        // The repository's own status lines must already agree.
        verify_status_versioning().expect("status lines name the workspace version");
    }

    #[test]
    fn status_version_targets_all_exist_and_declare_status() {
        let mut problems = Vec::new();
        for target in STATUS_VERSION_TARGETS {
            let path = workspace_root().join(target);
            match fs::read_to_string(&path) {
                Ok(source) if source.contains("Engineering Preview") => {}
                Ok(_) => problems.push(format!("{target} no longer declares a release status")),
                Err(err) => problems.push(format!("{target} is unreadable: {err}")),
            }
        }
        assert!(
            problems.is_empty(),
            "drop these from STATUS_VERSION_TARGETS or restore the claim: {}",
            problems.join("; ")
        );
    }

    #[test]
    fn facade_dependency_invariant_accepts_forwarding_alias_without_dependency() {
        let lib_modules = BTreeSet::from([
            "codec".to_string(),
            "determinism".to_string(),
            "options".to_string(),
        ]);
        let dependency_modules = BTreeSet::from(["codec".to_string(), "options".to_string()]);

        assert!(
            verify_facade_module_dependency_invariant(
                &lib_modules,
                &dependency_modules,
                &["determinism"],
            )
            .is_ok()
        );
    }

    #[test]
    fn facade_dependency_invariant_rejects_dependency_without_facade_module() {
        let lib_modules = BTreeSet::from(["codec".to_string(), "options".to_string()]);
        let dependency_modules = BTreeSet::from([
            "codec".to_string(),
            "determinism".to_string(),
            "options".to_string(),
        ]);

        assert!(
            verify_facade_module_dependency_invariant(
                &lib_modules,
                &dependency_modules,
                &["determinism"],
            )
            .is_err()
        );
    }

    fn failing() -> Result<()> {
        bail!("boom");
    }

    #[test]
    fn run_checks_reports_named_failure() {
        let checks: [Verifier; 2] = [("ok", ok), ("failing-step", failing)];

        let err = run_checks(&checks).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("failing-step"));
        assert!(message.contains("boom"));
    }

    static ORDER: OnceLock<Mutex<Vec<&'static str>>> = OnceLock::new();

    fn with_order() -> &'static Mutex<Vec<&'static str>> {
        ORDER.get_or_init(|| Mutex::new(Vec::new()))
    }

    #[expect(
        clippy::unnecessary_wraps,
        reason = "Order probes share the Result-returning verifier signature"
    )]
    fn record_order(name: &'static str) -> Result<()> {
        with_order().lock().unwrap().push(name);
        Ok(())
    }

    fn order_alpha() -> Result<()> {
        record_order("alpha")
    }

    fn order_bravo() -> Result<()> {
        record_order("bravo")
    }

    fn order_charlie() -> Result<()> {
        record_order("charlie")
    }

    #[test]
    fn run_checks_is_deterministic() {
        {
            with_order().lock().unwrap().clear();
        }

        let checks: [Verifier; 3] = [
            ("alpha", order_alpha),
            ("bravo", order_bravo),
            ("charlie", order_charlie),
        ];

        assert!(run_checks(&checks).is_ok());

        let observed = with_order().lock().unwrap().clone();
        assert_eq!(observed, vec!["alpha", "bravo", "charlie"]);
    }

    #[test]
    fn parse_error_index_from_docs() {
        let source = "##\nAll 63 stable error codes across 10 families:";
        assert_eq!(parse_error_index_count(source).unwrap(), 63);
    }

    #[test]
    fn parse_error_code_variant_count_supports_discriminants_and_comments() {
        let source = r#"pub enum ErrorCode {
    CBK001_ORDINAL = 1,
    CBK002_COMMENTS = 2, // explicit discriminant with comment
    CBK003_FLAG, // plain variant
}"#;

        assert_eq!(parse_error_code_variant_count(source).unwrap(), 3);
    }

    #[test]
    fn parse_stable_contract_exit_code_tags() {
        let source = r#"pub enum ExitCode {
    Ok = 0,
    Unknown = 1,
    Data = 2,
    Encode = 3,
    Format = 4,
    Internal = 5,
}"#;

        let inventory = parse_exit_code_inventory(source).unwrap();
        assert_eq!(
            inventory.variants,
            vec!["Data", "Encode", "Format", "Internal", "Ok", "Unknown"]
        );
        assert_eq!(inventory.tags, vec!["CBKD", "CBKE", "CBKF", "CBKI"]);
    }

    #[test]
    fn parse_cli_reference_env_contracts_ignores_partial_prefix() {
        let source = "- `COPYBOOK_STRICT_POLICY` and `COPYBOOK_FF_SIGN_SEPARATE`";
        let vars = parse_cli_reference_env_contracts(source).unwrap();
        assert_eq!(
            vars,
            vec!["COPYBOOK_FF_SIGN_SEPARATE", "COPYBOOK_STRICT_POLICY"]
        );
    }

    #[test]
    fn diff_contracts_reports_removed_and_added_contract_items() {
        let baseline = StableSurfaceContractManifest {
            schema_version: "1.0.0".to_string(),
            generated_at: "baseline".to_string(),
            generated_by: "baseline".to_string(),
            source_revision: "baseline".to_string(),
            source_paths: vec!["crates/copybook-core/src/lib.rs".to_string()],
            cli: ContractCliInventory {
                commands: vec!["decode".to_string()],
                options: vec!["input".to_string()],
                env_vars: vec!["COPYBOOK_STRICT_POLICY".to_string()],
            },
            error: ContractErrorInventory {
                variants: vec!["CBK001".to_string()],
            },
            exit_code: ContractExitCodeInventory {
                variants: vec!["Data".to_string()],
                tags: vec!["CBKD".to_string()],
            },
            jsonl: ContractJsonlInventory {
                schema_keys: vec!["schema".to_string()],
                required_keys: vec!["schema".to_string()],
                pattern_properties: vec!["^prefix_".to_string()],
                compatibility_keys: vec!["raw_b64".to_string()],
            },
            raw_capture: ContractRawCaptureInventory {
                modes: vec!["Off".to_string()],
                emitted_keys: vec!["__raw_b64".to_string()],
            },
        };

        let current = StableSurfaceContractManifest {
            schema_version: "1.0.0".to_string(),
            generated_at: "current".to_string(),
            generated_by: "current".to_string(),
            source_revision: "current".to_string(),
            source_paths: vec!["crates/copybook-cli/src/main.rs".to_string()],
            cli: ContractCliInventory {
                commands: vec!["decode".to_string(), "parse".to_string()],
                options: vec!["input".to_string()],
                env_vars: vec!["COPYBOOK_STRICT_POLICY".to_string()],
            },
            error: ContractErrorInventory {
                variants: vec!["CBK001".to_string(), "CBK002".to_string()],
            },
            exit_code: ContractExitCodeInventory {
                variants: vec!["Data".to_string(), "Encode".to_string()],
                tags: vec!["CBKD".to_string(), "CBKE".to_string()],
            },
            jsonl: ContractJsonlInventory {
                schema_keys: vec!["schema".to_string()],
                required_keys: vec!["schema".to_string(), "record_index".to_string()],
                pattern_properties: vec!["^prefix_".to_string()],
                compatibility_keys: vec!["raw_b64".to_string()],
            },
            raw_capture: ContractRawCaptureInventory {
                modes: vec!["Field".to_string()],
                emitted_keys: vec!["__raw_b64".to_string()],
            },
        };

        let deltas = diff_contracts(&baseline, &current);
        assert!(
            deltas
                .removed
                .iter()
                .all(|item| item.category != "cli.command")
        );
        assert!(
            deltas
                .added
                .iter()
                .any(|item| { item.category == "cli.command" && item.item == "parse" })
        );
        assert!(
            deltas
                .added
                .iter()
                .any(|item| { item.category == "error-code" && item.item == "CBK002" })
        );
        assert!(!deltas.removed.is_empty());
        assert!(!deltas.added.is_empty());
    }

    fn simple_contract_manifest(
        commands: &[&str],
        error_codes: &[&str],
        exit_variants: &[&str],
        exit_tags: &[&str],
    ) -> StableSurfaceContractManifest {
        StableSurfaceContractManifest {
            schema_version: "1.0.0".to_string(),
            generated_at: "current".to_string(),
            generated_by: "current".to_string(),
            source_revision: "current".to_string(),
            source_paths: STABLE_CONTRACT_SOURCE_PATHS
                .iter()
                .map(ToString::to_string)
                .collect(),
            cli: ContractCliInventory {
                commands: commands
                    .iter()
                    .map(|command| (*command).to_string())
                    .collect(),
                options: vec!["input".to_string()],
                env_vars: vec!["COPYBOOK_STRICT_POLICY".to_string()],
            },
            error: ContractErrorInventory {
                variants: error_codes
                    .iter()
                    .map(|error| (*error).to_string())
                    .collect(),
            },
            exit_code: ContractExitCodeInventory {
                variants: exit_variants
                    .iter()
                    .map(|variant| (*variant).to_string())
                    .collect(),
                tags: exit_tags.iter().map(|tag| (*tag).to_string()).collect(),
            },
            jsonl: ContractJsonlInventory {
                schema_keys: vec!["schema".to_string()],
                required_keys: vec!["schema".to_string()],
                pattern_properties: vec!["^prefix_".to_string()],
                compatibility_keys: vec!["raw_b64".to_string()],
            },
            raw_capture: ContractRawCaptureInventory {
                modes: vec!["Off".to_string()],
                emitted_keys: vec!["__raw_b64".to_string()],
            },
        }
    }

    #[test]
    fn verify_contracts_with_strictness_allows_added_items_when_non_strict() {
        let baseline = simple_contract_manifest(&["decode"], &["CBK001"], &["Data"], &["CBKD"]);
        let current =
            simple_contract_manifest(&["decode", "parse"], &["CBK001"], &["Data"], &["CBKD"]);

        assert!(verify_contracts_with_strictness(&baseline, &current, false).is_ok());
    }

    #[test]
    fn verify_contracts_with_strictness_rejects_added_items_when_strict() {
        let baseline = simple_contract_manifest(&["decode"], &["CBK001"], &["Data"], &["CBKD"]);
        let current =
            simple_contract_manifest(&["decode", "parse"], &["CBK001"], &["Data"], &["CBKD"]);

        let err = verify_contracts_with_strictness(&baseline, &current, true).unwrap_err();
        assert!(
            err.to_string()
                .contains("stable-contract inventory has incompatible changes")
        );
    }

    #[test]
    fn verify_contracts_with_strictness_rejects_removed_items_when_non_strict() {
        let baseline = simple_contract_manifest(
            &["decode", "parse"],
            &["CBK001", "CBK002"],
            &["Data", "Encode"],
            &["CBKD", "CBKE"],
        );
        let current = simple_contract_manifest(&["decode"], &["CBK001"], &["Data"], &["CBKD"]);

        let err = verify_contracts_with_strictness(&baseline, &current, false).unwrap_err();
        assert!(
            err.to_string()
                .contains("stable-contract inventory has breaking changes")
        );
    }

    #[test]
    fn parse_copybook_versions_from_snippet() {
        let source = "[dependencies]\ncopybook = \"0.4.5\"\n";
        assert_eq!(
            parse_copybook_dependency_versions(source).unwrap(),
            vec!["0.4.5"]
        );
    }

    #[test]
    fn parse_cli_command_variants_from_snippet() {
        let source = "enum Commands {\n    Parse {\n    }\n    Decode {\n    }\n}\n";
        let commands = parse_cli_command_variants(source).unwrap();
        assert_eq!(commands, vec!["parse", "decode"]);
    }

    fn metadata_package(
        name: &str,
        id: &str,
        publish: Option<Value>,
        features: &[&str],
    ) -> CargoMetadataPackage {
        let mut feature_map = BTreeMap::new();
        for feature in features {
            feature_map.insert((*feature).to_string(), Value::Array(vec![]));
        }

        CargoMetadataPackage {
            id: id.to_string(),
            name: name.to_string(),
            publish,
            features: feature_map,
        }
    }

    fn package_entry(
        name: &str,
        class: &str,
        publish: bool,
        features: Vec<StabilityFeature>,
    ) -> StabilityPackage {
        StabilityPackage {
            name: name.to_string(),
            publish,
            class: class.to_string(),
            stability_statement: format!("{name} surface class {class}"),
            limitations: if class == "beta" || class == "experimental" {
                vec!["Documented stability limitations".to_string()]
            } else {
                Vec::new()
            },
            graduation_criteria: if class == "beta" || class == "experimental" {
                vec!["Graduation criteria are satisfied in planning".to_string()]
            } else {
                Vec::new()
            },
            source_of_truth: vec!["docs/STABILITY_GUARANTEES.md".to_string()],
            features,
        }
    }

    fn feature_entry(name: &str, class: &str) -> StabilityFeature {
        StabilityFeature {
            name: name.to_string(),
            class: class.to_string(),
            stability_statement: format!("{name} feature class {class}"),
            limitations: if class == "beta" || class == "experimental" {
                vec!["Documented feature limitations".to_string()]
            } else {
                Vec::new()
            },
            graduation_criteria: if class == "beta" || class == "experimental" {
                vec!["Feature graduation criteria".to_string()]
            } else {
                Vec::new()
            },
        }
    }

    #[test]
    fn verify_stability_registry_catches_unknown_package() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package("copybook-core", "id-core", None, &[])],
        };

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![package_entry("copybook", "stable", true, vec![])],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_unknown_feature() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string(), "id-charset".to_string()],
            packages: vec![
                metadata_package("copybook-core", "id-core", None, &[]),
                metadata_package("copybook-charset", "id-charset", None, &["clap"]),
            ],
        };

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![
                package_entry("copybook-core", "stable", true, vec![]),
                package_entry(
                    "copybook-charset",
                    "stable",
                    true,
                    vec![feature_entry("unknown", "internal-dev-only")],
                ),
            ],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_publish_mismatch() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package(
                "copybook-core",
                "id-core",
                Some(Value::Bool(false)),
                &[],
            )],
        };

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![package_entry("copybook-core", "stable", true, vec![])],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_invalid_class() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package("copybook-core", "id-core", None, &[])],
        };

        let mut entry = package_entry("copybook-core", "stable", true, vec![]);
        entry.class = "deprecated".to_string();
        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![entry],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_beta_without_contracts() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package("copybook-core", "id-core", None, &[])],
        };

        let mut entry = package_entry("copybook-core", "beta", true, vec![]);
        entry.limitations.clear();
        entry.graduation_criteria.clear();
        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![entry],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_duplicate_package_entries() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package("copybook-core", "id-core", None, &[])],
        };

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![
                package_entry("copybook-core", "stable", true, vec![]),
                package_entry("copybook-core", "stable", true, vec![]),
            ],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_duplicate_feature_entries() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package(
                "copybook-core",
                "id-core",
                None,
                &["audit", "comprehensive-tests"],
            )],
        };

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![package_entry(
                "copybook-core",
                "stable",
                true,
                vec![
                    feature_entry("audit", "experimental"),
                    feature_entry("audit", "internal-dev-only"),
                ],
            )],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_missing_source_of_truth() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package("copybook-core", "id-core", None, &[])],
        };

        let mut entry = package_entry("copybook-core", "stable", true, vec![]);
        entry.source_of_truth.clear();

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![entry],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_nonexistent_source_of_truth_path() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package("copybook-core", "id-core", None, &[])],
        };

        let mut entry = package_entry("copybook-core", "stable", true, vec![]);
        entry.source_of_truth = vec!["docs/does_not_exist.md".to_string()];

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![entry],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_catches_schema_version_mismatch() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package("copybook-core", "id-core", None, &[])],
        };

        let registry = StabilityRegistry {
            schema_version: "2.0.0".to_string(),
            packages: vec![package_entry("copybook-core", "stable", true, vec![])],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_matches_current_workspace() {
        let registry = load_stability_registry().expect("failed to load surface registry");
        let metadata = load_cargo_metadata().expect("failed to load cargo metadata");

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_ok());
    }

    #[test]
    fn verify_stability_registry_rejects_placeholder_text() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string()],
            packages: vec![metadata_package("copybook-core", "id-core", None, &[])],
        };

        let mut entry = package_entry("copybook-core", "stable", true, vec![]);
        entry.stability_statement = "TBD".to_string();

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![entry],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_err());
    }

    #[test]
    fn verify_stability_registry_accepts_valid_minimal_inventory() {
        let metadata = CargoMetadata {
            workspace_members: vec!["id-core".to_string(), "id-charset".to_string()],
            packages: vec![
                metadata_package(
                    "copybook-core",
                    "id-core",
                    None,
                    &["audit", "comprehensive-tests"],
                ),
                metadata_package(
                    "copybook-charset",
                    "id-charset",
                    None,
                    &["clap", "comprehensive-tests"],
                ),
            ],
        };

        let registry = StabilityRegistry {
            schema_version: STABILITY_SCHEMA_VERSION.to_string(),
            packages: vec![
                package_entry(
                    "copybook-core",
                    "stable",
                    true,
                    vec![
                        feature_entry("audit", "experimental"),
                        feature_entry("comprehensive-tests", "internal-dev-only"),
                    ],
                ),
                package_entry(
                    "copybook-charset",
                    "stable",
                    true,
                    vec![
                        feature_entry("clap", "beta"),
                        feature_entry("comprehensive-tests", "internal-dev-only"),
                    ],
                ),
            ],
        };

        assert!(verify_stability_registry_against_metadata(&registry, &metadata).is_ok());
    }

    #[test]
    fn verify_deprecation_audit_coverage() {
        let discovered =
            collect_deprecated_api_inventory().expect("collect deprecated declarations");
        assert!(
            discovered.len() >= 9,
            "expected at least existing deprecated declarations, found {}",
            discovered.len()
        );
        let discovered_keys: BTreeSet<String> = discovered
            .iter()
            .map(|item| format!("{}:{}", item.path, item.symbol))
            .collect();

        assert!(
            discovered_keys.contains("crates/copybook-arrow/src/legacy.rs:json_type_to_arrow"),
            "missing json_type_to_arrow"
        );
        assert!(
            discovered_keys.contains("crates/copybook-arrow/src/legacy.rs:json_to_schema"),
            "missing json_to_schema"
        );
        assert!(
            discovered_keys.contains("crates/copybook-arrow/src/legacy.rs:json_value_to_array"),
            "missing json_value_to_array"
        );
        assert!(
            discovered_keys.contains("crates/copybook-arrow/src/legacy.rs:json_to_record_batch"),
            "missing json_to_record_batch"
        );
        assert!(
            discovered_keys.contains("crates/copybook-arrow/src/legacy.rs:LegacyArrowWriter"),
            "missing LegacyArrowWriter"
        );
        assert!(
            discovered_keys.contains("crates/copybook-arrow/src/legacy.rs:LegacyParquetFileWriter"),
            "missing LegacyParquetFileWriter"
        );
        assert!(
            discovered_keys.contains("crates/copybook-rdw/src/record.rs:new"),
            "missing RDWRecord::new"
        );
        assert!(
            discovered_keys.contains("crates/copybook-rdw/src/record.rs:with_reserved"),
            "missing RDWRecord::with_reserved"
        );
        assert!(
            discovered_keys.contains("crates/copybook-rdw/src/record.rs:recompute_length"),
            "missing RDWRecord::recompute_length"
        );
    }

    #[test]
    fn verify_deprecation_audit_report_matches_discovered_inventory() {
        let discovered =
            collect_deprecated_api_inventory().expect("collect deprecated declarations");
        let report = load_deprecation_audit_report().expect("load deprecation audit report");

        let discovered_keys: BTreeSet<String> = discovered
            .iter()
            .map(|item| format!("{}:{}", item.path, item.symbol))
            .collect();
        let report_keys: BTreeSet<String> = report
            .items
            .iter()
            .map(|item| format!("{}:{}", item.path, item.symbol))
            .collect();

        assert_eq!(
            discovered_keys, report_keys,
            "deprecation report keys do not match discovered deprecated declarations"
        );

        assert!(
            !report.items.is_empty(),
            "deprecation audit report is empty"
        );
        for item in report.items {
            assert!(!item.deprecated_since.trim().is_empty());
            assert!(!item.replacement.trim().is_empty());
            assert!(!item.migration_example.trim().is_empty());
            assert!(!item.planned_removal.trim().is_empty());
            assert!(!item.compatibility_impact.trim().is_empty());
            assert!(!item.note.trim().is_empty());
        }
    }

    #[test]
    fn verify_surface_deprecation_audit_required_surfaces_are_present() {
        let report =
            load_surface_deprecation_audit_report().expect("load surface deprecation audit report");
        assert!(
            !report.entries.is_empty(),
            "surface deprecation audit report is empty"
        );

        let mut seen = BTreeSet::new();
        for entry in report.entries {
            assert!(
                SURFACE_DEPRECATION_SURFACES.contains(&entry.surface.as_str()),
                "unexpected surface `{}` in surface deprecation audit",
                entry.surface
            );
            seen.insert(entry.surface.clone());
            assert!(
                SURFACE_DEPRECATION_STATES.contains(&entry.state.as_str()),
                "unexpected state `{}` for surface `{}`",
                entry.state,
                entry.surface
            );
        }

        for surface in SURFACE_DEPRECATION_SURFACES {
            assert!(
                seen.contains(surface),
                "missing required surface `{surface}` from surface deprecation audit"
            );
        }
    }

    #[test]
    fn verify_surface_deprecation_audit_output_keys_are_included() {
        let report =
            load_surface_deprecation_audit_report().expect("load surface deprecation audit report");
        let output_deprecated_keys: BTreeSet<String> = report
            .entries
            .into_iter()
            .filter(|entry| entry.surface == "output" && entry.state == "deprecated")
            .map(|entry| entry.symbol)
            .collect();

        assert!(
            output_deprecated_keys.contains("__schema_id"),
            "surface deprecation audit missing deprecated output key `__schema_id`"
        );
        assert!(
            output_deprecated_keys.contains("__raw_b64"),
            "surface deprecation audit missing deprecated output key `__raw_b64`"
        );
    }

    #[test]
    fn record_pipeline_registry_rejects_missing_required_fields() {
        let result = toml::from_str::<RecordPipelineEvidence>(
            "schema_version = 1\nscope = 'fixed-rdw-pipeline'\n",
        );
        assert!(
            result.is_err(),
            "incomplete evidence registry must be rejected"
        );
    }

    #[test]
    fn record_pipeline_anchor_rejects_missing_file() {
        let root = tempfile::tempdir().expect("create temporary workspace");
        let result = verify_test_anchor(
            root.path(),
            "tests/missing.rs::missing_test",
            "format.fixed.basic",
        );
        assert!(result.is_err(), "missing test anchor must be rejected");
    }

    #[test]
    fn record_pipeline_anchor_accepts_exact_function_name() {
        let root = tempfile::tempdir().expect("create temporary workspace");
        fs::create_dir_all(root.path().join("tests")).expect("create anchor fixture directory");
        let source_path = root.path().join("tests/anchors.rs");
        fs::write(
            &source_path,
            "fn exact_anchor() {}\nfn exact_anchor_parallel() {}\n",
        )
        .expect("write anchor fixture");

        verify_test_anchor(
            root.path(),
            "tests/anchors.rs::exact_anchor",
            "format.rdw.odo_variable",
        )
        .expect("exact function anchor must be accepted");
        assert!(
            verify_test_anchor(
                root.path(),
                "tests/anchors.rs::exact_anchor_missing",
                "format.rdw.odo_variable",
            )
            .is_err(),
            "prefix collision must not satisfy an anchor"
        );
    }

    #[test]
    fn record_pipeline_scenarios_accept_valid_row() {
        let root = tempfile::tempdir().expect("create temporary workspace");
        fs::create_dir_all(root.path().join("tests")).expect("create anchor fixture directory");
        let source_path = root.path().join("tests/anchors.rs");
        fs::write(&source_path, "fn valid_anchor() {}\n").expect("write anchor fixture");
        let scenarios = vec![RecordPipelineScenario {
            id: "format.fixed.basic".to_string(),
            record_formats: vec!["fixed".to_string()],
            api_tests: vec!["tests/anchors.rs::valid_anchor".to_string()],
            cli_tests: Vec::new(),
            error_codes: Vec::new(),
            cli_commands: Vec::new(),
            known_limitation: "fixture-only acceptance row".to_string(),
        }];

        verify_record_pipeline_scenarios(root.path(), &scenarios, &[])
            .expect("valid scenario row must be accepted");
    }

    #[test]
    fn verify_surface_deprecation_audit_check_passes() {
        verify_surface_deprecation_audit().expect("surface deprecation audit check failed");
    }
}
