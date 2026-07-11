// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use copybook_bench::{COMP3_CI_FLOOR_MIBPS, DISPLAY_FLOOR_MIBPS};
use regex::Regex;
use serde::Deserialize;
use serde_json::Value;
use std::{
    collections::{BTreeMap, BTreeSet},
    env, fs,
    path::Path,
    process::Command,
};

use super::{verify, verify_support_matrix};
use xtask::junit_xml_path;
use xtask::perf;

type Verifier = (&'static str, fn() -> Result<()>);

pub(crate) fn run() -> Result<()> {
    let checks: [Verifier; 11] = [
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
        ("quick-start-versioning", verify_quick_start_versioning),
    ];
    run_checks(&checks)
}

fn run_checks(checks: &[Verifier]) -> Result<()> {
    for (name, check) in checks {
        check().map_err(|err| anyhow::anyhow!("{name} failed: {err}"))?;
    }

    println!("docs verify-all completed");
    Ok(())
}

const STABILITY_SCHEMA_VERSION: &str = "1.0.0";
const STABILITY_REGISTRY_PATH: &str = "docs/stability/surface-registry.json";
const STABILITY_MANUAL_REVIEW_PLACEHOLDERS: [&str; 2] = ["tbd", "set during manual review"];

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

    let mut current = env::current_dir().ok()?;
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

    let (lib_only, dep_only) = symmetric_diff(&lib_module_set, &dep_module_set);
    if !(lib_only.is_empty() && dep_only.is_empty()) {
        bail!(
            "copybook facade modules mismatch dependency list: lib-only={lib_only:?} dep-only={dep_only:?} | authoritative-source=crates/copybook/src/lib.rs and crates/copybook/Cargo.toml"
        );
    }

    let (readme_only, lib_readme_only) = symmetric_diff(&readme_module_set, &lib_module_set);
    if !(readme_only.is_empty() && lib_readme_only.is_empty()) {
        bail!(
            "copybook README module inventory mismatch: readme-only={readme_only:?} lib-only={lib_readme_only:?} | authoritative-source=crates/copybook/README.md and crates/copybook/src/lib.rs"
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
    use std::collections::BTreeMap;
    use std::sync::{Mutex, OnceLock};

    use super::*;

    fn ok() -> Result<()> {
        Ok(())
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
}
