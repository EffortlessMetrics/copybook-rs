// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use copybook_bench::{COMP3_CI_FLOOR_MIBPS, DISPLAY_FLOOR_MIBPS};
use regex::Regex;
use std::{collections::BTreeSet, fs, path::Path};

use super::{verify, verify_support_matrix};
use xtask::junit_xml_path;
use xtask::perf;

type Verifier = (&'static str, fn() -> Result<()>);

pub(crate) fn run() -> Result<()> {
    let checks: [Verifier; 10] = [
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
}
