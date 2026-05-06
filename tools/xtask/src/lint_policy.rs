// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail, ensure};
use chrono::{Local, NaiveDate};
use serde::Deserialize;
use std::{collections::BTreeMap, fs, path::Path};
use toml::Value;

const ROOT_MANIFEST: &str = "Cargo.toml";
const LINT_LEDGER: &str = "policy/clippy-lints.toml";
const DEBT_LEDGER: &str = "policy/clippy-debt.toml";
const CLIPPY_CONFIG: &str = "clippy.toml";
const FORBIDDEN_TEST_CARVEOUTS: &[&str] = &[
    "allow-unwrap-in-tests",
    "allow-expect-in-tests",
    "allow-panic-in-tests",
    "allow-indexing-slicing-in-tests",
    "allow-dbg-in-tests",
];
const PLANNED_MSRVS: &[&str] = &["1.94", "1.95"];

#[derive(Debug, Deserialize)]
struct LintLedger {
    schema: u64,
    msrv: String,
    policy: Policy,
    lint: Vec<LintEntry>,
}

#[derive(Debug, Deserialize)]
struct Policy {
    panic_free_tests: bool,
    allow_test_carveouts: bool,
    suppression_style: String,
    blanket_categories: bool,
}

#[derive(Debug, Deserialize)]
struct LintEntry {
    name: String,
    level: String,
    status: String,
    activate_when_msrv: Option<String>,
    class: String,
    reason: String,
}

#[derive(Debug, Deserialize)]
struct DebtLedger {
    schema: u64,
    #[serde(default)]
    debt: Vec<DebtEntry>,
}

#[derive(Debug, Deserialize)]
struct DebtEntry {
    lint: String,
    path: String,
    owner: String,
    reason: String,
    expires: String,
}

/// Verify the workspace lint policy ledgers and manifest wiring.
///
/// # Errors
///
/// Returns an error when Cargo, Clippy, or policy ledgers drift from the shared
/// Effortless Metrics lint policy model.
pub fn check() -> Result<()> {
    let root = read_toml(ROOT_MANIFEST)?;
    let ledger = read_lint_ledger()?;

    ensure!(ledger.schema == 1, "{LINT_LEDGER}: schema must be 1");
    ensure!(
        ledger.policy.panic_free_tests,
        "{LINT_LEDGER}: panic_free_tests must be true"
    );
    ensure!(
        !ledger.policy.allow_test_carveouts,
        "{LINT_LEDGER}: allow_test_carveouts must be false"
    );
    ensure!(
        ledger.policy.suppression_style == "expect-with-reason",
        "{LINT_LEDGER}: suppression_style must be expect-with-reason"
    );
    ensure!(
        !ledger.policy.blanket_categories,
        "{LINT_LEDGER}: blanket_categories must be false"
    );

    let workspace_msrv = root
        .get("workspace")
        .and_then(|v| v.get("package"))
        .and_then(|v| v.get("rust-version"))
        .and_then(Value::as_str)
        .context("Cargo.toml must set workspace.package.rust-version")?;
    ensure!(
        workspace_msrv == ledger.msrv,
        "workspace.package.rust-version ({workspace_msrv}) must match {LINT_LEDGER} msrv ({})",
        ledger.msrv
    );

    check_clippy_config()?;
    check_active_lints(&root, &ledger)?;
    check_workspace_members_inherit_lints(&root)?;
    check_debt_ledger()?;

    println!(
        "✓ lint policy is coherent: MSRV {workspace_msrv}, active lint ledger, planned flips, clippy.toml, workspace inheritance, and debt metadata verified"
    );
    Ok(())
}

fn read_toml(path: &str) -> Result<Value> {
    let content = fs::read_to_string(path).with_context(|| format!("read {path}"))?;
    toml::from_str(&content).with_context(|| format!("parse {path}"))
}

fn read_lint_ledger() -> Result<LintLedger> {
    let content = fs::read_to_string(LINT_LEDGER).with_context(|| format!("read {LINT_LEDGER}"))?;
    toml::from_str(&content).with_context(|| format!("parse {LINT_LEDGER}"))
}

fn check_clippy_config() -> Result<()> {
    let content =
        fs::read_to_string(CLIPPY_CONFIG).with_context(|| format!("read {CLIPPY_CONFIG}"))?;
    for carveout in FORBIDDEN_TEST_CARVEOUTS {
        ensure!(
            !content
                .lines()
                .any(|line| line.trim_start().starts_with(carveout)),
            "{CLIPPY_CONFIG}: forbidden test carveout `{carveout}` is not allowed"
        );
    }
    Ok(())
}

fn check_active_lints(root: &Value, ledger: &LintLedger) -> Result<()> {
    let manifest_lints = manifest_lints(root)?;
    ensure!(
        !manifest_lints.is_empty(),
        "Cargo.toml must define workspace lints"
    );

    let mut active = BTreeMap::new();
    let mut planned = Vec::new();
    for lint in &ledger.lint {
        ensure!(
            !lint.name.trim().is_empty(),
            "{LINT_LEDGER}: lint name is required"
        );
        ensure!(
            !lint.level.trim().is_empty(),
            "{LINT_LEDGER}: lint level is required for {}",
            lint.name
        );
        ensure!(
            !lint.class.trim().is_empty(),
            "{LINT_LEDGER}: class is required for {}",
            lint.name
        );
        ensure!(
            !lint.reason.trim().is_empty(),
            "{LINT_LEDGER}: reason is required for {}",
            lint.name
        );
        match lint.status.as_str() {
            "active" => {
                ensure!(
                    lint.activate_when_msrv.is_none(),
                    "{LINT_LEDGER}: active lint {} must not set activate_when_msrv",
                    lint.name
                );
                active.insert(lint.name.clone(), lint.level.clone());
            }
            "planned" => {
                let msrv = lint.activate_when_msrv.as_deref().with_context(|| {
                    format!(
                        "{LINT_LEDGER}: planned lint {} must set activate_when_msrv",
                        lint.name
                    )
                })?;
                ensure!(
                    PLANNED_MSRVS.contains(&msrv),
                    "{LINT_LEDGER}: planned lint {} must target Rust 1.94 or 1.95",
                    lint.name
                );
                planned.push(&lint.name);
            }
            other => bail!(
                "{LINT_LEDGER}: lint {} has unsupported status `{other}`",
                lint.name
            ),
        }
    }

    for (name, level) in &manifest_lints {
        let Some(active_level) = active.get(name) else {
            bail!("{LINT_LEDGER}: active lint ledger is missing manifest lint {name}");
        };
        ensure!(
            active_level == level,
            "{LINT_LEDGER}: lint {name} level {active_level} does not match Cargo.toml level {level}"
        );
    }
    for (name, level) in &active {
        let Some(manifest_level) = manifest_lints.get(name) else {
            bail!("Cargo.toml: active ledger lint {name} is missing from workspace lints");
        };
        ensure!(
            manifest_level == level,
            "Cargo.toml: lint {name} level {manifest_level} does not match ledger level {level}"
        );
    }
    for name in planned {
        ensure!(
            !manifest_lints.contains_key(name),
            "Cargo.toml: planned lint {name} must not be active before its MSRV bump"
        );
    }
    Ok(())
}

fn manifest_lints(root: &Value) -> Result<BTreeMap<String, String>> {
    let mut out = BTreeMap::new();
    let workspace_lints = root
        .get("workspace")
        .and_then(|v| v.get("lints"))
        .context("Cargo.toml must define [workspace.lints]")?;
    if let Some(rust) = workspace_lints.get("rust").and_then(Value::as_table) {
        for (name, value) in rust {
            if let Some(level) = value.as_str() {
                if level != "allow" {
                    out.insert(name.clone(), level.to_owned());
                }
            }
        }
    }
    if let Some(clippy) = workspace_lints.get("clippy").and_then(Value::as_table) {
        for (name, value) in clippy {
            if let Some(level) = value.as_str() {
                if level != "allow" {
                    out.insert(format!("clippy::{name}"), level.to_owned());
                }
            }
        }
    }
    Ok(out)
}

fn check_workspace_members_inherit_lints(root: &Value) -> Result<()> {
    let members = root
        .get("workspace")
        .and_then(|v| v.get("members"))
        .and_then(Value::as_array)
        .context("Cargo.toml must define workspace.members")?;

    for member in members {
        let Some(member) = member.as_str() else {
            continue;
        };
        if member.contains('*') {
            continue;
        }
        let manifest = Path::new(member).join("Cargo.toml");
        ensure!(
            manifest.exists(),
            "workspace member {} has no Cargo.toml",
            member
        );
        let value = read_toml_path(&manifest)?;
        let inherits = value
            .get("lints")
            .and_then(|v| v.get("workspace"))
            .and_then(Value::as_bool)
            .unwrap_or(false);
        ensure!(
            inherits,
            "{} must contain [lints]\nworkspace = true",
            manifest.display()
        );
    }
    Ok(())
}

fn read_toml_path(path: &Path) -> Result<Value> {
    let content = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
    toml::from_str(&content).with_context(|| format!("parse {}", path.display()))
}

fn check_debt_ledger() -> Result<()> {
    let content = fs::read_to_string(DEBT_LEDGER).with_context(|| format!("read {DEBT_LEDGER}"))?;
    let ledger: DebtLedger =
        toml::from_str(&content).with_context(|| format!("parse {DEBT_LEDGER}"))?;
    ensure!(ledger.schema == 1, "{DEBT_LEDGER}: schema must be 1");
    let today = Local::now().date_naive();
    for debt in ledger.debt {
        ensure!(
            !debt.lint.trim().is_empty(),
            "{DEBT_LEDGER}: debt lint is required"
        );
        ensure!(
            !debt.path.trim().is_empty(),
            "{DEBT_LEDGER}: debt path is required for {}",
            debt.lint
        );
        ensure!(
            !debt.owner.trim().is_empty(),
            "{DEBT_LEDGER}: debt owner is required for {}",
            debt.lint
        );
        ensure!(
            !debt.reason.trim().is_empty(),
            "{DEBT_LEDGER}: debt reason is required for {}",
            debt.lint
        );
        let expires = NaiveDate::parse_from_str(&debt.expires, "%Y-%m-%d")
            .with_context(|| format!("{DEBT_LEDGER}: invalid expires date for {}", debt.lint))?;
        ensure!(
            expires >= today,
            "{DEBT_LEDGER}: debt for {} at {} expired on {expires}",
            debt.lint,
            debt.path
        );
    }
    Ok(())
}
