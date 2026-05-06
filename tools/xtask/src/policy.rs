// SPDX-License-Identifier: AGPL-3.0-or-later
//! Policy checks for the governed lint and allowlist surface.

use anyhow::{Context, Result, bail};
use std::{collections::BTreeMap, fs, path::Path};
use toml::Value;

const ROOT_MANIFEST: &str = "Cargo.toml";
const CLIPPY_LEDGER: &str = "policy/clippy-lints.toml";
const CLIPPY_DEBT: &str = "policy/clippy-debt.toml";
const CLIPPY_CONFIG: &str = "clippy.toml";
const NO_PANIC_ALLOWLIST: &str = "policy/no-panic-allowlist.toml";
const NON_RUST_ALLOWLIST: &str = "policy/non-rust-allowlist.toml";

const TEST_CARVEOUTS: &[&str] = &[
    "allow-unwrap-in-tests",
    "allow-expect-in-tests",
    "allow-panic-in-tests",
    "allow-indexing-slicing-in-tests",
    "allow-dbg-in-tests",
];

/// Run the Clippy policy gate.
///
/// # Errors
///
/// Returns an error if the manifest, lint ledger, clippy config, or debt ledger
/// diverges from the governed workspace policy.
pub fn check_lint_policy() -> Result<()> {
    let root = read_toml(ROOT_MANIFEST)?;
    let ledger = read_toml(CLIPPY_LEDGER)?;

    let manifest_msrv = value_path(&root, &["workspace", "package", "rust-version"])?
        .as_str()
        .context("workspace.package.rust-version must be a string")?;
    let policy_msrv = value_path(&ledger, &["msrv"])?
        .as_str()
        .context("policy/clippy-lints.toml msrv must be a string")?;
    if manifest_msrv != policy_msrv {
        bail!("MSRV drift: Cargo.toml has {manifest_msrv}, policy ledger has {policy_msrv}");
    }

    check_policy_flags(&ledger)?;
    check_member_lint_inheritance(&root)?;
    check_active_lints_match_manifest(&root, &ledger)?;
    check_planned_lints_inactive(&root, &ledger, manifest_msrv)?;
    check_no_test_carveouts()?;
    check_clippy_debt()?;

    println!("✓ lint policy is coherent");
    Ok(())
}

/// Validate the panic-family allowlist schema.
///
/// # Errors
///
/// Returns an error when an allowlist entry is missing required structured
/// receipt fields or has expired.
pub fn check_no_panic_family() -> Result<()> {
    let value = read_toml(NO_PANIC_ALLOWLIST)?;
    let entries = table_array(&value, "allow")?;
    for entry in &entries {
        require_entry_string(entry, "path", NO_PANIC_ALLOWLIST)?;
        require_entry_string(entry, "family", NO_PANIC_ALLOWLIST)?;
        require_entry_string(entry, "classification", NO_PANIC_ALLOWLIST)?;
        require_entry_string(entry, "owner", NO_PANIC_ALLOWLIST)?;
        require_entry_string(entry, "explanation", NO_PANIC_ALLOWLIST)?;
        require_future_expiry(entry, NO_PANIC_ALLOWLIST)?;
        let Some(selector) = entry.get("selector").and_then(Value::as_table) else {
            bail!("{NO_PANIC_ALLOWLIST}: every allow entry needs [allow.selector]");
        };
        require_table_string(selector, "kind", NO_PANIC_ALLOWLIST)?;
    }
    println!(
        "✓ no-panic allowlist schema is coherent ({} entries)",
        entries.len()
    );
    Ok(())
}

/// Validate the non-Rust file policy allowlist schema.
///
/// # Errors
///
/// Returns an error when an allowlist entry is missing ownership, reason,
/// classification, surface, coverage, or has expired.
pub fn check_file_policy() -> Result<()> {
    let value = read_toml(NON_RUST_ALLOWLIST)?;
    let entries = table_array(&value, "allow")?;
    for entry in &entries {
        if !entry.contains_key("path") && !entry.contains_key("glob") {
            bail!("{NON_RUST_ALLOWLIST}: every allow entry needs path or glob");
        }
        require_entry_string(entry, "kind", NON_RUST_ALLOWLIST)?;
        require_entry_string(entry, "owner", NON_RUST_ALLOWLIST)?;
        require_entry_string(entry, "reason", NON_RUST_ALLOWLIST)?;
        require_entry_string(entry, "surface", NON_RUST_ALLOWLIST)?;
        require_entry_string(entry, "classification", NON_RUST_ALLOWLIST)?;
        let covered_by = entry
            .get("covered_by")
            .and_then(Value::as_array)
            .context("policy/non-rust-allowlist.toml: every allow entry needs covered_by")?;
        if covered_by.is_empty() || !covered_by.iter().all(|item| item.as_str().is_some()) {
            bail!("{NON_RUST_ALLOWLIST}: covered_by must be a non-empty string array");
        }
        require_future_expiry(entry, NON_RUST_ALLOWLIST)?;
    }
    println!(
        "✓ non-Rust file policy schema is coherent ({} entries)",
        entries.len()
    );
    Ok(())
}

/// Print a compact policy report.
///
/// # Errors
///
/// Returns an error if any policy file cannot be read or parsed.
pub fn report() -> Result<()> {
    let clippy = read_toml(CLIPPY_LEDGER)?;
    let panic = read_toml(NO_PANIC_ALLOWLIST)?;
    let non_rust = read_toml(NON_RUST_ALLOWLIST)?;
    let debt = read_toml(CLIPPY_DEBT)?;

    let lints = table_array(&clippy, "lint")?;
    let active = lints
        .iter()
        .filter(|entry| entry.get("status").and_then(Value::as_str) == Some("active"))
        .count();
    let planned = lints.len().saturating_sub(active);
    let panic_entries = table_array(&panic, "allow")?.len();
    let non_rust_entries = table_array(&non_rust, "allow")?.len();
    let debt_entries = table_array(&debt, "debt")?.len();

    println!("lint policy: {active} active, {planned} planned");
    println!("panic exceptions: {panic_entries} active");
    println!("non-rust exceptions: {non_rust_entries} active");
    println!("clippy debt: {debt_entries} active");
    Ok(())
}

fn read_toml(path: &str) -> Result<Value> {
    let content = fs::read_to_string(path).with_context(|| format!("failed to read {path}"))?;
    toml::from_str(&content).with_context(|| format!("failed to parse {path}"))
}

fn value_path<'a>(value: &'a Value, path: &[&str]) -> Result<&'a Value> {
    let mut current = value;
    for key in path {
        current = current
            .get(*key)
            .with_context(|| format!("missing TOML path {}", path.join(".")))?;
    }
    Ok(current)
}

fn table_array<'a>(value: &'a Value, key: &str) -> Result<Vec<&'a toml::Table>> {
    let Some(array) = value.get(key).and_then(Value::as_array) else {
        return Ok(Vec::new());
    };
    let mut tables = Vec::with_capacity(array.len());
    for item in array {
        let table = item
            .as_table()
            .with_context(|| format!("{key} entries must be TOML tables"))?;
        tables.push(table);
    }
    Ok(tables)
}

fn check_policy_flags(ledger: &Value) -> Result<()> {
    let policy = value_path(ledger, &["policy"])?
        .as_table()
        .context("policy must be a table")?;
    expect_bool(policy, "panic_free_tests", true)?;
    expect_bool(policy, "allow_test_carveouts", false)?;
    expect_bool(policy, "blanket_categories", false)?;
    let style = policy
        .get("suppression_style")
        .and_then(Value::as_str)
        .context("policy.suppression_style must be a string")?;
    if style != "expect-with-reason" {
        bail!("policy.suppression_style must be expect-with-reason");
    }
    Ok(())
}

fn expect_bool(table: &toml::Table, key: &str, expected: bool) -> Result<()> {
    let actual = table
        .get(key)
        .and_then(Value::as_bool)
        .with_context(|| format!("policy.{key} must be a boolean"))?;
    if actual != expected {
        bail!("policy.{key} must be {expected}");
    }
    Ok(())
}

fn check_member_lint_inheritance(root: &Value) -> Result<()> {
    let members = value_path(root, &["workspace", "members"])?
        .as_array()
        .context("workspace.members must be an array")?;
    for member in members {
        let path = member
            .as_str()
            .context("workspace.members entries must be strings")?;
        let manifest_path = Path::new(path).join("Cargo.toml");
        let manifest = read_toml_path(&manifest_path)?;
        let workspace_lints = manifest
            .get("lints")
            .and_then(Value::as_table)
            .and_then(|lints| lints.get("workspace"))
            .and_then(Value::as_bool);
        if workspace_lints != Some(true) {
            bail!(
                "{} must inherit workspace lints with [lints] workspace = true",
                manifest_path.display()
            );
        }
    }
    Ok(())
}

fn read_toml_path(path: &Path) -> Result<Value> {
    let content =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    toml::from_str(&content).with_context(|| format!("failed to parse {}", path.display()))
}

fn check_active_lints_match_manifest(root: &Value, ledger: &Value) -> Result<()> {
    let manifest_lints = manifest_lints(root)?;
    let mut ledger_lints = BTreeMap::new();
    for entry in table_array(ledger, "lint")? {
        if entry.get("status").and_then(Value::as_str) != Some("active") {
            continue;
        }
        let name = require_entry_string(entry, "name", CLIPPY_LEDGER)?;
        let level = require_entry_string(entry, "level", CLIPPY_LEDGER)?;
        ledger_lints.insert(name.to_owned(), level.to_owned());
    }
    if manifest_lints != ledger_lints {
        let missing: Vec<_> = manifest_lints
            .keys()
            .filter(|key| !ledger_lints.contains_key(*key))
            .cloned()
            .collect();
        let stale: Vec<_> = ledger_lints
            .keys()
            .filter(|key| !manifest_lints.contains_key(*key))
            .cloned()
            .collect();
        bail!(
            "active lint ledger must match Cargo.toml (missing in ledger: {:?}; stale in ledger: {:?})",
            missing,
            stale
        );
    }
    Ok(())
}

fn manifest_lints(root: &Value) -> Result<BTreeMap<String, String>> {
    let mut lints = BTreeMap::new();
    for (scope, path) in [
        ("rust", ["workspace", "lints", "rust"]),
        ("clippy", ["workspace", "lints", "clippy"]),
    ] {
        let table = value_path(root, &path)?
            .as_table()
            .with_context(|| format!("{} must be a table", path.join(".")))?;
        for (name, value) in table {
            let level = value
                .as_str()
                .with_context(|| format!("lint {scope}::{name} level must be a string"))?;
            let full_name = if scope == "clippy" {
                format!("clippy::{name}")
            } else {
                name.to_owned()
            };
            lints.insert(full_name, level.to_owned());
        }
    }
    Ok(lints)
}

fn check_planned_lints_inactive(root: &Value, ledger: &Value, msrv: &str) -> Result<()> {
    let manifest = manifest_lints(root)?;
    for entry in table_array(ledger, "lint")? {
        if entry.get("status").and_then(Value::as_str) != Some("planned") {
            continue;
        }
        let name = require_entry_string(entry, "name", CLIPPY_LEDGER)?;
        let activate_when = require_entry_string(entry, "activate_when_msrv", CLIPPY_LEDGER)?;
        if semver_less(msrv, activate_when) && manifest.contains_key(name) {
            bail!("planned lint {name} must stay inactive until MSRV {activate_when}");
        }
    }
    Ok(())
}

fn semver_less(left: &str, right: &str) -> bool {
    parse_minor_version(left) < parse_minor_version(right)
}

fn parse_minor_version(version: &str) -> (u64, u64) {
    let mut parts = version.split('.');
    let major = parts.next().and_then(|part| part.parse().ok()).unwrap_or(0);
    let minor = parts.next().and_then(|part| part.parse().ok()).unwrap_or(0);
    (major, minor)
}

fn check_no_test_carveouts() -> Result<()> {
    let content = fs::read_to_string(CLIPPY_CONFIG).context("failed to read clippy.toml")?;
    for carveout in TEST_CARVEOUTS {
        let banned = format!("{carveout} = true");
        if content.contains(&banned) {
            bail!("{CLIPPY_CONFIG} must not enable test carveout {carveout}");
        }
    }
    Ok(())
}

fn check_clippy_debt() -> Result<()> {
    let debt = read_toml(CLIPPY_DEBT)?;
    for entry in table_array(&debt, "debt")? {
        require_entry_string(entry, "lint", CLIPPY_DEBT)?;
        require_entry_string(entry, "path", CLIPPY_DEBT)?;
        require_entry_string(entry, "owner", CLIPPY_DEBT)?;
        require_entry_string(entry, "reason", CLIPPY_DEBT)?;
        require_entry_string(entry, "expires", CLIPPY_DEBT)?;
        require_future_expiry(entry, CLIPPY_DEBT)?;
    }
    Ok(())
}

fn require_entry_string<'a>(entry: &'a toml::Table, key: &str, file: &str) -> Result<&'a str> {
    let value = entry
        .get(key)
        .and_then(Value::as_str)
        .with_context(|| format!("{file}: every entry needs string field {key}"))?;
    if value.trim().is_empty() {
        bail!("{file}: field {key} must not be empty");
    }
    Ok(value)
}

fn require_table_string<'a>(entry: &'a toml::Table, key: &str, file: &str) -> Result<&'a str> {
    require_entry_string(entry, key, file)
}

fn require_future_expiry(entry: &toml::Table, file: &str) -> Result<()> {
    let Some(expires) = entry.get("expires").and_then(Value::as_str) else {
        return Ok(());
    };
    if expires <= "2026-05-06" {
        bail!("{file}: entry expired on {expires}");
    }
    Ok(())
}
