// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use chrono::{Datelike, Utc};
use std::{collections::BTreeSet, fs, path::Path};
use toml::Value;

const ROOT_CARGO: &str = "Cargo.toml";
const CLIPPY_CONFIG: &str = "clippy.toml";
const LINT_POLICY: &str = "policy/clippy-lints.toml";
const LINT_DEBT: &str = "policy/clippy-debt.toml";
const NO_PANIC_ALLOWLIST: &str = "policy/no-panic-allowlist.toml";
const NON_RUST_ALLOWLIST: &str = "policy/non-rust-allowlist.toml";
const TEST_CARVEOUTS: [&str; 5] = [
    "allow-unwrap-in-tests",
    "allow-expect-in-tests",
    "allow-panic-in-tests",
    "allow-indexing-slicing-in-tests",
    "allow-dbg-in-tests",
];

/// Verify the workspace Clippy policy, policy ledger, and exception ledgers.
///
/// # Errors
///
/// Returns an error when the workspace lint policy drifts from the machine-readable
/// policy files or when required governance fields are missing.
pub fn check_lint_policy() -> Result<()> {
    let root = read_toml(ROOT_CARGO)?;
    let policy = read_toml(LINT_POLICY)?;

    verify_msrv(&root, &policy)?;
    verify_workspace_lints(&root, &policy)?;
    verify_workspace_member_inheritance(&root)?;
    verify_no_test_carveouts()?;
    verify_planned_lints_not_active_early(&root, &policy)?;
    verify_clippy_debt()?;
    verify_no_panic_allowlist()?;
    verify_non_rust_allowlist()?;

    println!("✓ lint policy verified");
    Ok(())
}

fn read_toml(path: &str) -> Result<Value> {
    let content = fs::read_to_string(path).with_context(|| format!("reading {path}"))?;
    toml::from_str(&content).with_context(|| format!("parsing {path}"))
}

fn table<'a>(value: &'a Value, path: &[&str]) -> Result<&'a toml::map::Map<String, Value>> {
    let mut current = value;
    for segment in path {
        current = current
            .get(*segment)
            .with_context(|| format!("missing TOML table segment `{segment}`"))?;
    }
    current
        .as_table()
        .with_context(|| format!("{} is not a TOML table", path.join(".")))
}

fn string_at<'a>(value: &'a Value, path: &[&str]) -> Result<&'a str> {
    let mut current = value;
    for segment in path {
        current = current
            .get(*segment)
            .with_context(|| format!("missing TOML key `{segment}`"))?;
    }
    current
        .as_str()
        .with_context(|| format!("{} must be a string", path.join(".")))
}

fn verify_msrv(root: &Value, policy: &Value) -> Result<()> {
    let cargo_msrv = string_at(root, &["workspace", "package", "rust-version"])?;
    let policy_msrv = string_at(policy, &["msrv"])?;
    if cargo_msrv != policy_msrv {
        bail!("workspace.package.rust-version ({cargo_msrv}) != policy msrv ({policy_msrv})");
    }
    Ok(())
}

fn verify_workspace_lints(root: &Value, policy: &Value) -> Result<()> {
    let rust_lints = table(root, &["workspace", "lints", "rust"])?;
    let clippy_lints = table(root, &["workspace", "lints", "clippy"])?;
    let mut active = BTreeSet::new();

    if let Some(entries) = policy.get("lint").and_then(Value::as_array) {
        for entry in entries {
            let name = required_str(entry, "name", "policy lint")?;
            let level = required_str(entry, "level", name)?;
            let status = required_str(entry, "status", name)?;
            let class = required_str(entry, "class", name)?;
            let reason = required_str(entry, "reason", name)?;
            require_non_empty(class, "class", name)?;
            require_non_empty(reason, "reason", name)?;
            if status == "active" {
                active.insert(format!("{name}={level}"));
                verify_lint_level(name, level, rust_lints, clippy_lints)?;
            }
        }
    } else {
        bail!("policy/clippy-lints.toml must define [[lint]] entries");
    }

    for (name, value) in rust_lints {
        let Some(level) = value.as_str() else {
            bail!("workspace rust lint {name} must be a string level");
        };
        let key = format!("rust::{name}={level}");
        if !active.contains(&key) {
            bail!("active workspace rust lint `{name}` is missing from policy/clippy-lints.toml");
        }
    }
    for (name, value) in clippy_lints {
        let Some(level) = value.as_str() else {
            bail!("workspace clippy lint {name} must be a string level");
        };
        let key = format!("clippy::{name}={level}");
        if !active.contains(&key) {
            bail!("active workspace clippy lint `{name}` is missing from policy/clippy-lints.toml");
        }
    }
    Ok(())
}

fn verify_lint_level(
    name: &str,
    level: &str,
    rust_lints: &toml::map::Map<String, Value>,
    clippy_lints: &toml::map::Map<String, Value>,
) -> Result<()> {
    let (namespace, lint_name) = name
        .split_once("::")
        .with_context(|| format!("lint name `{name}` must include namespace"))?;
    let source = match namespace {
        "rust" => rust_lints,
        "clippy" => clippy_lints,
        _ => bail!("unsupported lint namespace `{namespace}` for {name}"),
    };
    let actual = source
        .get(lint_name)
        .and_then(Value::as_str)
        .with_context(|| format!("active policy lint `{name}` is not present in Cargo.toml"))?;
    if actual != level {
        bail!("active policy lint `{name}` level {level} != Cargo.toml level {actual}");
    }
    Ok(())
}

fn verify_workspace_member_inheritance(root: &Value) -> Result<()> {
    let members = root
        .get("workspace")
        .and_then(|workspace| workspace.get("members"))
        .and_then(Value::as_array)
        .context("workspace.members must be an array")?;

    for member in members {
        let Some(member_path) = member.as_str() else {
            bail!("workspace member entries must be strings");
        };
        let manifest_path = Path::new(member_path).join("Cargo.toml");
        let manifest_text = fs::read_to_string(&manifest_path)
            .with_context(|| format!("reading {}", manifest_path.display()))?;
        let manifest: Value = toml::from_str(&manifest_text)
            .with_context(|| format!("parsing {}", manifest_path.display()))?;
        let inherits = manifest
            .get("lints")
            .and_then(|lints| lints.get("workspace"))
            .and_then(Value::as_bool)
            .unwrap_or(false);
        if !inherits {
            bail!("workspace member `{member_path}` must set [lints] workspace = true");
        }
    }
    Ok(())
}

fn verify_no_test_carveouts() -> Result<()> {
    let config =
        fs::read_to_string(CLIPPY_CONFIG).with_context(|| format!("reading {CLIPPY_CONFIG}"))?;
    for carveout in TEST_CARVEOUTS {
        if config.contains(carveout) {
            bail!("{CLIPPY_CONFIG} must not contain test carveout `{carveout}`");
        }
    }
    Ok(())
}

fn verify_planned_lints_not_active_early(root: &Value, policy: &Value) -> Result<()> {
    let cargo_msrv = string_at(root, &["workspace", "package", "rust-version"])?;
    let rust_lints = table(root, &["workspace", "lints", "rust"])?;
    let clippy_lints = table(root, &["workspace", "lints", "clippy"])?;
    if let Some(planned) = policy.get("planned").and_then(Value::as_array) {
        for entry in planned {
            let name = required_str(entry, "name", "planned lint")?;
            let activate_when = required_str(entry, "activate_when_msrv", name)?;
            let reason = required_str(entry, "reason", name)?;
            require_non_empty(reason, "reason", name)?;
            if cargo_msrv < activate_when && lint_is_active(name, rust_lints, clippy_lints)? {
                bail!("planned lint `{name}` must not be active before MSRV {activate_when}");
            }
        }
    } else {
        bail!("policy/clippy-lints.toml must define [[planned]] entries");
    }
    Ok(())
}

fn lint_is_active(
    name: &str,
    rust_lints: &toml::map::Map<String, Value>,
    clippy_lints: &toml::map::Map<String, Value>,
) -> Result<bool> {
    let (namespace, lint_name) = name
        .split_once("::")
        .with_context(|| format!("lint name `{name}` must include namespace"))?;
    let is_active = match namespace {
        "rust" => rust_lints.contains_key(lint_name),
        "clippy" => clippy_lints.contains_key(lint_name),
        _ => bail!("unsupported planned lint namespace `{namespace}` for {name}"),
    };
    Ok(is_active)
}

fn verify_clippy_debt() -> Result<()> {
    let debt = read_toml(LINT_DEBT)?;
    verify_schema_int(&debt, "schema", LINT_DEBT)?;
    if let Some(entries) = debt.get("debt").and_then(Value::as_array) {
        for entry in entries {
            verify_required_policy_fields(
                entry,
                &["lint", "path", "owner", "reason", "expires"],
                "clippy debt",
            )?;
            verify_not_expired(required_str(entry, "expires", "clippy debt")?, LINT_DEBT)?;
        }
    }
    Ok(())
}

fn verify_no_panic_allowlist() -> Result<()> {
    let allowlist = read_toml(NO_PANIC_ALLOWLIST)?;
    let schema = string_at(&allowlist, &["schema_version"])?;
    if schema != "0.3" {
        bail!("{NO_PANIC_ALLOWLIST} schema_version must be 0.3");
    }
    if let Some(entries) = allowlist.get("allow").and_then(Value::as_array) {
        for entry in entries {
            verify_required_policy_fields(
                entry,
                &["path", "family", "classification", "owner", "explanation"],
                "no-panic allowlist",
            )?;
            let selector = entry
                .get("selector")
                .and_then(Value::as_table)
                .context("no-panic allowlist entry must include [allow.selector]")?;
            for field in ["kind", "container"] {
                let value = selector
                    .get(field)
                    .and_then(Value::as_str)
                    .with_context(|| format!("no-panic selector missing {field}"))?;
                require_non_empty(value, field, "no-panic selector")?;
            }
            if let Some(expires) = entry.get("expires").and_then(Value::as_str) {
                verify_not_expired(expires, NO_PANIC_ALLOWLIST)?;
            }
        }
    }
    Ok(())
}

fn verify_non_rust_allowlist() -> Result<()> {
    let allowlist = read_toml(NON_RUST_ALLOWLIST)?;
    let schema = string_at(&allowlist, &["schema_version"])?;
    if schema != "1.0" {
        bail!("{NON_RUST_ALLOWLIST} schema_version must be 1.0");
    }
    if let Some(entries) = allowlist.get("allow").and_then(Value::as_array) {
        for entry in entries {
            let has_path = entry.get("path").and_then(Value::as_str).is_some();
            let has_glob = entry.get("glob").and_then(Value::as_str).is_some();
            if has_path == has_glob {
                bail!("non-rust allowlist entries must include exactly one of path or glob");
            }
            verify_required_policy_fields(
                entry,
                &["kind", "owner", "reason", "surface", "classification"],
                "non-rust allowlist",
            )?;
            let covered_by = entry
                .get("covered_by")
                .and_then(Value::as_array)
                .context("non-rust allowlist entries must include covered_by")?;
            if covered_by.is_empty() {
                bail!("non-rust allowlist covered_by must not be empty");
            }
            for command in covered_by {
                let Some(command) = command.as_str() else {
                    bail!("non-rust allowlist covered_by entries must be strings");
                };
                require_non_empty(command, "covered_by", "non-rust allowlist")?;
            }
            if let Some(expires) = entry.get("expires").and_then(Value::as_str) {
                verify_not_expired(expires, NON_RUST_ALLOWLIST)?;
            }
        }
    } else {
        bail!("policy/non-rust-allowlist.toml must define [[allow]] entries");
    }
    Ok(())
}

fn verify_schema_int(value: &Value, key: &str, context: &str) -> Result<()> {
    if value.get(key).and_then(Value::as_integer).is_none() {
        bail!("{context} must define integer `{key}`");
    }
    Ok(())
}

fn verify_required_policy_fields(entry: &Value, fields: &[&str], context: &str) -> Result<()> {
    for field in fields {
        let value = required_str(entry, field, context)?;
        require_non_empty(value, field, context)?;
    }
    Ok(())
}

fn required_str<'a>(entry: &'a Value, key: &str, context: &str) -> Result<&'a str> {
    entry
        .get(key)
        .and_then(Value::as_str)
        .with_context(|| format!("{context} missing string field `{key}`"))
}

fn require_non_empty(value: &str, key: &str, context: &str) -> Result<()> {
    if value.trim().is_empty() {
        bail!("{context} field `{key}` must not be empty");
    }
    Ok(())
}

fn verify_not_expired(expires: &str, path: &str) -> Result<()> {
    let today = Utc::now().date_naive();
    let rendered_today = format!(
        "{:04}-{:02}-{:02}",
        today.year(),
        today.month(),
        today.day()
    );
    if expires < rendered_today.as_str() {
        bail!("{path} contains expired policy entry with expires = {expires}");
    }
    Ok(())
}
