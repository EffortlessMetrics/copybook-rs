// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use regex::Regex;
use std::{collections::BTreeMap, fs, path::Path};
use toml::Value;

const ROOT_MANIFEST: &str = "Cargo.toml";
const CLIPPY_CONFIG: &str = "clippy.toml";
const LINT_LEDGER: &str = "policy/clippy-lints.toml";
const DEBT_LEDGER: &str = "policy/clippy-debt.toml";
const NON_RUST_ALLOWLIST: &str = "policy/non-rust-allowlist.toml";
const NO_PANIC_ALLOWLIST: &str = "policy/no-panic-allowlist.toml";
const CURRENT_DATE: &str = "2026-05-06";

const TEST_CARVEOUTS: &[&str] = &[
    "allow-unwrap-in-tests",
    "allow-expect-in-tests",
    "allow-panic-in-tests",
    "allow-indexing-slicing-in-tests",
    "allow-dbg-in-tests",
];

const NON_RUST_PROGRAMMING_EXTENSIONS: &[&str] = &[
    "py", "js", "ts", "tsx", "jsx", "sh", "bash", "zsh", "ps1", "rb", "go", "java", "c", "h",
    "cpp", "hpp",
];

#[derive(Debug, Clone)]
struct DebtEntry {
    lint: String,
    path: String,
    owner: String,
    reason: String,
    expires: String,
}

#[derive(Debug, Clone)]
struct NonRustAllow {
    pattern: String,
    owner: String,
    reason: String,
    kind: String,
    surface: String,
    classification: String,
    covered_by: Vec<String>,
    expires: Option<String>,
}

pub fn check() -> Result<()> {
    let root = read_toml(ROOT_MANIFEST)?;
    let policy = read_toml(LINT_LEDGER)?;
    check_msrv(&root, &policy)?;
    check_workspace_lints_inheritance(&root)?;
    check_clippy_config()?;
    check_active_lints(&root, &policy)?;
    check_planned_lints(&root, &policy)?;
    check_no_panic_allowlist()?;

    let debt = load_debt()?;
    check_allow_suppressions(&debt)?;
    check_non_rust_files()?;

    println!("✓ lint policy is coherent");
    Ok(())
}

fn read_toml(path: &str) -> Result<Value> {
    toml::from_str::<Value>(&fs::read_to_string(path).with_context(|| format!("reading {path}"))?)
        .with_context(|| format!("parsing {path}"))
}

fn table<'a>(value: &'a Value, path: &[&str]) -> Result<&'a toml::map::Map<String, Value>> {
    let mut cursor = value;
    for part in path {
        cursor = cursor
            .get(*part)
            .with_context(|| format!("missing TOML table path {}", path.join(".")))?;
    }
    cursor
        .as_table()
        .with_context(|| format!("{} is not a TOML table", path.join(".")))
}

fn string_at<'a>(
    table: &'a toml::map::Map<String, Value>,
    key: &str,
    context: &str,
) -> Result<&'a str> {
    table
        .get(key)
        .and_then(Value::as_str)
        .with_context(|| format!("{context} missing required string field `{key}`"))
}

fn check_msrv(root: &Value, policy: &Value) -> Result<()> {
    let workspace_package = table(root, &["workspace", "package"])?;
    let root_msrv = string_at(workspace_package, "rust-version", ROOT_MANIFEST)?;
    let policy_msrv = policy
        .get("msrv")
        .and_then(Value::as_str)
        .context("policy/clippy-lints.toml missing `msrv`")?;

    if root_msrv != policy_msrv {
        bail!(
            "workspace.package.rust-version ({root_msrv}) must match policy msrv ({policy_msrv})"
        );
    }
    Ok(())
}

fn check_workspace_lints_inheritance(root: &Value) -> Result<()> {
    let workspace = table(root, &["workspace"])?;
    let members = workspace
        .get("members")
        .and_then(Value::as_array)
        .context("workspace.members must be an array")?;

    let mut missing = Vec::new();
    for member in members {
        let Some(member) = member.as_str() else {
            continue;
        };
        let manifest = Path::new(member).join("Cargo.toml");
        let manifest_text = fs::read_to_string(&manifest)
            .with_context(|| format!("reading workspace member manifest {}", manifest.display()))?;
        let manifest_toml = toml::from_str::<Value>(&manifest_text)
            .with_context(|| format!("parsing workspace member manifest {}", manifest.display()))?;
        let inherits = manifest_toml
            .get("lints")
            .and_then(Value::as_table)
            .and_then(|lints| lints.get("workspace"))
            .and_then(Value::as_bool)
            .unwrap_or(false);
        if !inherits {
            missing.push(manifest.display().to_string());
        }
    }

    if !missing.is_empty() {
        bail!(
            "workspace members must inherit `[lints] workspace = true`:\n  - {}",
            missing.join("\n  - ")
        );
    }
    Ok(())
}

fn check_clippy_config() -> Result<()> {
    let config =
        fs::read_to_string(CLIPPY_CONFIG).with_context(|| format!("reading {CLIPPY_CONFIG}"))?;
    let parsed =
        toml::from_str::<Value>(&config).with_context(|| format!("parsing {CLIPPY_CONFIG}"))?;

    let mut found = Vec::new();
    for key in TEST_CARVEOUTS {
        if parsed.get(*key).is_some() {
            found.push(*key);
        }
    }
    if !found.is_empty() {
        bail!(
            "clippy.toml must not enable test carveouts: {}",
            found.join(", ")
        );
    }

    let msrv = parsed
        .get("msrv")
        .and_then(Value::as_str)
        .context("clippy.toml missing `msrv`")?;
    let policy_msrv = read_toml(LINT_LEDGER)?
        .get("msrv")
        .and_then(Value::as_str)
        .map(str::to_owned)
        .context("policy/clippy-lints.toml missing `msrv`")?;
    if msrv != policy_msrv {
        bail!("clippy.toml msrv ({msrv}) must match policy msrv ({policy_msrv})");
    }
    Ok(())
}

fn check_active_lints(root: &Value, policy: &Value) -> Result<()> {
    let mut cargo_lints = BTreeMap::new();
    for (tool, path) in [
        ("rust", ["workspace", "lints", "rust"]),
        ("clippy", ["workspace", "lints", "clippy"]),
    ] {
        for (name, level) in table(root, &path)? {
            let Some(level) = level.as_str() else {
                bail!("{ROOT_MANIFEST} lint {tool}::{name} must have string level");
            };
            cargo_lints.insert(format!("{tool}::{name}"), level.to_owned());
        }
    }

    let mut policy_lints = BTreeMap::new();
    for lint in lint_entries(policy)? {
        let lint_table = lint.as_table().context("[[lint]] entry must be a table")?;
        let status = string_at(lint_table, "status", LINT_LEDGER)?;
        if status != "active" {
            continue;
        }
        let name = string_at(lint_table, "name", LINT_LEDGER)?;
        let level = string_at(lint_table, "level", LINT_LEDGER)?;
        for required in ["class", "reason"] {
            let value = string_at(lint_table, required, LINT_LEDGER)?;
            if value.trim().is_empty() {
                bail!("active lint {name} has empty `{required}`");
            }
        }
        policy_lints.insert(name.to_owned(), level.to_owned());
    }

    if cargo_lints != policy_lints {
        let cargo_only: Vec<_> = cargo_lints
            .keys()
            .filter(|key| !policy_lints.contains_key(*key))
            .cloned()
            .collect();
        let policy_only: Vec<_> = policy_lints
            .keys()
            .filter(|key| !cargo_lints.contains_key(*key))
            .cloned()
            .collect();
        bail!(
            "active lints in {LINT_LEDGER} must match {ROOT_MANIFEST}\nCargo-only: {:?}\nPolicy-only: {:?}",
            cargo_only,
            policy_only
        );
    }
    Ok(())
}

fn check_planned_lints(root: &Value, policy: &Value) -> Result<()> {
    let root_msrv = string_at(
        table(root, &["workspace", "package"])?,
        "rust-version",
        ROOT_MANIFEST,
    )?;
    let active = active_lint_names(root)?;
    for lint in lint_entries(policy)? {
        let lint_table = lint.as_table().context("[[lint]] entry must be a table")?;
        let status = string_at(lint_table, "status", LINT_LEDGER)?;
        if status != "planned" {
            continue;
        }
        let name = string_at(lint_table, "name", LINT_LEDGER)?;
        let activate_when = string_at(lint_table, "activate_when_msrv", LINT_LEDGER)?;
        for required in ["level", "class", "reason"] {
            let value = string_at(lint_table, required, LINT_LEDGER)?;
            if value.trim().is_empty() {
                bail!("planned lint {name} has empty `{required}`");
            }
        }
        if version_lt(root_msrv, activate_when) && active.contains(&name.to_owned()) {
            bail!("planned lint {name} must not be active before MSRV {activate_when}");
        }
    }
    Ok(())
}

fn lint_entries(policy: &Value) -> Result<&Vec<Value>> {
    policy
        .get("lint")
        .and_then(Value::as_array)
        .context("policy/clippy-lints.toml must contain [[lint]] entries")
}

fn active_lint_names(root: &Value) -> Result<Vec<String>> {
    let mut names = Vec::new();
    for (tool, path) in [
        ("rust", ["workspace", "lints", "rust"]),
        ("clippy", ["workspace", "lints", "clippy"]),
    ] {
        names.extend(
            table(root, &path)?
                .keys()
                .map(|name| format!("{tool}::{name}")),
        );
    }
    Ok(names)
}

fn version_lt(left: &str, right: &str) -> bool {
    let parse = |s: &str| -> Vec<u64> {
        s.split('.')
            .map(|part| part.parse::<u64>().unwrap_or(0))
            .collect()
    };
    parse(left) < parse(right)
}

fn check_no_panic_allowlist() -> Result<()> {
    let value = read_toml(NO_PANIC_ALLOWLIST)?;
    if value.get("schema_version").and_then(Value::as_str) != Some("0.3") {
        bail!("{NO_PANIC_ALLOWLIST} must set schema_version = \"0.3\"");
    }

    let Some(entries) = value.get("allow") else {
        return Ok(());
    };
    let entries = entries
        .as_array()
        .context("policy/no-panic-allowlist.toml [[allow]] entries must be an array")?;
    for entry in entries {
        let entry = entry
            .as_table()
            .context("[[allow]] entry must be a table")?;
        for field in ["path", "family", "classification", "owner", "explanation"] {
            let value = string_at(entry, field, NO_PANIC_ALLOWLIST)?;
            if value.trim().is_empty() {
                bail!("panic allowlist entry has empty `{field}`");
            }
        }
        let selector = entry
            .get("selector")
            .and_then(Value::as_table)
            .context("panic allowlist entry missing [allow.selector]")?;
        for field in ["kind", "container", "callee"] {
            let value = string_at(selector, field, NO_PANIC_ALLOWLIST)?;
            if value.trim().is_empty() {
                bail!("panic allowlist selector has empty `{field}`");
            }
        }
        if let Some(expires) = entry.get("expires").and_then(Value::as_str) {
            if expires <= CURRENT_DATE {
                let path = string_at(entry, "path", NO_PANIC_ALLOWLIST)?;
                bail!("expired panic allowlist entry for {path}: expires {expires}");
            }
        }
    }
    Ok(())
}

fn load_debt() -> Result<Vec<DebtEntry>> {
    let debt_toml = read_toml(DEBT_LEDGER)?;
    if debt_toml.get("schema").and_then(Value::as_integer) != Some(1) {
        bail!("{DEBT_LEDGER} must set schema = 1");
    }

    let entries = debt_toml
        .get("debt")
        .and_then(Value::as_array)
        .context("policy/clippy-debt.toml must contain [[debt]] entries")?;
    let mut debt = Vec::new();
    for entry in entries {
        let entry = entry.as_table().context("[[debt]] entry must be a table")?;
        let lint = required_nonempty(entry, "lint", DEBT_LEDGER)?;
        let path = required_nonempty(entry, "path", DEBT_LEDGER)?;
        let owner = required_nonempty(entry, "owner", DEBT_LEDGER)?;
        let reason = required_nonempty(entry, "reason", DEBT_LEDGER)?;
        let expires = required_nonempty(entry, "expires", DEBT_LEDGER)?;
        if expires.as_str() <= CURRENT_DATE {
            bail!("expired lint debt for {lint} at {path}: expires {expires}");
        }
        debt.push(DebtEntry {
            lint,
            path,
            owner,
            reason,
            expires,
        });
    }
    Ok(debt)
}

fn required_nonempty(
    table: &toml::map::Map<String, Value>,
    key: &str,
    context: &str,
) -> Result<String> {
    let value = string_at(table, key, context)?.trim().to_owned();
    if value.is_empty() {
        bail!("{context} contains empty required field `{key}`");
    }
    Ok(value)
}

fn check_allow_suppressions(debt: &[DebtEntry]) -> Result<()> {
    let allow_re =
        Regex::new(r"#\s*!?\s*\[\s*allow\s*\(([^\)]*)\)").context("compiling allow regex")?;
    let mut uncovered = Vec::new();
    for path in rust_files(Path::new("."))? {
        let display_path = normalize_path(&path);
        let source =
            fs::read_to_string(&path).with_context(|| format!("reading {display_path}"))?;
        for (line_idx, line) in source.lines().enumerate() {
            let Some(captures) = allow_re.captures(line) else {
                continue;
            };
            let Some(lints_match) = captures.get(1) else {
                continue;
            };
            let covered = debt.iter().any(|entry| {
                let _ = (&entry.owner, &entry.reason, &entry.expires);
                glob_matches(&entry.path, &display_path)
                    && (entry.lint == "allow-attributes"
                        || lints_match.as_str().contains(&entry.lint))
            });
            if !covered {
                uncovered.push(format!("{display_path}:{}: {line}", line_idx + 1));
            }
        }
    }
    if !uncovered.is_empty() {
        bail!(
            "#[allow] suppressions must be migrated to #[expect(..., reason = ...)] or covered by expiring policy debt:\n  - {}",
            uncovered.join("\n  - ")
        );
    }
    Ok(())
}

fn rust_files(root: &Path) -> Result<Vec<std::path::PathBuf>> {
    let mut files = Vec::new();
    collect_files(root, &mut files, |path| {
        path.extension().and_then(|ext| ext.to_str()) == Some("rs")
    })?;
    Ok(files)
}

fn check_non_rust_files() -> Result<()> {
    let allowlist = load_non_rust_allowlist()?;
    let mut files = Vec::new();
    collect_files(Path::new("."), &mut files, |path| {
        path.extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| NON_RUST_PROGRAMMING_EXTENSIONS.contains(&ext))
    })?;

    let mut uncovered = Vec::new();
    for file in files {
        let display_path = normalize_path(&file);
        let covered = allowlist.iter().any(|entry| {
            let _ = (
                &entry.owner,
                &entry.reason,
                &entry.kind,
                &entry.surface,
                &entry.classification,
                &entry.covered_by,
                &entry.expires,
            );
            glob_matches(&entry.pattern, &display_path)
        });
        if !covered {
            uncovered.push(display_path);
        }
    }

    if !uncovered.is_empty() {
        bail!(
            "non-Rust programming files must be covered by {NON_RUST_ALLOWLIST}:\n  - {}",
            uncovered.join("\n  - ")
        );
    }
    Ok(())
}

fn load_non_rust_allowlist() -> Result<Vec<NonRustAllow>> {
    let value = read_toml(NON_RUST_ALLOWLIST)?;
    if value.get("schema_version").and_then(Value::as_str) != Some("1.0") {
        bail!("{NON_RUST_ALLOWLIST} must set schema_version = \"1.0\"");
    }
    let entries = value
        .get("allow")
        .and_then(Value::as_array)
        .context("policy/non-rust-allowlist.toml must contain [[allow]] entries")?;
    let mut allowlist = Vec::new();
    for entry in entries {
        let entry = entry
            .as_table()
            .context("[[allow]] entry must be a table")?;
        let path = entry.get("path").and_then(Value::as_str);
        let glob = entry.get("glob").and_then(Value::as_str);
        let pattern = match (path, glob) {
            (Some(_), Some(_)) => {
                bail!("{NON_RUST_ALLOWLIST} entries must use either path or glob, not both")
            }
            (Some(path), None) => path.to_owned(),
            (None, Some(glob)) => glob.to_owned(),
            (None, None) => bail!("{NON_RUST_ALLOWLIST} entry missing path or glob"),
        };
        let owner = required_nonempty(entry, "owner", NON_RUST_ALLOWLIST)?;
        let reason = required_nonempty(entry, "reason", NON_RUST_ALLOWLIST)?;
        let kind = required_nonempty(entry, "kind", NON_RUST_ALLOWLIST)?;
        let surface = required_nonempty(entry, "surface", NON_RUST_ALLOWLIST)?;
        let classification = required_nonempty(entry, "classification", NON_RUST_ALLOWLIST)?;
        let covered_by = entry
            .get("covered_by")
            .and_then(Value::as_array)
            .context("non-Rust allowlist entry missing covered_by array")?
            .iter()
            .map(|item| {
                item.as_str()
                    .map(str::to_owned)
                    .context("covered_by entries must be strings")
            })
            .collect::<Result<Vec<_>>>()?;
        if covered_by.is_empty() {
            bail!(
                "non-Rust allowlist entry for {pattern} must have at least one covered_by command"
            );
        }
        let expires = entry
            .get("expires")
            .and_then(Value::as_str)
            .map(str::to_owned);
        if let Some(expires) = &expires {
            if expires.as_str() <= CURRENT_DATE {
                bail!("expired non-Rust allowlist entry for {pattern}: expires {expires}");
            }
        }
        allowlist.push(NonRustAllow {
            pattern,
            owner,
            reason,
            kind,
            surface,
            classification,
            covered_by,
            expires,
        });
    }
    Ok(allowlist)
}

fn collect_files(
    dir: &Path,
    files: &mut Vec<std::path::PathBuf>,
    predicate: impl Fn(&Path) -> bool + Copy,
) -> Result<()> {
    for entry in
        fs::read_dir(dir).with_context(|| format!("reading directory {}", dir.display()))?
    {
        let entry = entry?;
        let path = entry.path();
        if is_ignored_path(&path) {
            continue;
        }
        if path.is_dir() {
            collect_files(&path, files, predicate)?;
        } else if path.is_file() && predicate(&path) {
            files.push(path);
        }
    }
    Ok(())
}

fn is_ignored_path(path: &Path) -> bool {
    path.components().any(|component| {
        let part = component.as_os_str().to_string_lossy();
        matches!(part.as_ref(), ".git" | "target")
    })
}

fn normalize_path(path: &Path) -> String {
    path.strip_prefix(".")
        .unwrap_or(path)
        .to_string_lossy()
        .trim_start_matches('/')
        .replace('\\', "/")
}

fn glob_matches(pattern: &str, path: &str) -> bool {
    if pattern == path {
        return true;
    }
    let mut regex = String::from("^");
    let mut chars = pattern.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '*' if chars.peek() == Some(&'*') => {
                chars.next();
                if chars.peek() == Some(&'/') {
                    chars.next();
                    regex.push_str("(?:.*/)?");
                } else {
                    regex.push_str(".*");
                }
            }
            '*' => regex.push_str("[^/]*"),
            '?' => regex.push('.'),
            '.' | '+' | '(' | ')' | '|' | '^' | '$' | '{' | '}' | '[' | ']' | '\\' => {
                regex.push('\\');
                regex.push(ch);
            }
            other => regex.push(other),
        }
    }
    regex.push('$');
    Regex::new(&regex).is_ok_and(|re| re.is_match(path))
}
