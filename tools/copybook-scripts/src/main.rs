//! SPDX-License-Identifier: AGPL-3.0-or-later
use std::collections::HashSet;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, bail};
use chrono::SecondsFormat;
use clap::{Parser, Subcommand};
use regex::Regex;
use serde_json::{Map, Value};
use sha2::{Digest, Sha256};

#[derive(Debug, Parser)]
#[command(name = "copybook-scripts")]
#[command(about = "Run copybook-rs repository script-equivalent checks.")]
struct Cli {
    #[command(subcommand)]
    command: CommandKind,
}

#[derive(Debug, Subcommand)]
enum CommandKind {
    CheckNoUnwrapExpect,
    CheckPerformanceDocs,
    CheckPublicResultDocs,
    GuardHotpaths,
    PerfAnnotateHost,
    SoakDispatch,
    ValidatePerfReceipt {
        #[arg(value_name = "RECEIPT", default_value = "scripts/bench/perf.json")]
        receipt: PathBuf,
    },
    CleanMergeConflicts {
        #[arg(value_name = "PATH")]
        file: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        CommandKind::CheckNoUnwrapExpect => scan_disallowed_panic_calls(),
        CommandKind::CheckPerformanceDocs => check_performance_docs(),
        CommandKind::CheckPublicResultDocs => check_public_result_docs(),
        CommandKind::GuardHotpaths => guard_hotpaths(),
        CommandKind::PerfAnnotateHost => perf_annotate_host(),
        CommandKind::SoakDispatch => soak_dispatch(),
        CommandKind::ValidatePerfReceipt { receipt } => validate_perf_receipt(receipt),
        CommandKind::CleanMergeConflicts { file } => clean_merge_conflicts(file),
    }
}

fn workspace_root() -> Result<PathBuf> {
    let mut current = env::current_dir()?;
    loop {
        if current.join(".git").exists() {
            return Ok(current);
        }
        if !current.pop() {
            break;
        }
    }
    bail!(
        "unable to locate workspace root from {}",
        env::current_dir()?.display()
    )
}

fn has_component(path: &Path, component: &str) -> bool {
    path.components()
        .any(|comp| matches!(comp, Component::Normal(name) if name == OsStr::new(component)))
}

fn is_identifier_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

fn contains_disallowed_call(source: &str, pattern: &str) -> bool {
    let bytes = source.as_bytes();
    let mut start = 0;

    while let Some(offset) = source[start..].find(pattern) {
        let hit = start + offset;
        let preceded_by_identifier = hit > 0 && is_identifier_byte(bytes[hit - 1]);
        if !preceded_by_identifier {
            return true;
        }
        start = hit + pattern.len();
    }

    false
}

fn scan_disallowed_panic_calls() -> Result<()> {
    let root = workspace_root()?;
    let mut fail = false;
    let mut paths = Vec::new();
    collect_rs_paths(&root, &mut paths)?;

    let patterns = [
        (["un", "wrap", "("].concat(), "unwrap"),
        (["ex", "pect", "("].concat(), "expect"),
    ];

    for entry in paths {
        let source = fs::read_to_string(&entry)
            .with_context(|| format!("failed to read {}", entry.display()))?;
        let rel = entry.strip_prefix(&root).unwrap_or(&entry);

        for (pattern, symbol) in &patterns {
            if contains_disallowed_call(&source, pattern) {
                println!("error: disallowed {} usage in {}", symbol, rel.display());
                fail = true;
            }
        }
    }

    if fail {
        bail!("disallowed unwrap/expect usage found");
    }

    Ok(())
}

fn collect_rs_paths(root: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    let mut entries = vec![root.to_path_buf()];

    while let Some(path) = entries.pop() {
        for item in fs::read_dir(&path)? {
            let entry = item?;
            let file_type = entry.file_type()?;
            let entry_path = entry.path();

            if file_type.is_dir() {
                let skip =
                    has_component(&entry_path, "target") || has_component(&entry_path, ".git");
                if !skip {
                    entries.push(entry_path);
                }
                continue;
            }

            if !file_type.is_file() {
                continue;
            }

            if entry_path.extension().and_then(|ext| ext.to_str()) != Some("rs") {
                continue;
            }

            if has_component(&entry_path, "src") {
                out.push(entry_path);
            }
        }
    }

    Ok(())
}

fn guard_hotpaths() -> Result<()> {
    let root = workspace_root()?;
    let lib_api = root
        .join("crates")
        .join("copybook-codec")
        .join("src")
        .join("lib_api.rs");
    let source = fs::read_to_string(&lib_api)
        .with_context(|| format!("failed to read {}", lib_api.display()))?;

    let mut fail = false;

    for (line_no, line) in source.lines().enumerate() {
        if line.contains("Value::String(") && line.contains("to_string(") {
            println!("{}:{}", lib_api.display(), line_no + 1);
            fail = true;
        }
    }

    let lines: Vec<&str> = source.lines().collect();
    let mut seen = HashSet::new();

    for (line_no, line) in lines.iter().enumerate() {
        if !(line.contains("decode_packed_decimal_") || line.contains("decode_zoned_decimal_")) {
            continue;
        }

        let line_start = line_no.saturating_sub(2);
        let line_end = (line_no + 2).min(lines.len() - 1);
        let mut hit = false;
        let mut context = String::new();

        for context_line in &lines[line_start..=line_end] {
            if context_line.contains(".to_string(") {
                hit = true;
            }
            context.push_str(context_line);
            context.push('\n');
        }

        if hit && seen.insert(line_no) {
            fail = true;
            print!("{context}");
            eprintln!(
                "❌ to_string() adjacent to decimal decode in {}",
                lib_api.display()
            );
        }
    }

    if fail {
        bail!("hot-path allocation guard failed");
    }

    println!("✅ Hot-path allocation guard clean");
    Ok(())
}

fn parse_cpu_model() -> String {
    fs::read_to_string("/proc/cpuinfo")
        .ok()
        .and_then(|content| {
            content
                .lines()
                .find_map(|line| line.strip_prefix("model name"))
                .and_then(|line| line.split_once(':').map(|(_, val)| val.trim().to_string()))
        })
        .unwrap_or_else(|| "unknown".to_string())
}

fn parse_wsl2() -> bool {
    fs::read_to_string("/proc/version")
        .ok()
        .is_some_and(|content| content.to_lowercase().contains("microsoft"))
}

fn perf_annotate_host() -> Result<()> {
    let root = workspace_root()?;
    let perf_path = root.join("scripts").join("bench").join("perf.json");

    let mut receipt: Value = serde_json::from_str(
        &fs::read_to_string(&perf_path).context("unable to read scripts/bench/perf.json")?,
    )?;

    let cpu = parse_cpu_model();
    let cores = num_cpus::get();
    let kernel = fs::read_to_string("/proc/sys/kernel/osrelease")
        .ok()
        .map(|k| k.trim().to_string())
        .filter(|k| !k.is_empty())
        .unwrap_or_else(|| "unknown".to_string());
    let os = std::env::consts::OS.to_string();
    let ts = chrono::Utc::now().to_rfc3339_opts(SecondsFormat::Secs, true);
    let wsl2 = parse_wsl2();

    let mut summary = match receipt.get("summary").and_then(Value::as_object).cloned() {
        Some(summary) => summary,
        None => Map::new(),
    };

    summary.insert("host_cpu".into(), cpu.into());
    summary.insert("host_cores".into(), cores.into());
    summary.insert("host_kernel".into(), kernel.into());
    summary.insert("host_os".into(), os.into());
    summary.insert("wsl2_detected".into(), wsl2.into());
    summary.insert("ts".into(), ts.into());

    if let Value::Object(obj) = &mut receipt {
        obj.insert("summary".into(), Value::Object(summary));
    } else {
        bail!("perf receipt root must be a JSON object");
    }

    let serialized = serde_json::to_string(&receipt).context("failed to serialize perf receipt")?;
    let tmp_path = perf_path.with_extension("json.tmp");
    fs::write(&tmp_path, format!("{serialized}\n"))
        .context("unable to write temporary perf receipt")?;
    fs::rename(&tmp_path, &perf_path).context("unable to finalize perf receipt")?;

    println!(
        "Annotated {} with host info (WSL2: {}).",
        perf_path.display(),
        if wsl2 { "true" } else { "false" }
    );
    Ok(())
}

fn soak_dispatch() -> Result<()> {
    let gh = Command::new("gh").arg("--version").status();
    if gh.is_err() || !gh.as_ref().is_ok_and(std::process::ExitStatus::success) {
        bail!("gh CLI not found; install GitHub CLI and authenticate before running.");
    }

    let status = Command::new("gh")
        .args(["workflow", "run", "soak.yml"])
        .status()
        .context("failed to invoke gh workflow run")?;

    if !status.success() {
        bail!("gh workflow run soak.yml failed");
    }

    println!("Triggered soak workflow; check Actions → Soak for artifacts and check-runs.");
    Ok(())
}

fn clean_merge_conflicts(file: PathBuf) -> Result<()> {
    let root = workspace_root()?;
    let target = if file.is_absolute() {
        file
    } else {
        root.join(file)
    };
    let lines = fs::read_to_string(&target)
        .with_context(|| format!("failed to read {}", target.display()))?;

    let mut dropping = false;
    let mut out = String::with_capacity(lines.len());
    let marker = ">>>>>>> fc3ebfd (chore: drop unused import in iterator)";

    for raw in lines.split_inclusive('\n') {
        let line = raw.trim_end_matches('\n').trim_end_matches('\r');
        if line.starts_with("<<<<<<< HEAD") {
            dropping = true;
            continue;
        }

        if dropping {
            if line.starts_with("=======") {
                dropping = false;
            }
            continue;
        }

        if line == marker {
            continue;
        }

        out.push_str(line);
        if raw.ends_with('\n') {
            out.push('\n');
        }
    }

    fs::write(&target, out).with_context(|| format!("failed to write {}", target.display()))?;
    Ok(())
}

fn check_public_result_docs() -> Result<()> {
    let root = workspace_root()?;
    let scan_dirs = [
        root.join("crates").join("copybook-codec").join("src"),
        root.join("crates").join("copybook-core").join("src"),
    ];
    let signature = Regex::new(r"^\s*pub\s+fn\s+\w+.*->\s*Result<")?;
    let inline = Regex::new(r"^\s*#\[\s*inline\s*\]")?;
    let must_use = Regex::new(r"^\s*#\[\s*must_use[^\]]*\]")?;
    let errors_doc = Regex::new(r"^\s*///\s*#\s*Errors")?;

    let mut files = Vec::new();
    for dir in scan_dirs {
        collect_all_rs_paths(&dir, &mut files)?;
    }
    files.sort();

    let mut missing = false;
    for file in files {
        let source = fs::read_to_string(&file)
            .with_context(|| format!("failed to read {}", file.display()))?;
        let lines: Vec<&str> = source.lines().collect();
        for (idx, line) in lines.iter().enumerate() {
            if !signature.is_match(line) {
                continue;
            }
            let start = idx.saturating_sub(4);
            let header = lines[start..idx].join("\n");
            let rel = file.strip_prefix(&root).unwrap_or(&file);
            let line_no = idx + 1;
            if !header.lines().any(|line| inline.is_match(line)) {
                println!("missing #[inline]      @ {}:{line_no}", rel.display());
                missing = true;
            }
            if !header.lines().any(|line| must_use.is_match(line)) {
                println!("missing #[must_use]    @ {}:{line_no}", rel.display());
                missing = true;
            }
            if !header.lines().any(|line| errors_doc.is_match(line)) {
                println!("missing doc '# Errors' @ {}:{line_no}", rel.display());
                missing = true;
            }
        }
    }

    if missing {
        bail!("public Result documentation check failed");
    }
    Ok(())
}

fn collect_all_rs_paths(root: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    if !root.exists() {
        return Ok(());
    }

    let mut entries = vec![root.to_path_buf()];
    while let Some(path) = entries.pop() {
        for item in fs::read_dir(&path)? {
            let entry = item?;
            let file_type = entry.file_type()?;
            let entry_path = entry.path();
            if file_type.is_dir() {
                if !has_component(&entry_path, "target") && !has_component(&entry_path, ".git") {
                    entries.push(entry_path);
                }
            } else if file_type.is_file()
                && entry_path.extension().and_then(|ext| ext.to_str()) == Some("rs")
            {
                out.push(entry_path);
            }
        }
    }
    Ok(())
}

fn check_performance_docs() -> Result<()> {
    let root = workspace_root()?;
    let docs_dir = root.join("docs");
    let perf_number = Regex::new(r"MiB/s|GiB/s|MB/s")?;
    let receipt_hash = Regex::new(r"[a-f0-9]{7,40}")?;
    let historical_claim = Regex::new(r"4\.1 GiB/s|560 MiB/s|GiB/s")?;
    let historical_label = Regex::new(r"OUTDATED|HISTORICAL|ARCHIVED")?;

    println!("🔍 Checking performance documentation compliance...");

    let mut issues = 0usize;
    let mut docs = Vec::new();
    if docs_dir.exists() {
        for item in fs::read_dir(&docs_dir)? {
            let entry = item?;
            let path = entry.path();
            if entry.file_type()?.is_file()
                && path.extension().and_then(|ext| ext.to_str()) == Some("md")
            {
                docs.push(path);
            }
        }
    }
    docs.sort();

    for doc_file in docs {
        let content = fs::read_to_string(&doc_file)
            .with_context(|| format!("failed to read {}", doc_file.display()))?;
        let name = doc_file
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>");
        print!("🔍 Checking: {name}");
        std::io::stdout().flush()?;

        if perf_number.is_match(&content) {
            println!("  ❌ Contains performance numbers");
            if !(content.contains("scripts/bench/perf.json") || receipt_hash.is_match(&content)) {
                println!("  ❌ Does not reference canonical receipts");
                issues += 1;
            } else {
                println!("  ✅ References canonical receipts");
            }
            if historical_claim.is_match(&content) && !historical_label.is_match(&content) {
                println!("  ⚠️  Contains unlabeled historical claims");
                issues += 1;
            }
            if !content.contains("format_version") {
                println!("  ⚠️  Does not use receipt format version");
                issues += 1;
            }
        } else {
            println!("  ✅ No performance compliance issues");
        }
    }

    let required = [
        (
            "docs/PERFORMANCE_GOVERNANCE.md",
            "Performance governance policy",
        ),
        (
            "docs/HISTORICAL_PERFORMANCE.md",
            "Historical performance file",
        ),
        (
            "schemas/perf-receipt-schema.json",
            "Performance receipt schema",
        ),
        (
            "scripts/validate-perf-receipt.sh",
            "Receipt validation script",
        ),
        ("scripts/bench-enhanced.sh", "Enhanced benchmark script"),
        ("scripts/bench/perf.json", "Canonical receipt file"),
    ];

    for (rel, label) in required {
        if root.join(rel).is_file() {
            println!("  ✅ {label} exists");
        } else {
            println!("  ❌ {label} missing");
            issues += 1;
        }
    }

    println!();
    if issues == 0 {
        println!("✅ All performance documentation compliance checks passed");
        Ok(())
    } else {
        bail!("❌ Found {issues} performance documentation compliance issues")
    }
}

fn validate_perf_receipt(receipt: PathBuf) -> Result<()> {
    let root = workspace_root()?;
    let receipt_file = if receipt.is_absolute() {
        receipt
    } else {
        root.join(receipt)
    };
    println!(
        "🔍 Validating performance receipt: {}",
        display_path(&root, &receipt_file).display()
    );

    if !receipt_file.is_file() {
        bail!(
            "❌ Receipt file not found: {}",
            display_path(&root, &receipt_file).display()
        );
    }

    let content = fs::read_to_string(&receipt_file)
        .with_context(|| format!("failed to read {}", receipt_file.display()))?;
    let value: Value = serde_json::from_str(&content).context("❌ Invalid JSON format")?;

    validate_receipt_structure(&value)?;
    validate_format_version(&value)?;
    validate_timestamp(&value)?;
    validate_commit_hash(&value)?;
    validate_performance_values(&value)?;
    validate_receipt_integrity(&value)?;

    println!("✅ All receipt validations passed");
    Ok(())
}

fn display_path<'a>(root: &'a Path, path: &'a Path) -> &'a Path {
    path.strip_prefix(root).unwrap_or(path)
}

fn validate_receipt_structure(value: &Value) -> Result<()> {
    let obj = value
        .as_object()
        .context("receipt root must be a JSON object")?;
    for field in [
        "format_version",
        "timestamp",
        "commit",
        "build_profile",
        "target_cpu",
        "environment",
        "benchmarks",
        "summary",
    ] {
        if !obj.contains_key(field) {
            bail!("❌ Missing required field: {field}");
        }
    }

    let environment = value
        .get("environment")
        .and_then(Value::as_object)
        .context("environment must be a JSON object")?;
    for field in ["os", "kernel", "cpu_model", "cpu_cores", "wsl2_detected"] {
        if !environment.contains_key(field) {
            bail!("❌ Missing required environment field: {field}");
        }
    }

    let benchmarks = value
        .get("benchmarks")
        .and_then(Value::as_array)
        .context("benchmarks must be an array")?;
    if benchmarks.is_empty()
        || !benchmarks.iter().any(|bench| {
            bench.as_object().is_some_and(|bench_obj| {
                ["name", "mean_ns", "bytes_processed", "mean_mibps"]
                    .iter()
                    .all(|field| bench_obj.contains_key(*field))
            })
        })
    {
        bail!("❌ Benchmarks missing required fields");
    }

    let summary = value
        .get("summary")
        .and_then(Value::as_object)
        .context("summary must be a JSON object")?;
    for field in ["display_mibps", "comp3_mibps", "max_rss_mib"] {
        if !summary.contains_key(field) {
            bail!("❌ Missing required summary field: {field}");
        }
    }

    if value.pointer("/integrity/sha256").is_none() {
        bail!("❌ Missing integrity SHA256 hash");
    }

    println!("✅ Receipt structure validation passed");
    Ok(())
}

fn validate_format_version(value: &Value) -> Result<()> {
    let version = value
        .get("format_version")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let semver = Regex::new(r"^[0-9]+\.[0-9]+\.[0-9]+$")?;
    if semver.is_match(version) {
        println!("✅ Format version validation passed: {version}");
        Ok(())
    } else {
        bail!("❌ Invalid format version: {version}")
    }
}

fn validate_timestamp(value: &Value) -> Result<()> {
    let timestamp = value
        .get("timestamp")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let iso8601 = Regex::new(r"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")?;
    if iso8601.is_match(timestamp) {
        println!("✅ Timestamp format validation passed: {timestamp}");
        Ok(())
    } else {
        bail!("❌ Invalid timestamp format: {timestamp}")
    }
}

fn validate_commit_hash(value: &Value) -> Result<()> {
    let commit = value
        .get("commit")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let hash = Regex::new(r"^[a-f0-9]{7,40}$")?;
    if hash.is_match(commit) {
        println!("✅ Commit hash validation passed: {commit}");
        Ok(())
    } else {
        bail!("❌ Invalid commit hash format: {commit}")
    }
}

fn validate_performance_values(value: &Value) -> Result<()> {
    let display_mibps = value
        .pointer("/summary/display_mibps")
        .and_then(Value::as_f64)
        .context("summary.display_mibps must be numeric")?;
    let comp3_mibps = value
        .pointer("/summary/comp3_mibps")
        .and_then(Value::as_f64)
        .context("summary.comp3_mibps must be numeric")?;

    if display_mibps < 0.0 {
        bail!("❌ Invalid DISPLAY throughput: {display_mibps} MiB/s (must be >= 0)");
    }
    if comp3_mibps < 0.0 {
        bail!("❌ Invalid COMP-3 throughput: {comp3_mibps} MiB/s (must be >= 0)");
    }
    if display_mibps > 100_000.0 {
        eprintln!("⚠️  DISPLAY throughput seems unusually high: {display_mibps} MiB/s");
    }
    if comp3_mibps > 10_000.0 {
        eprintln!("⚠️  COMP-3 throughput seems unusually high: {comp3_mibps} MiB/s");
    }

    println!("✅ Performance values validation passed");
    Ok(())
}

fn validate_receipt_integrity(value: &Value) -> Result<()> {
    let stored_hash = value
        .pointer("/integrity/sha256")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let mut without_integrity = value.clone();
    if let Value::Object(obj) = &mut without_integrity {
        obj.shift_remove("integrity");
    }

    let canonical = canonical_json(&without_integrity)?;
    let mut hasher = Sha256::new();
    hasher.update(canonical.as_bytes());
    hasher.update(b"\n");
    let actual_hash = format!("{:x}", hasher.finalize());

    if stored_hash == actual_hash {
        println!("✅ Receipt integrity validation passed");
        Ok(())
    } else {
        eprintln!("❌ Receipt integrity validation failed");
        eprintln!("  Stored hash: {stored_hash}");
        eprintln!("  Actual hash: {actual_hash}");
        eprintln!("  Hint: Receipt may have been modified after hashing");
        bail!("receipt integrity validation failed")
    }
}

fn canonical_json(value: &Value) -> Result<String> {
    let mut output = String::new();
    write_canonical_json(value, &mut output)?;
    Ok(output)
}

fn write_canonical_json(value: &Value, out: &mut String) -> Result<()> {
    match value {
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {
            out.push_str(&serde_json::to_string(value)?);
        }
        Value::Array(values) => {
            out.push('[');
            for (idx, item) in values.iter().enumerate() {
                if idx > 0 {
                    out.push(',');
                }
                write_canonical_json(item, out)?;
            }
            out.push(']');
        }
        Value::Object(map) => {
            let mut keys: Vec<&String> = map.keys().collect();
            keys.sort();
            out.push('{');
            for (idx, key) in keys.iter().enumerate() {
                if idx > 0 {
                    out.push(',');
                }
                out.push_str(&serde_json::to_string(key)?);
                out.push(':');
                if let Some(item) = map.get(*key) {
                    write_canonical_json(item, out)?;
                }
            }
            out.push('}');
        }
    }
    Ok(())
}
