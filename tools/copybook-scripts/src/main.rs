//! SPDX-License-Identifier: AGPL-3.0-or-later
use std::collections::HashSet;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, bail};
use chrono::SecondsFormat;
use clap::{Parser, Subcommand};
use regex::Regex;
use serde_json::{Map, Value};

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
    GuardHotpaths,
    PerfAnnotateHost,
    SoakAggregate,
    SoakDispatch,
    CleanMergeConflicts {
        #[arg(value_name = "PATH")]
        file: PathBuf,
    },
    AdaptReviewAgents,
    FixAgentIssues,
    FinalCleanupAgents,
    CheckPublicResultDocs,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        CommandKind::CheckNoUnwrapExpect => scan_disallowed_panic_calls(),
        CommandKind::GuardHotpaths => guard_hotpaths(),
        CommandKind::PerfAnnotateHost => perf_annotate_host(),
        CommandKind::SoakAggregate => soak_aggregate(),
        CommandKind::SoakDispatch => soak_dispatch(),
        CommandKind::CleanMergeConflicts { file } => clean_merge_conflicts(file),
        CommandKind::AdaptReviewAgents => adapt_review_agents(),
        CommandKind::FixAgentIssues => fix_agent_issues(),
        CommandKind::FinalCleanupAgents => final_cleanup_agents(),
        CommandKind::CheckPublicResultDocs => check_public_result_docs(),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PublicResultDocCheck {
    Inline,
    MustUse,
    ErrorsDoc,
}

impl PublicResultDocCheck {
    fn label(self) -> &'static str {
        match self {
            Self::Inline => "missing #[inline]",
            Self::MustUse => "missing #[must_use]",
            Self::ErrorsDoc => "missing doc '# Errors'",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PublicResultDocFinding {
    check: PublicResultDocCheck,
    path: PathBuf,
    line: usize,
}

fn rust_item_header(lines: &[&str], function_line_index: usize) -> Vec<String> {
    let mut header = Vec::new();
    let mut cursor = function_line_index;

    while cursor > 0 {
        cursor -= 1;
        let line = lines[cursor];
        let trimmed = line.trim_start();

        if trimmed.starts_with("///")
            || trimmed.starts_with("#[")
            || trimmed.is_empty()
            || trimmed.starts_with("//")
        {
            header.push(line.to_string());
            continue;
        }

        break;
    }

    header.reverse();
    header
}

fn missing_public_result_docs_for_source(
    path: &Path,
    source: &str,
    function_start_regex: &Regex,
    result_signature_regex: &Regex,
) -> Vec<PublicResultDocFinding> {
    let lines: Vec<&str> = source.lines().collect();
    let mut findings = Vec::new();

    for (index, line) in lines.iter().enumerate() {
        if !function_start_regex.is_match(line) {
            continue;
        }

        let signature = collect_function_signature(&lines, index);
        if !result_signature_regex.is_match(&signature) {
            continue;
        }

        let header = rust_item_header(&lines, index);
        let line_number = index + 1;

        if !header.iter().any(|line| {
            let trimmed = line.trim_start();
            trimmed.starts_with("#[inline]") || trimmed.starts_with("#[inline(")
        }) {
            findings.push(PublicResultDocFinding {
                check: PublicResultDocCheck::Inline,
                path: path.to_path_buf(),
                line: line_number,
            });
        }

        if !header.iter().any(|line| {
            let trimmed = line.trim_start();
            trimmed.starts_with("#[must_use")
        }) {
            findings.push(PublicResultDocFinding {
                check: PublicResultDocCheck::MustUse,
                path: path.to_path_buf(),
                line: line_number,
            });
        }

        if !header
            .iter()
            .any(|line| line.trim_start().starts_with("/// # Errors"))
        {
            findings.push(PublicResultDocFinding {
                check: PublicResultDocCheck::ErrorsDoc,
                path: path.to_path_buf(),
                line: line_number,
            });
        }
    }

    findings
}

fn collect_function_signature(lines: &[&str], function_line_index: usize) -> String {
    let mut signature = String::new();

    for line in &lines[function_line_index..] {
        if !signature.is_empty() {
            signature.push(' ');
        }
        signature.push_str(line.trim());

        if line.contains('{') || line.contains(';') {
            break;
        }
    }

    signature
}

fn collect_public_result_docs_findings(root: &Path) -> Result<Vec<PublicResultDocFinding>> {
    let function_start_regex = Regex::new(r"^\s*pub\s+fn\s+\w+")?;
    let result_signature_regex =
        Regex::new(r"^\s*pub\s+fn\s+\w+.*->\s*(?:[A-Za-z_]\w*::)*Result<")?;
    let scan_dirs = [
        root.join("crates").join("copybook-codec").join("src"),
        root.join("crates").join("copybook-core").join("src"),
    ];
    let mut findings = Vec::new();

    for scan_dir in scan_dirs {
        let mut paths = Vec::new();
        collect_rs_files(&scan_dir, &mut paths)?;
        paths.sort();

        for path in paths {
            let source = fs::read_to_string(&path)
                .with_context(|| format!("failed to read {}", path.display()))?;
            findings.extend(missing_public_result_docs_for_source(
                &path,
                &source,
                &function_start_regex,
                &result_signature_regex,
            ));
        }
    }

    Ok(findings)
}

fn collect_rs_files(root: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    let mut entries = vec![root.to_path_buf()];

    while let Some(path) = entries.pop() {
        for item in fs::read_dir(&path)? {
            let entry = item?;
            let file_type = entry.file_type()?;
            let entry_path = entry.path();

            if file_type.is_dir() {
                entries.push(entry_path);
                continue;
            }

            if file_type.is_file()
                && entry_path.extension().and_then(|ext| ext.to_str()) == Some("rs")
            {
                out.push(entry_path);
            }
        }
    }

    Ok(())
}

fn check_public_result_docs() -> Result<()> {
    let root = workspace_root()?;
    let findings = collect_public_result_docs_findings(&root)?;

    for finding in &findings {
        let rel = finding.path.strip_prefix(&root).unwrap_or(&finding.path);
        println!(
            "{:<23} @ {}:{}",
            finding.check.label(),
            rel.display(),
            finding.line
        );
    }

    if !findings.is_empty() {
        bail!(
            "public Result function documentation check found {} issue(s)",
            findings.len()
        );
    }

    println!("✅ Public Result function documentation clean");
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

fn value_as_f64(value: &Value) -> Option<f64> {
    value.as_f64().filter(|number| number.is_finite())
}

fn percentile(sorted_samples: &[f64], numerator: usize, denominator: usize) -> Option<f64> {
    if sorted_samples.is_empty() || denominator == 0 {
        return None;
    }

    let count = sorted_samples.len();
    let rank = count.saturating_mul(numerator) / denominator;
    let rank = rank.clamp(1, count);
    sorted_samples.get(rank - 1).copied()
}

fn soak_aggregate() -> Result<()> {
    let root = workspace_root()?;
    let perf_path = root.join("scripts").join("bench").join("perf.json");

    if !perf_path.is_file() {
        println!(
            "No perf receipts found at {}; skipping aggregate.",
            perf_path
                .strip_prefix(&root)
                .unwrap_or(&perf_path)
                .display()
        );
        return Ok(());
    }

    let mut perf = read_json_file(&perf_path)?;

    let Some(benchmarks) = perf.get("benchmarks").and_then(Value::as_array) else {
        println!("No benchmarks found in perf.json; skipping aggregate.");
        return Ok(());
    };

    if benchmarks.is_empty() {
        println!("No benchmarks found in perf.json; skipping aggregate.");
        return Ok(());
    }

    let mut samples = collect_soak_samples(&root, benchmarks)?;

    if samples.is_empty() {
        println!("No samples found; skipping aggregate.");
        return Ok(());
    }

    samples.sort_by(f64::total_cmp);
    let p50 = percentile(&samples, 50, 100).context("failed to calculate p50")?;
    let p90 = percentile(&samples, 90, 100).context("failed to calculate p90")?;
    let p99 = percentile(&samples, 99, 100).context("failed to calculate p99")?;

    update_perf_summary(&mut perf, p50, p90, p99)?;
    write_json_file(&perf_path, &perf)?;

    println!(
        "Added percentiles to {}: p50={p50:.2} p90={p90:.2} p99={p99:.2}",
        perf_path
            .strip_prefix(&root)
            .unwrap_or(&perf_path)
            .display()
    );
    Ok(())
}

fn read_json_file(path: &Path) -> Result<Value> {
    let content =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    serde_json::from_str(&content).with_context(|| format!("failed to parse {}", path.display()))
}

fn write_json_file(path: &Path, value: &Value) -> Result<()> {
    let serialized = serde_json::to_string_pretty(value).context("failed to serialize JSON")?;
    fs::write(path, format!("{serialized}\n"))
        .with_context(|| format!("failed to write {}", path.display()))
}

fn collect_soak_samples(root: &Path, benchmarks: &[Value]) -> Result<Vec<f64>> {
    let mut samples = Vec::new();

    for benchmark in benchmarks {
        let Some(name) = benchmark.get("name").and_then(Value::as_str) else {
            continue;
        };

        collect_benchmark_samples(root, name, &mut samples)?;
    }

    Ok(samples)
}

fn collect_benchmark_samples(root: &Path, name: &str, samples: &mut Vec<f64>) -> Result<()> {
    let bench_root = root.join("target").join("criterion").join(name).join("new");
    let sample_path = bench_root.join("sample.json");
    let benchmark_path = bench_root.join("benchmark.json");

    if !sample_path.is_file() || !benchmark_path.is_file() {
        return Ok(());
    }

    let Some(sample) = read_criterion_json(&sample_path, name)? else {
        return Ok(());
    };
    let Some(benchmark) = read_criterion_json(&benchmark_path, name)? else {
        return Ok(());
    };

    let Some(throughput_bytes) = benchmark
        .get("throughput")
        .and_then(|throughput| throughput.get("Bytes"))
        .and_then(value_as_f64)
    else {
        return Ok(());
    };

    append_mib_per_second_samples(&sample, throughput_bytes, samples);
    Ok(())
}

fn read_criterion_json(path: &Path, benchmark: &str) -> Result<Option<Value>> {
    let content =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;

    match serde_json::from_str(&content) {
        Ok(value) => Ok(Some(value)),
        Err(err) => {
            eprintln!("Failed to parse criterion output for {benchmark}: {err}");
            Ok(None)
        }
    }
}

fn append_mib_per_second_samples(sample: &Value, throughput_bytes: f64, samples: &mut Vec<f64>) {
    let Some(iters) = sample.get("iters").and_then(Value::as_array) else {
        return;
    };
    let Some(times) = sample.get("times").and_then(Value::as_array) else {
        return;
    };

    for (iterations, elapsed_ns) in iters.iter().zip(times) {
        let Some(iterations) = value_as_f64(iterations) else {
            continue;
        };
        let Some(elapsed_ns) = value_as_f64(elapsed_ns) else {
            continue;
        };

        if iterations <= 0.0 || elapsed_ns <= 0.0 {
            continue;
        }

        let seconds = elapsed_ns / 1_000_000_000.0;
        let total_bytes = throughput_bytes * iterations;
        samples.push(total_bytes / seconds / 1_048_576.0);
    }
}

fn update_perf_summary(perf: &mut Value, p50: f64, p90: f64, p99: f64) -> Result<()> {
    let summary = perf
        .as_object_mut()
        .context("perf receipt root must be a JSON object")?
        .entry("summary")
        .or_insert_with(|| Value::Object(Map::new()));

    let summary = summary
        .as_object_mut()
        .context("perf receipt summary must be a JSON object")?;
    summary.insert("p50_mibps".into(), p50.into());
    summary.insert("p90_mibps".into(), p90.into());
    summary.insert("p99_mibps".into(), p99.into());
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

const AGENTS_REVIEW_DIR: &str = ".claude/agents4/review";

#[derive(Clone, Copy)]
enum TextRule {
    Literal(&'static str, &'static str),
    Regex(&'static str, &'static str),
    BitnetCrate,
}

fn bitnet_crate_replacement(hit: &str) -> &str {
    match hit {
        "bitnet-kernels" => "copybook-codec",
        "bitnet-inference" => "copybook-cli",
        "bitnet-wasm" => "copybook-gen",
        "bitnet-tokenizers" => "copybook-bench",
        _ => "copybook-core",
    }
}

fn replace_bitnet_crate_names(content: &str) -> String {
    let mut output = String::with_capacity(content.len());
    let mut cursor = 0usize;

    while let Some(relative_start) = content[cursor..].find("bitnet-") {
        let start = cursor + relative_start;
        output.push_str(&content[cursor..start]);

        let mut end = start + "bitnet-".len();
        for (offset, ch) in content[end..].char_indices() {
            if !ch.is_ascii_alphabetic() {
                break;
            }
            end = start + "bitnet-".len() + offset + ch.len_utf8();
        }

        output.push_str(bitnet_crate_replacement(&content[start..end]));
        cursor = end;
    }

    output.push_str(&content[cursor..]);
    output
}

fn apply_text_rules(mut content: String, rules: &[TextRule]) -> Result<String> {
    for rule in rules {
        match rule {
            TextRule::Literal(from, to) => {
                content = content.replace(from, to);
            }
            TextRule::Regex(pattern, replacement) => {
                let re = Regex::new(pattern)
                    .with_context(|| format!("invalid cleanup regex: {pattern}"))?;
                content = re.replace_all(&content, *replacement).into_owned();
            }
            TextRule::BitnetCrate => {
                content = replace_bitnet_crate_names(&content);
            }
        }
    }

    Ok(content)
}

fn agent_markdown_files(root: &Path) -> Result<Vec<PathBuf>> {
    if !root.exists() {
        bail!("Error: Directory {} does not exist", root.display());
    }

    let mut paths = Vec::new();
    for item in fs::read_dir(root).with_context(|| format!("failed to read {}", root.display()))? {
        let entry = item?;
        let path = entry.path();
        if entry.file_type()?.is_file() && path.extension().and_then(OsStr::to_str) == Some("md") {
            paths.push(path);
        }
    }
    paths.sort();

    if paths.is_empty() {
        bail!("No .md files found in {}", root.display());
    }

    Ok(paths)
}

fn process_agent_files(action: &str, intro: &str, rules: &[TextRule]) -> Result<()> {
    let root = workspace_root()?.join(AGENTS_REVIEW_DIR);
    let agent_files = agent_markdown_files(&root)?;
    println!("Found {} agent files {}", agent_files.len(), intro);

    let mut changed_count = 0usize;
    for path in agent_files {
        let name = path
            .file_name()
            .and_then(OsStr::to_str)
            .unwrap_or("<unknown>");
        println!("{action} {name}...");

        let original = fs::read_to_string(&path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        let updated = apply_text_rules(original.clone(), rules)?;

        if updated == original {
            println!("  - No changes needed for {name}");
            continue;
        }

        fs::write(&path, updated).with_context(|| format!("failed to write {}", path.display()))?;
        changed_count += 1;
        println!("  ✓ Updated {name}");
    }

    println!("\nCompleted! Updated {changed_count} agent files.");
    Ok(())
}

const ADAPT_REVIEW_AGENT_RULES: &[TextRule] = &[
    TextRule::Literal(
        "BitNet.rs neural network inference",
        "copybook-rs enterprise mainframe data processing",
    ),
    TextRule::Regex(
        r"cargo test --workspace --no-default-features --features \w+",
        "cargo test --workspace",
    ),
    TextRule::Regex(
        r"cargo build --release --no-default-features --features \w+",
        "cargo build --workspace --release",
    ),
    TextRule::Regex(
        r"tests: cargo test: (\d+)/(\d+) pass; CPU: (\d+)/(\d+), GPU: (\d+)/(\d+); quarantined: (\d+) \(linked\)",
        "tests: nextest: $1/$2 pass; enterprise validation: $3/$4; quarantined: $7 (linked)",
    ),
    TextRule::Literal("BitNet neural network", "copybook-rs enterprise mainframe"),
    TextRule::Literal("BitNet.rs", "copybook-rs"),
    TextRule::Literal("neural network", "COBOL parsing"),
    TextRule::Literal("quantization", "COBOL parsing"),
    TextRule::Literal("inference", "data conversion"),
    TextRule::Literal("GPU", "enterprise performance"),
    TextRule::Literal("I2S, TL1, TL2", "DISPLAY, COMP, COMP-3"),
    TextRule::Literal("quantization accuracy", "COBOL parsing accuracy"),
    TextRule::Literal("cross-validation", "mainframe compatibility"),
    TextRule::Literal("GGUF", "EBCDIC"),
    TextRule::Literal("tensor", "field"),
    TextRule::Literal("model", "copybook"),
    TextRule::Literal("CUDA", "SIMD"),
    TextRule::Literal(
        ">99% accuracy",
        "enterprise performance targets (DISPLAY ≥ 4.1 GiB/s, COMP-3 ≥ 560 MiB/s)",
    ),
    TextRule::Literal("99.8%", "4.1 GiB/s"),
    TextRule::Literal("99.6%", "560 MiB/s"),
    TextRule::Literal("bitnet-quantization", "copybook-core"),
    TextRule::Literal("bitnet-kernels", "copybook-codec"),
    TextRule::Literal("bitnet-inference", "copybook-cli"),
    TextRule::Literal("bitnet-wasm", "copybook-gen"),
    TextRule::Literal("bitnet-tokenizers", "copybook-bench"),
    TextRule::Literal("--no-default-features --features cpu", "--workspace"),
    TextRule::Literal(
        "--no-default-features --features gpu",
        "--workspace --release",
    ),
    TextRule::Literal("cargo run -p xtask -- crossval", "cargo xtask ci"),
    TextRule::Literal(
        "cargo run -p xtask -- benchmark",
        "cargo bench --package copybook-bench",
    ),
    TextRule::Literal("./scripts/verify-tests.sh", "cargo xtask ci --quick"),
    TextRule::Literal("CUDA unavailable", "xtask unavailable"),
    TextRule::Literal("GPU memory", "parsing memory"),
    TextRule::Literal("C++ reference", "mainframe compatibility"),
    TextRule::Literal("CPU: ok, GPU: ok", "workspace release ok"),
    TextRule::Literal("tokens/sec", "records/sec"),
    TextRule::Literal("I2S: 99.X%", "DISPLAY: X.Y GiB/s"),
    TextRule::Literal("quantization kernels", "COBOL parsing kernels"),
    TextRule::Literal("inference pipeline", "data processing pipeline"),
    TextRule::Literal(
        "1-bit neural networks",
        "enterprise mainframe data processing",
    ),
    TextRule::BitnetCrate,
];

const FIX_AGENT_ISSUE_RULES: &[TextRule] = &[
    TextRule::Regex(r"(?m)^copybook: sonnet$", "model: sonnet"),
    TextRule::Literal("--workspace --workspace", "--workspace"),
    TextRule::Literal("--workspace --release --workspace", "--workspace --release"),
    TextRule::Literal("copybook-core parsing", "copybook-core"),
    TextRule::Literal("copybook-codec parsing", "copybook-codec"),
    TextRule::Literal("deCOBOL parsing", "data conversion"),
    TextRule::Literal(
        "I2S: 4.1 GiB/s, TL1: 560 MiB/s, TL2: 99.7%",
        "DISPLAY: ≥4.1 GiB/s, COMP-3: ≥560 MiB/s",
    ),
    TextRule::Literal("copybook.gguf", "copybook.cpy"),
    TextRule::Literal("copybooks/bitnet/", "examples/"),
    TextRule::Literal("weight deCOBOL parsing", "field layout computation"),
    TextRule::Literal(
        "COBOL parsing/deCOBOL parsing",
        "COBOL parsing/data conversion",
    ),
    TextRule::Literal(
        "COBOL parsing kernels (DISPLAY, COMP, COMP-3)",
        "COBOL parsing engines (lexer, parser, AST)",
    ),
    TextRule::Literal("records/sec", "GiB/s for DISPLAY, MiB/s for COMP-3"),
    TextRule::Literal("BITNET_DETERMINISTIC=1", "deterministic parsing"),
    TextRule::Literal("BITNET_EBCDIC", "COPYBOOK_DATA"),
    TextRule::Regex(r"bitnet-\*", "copybook-*"),
];

const FINAL_CLEANUP_AGENT_RULES: &[TextRule] = &[
    TextRule::Literal(
        "1-bit quantized COBOL parsings",
        "enterprise mainframe data processing",
    ),
    TextRule::Literal(
        "Neural Network Security Testing (NNST)",
        "COBOL Parsing Security Testing",
    ),
    TextRule::Literal("HuggingFace tokens", "mainframe authentication tokens"),
    TextRule::Literal("copybook poisoning attacks", "malicious copybook attacks"),
    TextRule::Literal(
        "copybook-rs workspace crates",
        "copybook-rs 5-crate workspace (core, codec, cli, gen, bench)",
    ),
    TextRule::Literal(
        "cargo clippy --workspace --all-targets --workspace",
        "cargo clippy --workspace --all-targets",
    ),
    TextRule::Literal(
        "--workspace -- -D warnings",
        "-- -D warnings -W clippy::pedantic",
    ),
    TextRule::Literal("COBOL parsing COBOL parsing", "COBOL parsing"),
    TextRule::Literal("enterprise performance/CPU", "high-performance"),
    TextRule::Literal("enterprise performance memory", "memory"),
    TextRule::Literal("SIMD enterprise performance", "SIMD CPU"),
    TextRule::Literal("GiB/s for DISPLAY, MiB/s for COMP-3ond", "records/second"),
    TextRule::Literal(
        "GiB/s for DISPLAY, MiB/s for COMP-3",
        "GiB/s (DISPLAY), MiB/s (COMP-3)",
    ),
    TextRule::Literal(
        "cargo bench --workspace --workspace",
        "cargo bench --package copybook-bench",
    ),
    TextRule::Literal(
        "cargo bench --workspace",
        "cargo bench --package copybook-bench",
    ),
    TextRule::Literal(
        "cargo test --workspace --workspace",
        "cargo test --workspace",
    ),
    TextRule::Literal(
        "I2S ≥4.1 GiB/s, TL1 ≥560 MiB/s, TL2 ≥99.7%",
        "DISPLAY ≥4.1 GiB/s, COMP-3 ≥560 MiB/s",
    ),
    TextRule::Literal("I2S: 4.1 GiB/s", "DISPLAY: 4.1+ GiB/s"),
    TextRule::Literal("TL1: 560 MiB/s", "COMP-3: 560+ MiB/s"),
    TextRule::Literal("copybook weight handling", "copybook field handling"),
    TextRule::Literal("weight data conversion", "field layout computation"),
    TextRule::Literal("Tensor Core acceleration", "SIMD acceleration"),
    TextRule::Literal("mixed precision", "high-precision"),
    TextRule::Literal("--tokens 128", "--batch-size 128"),
    TextRule::Literal(
        "--copybook examples/copybook.cpy --tokens",
        "--input examples/data.bin --copybook examples/schema.cpy --records",
    ),
    TextRule::Literal("Neural Network Validation", "COBOL Parsing Validation"),
    TextRule::Literal("attention computation", "field processing"),
    TextRule::Literal("KV cache", "field cache"),
    TextRule::Regex(
        r"test_dequantize_cpu_and_gpu_paths",
        "enterprise_performance_validation",
    ),
    TextRule::Regex(
        r#"COPYBOOK_DATA="[^"]*""#,
        "COPYBOOK_TEST_DATA=\"examples/test.cpy\"",
    ),
];

fn adapt_review_agents() -> Result<()> {
    process_agent_files("Processing", "to process", ADAPT_REVIEW_AGENT_RULES)
}

fn fix_agent_issues() -> Result<()> {
    process_agent_files("Fixing", "to fix", FIX_AGENT_ISSUE_RULES)
}

fn final_cleanup_agents() -> Result<()> {
    process_agent_files(
        "Final cleanup of",
        "for final cleanup",
        FINAL_CLEANUP_AGENT_RULES,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn apply_rules(input: &str, rules: &[super::TextRule]) -> String {
        match apply_text_rules(input.to_string(), rules) {
            Ok(output) => output,
            Err(error) => panic!("{error}"),
        }
    }

    fn signature_regex() -> Regex {
        match Regex::new(r"^\s*pub\s+fn\s+\w+.*->\s*(?:[A-Za-z_]\w*::)*Result<") {
            Ok(regex) => regex,
            Err(err) => panic!("test regex must compile: {err}"),
        }
    }

    fn function_start_regex() -> Regex {
        match Regex::new(r"^\s*pub\s+fn\s+\w+") {
            Ok(regex) => regex,
            Err(err) => panic!("test regex must compile: {err}"),
        }
    }

    #[test]
    fn public_result_doc_check_accepts_full_rust_item_header() {
        let source = r#"
/// Decode a value.
///
/// # Errors
/// Returns a decode error.
#[inline]
#[allow(clippy::too_many_lines)]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_value() -> Result<()> {
    Ok(())
}
"#;

        let findings = missing_public_result_docs_for_source(
            Path::new("src/lib.rs"),
            source,
            &function_start_regex(),
            &signature_regex(),
        );

        assert!(findings.is_empty());
    }

    #[test]
    fn public_result_doc_check_reports_missing_contract_items() {
        let source = r#"
/// Decode a value.
pub fn decode_value() -> Result<()> {
    Ok(())
}
"#;

        let findings = missing_public_result_docs_for_source(
            Path::new("src/lib.rs"),
            source,
            &function_start_regex(),
            &signature_regex(),
        );
        let checks: Vec<PublicResultDocCheck> =
            findings.iter().map(|finding| finding.check).collect();

        assert_eq!(
            checks,
            vec![
                PublicResultDocCheck::Inline,
                PublicResultDocCheck::MustUse,
                PublicResultDocCheck::ErrorsDoc,
            ]
        );
    }

    #[test]
    fn public_result_doc_check_reports_multiline_result_signatures() {
        let source = r#"
/// Encode a value.
///
/// # Errors
/// Returns an encode error.
#[inline]
pub fn encode_value(
    value: u32,
    buffer: &mut [u8],
) -> std::result::Result<(), Error> {
    Ok(())
}
"#;

        let findings = missing_public_result_docs_for_source(
            Path::new("src/lib.rs"),
            source,
            &function_start_regex(),
            &signature_regex(),
        );

        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].check, PublicResultDocCheck::MustUse);
    }

    #[test]
    fn bitnet_crate_mapper_handles_known_and_default_crates() {
        let input =
            "bitnet-kernels bitnet-inference bitnet-quantization bitnet-tokenizers bitnet-extra";

        assert_eq!(
            replace_bitnet_crate_names(input),
            "copybook-codec copybook-cli copybook-core copybook-bench copybook-core"
        );
    }

    #[test]
    fn adapt_review_rules_rewrite_commands_and_evidence() {
        let input = concat!(
            "BitNet.rs neural network inference\n",
            "cargo test --workspace --no-default-features --features gpu\n",
            "tests: cargo test: 9/10 pass; CPU: 4/5, GPU: 6/7; quarantined: 1 (linked)\n",
        );

        let output = apply_rules(input, ADAPT_REVIEW_AGENT_RULES);

        assert!(output.contains("copybook-rs enterprise mainframe data processing"));
        assert!(output.contains("cargo test --workspace"));
        assert!(output.contains(
            "tests: nextest: 9/10 pass; enterprise validation: 4/5; quarantined: 1 (linked)"
        ));
    }

    #[test]
    fn fix_agent_issue_rules_repair_frontmatter_and_workspace_terms() {
        let input = concat!(
            "---\n",
            "copybook: sonnet\n",
            "---\n",
            "cargo test --workspace --workspace\n",
            "bitnet-* copybook.gguf BITNET_EBCDIC\n",
        );

        let output = apply_rules(input, FIX_AGENT_ISSUE_RULES);

        assert!(output.contains("model: sonnet"));
        assert!(output.contains("cargo test --workspace"));
        assert!(output.contains("copybook-* copybook.cpy COPYBOOK_DATA"));
    }

    #[test]
    fn final_cleanup_rules_rewrite_remaining_agent_terms() {
        let input = concat!(
            "Neural Network Validation uses COPYBOOK_DATA=\"fixtures/demo.cpy\"\n",
            "attention computation and KV cache\n",
            "cargo bench --workspace --workspace\n",
            "cargo bench --workspace\n",
        );

        let output = apply_rules(input, FINAL_CLEANUP_AGENT_RULES);

        assert!(output.contains("COBOL Parsing Validation"));
        assert!(output.contains("COPYBOOK_TEST_DATA=\"examples/test.cpy\""));
        assert!(output.contains("field processing and field cache"));
        assert_eq!(
            output
                .matches("cargo bench --package copybook-bench")
                .count(),
            2
        );
    }

    #[test]
    fn percentile_matches_legacy_rank_selection() {
        let samples = [10.0, 20.0, 30.0, 40.0, 50.0];

        assert_eq!(percentile(&samples, 50, 100), Some(20.0));
        assert_eq!(percentile(&samples, 90, 100), Some(40.0));
        assert_eq!(percentile(&samples, 99, 100), Some(40.0));
    }

    #[test]
    fn percentile_clamps_rank_to_sample_bounds() {
        let samples = [42.0];

        assert_eq!(percentile(&samples, 0, 100), Some(42.0));
        assert_eq!(percentile(&samples, 2, 1), Some(42.0));
        assert_eq!(percentile(&[], 50, 100), None);
        assert_eq!(percentile(&samples, 1, 0), None);
    }
}
