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
use regex::{Captures, Regex};
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
    SoakDispatch,
    CleanMergeConflicts {
        #[arg(value_name = "PATH")]
        file: PathBuf,
    },
    AdaptReviewAgents {
        #[arg(long, default_value = ".claude/agents4/review", value_name = "DIR")]
        agents_dir: PathBuf,
    },
    FixAgentIssues {
        #[arg(long, default_value = ".claude/agents4/review", value_name = "DIR")]
        agents_dir: PathBuf,
    },
    FinalCleanupAgents {
        #[arg(long, default_value = ".claude/agents4/review", value_name = "DIR")]
        agents_dir: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        CommandKind::CheckNoUnwrapExpect => scan_disallowed_panic_calls(),
        CommandKind::GuardHotpaths => guard_hotpaths(),
        CommandKind::PerfAnnotateHost => perf_annotate_host(),
        CommandKind::SoakDispatch => soak_dispatch(),
        CommandKind::CleanMergeConflicts { file } => clean_merge_conflicts(file),
        CommandKind::AdaptReviewAgents { agents_dir } => adapt_review_agents(agents_dir),
        CommandKind::FixAgentIssues { agents_dir } => fix_agent_issues(agents_dir),
        CommandKind::FinalCleanupAgents { agents_dir } => final_cleanup_agents(agents_dir),
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

#[derive(Debug, Clone, Copy)]
enum AgentTransform {
    Adapt,
    Fix,
    FinalCleanup,
}

#[derive(Debug, Clone, Copy)]
enum ReplacementKind {
    Literal,
    Regex,
}

#[derive(Debug, Clone, Copy)]
struct ReplacementRule {
    kind: ReplacementKind,
    from: &'static str,
    to: &'static str,
}

fn adapt_review_agents(agents_dir: PathBuf) -> Result<()> {
    process_agent_files(agents_dir, AgentTransform::Adapt)
}

fn fix_agent_issues(agents_dir: PathBuf) -> Result<()> {
    process_agent_files(agents_dir, AgentTransform::Fix)
}

fn final_cleanup_agents(agents_dir: PathBuf) -> Result<()> {
    process_agent_files(agents_dir, AgentTransform::FinalCleanup)
}

fn process_agent_files(agents_dir: PathBuf, transform: AgentTransform) -> Result<()> {
    let root = workspace_root()?;
    let dir = if agents_dir.is_absolute() {
        agents_dir
    } else {
        root.join(agents_dir)
    };

    if !dir.exists() {
        bail!("Error: Directory {} does not exist", dir.display());
    }

    let mut agent_files = Vec::new();
    for entry in fs::read_dir(&dir).with_context(|| format!("failed to read {}", dir.display()))? {
        let entry = entry?;
        let path = entry.path();
        if entry.file_type()?.is_file()
            && path.extension().and_then(|ext| ext.to_str()) == Some("md")
        {
            agent_files.push(path);
        }
    }
    agent_files.sort();

    if agent_files.is_empty() {
        bail!("No .md files found in {}", dir.display());
    }

    println!("Found {} agent files to process", agent_files.len());

    let mut changed_count = 0usize;
    for file in &agent_files {
        if apply_agent_transform(file, transform)? {
            changed_count += 1;
        }
    }

    let action = match transform {
        AgentTransform::Adapt => "Updated",
        AgentTransform::Fix => "Fixed",
        AgentTransform::FinalCleanup => "Cleaned up",
    };
    println!(
        "\nCompleted! {action} {changed_count} of {} agent files.",
        agent_files.len()
    );
    Ok(())
}

fn apply_agent_transform(path: &Path, transform: AgentTransform) -> Result<bool> {
    let name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("<unknown>");
    let verb = match transform {
        AgentTransform::Adapt => "Processing",
        AgentTransform::Fix => "Fixing",
        AgentTransform::FinalCleanup => "Final cleanup of",
    };
    println!("{verb} {name}...");

    let original =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    let mut content = original.clone();

    match transform {
        AgentTransform::Adapt => apply_adapt_review_agent_rules(&mut content)?,
        AgentTransform::Fix => apply_rules(&mut content, FIX_AGENT_RULES)?,
        AgentTransform::FinalCleanup => apply_rules(&mut content, FINAL_CLEANUP_AGENT_RULES)?,
    }

    if content == original {
        println!("  - No changes needed for {name}");
        return Ok(false);
    }

    fs::write(path, content).with_context(|| format!("failed to write {}", path.display()))?;
    let action = match transform {
        AgentTransform::Adapt => "Updated",
        AgentTransform::Fix => "Fixed",
        AgentTransform::FinalCleanup => "Cleaned up",
    };
    println!("  ✓ {action} {name}");
    Ok(true)
}

fn apply_adapt_review_agent_rules(content: &mut String) -> Result<()> {
    apply_rules(content, ADAPT_AGENT_REGEX_RULES)?;
    apply_rules(content, ADAPT_AGENT_LITERAL_RULES)?;

    replace_regex_with(content, r"bitnet-[a-zA-Z]+", |captures: &Captures<'_>| {
        let matched = captures.get(0).map(|m| m.as_str()).unwrap_or_default();
        match matched {
            "bitnet-quantization" => "copybook-core".to_string(),
            "bitnet-kernels" => "copybook-codec".to_string(),
            "bitnet-inference" => "copybook-cli".to_string(),
            "bitnet-wasm" => "copybook-gen".to_string(),
            "bitnet-tokenizers" => "copybook-bench".to_string(),
            _ => "copybook-core".to_string(),
        }
    })?;

    Ok(())
}

fn apply_rules(content: &mut String, rules: &[ReplacementRule]) -> Result<()> {
    for rule in rules {
        match rule.kind {
            ReplacementKind::Literal => {
                *content = content.replace(rule.from, rule.to);
            }
            ReplacementKind::Regex => {
                let regex = Regex::new(rule.from)
                    .with_context(|| format!("invalid regex replacement pattern: {}", rule.from))?;
                *content = regex.replace_all(content, rule.to).into_owned();
            }
        }
    }
    Ok(())
}

fn replace_regex_with<F>(content: &mut String, pattern: &str, replacer: F) -> Result<()>
where
    F: Fn(&Captures<'_>) -> String,
{
    let regex = Regex::new(pattern)
        .with_context(|| format!("invalid regex replacement pattern: {pattern}"))?;
    *content = regex.replace_all(content, replacer).into_owned();
    Ok(())
}

const fn literal(from: &'static str, to: &'static str) -> ReplacementRule {
    ReplacementRule {
        kind: ReplacementKind::Literal,
        from,
        to,
    }
}

const fn regex_rule(from: &'static str, to: &'static str) -> ReplacementRule {
    ReplacementRule {
        kind: ReplacementKind::Regex,
        from,
        to,
    }
}

const ADAPT_AGENT_LITERAL_RULES: &[ReplacementRule] = &[
    literal(
        "BitNet.rs neural network inference",
        "copybook-rs enterprise mainframe data processing",
    ),
    literal("BitNet neural network", "copybook-rs enterprise mainframe"),
    literal("BitNet.rs", "copybook-rs"),
    literal("neural network", "COBOL parsing"),
    literal("quantization", "COBOL parsing"),
    literal("inference", "data conversion"),
    literal("GPU", "enterprise performance"),
    literal("I2S, TL1, TL2", "DISPLAY, COMP, COMP-3"),
    literal("quantization accuracy", "COBOL parsing accuracy"),
    literal("cross-validation", "mainframe compatibility"),
    literal("GGUF", "EBCDIC"),
    literal("tensor", "field"),
    literal("model", "copybook"),
    literal("CUDA", "SIMD"),
    literal(
        ">99% accuracy",
        "enterprise performance targets (DISPLAY ≥ 4.1 GiB/s, COMP-3 ≥ 560 MiB/s)",
    ),
    literal("99.8%", "4.1 GiB/s"),
    literal("99.6%", "560 MiB/s"),
    literal("bitnet-quantization", "copybook-core"),
    literal("bitnet-kernels", "copybook-codec"),
    literal("bitnet-inference", "copybook-cli"),
    literal("bitnet-wasm", "copybook-gen"),
    literal("bitnet-tokenizers", "copybook-bench"),
    literal("--no-default-features --features cpu", "--workspace"),
    literal(
        "--no-default-features --features gpu",
        "--workspace --release",
    ),
    literal("cargo run -p xtask -- crossval", "cargo xtask ci"),
    literal(
        "cargo run -p xtask -- benchmark",
        "cargo bench --package copybook-bench",
    ),
    literal("./scripts/verify-tests.sh", "cargo xtask ci --quick"),
    literal("CUDA unavailable", "xtask unavailable"),
    literal("GPU memory", "parsing memory"),
    literal("C++ reference", "mainframe compatibility"),
    literal("CPU: ok, GPU: ok", "workspace release ok"),
    literal("tokens/sec", "records/sec"),
    literal("I2S: 99.X%", "DISPLAY: X.Y GiB/s"),
    literal("quantization kernels", "COBOL parsing kernels"),
    literal("inference pipeline", "data processing pipeline"),
    literal(
        "1-bit neural networks",
        "enterprise mainframe data processing",
    ),
];

const ADAPT_AGENT_REGEX_RULES: &[ReplacementRule] = &[
    regex_rule(
        r"cargo test --workspace --no-default-features --features \w+",
        "cargo test --workspace",
    ),
    regex_rule(
        r"cargo build --release --no-default-features --features \w+",
        "cargo build --workspace --release",
    ),
    regex_rule(
        r"tests: cargo test: (\d+)/(\d+) pass; CPU: (\d+)/(\d+), GPU: (\d+)/(\d+); quarantined: (\d+) \(linked\)",
        "tests: nextest: $1/$2 pass; enterprise validation: $3/$4; quarantined: $7 (linked)",
    ),
];

const FIX_AGENT_RULES: &[ReplacementRule] = &[
    regex_rule(r"(?m)^copybook: sonnet$", "model: sonnet"),
    literal("--workspace --workspace", "--workspace"),
    literal("--workspace --release --workspace", "--workspace --release"),
    literal("copybook-core parsing", "copybook-core"),
    literal("copybook-codec parsing", "copybook-codec"),
    literal("deCOBOL parsing", "data conversion"),
    literal(
        "I2S: 4.1 GiB/s, TL1: 560 MiB/s, TL2: 99.7%",
        "DISPLAY: ≥4.1 GiB/s, COMP-3: ≥560 MiB/s",
    ),
    literal("copybook.gguf", "copybook.cpy"),
    literal("copybooks/bitnet/", "examples/"),
    literal("weight deCOBOL parsing", "field layout computation"),
    literal(
        "COBOL parsing/deCOBOL parsing",
        "COBOL parsing/data conversion",
    ),
    literal(
        "COBOL parsing kernels (DISPLAY, COMP, COMP-3)",
        "COBOL parsing engines (lexer, parser, AST)",
    ),
    literal("records/sec", "GiB/s for DISPLAY, MiB/s for COMP-3"),
    literal("BITNET_DETERMINISTIC=1", "deterministic parsing"),
    literal("BITNET_EBCDIC", "COPYBOOK_DATA"),
    regex_rule(r"bitnet-\*", "copybook-*"),
];

const FINAL_CLEANUP_AGENT_RULES: &[ReplacementRule] = &[
    literal(
        "1-bit quantized COBOL parsings",
        "enterprise mainframe data processing",
    ),
    literal(
        "Neural Network Security Testing (NNST)",
        "COBOL Parsing Security Testing",
    ),
    literal("HuggingFace tokens", "mainframe authentication tokens"),
    literal("copybook poisoning attacks", "malicious copybook attacks"),
    literal(
        "copybook-rs workspace crates",
        "copybook-rs 5-crate workspace (core, codec, cli, gen, bench)",
    ),
    literal(
        "cargo clippy --workspace --all-targets --workspace",
        "cargo clippy --workspace --all-targets",
    ),
    literal(
        "--workspace -- -D warnings",
        "-- -D warnings -W clippy::pedantic",
    ),
    literal("COBOL parsing COBOL parsing", "COBOL parsing"),
    literal("enterprise performance/CPU", "high-performance"),
    literal("enterprise performance memory", "memory"),
    literal("SIMD enterprise performance", "SIMD CPU"),
    literal("GiB/s for DISPLAY, MiB/s for COMP-3ond", "records/second"),
    literal(
        "GiB/s for DISPLAY, MiB/s for COMP-3",
        "GiB/s (DISPLAY), MiB/s (COMP-3)",
    ),
    literal(
        "cargo bench --workspace --workspace",
        "cargo bench --package copybook-bench",
    ),
    literal(
        "cargo test --workspace --workspace",
        "cargo test --workspace",
    ),
    literal(
        "I2S ≥4.1 GiB/s, TL1 ≥560 MiB/s, TL2 ≥99.7%",
        "DISPLAY ≥4.1 GiB/s, COMP-3 ≥560 MiB/s",
    ),
    literal("I2S: 4.1 GiB/s", "DISPLAY: 4.1+ GiB/s"),
    literal("TL1: 560 MiB/s", "COMP-3: 560+ MiB/s"),
    literal("copybook weight handling", "copybook field handling"),
    literal("weight data conversion", "field layout computation"),
    literal("Tensor Core acceleration", "SIMD acceleration"),
    literal("mixed precision", "high-precision"),
    literal("--tokens 128", "--batch-size 128"),
    literal(
        "--copybook examples/copybook.cpy --tokens",
        "--input examples/data.bin --copybook examples/schema.cpy --records",
    ),
    literal("Neural Network Validation", "COBOL Parsing Validation"),
    literal("attention computation", "field processing"),
    literal("KV cache", "field cache"),
    regex_rule(
        r"test_dequantize_cpu_and_gpu_paths",
        "enterprise_performance_validation",
    ),
    regex_rule(
        r#"COPYBOOK_DATA="[^"]*""#,
        "COPYBOOK_TEST_DATA=\"examples/test.cpy\"",
    ),
];
