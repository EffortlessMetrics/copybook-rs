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
    SoakDispatch,
    AdaptReviewAgents {
        #[arg(long, value_name = "DIR", default_value = ".claude/agents4/review")]
        agents_dir: PathBuf,
    },
    FixAgentIssues {
        #[arg(long, value_name = "DIR", default_value = ".claude/agents4/review")]
        agents_dir: PathBuf,
    },
    FinalCleanupAgents {
        #[arg(long, value_name = "DIR", default_value = ".claude/agents4/review")]
        agents_dir: PathBuf,
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
        CommandKind::GuardHotpaths => guard_hotpaths(),
        CommandKind::PerfAnnotateHost => perf_annotate_host(),
        CommandKind::SoakDispatch => soak_dispatch(),
        CommandKind::AdaptReviewAgents { agents_dir } => {
            process_agents(agents_dir, AgentOperation::Adapt)
        }
        CommandKind::FixAgentIssues { agents_dir } => {
            process_agents(agents_dir, AgentOperation::Fix)
        }
        CommandKind::FinalCleanupAgents { agents_dir } => {
            process_agents(agents_dir, AgentOperation::FinalCleanup)
        }
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

#[derive(Debug, Clone, Copy)]
enum AgentOperation {
    Adapt,
    Fix,
    FinalCleanup,
}

impl AgentOperation {
    fn gerund(self) -> &'static str {
        match self {
            AgentOperation::Adapt => "Processing",
            AgentOperation::Fix => "Fixing",
            AgentOperation::FinalCleanup => "Final cleanup of",
        }
    }

    fn summary_action(self) -> &'static str {
        match self {
            AgentOperation::Adapt => "Updated",
            AgentOperation::Fix => "Fixed",
            AgentOperation::FinalCleanup => "Final cleanup applied to",
        }
    }

    fn unchanged_message(self) -> &'static str {
        match self {
            AgentOperation::Adapt => "No changes needed for",
            AgentOperation::Fix => "No fixes needed for",
            AgentOperation::FinalCleanup => "No cleanup needed for",
        }
    }

    fn changed_message(self) -> &'static str {
        match self {
            AgentOperation::Adapt => "Updated",
            AgentOperation::Fix => "Fixed",
            AgentOperation::FinalCleanup => "Cleaned up",
        }
    }
}

fn resolve_repo_path(path: PathBuf) -> Result<PathBuf> {
    if path.is_absolute() {
        Ok(path)
    } else {
        Ok(workspace_root()?.join(path))
    }
}

fn replace_literal(content: &mut String, from: &str, to: &str) {
    if content.contains(from) {
        *content = content.replace(from, to);
    }
}

fn replace_regex(content: &mut String, pattern: &str, replacement: &str) -> Result<()> {
    let regex = Regex::new(pattern).with_context(|| format!("invalid regex pattern {pattern}"))?;
    if regex.is_match(content) {
        *content = regex.replace_all(content, replacement).into_owned();
    }
    Ok(())
}

fn adapt_review_agents_content(content: &mut String) -> Result<()> {
    for (from, to) in [
        (
            "BitNet.rs neural network inference",
            "copybook-rs enterprise mainframe data processing",
        ),
        ("BitNet neural network", "copybook-rs enterprise mainframe"),
        ("BitNet.rs", "copybook-rs"),
        ("neural network", "COBOL parsing"),
        ("quantization", "COBOL parsing"),
        ("inference", "data conversion"),
        ("GPU", "enterprise performance"),
        ("I2S, TL1, TL2", "DISPLAY, COMP, COMP-3"),
        ("quantization accuracy", "COBOL parsing accuracy"),
        ("cross-validation", "mainframe compatibility"),
        ("GGUF", "EBCDIC"),
        ("tensor", "field"),
        ("model", "copybook"),
        ("CUDA", "SIMD"),
        (
            ">99% accuracy",
            "enterprise performance targets (DISPLAY ≥ 4.1 GiB/s, COMP-3 ≥ 560 MiB/s)",
        ),
        ("99.8%", "4.1 GiB/s"),
        ("99.6%", "560 MiB/s"),
        ("bitnet-quantization", "copybook-core"),
        ("bitnet-kernels", "copybook-codec"),
        ("bitnet-inference", "copybook-cli"),
        ("bitnet-wasm", "copybook-gen"),
        ("bitnet-tokenizers", "copybook-bench"),
        ("--no-default-features --features cpu", "--workspace"),
        (
            "--no-default-features --features gpu",
            "--workspace --release",
        ),
        ("cargo run -p xtask -- crossval", "cargo xtask ci"),
        (
            "cargo run -p xtask -- benchmark",
            "cargo bench --package copybook-bench",
        ),
        ("./scripts/verify-tests.sh", "cargo xtask ci --quick"),
        ("CUDA unavailable", "xtask unavailable"),
        ("GPU memory", "parsing memory"),
        ("C++ reference", "mainframe compatibility"),
        ("CPU: ok, GPU: ok", "workspace release ok"),
        ("tokens/sec", "records/sec"),
        ("I2S: 99.X%", "DISPLAY: X.Y GiB/s"),
        ("quantization kernels", "COBOL parsing kernels"),
        ("inference pipeline", "data processing pipeline"),
        (
            "1-bit neural networks",
            "enterprise mainframe data processing",
        ),
    ] {
        replace_literal(content, from, to);
    }

    let crate_regex = Regex::new(r"bitnet-[a-zA-Z]+")?;
    *content = crate_regex
        .replace_all(content, |captures: &regex::Captures<'_>| {
            match &captures[0] {
                "bitnet-quantization" => "copybook-core",
                "bitnet-kernels" => "copybook-codec",
                "bitnet-inference" => "copybook-cli",
                "bitnet-wasm" => "copybook-gen",
                "bitnet-tokenizers" => "copybook-bench",
                _ => "copybook-core",
            }
        })
        .into_owned();

    replace_regex(
        content,
        r"cargo test --workspace --no-default-features --features \w+",
        "cargo test --workspace",
    )?;
    replace_regex(
        content,
        r"cargo build --release --no-default-features --features \w+",
        "cargo build --workspace --release",
    )?;
    replace_regex(
        content,
        r"tests: cargo test: (\d+)/(\d+) pass; CPU: (\d+)/(\d+), GPU: (\d+)/(\d+); quarantined: (\d+) \(linked\)",
        "tests: nextest: $1/$2 pass; enterprise validation: $3/$4; quarantined: $7 (linked)",
    )?;

    Ok(())
}

fn fix_agent_issues_content(content: &mut String) -> Result<()> {
    replace_regex(content, r"(?m)^copybook: sonnet$", "model: sonnet")?;

    for (from, to) in [
        ("--workspace --workspace", "--workspace"),
        ("--workspace --release --workspace", "--workspace --release"),
        ("copybook-core parsing", "copybook-core"),
        ("copybook-codec parsing", "copybook-codec"),
        ("deCOBOL parsing", "data conversion"),
        (
            "I2S: 4.1 GiB/s, TL1: 560 MiB/s, TL2: 99.7%",
            "DISPLAY: ≥4.1 GiB/s, COMP-3: ≥560 MiB/s",
        ),
        ("copybook.gguf", "copybook.cpy"),
        ("copybooks/bitnet/", "examples/"),
        ("weight deCOBOL parsing", "field layout computation"),
        (
            "COBOL parsing/deCOBOL parsing",
            "COBOL parsing/data conversion",
        ),
        (
            "COBOL parsing kernels (DISPLAY, COMP, COMP-3)",
            "COBOL parsing engines (lexer, parser, AST)",
        ),
        ("records/sec", "GiB/s for DISPLAY, MiB/s for COMP-3"),
        ("BITNET_DETERMINISTIC=1", "deterministic parsing"),
        ("BITNET_EBCDIC", "COPYBOOK_DATA"),
    ] {
        replace_literal(content, from, to);
    }

    replace_regex(content, r"bitnet-\*", "copybook-*")?;
    Ok(())
}

fn final_cleanup_agents_content(content: &mut String) -> Result<()> {
    for (from, to) in [
        (
            "1-bit quantized COBOL parsings",
            "enterprise mainframe data processing",
        ),
        (
            "Neural Network Security Testing (NNST)",
            "COBOL Parsing Security Testing",
        ),
        ("HuggingFace tokens", "mainframe authentication tokens"),
        ("copybook poisoning attacks", "malicious copybook attacks"),
        (
            "copybook-rs workspace crates",
            "copybook-rs 5-crate workspace (core, codec, cli, gen, bench)",
        ),
        (
            "cargo clippy --all-targets",
            "cargo clippy --workspace --all-targets",
        ),
        ("copybook model", "copybook schema"),
        ("copybook weights", "copybook fields"),
        ("tokenization", "field parsing"),
        ("tokenizer", "parser"),
        ("tokens", "records"),
        ("VRAM", "memory"),
        ("SIMD enterprise performance", "SIMD CPU"),
        ("GiB/s for DISPLAY, MiB/s for COMP-3ond", "records/second"),
        (
            "GiB/s for DISPLAY, MiB/s for COMP-3",
            "GiB/s (DISPLAY), MiB/s (COMP-3)",
        ),
        (
            "cargo bench --workspace --workspace",
            "cargo bench --package copybook-bench",
        ),
        (
            "cargo test --workspace --workspace",
            "cargo test --workspace",
        ),
        (
            "I2S ≥4.1 GiB/s, TL1 ≥560 MiB/s, TL2 ≥99.7%",
            "DISPLAY ≥4.1 GiB/s, COMP-3 ≥560 MiB/s",
        ),
        ("I2S: 4.1 GiB/s", "DISPLAY: 4.1+ GiB/s"),
        ("TL1: 560 MiB/s", "COMP-3: 560+ MiB/s"),
        ("copybook weight handling", "copybook field handling"),
        ("weight data conversion", "field layout computation"),
        ("Tensor Core acceleration", "SIMD acceleration"),
        ("mixed precision", "high-precision"),
        ("--tokens 128", "--batch-size 128"),
        (
            "--copybook examples/copybook.cpy --tokens",
            "--input examples/data.bin --copybook examples/schema.cpy --records",
        ),
        ("Neural Network Validation", "COBOL Parsing Validation"),
        ("attention computation", "field processing"),
        ("KV cache", "field cache"),
    ] {
        replace_literal(content, from, to);
    }

    replace_regex(
        content,
        r"test_dequantize_cpu_and_gpu_paths",
        "enterprise_performance_validation",
    )?;
    replace_regex(
        content,
        r#"COPYBOOK_DATA="[^"]*""#,
        r#"COPYBOOK_TEST_DATA="examples/test.cpy""#,
    )?;
    Ok(())
}

fn apply_agent_operation(content: &mut String, operation: AgentOperation) -> Result<()> {
    match operation {
        AgentOperation::Adapt => adapt_review_agents_content(content),
        AgentOperation::Fix => fix_agent_issues_content(content),
        AgentOperation::FinalCleanup => final_cleanup_agents_content(content),
    }
}

fn process_agents(agents_dir: PathBuf, operation: AgentOperation) -> Result<()> {
    let dir = resolve_repo_path(agents_dir)?;
    if !dir.exists() {
        bail!("Error: Directory {} does not exist", dir.display());
    }

    let mut agent_files = Vec::new();
    for item in fs::read_dir(&dir).with_context(|| format!("failed to read {}", dir.display()))? {
        let entry = item?;
        let path = entry.path();
        if entry.file_type()?.is_file()
            && path.extension().and_then(|ext| ext.to_str()) == Some("md")
        {
            agent_files.push(path);
        }
    }
    agent_files.sort();

    if agent_files.is_empty() {
        println!("No .md files found in {}", dir.display());
        return Ok(());
    }

    println!("Found {} agent files to process", agent_files.len());
    let mut changed_count = 0usize;

    for file in &agent_files {
        let name = file
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>");
        println!("{} {name}...", operation.gerund());
        let original = fs::read_to_string(file)
            .with_context(|| format!("failed to read {}", file.display()))?;
        let mut content = original.clone();
        apply_agent_operation(&mut content, operation)?;
        if content == original {
            println!("  - {} {name}", operation.unchanged_message());
            continue;
        }

        fs::write(file, content).with_context(|| format!("failed to write {}", file.display()))?;
        println!("  ✓ {} {name}", operation.changed_message());
        changed_count += 1;
    }

    println!(
        "\nCompleted! {} {} of {} agent files.",
        operation.summary_action(),
        changed_count,
        agent_files.len()
    );
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
