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
    AdaptReviewAgents,
    FixAgentIssues,
    FinalCleanupAgents,
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
        CommandKind::AdaptReviewAgents => adapt_review_agents(),
        CommandKind::FixAgentIssues => fix_agent_issues(),
        CommandKind::FinalCleanupAgents => final_cleanup_agents(),
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

const AGENTS_REVIEW_DIR: &str = ".claude/agents4/review";

fn review_agent_files(root: &Path) -> Result<Vec<PathBuf>> {
    let agents_dir = root.join(AGENTS_REVIEW_DIR);
    if !agents_dir.exists() {
        bail!("Error: Directory {} does not exist", agents_dir.display());
    }

    let mut agent_files = Vec::new();
    for entry in fs::read_dir(&agents_dir)
        .with_context(|| format!("failed to read {}", agents_dir.display()))?
    {
        let path = entry?.path();
        if path.extension().and_then(|ext| ext.to_str()) == Some("md") {
            agent_files.push(path);
        }
    }
    agent_files.sort();

    if agent_files.is_empty() {
        bail!("No .md files found in {}", agents_dir.display());
    }

    Ok(agent_files)
}

fn replace_all(mut content: String, replacements: &[(&str, &str)]) -> String {
    for (old, new) in replacements {
        content = content.replace(old, new);
    }
    content
}

fn process_agent_file(
    filepath: &Path,
    action: &str,
    changed_action: &str,
    unchanged_action: &str,
    transform: impl FnOnce(String) -> Result<String>,
) -> Result<bool> {
    let filename = filepath
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("<unknown>");
    println!("{action} {filename}...");

    let original_content = fs::read_to_string(filepath)
        .with_context(|| format!("failed to read {}", filepath.display()))?;
    let content = transform(original_content.clone())?;

    if content != original_content {
        fs::write(filepath, content)
            .with_context(|| format!("failed to write {}", filepath.display()))?;
        println!("  ✓ {changed_action} {filename}");
        Ok(true)
    } else {
        println!("  - No {unchanged_action} needed for {filename}");
        Ok(false)
    }
}

fn adapt_review_agents() -> Result<()> {
    let root = workspace_root()?;
    let agent_files = review_agent_files(&root)?;

    println!("Found {} agent files to process", agent_files.len());

    let replacements = [
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
    ];

    let bitnet_crate = Regex::new(r"bitnet-[a-zA-Z]+")?;
    let cargo_test_features =
        Regex::new(r"cargo test --workspace --no-default-features --features \w+")?;
    let cargo_build_features =
        Regex::new(r"cargo build --release --no-default-features --features \w+")?;
    let evidence = Regex::new(
        r"tests: cargo test: (\d+)/(\d+) pass; CPU: (\d+)/(\d+), GPU: (\d+)/(\d+); quarantined: (\d+) \(linked\)",
    )?;

    let mut updated_count = 0;
    for agent_file in &agent_files {
        let changed = process_agent_file(
            agent_file,
            "Processing",
            "Updated",
            "changes",
            |content| {
                let mut content = replace_all(content, &replacements);
                content = bitnet_crate
                    .replace_all(&content, |caps: &regex::Captures<'_>| match &caps[0] {
                        "bitnet-quantization" => "copybook-core",
                        "bitnet-kernels" => "copybook-codec",
                        "bitnet-inference" => "copybook-cli",
                        "bitnet-wasm" => "copybook-gen",
                        "bitnet-tokenizers" => "copybook-bench",
                        _ => "copybook-core",
                    })
                    .into_owned();
                content = cargo_test_features
                    .replace_all(&content, "cargo test --workspace")
                    .into_owned();
                content = cargo_build_features
                    .replace_all(&content, "cargo build --workspace --release")
                    .into_owned();
                content = evidence
                    .replace_all(
                        &content,
                        "tests: nextest: $1/$2 pass; enterprise validation: $3/$4; quarantined: $7 (linked)",
                    )
                    .into_owned();
                Ok(content)
            },
        )?;
        if changed {
            updated_count += 1;
        }
    }

    println!(
        "\nCompleted! Updated {updated_count} of {} agent files.",
        agent_files.len()
    );
    Ok(())
}

fn fix_agent_issues() -> Result<()> {
    let root = workspace_root()?;
    let agent_files = review_agent_files(&root)?;

    println!("Found {} agent files to fix", agent_files.len());

    let replacements = [
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
    ];
    let frontmatter_model = Regex::new(r"(?m)^copybook: sonnet$")?;
    let bitnet_wildcard = Regex::new(r"bitnet-\*")?;

    let mut fixed_count = 0;
    for agent_file in &agent_files {
        let changed = process_agent_file(agent_file, "Fixing", "Fixed", "fixes", |content| {
            let content = frontmatter_model
                .replace_all(&content, "model: sonnet")
                .into_owned();
            let content = replace_all(content, &replacements);
            Ok(bitnet_wildcard
                .replace_all(&content, "copybook-*")
                .into_owned())
        })?;
        if changed {
            fixed_count += 1;
        }
    }

    println!(
        "\nCompleted! Fixed {fixed_count} of {} agent files.",
        agent_files.len()
    );
    Ok(())
}

fn final_cleanup_agents() -> Result<()> {
    let root = workspace_root()?;
    let agent_files = review_agent_files(&root)?;

    println!("Found {} agent files for final cleanup", agent_files.len());

    let replacements = [
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
            "cargo clippy --workspace --all-targets --workspace",
            "cargo clippy --workspace --all-targets",
        ),
        (
            "--workspace -- -D warnings",
            "-- -D warnings -W clippy::pedantic",
        ),
        ("COBOL parsing COBOL parsing", "COBOL parsing"),
        ("enterprise performance/CPU", "high-performance"),
        ("enterprise performance memory", "memory"),
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
    ];
    let dequantize = Regex::new(r"test_dequantize_cpu_and_gpu_paths")?;
    let data_env = Regex::new(r#"COPYBOOK_DATA="[^"]*""#)?;

    let mut cleaned_count = 0;
    for agent_file in &agent_files {
        let changed = process_agent_file(
            agent_file,
            "Final cleanup of",
            "Cleaned up",
            "cleanup",
            |content| {
                let content = replace_all(content, &replacements);
                let content = dequantize
                    .replace_all(&content, "enterprise_performance_validation")
                    .into_owned();
                Ok(data_env
                    .replace_all(&content, "COPYBOOK_TEST_DATA=\"examples/test.cpy\"")
                    .into_owned())
            },
        )?;
        if changed {
            cleaned_count += 1;
        }
    }

    println!(
        "\nCompleted! Final cleanup applied to {cleaned_count} of {} agent files.",
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
