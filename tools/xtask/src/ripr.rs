// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use serde_json::Value;
use std::path::{Path, PathBuf};
use std::process::Command;

const RIPR_PR_DIR: &str = "target/ripr/pr";
const RIPR_REVIEW_DIR: &str = "target/ripr/review";
const RIPR_PR_JSON: &str = "target/ripr/pr/repo-exposure.json";
const RIPR_PR_MD: &str = "target/ripr/pr/repo-exposure.md";
const RIPR_REVIEW_JSON: &str = "target/ripr/review/comments.json";
const RIPR_REVIEW_MD: &str = "target/ripr/review/comments.md";

pub fn pr(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path();
    if check {
        check_json_file(&workspace_root.join(RIPR_PR_JSON))?;
        check_non_empty_file(&workspace_root.join(RIPR_PR_MD))?;
        println!("ripr-pr: output contract is intact");
        return Ok(());
    }

    std::fs::create_dir_all(workspace_root.join(RIPR_PR_DIR))?;
    let ripr_bin = ripr_bin();
    run_ripr_check_format(
        &ripr_bin,
        &workspace_root,
        "repo-exposure-json",
        &workspace_root.join(RIPR_PR_JSON),
    )?;
    run_ripr_check_format(
        &ripr_bin,
        &workspace_root,
        "repo-exposure-md",
        &workspace_root.join(RIPR_PR_MD),
    )?;

    check_json_file(&workspace_root.join(RIPR_PR_JSON))?;
    check_non_empty_file(&workspace_root.join(RIPR_PR_MD))?;
    println!("ripr-pr: wrote target/ripr/pr repo exposure evidence");
    Ok(())
}

pub fn review_comments(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path();
    if check {
        check_json_file(&workspace_root.join(RIPR_REVIEW_JSON))?;
        check_non_empty_file(&workspace_root.join(RIPR_REVIEW_MD))?;
        println!("ripr-review-comments: output contract is intact");
        return Ok(());
    }

    std::fs::create_dir_all(workspace_root.join(RIPR_REVIEW_DIR))?;
    let ripr_bin = ripr_bin();
    let output = Command::new(&ripr_bin)
        .arg("review-comments")
        .arg("--root")
        .arg(&workspace_root)
        .arg("--base")
        .arg(base_ref(&workspace_root))
        .arg("--head")
        .arg(head_ref())
        .arg("--out")
        .arg(workspace_root.join(RIPR_REVIEW_JSON))
        .current_dir(&workspace_root)
        .output()
        .with_context(|| format!("running {ripr_bin} review-comments"))?;

    if !output.status.success() {
        bail!(
            "{ripr_bin} review-comments failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    check_json_file(&workspace_root.join(RIPR_REVIEW_JSON))?;
    check_non_empty_file(&workspace_root.join(RIPR_REVIEW_MD))?;
    println!("ripr-review-comments: wrote target/ripr/review guidance");
    Ok(())
}

fn run_ripr_check_format(
    ripr_bin: &str,
    workspace_root: &Path,
    format: &str,
    out: &Path,
) -> Result<()> {
    let output = Command::new(ripr_bin)
        .arg("check")
        .arg("--root")
        .arg(workspace_root)
        .arg("--format")
        .arg(format)
        .current_dir(workspace_root)
        .output()
        .with_context(|| format!("running {ripr_bin} {format}"))?;

    if !output.status.success() {
        bail!(
            "{ripr_bin} {format} failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    std::fs::write(out, output.stdout).with_context(|| format!("writing {}", out.display()))
}

fn workspace_root_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .map_or_else(|| PathBuf::from("."), Path::to_path_buf)
}

fn ripr_bin() -> String {
    std::env::var("RIPR_BIN").unwrap_or_else(|_| "ripr".to_string())
}

fn base_ref(workspace_root: &Path) -> String {
    if let Ok(base) = std::env::var("RIPR_BASE") {
        return base;
    }

    let origin_main_exists = Command::new("git")
        .arg("rev-parse")
        .arg("--verify")
        .arg("origin/main")
        .current_dir(workspace_root)
        .output()
        .is_ok_and(|output| output.status.success());

    if origin_main_exists {
        "origin/main".to_string()
    } else {
        "HEAD".to_string()
    }
}

fn head_ref() -> String {
    std::env::var("RIPR_HEAD").unwrap_or_else(|_| "HEAD".to_string())
}

fn check_json_file(path: &Path) -> Result<()> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("missing required RIPR JSON file {}", path.display()))?;
    let _: Value = serde_json::from_str(&content)
        .with_context(|| format!("invalid RIPR JSON in {}", path.display()))?;
    Ok(())
}

fn check_non_empty_file(path: &Path) -> Result<()> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("missing required RIPR Markdown file {}", path.display()))?;
    if content.trim().is_empty() {
        bail!("required RIPR Markdown file is empty: {}", path.display());
    }
    Ok(())
}
