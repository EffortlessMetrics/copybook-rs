// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

const RIPR_PR_DIR: &str = "target/ripr/pr";
const RIPR_REVIEW_DIR: &str = "target/ripr/review";

pub fn pr(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path()?;
    let out_dir = workspace_root.join(RIPR_PR_DIR);
    let json = out_dir.join("repo-exposure.json");
    let markdown = out_dir.join("repo-exposure.md");

    if check {
        verify_json_file(&json)?;
        verify_non_empty_file(&markdown)?;
        println!("ripr-pr: output contract is intact");
        return Ok(());
    }

    fs::create_dir_all(&out_dir)?;
    let ripr_bin = ripr_bin();
    let output = Command::new(&ripr_bin)
        .arg("check")
        .arg("--root")
        .arg(&workspace_root)
        .arg("--format")
        .arg("repo-exposure")
        .current_dir(&workspace_root)
        .output()
        .with_context(|| format!("failed to run {ripr_bin}; install ripr or set RIPR_BIN"))?;

    if !output.status.success() {
        bail!(
            "{ripr_bin} repo-exposure failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fs::write(&json, ensure_trailing_newline(&output.stdout))?;
    write_pr_markdown(&markdown, &json)?;
    println!(
        "ripr-pr: wrote {} and {}",
        json.display(),
        markdown.display()
    );
    Ok(())
}

pub fn review_comments(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path()?;
    let out_dir = workspace_root.join(RIPR_REVIEW_DIR);
    let json = out_dir.join("comments.json");
    let markdown = out_dir.join("comments.md");

    if check {
        verify_json_file(&json)?;
        verify_non_empty_file(&markdown)?;
        println!("ripr-review-comments: output contract is intact");
        return Ok(());
    }

    fs::create_dir_all(&out_dir)?;
    let ripr_bin = ripr_bin();
    let output = Command::new(&ripr_bin)
        .arg("review-comments")
        .arg("--root")
        .arg(&workspace_root)
        .arg("--base")
        .arg(ripr_base())
        .arg("--head")
        .arg(ripr_head())
        .arg("--out")
        .arg(&json)
        .current_dir(&workspace_root)
        .output()
        .with_context(|| format!("failed to run {ripr_bin}; install ripr or set RIPR_BIN"))?;

    if !output.status.success() {
        bail!(
            "{ripr_bin} review-comments failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    if !json.exists() && !output.stdout.is_empty() {
        fs::write(&json, ensure_trailing_newline(&output.stdout))?;
    }
    if !markdown.exists() {
        write_review_markdown(&markdown, &json)?;
    }

    verify_json_file(&json)?;
    verify_non_empty_file(&markdown)?;
    println!("ripr-review-comments: wrote {}", out_dir.display());
    Ok(())
}

fn ripr_bin() -> String {
    std::env::var("RIPR_BIN").unwrap_or_else(|_| "ripr".to_string())
}

fn ripr_base() -> String {
    std::env::var("RIPR_BASE").unwrap_or_else(|_| "origin/main".to_string())
}

fn ripr_head() -> String {
    std::env::var("RIPR_HEAD").unwrap_or_else(|_| "HEAD".to_string())
}

fn workspace_root_path() -> Result<PathBuf> {
    let output = Command::new("cargo")
        .args(["metadata", "--no-deps", "--format-version", "1"])
        .output()
        .context("failed to run cargo metadata while locating workspace root")?;

    if !output.status.success() {
        bail!(
            "cargo metadata failed while locating workspace root: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let metadata: serde_json::Value = serde_json::from_slice(&output.stdout)
        .context("cargo metadata emitted invalid JSON while locating workspace root")?;
    let root = metadata
        .get("workspace_root")
        .and_then(serde_json::Value::as_str)
        .context("cargo metadata did not include workspace_root")?;
    Ok(PathBuf::from(root))
}

fn verify_json_file(path: &Path) -> Result<()> {
    let content = fs::read_to_string(path)
        .with_context(|| format!("missing required RIPR JSON file {}", path.display()))?;
    if content.trim().is_empty() {
        bail!("RIPR JSON file is empty: {}", path.display());
    }
    let _: serde_json::Value = serde_json::from_str(&content)
        .with_context(|| format!("invalid RIPR JSON file {}", path.display()))?;
    Ok(())
}

fn verify_non_empty_file(path: &Path) -> Result<()> {
    let content = fs::read_to_string(path)
        .with_context(|| format!("missing required RIPR Markdown file {}", path.display()))?;
    if content.trim().is_empty() {
        bail!("RIPR Markdown file is empty: {}", path.display());
    }
    Ok(())
}

fn ensure_trailing_newline(bytes: &[u8]) -> Vec<u8> {
    let mut out = bytes.to_vec();
    if !out.ends_with(b"\n") {
        out.push(b'\n');
    }
    out
}

fn write_pr_markdown(path: &Path, json_path: &Path) -> Result<()> {
    let content = fs::read_to_string(json_path)?;
    let value: serde_json::Value = serde_json::from_str(&content)?;
    let findings = value
        .get("findings")
        .and_then(serde_json::Value::as_array)
        .map_or(0, Vec::len);
    fs::write(
        path,
        format!(
            "# RIPR PR Evidence\n\n- Findings: `{findings}`\n- Source: `{}`\n",
            json_path.display()
        ),
    )?;
    Ok(())
}

fn write_review_markdown(path: &Path, json_path: &Path) -> Result<()> {
    let content = fs::read_to_string(json_path)?;
    let value: serde_json::Value = serde_json::from_str(&content)?;
    let comments = value
        .get("comments")
        .and_then(serde_json::Value::as_array)
        .map_or(0, Vec::len);
    let summary_only = value
        .get("summary_only")
        .and_then(serde_json::Value::as_array)
        .map_or(0, Vec::len);
    fs::write(
        path,
        format!(
            "# RIPR Review Guidance\n\n- Line-placeable comments: `{comments}`\n- Summary-only findings: `{summary_only}`\n- Source: `{}`\n",
            json_path.display()
        ),
    )?;
    Ok(())
}
