// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use std::path::{Path, PathBuf};

const RIPR_PR_DIR: &str = "target/ripr/pr";
const RIPR_REVIEW_DIR: &str = "target/ripr/review";

/// Generate or check PR-scoped RIPR exposure evidence.
///
/// # Errors
///
/// Returns an error when workspace discovery fails, `ripr` cannot produce
/// evidence, or the required JSON/Markdown artifacts are missing or invalid.
#[inline]
pub fn pr(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path()?;
    if check {
        check_pr_contract(&workspace_root)
    } else {
        run_ripr_pr(&workspace_root)
    }
}

/// Generate or check RIPR changed-line review guidance.
///
/// # Errors
///
/// Returns an error when workspace discovery fails, `ripr review-comments`
/// fails for the configured base/head, or the output contract is invalid.
#[inline]
pub fn review_comments(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path()?;
    if check {
        check_review_contract(&workspace_root)
    } else {
        run_review_comments(&workspace_root)
    }
}

fn run_ripr_pr(workspace_root: &Path) -> Result<()> {
    let ripr_bin = std::env::var("RIPR_BIN").unwrap_or_else(|_| "ripr".to_string());
    let out_dir = workspace_root.join(RIPR_PR_DIR);
    std::fs::create_dir_all(&out_dir)?;

    let output = std::process::Command::new(&ripr_bin)
        .arg("check")
        .arg("--root")
        .arg(workspace_root)
        .arg("--format")
        .arg("repo-exposure-json")
        .current_dir(workspace_root)
        .output()
        .with_context(|| format!("failed to run {ripr_bin} for PR evidence"))?;

    if !output.status.success() {
        bail!(
            "{ripr_bin} repo-exposure-json failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let json_path = out_dir.join("repo-exposure.json");
    std::fs::write(&json_path, &output.stdout)?;
    validate_json_file(&json_path)?;

    let md_output = std::process::Command::new(&ripr_bin)
        .arg("check")
        .arg("--root")
        .arg(workspace_root)
        .arg("--format")
        .arg("repo-exposure-md")
        .current_dir(workspace_root)
        .output()
        .with_context(|| format!("failed to run {ripr_bin} for Markdown PR evidence"))?;

    if !md_output.status.success() {
        bail!(
            "{ripr_bin} repo-exposure-md failed: {}",
            String::from_utf8_lossy(&md_output.stderr)
        );
    }

    let md_path = out_dir.join("repo-exposure.md");
    std::fs::write(&md_path, &md_output.stdout)?;
    ensure_non_empty(&md_path)?;

    println!("ripr-pr: wrote PR-scoped evidence under {RIPR_PR_DIR}/");
    Ok(())
}

fn run_review_comments(workspace_root: &Path) -> Result<()> {
    let ripr_bin = std::env::var("RIPR_BIN").unwrap_or_else(|_| "ripr".to_string());
    let out_dir = workspace_root.join(RIPR_REVIEW_DIR);
    std::fs::create_dir_all(&out_dir)?;
    let out_file = out_dir.join("comments.json");

    let base = std::env::var("RIPR_BASE").unwrap_or_else(|_| "origin/main".to_string());
    let head = std::env::var("RIPR_HEAD").unwrap_or_else(|_| "HEAD".to_string());

    let output = std::process::Command::new(&ripr_bin)
        .arg("review-comments")
        .arg("--root")
        .arg(workspace_root)
        .arg("--base")
        .arg(&base)
        .arg("--head")
        .arg(&head)
        .arg("--out")
        .arg(&out_file)
        .current_dir(workspace_root)
        .output()
        .with_context(|| format!("failed to run {ripr_bin} review-comments"))?;

    if !output.status.success() {
        bail!(
            "{ripr_bin} review-comments failed for {base}..{head}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    check_review_contract(workspace_root)?;
    println!("ripr-review-comments: wrote review guidance under {RIPR_REVIEW_DIR}/");
    Ok(())
}

fn check_pr_contract(workspace_root: &Path) -> Result<()> {
    let json_path = workspace_root.join(RIPR_PR_DIR).join("repo-exposure.json");
    let md_path = workspace_root.join(RIPR_PR_DIR).join("repo-exposure.md");
    validate_json_file(&json_path)?;
    ensure_non_empty(&md_path)?;
    println!("ripr-pr: output contract is intact");
    Ok(())
}

fn check_review_contract(workspace_root: &Path) -> Result<()> {
    let json_path = workspace_root.join(RIPR_REVIEW_DIR).join("comments.json");
    let md_path = workspace_root.join(RIPR_REVIEW_DIR).join("comments.md");
    validate_json_file(&json_path)?;
    ensure_non_empty(&md_path)?;
    println!("ripr-review-comments: output contract is intact");
    Ok(())
}

fn validate_json_file(path: &Path) -> Result<()> {
    let bytes = std::fs::read(path)
        .with_context(|| format!("missing required RIPR JSON artifact {}", path.display()))?;
    if bytes.is_empty() {
        bail!("RIPR JSON artifact is empty: {}", path.display());
    }
    let _: serde_json::Value = serde_json::from_slice(&bytes)
        .with_context(|| format!("invalid JSON in RIPR artifact {}", path.display()))?;
    Ok(())
}

fn ensure_non_empty(path: &Path) -> Result<()> {
    let metadata = std::fs::metadata(path)
        .with_context(|| format!("missing required RIPR artifact {}", path.display()))?;
    if metadata.len() == 0 {
        bail!("RIPR artifact is empty: {}", path.display());
    }
    Ok(())
}

fn workspace_root_path() -> Result<PathBuf> {
    let output = std::process::Command::new("cargo")
        .args(["metadata", "--no-deps", "--format-version", "1"])
        .output()
        .context("failed to run cargo metadata to locate workspace root")?;

    if !output.status.success() {
        bail!(
            "cargo metadata failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let metadata: serde_json::Value = serde_json::from_slice(&output.stdout)
        .context("cargo metadata emitted invalid JSON while locating workspace root")?;
    let Some(root) = metadata
        .get("workspace_root")
        .and_then(serde_json::Value::as_str)
    else {
        bail!("cargo metadata did not include workspace_root");
    };

    Ok(PathBuf::from(root))
}
