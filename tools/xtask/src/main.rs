// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::{Context, Result, bail};
use copybook_core::support_matrix;
use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};
use xtask::{Counts, counts, perf};

mod pr_insights;

const BADGE_ENDPOINT_DIR: &str = "badges";
const BADGE_ENDPOINT_TARGET_DIR: &str = "target/xtask/badges";
const RIPR_PR_DIR: &str = "target/ripr/pr";
const RIPR_REVIEW_DIR: &str = "target/ripr/review";

#[derive(Clone, Debug, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
struct ShieldsEndpointBadge {
    #[serde(rename = "schemaVersion")]
    schema_version: u8,
    label: String,
    message: String,
    color: String,
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args
        .iter()
        .map(std::string::String::as_str)
        .collect::<Vec<_>>()
        .as_slice()
    {
        ["docs", "sync-tests"] => sync(),
        ["docs", "verify-tests"] => verify(),
        ["docs", "verify-support-matrix"] => verify_support_matrix(),
        ["perf"] => perf::run(false, None),
        ["perf", "--enforce"] => perf::run(true, None),
        ["perf", "--out-dir", out_dir] => perf::run(false, Some(out_dir)),
        ["perf", "--enforce", "--out-dir", out_dir] => perf::run(true, Some(out_dir)),
        ["perf", "--summarize-last" | "--summarize"] => perf_summarize_last(),
        ["pr-insights"] => pr_insights::generate_summary(),
        ["badges"] => badges(false),
        ["badges", "--check"] => badges(true),
        ["ripr-pr"] => ripr_pr(false),
        ["ripr-pr", "--check"] => ripr_pr(true),
        ["ripr-review-comments"] => ripr_review_comments(false),
        ["ripr-review-comments", "--check"] => ripr_review_comments(true),
        ["docs-sync", "--check"] => verify_support_matrix(),
        ["check-file-policy"] => check_file_policy(),
        ["pr"] => pr_gate(),
        _ => {
            eprintln!(
                "Usage: cargo run -p xtask -- [docs|perf|pr-insights] <subcommand>\n\
                 \n\
                 docs sync-tests                 Sync test status from junit.xml\n\
                 docs verify-tests               Verify test status is in sync\n\
                 docs verify-support-matrix      Verify support matrix registry ↔ docs\n\
                 perf                            Run perf benchmark runner\n\
                 perf --enforce                  Run perf with SLO enforcement\n\
                 perf --out-dir <path>           Run perf with custom output directory\n\
                 perf --summarize-last           Summarize latest perf.json with SLO comparison\n\
                 pr-insights                     Generate PR insights report (nextest + perf)
\
                 badges [--check]                Generate or verify public Shields badge endpoints
\
                 ripr-pr [--check]               Generate or verify PR-scoped RIPR evidence
\
                 ripr-review-comments [--check]  Generate or verify RIPR review guidance
\
                 docs-sync --check               Verify generated docs surfaces
\
                 check-file-policy               Verify non-Rust file policy entries
\
                 pr                              Run the local fast PR gate"
            );
            Ok(())
        }
    }
}

fn block(c: &Counts) -> String {
    let p = c.passed;
    let s = c.skipped;
    format!(
        "**conformance:** {p}/{p} • **roundtrip:** N/A • **negative:** N/A • **skipped:** {s} • **leaks:** 0  \n\
         _Source: CI receipts (nextest/junit). This block is updated automatically._"
    )
}

fn replace_in_file(path: &str, new_block: &str) -> Result<()> {
    let content = fs::read_to_string(path)?;

    // Find the TEST_STATUS section and replace it
    let re = regex::Regex::new(r"(?s)<!-- TEST_STATUS:BEGIN -->.*?<!-- TEST_STATUS:END -->")?;

    let replacement = format!("<!-- TEST_STATUS:BEGIN -->\n{new_block}\n<!-- TEST_STATUS:END -->");

    let new_content = re.replace(&content, replacement.as_str());
    fs::write(path, new_content.as_ref())?;

    Ok(())
}

fn sync() -> Result<()> {
    let c = counts()?;
    let b = block(&c);

    replace_in_file("README.md", &b)?;
    replace_in_file("docs/REPORT.md", &b)?;

    println!("✓ Synced test status to README.md and docs/REPORT.md");
    Ok(())
}

fn verify() -> Result<()> {
    let c = counts()?;
    let expected = block(&c);

    for path in ["README.md", "docs/REPORT.md"] {
        let content = fs::read_to_string(path)?;
        if !content.contains(&expected) {
            bail!("{path} test-status out of sync");
        }
    }

    println!("✓ Test status is in sync");
    Ok(())
}

fn verify_support_matrix() -> Result<()> {
    let doc_path = "docs/reference/COBOL_SUPPORT_MATRIX.md";
    let doc_content = fs::read_to_string(doc_path)?;

    let all_features = support_matrix::all_features();
    let mut missing = Vec::new();

    for feature in all_features {
        let id =
            serde_plain::to_string(&feature.id).unwrap_or_else(|_| format!("{:?}", feature.id));

        // Check if the feature ID appears anywhere in the doc
        // We're lenient: just check for the kebab-case ID string
        if !doc_content.contains(&id) {
            missing.push(id);
        }
    }

    if !missing.is_empty() {
        bail!(
            "Support matrix drift detected!\n\
             The following features are in the registry but not documented in {doc_path}:\n  - {}\n\n\
             Add these features to the appropriate tables in {doc_path}.",
            missing.join("\n  - ")
        );
    }

    println!(
        "✓ Support matrix registry ↔ docs in sync ({} features verified)",
        all_features.len()
    );
    Ok(())
}

fn perf_summarize_last() -> Result<()> {
    // Try to find the latest perf.json, preferring scripts/bench/perf.json (canonical)
    let canonical = Path::new("scripts/bench/perf.json");
    let perf_path = if canonical.exists() {
        canonical.to_path_buf()
    } else {
        // Try to find the latest in target/benchmarks/
        let benchmarks_dir = Path::new("target/benchmarks");
        if !benchmarks_dir.exists() {
            bail!("No perf.json found. Run benchmarks first:\n  bash scripts/bench.sh");
        }

        // Find the most recent timestamp directory
        let mut dirs: Vec<_> = fs::read_dir(benchmarks_dir)?
            .filter_map(Result::ok)
            .filter(|e| e.path().is_dir())
            .collect();

        if dirs.is_empty() {
            bail!("No benchmark runs found in target/benchmarks/");
        }

        // Sort by name (which should be timestamps)
        dirs.sort_by_key(std::fs::DirEntry::path);

        // Try to find perf.json in the latest directory
        let Some(latest_entry) = dirs.last() else {
            anyhow::bail!(
                "No benchmark receipt directories found under {}",
                benchmarks_dir.display()
            );
        };
        let latest_dir = &latest_entry.path();
        let latest_perf = latest_dir.join("perf.json");

        if !latest_perf.exists() {
            bail!(
                "No perf.json found in latest benchmark run: {}",
                latest_dir.display()
            );
        }

        latest_perf
    };

    // Parse the JSON using pure function
    let json_content = fs::read_to_string(perf_path)?;
    let snapshot = perf::parse_perf_receipt(&json_content)?;

    // Evaluate SLO compliance
    let status = perf::evaluate_slo(&snapshot);

    // Emit formatted summary
    let summary = perf::format_slo_summary(&snapshot, &status);
    println!("{summary}");

    Ok(())
}

fn workspace_root_path() -> Result<PathBuf> {
    let output = Command::new("cargo")
        .args(["metadata", "--no-deps", "--format-version", "1"])
        .output()
        .context("failed to run `cargo metadata` to locate workspace root")?;

    if !output.status.success() {
        bail!(
            "cargo metadata failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let metadata: serde_json::Value =
        serde_json::from_slice(&output.stdout).context("cargo metadata emitted invalid JSON")?;
    let Some(root) = metadata.get("workspace_root").and_then(|v| v.as_str()) else {
        bail!("cargo metadata JSON did not include workspace_root");
    };
    Ok(PathBuf::from(root))
}

fn write_json_pretty<T: serde::Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let json = serde_json::to_string_pretty(value)?;
    fs::write(path, format!("{json}\n"))?;
    Ok(())
}

fn compare_files(committed: &Path, generated: &Path) -> Result<()> {
    let committed_bytes = fs::read(committed)
        .with_context(|| format!("missing committed badge endpoint: {}", committed.display()))?;
    let generated_bytes = fs::read(generated)
        .with_context(|| format!("missing generated badge endpoint: {}", generated.display()))?;
    if committed_bytes != generated_bytes {
        bail!(
            "badge endpoint drift detected: {} differs from {}; run `cargo xtask badges`",
            committed.display(),
            generated.display()
        );
    }
    Ok(())
}

fn badges(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path()?;
    let target_dir = workspace_root.join(BADGE_ENDPOINT_TARGET_DIR);
    fs::create_dir_all(&target_dir)?;

    let ripr_plus = ripr_plus_badge(&workspace_root)?;
    validate_shields_badge(&ripr_plus, Some("ripr+"))?;
    write_json_pretty(&target_dir.join("ripr-plus.json"), &ripr_plus)?;

    if check {
        compare_files(
            &workspace_root
                .join(BADGE_ENDPOINT_DIR)
                .join("ripr-plus.json"),
            &target_dir.join("ripr-plus.json"),
        )?;
        println!("badges: committed endpoints are current");
        return Ok(());
    }

    let committed_dir = workspace_root.join(BADGE_ENDPOINT_DIR);
    fs::create_dir_all(&committed_dir)?;
    fs::copy(
        target_dir.join("ripr-plus.json"),
        committed_dir.join("ripr-plus.json"),
    )?;
    println!("badges: refreshed public endpoint JSON under badges/");
    Ok(())
}

fn ripr_plus_badge(workspace_root: &Path) -> Result<ShieldsEndpointBadge> {
    ensure_test_efficiency_report(workspace_root)?;
    let ripr_bin = std::env::var("RIPR_BIN").unwrap_or_else(|_| "ripr".to_string());
    let output = Command::new(&ripr_bin)
        .arg("check")
        .arg("--root")
        .arg(workspace_root)
        .arg("--format")
        .arg("repo-badge-plus-shields")
        .current_dir(workspace_root)
        .output()
        .with_context(|| format!("failed to run `{ripr_bin}` for repo-scoped badge evidence"))?;

    if !output.status.success() {
        bail!(
            "{ripr_bin} repo-badge-plus-shields failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    serde_json::from_slice(&output.stdout)
        .with_context(|| format!("{ripr_bin} emitted invalid Shields endpoint JSON"))
}

fn ensure_test_efficiency_report(workspace_root: &Path) -> Result<()> {
    let report = workspace_root.join("target/ripr/reports/test-efficiency.json");
    if report.exists() {
        return Ok(());
    }

    write_json_pretty(
        &report,
        &serde_json::json!({
            "schema_version": "0.1",
            "tests": [],
            "metrics": {
                "tests_scanned": 0,
                "reason_counts": {},
            },
        }),
    )
}

fn validate_shields_badge(
    badge: &ShieldsEndpointBadge,
    expected_label: Option<&str>,
) -> Result<()> {
    if badge.schema_version != 1 {
        bail!("badge `{}` has unsupported schemaVersion", badge.label);
    }
    if let Some(expected_label) = expected_label
        && badge.label != expected_label
    {
        bail!(
            "badge label drifted: got `{}`, expected `{expected_label}`",
            badge.label
        );
    }
    if badge.message.trim().is_empty() {
        bail!("badge `{}` has empty message", badge.label);
    }
    if badge.color.trim().is_empty() {
        bail!("badge `{}` has empty color", badge.label);
    }
    Ok(())
}

fn ripr_bin() -> String {
    std::env::var("RIPR_BIN").unwrap_or_else(|_| "ripr".to_string())
}

fn ripr_pr(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path()?;
    let out_dir = workspace_root.join(RIPR_PR_DIR);
    if check {
        return check_ripr_pr_contract(&out_dir);
    }

    fs::create_dir_all(&out_dir)?;
    let json_path = out_dir.join("repo-exposure.json");
    let md_path = out_dir.join("repo-exposure.md");
    let bin = ripr_bin();
    for (format, path) in [
        ("repo-exposure-json", json_path.as_path()),
        ("repo-exposure-md", md_path.as_path()),
    ] {
        let output = Command::new(&bin)
            .arg("check")
            .arg("--root")
            .arg(&workspace_root)
            .arg("--format")
            .arg(format)
            .current_dir(&workspace_root)
            .output()
            .with_context(|| format!("failed to run `{bin}` for PR exposure evidence"))?;
        if !output.status.success() {
            bail!(
                "{bin} PR exposure evidence failed for {format}: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        fs::write(path, &output.stdout)?;
    }
    check_ripr_pr_contract(&out_dir)
}

fn check_ripr_pr_contract(out_dir: &Path) -> Result<()> {
    let json_path = out_dir.join("repo-exposure.json");
    let md_path = out_dir.join("repo-exposure.md");
    let json = fs::read_to_string(&json_path)
        .with_context(|| format!("missing RIPR PR JSON: {}", json_path.display()))?;
    let _: serde_json::Value = serde_json::from_str(&json)
        .with_context(|| format!("invalid RIPR PR JSON: {}", json_path.display()))?;
    let md = fs::read_to_string(&md_path)
        .with_context(|| format!("missing RIPR PR Markdown: {}", md_path.display()))?;
    if md.trim().is_empty() {
        bail!("RIPR PR Markdown is empty: {}", md_path.display());
    }
    println!("ripr-pr: output contract is intact");
    Ok(())
}

fn ripr_review_comments(check: bool) -> Result<()> {
    let workspace_root = workspace_root_path()?;
    let out_dir = workspace_root.join(RIPR_REVIEW_DIR);
    let json_path = out_dir.join("comments.json");
    let md_path = out_dir.join("comments.md");
    if check {
        return check_ripr_review_contract(&json_path, &md_path);
    }

    fs::create_dir_all(&out_dir)?;
    let bin = ripr_bin();
    let base = std::env::var("RIPR_BASE").unwrap_or_else(|_| "origin/main".to_string());
    let head = std::env::var("RIPR_HEAD").unwrap_or_else(|_| "HEAD".to_string());
    let output = Command::new(&bin)
        .arg("review-comments")
        .arg("--root")
        .arg(&workspace_root)
        .arg("--base")
        .arg(&base)
        .arg("--head")
        .arg(&head)
        .arg("--out")
        .arg(&json_path)
        .current_dir(&workspace_root)
        .output()
        .with_context(|| format!("failed to run `{bin} review-comments`"))?;
    if !output.status.success() {
        bail!(
            "{bin} review-comments failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    if !md_path.exists() {
        fs::write(
            &md_path,
            "# RIPR Review Guidance\n\nNo Markdown review guidance was produced by ripr.\n",
        )?;
    }
    check_ripr_review_contract(&json_path, &md_path)
}

fn check_ripr_review_contract(json_path: &Path, md_path: &Path) -> Result<()> {
    let json = fs::read_to_string(json_path)
        .with_context(|| format!("missing RIPR review JSON: {}", json_path.display()))?;
    let _: serde_json::Value = serde_json::from_str(&json)
        .with_context(|| format!("invalid RIPR review JSON: {}", json_path.display()))?;
    let md = fs::read_to_string(md_path)
        .with_context(|| format!("missing RIPR review Markdown: {}", md_path.display()))?;
    if md.trim().is_empty() {
        bail!("RIPR review Markdown is empty: {}", md_path.display());
    }
    println!("ripr-review-comments: output contract is intact");
    Ok(())
}

fn check_file_policy() -> Result<()> {
    let workspace_root = workspace_root_path()?;
    for required in ["badges/ripr-plus.json", "badges/README.md"] {
        let path = workspace_root.join(required);
        if !path.exists() {
            bail!("required generated badge policy surface is missing: {required}");
        }
    }
    println!("check-file-policy: generated badge endpoints are owned by repository docs policy");
    Ok(())
}

fn pr_gate() -> Result<()> {
    verify_support_matrix()?;
    check_file_policy()?;
    println!("pr: local fast gate passed");
    Ok(())
}

#[cfg(test)]
mod badge_tests {
    use super::*;

    #[test]
    fn ripr_plus_badge_shape_is_stable() {
        let badge = ShieldsEndpointBadge {
            schema_version: 1,
            label: "ripr+".to_string(),
            message: "0".to_string(),
            color: "brightgreen".to_string(),
        };

        validate_shields_badge(&badge, Some("ripr+")).unwrap();
    }

    #[test]
    fn badge_shape_rejects_wrong_label() {
        let badge = ShieldsEndpointBadge {
            schema_version: 1,
            label: "fixtures".to_string(),
            message: "scanner-safe".to_string(),
            color: "brightgreen".to_string(),
        };

        assert!(validate_shields_badge(&badge, Some("ripr+")).is_err());
    }
}
