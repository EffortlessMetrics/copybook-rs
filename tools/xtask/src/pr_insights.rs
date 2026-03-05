use anyhow::Result;
use std::{fs, path::Path};
use crate::perf;

pub fn generate_summary() -> Result<()> {
    let mut markdown = String::new();
    markdown.push_str("## 🚀 DevEx Automated PR Insights\n\n");

    // Test Status
    if let Ok(c) = crate::counts() {
        markdown.push_str("### 🧪 Test Status\n");
        markdown.push_str(&format!("- **Passed**: {}\n", c.passed));
        markdown.push_str(&format!("- **Failed**: {}\n", c.failed));
        markdown.push_str(&format!("- **Skipped**: {}\n\n", c.skipped));
    } else {
        markdown.push_str("### 🧪 Test Status\n");
        markdown.push_str("⚠️ Could not load test results (missing junit.xml).\n\n");
    }

    // Perf Status
    markdown.push_str("### ⚡ Performance Delta\n");
    let canonical = Path::new("scripts/bench/perf.json");
    if canonical.exists() {
        if let Ok(json_content) = fs::read_to_string(canonical) {
            if let Ok(snapshot) = perf::parse_perf_receipt(&json_content) {
                let status = perf::evaluate_slo(&snapshot);
                let summary = perf::format_slo_summary(&snapshot, &status);
                markdown.push_str("```text\n");
                markdown.push_str(&summary);
                markdown.push_str("\n```\n");
            } else {
                markdown.push_str("⚠️ Failed to parse performance receipt.\n");
            }
        }
    } else {
        markdown.push_str("ℹ️ No performance receipt found for this PR.\n");
    }

    // Write to a GitHub step summary file if available
    if let Ok(gh_step_summary) = std::env::var("GITHUB_STEP_SUMMARY") {
        let mut existing = fs::read_to_string(&gh_step_summary).unwrap_or_default();
        existing.push_str(&markdown);
        let _ = fs::write(&gh_step_summary, existing);
    }

    // Also write to a generic pr_comment.md for easy commenting
    fs::write("pr_comment.md", &markdown)?;

    println!("✓ Generated PR insights at pr_comment.md");
    Ok(())
}