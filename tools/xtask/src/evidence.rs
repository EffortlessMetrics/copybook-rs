// SPDX-License-Identifier: AGPL-3.0-or-later
use anyhow::Result;
use serde_json::json;
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn impacted_evidence() -> Result<()> {
    let workspace_root = workspace_root_path();
    let out_dir = workspace_root.join("target/xtask/impacted-evidence");
    std::fs::create_dir_all(&out_dir)?;

    let evidence = json!({
        "schema_version": 1,
        "requires_targeted_mutation": false,
        "reason": "No repository-specific impacted-evidence router has requested targeted mutation.",
        "ripr": {
            "requires_targeted_evidence": false
        }
    });
    std::fs::write(
        out_dir.join("latest.json"),
        format!("{}\n", serde_json::to_string_pretty(&evidence)?),
    )?;
    std::fs::write(
        out_dir.join("latest.md"),
        "# Impacted Evidence\n\nTargeted mutation is not required by the default router.\n",
    )?;
    println!("impacted-evidence: wrote target/xtask/impacted-evidence/latest.json");
    Ok(())
}

pub fn mutants_pr(args: &[&str]) -> Result<()> {
    let workspace_root = workspace_root_path();
    let dry_run = args.contains(&"--dry-run");
    let full_owner = args.contains(&"--full-owner");

    let mut command = Command::new("cargo");
    command
        .arg("mutants")
        .arg("--workspace")
        .arg("--file")
        .arg("mutants.toml")
        .arg("--test-tool")
        .arg("nextest")
        .arg("--in-place")
        .current_dir(&workspace_root);

    if dry_run {
        println!(
            "mutants-pr: dry-run{}: {:?}",
            if full_owner { " full-owner" } else { "" },
            command
        );
        return Ok(());
    }

    let status = command.status()?;
    if !status.success() {
        anyhow::bail!("cargo mutants PR run failed with status {status}");
    }
    Ok(())
}

fn workspace_root_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .map_or_else(|| PathBuf::from("."), Path::to_path_buf)
}
