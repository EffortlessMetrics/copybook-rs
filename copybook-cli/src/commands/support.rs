//! Support command for COBOL feature matrix
//!
//! Provides CLI access to the feature support matrix defined in `copybook-core::support_matrix`.

use crate::exit_codes::ExitCode;
use anyhow::{Result, bail};
use copybook_core::support_matrix::{self, FeatureSupport, SupportStatus};

#[derive(clap::Args)]
pub struct SupportArgs {
    /// Output as JSON instead of a table
    #[arg(long)]
    pub json: bool,

    /// Check a single feature by ID (e.g. "level-88")
    #[arg(long)]
    pub check: Option<String>,
}

pub fn run(args: &SupportArgs) -> Result<ExitCode> {
    let all = support_matrix::all_features();

    if let Some(id) = args.check.as_deref() {
        return run_check(id, all);
    }

    if args.json {
        output_json(all)?;
    } else {
        output_table(all)?;
    }

    Ok(ExitCode::Ok)
}

fn run_check(id: &str, _all: &[FeatureSupport]) -> Result<ExitCode> {
    if let Some(feature) = support_matrix::find_feature(id) {
        // Simple rule: only `supported` is success; everything else is non-zero exit.
        match feature.status {
            SupportStatus::Supported => Ok(ExitCode::Ok),
            _status => {
                eprintln!(
                    "❌ Feature '{}' not fully supported (status: {:?}). See {}",
                    id,
                    feature.status,
                    feature.doc_ref.unwrap_or("project documentation"),
                );
                Ok(ExitCode::Encode) // Use non-zero exit code (policy/validation failure)
            }
        }
    } else {
        bail!("Unknown feature id '{id}'. Run `copybook support` to see valid IDs.");
    }
}

fn output_json(all: &[FeatureSupport]) -> Result<()> {
    let json = serde_json::to_string_pretty(all)?;
    println!("{json}");
    Ok(())
}

fn output_table(all: &[FeatureSupport]) -> Result<()> {
    // Simple fixed-width columns
    println!("{:<24}  {:<12}  DESCRIPTION", "FEATURE", "STATUS");
    println!("{:-<24}  {:-<12}  {:-<60}", "", "", "");

    for f in all {
        let id = serde_plain::to_string(&f.id).unwrap_or_else(|_| "<invalid>".into());
        let status = format!("{:?}", f.status);
        println!(
            "{:<24}  {:<12}  {}",
            id,
            status.to_lowercase(),
            f.description,
        );
    }

    println!();
    println!("Use 'copybook support --check <feature-id>' to check a specific feature.");
    println!("Use 'copybook support --json' for machine-readable output.");

    Ok(())
}
