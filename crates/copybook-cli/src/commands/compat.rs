// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compat command implementation.
//!
//! Compares two copybooks (base vs head) under identical effective options
//! and reports scenario-level change: what newly passes, what newly fails.
//! The comparison domain ([`compare_assessments`], ranks, breaking rules)
//! lives in the advise library; this module only renders the verdict and
//! maps it to the process exit code, so CI can reject breaking copybook
//! changes with `--fail-on`.

use crate::commands::support::OutputFormat;
use crate::exit_codes::ExitCode;
use crate::write_stdout_all;
use copybook::support_matrix::advise::{
    AdviseResult, AssessmentStatus, ScenarioChange, Verdict, compare_assessments,
};
use serde::Serialize;
use std::fmt::Write as _;
use std::path::Path;

#[derive(Clone, Copy, Debug, clap::ValueEnum, PartialEq, Eq)]
pub enum FailOn {
    /// Fail only when the head copybook newly refuses input the base
    /// accepted (new `Rejected`, `Invalid`, or `ToolFailure` outcomes).
    Breaking,
    /// Fail on any scenario outcome that got worse, including new
    /// limits, beta, or unknown outcomes.
    Any,
}

/// Machine-readable compat verdict, serialized deterministically.
#[derive(Serialize)]
struct CompatReport {
    report_version: u32,
    verdict: &'static str,
    fail_on: &'static str,
    base_fingerprint: Option<String>,
    head_fingerprint: Option<String>,
    changes: Vec<CompatChange>,
}

/// One scenario change in report shape.
#[derive(Serialize)]
struct CompatChange {
    scenario_id: String,
    base: AssessmentStatus,
    head: AssessmentStatus,
    regression: bool,
    breaking: bool,
    improvement: bool,
}

/// Compare two copybooks and exit non-zero when the change violates the
/// `--fail-on` policy. Both sides are evaluated under identical options,
/// so a reported change is always a copybook change, never an option skew.
#[allow(clippy::too_many_arguments)]
pub fn run(
    base: &Path,
    head: &Path,
    record_format: &str,
    codepage: &str,
    dialect: Option<crate::cli_config::DialectPreference>,
    fail_on: FailOn,
    output: OutputFormat,
) -> anyhow::Result<ExitCode> {
    let base_result =
        super::support_advise::analyze_copybook(base, record_format, codepage, dialect)?;
    let head_result =
        super::support_advise::analyze_copybook(head, record_format, codepage, dialect)?;

    let verdict = decide(&base_result, &head_result, fail_on);
    let changes = compare_assessments(&base_result.scenarios, &head_result.scenarios);
    render(
        verdict,
        fail_on,
        &base_result,
        &head_result,
        &changes,
        output,
    )?;
    Ok(match verdict {
        CompatVerdict::Compatible => ExitCode::Ok,
        CompatVerdict::Incompatible | CompatVerdict::Inconclusive => ExitCode::Encode,
    })
}

/// Compat outcome vocabulary. `Incompatible` means the `--fail-on` policy
/// fired; `Inconclusive` means the two sides could not be compared (the
/// base copybook itself does not analyze).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CompatVerdict {
    Compatible,
    Incompatible,
    Inconclusive,
}

impl CompatVerdict {
    fn as_str(self) -> &'static str {
        match self {
            CompatVerdict::Compatible => "compatible",
            CompatVerdict::Incompatible => "incompatible",
            CompatVerdict::Inconclusive => "inconclusive",
        }
    }
}

fn decide(base: &AdviseResult, head: &AdviseResult, fail_on: FailOn) -> CompatVerdict {
    if matches!(base.verdict, Verdict::InvalidInput | Verdict::ToolFailure) {
        return CompatVerdict::Inconclusive;
    }
    if matches!(head.verdict, Verdict::InvalidInput | Verdict::ToolFailure) {
        return CompatVerdict::Incompatible;
    }
    let changes = compare_assessments(&base.scenarios, &head.scenarios);
    let violates = changes.iter().any(|change| match fail_on {
        FailOn::Breaking => change.is_breaking(),
        FailOn::Any => change.is_regression(),
    });
    if violates {
        CompatVerdict::Incompatible
    } else {
        CompatVerdict::Compatible
    }
}

fn render(
    verdict: CompatVerdict,
    fail_on: FailOn,
    base: &AdviseResult,
    head: &AdviseResult,
    changes: &[ScenarioChange],
    output: OutputFormat,
) -> anyhow::Result<()> {
    let fail_on_str = match fail_on {
        FailOn::Breaking => "breaking",
        FailOn::Any => "any",
    };
    match output {
        OutputFormat::Table => {
            let mut out = String::new();
            let _ = writeln!(out, "compat verdict: {}", verdict.as_str());
            if changes.is_empty() {
                let _ = writeln!(out, "no scenario changed between base and head");
            } else {
                for change in changes {
                    let kind = if change.is_breaking() {
                        "breaking"
                    } else if change.is_regression() {
                        "regression"
                    } else {
                        "improvement"
                    };
                    let _ = writeln!(
                        out,
                        "  {}: {:?} -> {:?} ({kind})",
                        change.scenario_id, change.base, change.head,
                    );
                }
            }
            write_stdout_all(out.as_bytes())?;
        }
        OutputFormat::Json => {
            let report = CompatReport {
                report_version: 1,
                verdict: verdict.as_str(),
                fail_on: fail_on_str,
                base_fingerprint: base.copybook_fingerprint.clone(),
                head_fingerprint: head.copybook_fingerprint.clone(),
                changes: changes
                    .iter()
                    .map(|change| CompatChange {
                        scenario_id: change.scenario_id.clone(),
                        base: change.base,
                        head: change.head,
                        regression: change.is_regression(),
                        breaking: change.is_breaking(),
                        // Every listed change moved; non-regressions improved.
                        improvement: !change.is_regression(),
                    })
                    .collect(),
            };
            let mut rendered = serde_json::to_string_pretty(&report)
                .unwrap_or_else(|_| "{\"error\":\"json render failed\"}".to_string());
            rendered.push('\n');
            write_stdout_all(rendered.as_bytes())?;
        }
    }
    Ok(())
}
