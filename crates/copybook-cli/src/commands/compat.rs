// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compat command implementation.
//!
//! Compares two copybooks (base vs head) under identical effective options
//! and reports two change dimensions: scenario-level change (what newly
//! passes, what newly fails) and resolved-schema change (what moved in the
//! record layout). The comparison domains ([`compare_assessments`], ranks,
//! breaking rules, [`diff_schemas`]) live in libraries; this module only
//! renders the verdict and maps it to the process exit code, so CI can
//! reject breaking copybook changes with `--fail-on`.

use crate::commands::support::OutputFormat;
use crate::exit_codes::ExitCode;
use crate::write_stdout_all;
use copybook::core::schema_diff::{FieldChange, FieldChangeKind, SchemaDiff, diff_schemas};
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
    schema_breaking: bool,
    schema_changes: Vec<CompatSchemaChange>,
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

/// One resolved-schema change in report shape.
#[derive(Serialize)]
struct CompatSchemaChange {
    field: String,
    kind: &'static str,
    breaking: bool,
    base_offset: Option<u32>,
    base_len: Option<u32>,
    head_offset: Option<u32>,
    head_len: Option<u32>,
}

impl CompatSchemaChange {
    fn render(change: &FieldChange) -> Self {
        let kind = match change.kind {
            FieldChangeKind::Added => "added",
            FieldChangeKind::Removed => "removed",
            FieldChangeKind::Changed => "changed",
        };
        Self {
            field: change.path.clone(),
            kind,
            breaking: !matches!(change.kind, FieldChangeKind::Added),
            base_offset: change.base.as_ref().map(|layout| layout.offset),
            base_len: change.base.as_ref().map(|layout| layout.len),
            head_offset: change.head.as_ref().map(|layout| layout.offset),
            head_len: change.head.as_ref().map(|layout| layout.len),
        }
    }
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

    // Resolved-schema comparison judges the record layout both sides
    // established. Either side failing to resolve means there is no layout
    // to compare, which is inconclusive, never compatible.
    let schema_diff = match (
        super::support_advise::parse_resolved_schema(base, dialect),
        super::support_advise::parse_resolved_schema(head, dialect),
    ) {
        (Ok(base_schema), Ok(head_schema)) => Some(diff_schemas(&base_schema, &head_schema)),
        _ => None,
    };

    let verdict = decide(&base_result, &head_result, schema_diff.as_ref(), fail_on);
    let changes = compare_assessments(&base_result.scenarios, &head_result.scenarios);
    render(
        verdict,
        fail_on,
        &base_result,
        &head_result,
        &changes,
        schema_diff.as_ref(),
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

fn decide(
    base: &AdviseResult,
    head: &AdviseResult,
    schema_diff: Option<&SchemaDiff>,
    fail_on: FailOn,
) -> CompatVerdict {
    if matches!(base.verdict, Verdict::InvalidInput | Verdict::ToolFailure) {
        return CompatVerdict::Inconclusive;
    }
    if matches!(head.verdict, Verdict::InvalidInput | Verdict::ToolFailure) {
        return CompatVerdict::Incompatible;
    }
    // No comparable layout is inconclusive, never compatible: without a
    // resolved record on both sides there is nothing to judge.
    let Some(schema_diff) = schema_diff else {
        return CompatVerdict::Inconclusive;
    };
    // A layout break refuses bytes the base accepted, under every policy.
    if schema_diff.is_breaking() {
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
    schema_diff: Option<&SchemaDiff>,
    output: OutputFormat,
) -> anyhow::Result<()> {
    let fail_on_str = match fail_on {
        FailOn::Breaking => "breaking",
        FailOn::Any => "any",
    };
    let schema_changes: Vec<CompatSchemaChange> = schema_diff
        .map(|diff| {
            diff.changes
                .iter()
                .map(CompatSchemaChange::render)
                .collect()
        })
        .unwrap_or_default();
    let schema_breaking = schema_diff.is_some_and(SchemaDiff::is_breaking);
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
            match schema_diff {
                None => {
                    let _ = writeln!(out, "no comparable record layout");
                }
                Some(diff) if diff.changes.is_empty() && !diff.lrecl_changed() => {
                    let _ = writeln!(out, "record layout unchanged");
                }
                Some(diff) => {
                    if diff.lrecl_changed() {
                        let _ = writeln!(
                            out,
                            "  record length: {:?} -> {:?} (breaking)",
                            diff.base_lrecl, diff.head_lrecl,
                        );
                    }
                    for change in &schema_changes {
                        let kind = if change.breaking { "breaking" } else { "added" };
                        let _ = writeln!(
                            out,
                            "  field {}: {:?} -> {:?} bytes at {:?} -> {:?} ({kind})",
                            change.field,
                            change.base_len,
                            change.head_len,
                            change.base_offset,
                            change.head_offset,
                        );
                    }
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
                schema_breaking,
                schema_changes,
            };
            let mut rendered = serde_json::to_string_pretty(&report)
                .unwrap_or_else(|_| "{\"error\":\"json render failed\"}".to_string());
            rendered.push('\n');
            write_stdout_all(rendered.as_bytes())?;
        }
    }
    Ok(())
}
