// SPDX-License-Identifier: AGPL-3.0-or-later
//! `support --advise` orchestration (Slice C).
//!
//! Thin by design: read the copybook, parse it, extract the construct
//! inventory, run [`analyze`], and project the same [`AdviseResult`] value
//! through the human and JSON renderers.
//! Verdict content lives in the domain; this module adds no verdict of its
//! own. Exit mapping mirrors `support --check`: only `Supported` exits 0,
//! any other verdict exits `Encode`, and hard tool failures propagate as
//! typed errors through the existing exit-code mapping.

use crate::commands::support::OutputFormat;
use crate::exit_codes::ExitCode;
use copybook::core::{Field, FieldKind, Occurs, Schema, parse_copybook};
use copybook::support_matrix::advise::{
    AdviseConstruct, AdviseInput, AdviseResult, ConstructKind, EffectiveOptions, Verdict, analyze,
};
use std::fmt::Write as _;
use std::path::Path;

/// Run advisory analysis for one copybook under the requested options.
///
/// The dialect lever selects layout interpretation before extraction, so the
/// evaluated dialect is the analyzed dialect, not a label.
pub fn run_advise(
    copybook: &Path,
    format: &str,
    codepage: &str,
    dialect: Option<crate::cli_config::DialectPreference>,
    output: OutputFormat,
) -> anyhow::Result<ExitCode> {
    use crate::cli_config::DialectPreference;
    let core_dialect = match dialect {
        None | Some(DialectPreference::N) => copybook::core::Dialect::Normative,
        Some(DialectPreference::Zero) => copybook::core::Dialect::ZeroTolerant,
        Some(DialectPreference::One) => copybook::core::Dialect::OneTolerant,
    };
    let dialect_str = match core_dialect {
        copybook::core::Dialect::Normative => "normative",
        copybook::core::Dialect::ZeroTolerant => "zero-tolerant",
        copybook::core::Dialect::OneTolerant => "one-tolerant",
    };
    let source = std::fs::read_to_string(copybook).map_err(|error| {
        copybook::core::Error::new(
            copybook::core::ErrorCode::CBKF001_FILE_READ_ERROR,
            format!("Cannot read copybook {}: {error}", copybook.display()),
        )
    })?;
    let source_fingerprint = sha256_hex(source.as_bytes());

    // The typed error code travels beside the prose diagnostic (#979): the
    // machine field carries `error.code()` (never regex over `Display`),
    // while prose stays human-readable and free to change.
    let (schema, parse_error, parse_error_identity) = match parse_copybook(&source) {
        Ok(mut schema) => {
            // Layout resolution interprets ODO placement under the dialect;
            // a layout failure is malformed input, never a tool failure.
            match copybook::core::layout::resolve_layout(&mut schema, core_dialect) {
                Ok(()) => {
                    schema.calculate_fingerprint();
                    (Some(schema), None, None)
                }
                Err(error) => (
                    None,
                    Some(error.to_string()),
                    Some(error.code().to_string()),
                ),
            }
        }
        Err(error) => (
            None,
            Some(error.to_string()),
            Some(error.code().to_string()),
        ),
    };

    // Redaction precondition (`AdviseInput::bounded`): construct detail carries
    // schema field paths only, and the parse error is an in-memory diagnostic
    // string; neither ever carries filesystem paths or record payload. The
    // identity is a stable `CBK*` code, likewise free of paths and payload.
    let constructs = schema.as_ref().map(extract_constructs).unwrap_or_default();
    let mut input = AdviseInput::bounded(
        constructs,
        parse_error,
        EffectiveOptions::bounded(format, codepage, dialect_str),
        tool_version(),
    );
    if let Some(identity) = parse_error_identity {
        input = input.with_parse_error_identity(identity);
    }
    input.source_fingerprint = Some(source_fingerprint);
    if let Some(schema) = &schema {
        input.copybook_fingerprint = Some(schema.fingerprint.clone());
    }
    let result = analyze(&input);

    match output {
        OutputFormat::Table => {
            let mut out = String::new();
            render_human(&result, &mut out);
            super::support::write_stdout(&out);
        }
        OutputFormat::Json => {
            let mut out = result.to_canonical_json();
            out.push('\n');
            super::support::write_stdout(&out);
        }
    }

    Ok(match result.verdict {
        Verdict::Supported => ExitCode::Ok,
        // Forward-compatible: any other (or future) verdict never exits
        // success silently.
        _ => ExitCode::Encode,
    })
}

/// Extract the construct inventory from a parsed schema.
fn extract_constructs(schema: &Schema) -> Vec<AdviseConstruct> {
    let mut constructs = Vec::new();
    let tail_array = schema
        .tail_odo
        .as_ref()
        .map(|tail| tail.array_path.as_str());
    for field in &schema.fields {
        walk_field(field, false, tail_array, &mut constructs);
    }
    constructs
}

/// Walk one field, tracking whether an ancestor carries `OCCURS`.
fn walk_field(
    field: &Field,
    in_occurs: bool,
    tail_array: Option<&str>,
    constructs: &mut Vec<AdviseConstruct>,
) {
    let mut nested_occurs = in_occurs;
    if let Some(Occurs::ODO { .. }) = &field.occurs {
        // tail_odo.array_path stores the bare field name (see
        // layout::detect_tail_odo), not the dotted path.
        let kind = if Some(field.name.as_str()) == tail_array {
            ConstructKind::OccursDepending
        } else if in_occurs {
            ConstructKind::NestedOdo
        } else {
            ConstructKind::NonTailOdo
        };
        constructs.push(AdviseConstruct::bounded(kind, field.path.clone(), None));
        nested_occurs = true;
    }
    if field.redefines_of.is_some() {
        constructs.push(AdviseConstruct::bounded(
            ConstructKind::Redefines,
            field.path.clone(),
            None,
        ));
    }
    match &field.kind {
        FieldKind::Condition { .. } => constructs.push(AdviseConstruct::bounded(
            ConstructKind::Level88,
            field.path.clone(),
            None,
        )),
        FieldKind::Renames { .. } => constructs.push(AdviseConstruct::bounded(
            ConstructKind::Renames,
            field.path.clone(),
            None,
        )),
        FieldKind::EditedNumeric { .. } => constructs.push(AdviseConstruct::bounded(
            ConstructKind::EditedPic,
            field.path.clone(),
            None,
        )),
        FieldKind::FloatSingle | FieldKind::FloatDouble => {
            constructs.push(AdviseConstruct::bounded(
                ConstructKind::Comp1Comp2,
                field.path.clone(),
                None,
            ));
        }
        FieldKind::ZonedDecimal { sign_separate, .. } if sign_separate.is_some() => {
            constructs.push(AdviseConstruct::bounded(
                ConstructKind::SignSeparate,
                field.path.clone(),
                None,
            ));
        }
        _ => {}
    }
    for child in &field.children {
        walk_field(child, nested_occurs, tail_array, constructs);
    }
}

/// Fixed-width lowercase hex SHA-256 (matches the `^[a-f0-9]{64}$` contract).
fn sha256_hex(bytes: &[u8]) -> String {
    use sha2::{Digest as _, Sha256};
    let digest = Sha256::digest(bytes);
    let mut hex = String::with_capacity(64);
    for byte in digest {
        let _ = write!(hex, "{byte:02x}");
    }
    hex
}

/// Producing tool identity.
fn tool_version() -> String {
    format!("copybook {}", env!("CARGO_PKG_VERSION"))
}

/// Human projection of an advisory result. Same value as the JSON
/// projection; adds no verdict content.
fn render_human(result: &AdviseResult, out: &mut String) {
    let _ = writeln!(out, "Advisory verdict: {}", verdict_str(result.verdict));
    let _ = writeln!(
        out,
        "Contract: {} ({})",
        result.schema_version, result.stability_class
    );
    let _ = writeln!(
        out,
        "Evaluated: format={} codepage={} dialect={}",
        result.effective_options.format,
        result.effective_options.codepage,
        result.effective_options.dialect
    );
    if let Some(fingerprint) = &result.copybook_fingerprint {
        let _ = writeln!(out, "Copybook fingerprint: {fingerprint}");
    }
    for scenario in &result.scenarios {
        let _ = writeln!(
            out,
            "- [{}] {}",
            status_str(scenario.status),
            scenario.scenario_id
        );
        if !scenario.limitation_or_remediation.is_empty() {
            let _ = writeln!(out, "    note: {}", scenario.limitation_or_remediation);
        }
        if let Some(error) = &scenario.error_identity {
            let _ = writeln!(out, "    identity: {error}");
        }
        if !scenario.next_action.is_empty() {
            let _ = writeln!(out, "    next: {}", scenario.next_action);
        }
    }
    if result.truncation.scenarios_considered != result.truncation.scenarios_reported {
        let _ = writeln!(
            out,
            "Truncated: {}/{} scenarios reported ({} evidence refs dropped)",
            result.truncation.scenarios_reported,
            result.truncation.scenarios_considered,
            result.truncation.evidence_dropped
        );
    }
    if !result.redaction.is_log_safe() {
        let _ = writeln!(out, "Warning: redaction posture is not log-safe");
    }
}

/// Canonical kebab-case verdict spelling, matching the JSON contract.
fn verdict_str(verdict: Verdict) -> &'static str {
    match verdict {
        Verdict::Supported => "supported",
        Verdict::SupportedWithLimits => "supported-with-limits",
        Verdict::Beta => "beta",
        Verdict::PartialUnknown => "partial-unknown",
        Verdict::Rejected => "rejected",
        Verdict::InvalidInput => "invalid-input",
        Verdict::ToolFailure => "tool-failure",
        // Forward-compatible: never render an unknown verdict as certainty.
        _ => "unknown",
    }
}

/// Canonical kebab-case outcome spelling.
fn status_str(status: copybook::support_matrix::advise::AssessmentStatus) -> &'static str {
    use copybook::support_matrix::advise::AssessmentStatus;
    match status {
        AssessmentStatus::Supported => "supported",
        AssessmentStatus::Limited => "limited",
        AssessmentStatus::Beta => "beta",
        AssessmentStatus::Rejected => "rejected",
        AssessmentStatus::Invalid => "invalid",
        AssessmentStatus::ToolFailure => "tool-failure",
        AssessmentStatus::NotApplicable => "not-applicable",
        // Forward-compatible: never render an unknown outcome as certainty.
        _ => "unknown",
    }
}
