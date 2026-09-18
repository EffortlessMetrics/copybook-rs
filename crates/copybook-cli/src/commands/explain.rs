// SPDX-License-Identifier: AGPL-3.0-or-later
//! Explain command implementation.
//!
//! Two modes share one command. Identity mode renders operator knowledge
//! for one stable error identity from the generated
//! [`copybook::error::explain`] table (source:
//! `docs/reference/ERROR_CODES.md`), so explanations can never drift from
//! the documented taxonomy. Occurrence mode (`--copybook` plus `--input`)
//! re-runs decoding to the failing record through
//! [`copybook::codec::occurrence`] and renders the strongest real record,
//! offset, field, and representation context available; anything unknown
//! stays unknown.

use crate::exit_codes::ExitCode;
use crate::write_stdout_all;
use copybook::codec::occurrence::{
    Occurrence, OccurrenceAbsence, OccurrenceOptions, OccurrenceOutcome, explain_occurrence,
};
use copybook::codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
    ZonedEncodingFormat,
};
use copybook::error::explain::explanation_for;
use std::fmt::Write as _;
use std::path::PathBuf;

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
pub enum ExplainFormat {
    Text,
    Json,
}

/// Arguments for [`run`]: identity selection plus occurrence selection.
pub struct ExplainArgs<'a> {
    /// Stable error identity: full (`CBKE501_JSON_TYPE_MISMATCH`) or short
    /// (`CBKE501`). In occurrence mode it filters to that identity.
    pub code: Option<String>,
    /// Output format.
    pub format: ExplainFormat,
    /// Copybook file path (occurrence mode).
    pub copybook: Option<PathBuf>,
    /// Input data file path (occurrence mode).
    pub input: Option<PathBuf>,
    /// 1-based record to explain (occurrence mode; default: first failure).
    pub record: Option<u64>,
    /// Record framing (occurrence mode; required with files).
    pub record_format: Option<RecordFormat>,
    /// Character encoding (occurrence mode).
    pub codepage: Codepage,
    /// Enable strict mode (occurrence mode).
    pub strict: bool,
    /// Reject `#`-style comments (occurrence mode).
    pub strict_comments: bool,
    /// Copybook dialect (occurrence mode).
    pub dialect: copybook::core::dialect::Dialect,
    /// Feature flags for parsing (occurrence mode).
    pub feature_flags: &'a copybook::core::FeatureFlags,
}

/// Explain one stable error identity, or one failure occurrence.
///
/// `code` accepts the full identity (`CBKE501_JSON_TYPE_MISMATCH`) or the
/// short code (`CBKE501`), case-insensitively. Unknown identities exit 3
/// (validation failure) with the closest guidance the CLI can offer.
pub fn run(args: &ExplainArgs<'_>) -> anyhow::Result<ExitCode> {
    match (&args.copybook, &args.input) {
        (Some(copybook), Some(input)) => run_occurrence(args, copybook, input),
        (None, None) => run_identity(args.code.as_deref(), args.format),
        _ => usage_error(),
    }
}

/// Identity mode: what one stable error identity means in general.
fn run_identity(code: Option<&str>, format: ExplainFormat) -> anyhow::Result<ExitCode> {
    let Some(code) = code else {
        return usage_error();
    };
    let Some(entry) = explanation_for(code) else {
        let mut stderr = String::new();
        let _ = writeln!(
            stderr,
            "error: unknown error identity `{code}`; expected a stable CBK* code such as CBKE501_JSON_TYPE_MISMATCH"
        );
        crate::write_stderr_all(stderr.as_bytes())?;
        return Ok(ExitCode::Encode);
    };
    let family = entry.code.get(..4).unwrap_or(entry.code);
    match format {
        ExplainFormat::Text => {
            let mut out = String::new();
            let _ = writeln!(out, "{} (family {family}, {})", entry.code, entry.severity);
            let _ = writeln!(out);
            let _ = writeln!(out, "  What: {}", entry.description);
            let _ = writeln!(out, "  Context: {}", entry.context);
            let _ = writeln!(out, "  Fix: {}", entry.resolution);
            write_stdout_all(out.as_bytes())?;
        }
        ExplainFormat::Json => {
            let value = serde_json::json!({
                "code": entry.code,
                "family": family,
                "severity": entry.severity,
                "description": entry.description,
                "context": entry.context,
                "resolution": entry.resolution,
            });
            let mut rendered = serde_json::to_string_pretty(&value)
                .unwrap_or_else(|_| "{\"error\":\"json render failed\"}".to_string());
            rendered.push('\n');
            write_stdout_all(rendered.as_bytes())?;
        }
    }
    Ok(ExitCode::Ok)
}

/// Usage error shared by both modes.
fn usage_error() -> anyhow::Result<ExitCode> {
    let mut stderr = String::new();
    let _ = writeln!(
        stderr,
        "error: explain needs an identity (`copybook explain CBKE501`) or an occurrence (`copybook explain --copybook <CPY> --input <DATA> --record-format <F>`)"
    );
    crate::write_stderr_all(stderr.as_bytes())?;
    Ok(ExitCode::Encode)
}

/// Occurrence mode: why a failure in this file happened here.
#[allow(clippy::too_many_lines)]
fn run_occurrence(
    args: &ExplainArgs<'_>,
    copybook: &std::path::Path,
    input: &std::path::Path,
) -> anyhow::Result<ExitCode> {
    let Some(record_format) = args.record_format else {
        let mut stderr = String::new();
        let _ = writeln!(
            stderr,
            "error: occurrence mode needs --record-format <fixed|rdw|vb> (explicit, no auto-detection)"
        );
        crate::write_stderr_all(stderr.as_bytes())?;
        return Ok(ExitCode::Encode);
    };
    if args.record == Some(0) {
        let mut stderr = String::new();
        let _ = writeln!(
            stderr,
            "error: --record is 1-based; record 0 does not exist"
        );
        crate::write_stderr_all(stderr.as_bytes())?;
        return Ok(ExitCode::Encode);
    }
    // A caller-supplied identity filters the search; resolve it now so an
    // unknown filter fails the same way identity mode fails.
    let code_filter = match args.code.as_deref() {
        None => None,
        Some(code) => {
            if let Some(entry) = explanation_for(code) {
                Some(entry.code.to_string())
            } else {
                let mut stderr = String::new();
                let _ = writeln!(
                    stderr,
                    "error: unknown error identity `{code}`; expected a stable CBK* code such as CBKE501_JSON_TYPE_MISMATCH"
                );
                crate::write_stderr_all(stderr.as_bytes())?;
                return Ok(ExitCode::Encode);
            }
        }
    };

    let copybook_text = std::fs::read_to_string(copybook)?;
    let parse_options = crate::utils::build_parse_options(&crate::utils::ParseOptionsConfig {
        strict: args.strict,
        strict_comments: args.strict_comments,
        codepage: &args.codepage.to_string(),
        emit_filler: false,
        dialect: args.dialect,
    });
    let schema = copybook::core::parse_copybook_with_feature_flags(
        &copybook_text,
        &parse_options,
        args.feature_flags,
    )?;
    let decode_options = DecodeOptions::new()
        .with_format(record_format)
        .with_codepage(args.codepage)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(args.strict)
        .with_unmappable_policy(UnmappablePolicy::Error)
        .with_threads(1)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Auto);
    let file = std::fs::File::open(input)?;
    let outcome = explain_occurrence(
        file,
        &schema,
        &decode_options,
        &OccurrenceOptions {
            format: record_format,
            codepage: args.codepage,
            target_record: args.record,
            code_filter: code_filter.clone(),
        },
    )?;

    match outcome {
        OccurrenceOutcome::Found(occurrence) => {
            render_occurrence(&occurrence, args, copybook, input)
        }
        OccurrenceOutcome::Absent(absence) => {
            render_absence(&absence, code_filter.as_deref(), args.format)
        }
    }
}

/// Render the occurrence card: identity header, strongest real context,
/// then failure, fix, and next action. Unknown slots say so.
fn render_occurrence(
    occurrence: &Occurrence,
    args: &ExplainArgs<'_>,
    copybook: &std::path::Path,
    input: &std::path::Path,
) -> anyhow::Result<ExitCode> {
    let Some(entry) = explanation_for(&occurrence.code) else {
        return run_identity(Some(&occurrence.code), args.format);
    };
    let family = entry.code.get(..4).unwrap_or(entry.code);
    let field = occurrence.field_path.as_deref().unwrap_or("unknown");
    match args.format {
        ExplainFormat::Text => {
            let mut out = String::new();
            let _ = writeln!(out, "{} (family {family}, {})", entry.code, entry.severity);
            let _ = writeln!(out);
            let _ = writeln!(
                out,
                "Record {} (scanned {})",
                occurrence.record_index, occurrence.records_scanned
            );
            match occurrence.physical_offset {
                Some(offset) => {
                    let _ = writeln!(out, "Physical offset: {offset}");
                }
                None => {
                    let _ = writeln!(out, "Physical offset: unknown");
                }
            }
            let _ = writeln!(out, "Field: {field}");
            if let Some((start, len)) = occurrence.field_range {
                let _ = writeln!(out, "Field bytes: {start}..{}", start + len);
            }
            if let Some(representation) = occurrence.representation.as_deref() {
                let _ = writeln!(out, "Representation: {representation}");
            }
            let _ = writeln!(
                out,
                "Format: {}, Codepage: {}",
                format!("{:?}", occurrence.format).to_lowercase(),
                occurrence.codepage,
            );
            let _ = writeln!(out, "  What: {}", entry.description);
            let _ = writeln!(out, "  Failure: {}", occurrence.message);
            let _ = writeln!(out, "  Fix: {}", entry.resolution);
            let _ = writeln!(
                out,
                "  Next: copybook doctor {} {} --format {} --codepage {}",
                copybook.display(),
                input.display(),
                format!("{:?}", occurrence.format).to_lowercase(),
                occurrence.codepage,
            );
            write_stdout_all(out.as_bytes())?;
        }
        ExplainFormat::Json => {
            let value = serde_json::json!({
                "code": entry.code,
                "family": family,
                "severity": entry.severity,
                "description": entry.description,
                "context": entry.context,
                "resolution": entry.resolution,
                "failure": occurrence.message,
                "occurrence": {
                    "record": occurrence.record_index,
                    "records_scanned": occurrence.records_scanned,
                    "physical_offset": occurrence.physical_offset,
                    "field": occurrence.field_path,
                    "field_bytes": occurrence.field_range.map(|(start, len)| serde_json::json!([start, start + len])),
                    "representation": occurrence.representation,
                    "format": format!("{:?}", occurrence.format).to_lowercase(),
                    "codepage": occurrence.codepage.to_string(),
                },
            });
            let mut rendered = serde_json::to_string_pretty(&value)
                .unwrap_or_else(|_| "{\"error\":\"json render failed\"}".to_string());
            rendered.push('\n');
            write_stdout_all(rendered.as_bytes())?;
        }
    }
    Ok(ExitCode::Ok)
}

/// Render a no-failure outcome with its reason. When a code filter
/// excluded real failures, name what was seen so the operator can retry
/// without the filter instead of concluding the file is clean.
fn render_absence(
    absence: &OccurrenceAbsence,
    code_filter: Option<&str>,
    format: ExplainFormat,
) -> anyhow::Result<ExitCode> {
    let message = match absence {
        OccurrenceAbsence::CleanRecord { record } => {
            format!("Record {record} decoded without errors.")
        }
        OccurrenceAbsence::BeyondEnd { requested, present } => {
            format!("Record {requested} does not exist; the input holds {present} record(s).")
        }
        OccurrenceAbsence::NotFound {
            scanned,
            limit,
            seen,
        } => {
            let base = match limit {
                Some(limit) => format!(
                    "No matching failure in the first {scanned} record(s); scan stopped at the {limit}-record scope bound."
                ),
                None => format!("No matching failure in the {scanned} record(s) scanned."),
            };
            if seen.is_empty() {
                base
            } else {
                let filter = code_filter.unwrap_or("requested");
                format!(
                    "No {filter} failure in the {scanned} record(s) scanned; failures seen with other identities: {}.",
                    seen.join(", ")
                )
            }
        }
    };
    let failing = matches!(
        absence,
        OccurrenceAbsence::BeyondEnd { .. } | OccurrenceAbsence::NotFound { limit: Some(_), .. }
    );
    match format {
        ExplainFormat::Text => {
            let mut out = String::new();
            let _ = writeln!(out, "{message}");
            write_stdout_all(out.as_bytes())?;
        }
        ExplainFormat::Json => {
            let value = serde_json::json!({ "verdict": if failing { "inconclusive" } else { "clean" }, "message": message });
            let mut rendered = serde_json::to_string_pretty(&value)
                .unwrap_or_else(|_| "{\"error\":\"json render failed\"}".to_string());
            rendered.push('\n');
            write_stdout_all(rendered.as_bytes())?;
        }
    }
    Ok(if failing {
        ExitCode::Encode
    } else {
        ExitCode::Ok
    })
}
