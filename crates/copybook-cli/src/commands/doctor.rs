// SPDX-License-Identifier: AGPL-3.0-or-later
//! Doctor command implementation: thin orchestration over the diagnosis
//! domain.
//!
//! File loading and output rendering live here. The staged diagnosis
//! itself (parse, record length, framing and codepage probes, trial
//! decode) belongs to [`copybook::codec::diagnose`], so human text and
//! JSON render the same typed result.

use crate::exit_codes::ExitCode;
use crate::write_stdout_all;
use copybook::codec::diagnose::{
    DiagnoseOptions, Diagnosis, DiagnosisEvidence, DiagnosisFinding, DiagnosisInput,
    DiagnosisStatus, diagnose, remediation_for,
};
use copybook::codec::{Codepage, RecordFormat};
use copybook::core::Dialect;
use std::fmt::Write as _;
use std::io::Read as _;
use std::path::{Path, PathBuf};

/// Diagnosis never ingests a whole extract: probes see this many leading
/// bytes at most, while size-based checks use file metadata. The inspected
/// scope is part of the rendered result.
const INPUT_PREFIX_CAP: u64 = 1_048_576;

/// Diagnose a copybook and optional data file.
///
/// Doctor speaks through findings, never through speculative probe logs:
/// trial framings and losing codepages necessarily warn inside the library
/// primitives they reuse, and rejected hypotheses are not diagnoses. Unless
/// `verbose` was requested, probes run under an error-only scoped
/// dispatcher; genuine errors still reach stderr, and every outcome of note
/// is rendered as a finding with its stable identity.
#[allow(clippy::too_many_arguments)]
pub fn run(
    copybook: &Path,
    input: Option<PathBuf>,
    format: Option<RecordFormat>,
    codepage: Option<Codepage>,
    sample: u32,
    json: bool,
    strict_comments: bool,
    dialect: crate::DialectPreference,
    verbose: bool,
    emit_profile: Option<PathBuf>,
    dialect_flag: Option<crate::DialectPreference>,
) -> anyhow::Result<ExitCode> {
    if verbose {
        return run_inner(
            copybook,
            input,
            format,
            codepage,
            sample,
            json,
            strict_comments,
            dialect,
            emit_profile,
            dialect_flag,
        );
    }
    let quiet = tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::new("error"))
        .with_ansi(false)
        .with_writer(std::io::sink)
        .finish();
    tracing::dispatcher::with_default(&tracing::dispatcher::Dispatch::new(quiet), || {
        run_inner(
            copybook,
            input,
            format,
            codepage,
            sample,
            json,
            strict_comments,
            dialect,
            emit_profile,
            dialect_flag,
        )
    })
}

#[allow(clippy::too_many_arguments)]
fn run_inner(
    copybook: &Path,
    input: Option<PathBuf>,
    format: Option<RecordFormat>,
    codepage: Option<Codepage>,
    sample: u32,
    json: bool,
    strict_comments: bool,
    dialect: crate::DialectPreference,
    emit_profile: Option<PathBuf>,
    dialect_flag: Option<crate::DialectPreference>,
) -> anyhow::Result<ExitCode> {
    let core_dialect = match dialect {
        crate::DialectPreference::N => Dialect::Normative,
        crate::DialectPreference::Zero => Dialect::ZeroTolerant,
        crate::DialectPreference::One => Dialect::OneTolerant,
    };

    let copybook_text = match std::fs::read_to_string(copybook) {
        Ok(text) => text,
        Err(error) => {
            let diagnosis = Diagnosis {
                findings: vec![DiagnosisFinding {
                    check: "copybook-load",
                    status: DiagnosisStatus::Fail,
                    detail: format!("cannot read {}: {error}", copybook.display()),
                    code: Some("CBKF001_FILE_READ_ERROR".to_string()),
                    remediation: remediation_for("CBKF001_FILE_READ_ERROR"),
                    next: None,
                }],
                evidence: DiagnosisEvidence::default(),
            };
            return finish(&diagnosis.findings, json);
        }
    };
    // Successful load is itself a finding, recorded before diagnosis.
    let mut preload = Diagnosis {
        findings: vec![DiagnosisFinding {
            check: "copybook-load",
            status: DiagnosisStatus::Pass,
            detail: format!("read {} bytes", copybook_text.len()),
            code: None,
            remediation: String::new(),
            next: None,
        }],
        evidence: DiagnosisEvidence::default(),
    };

    let input_bytes = match input {
        None => None,
        Some(path) => match read_input_prefix(&path) {
            Ok((total_bytes, prefix)) => Some((path, total_bytes, prefix)),
            Err(error) => {
                preload.findings.push(DiagnosisFinding {
                    check: "input-load",
                    status: DiagnosisStatus::Fail,
                    detail: format!("cannot read {}: {error}", path.display()),
                    code: Some("CBKF001_FILE_READ_ERROR".to_string()),
                    remediation: remediation_for("CBKF001_FILE_READ_ERROR"),
                    next: None,
                });
                return finish(&preload.findings, json);
            }
        },
    };
    let input_view = input_bytes
        .as_ref()
        .map(|(path, total_bytes, prefix)| DiagnosisInput {
            path: path.as_path(),
            prefix: prefix.as_slice(),
            total_bytes: *total_bytes,
        });

    let options = DiagnoseOptions {
        format,
        codepage,
        sample,
        strict_comments,
        dialect: core_dialect,
    };
    let mut diagnosis = diagnose(&copybook_text, copybook, input_view, &options);
    let mut emit_failed = false;
    if let Some(path) = emit_profile {
        emit_failed = !emit_drafted_profile(&diagnosis, dialect_flag, &path);
    }
    preload.findings.append(&mut diagnosis.findings);
    let exit = finish(&preload.findings, json)?;
    if emit_failed && exit == ExitCode::Ok {
        // A failed profile write is an orchestration error, not a record
        // diagnosis: escalate the way unclassifiable failures do.
        return Ok(ExitCode::Internal);
    }
    Ok(exit)
}

/// Draft an interpretation profile from a healthy diagnosis and write it.
///
/// Returns whether a profile was written. Refusals (failing findings or
/// unestablished framing/codepage) and write errors report to stderr and
/// return `false`; the diagnosis exit code is left to [`finish`], except
/// that a failed write escalates a healthy verdict (see [`run_inner`]).
fn emit_drafted_profile(
    diagnosis: &Diagnosis,
    dialect_flag: Option<crate::DialectPreference>,
    path: &Path,
) -> bool {
    if diagnosis.has_failures() {
        eprintln!("profile not emitted: diagnosis has failures; fix them and rerun");
        return false;
    }
    let Some(drafted) = crate::profile_generate::assemble(&diagnosis.evidence, dialect_flag) else {
        eprintln!("profile not emitted: framing or codepage was never established");
        return false;
    };
    let rendered = crate::profile_generate::render(&drafted);
    match std::fs::write(path, rendered) {
        Ok(()) => {
            if drafted.needs_review {
                eprintln!(
                    "profile drafted: {} ({} key(s) need review)",
                    path.display(),
                    drafted
                        .notes
                        .iter()
                        .filter(|note| note.starts_with("REVIEW"))
                        .count()
                );
            } else {
                eprintln!("profile drafted: {} (fully pinned)", path.display());
            }
            true
        }
        Err(error) => {
            eprintln!(
                "profile not emitted: cannot write {}: {error}",
                path.display()
            );
            false
        }
    }
}

/// Read file metadata plus a bounded leading prefix: diagnosis probes the
/// prefix while size-based checks use the metadata length.
fn read_input_prefix(path: &Path) -> anyhow::Result<(u64, Vec<u8>)> {
    let total_bytes = std::fs::metadata(path)?.len();
    let mut file = std::fs::File::open(path)?;
    let mut prefix = Vec::new();
    file.by_ref()
        .take(INPUT_PREFIX_CAP)
        .read_to_end(&mut prefix)?;
    Ok((total_bytes, prefix))
}

/// Render findings and map the worst failure to its taxonomy exit code.
fn finish(findings: &[DiagnosisFinding], json: bool) -> anyhow::Result<ExitCode> {
    let mut exit = ExitCode::Ok;
    for finding in findings {
        if finding.status != DiagnosisStatus::Fail {
            continue;
        }
        let mapped = finding
            .code
            .as_deref()
            .and_then(|code| code.get(..4))
            .and_then(ExitCode::from_family_prefix)
            .unwrap_or(ExitCode::Format);
        if (mapped as i32) > (exit as i32) {
            exit = mapped;
        }
    }
    if json {
        let value = serde_json::json!({
            "verdict": if exit == ExitCode::Ok { "healthy" } else { "needs-attention" },
            "findings": findings.iter().map(|finding| serde_json::json!({
                "check": finding.check,
                "status": finding.status.as_str(),
                "detail": finding.detail,
                "code": finding.code,
                "remediation": finding.remediation,
                "next": finding.next,
            })).collect::<Vec<_>>(),
        });
        let mut rendered = serde_json::to_string_pretty(&value)
            .unwrap_or_else(|_| "{\"error\":\"json render failed\"}".to_string());
        rendered.push('\n');
        write_stdout_all(rendered.as_bytes())?;
    } else {
        let mut out = String::new();
        let _ = writeln!(
            out,
            "doctor verdict: {}",
            if exit == ExitCode::Ok {
                "healthy"
            } else {
                "needs attention"
            }
        );
        for finding in findings {
            let _ = writeln!(
                out,
                "  [{}] {}: {}",
                finding.status.as_str(),
                finding.check,
                finding.detail,
            );
            if let Some(code) = &finding.code {
                let _ = writeln!(out, "    code: {code}");
            }
            if !finding.remediation.is_empty() {
                let _ = writeln!(out, "    fix: {}", finding.remediation);
            }
            if let Some(next) = &finding.next {
                let _ = writeln!(out, "    next: {next}");
            }
        }
        write_stdout_all(out.as_bytes())?;
    }
    Ok(exit)
}
