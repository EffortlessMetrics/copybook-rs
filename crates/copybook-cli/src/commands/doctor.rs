// SPDX-License-Identifier: AGPL-3.0-or-later
//! Doctor command implementation.
//!
//! Diagnoses a copybook plus (optionally) a data file the way a frustrated
//! operator would: does the copybook parse, what record length resolved,
//! which framing the bytes look like, which codepage they decode under,
//! and whether the first records actually decode. Every probe reuses a
//! library primitive (framing readers, charset conversion, trial decode)
//! and every failure names its stable error identity with remediation from
//! the shared explain table, plus the exact next command to run.
//!
//! Probes that guess (format, codepage) always state their confidence and
//! their evidence window; a guess is never rendered as certainty.

use crate::exit_codes::ExitCode;
use crate::write_stdout_all;
use copybook::charset::ebcdic_to_utf8;
use copybook::codec::{Codepage, DecodeOptions, RecordFormat, UnmappablePolicy, decode_record};
use copybook::error::explain::explanation_for;
use copybook::framing::rdw::{RDWRecordReader, diagnostics::rdw_is_suspect_ascii_corruption_slice};
use std::fmt::Write as _;
use std::io::Cursor;
use std::path::{Path, PathBuf};

/// Bytes of input scored by the codepage probe.
const CODEPAGE_PROBE_WINDOW: usize = 4096;

/// Minimum score margin for a high-confidence codepage call.
const CODEPAGE_CONFIDENCE_MARGIN: f64 = 0.15;

/// Maximum characters kept from an error rendering inside a finding.
const MAX_DETAIL_CHARS: usize = 300;

/// Check outcome. Warnings never fail the run; failures map to the
/// taxonomy exit code of their stable identity (or `Format` without one).
#[derive(Clone, Copy, PartialEq, Eq)]
enum Status {
    Pass,
    Warn,
    Fail,
}

impl Status {
    fn as_str(self) -> &'static str {
        match self {
            Status::Pass => "pass",
            Status::Warn => "warn",
            Status::Fail => "fail",
        }
    }
}

struct Finding {
    check: &'static str,
    status: Status,
    detail: String,
    code: Option<String>,
    remediation: String,
    next: Option<String>,
}

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
) -> anyhow::Result<ExitCode> {
    let mut findings: Vec<Finding> = Vec::new();

    let Some(copybook_text) = load_copybook_text(&mut findings, copybook) else {
        return finish(&findings, json);
    };

    let schema = match parse_schema(&copybook_text, strict_comments, dialect) {
        Ok(schema) => {
            push(
                &mut findings,
                Finding {
                    check: "copybook-parse",
                    status: Status::Pass,
                    detail: "copybook parses".to_string(),
                    code: None,
                    remediation: String::new(),
                    next: None,
                },
            );
            schema
        }
        Err((code, detail)) => {
            push(
                &mut findings,
                Finding {
                    check: "copybook-parse",
                    status: Status::Fail,
                    detail,
                    code: Some(code.clone()),
                    remediation: remediation_for(&code),
                    next: Some(format!("copybook parse {}", copybook.display())),
                },
            );
            return finish(&findings, json);
        }
    };

    let lrecl = schema.lrecl_fixed;
    push(
        &mut findings,
        Finding {
            check: "record-length",
            status: Status::Pass,
            detail: match lrecl {
                Some(len) => format!("fixed record length {len} bytes"),
                None => "variable record length (ODO); fixed-size checks do not apply".to_string(),
            },
            code: None,
            remediation: String::new(),
            next: None,
        },
    );

    let Some(input_path) = input else {
        push(
            &mut findings,
            Finding {
                check: "input",
                status: Status::Pass,
                detail: "no data file given; copybook-only diagnosis".to_string(),
                code: None,
                remediation: String::new(),
                next: Some(format!("copybook doctor {} <DATA>", copybook.display())),
            },
        );
        return finish(&findings, json);
    };

    let Some(bytes) = load_input_bytes(&mut findings, &input_path) else {
        return finish(&findings, json);
    };

    // Format: confirm the explicit choice or probe both framings.
    let resolved_format = match format {
        Some(given) => {
            confirm_format(&mut findings, given, &bytes, lrecl);
            Some(given)
        }
        None => probe_format(&mut findings, &bytes, lrecl),
    };
    let Some(resolved_format) = resolved_format else {
        return finish(&findings, json);
    };

    // Codepage: confirm the explicit choice implicitly via trial decode, or
    // probe all six codepages over a byte window and state the confidence.
    let resolved_codepage = match codepage {
        Some(given) => given,
        None => probe_codepage(&mut findings, &bytes),
    };

    trial_decode(
        &mut findings,
        &schema,
        copybook,
        &input_path,
        &bytes,
        resolved_format,
        resolved_codepage,
        lrecl,
        sample,
    );
    finish(&findings, json)
}

fn push(findings: &mut Vec<Finding>, finding: Finding) {
    findings.push(finding);
}

/// Load the copybook text, recording a pass or fail finding. Returns
/// `None` when diagnosis cannot continue past this stage.
fn load_copybook_text(findings: &mut Vec<Finding>, copybook: &Path) -> Option<String> {
    match std::fs::read_to_string(copybook) {
        Ok(text) => {
            push(
                &mut *findings,
                Finding {
                    check: "copybook-load",
                    status: Status::Pass,
                    detail: format!("read {} bytes", text.len()),
                    code: None,
                    remediation: String::new(),
                    next: None,
                },
            );
            Some(text)
        }
        Err(error) => {
            push(
                &mut *findings,
                Finding {
                    check: "copybook-load",
                    status: Status::Fail,
                    detail: format!("cannot read {}: {error}", copybook.display()),
                    code: Some("CBKF001_FILE_READ_ERROR".to_string()),
                    remediation: remediation_for("CBKF001_FILE_READ_ERROR"),
                    next: None,
                },
            );
            None
        }
    }
}

/// Load the data file bytes, recording a pass or fail finding. Returns
/// `None` when diagnosis cannot continue past this stage.
fn load_input_bytes(findings: &mut Vec<Finding>, input: &Path) -> Option<Vec<u8>> {
    match std::fs::read(input) {
        Ok(bytes) => {
            if bytes.is_empty() {
                push(
                    &mut *findings,
                    Finding {
                        check: "input-load",
                        status: Status::Fail,
                        detail: "data file is empty".to_string(),
                        code: Some("CBKF001_FILE_READ_ERROR".to_string()),
                        remediation: remediation_for("CBKF001_FILE_READ_ERROR"),
                        next: None,
                    },
                );
                return None;
            }
            push(
                &mut *findings,
                Finding {
                    check: "input-load",
                    status: Status::Pass,
                    detail: format!("read {} bytes", bytes.len()),
                    code: None,
                    remediation: String::new(),
                    next: None,
                },
            );
            Some(bytes)
        }
        Err(error) => {
            push(
                &mut *findings,
                Finding {
                    check: "input-load",
                    status: Status::Fail,
                    detail: format!("cannot read {}: {error}", input.display()),
                    code: Some("CBKF001_FILE_READ_ERROR".to_string()),
                    remediation: remediation_for("CBKF001_FILE_READ_ERROR"),
                    next: None,
                },
            );
            None
        }
    }
}

fn truncate_detail(detail: String) -> String {
    if detail.chars().count() <= MAX_DETAIL_CHARS {
        return detail;
    }
    detail.chars().take(MAX_DETAIL_CHARS).collect()
}

/// Remediation text for a stable identity from the shared explain table.
fn remediation_for(code: &str) -> String {
    explanation_for(code)
        .map(|entry| entry.resolution.to_string())
        .unwrap_or_default()
}

fn parse_schema(
    text: &str,
    strict_comments: bool,
    dialect: crate::DialectPreference,
) -> Result<copybook::core::Schema, (String, String)> {
    use copybook::core::{Dialect, ParseOptions};
    let core_dialect = match dialect {
        crate::DialectPreference::N => Dialect::Normative,
        crate::DialectPreference::Zero => Dialect::ZeroTolerant,
        crate::DialectPreference::One => Dialect::OneTolerant,
    };
    let options = ParseOptions {
        strict_comments,
        strict: false,
        codepage: "cp037".to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
        dialect: core_dialect,
    };
    let mut schema = parse_copybook_with_options(text, &options)
        .map_err(|error| (error.code().to_string(), truncate_detail(error.to_string())))?;
    copybook::core::layout::resolve_layout(&mut schema, core_dialect)
        .map_err(|error| (error.code().to_string(), truncate_detail(error.to_string())))?;
    Ok(schema)
}

fn parse_copybook_with_options(
    text: &str,
    options: &copybook::core::ParseOptions,
) -> copybook::core::Result<copybook::core::Schema> {
    copybook::core::parse_copybook_with_options(text, options)
}

/// Confirm an explicitly requested framing against the bytes.
fn confirm_format(
    findings: &mut Vec<Finding>,
    format: RecordFormat,
    bytes: &[u8],
    lrecl: Option<u32>,
) {
    match format {
        RecordFormat::Fixed => match lrecl {
            Some(len) => {
                let len = len as usize;
                if len > 0 && bytes.len().is_multiple_of(len) {
                    push(
                        &mut *findings,
                        Finding {
                            check: "format-confirm",
                            status: Status::Pass,
                            detail: format!(
                                "file size {} is a multiple of record length {len}",
                                bytes.len()
                            ),
                            code: None,
                            remediation: String::new(),
                            next: None,
                        },
                    );
                } else {
                    push(
                        &mut *findings,
                        Finding {
                            check: "format-confirm",
                            status: Status::Warn,
                            detail: format!(
                                "file size {} is not a multiple of record length {len}",
                                bytes.len()
                            ),
                            code: Some("CBKR101_FIXED_RECORD_ERROR".to_string()),
                            remediation: remediation_for("CBKR101_FIXED_RECORD_ERROR"),
                            next: None,
                        },
                    );
                }
            }
            None => push(
                &mut *findings,
                Finding {
                    check: "format-confirm",
                    status: Status::Warn,
                    detail: "variable-length layout with fixed framing requested".to_string(),
                    code: None,
                    remediation:
                        "Use RDW framing for variable records, or check the ODO definition."
                            .to_string(),
                    next: None,
                },
            ),
        },
        RecordFormat::RDW => {
            if bytes.len() >= 4 && rdw_is_suspect_ascii_corruption_slice(&bytes[..4]) {
                push(
                    &mut *findings,
                    Finding {
                        check: "format-confirm",
                        status: Status::Warn,
                        detail: "first RDW header looks ASCII-corrupted".to_string(),
                        code: Some("CBKF104_RDW_SUSPECT_ASCII".to_string()),
                        remediation: remediation_for("CBKF104_RDW_SUSPECT_ASCII"),
                        next: None,
                    },
                );
            } else {
                push(
                    &mut *findings,
                    Finding {
                        check: "format-confirm",
                        status: Status::Pass,
                        detail: "RDW framing requested; header corruption not detected".to_string(),
                        code: None,
                        remediation: String::new(),
                        next: None,
                    },
                );
            }
        }
        RecordFormat::Vb => push(
            &mut *findings,
            Finding {
                check: "format-confirm",
                status: Status::Warn,
                detail: "VB framing is beta; framing proof is limited".to_string(),
                code: None,
                remediation: String::new(),
                next: None,
            },
        ),
    }
}

/// Probe fixed vs RDW framing. Returns the winner, or `None` (with a fail
/// finding) when the bytes fit neither framing.
fn probe_format(
    findings: &mut Vec<Finding>,
    bytes: &[u8],
    lrecl: Option<u32>,
) -> Option<RecordFormat> {
    let fixed_fits = match lrecl {
        Some(len) => len > 0 && !bytes.is_empty() && bytes.len().is_multiple_of(len as usize),
        None => false,
    };
    let rdw_records = read_rdw_records(bytes, 3);
    let rdw_fits = rdw_records.is_some();
    if bytes.len() >= 4 && rdw_is_suspect_ascii_corruption_slice(&bytes[..4]) {
        push(
            &mut *findings,
            Finding {
                check: "format-probe",
                status: Status::Warn,
                detail: "first RDW header looks ASCII-corrupted; transfer in binary mode"
                    .to_string(),
                code: Some("CBKF104_RDW_SUSPECT_ASCII".to_string()),
                remediation: remediation_for("CBKF104_RDW_SUSPECT_ASCII"),
                next: None,
            },
        );
    }
    match (fixed_fits, rdw_fits) {
        (true, false) => {
            probe_pass(findings, "fixed");
            Some(RecordFormat::Fixed)
        }
        (false, true) => {
            probe_pass(findings, "rdw");
            Some(RecordFormat::RDW)
        }
        (true, true) => {
            push(
                &mut *findings,
                Finding {
                    check: "format-probe",
                    status: Status::Warn,
                    detail: "bytes fit both fixed and RDW framing; pass --format explicitly"
                        .to_string(),
                    code: None,
                    remediation: "Rerun with --format fixed or --format rdw to pin the framing."
                        .to_string(),
                    next: None,
                },
            );
            Some(RecordFormat::Fixed)
        }
        (false, false) => {
            push(&mut *findings, Finding {
                check: "format-probe",
                status: Status::Fail,
                detail: "bytes fit neither fixed nor RDW framing".to_string(),
                code: None,
                remediation: "Check the transfer mode (binary, not text), the record length, and whether the file carries block headers."
                    .to_string(),
                next: None,
            });
            None
        }
    }
}

fn probe_pass(findings: &mut Vec<Finding>, format: &str) {
    push(
        &mut *findings,
        Finding {
            check: "format-probe",
            status: Status::Pass,
            detail: format!("bytes fit {format} framing"),
            code: None,
            remediation: String::new(),
            next: None,
        },
    );
}

/// Split input bytes into trial records under the resolved framing.
/// Returns `None` (recording the reason) when no trial records exist:
/// unknown fixed length, unframeable RDW/VB bytes, or no complete record.
fn frame_records(
    findings: &mut Vec<Finding>,
    bytes: &[u8],
    format: RecordFormat,
    lrecl: Option<u32>,
    sample: u32,
) -> Option<Vec<Vec<u8>>> {
    let records: Vec<Vec<u8>> = match format {
        RecordFormat::Fixed => match lrecl {
            Some(len) if len > 0 => bytes
                .chunks(len as usize)
                .take(sample as usize)
                .filter(|chunk| chunk.len() == len as usize)
                .map(<[u8]>::to_vec)
                .collect(),
            _ => {
                push(
                    &mut *findings,
                    Finding {
                        check: "trial-decode",
                        status: Status::Warn,
                        detail: "variable-length layout; trial decode skipped".to_string(),
                        code: None,
                        remediation:
                            "Run copybook decode directly; fixed chunking needs a known length."
                                .to_string(),
                        next: None,
                    },
                );
                return None;
            }
        },
        RecordFormat::RDW | RecordFormat::Vb => {
            let Some(records) = read_rdw_records(bytes, sample as usize) else {
                push(
                    &mut *findings,
                    Finding {
                        check: "trial-decode",
                        status: Status::Fail,
                        detail: "could not frame records for trial decode".to_string(),
                        code: Some("CBKF221_RDW_UNDERFLOW".to_string()),
                        remediation: remediation_for("CBKF221_RDW_UNDERFLOW"),
                        next: None,
                    },
                );
                return None;
            };
            records
        }
    };
    if records.is_empty() {
        push(
            &mut *findings,
            Finding {
                check: "trial-decode",
                status: Status::Warn,
                detail: "no complete records available for trial decode".to_string(),
                code: None,
                remediation: String::new(),
                next: None,
            },
        );
        return None;
    }
    Some(records)
}

/// Read up to `limit` RDW records; `None` when framing fails early.
fn read_rdw_records(bytes: &[u8], limit: usize) -> Option<Vec<Vec<u8>>> {
    let mut reader = RDWRecordReader::new(Cursor::new(bytes), false);
    let mut records = Vec::new();
    for _ in 0..limit {
        match reader.read_record() {
            Ok(Some(record)) => records.push(record.payload.clone()),
            Ok(None) => break,
            Err(_) => return None,
        }
    }
    if records.is_empty() {
        return None;
    }
    Some(records)
}

/// Score text printability: fraction of alphanumeric, whitespace, or common
/// punctuation characters. Diagnostic heuristic only.
fn printable_score(text: &str) -> f64 {
    // Counters stay well below u32 range: the probe window is 4 KiB.
    let mut total = 0u32;
    let mut printable = 0u32;
    for ch in text.chars() {
        total += 1;
        if ch.is_alphanumeric() || ch.is_whitespace() || "-.,:/()_+=*%$#@!?'\"".contains(ch) {
            printable += 1;
        }
    }
    if total == 0 {
        return 0.0;
    }
    f64::from(printable) / f64::from(total)
}

/// Probe all six codepages over a byte window. Always states the winner,
/// the runner-up margin, and the confidence; never asserts certainty.
fn probe_codepage(findings: &mut Vec<Finding>, bytes: &[u8]) -> Codepage {
    use Codepage::{ASCII, CP037, CP273, CP500, CP1047, CP1140};
    let window = &bytes[..bytes.len().min(CODEPAGE_PROBE_WINDOW)];
    let ascii_text = String::from_utf8_lossy(window).into_owned();
    let mut scored: Vec<(Codepage, f64)> = vec![(ASCII, printable_score(&ascii_text))];
    for codepage in [CP037, CP273, CP500, CP1047, CP1140] {
        let score = match ebcdic_to_utf8(window, codepage, UnmappablePolicy::Replace) {
            Ok(text) => printable_score(&text),
            Err(_) => 0.0,
        };
        scored.push((codepage, score));
    }
    scored.sort_by(|left, right| {
        right
            .1
            .partial_cmp(&left.1)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    let (winner, winner_score) = scored[0];
    let runner_up = scored.get(1).map_or(0.0, |item| item.1);
    let margin = winner_score - runner_up;
    let confidence = if margin >= CODEPAGE_CONFIDENCE_MARGIN {
        "high"
    } else {
        "low"
    };
    push(
        &mut *findings,
        Finding {
            check: "codepage-probe",
            status: Status::Pass,
            detail: format!(
                "{winner} wins over {} bytes with {confidence} confidence (score {winner_score:.2}, margin {margin:.2}); heuristic, pass --codepage to pin it",
                window.len(),
            ),
            code: None,
            remediation: String::new(),
            next: None,
        },
    );
    winner
}

/// Trial-decode the first `sample` records under the resolved options.
/// The first failure becomes a fail finding whose remediation comes from
/// the shared explain table.
#[allow(clippy::too_many_arguments)]
fn trial_decode(
    findings: &mut Vec<Finding>,
    schema: &copybook::core::Schema,
    copybook: &Path,
    input: &Path,
    bytes: &[u8],
    format: RecordFormat,
    codepage: Codepage,
    lrecl: Option<u32>,
    sample: u32,
) {
    if sample == 0 {
        push(
            &mut *findings,
            Finding {
                check: "trial-decode",
                status: Status::Pass,
                detail: "skipped (--sample 0)".to_string(),
                code: None,
                remediation: String::new(),
                next: None,
            },
        );
        return;
    }
    let Some(records) = frame_records(&mut *findings, bytes, format, lrecl, sample) else {
        return;
    };
    let options = DecodeOptions::new()
        .with_format(format)
        .with_codepage(codepage);
    for (index, record) in records.iter().enumerate() {
        if let Err(error) = decode_record(schema, record, &options) {
            let code = error.code().to_string();
            push(
                &mut *findings,
                Finding {
                    check: "trial-decode",
                    status: Status::Fail,
                    detail: format!(
                        "record {} of {} failed: {}",
                        index + 1,
                        records.len(),
                        truncate_detail(error.to_string()),
                    ),
                    code: Some(code.clone()),
                    remediation: remediation_for(&code),
                    next: Some(suggested_decode(copybook, input, format, codepage)),
                },
            );
            return;
        }
    }
    push(
        &mut *findings,
        Finding {
            check: "trial-decode",
            status: Status::Pass,
            detail: format!("first {} record(s) decode", records.len()),
            code: None,
            remediation: String::new(),
            next: Some(suggested_decode(copybook, input, format, codepage)),
        },
    );
}

fn suggested_decode(
    copybook: &Path,
    input: &Path,
    format: RecordFormat,
    codepage: Codepage,
) -> String {
    format!(
        "copybook decode {} {} --format {format} --codepage {codepage} --output out.jsonl",
        copybook.display(),
        input.display(),
    )
}

/// Render findings and map the worst failure to its taxonomy exit code.
fn finish(findings: &[Finding], json: bool) -> anyhow::Result<ExitCode> {
    let mut exit = ExitCode::Ok;
    for finding in findings {
        if finding.status != Status::Fail {
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
