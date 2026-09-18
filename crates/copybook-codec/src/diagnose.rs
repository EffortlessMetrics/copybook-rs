// SPDX-License-Identifier: AGPL-3.0-or-later
//! Bounded-first diagnosis of a copybook plus data file.
//!
//! This module owns the operator-diagnosis result: given copybook text and
//! input bytes it stages the same checks a frustrated operator would run
//! by hand (parse, record length, framing fit, codepage fit, trial decode)
//! and records every outcome as a typed [`DiagnosisFinding`] with its
//! stable error identity, remediation from the shared explain table, and
//! the exact next command to run. Callers render findings for humans and
//! machines from this single result; no caller reimplements the probes.
//!
//! Probes that guess (format, codepage) always state their confidence and
//! their evidence window; a guess is never rendered as certainty.

use crate::record::RDWRecordReader;
use crate::{Codepage, DecodeOptions, RecordFormat, UnmappablePolicy, decode_record};
use copybook_core::Dialect;
use copybook_error::explain::explanation_for;
use copybook_rdw::diagnostics::rdw_is_suspect_ascii_corruption_slice;
use std::io::Cursor;
use std::path::Path;

/// Bytes of input scored by the codepage probe.
const CODEPAGE_PROBE_WINDOW: usize = 4096;

/// Minimum score margin for a high-confidence codepage call.
const CODEPAGE_CONFIDENCE_MARGIN: f64 = 0.15;

/// Maximum characters kept from an error rendering inside a finding.
const MAX_DETAIL_CHARS: usize = 300;

/// Outcome of one diagnostic check. Warnings never fail the run; failures
/// map to the taxonomy exit code of their stable identity (or `Format`
/// without one) at the rendering layer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosisStatus {
    /// The check held.
    Pass,
    /// Worth surfacing, but not a failure on its own.
    Warn,
    /// The check failed; the run needs attention.
    Fail,
}

impl DiagnosisStatus {
    /// Machine-stable rendering of the status.
    #[inline]
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            DiagnosisStatus::Pass => "pass",
            DiagnosisStatus::Warn => "warn",
            DiagnosisStatus::Fail => "fail",
        }
    }
}

/// One diagnostic outcome: what was checked, how it went, and what to do
/// next. Every failure names its stable error identity where one exists.
#[derive(Clone, Debug)]
pub struct DiagnosisFinding {
    /// Short machine-stable check name (for example `format-probe`).
    pub check: &'static str,
    /// The outcome.
    pub status: DiagnosisStatus,
    /// Human-readable detail, bounded to [`MAX_DETAIL_CHARS`] characters.
    pub detail: String,
    /// Stable error identity, when the outcome carries one.
    pub code: Option<String>,
    /// Remediation from the shared explain table, when one exists.
    pub remediation: String,
    /// Exact next command to run, when the diagnosis hands one off.
    pub next: Option<String>,
}

/// The data-file side of a diagnosis: a bounded leading prefix plus the
/// full size, so probes can stay bounded while size-based checks (fixed
/// multiple, truncation) still see the whole file. `prefix.len()` below
/// `total_bytes` means the scope is truncated; findings say so.
#[derive(Clone, Copy, Debug)]
pub struct DiagnosisInput<'a> {
    /// Display path of the data file (names the next command).
    pub path: &'a Path,
    /// Leading bytes actually inspected.
    pub prefix: &'a [u8],
    /// Full file size in bytes.
    pub total_bytes: u64,
}

impl DiagnosisInput<'_> {
    /// Whether the prefix covers the whole file.
    #[inline]
    #[must_use]
    pub const fn scope_complete(&self) -> bool {
        self.prefix.len() as u64 >= self.total_bytes
    }
}

/// Inputs to [`diagnose`] that select, rather than guess, the configuration.
#[derive(Clone, Debug)]
pub struct DiagnoseOptions {
    /// Explicit framing; `None` probes fixed versus RDW.
    pub format: Option<RecordFormat>,
    /// Explicit codepage; `None` probes all six codepages.
    pub codepage: Option<Codepage>,
    /// Trial-decode at most this many records (`0` skips the trial).
    pub sample: u32,
    /// Reject `#`-style comments instead of tolerating them.
    pub strict_comments: bool,
    /// Copybook dialect for parsing and layout.
    pub dialect: Dialect,
}

/// The complete diagnosis: every staged finding in order.
#[derive(Clone, Debug, Default)]
pub struct Diagnosis {
    /// Staged findings, in diagnosis order.
    pub findings: Vec<DiagnosisFinding>,
}

impl Diagnosis {
    /// Whether any finding failed.
    #[inline]
    #[must_use]
    pub fn has_failures(&self) -> bool {
        self.findings
            .iter()
            .any(|finding| finding.status == DiagnosisStatus::Fail)
    }
}

/// Diagnose copybook text with an optional data file.
///
/// `input` carries the display path alongside the bytes so findings can
/// name the exact next command. Every probe reuses a library primitive
/// (framing readers, charset conversion, trial decode) and every failure
/// names its stable error identity with remediation from the shared
/// explain table.
#[inline]
pub fn diagnose(
    copybook_text: &str,
    copybook_path: &Path,
    input: Option<DiagnosisInput<'_>>,
    options: &DiagnoseOptions,
) -> Diagnosis {
    let mut diagnosis = Diagnosis::default();

    let schema = match parse_schema(copybook_text, options.strict_comments, options.dialect) {
        Ok(schema) => {
            push(
                &mut diagnosis,
                DiagnosisFinding {
                    check: "copybook-parse",
                    status: DiagnosisStatus::Pass,
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
                &mut diagnosis,
                DiagnosisFinding {
                    check: "copybook-parse",
                    status: DiagnosisStatus::Fail,
                    detail,
                    code: Some(code.clone()),
                    remediation: remediation_for(&code),
                    next: Some(format!("copybook parse {}", copybook_path.display())),
                },
            );
            return diagnosis;
        }
    };

    let lrecl = schema.lrecl_fixed;
    push(
        &mut diagnosis,
        DiagnosisFinding {
            check: "record-length",
            status: DiagnosisStatus::Pass,
            detail: match lrecl {
                Some(len) => format!("fixed record length {len} bytes"),
                None => "variable record length (ODO); fixed-size checks do not apply".to_string(),
            },
            code: None,
            remediation: String::new(),
            next: None,
        },
    );

    let Some(input) = input else {
        push(
            &mut diagnosis,
            DiagnosisFinding {
                check: "input",
                status: DiagnosisStatus::Pass,
                detail: "no data file given; copybook-only diagnosis".to_string(),
                code: None,
                remediation: String::new(),
                next: Some(format!(
                    "copybook doctor {} <DATA>",
                    copybook_path.display()
                )),
            },
        );
        return diagnosis;
    };

    if input.total_bytes == 0 {
        push(
            &mut diagnosis,
            DiagnosisFinding {
                check: "input-load",
                status: DiagnosisStatus::Fail,
                detail: "data file is empty".to_string(),
                code: Some("CBKF001_FILE_READ_ERROR".to_string()),
                remediation: remediation_for("CBKF001_FILE_READ_ERROR"),
                next: None,
            },
        );
        return diagnosis;
    }
    push(
        &mut diagnosis,
        DiagnosisFinding {
            check: "input-load",
            status: DiagnosisStatus::Pass,
            detail: if input.scope_complete() {
                format!("read {} bytes", input.total_bytes)
            } else {
                format!(
                    "inspecting first {} of {} bytes; later bytes unscanned",
                    input.prefix.len(),
                    input.total_bytes
                )
            },
            code: None,
            remediation: String::new(),
            next: None,
        },
    );

    // Size-based checks see the whole file; content probes see the prefix.
    let bytes = input.prefix;
    let total_bytes = input.total_bytes as usize;

    // Format: confirm the explicit choice or probe both framings.
    let resolved_format = match options.format {
        Some(given) => {
            confirm_format(&mut diagnosis, given, bytes, total_bytes, lrecl);
            Some(given)
        }
        None => probe_format(
            &mut diagnosis,
            bytes,
            input.scope_complete(),
            total_bytes,
            lrecl,
        ),
    };
    let Some(resolved_format) = resolved_format else {
        return diagnosis;
    };

    // Codepage: confirm the explicit choice implicitly via trial decode, or
    // probe all six codepages over a byte window and state the confidence.
    let resolved_codepage = match options.codepage {
        Some(given) => given,
        None => probe_codepage(&mut diagnosis, bytes),
    };

    trial_decode(
        &mut diagnosis,
        &schema,
        copybook_path,
        input.path,
        bytes,
        input.scope_complete(),
        resolved_format,
        resolved_codepage,
        lrecl,
        options.sample,
    );
    diagnosis
}

fn push(diagnosis: &mut Diagnosis, finding: DiagnosisFinding) {
    diagnosis.findings.push(finding);
}

fn truncate_detail(detail: String) -> String {
    if detail.chars().count() <= MAX_DETAIL_CHARS {
        return detail;
    }
    detail.chars().take(MAX_DETAIL_CHARS).collect()
}

/// Remediation text for a stable identity from the shared explain table.
/// Empty when the identity carries no recorded resolution.
#[inline]
#[must_use]
pub fn remediation_for(code: &str) -> String {
    explanation_for(code)
        .map(|entry| entry.resolution.to_string())
        .unwrap_or_default()
}

fn parse_schema(
    text: &str,
    strict_comments: bool,
    dialect: Dialect,
) -> Result<copybook_core::Schema, (String, String)> {
    use copybook_core::ParseOptions;
    let options = ParseOptions {
        strict_comments,
        strict: false,
        codepage: "cp037".to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
        dialect,
    };
    let mut schema = parse_copybook_with_options(text, &options)
        .map_err(|error| (error.code().to_string(), truncate_detail(error.to_string())))?;
    copybook_core::layout::resolve_layout(&mut schema, dialect)
        .map_err(|error| (error.code().to_string(), truncate_detail(error.to_string())))?;
    Ok(schema)
}

fn parse_copybook_with_options(
    text: &str,
    options: &copybook_core::ParseOptions,
) -> copybook_core::Result<copybook_core::Schema> {
    copybook_core::parse_copybook_with_options(text, options)
}

/// Confirm an explicitly requested framing against the bytes. Size checks
/// use the full file size, never the inspected prefix.
fn confirm_format(
    diagnosis: &mut Diagnosis,
    format: RecordFormat,
    bytes: &[u8],
    total_bytes: usize,
    lrecl: Option<u32>,
) {
    match format {
        RecordFormat::Fixed => match lrecl {
            Some(len) => {
                let len = len as usize;
                if len > 0 && total_bytes.is_multiple_of(len) {
                    push(
                        &mut *diagnosis,
                        DiagnosisFinding {
                            check: "format-confirm",
                            status: DiagnosisStatus::Pass,
                            detail: format!(
                                "file size {total_bytes} is a multiple of record length {len}"
                            ),
                            code: None,
                            remediation: String::new(),
                            next: None,
                        },
                    );
                } else {
                    push(
                        &mut *diagnosis,
                        DiagnosisFinding {
                            check: "format-confirm",
                            status: DiagnosisStatus::Warn,
                            detail: format!(
                                "file size {total_bytes} is not a multiple of record length {len}"
                            ),
                            code: Some("CBKR101_FIXED_RECORD_ERROR".to_string()),
                            remediation: remediation_for("CBKR101_FIXED_RECORD_ERROR"),
                            next: None,
                        },
                    );
                }
            }
            None => push(
                &mut *diagnosis,
                DiagnosisFinding {
                    check: "format-confirm",
                    status: DiagnosisStatus::Warn,
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
                    &mut *diagnosis,
                    DiagnosisFinding {
                        check: "format-confirm",
                        status: DiagnosisStatus::Warn,
                        detail: "first RDW header looks ASCII-corrupted".to_string(),
                        code: Some("CBKF104_RDW_SUSPECT_ASCII".to_string()),
                        remediation: remediation_for("CBKF104_RDW_SUSPECT_ASCII"),
                        next: None,
                    },
                );
            } else {
                push(
                    &mut *diagnosis,
                    DiagnosisFinding {
                        check: "format-confirm",
                        status: DiagnosisStatus::Pass,
                        detail: "RDW framing requested; header corruption not detected".to_string(),
                        code: None,
                        remediation: String::new(),
                        next: None,
                    },
                );
            }
        }
        RecordFormat::Vb => push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-confirm",
                status: DiagnosisStatus::Warn,
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
    diagnosis: &mut Diagnosis,
    bytes: &[u8],
    scope_complete: bool,
    total_bytes: usize,
    lrecl: Option<u32>,
) -> Option<RecordFormat> {
    let fixed_fits = match lrecl {
        Some(len) => len > 0 && total_bytes > 0 && total_bytes.is_multiple_of(len as usize),
        None => false,
    };
    let rdw_records = read_rdw_records(bytes, 3, scope_complete);
    let rdw_fits = rdw_records.is_some();
    if bytes.len() >= 4 && rdw_is_suspect_ascii_corruption_slice(&bytes[..4]) {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-probe",
                status: DiagnosisStatus::Warn,
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
            probe_pass(diagnosis, "fixed");
            Some(RecordFormat::Fixed)
        }
        (false, true) => {
            probe_pass(diagnosis, "rdw");
            Some(RecordFormat::RDW)
        }
        (true, true) => {
            push(
                &mut *diagnosis,
                DiagnosisFinding {
                    check: "format-probe",
                    status: DiagnosisStatus::Warn,
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
            push(&mut *diagnosis, DiagnosisFinding {
                check: "format-probe",
                status: DiagnosisStatus::Fail,
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

fn probe_pass(diagnosis: &mut Diagnosis, format: &str) {
    push(
        &mut *diagnosis,
        DiagnosisFinding {
            check: "format-probe",
            status: DiagnosisStatus::Pass,
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
    diagnosis: &mut Diagnosis,
    bytes: &[u8],
    scope_complete: bool,
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
                    &mut *diagnosis,
                    DiagnosisFinding {
                        check: "trial-decode",
                        status: DiagnosisStatus::Warn,
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
            let Some(records) = read_rdw_records(bytes, sample as usize, scope_complete) else {
                push(
                    &mut *diagnosis,
                    DiagnosisFinding {
                        check: "trial-decode",
                        status: DiagnosisStatus::Fail,
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
            &mut *diagnosis,
            DiagnosisFinding {
                check: "trial-decode",
                status: DiagnosisStatus::Warn,
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
/// When the inspected scope is truncated (`scope_complete` false), a framing
/// error after at least one record ends the scope instead of failing: the
/// tail was chosen away, not proven corrupt. A complete scope stays strict.
fn read_rdw_records(bytes: &[u8], limit: usize, scope_complete: bool) -> Option<Vec<Vec<u8>>> {
    let mut reader = RDWRecordReader::new(Cursor::new(bytes), false);
    let mut records = Vec::new();
    for _ in 0..limit {
        match reader.read_record() {
            Ok(Some(record)) => records.push(record.payload.clone()),
            Ok(None) => break,
            Err(_) => {
                if scope_complete || records.is_empty() {
                    return None;
                }
                break;
            }
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
fn probe_codepage(diagnosis: &mut Diagnosis, bytes: &[u8]) -> Codepage {
    use Codepage::{ASCII, CP037, CP273, CP500, CP1047, CP1140};
    let window = &bytes[..bytes.len().min(CODEPAGE_PROBE_WINDOW)];
    let ascii_text = String::from_utf8_lossy(window).into_owned();
    let mut scored: Vec<(Codepage, f64)> = vec![(ASCII, printable_score(&ascii_text))];
    for codepage in [CP037, CP273, CP500, CP1047, CP1140] {
        let score =
            match crate::charset::ebcdic_to_utf8(window, codepage, UnmappablePolicy::Replace) {
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
        &mut *diagnosis,
        DiagnosisFinding {
            check: "codepage-probe",
            status: DiagnosisStatus::Pass,
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
    diagnosis: &mut Diagnosis,
    schema: &copybook_core::Schema,
    copybook: &Path,
    input: &Path,
    bytes: &[u8],
    scope_complete: bool,
    format: RecordFormat,
    codepage: Codepage,
    lrecl: Option<u32>,
    sample: u32,
) {
    if sample == 0 {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "trial-decode",
                status: DiagnosisStatus::Pass,
                detail: "skipped (--sample 0)".to_string(),
                code: None,
                remediation: String::new(),
                next: None,
            },
        );
        return;
    }
    let Some(records) = frame_records(
        &mut *diagnosis,
        bytes,
        scope_complete,
        format,
        lrecl,
        sample,
    ) else {
        return;
    };
    let options = DecodeOptions::new()
        .with_format(format)
        .with_codepage(codepage);
    for (index, record) in records.iter().enumerate() {
        if let Err(error) = decode_record(schema, record, &options) {
            let code = error.code().to_string();
            push(
                &mut *diagnosis,
                DiagnosisFinding {
                    check: "trial-decode",
                    status: DiagnosisStatus::Fail,
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
        &mut *diagnosis,
        DiagnosisFinding {
            check: "trial-decode",
            status: DiagnosisStatus::Pass,
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
