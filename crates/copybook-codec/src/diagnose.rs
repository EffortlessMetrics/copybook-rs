// SPDX-License-Identifier: AGPL-3.0-or-later
//! Bounded-first diagnosis of a copybook plus data file.
//!
//! This module owns the operator-diagnosis result: given copybook text and
//! input bytes it stages the same checks a frustrated operator would run
//! by hand (parse, record length, framing fit, codepage fit, trial decode)
//! and records every outcome as a typed
//! [`DiagnosisFinding`](crate::diagnose::DiagnosisFinding) with its
//! stable error identity, remediation from the shared explain table, and
//! the exact next command to run. Callers render findings for humans and
//! machines from this single result; no caller reimplements the probes.
//!
//! Probes that guess (format, codepage) always state their confidence and
//! their evidence window; a guess is never rendered as certainty.

use crate::record::{RDWRecordReader, VbBlockReader};
use crate::{Codepage, DecodeOptions, RecordFormat, UnmappablePolicy, decode_record};
use copybook_core::{Dialect, ErrorCode};
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
    /// Human-readable detail, bounded to `MAX_DETAIL_CHARS` characters.
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
    /// Machine-usable evidence behind the findings.
    pub evidence: DiagnosisEvidence,
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

/// Machine-usable evidence behind a [`Diagnosis`], for callers that act on
/// the diagnosis instead of only rendering it (for example, drafting an
/// interpretation profile from a healthy diagnosis).
///
/// Every field states what the probes established; nothing here guesses.
/// `None` (or `false` with no supporting observation) means the probes did
/// not establish that input, and callers must refuse or mark review rather
/// than fill a default silently.
#[derive(Clone, Debug, Default)]
#[allow(clippy::struct_excessive_bools)] // Each flag qualifies its paired observation; enums would split the pairs
pub struct DiagnosisEvidence {
    /// Confirmed framing: the explicit `--format` choice or the single
    /// probe fit. `None` when framing is unconfirmed (diagnosis stops).
    pub format: Option<RecordFormat>,
    /// Whether the framing came from an explicit flag rather than the probe.
    pub format_explicit: bool,
    /// Resolved codepage: explicit choice, confident probe winner, or
    /// leading candidate for the trial decode.
    pub codepage: Option<Codepage>,
    /// Whether the codepage is pinned (explicit flag or confident winner
    /// corroborated by the trial decode) as opposed to a leading candidate.
    pub codepage_pinned: bool,
    /// Whether the codepage came from an explicit flag.
    pub codepage_explicit: bool,
    /// Record size in bytes when established: the fixed LRECL, or the
    /// largest observed RDW wire record (payload plus 4-byte header).
    /// `None` for VB (block overhead makes wire size ill-defined) and when
    /// no record was framed.
    pub record_length: Option<u64>,
    /// Whether `record_length` is exact (fixed LRECL) as opposed to an
    /// observed sample maximum.
    pub record_length_exact: bool,
    /// Whether any probed record carried non-zero framing reserved bytes.
    pub reserved_nonzero_observed: bool,
    /// Whether the layout is variable-length (no fixed LRECL), in which
    /// case dialect interpretation may matter.
    pub variable_layout: bool,
    /// Whether the trial decode decoded every framed record.
    pub trial_succeeded: bool,
    /// How many records the trial decode framed.
    pub trial_records: u32,
}

/// Diagnose copybook text with an optional data file.
///
/// `input` carries the display path alongside the bytes so findings can
/// name the exact next command. Every probe reuses a library primitive
/// (framing readers, charset conversion, trial decode) and every failure
/// names its stable error identity with remediation from the shared
/// explain table.
#[inline]
#[must_use]
pub fn diagnose(
    copybook_text: &str,
    copybook_path: &Path,
    input: Option<DiagnosisInput<'_>>,
    options: &DiagnoseOptions,
) -> Diagnosis {
    let mut diagnosis = Diagnosis::default();

    let Some(schema) = diagnose_schema(&mut diagnosis, copybook_text, copybook_path, options)
    else {
        return diagnosis;
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

    // Size-based checks see the whole file; content probes see the prefix.
    let Some(input) = diagnose_input_scope(&mut diagnosis, copybook_path, input) else {
        return diagnosis;
    };
    let bytes = input.prefix;
    let total_bytes = input.total_bytes;

    // Format: confirm the explicit choice or probe both framings.
    let resolved_format = match options.format {
        Some(given) => {
            confirm_format(
                &mut diagnosis,
                given,
                bytes,
                input.scope_complete(),
                total_bytes,
                lrecl,
            );
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

    // Codepage: the explicit choice is pinned; a probe winner is only a
    // candidate until the trial decode corroborates it.
    let (resolved_codepage, codepage_winner) = match options.codepage {
        Some(given) => (given, true),
        None => probe_codepage(&mut diagnosis, bytes),
    };

    let trial = trial_decode(
        &mut diagnosis,
        &schema,
        copybook_path,
        input.path,
        bytes,
        input.scope_complete(),
        resolved_format,
        resolved_codepage,
        codepage_winner,
        lrecl,
        options.sample,
    );
    let codepage_explicit = options.codepage.is_some();
    diagnosis.evidence.format = Some(resolved_format);
    diagnosis.evidence.format_explicit = options.format.is_some();
    diagnosis.evidence.codepage = Some(resolved_codepage);
    diagnosis.evidence.codepage_explicit = codepage_explicit;
    // A probe winner only counts as pinned when the trial decode
    // corroborated it on at least one record; explicit choices and
    // uncorroborated winners stay distinguishable downstream.
    diagnosis.evidence.codepage_pinned =
        codepage_explicit || (codepage_winner && trial.all_decoded && trial.framed > 0);
    diagnosis.evidence.variable_layout = lrecl.is_none();
    let (record_length, record_length_exact) = match resolved_format {
        RecordFormat::Fixed | RecordFormat::Text => (lrecl.map(u64::from), true),
        RecordFormat::RDW => (trial.max_wire_len, false),
        RecordFormat::Vb => (None, false),
    };
    diagnosis.evidence.record_length = record_length;
    diagnosis.evidence.record_length_exact = record_length_exact;
    diagnosis.evidence.reserved_nonzero_observed =
        scan_reserved_nonzero(bytes, resolved_format, options.sample as usize);
    diagnosis.evidence.trial_succeeded = trial.all_decoded && trial.framed > 0;
    diagnosis.evidence.trial_records = trial.framed;
    diagnosis
}

/// Trial-decode outcome for evidence: how many records framed, whether all
/// of them decoded, and the largest observed payload.
struct TrialOutcome {
    framed: u32,
    all_decoded: bool,
    max_wire_len: Option<u64>,
}

/// Scan the first `limit` records with strict reserved-byte validation,
/// reporting whether any record carries non-zero framing reserved bytes.
/// Other framing errors end the scan as inconclusive (not observed); fixed
/// framing has no reserved bytes.
fn scan_reserved_nonzero(bytes: &[u8], format: RecordFormat, limit: usize) -> bool {
    if limit == 0 {
        return false;
    }
    match format {
        // Fixed framing has no reserved bytes; text terminators are
        // framing, not reserved bytes either.
        RecordFormat::Fixed | RecordFormat::Text => false,
        RecordFormat::RDW => {
            let mut reader = RDWRecordReader::new(Cursor::new(bytes), true);
            for _ in 0..limit {
                match reader.read_record() {
                    Ok(Some(_)) => {}
                    Ok(None) => break,
                    Err(error) => {
                        if error.code() == ErrorCode::CBKR211_RDW_RESERVED_NONZERO {
                            return true;
                        }
                        break;
                    }
                }
            }
            false
        }
        RecordFormat::Vb => {
            let mut reader = VbBlockReader::new(Cursor::new(bytes), true);
            for _ in 0..limit {
                match reader.read_record() {
                    Ok(Some(_)) => {}
                    Ok(None) => break,
                    Err(error) => {
                        if matches!(
                            error.code(),
                            ErrorCode::CBKR211_RDW_RESERVED_NONZERO
                                | ErrorCode::CBKF225_BDW_RESERVED_NONZERO
                        ) {
                            return true;
                        }
                        break;
                    }
                }
            }
            false
        }
    }
}

fn push(diagnosis: &mut Diagnosis, finding: DiagnosisFinding) {
    diagnosis.findings.push(finding);
}

/// Parse the copybook, recording the pass or fail finding. Returns `None`
/// when diagnosis cannot continue past this stage.
fn diagnose_schema(
    diagnosis: &mut Diagnosis,
    copybook_text: &str,
    copybook_path: &Path,
    options: &DiagnoseOptions,
) -> Option<copybook_core::Schema> {
    match parse_schema(copybook_text, options.strict_comments, options.dialect) {
        Ok(schema) => {
            push(
                &mut *diagnosis,
                DiagnosisFinding {
                    check: "copybook-parse",
                    status: DiagnosisStatus::Pass,
                    detail: "copybook parses".to_string(),
                    code: None,
                    remediation: String::new(),
                    next: None,
                },
            );
            Some(schema)
        }
        Err((code, detail)) => {
            push(
                &mut *diagnosis,
                DiagnosisFinding {
                    check: "copybook-parse",
                    status: DiagnosisStatus::Fail,
                    detail,
                    code: Some(code.clone()),
                    remediation: remediation_for(&code),
                    next: Some(format!("copybook parse {}", copybook_path.display())),
                },
            );
            None
        }
    }
}

/// Record the input scope, passing the input through when diagnosis can
/// continue. Copybook-only and empty inputs terminate here.
fn diagnose_input_scope<'a>(
    diagnosis: &mut Diagnosis,
    copybook_path: &Path,
    input: Option<DiagnosisInput<'a>>,
) -> Option<DiagnosisInput<'a>> {
    let Some(input) = input else {
        push(
            &mut *diagnosis,
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
        return None;
    };
    if input.total_bytes == 0 {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "input-load",
                status: DiagnosisStatus::Fail,
                detail: "data file is empty".to_string(),
                code: Some("CBKF001_FILE_READ_ERROR".to_string()),
                remediation: remediation_for("CBKF001_FILE_READ_ERROR"),
                next: None,
            },
        );
        return None;
    }
    push(
        &mut *diagnosis,
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
    Some(input)
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
    scope_complete: bool,
    total_bytes: u64,
    lrecl: Option<u32>,
) {
    match format {
        RecordFormat::Fixed => confirm_fixed(diagnosis, total_bytes, lrecl),
        RecordFormat::RDW => confirm_rdw(diagnosis, bytes, scope_complete),
        RecordFormat::Vb => confirm_vb(diagnosis, bytes, scope_complete),
        RecordFormat::Text => confirm_text(diagnosis, bytes, scope_complete, lrecl),
    }
}

/// Confirm an explicit text request: every line payload must hold exactly
/// `lrecl` bytes after terminator stripping, tolerating a final line
/// without a terminator. A truncated inspection scope drops its trailing
/// partial line first: the fragment was chosen away, not proven corrupt.
fn confirm_text(diagnosis: &mut Diagnosis, bytes: &[u8], scope_complete: bool, lrecl: Option<u32>) {
    let Some(len) = lrecl.map(usize::try_from).and_then(Result::ok) else {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-confirm",
                status: DiagnosisStatus::Warn,
                detail: "variable-length layout with text framing requested".to_string(),
                code: None,
                remediation: "Use RDW framing for variable records, or check the ODO definition."
                    .to_string(),
                next: None,
            },
        );
        return;
    };
    let bytes = complete_text_prefix(bytes, scope_complete);
    // A truncated scope that ends mid-line carries no complete line to
    // confirm; silence beats a false malformation finding.
    if bytes.is_empty() && !scope_complete {
        return;
    }
    match split_text_payloads(bytes) {
        payloads if !payloads.is_empty() && payloads.iter().all(|line| line.len() == len) => {
            push(
                &mut *diagnosis,
                DiagnosisFinding {
                    check: "format-confirm",
                    status: DiagnosisStatus::Pass,
                    detail: format!(
                        "{} line(s) frame as text records ({len} bytes each)",
                        payloads.len()
                    ),
                    code: None,
                    remediation: String::new(),
                    next: None,
                },
            );
        }
        _ => {
            push(
                &mut *diagnosis,
                DiagnosisFinding {
                    check: "format-confirm",
                    status: DiagnosisStatus::Warn,
                    detail: format!(
                        "bytes do not frame as {len}-byte text lines; short/long lines are rejected"
                    ),
                    code: Some("CBKR101_FIXED_RECORD_ERROR".to_string()),
                    remediation: remediation_for("CBKR101_FIXED_RECORD_ERROR"),
                    next: None,
                },
            );
        }
    }
}

/// Drop the trailing partial line of a truncated inspection scope.
///
/// Diagnosis never ingests a whole large input; when the scope ends
/// mid-line that fragment is an artifact of the inspection cap, not
/// evidence. Complete scopes pass through untouched.
fn complete_text_prefix(bytes: &[u8], scope_complete: bool) -> &[u8] {
    if scope_complete {
        return bytes;
    }
    match bytes.iter().rposition(|byte| *byte == b'\n') {
        Some(pos) => &bytes[..=pos],
        None => &[],
    }
}

/// Split text-framed bytes into payloads: LF terminates, one CR before the
/// LF is stripped, and a final unterminated line is accepted. Splitting
/// never refuses; length policy belongs to the caller.
fn split_text_payloads(bytes: &[u8]) -> Vec<Vec<u8>> {
    let mut chunks: Vec<&[u8]> = bytes.split(|byte| *byte == b'\n').collect();
    // A final LF terminates the last line; the split residue after it is
    // not a new (empty) line.
    if bytes.last() == Some(&b'\n') {
        chunks.pop();
    }
    chunks
        .iter()
        .map(|chunk| chunk.strip_suffix(b"\r").unwrap_or(chunk).to_vec())
        .collect()
}

/// Confirm an explicit fixed request against the full file size.
fn confirm_fixed(diagnosis: &mut Diagnosis, total_bytes: u64, lrecl: Option<u32>) {
    let Some(len) = lrecl.map(u64::from) else {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-confirm",
                status: DiagnosisStatus::Warn,
                detail: "variable-length layout with fixed framing requested".to_string(),
                code: None,
                remediation: "Use RDW framing for variable records, or check the ODO definition."
                    .to_string(),
                next: None,
            },
        );
        return;
    };
    if len > 0 && total_bytes.is_multiple_of(len) {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-confirm",
                status: DiagnosisStatus::Pass,
                detail: format!("file size {total_bytes} is a multiple of record length {len}"),
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
                detail: format!("file size {total_bytes} is not a multiple of record length {len}"),
                code: Some("CBKR101_FIXED_RECORD_ERROR".to_string()),
                remediation: remediation_for("CBKR101_FIXED_RECORD_ERROR"),
                next: None,
            },
        );
    }
}

/// Confirm an explicit RDW request with positive framing evidence: records
/// must actually frame, not merely dodge one corruption signature.
fn confirm_rdw(diagnosis: &mut Diagnosis, bytes: &[u8], scope_complete: bool) {
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
    }
    match read_rdw_records(bytes, 3, scope_complete, false) {
        Some(records) => push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-confirm",
                status: DiagnosisStatus::Pass,
                detail: format!("first {} record(s) frame as RDW", records.len()),
                code: None,
                remediation: String::new(),
                next: None,
            },
        ),
        None => push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-confirm",
                status: DiagnosisStatus::Warn,
                detail: "bytes do not frame as RDW records".to_string(),
                code: Some("CBKF221_RDW_UNDERFLOW".to_string()),
                remediation: remediation_for("CBKF221_RDW_UNDERFLOW"),
                next: None,
            },
        ),
    }
}

/// Confirm an explicit VB request with positive block-framing evidence.
fn confirm_vb(diagnosis: &mut Diagnosis, bytes: &[u8], scope_complete: bool) {
    match read_vb_records(bytes, 3, scope_complete, false) {
        Some(records) => push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-confirm",
                status: DiagnosisStatus::Pass,
                detail: format!(
                    "first {} record(s) frame as VB blocks (beta framing)",
                    records.len()
                ),
                code: None,
                remediation: String::new(),
                next: None,
            },
        ),
        None => push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "format-confirm",
                status: DiagnosisStatus::Warn,
                detail: "bytes do not frame as VB blocks".to_string(),
                code: Some("CBKF223_BDW_UNDERFLOW".to_string()),
                remediation: remediation_for("CBKF223_BDW_UNDERFLOW"),
                next: None,
            },
        ),
    }
}

/// Probe fixed, RDW, and VB framing as peer candidates. A single fit
/// resolves; no fit fails; several fits stay inconclusive (a fail finding
/// with the pin command) instead of secretly continuing as one of them.
fn probe_format(
    diagnosis: &mut Diagnosis,
    bytes: &[u8],
    scope_complete: bool,
    total_bytes: u64,
    lrecl: Option<u32>,
) -> Option<RecordFormat> {
    let fixed_fits = match lrecl {
        Some(len) => {
            let len = u64::from(len);
            len > 0 && total_bytes > 0 && total_bytes.is_multiple_of(len)
        }
        None => false,
    };
    let rdw_fits = read_rdw_records(bytes, 3, scope_complete, true).is_some();
    let vb_fits = read_vb_records(bytes, 3, scope_complete, true).is_some();
    // Text fits when the bytes carry line structure (at least one LF
    // terminator) and every line payload holds the known layout width (or
    // a single consistent width when the layout is variable). A lone
    // newline-free run of exactly `lrecl` bytes is fixed evidence, not
    // text evidence.
    let text_fits = {
        bytes.contains(&b'\n') && {
            let payloads = split_text_payloads(complete_text_prefix(bytes, scope_complete));
            !payloads.is_empty()
                && if let Some(len) = lrecl {
                    payloads.iter().all(|line| line.len() == len as usize)
                } else {
                    let first = payloads[0].len();
                    first > 0 && payloads.iter().all(|line| line.len() == first)
                }
        }
    };
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
    let mut fits: Vec<RecordFormat> = Vec::new();
    if fixed_fits {
        fits.push(RecordFormat::Fixed);
    }
    if rdw_fits {
        fits.push(RecordFormat::RDW);
    }
    if vb_fits {
        fits.push(RecordFormat::Vb);
    }
    if text_fits {
        fits.push(RecordFormat::Text);
    }
    match fits.as_slice() {
        [single] => {
            probe_pass(diagnosis, format_probe_name(*single));
            Some(*single)
        }
        [] => {
            push(&mut *diagnosis, DiagnosisFinding {
                check: "format-probe",
                status: DiagnosisStatus::Fail,
                detail: "bytes fit neither fixed, RDW, VB, nor text framing".to_string(),
                code: None,
                remediation: "Check the transfer mode (binary, not text), the record length, and whether the file carries BDW block headers."
                    .to_string(),
                next: None,
            });
            None
        }
        several => {
            let names: Vec<&str> = several
                .iter()
                .map(|format| format_probe_name(*format))
                .collect();
            let pins: Vec<String> = several
                .iter()
                .map(|format| format!("--format {}", format_flag_name(*format)))
                .collect();
            push(
                &mut *diagnosis,
                DiagnosisFinding {
                    check: "format-probe",
                    status: DiagnosisStatus::Fail,
                    detail: format!(
                        "bytes fit {} framing; rerun pinned, diagnosis stops here",
                        join_probe_names(&names),
                    ),
                    code: None,
                    remediation: format!("Rerun with {} to pin the framing.", pins.join(" or ")),
                    next: None,
                },
            );
            None
        }
    }
}

/// Display name of a framing candidate inside probe findings.
fn format_probe_name(format: RecordFormat) -> &'static str {
    match format {
        RecordFormat::Fixed => "fixed",
        RecordFormat::RDW => "RDW",
        RecordFormat::Vb => "VB",
        RecordFormat::Text => "text",
    }
}

/// CLI spelling of a framing candidate for suggested commands.
fn format_flag_name(format: RecordFormat) -> &'static str {
    match format {
        RecordFormat::Fixed => "fixed",
        RecordFormat::RDW => "rdw",
        RecordFormat::Vb => "vb",
        RecordFormat::Text => "text",
    }
}

/// Join candidate names the way the finding reads them.
fn join_probe_names(names: &[&str]) -> String {
    match names {
        [] => "no".to_string(),
        [single] => (*single).to_string(),
        [first, second] => format!("{first} and {second}"),
        [first, rest @ ..] => format!(
            "{first}, {} and {}",
            rest[..rest.len() - 1].join(", "),
            rest[rest.len() - 1]
        ),
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
        RecordFormat::RDW => {
            let Some(records) = read_rdw_records(bytes, sample as usize, scope_complete, false)
            else {
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
        RecordFormat::Vb => {
            let Some(records) = read_vb_records(bytes, sample as usize, scope_complete, false)
            else {
                push(
                    &mut *diagnosis,
                    DiagnosisFinding {
                        check: "trial-decode",
                        status: DiagnosisStatus::Fail,
                        detail: "could not frame VB blocks for trial decode".to_string(),
                        code: Some("CBKF223_BDW_UNDERFLOW".to_string()),
                        remediation: remediation_for("CBKF223_BDW_UNDERFLOW"),
                        next: None,
                    },
                );
                return None;
            };
            records
        }
        RecordFormat::Text => frame_text_records(diagnosis, bytes, scope_complete, lrecl, sample)?,
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

/// Trial-frame text bytes into payloads: every line must hold `lrecl`
/// bytes. A short/long line fails the trial with `CBKR101` instead of
/// padding or truncating silently.
fn frame_text_records(
    diagnosis: &mut Diagnosis,
    bytes: &[u8],
    scope_complete: bool,
    lrecl: Option<u32>,
    sample: u32,
) -> Option<Vec<Vec<u8>>> {
    let Some(width) = lrecl.filter(|len| *len > 0).map(|len| len as usize) else {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "trial-decode",
                status: DiagnosisStatus::Warn,
                detail: "variable-length layout; trial decode skipped".to_string(),
                code: None,
                remediation: "Run copybook decode directly; text lines need a known length."
                    .to_string(),
                next: None,
            },
        );
        return None;
    };
    let payloads = split_text_payloads(complete_text_prefix(bytes, scope_complete));
    if let Some(line) = payloads.iter().find(|line| line.len() != width) {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "trial-decode",
                status: DiagnosisStatus::Fail,
                detail: format!("text line holds {} bytes, expected {width}", line.len()),
                code: Some("CBKR101_FIXED_RECORD_ERROR".to_string()),
                remediation: remediation_for("CBKR101_FIXED_RECORD_ERROR"),
                next: None,
            },
        );
        return None;
    }
    Some(payloads.into_iter().take(sample as usize).collect())
}

/// Read up to `limit` RDW records; `None` when framing fails early.
/// When the inspected scope is truncated (`scope_complete` false), a framing
/// error after at least one record ends the scope instead of failing: the
/// tail was chosen away, not proven corrupt. A complete scope stays strict.
/// `strict` selects candidate-grade headers (zero reserved, no ragged tail)
/// for probing versus decode-faithful leniency for trial framing.
fn read_rdw_records(
    bytes: &[u8],
    limit: usize,
    scope_complete: bool,
    strict: bool,
) -> Option<Vec<Vec<u8>>> {
    let mut reader = RDWRecordReader::new(Cursor::new(bytes), strict);
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

/// Read up to `limit` VB payloads through BDW blocks; same scope contract
/// as [`read_rdw_records`]. VB is BDW plus nested RDW, never bare RDW.
fn read_vb_records(
    bytes: &[u8],
    limit: usize,
    scope_complete: bool,
    strict: bool,
) -> Option<Vec<Vec<u8>>> {
    let mut reader = VbBlockReader::new(Cursor::new(bytes), strict);
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

/// Probe all six codepages over a byte window. A high-confidence winner
/// resolves; a low-confidence round names its leading candidates and stays
/// unresolved (warn, never pass-as-certain). Returns the winner alongside
/// whether it is pinned: only pinned codepages silently configure the
/// trial decode.
fn probe_codepage(diagnosis: &mut Diagnosis, bytes: &[u8]) -> (Codepage, bool) {
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
    // Six candidates are always scored, so the runner-up exists.
    let (runner_up_codepage, runner_up_score) = scored[1];
    let margin = winner_score - runner_up_score;
    if margin >= CODEPAGE_CONFIDENCE_MARGIN {
        push(
            &mut *diagnosis,
            DiagnosisFinding {
                check: "codepage-probe",
                status: DiagnosisStatus::Pass,
                detail: format!(
                    "{winner} wins over {} bytes with high confidence (score {winner_score:.2}, margin {margin:.2}); heuristic, pass --codepage to pin it",
                    window.len(),
                ),
                code: None,
                remediation: String::new(),
                next: None,
            },
        );
        return (winner, true);
    }
    push(
        &mut *diagnosis,
        DiagnosisFinding {
            check: "codepage-probe",
            status: DiagnosisStatus::Warn,
            detail: format!(
                "no reliable winner over {} bytes; leading candidates {winner} ({winner_score:.2}) and {runner_up_codepage} ({runner_up_score:.2}); trial decode proceeds under {winner} without pinning it",
                window.len(),
            ),
            code: None,
            remediation: "Rerun with --codepage to pin the encoding.".to_string(),
            next: None,
        },
    );
    (winner, false)
}

/// Trial-decode the first `sample` records under the resolved options.
/// The first failure becomes a fail finding whose remediation comes from
/// the shared explain table. Returns how many records framed, whether all
/// of them decoded, and the largest observed wire length (payload plus the
/// 4-byte RDW header for RDW framing) for evidence.
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
    codepage_pinned: bool,
    lrecl: Option<u32>,
    sample: u32,
) -> TrialOutcome {
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
        return TrialOutcome {
            framed: 0,
            all_decoded: false,
            max_wire_len: None,
        };
    }
    let Some(records) = frame_records(
        &mut *diagnosis,
        bytes,
        scope_complete,
        format,
        lrecl,
        sample,
    ) else {
        return TrialOutcome {
            framed: 0,
            all_decoded: false,
            max_wire_len: None,
        };
    };
    let framed = u32::try_from(records.len()).unwrap_or(u32::MAX);
    let max_payload = records.iter().map(Vec::len).max();
    let max_wire_len = match (format, max_payload) {
        (RecordFormat::RDW, Some(payload)) => u64::try_from(payload)
            .ok()
            .and_then(|len| len.checked_add(4)),
        _ => None,
    };
    let outcome = |all_decoded: bool| TrialOutcome {
        framed,
        all_decoded,
        max_wire_len,
    };
    let options = DecodeOptions::new()
        .with_format(format)
        .with_codepage(codepage);
    for (index, record) in records.iter().enumerate() {
        if let Err(error) = decode_record(schema, record, &options) {
            push_trial_failure(
                &mut *diagnosis,
                copybook,
                input,
                format,
                codepage,
                codepage_pinned,
                index,
                records.len(),
                &error.to_string(),
                &error.code().to_string(),
            );
            return outcome(false);
        }
    }
    push_trial_success(
        &mut *diagnosis,
        copybook,
        input,
        format,
        codepage,
        codepage_pinned,
        records.len(),
    );
    outcome(true)
}

/// Record a trial-decode failure with its stable identity, remediation, and
/// the exact decode command that reproduces it.
#[allow(clippy::too_many_arguments)]
fn push_trial_failure(
    diagnosis: &mut Diagnosis,
    copybook: &Path,
    input: &Path,
    format: RecordFormat,
    codepage: Codepage,
    codepage_pinned: bool,
    index: usize,
    total: usize,
    rendered: &str,
    code: &str,
) {
    push(
        &mut *diagnosis,
        DiagnosisFinding {
            check: "trial-decode",
            status: DiagnosisStatus::Fail,
            detail: if codepage_pinned {
                format!(
                    "record {} of {total} failed: {}",
                    index + 1,
                    truncate_detail(rendered.to_string()),
                )
            } else {
                format!(
                    "record {} of {total} failed under unpinned codepage {codepage}: {}; rerun with --codepage to rule out a mismatch",
                    index + 1,
                    truncate_detail(rendered.to_string()),
                )
            },
            code: Some(code.to_string()),
            remediation: remediation_for(code),
            next: Some(suggested_decode(copybook, input, format, codepage)),
        },
    );
}

/// Record a trial-decode success, naming the corroboration when the
/// codepage was a leading candidate rather than a pinned choice.
fn push_trial_success(
    diagnosis: &mut Diagnosis,
    copybook: &Path,
    input: &Path,
    format: RecordFormat,
    codepage: Codepage,
    codepage_pinned: bool,
    total: usize,
) {
    push(
        &mut *diagnosis,
        DiagnosisFinding {
            check: "trial-decode",
            status: DiagnosisStatus::Pass,
            detail: if codepage_pinned {
                format!("first {total} record(s) decode")
            } else {
                format!(
                    "first {total} record(s) decode under {codepage}, corroborating the leading candidate without pinning it",
                )
            },
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
        shell_quote(copybook),
        shell_quote(input),
    )
}

/// Render a path for a pasted shell command: bare when it carries only
/// filename-safe characters, single-quoted otherwise, so a copied `next`
/// command survives directories such as `Monthly Extract/`.
///
/// Shared with the CLI hint renderers so every pasted command quotes paths
/// the same way; the unit tests below pin the behavior once.
#[inline]
#[must_use]
pub fn shell_quote(path: &Path) -> String {
    let text = path.display().to_string();
    if text
        .chars()
        .all(|c| c.is_alphanumeric() || "-_./:+=".contains(c))
    {
        text
    } else {
        format!("'{}'", text.replace('\'', "'\\''"))
    }
}

#[cfg(test)]
mod shell_quote_tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn plain_paths_stay_bare() {
        assert_eq!(shell_quote(Path::new("data/simple.bin")), "data/simple.bin");
        assert_eq!(
            shell_quote(Path::new("C:/extracts/a.cpy")),
            "C:/extracts/a.cpy"
        );
    }

    #[test]
    fn spaced_paths_are_single_quoted() {
        assert_eq!(
            shell_quote(Path::new("Monthly Extract/data file.bin")),
            "'Monthly Extract/data file.bin'"
        );
    }

    #[test]
    fn embedded_quotes_escape_safely() {
        assert_eq!(
            shell_quote(&PathBuf::from("it's/raw.bin")),
            "'it'\\''s/raw.bin'"
        );
    }
}
