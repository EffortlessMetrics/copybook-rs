// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::missing_inline_in_public_items)]
//! Occurrence-bound failure explanation: why this record failed here.
//!
//! Identity explanation (`copybook explain CBKD401…`) says what a code
//! means in general. This module answers the operator follow-up: given a
//! copybook and data, re-run decoding to the failing record and report the
//! strongest real context available — record index, physical file offset,
//! field path, field byte range, and representation — reusing the exact
//! [`crate::RecordIterator`] and decode path verification uses, so the
//! explained failure is the failure the operator saw. Anything that cannot
//! be established stays unknown; nothing is fabricated.

use crate::lib_api::decode_record_with_raw_data;
use crate::{Codepage, DecodeOptions, RecordFormat, RecordIterator};
use copybook_core::{FieldKind, Schema};
use std::io::Read;

/// Records scanned without a target before the search stops scoped.
pub const OCCURRENCE_SCAN_CAP: u64 = 1000;

/// Inputs selecting which failure to explain.
#[derive(Clone, Debug)]
pub struct OccurrenceOptions {
    /// Framing under evaluation.
    pub format: RecordFormat,
    /// Codepage under evaluation.
    pub codepage: Codepage,
    /// 1-based record to explain; `None` explains the first failure.
    pub target_record: Option<u64>,
    /// Canonical full identity to filter by; `None` takes any failure.
    pub code_filter: Option<String>,
}

/// A located failure with its strongest real context.
#[derive(Clone, Debug)]
pub struct Occurrence {
    /// 1-based record index.
    pub record_index: u64,
    /// Physical file offset of the record start, when derivable
    /// (fixed and RDW; VB block nesting leaves this unknown).
    pub physical_offset: Option<u64>,
    /// Hierarchical field path from the error context, when present.
    pub field_path: Option<String>,
    /// Record-relative byte range of the field from schema layout.
    pub field_range: Option<(u32, u32)>,
    /// Human representation of the field (`S9(7)V99 COMP-3`).
    pub representation: Option<String>,
    /// Stable error identity.
    pub code: String,
    /// Failure message.
    pub message: String,
    /// Framing under evaluation.
    pub format: RecordFormat,
    /// Codepage under evaluation.
    pub codepage: Codepage,
    /// Records scanned to reach this failure.
    pub records_scanned: u64,
}

/// Why no failure was explained.
#[derive(Clone, Debug)]
pub enum OccurrenceAbsence {
    /// The target record decoded without errors.
    CleanRecord {
        /// Requested 1-based record.
        record: u64,
    },
    /// The target record does not exist.
    BeyondEnd {
        /// Requested 1-based record.
        requested: u64,
        /// Records present.
        present: u64,
    },
    /// No failure (or none matching the filter) in the scanned scope.
    /// `limit` bounds the work; the file may hold later failures.
    NotFound {
        /// Records scanned.
        scanned: u64,
        /// Scan bound that stopped the search, when hit.
        limit: Option<u64>,
        /// Distinct identities observed but excluded by the code filter,
        /// oldest first, so callers can name what was seen instead of
        /// reporting an empty scope.
        seen: Vec<String>,
    },
}

/// The outcome of [`explain_occurrence`].
#[derive(Clone, Debug)]
pub enum OccurrenceOutcome {
    /// A failure with its strongest real context.
    Found(Occurrence),
    /// No failure to explain, with the reason.
    Absent(OccurrenceAbsence),
}

/// Re-run decoding to the selected failure and locate it.
///
/// Streams `reader` through the same iterator and decode entry point
/// verification uses. The scan stops at the first failure matching the
/// options (or the target record), or at [`OCCURRENCE_SCAN_CAP`] records
/// when searching without a target, so an unbounded file cannot make one
/// explanation read forever.
///
/// # Errors
///
/// Returns [`copybook_core::Error`] when the record stream cannot be
/// opened (unsupported framing for the schema) or an I/O failure stops
/// the scan; per-record decode failures are reported as
/// [`OccurrenceOutcome::Found`], not errors.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn explain_occurrence<R: Read>(
    reader: R,
    schema: &Schema,
    decode_options: &DecodeOptions,
    options: &OccurrenceOptions,
) -> copybook_core::Result<OccurrenceOutcome> {
    let mut iterator = RecordIterator::new(reader, schema, decode_options)?;
    let lrecl = schema.lrecl_fixed.map(u64::from);
    let mut scanned = 0u64;
    let mut rdw_offset = 0u64;
    let mut seen: Vec<String> = Vec::new();
    let mut target_filtered = false;
    let scan_limit = options.target_record.unwrap_or(OCCURRENCE_SCAN_CAP);

    loop {
        if scanned >= scan_limit {
            return Ok(OccurrenceOutcome::Absent(OccurrenceAbsence::NotFound {
                scanned,
                limit: Some(scan_limit),
                seen,
            }));
        }
        match iterator.read_raw_record() {
            Ok(None) => {
                return Ok(finish_at_eof(
                    scanned,
                    options.target_record,
                    target_filtered,
                    seen,
                ));
            }
            Err(error) => {
                let occurrence =
                    framing_occurrence(&error, scanned, rdw_offset, lrecl, options, scanned + 1);
                if let Some(occurrence) = occurrence {
                    return Ok(OccurrenceOutcome::Found(occurrence));
                }
                if !code_matches(&error, options.code_filter.as_deref()) {
                    note_seen(&mut seen, &error);
                }
                return Ok(OccurrenceOutcome::Absent(OccurrenceAbsence::NotFound {
                    scanned: scanned + 1,
                    limit: None,
                    seen,
                }));
            }
            Ok(Some(payload)) => {
                scanned += 1;
                let record_index = iterator.current_record_index().max(scanned);
                let physical_offset = match options.format {
                    RecordFormat::Fixed => lrecl.map(|len| (record_index - 1) * len),
                    RecordFormat::RDW => Some(rdw_offset),
                    RecordFormat::Vb => None,
                };
                rdw_offset += 4 + payload.len() as u64;
                match decode_record_with_raw_data(
                    schema,
                    &payload,
                    decode_options,
                    None,
                    record_index,
                ) {
                    Ok(_) => {
                        if options.target_record == Some(record_index) {
                            return Ok(OccurrenceOutcome::Absent(OccurrenceAbsence::CleanRecord {
                                record: record_index,
                            }));
                        }
                    }
                    Err(error) => {
                        let record_index = error
                            .context
                            .as_ref()
                            .and_then(|context| context.record_index)
                            .unwrap_or(record_index);
                        if !code_matches(&error, options.code_filter.as_deref()) {
                            note_seen(&mut seen, &error);
                            // The target record failed with a different
                            // identity: remember, so end-of-input reports a
                            // filtered scope instead of a clean record.
                            if options.target_record == Some(record_index) {
                                target_filtered = true;
                            }
                            continue;
                        }
                        if let Some(target) = options.target_record
                            && target != record_index
                        {
                            continue;
                        }
                        return Ok(OccurrenceOutcome::Found(decode_occurrence(
                            schema,
                            &error,
                            record_index,
                            physical_offset,
                            options,
                            scanned,
                        )));
                    }
                }
            }
        }
    }
}

/// Terminal outcome when the input ends: the target is clean, missing, or
/// no failure appeared in scope. A target that failed under a filtered-out
/// identity is a filtered scope, never a clean record.
fn finish_at_eof(
    scanned: u64,
    target_record: Option<u64>,
    target_filtered: bool,
    seen: Vec<String>,
) -> OccurrenceOutcome {
    match target_record {
        Some(target) if target <= scanned && !target_filtered => {
            OccurrenceOutcome::Absent(OccurrenceAbsence::CleanRecord { record: target })
        }
        Some(target) if target <= scanned => {
            OccurrenceOutcome::Absent(OccurrenceAbsence::NotFound {
                scanned,
                limit: None,
                seen,
            })
        }
        Some(target) => OccurrenceOutcome::Absent(OccurrenceAbsence::BeyondEnd {
            requested: target,
            present: scanned,
        }),
        None => OccurrenceOutcome::Absent(OccurrenceAbsence::NotFound {
            scanned,
            limit: None,
            seen,
        }),
    }
}

/// Remember one filtered-out identity, oldest first, bounded so a large
/// file cannot grow the absence report without bound.
fn note_seen(seen: &mut Vec<String>, error: &copybook_core::Error) {
    const SEEN_CAP: usize = 5;
    if seen.len() >= SEEN_CAP {
        return;
    }
    let code = error.code().to_string();
    if !seen.contains(&code) {
        seen.push(code);
    }
}

/// Whether the error matches the requested identity filter.
fn code_matches(error: &copybook_core::Error, filter: Option<&str>) -> bool {
    match filter {
        None => true,
        Some(wanted) => error.code().to_string() == wanted,
    }
}

/// Build an occurrence from a framing error, which carries no decoded
/// field identity: field slots stay unknown.
fn framing_occurrence(
    error: &copybook_core::Error,
    scanned: u64,
    rdw_offset: u64,
    lrecl: Option<u64>,
    options: &OccurrenceOptions,
    record_index: u64,
) -> Option<Occurrence> {
    if !code_matches(error, options.code_filter.as_deref()) {
        return None;
    }
    if let Some(target) = options.target_record
        && target != record_index
    {
        return None;
    }
    let code = error.code().to_string();
    Some(Occurrence {
        record_index: error
            .context
            .as_ref()
            .and_then(|context| context.record_index)
            .unwrap_or(record_index),
        physical_offset: match options.format {
            RecordFormat::Fixed => lrecl.map(|len| (record_index - 1) * len),
            RecordFormat::RDW => Some(rdw_offset),
            RecordFormat::Vb => None,
        },
        field_path: error
            .context
            .as_ref()
            .and_then(|context| context.field_path.clone()),
        field_range: None,
        representation: None,
        code,
        message: error.message.clone(),
        format: options.format,
        codepage: options.codepage,
        records_scanned: scanned + 1,
    })
}

/// Build an occurrence from a decode failure, resolving the field extent
/// and representation from schema layout when the context names a field.
fn decode_occurrence(
    schema: &Schema,
    error: &copybook_core::Error,
    record_index: u64,
    physical_offset: Option<u64>,
    options: &OccurrenceOptions,
    scanned: u64,
) -> Occurrence {
    let field_path = error
        .context
        .as_ref()
        .and_then(|context| context.field_path.clone());
    let field = field_path
        .as_deref()
        .and_then(|path| schema.find_field(path));
    let (field_range, representation) = match field {
        Some(field) => (
            Some((field.offset, field.len)),
            representation_for(&field.kind),
        ),
        None => (None, None),
    };
    Occurrence {
        record_index,
        physical_offset,
        field_path,
        field_range,
        representation,
        code: error.code().to_string(),
        message: error.message.clone(),
        format: options.format,
        codepage: options.codepage,
        records_scanned: scanned,
    }
}

/// Human representation of a field kind (`S9(7)V99 COMP-3`).
/// Returns `None` for kinds with no data representation.
fn representation_for(kind: &FieldKind) -> Option<String> {
    match kind {
        FieldKind::Alphanum { len } => Some(format!("X({len})")),
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            ..
        } => Some(format!("{} (zoned)", pic_nines(*digits, *scale, *signed))),
        FieldKind::BinaryInt { bits, signed } => {
            let digits = match bits {
                16 => 4,
                32 => 9,
                _ => 18,
            };
            Some(format!("{} COMP", pic_nines(digits, 0, *signed)))
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => Some(format!("{} COMP-3", pic_nines(*digits, *scale, *signed))),
        FieldKind::EditedNumeric { pic_string, .. } => Some(format!("{pic_string} (edited)")),
        FieldKind::FloatSingle => Some("COMP-1".to_string()),
        FieldKind::FloatDouble => Some("COMP-2".to_string()),
        FieldKind::Group | FieldKind::Condition { .. } | FieldKind::Renames { .. } => None,
    }
}

/// Best-effort `9`-picture for digit counts (`S9(5)V99`).
fn pic_nines(digits: u16, scale: i16, signed: bool) -> String {
    let sign = if signed { "S" } else { "" };
    let digit = |count: i32| {
        if count <= 0 {
            String::new()
        } else if count <= 2 {
            "9".repeat(usize::try_from(count).unwrap_or_default())
        } else {
            format!("9({count})")
        }
    };
    match scale.cmp(&0) {
        std::cmp::Ordering::Equal => format!("{sign}{}", digit(i32::from(digits))),
        std::cmp::Ordering::Greater => {
            let scale = i32::from(scale);
            let integer = i32::from(digits) - scale;
            if integer <= 0 {
                format!("{sign}V{}", digit(scale))
            } else {
                format!("{sign}{}V{}", digit(integer), digit(scale))
            }
        }
        std::cmp::Ordering::Less => {
            format!("{sign}{}P({})", digit(i32::from(digits)), -scale)
        }
    }
}
