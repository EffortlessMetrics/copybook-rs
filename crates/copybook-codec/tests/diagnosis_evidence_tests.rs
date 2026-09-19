// SPDX-License-Identifier: AGPL-3.0-or-later
//! `DiagnosisEvidence`: the probes establish machine-usable facts.
//!
//! A healthy explicit diagnosis (fixed framing, ASCII codepage) pins
//! format and codepage, reports the exact LRECL, and records a
//! successful trial; an RDW input with non-zero reserved bytes sets the
//! reserved observation; a garbage copybook leaves evidence at its
//! default so callers refuse instead of guessing.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::diagnose::{DiagnoseOptions, DiagnosisInput, diagnose};
use copybook_codec::{Codepage, RecordFormat};
use copybook_core::Dialect;
use std::path::Path;

/// 15-byte layout: 10 bytes name + 5 bytes amount.
const COPYBOOK: &str =
    "       01  REC.\n           05  NAME     PIC X(10).\n           05  AMOUNT   PIC 9(5).\n";
const RECORD: &[u8] = b"ALICE     00100";

fn options(format: Option<RecordFormat>, codepage: Option<Codepage>) -> DiagnoseOptions {
    DiagnoseOptions {
        format,
        codepage,
        sample: 3,
        strict_comments: false,
        dialect: Dialect::Normative,
    }
}

fn input(bytes: &[u8]) -> DiagnosisInput<'_> {
    DiagnosisInput {
        path: Path::new("evidence.bin"),
        prefix: bytes,
        total_bytes: bytes.len() as u64,
    }
}

#[test]
fn healthy_explicit_fixed_diagnosis_establishes_evidence() {
    let diagnosis = diagnose(
        COPYBOOK,
        Path::new("evidence.cpy"),
        Some(input(RECORD)),
        &options(Some(RecordFormat::Fixed), Some(Codepage::ASCII)),
    );
    assert!(
        !diagnosis.has_failures(),
        "diagnosis should pass, got: {:?}",
        diagnosis.findings
    );
    let evidence = &diagnosis.evidence;
    assert_eq!(evidence.format, Some(RecordFormat::Fixed));
    assert!(evidence.format_explicit);
    assert_eq!(evidence.codepage, Some(Codepage::ASCII));
    assert!(evidence.codepage_pinned);
    assert!(evidence.codepage_explicit);
    assert_eq!(evidence.record_length, Some(15));
    assert!(evidence.record_length_exact);
    assert!(!evidence.reserved_nonzero_observed);
    assert!(!evidence.variable_layout);
    assert!(evidence.trial_succeeded);
    assert_eq!(evidence.trial_records, 1);
}

#[test]
fn rdw_nonzero_reserved_bytes_are_observed() {
    // RDW header with reserved byte 0x01 plus the 15-byte payload.
    let mut bytes = vec![0x00, 0x13, 0x00, 0x01];
    bytes.extend_from_slice(RECORD);
    let diagnosis = diagnose(
        COPYBOOK,
        Path::new("evidence.cpy"),
        Some(input(&bytes)),
        &options(Some(RecordFormat::RDW), Some(Codepage::ASCII)),
    );
    assert!(diagnosis.evidence.reserved_nonzero_observed);
    assert_eq!(diagnosis.evidence.format, Some(RecordFormat::RDW));
}

#[test]
fn garbage_copybook_leaves_evidence_default() {
    let diagnosis = diagnose(
        "THIS IS NOT A COPYBOOK ((( ",
        Path::new("bad.cpy"),
        Some(input(RECORD)),
        &options(Some(RecordFormat::Fixed), Some(Codepage::ASCII)),
    );
    assert!(diagnosis.has_failures());
    assert_eq!(diagnosis.evidence.format, None);
    assert_eq!(diagnosis.evidence.codepage, None);
    assert!(!diagnosis.evidence.trial_succeeded);
}
