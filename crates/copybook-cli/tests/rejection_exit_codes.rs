// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI command-context evidence for deliberate rejections (issue #576).
//!
//! `copybook-cli/tests/exit_code_mapping.rs` already covers the mapped error
//! families (`CBKD`→2, `CBKE`→3, `CBKF`→4, `CBKI`→5). This suite fills the
//! documented gap for **structural** rejections: parse/schema errors in the
//! `CBKP*` / `CBKS*` families are not present in `ExitCode::from_family_prefix`
//! (`crates/copybook-cli/src/exit_codes.rs`), so they fall through to exit code
//! **5 (Internal orchestration error)** — the same code a genuine panic
//! produces.
//!
//! These tests pin that **current, observed** behavior so a regression is
//! visible, and document the mismatch against `docs/reference/COBOL_SUPPORT_MATRIX.md`
//! (which assigns each scenario a stable `CBKP*`/`CBKS*` code). Whether these
//! structural rejections should map to a dedicated, non-`Internal` exit code is
//! a stability-contract decision left to the maintainers (see the PR notes); the
//! library layer already reports the precise, stable code — see
//! `copybook-codec/tests/rejection_evidence_matrix.rs`.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use assert_fs::prelude::*;
use common::bin;

/// Documented structural-rejection exit code (Internal). Named so the intent —
/// and the fact that it collides with the panic exit code — is explicit.
const STRUCTURAL_REJECTION_EXIT: i32 = 5;

fn path_str(p: &std::path::Path) -> String {
    p.to_string_lossy().into_owned()
}

/// Run `copybook parse` on `copybook_text` and assert the process exit code.
fn assert_parse_exit(copybook_text: &str, expected_code: i32) {
    let dir = assert_fs::TempDir::new().unwrap();
    let cpy = dir.child("rej.cpy");
    cpy.write_str(copybook_text).unwrap();

    bin()
        .args(["parse", &path_str(cpy.path())])
        .assert()
        .failure()
        .code(expected_code);
}

// ====================================================================
// CBKP* structural parse rejections (currently exit 5 / Internal)
// ====================================================================

#[test]
fn cli_odo_not_tail_exit_code() {
    // O4 / CBKP021_ODO_NOT_TAIL
    assert_parse_exit(
        "01 INV-REC.\n   05 ITEM-COUNT PIC 9(3).\n   05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.\n      10 ITEM-CODE PIC X(4).\n   05 TRAILER PIC X(5).\n",
        STRUCTURAL_REJECTION_EXIT,
    );
}

#[test]
fn cli_nested_odo_exit_code() {
    // O5 / CBKP022_NESTED_ODO
    assert_parse_exit(
        "01 OUTER-REC.\n   05 OUTER-COUNT PIC 9(2).\n   05 OUTER-GROUP OCCURS 1 TO 50 TIMES DEPENDING ON OUTER-COUNT.\n      10 INNER-COUNT PIC 9(2).\n      10 INNER-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON INNER-COUNT.\n         15 DATA-VALUE PIC X(10).\n",
        STRUCTURAL_REJECTION_EXIT,
    );
}

#[test]
fn cli_odo_over_redefines_exit_code() {
    // O6 / CBKP023_ODO_REDEFINES
    assert_parse_exit(
        "01 TRANSACTION-REC.\n   05 TRANS-TYPE PIC X(1).\n   05 TRANS-COUNT PIC 9(2).\n   05 TRANS-DATA PIC X(100).\n   05 TRANS-DETAIL REDEFINES TRANS-DATA.\n      10 DETAIL-ITEM OCCURS 1 TO 100 TIMES DEPENDING ON TRANS-COUNT.\n         15 DETAIL-FIELD PIC X(10).\n",
        STRUCTURAL_REJECTION_EXIT,
    );
}

// ====================================================================
// CBKS* schema/resolver parse rejection (currently exit 5 / Internal)
// ====================================================================

#[test]
fn cli_renames_over_occurs_exit_code() {
    // renames-occurs / CBKS607_RENAME_CROSSES_OCCURS
    assert_parse_exit(
        "01 ROOT-REC.\n   05 FIELD-A PIC X(5).\n   05 ARRAY-FIELD PIC 9(3) OCCURS 5 TIMES.\n   05 FIELD-B PIC X(2).\n   66 ALIAS RENAMES FIELD-A THRU FIELD-B.\n",
        STRUCTURAL_REJECTION_EXIT,
    );
}

// ====================================================================
// Control: a valid copybook parses successfully (exit 0), proving the
// rejection exit codes above are triggered by the defect, not by the
// harness.
// ====================================================================

#[test]
fn cli_valid_copybook_parses_successfully() {
    let dir = assert_fs::TempDir::new().unwrap();
    let cpy = dir.child("ok.cpy");
    cpy.write_str("01 REC.\n   05 ITEM-COUNT PIC 9(3).\n   05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.\n      10 ITEM-CODE PIC X(4).\n")
        .unwrap();
    bin()
        .args(["parse", &path_str(cpy.path())])
        .assert()
        .success();
}
