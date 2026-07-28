// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI command-context evidence for deliberate rejections (issue #576).
//!
//! `copybook-cli/tests/exit_code_mapping.rs` already covers the mapped error
//! families (`CBKD`→2, `CBKE`→3, `CBKF`→4, `CBKI`→5). This suite asserts the
//! mapped behavior for **structural** parse/schema rejections (`CBKP*` / `CBKS*`):
//! they are surfaced as exit code **3** via the `CBKE` mapping.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use assert_fs::prelude::*;
use common::bin;
use predicates::str::contains;

/// Documented structural-rejection exit code (mapped to `CBKE`).
const STRUCTURAL_REJECTION_EXIT: i32 = 3;

fn path_str(p: &std::path::Path) -> String {
    p.to_string_lossy().into_owned()
}

/// Run `copybook parse` on `copybook_text` and assert the process exit code.
fn assert_parse_exit(copybook_text: &str, expected_code: i32, expected_error: &str) {
    let dir = assert_fs::TempDir::new().unwrap();
    let cpy = dir.child("rej.cpy");
    cpy.write_str(copybook_text).unwrap();

    bin()
        .args(["parse", &path_str(cpy.path())])
        .assert()
        .failure()
        .code(expected_code)
        .stderr(contains(expected_error));
}

// ====================================================================
// CBKP* structural parse rejections (mapped to exit 3 / CBKE)
// ====================================================================

#[test]
fn cli_odo_not_tail_exit_code() {
    // O4 / CBKP021_ODO_NOT_TAIL
    assert_parse_exit(
        "01 INV-REC.\n   05 ITEM-COUNT PIC 9(3).\n   05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.\n      10 ITEM-CODE PIC X(4).\n   05 TRAILER PIC X(5).\n",
        STRUCTURAL_REJECTION_EXIT,
        "CBKP021_ODO_NOT_TAIL",
    );
}

#[test]
fn cli_nested_odo_exit_code() {
    // O5 / CBKP022_NESTED_ODO
    assert_parse_exit(
        "01 OUTER-REC.\n   05 OUTER-COUNT PIC 9(2).\n   05 OUTER-GROUP OCCURS 1 TO 50 TIMES DEPENDING ON OUTER-COUNT.\n      10 INNER-COUNT PIC 9(2).\n      10 INNER-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON INNER-COUNT.\n         15 DATA-VALUE PIC X(10).\n",
        STRUCTURAL_REJECTION_EXIT,
        "CBKP022_NESTED_ODO",
    );
}

#[test]
fn cli_odo_over_redefines_exit_code() {
    // O6 / CBKP023_ODO_REDEFINES
    assert_parse_exit(
        "01 TRANSACTION-REC.\n   05 TRANS-TYPE PIC X(1).\n   05 TRANS-COUNT PIC 9(2).\n   05 TRANS-DATA PIC X(100).\n   05 TRANS-DETAIL REDEFINES TRANS-DATA.\n      10 DETAIL-ITEM OCCURS 1 TO 100 TIMES DEPENDING ON TRANS-COUNT.\n         15 DETAIL-FIELD PIC X(10).\n",
        STRUCTURAL_REJECTION_EXIT,
        "CBKP023_ODO_REDEFINES",
    );
}

// ====================================================================
// CBKS* schema/resolver parse rejection (mapped to exit 3 / CBKE)
// ====================================================================

#[test]
fn cli_renames_over_occurs_exit_code() {
    // renames-occurs / CBKS607_RENAME_CROSSES_OCCURS
    assert_parse_exit(
        "01 ROOT-REC.\n   05 FIELD-A PIC X(5).\n   05 ARRAY-FIELD PIC 9(3) OCCURS 5 TIMES.\n   05 FIELD-B PIC X(2).\n   66 ALIAS RENAMES FIELD-A THRU FIELD-B.\n",
        STRUCTURAL_REJECTION_EXIT,
        "CBKS607_RENAME_CROSSES_OCCURS",
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
