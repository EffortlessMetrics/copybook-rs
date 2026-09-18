// SPDX-License-Identifier: AGPL-3.0-or-later
//! Coverage tests for the generated `explain` operator knowledge table.
//!
//! Every [`ErrorCode`] variant must resolve through
//! [`explanation_for`](copybook_error::explain::explanation_for) by both its
//! full identity (`CBKE501_JSON_TYPE_MISMATCH`) and its short code
//! (`CBKE501`), and every table row must name a real variant. The
//! `explain-knowledge` verify lane pins the table to
//! `docs/reference/ERROR_CODES.md`; these tests pin the lookup behavior.

#![allow(clippy::unwrap_used)]

use copybook_error::explain::{EXPLANATIONS, explanation_for};
use std::collections::HashSet;

mod all_codes;
use all_codes::all_error_codes;

#[test]
fn every_error_code_has_an_explanation() {
    let mut missing = Vec::new();
    for code in all_error_codes() {
        let full = format!("{code}");
        if explanation_for(&full).is_none() {
            missing.push(full);
        }
    }
    assert!(
        missing.is_empty(),
        "error codes without explain knowledge: {missing:?}"
    );
}

#[test]
fn every_explanation_names_a_real_error_code() {
    let known: HashSet<String> = all_error_codes()
        .iter()
        .map(|code| format!("{code}"))
        .collect();
    let mut unknown = Vec::new();
    for row in EXPLANATIONS {
        if !known.contains(row.code) {
            unknown.push(row.code.to_string());
        }
    }
    assert!(
        unknown.is_empty(),
        "explain rows naming unknown codes: {unknown:?}"
    );
}

#[test]
fn short_codes_resolve_case_insensitively() {
    let row = explanation_for("cbke501").expect("short code resolves");
    assert_eq!(row.code, "CBKE501_JSON_TYPE_MISMATCH");
    let same = explanation_for("CBKE501_JSON_TYPE_MISMATCH").expect("full code resolves");
    assert_eq!(same.code, row.code);
}

#[test]
fn unknown_identities_resolve_to_none() {
    assert!(explanation_for("CBKX000_MISSING").is_none());
    assert!(explanation_for("").is_none());
    assert!(explanation_for("not a code").is_none());
}

#[test]
fn explanation_table_is_sorted_and_unique() {
    let mut seen = HashSet::new();
    let mut previous: Option<&str> = None;
    for row in EXPLANATIONS {
        assert!(seen.insert(row.code), "duplicate explain row: {}", row.code);
        if let Some(prev) = previous {
            assert!(
                prev < row.code,
                "explain table not sorted: {prev} before {}",
                row.code
            );
        }
        previous = Some(row.code);
        assert!(
            !row.severity.trim().is_empty(),
            "empty severity: {}",
            row.code
        );
        assert!(
            !row.resolution.trim().is_empty(),
            "empty resolution: {}",
            row.code
        );
    }
}
