// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-safe-index.

use copybook_error::ErrorCode;
use copybook_safe_index::{safe_divide, safe_slice_get};
use std::panic::catch_unwind;

// ── safe_divide ─────────────────────────────────────────────────────

#[test]
fn divide_exact() {
    assert_eq!(safe_divide(20, 4, "ctx").unwrap(), 5);
}

#[test]
fn divide_with_remainder_truncates() {
    assert_eq!(safe_divide(7, 3, "ctx").unwrap(), 2);
}

#[test]
fn divide_zero_numerator() {
    assert_eq!(safe_divide(0, 100, "ctx").unwrap(), 0);
}

#[test]
fn divide_by_zero_returns_syntax_error() {
    let err = safe_divide(42, 0, "divide-ctx").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    assert!(err.message.contains("divide-ctx"));
}

#[test]
fn divide_max_by_one() {
    assert_eq!(safe_divide(usize::MAX, 1, "ctx").unwrap(), usize::MAX);
}

#[test]
fn divide_max_by_max() {
    assert_eq!(safe_divide(usize::MAX, usize::MAX, "ctx").unwrap(), 1);
}

#[test]
fn divide_one_by_max() {
    assert_eq!(safe_divide(1, usize::MAX, "ctx").unwrap(), 0);
}

#[test]
fn divide_never_panics_on_zero() {
    let result = catch_unwind(|| safe_divide(usize::MAX, 0, "panic-test"));
    assert!(
        result.is_ok(),
        "safe_divide must not panic on division by zero"
    );
    assert!(result.unwrap().is_err());
}

// ── safe_slice_get ──────────────────────────────────────────────────

#[test]
fn slice_get_first() {
    let data = [100u32, 200, 300];
    assert_eq!(safe_slice_get(&data, 0, "ctx").unwrap(), 100);
}

#[test]
fn slice_get_last() {
    let data = [100u32, 200, 300];
    assert_eq!(safe_slice_get(&data, 2, "ctx").unwrap(), 300);
}

#[test]
fn slice_get_empty_returns_error() {
    let data: &[u8] = &[];
    let err = safe_slice_get(data, 0, "empty-ctx").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    assert!(err.message.contains("empty-ctx"));
}

#[test]
fn slice_get_exactly_at_length_is_error() {
    let data = [1u8, 2, 3];
    assert!(safe_slice_get(&data, 3, "ctx").is_err());
}

#[test]
fn slice_get_way_past_end() {
    let data = [1u8];
    assert!(safe_slice_get(&data, usize::MAX, "ctx").is_err());
}

#[test]
fn slice_get_single_element() {
    let data = [42i64];
    assert_eq!(safe_slice_get(&data, 0, "ctx").unwrap(), 42);
}

#[test]
fn slice_get_bool_type() {
    let data = [true, false, true];
    assert!(!safe_slice_get(&data, 1, "ctx").unwrap());
}

#[test]
fn slice_get_never_panics_on_empty() {
    let result = catch_unwind(|| {
        let empty: &[u8] = &[];
        safe_slice_get(empty, 0, "panic-test")
    });
    assert!(
        result.is_ok(),
        "safe_slice_get must not panic on empty slice"
    );
    assert!(result.unwrap().is_err());
}

#[test]
fn slice_get_never_panics_on_max_index() {
    let result = catch_unwind(|| {
        let data = [1u8, 2, 3];
        safe_slice_get(&data, usize::MAX, "panic-test")
    });
    assert!(
        result.is_ok(),
        "safe_slice_get must not panic on usize::MAX index"
    );
    assert!(result.unwrap().is_err());
}

#[test]
fn slice_get_error_message_contains_index_and_length() {
    let data = [1u8, 2];
    let err = safe_slice_get(&data, 5, "my-ctx").unwrap_err();
    assert!(err.message.contains('5'), "error should contain the index");
    assert!(err.message.contains('2'), "error should contain the length");
}
