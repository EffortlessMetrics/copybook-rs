// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-utils.
//!
//! Covers all public utility functions, edge cases (empty strings, max-length,
//! Unicode), panic-safety, and extension trait behavior on all applicable types.

#![allow(clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_utils::{OptionExt, SliceExt, VecExt};
use std::panic::catch_unwind;

// ====================================================================
// OptionExt – edge cases
// ====================================================================

#[test]
fn option_ext_some_with_unit_type() {
    let opt: Option<()> = Some(());
    assert!(opt.ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx").is_ok());
}

#[test]
fn option_ext_none_with_unicode_message() {
    let err = Option::<i32>::None
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "字段未找到 — フィールド")
        .unwrap_err();
    assert!(err.message.contains('字'));
    assert!(err.message.contains('フ'));
}

#[test]
fn option_ext_some_with_large_string() {
    let big = "X".repeat(100_000);
    let val = Some(big.clone())
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    assert_eq!(val.len(), 100_000);
}

#[test]
fn option_ext_ok_or_error_preserves_context() {
    let err = copybook_error::Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "ctx")
        .with_record(42)
        .with_field("FIELD");
    let result: Result<i32, _> = None.ok_or_error(err);
    let got = result.unwrap_err();
    let ctx = got.context.as_ref().unwrap();
    assert_eq!(ctx.record_index, Some(42));
    assert_eq!(ctx.field_path.as_deref(), Some("FIELD"));
}

#[test]
fn option_ext_none_error_code_propagated_to_all_families() {
    let families = [
        ErrorCode::CBKP001_SYNTAX,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ErrorCode::CBKD301_RECORD_TOO_SHORT,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
        ErrorCode::CBKF102_RECORD_LENGTH_INVALID,
    ];
    for code in families {
        let err = Option::<()>::None
            .ok_or_cbkp_error(code, "test")
            .unwrap_err();
        assert_eq!(err.code, code);
    }
}

// ====================================================================
// VecExt – edge cases
// ====================================================================

#[test]
fn vec_pop_with_boxed_values() {
    let mut v: Vec<Box<dyn std::fmt::Debug + Send>> = vec![Box::new(1), Box::new("hello")];
    let val = v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx").unwrap();
    let dbg = format!("{val:?}");
    assert!(dbg.contains("hello"));
}

#[test]
fn vec_last_with_single_element_returns_that_element() {
    let v = vec![42];
    assert_eq!(
        *v.last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx").unwrap(),
        42
    );
}

#[test]
fn vec_last_mut_modifies_only_last() {
    let mut v = vec![1, 2, 3, 4, 5];
    *v.last_mut_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx").unwrap() = 999;
    assert_eq!(v, vec![1, 2, 3, 4, 999]);
}

#[test]
fn vec_pop_error_message_is_preserved() {
    let mut v: Vec<i32> = vec![];
    let err = v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "custom: stack empty").unwrap_err();
    assert_eq!(err.message, "custom: stack empty");
}

// ====================================================================
// SliceExt – edge cases
// ====================================================================

#[test]
fn slice_get_boundary_last_valid_index() {
    let data = [10, 20, 30, 40, 50];
    assert_eq!(
        *data.get_or_cbkp_error(4, ErrorCode::CBKP001_SYNTAX, "ctx").unwrap(),
        50
    );
}

#[test]
fn slice_get_boundary_first_invalid_index() {
    let data = [10, 20, 30];
    assert!(data.get_or_cbkp_error(3, ErrorCode::CBKP001_SYNTAX, "ctx").is_err());
}

#[test]
fn slice_get_mut_boundary_first_element() {
    let mut data = [100u8, 200];
    *data.get_mut_or_cbkp_error(0, ErrorCode::CBKP001_SYNTAX, "ctx").unwrap() = 0;
    assert_eq!(data, [0, 200]);
}

#[test]
fn slice_get_on_vec_via_deref() {
    let v = vec![1, 2, 3];
    let slice: &[i32] = &v;
    assert_eq!(
        *slice.get_or_cbkp_error(1, ErrorCode::CBKP001_SYNTAX, "ctx").unwrap(),
        2
    );
}

#[test]
fn slice_get_with_zero_length_slice_and_zero_index() {
    let data: &[u8] = &[];
    assert!(data.get_or_cbkp_error(0, ErrorCode::CBKP001_SYNTAX, "ctx").is_err());
}

// ====================================================================
// safe_ops – comprehensive edge cases
// ====================================================================

#[test]
fn safe_ops_parse_usize_max_value() {
    let max_str = usize::MAX.to_string();
    let val = copybook_utils::safe_ops::parse_usize(&max_str, "ctx").unwrap();
    assert_eq!(val, usize::MAX);
}

#[test]
fn safe_ops_parse_usize_leading_zeros() {
    let val = copybook_utils::safe_ops::parse_usize("007", "ctx").unwrap();
    assert_eq!(val, 7);
}

#[test]
fn safe_ops_parse_usize_whitespace_rejects() {
    assert!(copybook_utils::safe_ops::parse_usize(" 5", "ctx").is_err());
    assert!(copybook_utils::safe_ops::parse_usize("5 ", "ctx").is_err());
}

#[test]
fn safe_ops_parse_usize_unicode_digit_rejects() {
    // U+0661 is Arabic-Indic digit one — must reject
    assert!(copybook_utils::safe_ops::parse_usize("\u{0661}", "ctx").is_err());
}

#[test]
fn safe_ops_safe_divide_large_values() {
    let val = copybook_utils::safe_ops::safe_divide(usize::MAX, 1, "ctx").unwrap();
    assert_eq!(val, usize::MAX);
}

#[test]
fn safe_ops_safe_array_bound_zero_count_zero_item_size() {
    let val = copybook_utils::safe_ops::safe_array_bound(100, 0, 0, "ctx").unwrap();
    assert_eq!(val, 100);
}

#[test]
fn safe_ops_safe_array_bound_all_zeros() {
    let val = copybook_utils::safe_ops::safe_array_bound(0, 0, 0, "ctx").unwrap();
    assert_eq!(val, 0);
}

// ====================================================================
// Panic-safety guarantees
// ====================================================================

#[test]
fn no_panic_on_empty_vec_last_mut() {
    let result = catch_unwind(|| {
        let mut v: Vec<i32> = vec![];
        let _ = v.last_mut_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "safe");
    });
    assert!(result.is_ok(), "VecExt::last_mut must not panic on empty");
}

#[test]
fn no_panic_on_slice_get_mut_oob() {
    let result = catch_unwind(|| {
        let mut data = [1u8, 2];
        let _ = data.get_mut_or_cbkp_error(usize::MAX, ErrorCode::CBKP001_SYNTAX, "safe");
    });
    assert!(result.is_ok(), "SliceExt::get_mut must not panic on OOB");
}

#[test]
fn no_panic_on_option_ok_or_error_with_none() {
    let result = catch_unwind(|| {
        let err = copybook_error::Error::new(ErrorCode::CBKP001_SYNTAX, "safe");
        let _: Result<i32, _> = None.ok_or_error(err);
    });
    assert!(result.is_ok(), "OptionExt::ok_or_error must not panic");
}

#[test]
fn no_panic_safe_ops_parse_usize_empty_string() {
    let result = catch_unwind(|| {
        let _ = copybook_utils::safe_ops::parse_usize("", "ctx");
    });
    assert!(result.is_ok(), "parse_usize must not panic on empty input");
}

#[test]
fn no_panic_safe_ops_safe_array_bound_overflow() {
    let result = catch_unwind(|| {
        let _ = copybook_utils::safe_ops::safe_array_bound(usize::MAX, usize::MAX, usize::MAX, "ctx");
    });
    assert!(result.is_ok(), "safe_array_bound must not panic on overflow");
}

// ====================================================================
// Extension traits on different container types
// ====================================================================

#[test]
fn slice_ext_works_on_boxed_slice() {
    let data: Box<[i32]> = vec![10, 20, 30].into_boxed_slice();
    assert_eq!(
        *data.get_or_cbkp_error(2, ErrorCode::CBKP001_SYNTAX, "ctx").unwrap(),
        30
    );
}

#[test]
fn vec_ext_works_with_option_values() {
    let mut v: Vec<Option<i32>> = vec![Some(1), None, Some(3)];
    let popped = v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx").unwrap();
    assert_eq!(popped, Some(3));
    let last = v.last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx").unwrap();
    assert_eq!(*last, None);
}
