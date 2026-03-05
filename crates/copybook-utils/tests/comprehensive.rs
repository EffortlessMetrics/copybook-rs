// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-utils extension traits.

use copybook_error::{Error, ErrorCode};
use copybook_utils::{OptionExt, SliceExt, VecExt};
use std::panic::catch_unwind;

// ── OptionExt ───────────────────────────────────────────────────────

#[test]
fn option_ext_some_returns_value() {
    let opt = Some(42);
    assert_eq!(
        opt.ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        42
    );
}

#[test]
fn option_ext_none_returns_error_with_code() {
    let opt: Option<i32> = None;
    let err = opt
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "my-ctx")
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    assert_eq!(err.message, "my-ctx");
}

#[test]
fn option_ext_ok_or_error_some() {
    let opt = Some("hello");
    let err = Error::new(ErrorCode::CBKP001_SYNTAX, "unused");
    assert_eq!(opt.ok_or_error(err).unwrap(), "hello");
}

#[test]
fn option_ext_ok_or_error_none() {
    let opt: Option<i32> = None;
    let err = Error::new(ErrorCode::CBKD101_INVALID_FIELD_TYPE, "custom-error");
    let result = opt.ok_or_error(err);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD101_INVALID_FIELD_TYPE
    );
}

#[test]
fn option_ext_with_string_type() {
    let opt = Some(String::from("value"));
    let val = opt
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    assert_eq!(val, "value");
}

// ── VecExt ──────────────────────────────────────────────────────────

#[test]
fn vec_pop_returns_last() {
    let mut v = vec![1, 2, 3];
    assert_eq!(
        v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        3
    );
    assert_eq!(v.len(), 2);
}

#[test]
fn vec_pop_empty_returns_error() {
    let mut v: Vec<i32> = vec![];
    let err = v
        .pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "pop-empty")
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn vec_last_returns_ref() {
    let v = vec![10, 20, 30];
    assert_eq!(
        *v.last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        30
    );
}

#[test]
fn vec_last_empty_returns_error() {
    let v: Vec<i32> = vec![];
    assert!(
        v.last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .is_err()
    );
}

#[test]
fn vec_last_mut_modifies_in_place() {
    let mut v = vec![1, 2, 3];
    let last = v
        .last_mut_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    *last = 99;
    assert_eq!(v, vec![1, 2, 99]);
}

#[test]
fn vec_last_mut_empty_returns_error() {
    let mut v: Vec<i32> = vec![];
    assert!(
        v.last_mut_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .is_err()
    );
}

#[test]
fn vec_pop_single_element() {
    let mut v = vec![42];
    assert_eq!(
        v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        42
    );
    assert!(v.is_empty());
}

// ── SliceExt ────────────────────────────────────────────────────────

#[test]
fn slice_get_valid_index() {
    let data = [10u8, 20, 30];
    assert_eq!(
        *data
            .get_or_cbkp_error(1, ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        20
    );
}

#[test]
fn slice_get_out_of_bounds() {
    let data = [10u8, 20, 30];
    assert!(
        data.get_or_cbkp_error(3, ErrorCode::CBKP001_SYNTAX, "ctx")
            .is_err()
    );
}

#[test]
fn slice_get_empty_slice() {
    let data: &[u8] = &[];
    let err = data
        .get_or_cbkp_error(0, ErrorCode::CBKP001_SYNTAX, "empty-slice")
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn slice_get_mut_modifies_in_place() {
    let mut data = [1u8, 2, 3];
    let elem = data
        .get_mut_or_cbkp_error(1, ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    *elem = 99;
    assert_eq!(data, [1, 99, 3]);
}

#[test]
fn slice_get_mut_out_of_bounds() {
    let mut data = [1u8, 2];
    assert!(
        data.get_mut_or_cbkp_error(2, ErrorCode::CBKP001_SYNTAX, "ctx")
            .is_err()
    );
}

#[test]
fn slice_get_max_index_is_error() {
    let data = [1u8];
    assert!(
        data.get_or_cbkp_error(usize::MAX, ErrorCode::CBKP001_SYNTAX, "ctx")
            .is_err()
    );
}

// ── safe_ops re-export ──────────────────────────────────────────────

#[test]
fn safe_ops_reexport_divide() {
    assert_eq!(
        copybook_utils::safe_ops::safe_divide(10, 5, "ctx").unwrap(),
        2
    );
    assert!(copybook_utils::safe_ops::safe_divide(1, 0, "ctx").is_err());
}

#[test]
fn safe_ops_reexport_parse_usize() {
    assert_eq!(
        copybook_utils::safe_ops::parse_usize("42", "ctx").unwrap(),
        42
    );
    assert!(copybook_utils::safe_ops::parse_usize("bad", "ctx").is_err());
}

// ── no-panic guarantees ─────────────────────────────────────────────

#[test]
fn option_ext_never_panics_on_none() {
    let result = catch_unwind(|| {
        let opt: Option<i32> = None;
        opt.ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "panic-test")
    });
    assert!(result.is_ok(), "OptionExt must not panic on None");
    assert!(result.unwrap().is_err());
}

#[test]
fn vec_pop_never_panics_on_empty() {
    let result = catch_unwind(|| {
        let mut v: Vec<i32> = vec![];
        v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "panic-test")
    });
    assert!(result.is_ok(), "VecExt::pop must not panic on empty");
    assert!(result.unwrap().is_err());
}

#[test]
fn slice_get_never_panics_on_oob() {
    let result = catch_unwind(|| {
        let data = [1u8, 2, 3];
        data.get_or_cbkp_error(usize::MAX, ErrorCode::CBKP001_SYNTAX, "panic-test")
            .is_err()
    });
    assert!(
        result.is_ok(),
        "SliceExt::get must not panic on out-of-bounds"
    );
    assert!(result.unwrap());
}
