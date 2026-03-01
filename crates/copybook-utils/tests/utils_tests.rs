// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-utils extension traits and safe_ops re-exports.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::{Error, ErrorCode};
use copybook_utils::{OptionExt, SliceExt, VecExt};

// ── OptionExt: ok_or_cbkp_error ─────────────────────────────────────

#[test]
fn option_ext_some_returns_inner_value() {
    let result = Some(99).ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "unused");
    assert_eq!(result.unwrap(), 99);
}

#[test]
fn option_ext_none_returns_correct_error_code() {
    let result: Result<i32, _> =
        None.ok_or_cbkp_error(ErrorCode::CBKD101_INVALID_FIELD_TYPE, "field missing");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKD101_INVALID_FIELD_TYPE);
    assert_eq!(err.message, "field missing");
}

#[test]
fn option_ext_some_with_zero_value() {
    assert_eq!(
        Some(0u64)
            .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        0
    );
}

#[test]
fn option_ext_some_with_empty_string() {
    assert_eq!(
        Some(String::new())
            .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        ""
    );
}

#[test]
fn option_ext_none_preserves_message_with_special_chars() {
    let msg = "field 'CUST-ID' at offset 0x10 not found";
    let err: Error = Option::<()>::None
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, msg)
        .unwrap_err();
    assert_eq!(err.message, msg);
}

// ── OptionExt: ok_or_error ──────────────────────────────────────────

#[test]
fn option_ext_ok_or_error_some_ignores_provided_error() {
    let sentinel_err = Error::new(ErrorCode::CBKD101_INVALID_FIELD_TYPE, "never used");
    let val = Some(42).ok_or_error(sentinel_err).unwrap();
    assert_eq!(val, 42);
}

#[test]
fn option_ext_ok_or_error_none_returns_exact_error() {
    let err = Error::new(ErrorCode::CBKD101_INVALID_FIELD_TYPE, "custom detail");
    let result: Result<i32, _> = None.ok_or_error(err);
    let got = result.unwrap_err();
    assert_eq!(got.code, ErrorCode::CBKD101_INVALID_FIELD_TYPE);
    assert_eq!(got.message, "custom detail");
}

// ── VecExt: pop_or_cbkp_error ───────────────────────────────────────

#[test]
fn vec_pop_returns_last_element_and_shrinks() {
    let mut v = vec![10, 20, 30];
    let val = v
        .pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    assert_eq!(val, 30);
    assert_eq!(v.len(), 2);
}

#[test]
fn vec_pop_single_element_leaves_empty() {
    let mut v = vec![7];
    assert_eq!(
        v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        7
    );
    assert!(v.is_empty());
}

#[test]
fn vec_pop_empty_returns_error() {
    let mut v: Vec<String> = Vec::new();
    let err = v
        .pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "stack underflow")
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    assert_eq!(err.message, "stack underflow");
}

#[test]
fn vec_pop_successive_until_empty() {
    let mut v = vec![1, 2];
    assert!(v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "").is_ok());
    assert!(v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "").is_ok());
    assert!(v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "").is_err());
}

// ── VecExt: last_or_cbkp_error ──────────────────────────────────────

#[test]
fn vec_last_returns_reference_to_last() {
    let v = vec!["a", "b", "c"];
    assert_eq!(
        *v.last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        "c"
    );
}

#[test]
fn vec_last_does_not_consume_vector() {
    let v = vec![1, 2, 3];
    let _ = v
        .last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    assert_eq!(v.len(), 3);
}

#[test]
fn vec_last_empty_returns_error() {
    let v: Vec<u8> = vec![];
    assert!(
        v.last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "empty")
            .is_err()
    );
}

// ── VecExt: last_mut_or_cbkp_error ──────────────────────────────────

#[test]
fn vec_last_mut_allows_in_place_modification() {
    let mut v = vec![10, 20, 30];
    *v.last_mut_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap() = 99;
    assert_eq!(v, vec![10, 20, 99]);
}

#[test]
fn vec_last_mut_single_element() {
    let mut v = vec![5];
    *v.last_mut_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap() = 42;
    assert_eq!(v[0], 42);
}

#[test]
fn vec_last_mut_empty_returns_error() {
    let mut v: Vec<i64> = vec![];
    let err = v
        .last_mut_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "no elements")
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
}

// ── SliceExt: get_or_cbkp_error ─────────────────────────────────────

#[test]
fn slice_get_first_element() {
    let data = [100u16, 200, 300];
    assert_eq!(
        *data
            .get_or_cbkp_error(0, ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        100
    );
}

#[test]
fn slice_get_last_valid_index() {
    let data = [1, 2, 3];
    assert_eq!(
        *data
            .get_or_cbkp_error(2, ErrorCode::CBKP001_SYNTAX, "ctx")
            .unwrap(),
        3
    );
}

#[test]
fn slice_get_one_past_end_returns_error() {
    let data = [1, 2, 3];
    assert!(
        data.get_or_cbkp_error(3, ErrorCode::CBKP001_SYNTAX, "oob")
            .is_err()
    );
}

#[test]
fn slice_get_usize_max_returns_error() {
    let data = [1u8];
    assert!(
        data.get_or_cbkp_error(usize::MAX, ErrorCode::CBKP001_SYNTAX, "huge")
            .is_err()
    );
}

#[test]
fn slice_get_empty_slice_returns_error() {
    let data: &[u8] = &[];
    let err = data
        .get_or_cbkp_error(0, ErrorCode::CBKP001_SYNTAX, "empty")
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
}

// ── SliceExt: get_mut_or_cbkp_error ─────────────────────────────────

#[test]
fn slice_get_mut_modifies_element() {
    let mut data = [10u8, 20, 30];
    *data
        .get_mut_or_cbkp_error(1, ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap() = 77;
    assert_eq!(data, [10, 77, 30]);
}

#[test]
fn slice_get_mut_out_of_bounds_returns_error() {
    let mut data = [1u8, 2];
    assert!(
        data.get_mut_or_cbkp_error(5, ErrorCode::CBKP001_SYNTAX, "oob")
            .is_err()
    );
}

#[test]
fn slice_get_mut_empty_returns_error() {
    let mut data: Vec<u32> = vec![];
    assert!(
        data.get_mut_or_cbkp_error(0, ErrorCode::CBKP001_SYNTAX, "empty")
            .is_err()
    );
}

// ── safe_ops re-exports ─────────────────────────────────────────────

#[test]
fn safe_ops_parse_usize_valid() {
    assert_eq!(
        copybook_utils::safe_ops::parse_usize("0", "zero").unwrap(),
        0
    );
    assert_eq!(
        copybook_utils::safe_ops::parse_usize("99999", "large").unwrap(),
        99999
    );
}

#[test]
fn safe_ops_parse_usize_invalid_returns_error() {
    assert!(copybook_utils::safe_ops::parse_usize("", "empty").is_err());
    assert!(copybook_utils::safe_ops::parse_usize("-1", "negative").is_err());
    assert!(copybook_utils::safe_ops::parse_usize("abc", "alpha").is_err());
    assert!(copybook_utils::safe_ops::parse_usize("12.5", "decimal").is_err());
}

#[test]
fn safe_ops_safe_divide_valid() {
    assert_eq!(
        copybook_utils::safe_ops::safe_divide(100, 10, "ctx").unwrap(),
        10
    );
    assert_eq!(
        copybook_utils::safe_ops::safe_divide(0, 5, "ctx").unwrap(),
        0
    );
    assert_eq!(
        copybook_utils::safe_ops::safe_divide(7, 2, "ctx").unwrap(),
        3
    );
}

#[test]
fn safe_ops_safe_divide_by_zero_returns_error() {
    let err = copybook_utils::safe_ops::safe_divide(42, 0, "div-zero").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn safe_ops_safe_array_bound_valid() {
    // base + count * item_size = 10 + 3*4 = 22
    assert_eq!(
        copybook_utils::safe_ops::safe_array_bound(10, 3, 4, "ctx").unwrap(),
        22
    );
    // base + 0 * N = base
    assert_eq!(
        copybook_utils::safe_ops::safe_array_bound(5, 0, 100, "ctx").unwrap(),
        5
    );
}

#[test]
fn safe_ops_safe_array_bound_overflow_returns_error() {
    assert!(copybook_utils::safe_ops::safe_array_bound(0, usize::MAX, 2, "overflow").is_err());
    assert!(copybook_utils::safe_ops::safe_array_bound(usize::MAX, 1, 1, "overflow").is_err());
}

// ── no-panic guarantees ─────────────────────────────────────────────

#[test]
fn no_panic_on_none_option_ext() {
    let result = std::panic::catch_unwind(|| {
        let _: Result<i32, _> = None.ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "safe");
    });
    assert!(result.is_ok());
}

#[test]
fn no_panic_on_empty_vec_pop() {
    let result = std::panic::catch_unwind(|| {
        let mut v: Vec<i32> = vec![];
        let _ = v.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "safe");
    });
    assert!(result.is_ok());
}

#[test]
fn no_panic_on_empty_vec_last() {
    let result = std::panic::catch_unwind(|| {
        let v: Vec<i32> = vec![];
        let _ = v.last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "safe");
    });
    assert!(result.is_ok());
}

#[test]
fn no_panic_on_oob_slice_get() {
    let result = std::panic::catch_unwind(|| {
        let data = [1u8, 2, 3];
        let _ = data.get_or_cbkp_error(100, ErrorCode::CBKP001_SYNTAX, "safe");
    });
    assert!(result.is_ok());
}

// ── generic type parameters ─────────────────────────────────────────

#[test]
fn option_ext_works_with_complex_types() {
    let opt: Option<Vec<String>> = Some(vec!["hello".into(), "world".into()]);
    let val = opt
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    assert_eq!(val.len(), 2);
}

#[test]
fn vec_ext_works_with_tuple_type() {
    let mut v: Vec<(u32, &str)> = vec![(1, "a"), (2, "b")];
    let popped = v
        .pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    assert_eq!(popped, (2, "b"));
}

#[test]
fn slice_ext_works_with_struct_type() {
    #[derive(Debug, PartialEq)]
    struct Field {
        name: String,
    }
    let fields = vec![Field { name: "A".into() }, Field { name: "B".into() }];
    let got = fields
        .get_or_cbkp_error(1, ErrorCode::CBKP001_SYNTAX, "ctx")
        .unwrap();
    assert_eq!(got.name, "B");
}
