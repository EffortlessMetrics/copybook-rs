// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_error::{Error, ErrorCode};
use copybook_safe_access::{OptionExt, SliceExt, VecExt};

#[test]
fn option_ext_returns_value() {
    let value = Some(7)
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "present")
        .expect("option should contain value");
    assert_eq!(value, 7);
}

#[test]
fn option_ext_returns_error() {
    let result: Result<i32, _> =
        None.ok_or_error(Error::new(ErrorCode::CBKD101_INVALID_FIELD_TYPE, "missing"));
    assert!(matches!(
        result,
        Err(error) if error.code == ErrorCode::CBKD101_INVALID_FIELD_TYPE
    ));
}

#[test]
fn vec_ext_handles_empty() {
    let mut values: Vec<u8> = vec![];
    let result = values.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "empty");
    assert!(result.is_err());
}

#[test]
fn slice_ext_bounds_checks() {
    let values = [10, 20, 30];
    assert_eq!(
        *values
            .get_or_cbkp_error(1, ErrorCode::CBKP001_SYNTAX, "ok")
            .expect("index 1 must exist"),
        20
    );
    assert!(
        values
            .get_or_cbkp_error(5, ErrorCode::CBKP001_SYNTAX, "oob")
            .is_err()
    );
}
