// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_error::{Error, ErrorCode};
use copybook_safe_collections::{OptionExt, SliceExt, VecExt};

#[test]
fn option_ext_returns_value_or_error() {
    let v = Some(9u8)
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
        .expect("some");
    assert_eq!(v, 9);

    let err = None::<u8>
        .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "missing")
        .expect_err("none");
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn option_ext_ok_or_error_uses_given_error() {
    let source = Error::new(ErrorCode::CBKP001_SYNTAX, "missing");
    let err = None::<u8>.ok_or_error(source.clone()).expect_err("none");
    assert_eq!(err.code, source.code);
    assert_eq!(err.message, source.message);
}

#[test]
fn vec_ext_pop_and_last_contracts() {
    let mut values = vec![1u8, 2u8];
    assert_eq!(
        *values
            .last_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .expect("last"),
        2
    );
    assert_eq!(
        values
            .pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "ctx")
            .expect("pop"),
        2
    );

    let mut empty: Vec<u8> = Vec::new();
    assert!(
        empty
            .pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "empty")
            .is_err()
    );
}

#[test]
fn slice_ext_access_contracts() {
    let mut values = [10u8, 20u8, 30u8];
    assert_eq!(
        *values
            .get_or_cbkp_error(1, ErrorCode::CBKP001_SYNTAX, "ctx")
            .expect("index"),
        20
    );

    *values
        .get_mut_or_cbkp_error(2, ErrorCode::CBKP001_SYNTAX, "ctx")
        .expect("mut index") = 77;
    assert_eq!(values[2], 77);

    assert!(
        values
            .get_or_cbkp_error(99, ErrorCode::CBKP001_SYNTAX, "oob")
            .is_err()
    );
}
