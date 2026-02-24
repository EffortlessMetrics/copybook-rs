// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_safe_ops as safe_ops_crate;
use copybook_utils as utils;

#[test]
fn safe_ops_reexport_contract() {
    let parsed = utils::safe_ops::parse_usize("2048", "utils").expect("parse utils");
    let parsed_core = safe_ops_crate::parse_usize("2048", "crate").expect("parse crate");
    assert_eq!(parsed, parsed_core);

    let array = utils::safe_ops::safe_array_bound(10, 3, 4, "utils")
        .expect("utils array bound");
    let array_core = safe_ops_crate::safe_array_bound(10, 3, 4, "crate")
        .expect("crate array bound");
    assert_eq!(array, array_core);
}

#[test]
fn safe_ops_error_shape_is_stable() {
    let result = utils::safe_ops::safe_divide(10, 0, "division");
    assert!(result.is_err(), "division by zero should fail");
}
