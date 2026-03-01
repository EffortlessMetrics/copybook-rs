// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_error::ErrorCode;
use copybook_safe_index::{safe_divide, safe_slice_get};

#[test]
fn safe_divide_contract() {
    assert_eq!(safe_divide(10, 2, "contract").expect("divide"), 5);
    assert!(matches!(
        safe_divide(10, 0, "contract"),
        Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
    ));
}

#[test]
fn safe_slice_get_contract() {
    let values = [42usize, 17usize];
    assert_eq!(
        safe_slice_get(&values, 0, "contract").expect("index"),
        42usize
    );
    assert_eq!(
        safe_slice_get(&values, 1, "contract").expect("index"),
        17usize
    );
    assert!(matches!(
        safe_slice_get::<usize>(&values, 2, "contract"),
        Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
    ));
}
