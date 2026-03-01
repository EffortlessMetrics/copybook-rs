// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]

use copybook_corruption::detect_rdw_ascii_corruption;
use copybook_corruption_rdw::detect_rdw_ascii_corruption as detect_rdw_ascii_corruption_micro;
use copybook_rdw::rdw_is_suspect_ascii_corruption;
use copybook_rdw_predicates::{
    rdw_is_suspect_ascii_corruption as predicate, rdw_is_suspect_ascii_corruption_slice,
};

#[test]
fn exposes_ascii_corruption_heuristic_function() {
    assert!(predicate([b'1', b'2', 0, 0]));
    assert!(!predicate([b'0', b'x', 0, 0]));
}

#[test]
fn slices_with_incomplete_headers_are_false() {
    assert!(!rdw_is_suspect_ascii_corruption_slice(b"12"));
}

#[test]
fn rdw_heuristic_is_shared_and_stable() {
    let suspect = [b'1', b'2', 0x00, 0x00];
    let clean = [0x00, 0x05, 0x00, 0x00];

    assert_eq!(predicate(suspect), rdw_is_suspect_ascii_corruption(suspect));
    assert!(rdw_is_suspect_ascii_corruption_slice(&suspect));
    assert!(detect_rdw_ascii_corruption(&suspect).is_some());
    assert!(detect_rdw_ascii_corruption_micro(&suspect).is_some());

    assert_eq!(predicate(clean), rdw_is_suspect_ascii_corruption(clean));
    assert!(!rdw_is_suspect_ascii_corruption_slice(&clean));
    assert!(detect_rdw_ascii_corruption(&clean).is_none());
    assert!(detect_rdw_ascii_corruption_micro(&clean).is_none());
}
