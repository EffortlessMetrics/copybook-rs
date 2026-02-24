// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration contract tests that ensure the façade delegates to the detector
//! microcrate without behavior drift.

use copybook_corruption::{
    detect_ebcdic_corruption as detect_via_facade, detect_packed_corruption as detect_packed_via_facade,
};
use copybook_corruption_detectors::{
    detect_ebcdic_corruption as detect_via_microcrate,
    detect_packed_corruption as detect_packed_via_microcrate,
};

fn error_codes(errors: &[copybook_core::Error]) -> Vec<copybook_core::ErrorCode> {
    errors.iter().map(|error| error.code).collect()
}

#[test]
fn test_facade_and_microcrate_ebcdic_corruption_agree() {
    let data = [b'C', 0x00, b'K', 0x7F, b'B'];
    let facade = detect_via_facade(&data, "FIELD");
    let micro = detect_via_microcrate(&data, "FIELD");

    assert_eq!(error_codes(&facade), error_codes(&micro));
    assert_eq!(facade.len(), micro.len());
}

#[test]
fn test_facade_and_microcrate_packed_corruption_agree() {
    let data = [0x12, 0x34, 0x5A];
    let facade = detect_packed_via_facade(&data, "FIELD");
    let micro = detect_packed_via_microcrate(&data, "FIELD");

    assert_eq!(error_codes(&facade), error_codes(&micro));
    assert_eq!(facade.len(), micro.len());
}
