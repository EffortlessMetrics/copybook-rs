#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_corruption::{
    detect_ebcdic_corruption, detect_packed_corruption,
};
use copybook_corruption_detectors::{
    detect_ebcdic_corruption as detect_ebcdic_micro,
    detect_packed_corruption as detect_packed_micro,
};
use copybook_corruption_predicates::{
    is_invalid_comp3_high_nibble, is_invalid_comp3_low_nibble, is_invalid_comp3_sign_nibble,
    is_likely_corrupted_ebcdic_byte,
};
use copybook_corruption_rdw::detect_rdw_ascii_corruption;
use copybook_rdw_predicates::rdw_is_suspect_ascii_corruption_slice;
use libfuzzer_sys::fuzz_target;

/// Fuzz target for corruption detection with arbitrary record data.
///
/// Exercises all corruption detectors with varied slice offsets and
/// field name contexts, validates facade vs micro-crate consistency.
fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    // Use first byte to derive a synthetic field name prefix
    let field_name = format!("FUZZ-{:02X}", data[0]);

    // Full-slice detection
    let _ = detect_rdw_ascii_corruption(data);
    let _ = rdw_is_suspect_ascii_corruption_slice(data);
    let facade_ebc = detect_ebcdic_corruption(data, &field_name);
    let micro_ebc = detect_ebcdic_micro(data, &field_name);
    let facade_pak = detect_packed_corruption(data, &field_name);
    let micro_pak = detect_packed_micro(data, &field_name);

    // Facade and micro-crate must agree
    assert_eq!(facade_ebc.len(), micro_ebc.len());
    assert_eq!(facade_pak.len(), micro_pak.len());

    // Byte-level predicates
    for &byte in data {
        let _ = is_likely_corrupted_ebcdic_byte(byte);
        let _ = is_invalid_comp3_high_nibble(byte);
        let _ = is_invalid_comp3_low_nibble(byte);
        let _ = is_invalid_comp3_sign_nibble(byte);
    }

    // Sub-slices at various offsets
    for start in 1..data.len().min(4) {
        let sub = &data[start..];
        let sub_field = format!("{field_name}.SUB{start}");
        assert_eq!(
            detect_ebcdic_corruption(sub, &sub_field).len(),
            detect_ebcdic_micro(sub, &sub_field).len(),
        );
        assert_eq!(
            detect_packed_corruption(sub, &sub_field).len(),
            detect_packed_micro(sub, &sub_field).len(),
        );
    }
});
