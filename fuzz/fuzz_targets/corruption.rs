#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_corruption::{
    detect_ebcdic_corruption as detect_ebcdic_corruption_facade,
    detect_packed_corruption as detect_packed_corruption_facade,
};
use copybook_corruption_rdw::detect_rdw_ascii_corruption;
use copybook_corruption_detectors::{
    detect_ebcdic_corruption as detect_ebcdic_corruption_micro,
    detect_packed_corruption as detect_packed_corruption_micro,
};
use copybook_corruption_predicates::{
    is_invalid_comp3_high_nibble, is_invalid_comp3_low_nibble, is_invalid_comp3_sign_nibble,
    is_likely_corrupted_ebcdic_byte,
};
use copybook_rdw_predicates::rdw_is_suspect_ascii_corruption_slice;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let _ = detect_rdw_ascii_corruption(data);
    let _ = rdw_is_suspect_ascii_corruption_slice(data);

    let _ = detect_ebcdic_corruption_facade(data, "FUZZ.FIELD");
    let _ = detect_packed_corruption_facade(data, "FUZZ.FIELD");
    let _ = detect_ebcdic_corruption_micro(data, "FUZZ.FIELD");
    let _ = detect_packed_corruption_micro(data, "FUZZ.FIELD");

    assert_eq!(
        detect_ebcdic_corruption_facade(data, "FUZZ.FIELD").len(),
        detect_ebcdic_corruption_micro(data, "FUZZ.FIELD").len(),
    );
    assert_eq!(
        detect_packed_corruption_facade(data, "FUZZ.FIELD").len(),
        detect_packed_corruption_micro(data, "FUZZ.FIELD").len(),
    );

    if !data.is_empty() {
        let _ = data
            .iter()
            .copied()
            .any(is_likely_corrupted_ebcdic_byte);
        let _ = data.iter().copied().any(is_invalid_comp3_high_nibble);
        let _ = data.iter().copied().any(is_invalid_comp3_low_nibble);

        let last = data[data.len() - 1];
        let _ = is_invalid_comp3_sign_nibble(last);

        let _ = detect_ebcdic_corruption_facade(&data[1..], "FUZZ.SUBSLICE");
        let _ = detect_packed_corruption_facade(&data[1..], "FUZZ.SUBSLICE");
        let _ = detect_ebcdic_corruption_micro(&data[1..], "FUZZ.SUBSLICE");
        let _ = detect_packed_corruption_micro(&data[1..], "FUZZ.SUBSLICE");

        assert_eq!(
            detect_ebcdic_corruption_facade(&data[1..], "FUZZ.SUBSLICE").len(),
            detect_ebcdic_corruption_micro(&data[1..], "FUZZ.SUBSLICE").len(),
        );
        assert_eq!(
            detect_packed_corruption_facade(&data[1..], "FUZZ.SUBSLICE").len(),
            detect_packed_corruption_micro(&data[1..], "FUZZ.SUBSLICE").len(),
        );
    }
});
