#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_corruption::{
    detect_ebcdic_corruption, detect_packed_corruption, detect_rdw_ascii_corruption,
};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let _ = detect_rdw_ascii_corruption(data);

    let _ = detect_ebcdic_corruption(data, "FUZZ.FIELD");

    let _ = detect_packed_corruption(data, "FUZZ.FIELD");

    if !data.is_empty() {
        let _ = detect_ebcdic_corruption(&data[1..], "FUZZ.SUBSLICE");
        let _ = detect_packed_corruption(&data[1..], "FUZZ.SUBSLICE");
    }
});
