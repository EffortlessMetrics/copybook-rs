// SPDX-License-Identifier: AGPL-3.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;

use copybook_zoned_format::ZonedEncodingFormat;

fuzz_target!(|data: &[u8]| {
    let byte = data.first().copied().unwrap_or(0);
    let _ = ZonedEncodingFormat::detect_from_byte(byte);
});
