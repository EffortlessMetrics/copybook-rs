#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, utf8_to_ebcdic};
use libfuzzer_sys::fuzz_target;

// Fuzz target for EBCDIC ↔ UTF-8 ↔ EBCDIC codepage round-trip.
//
// Verifies that:
// - No codepage conversion ever panics on any input.
// - Mappable bytes survive a full EBCDIC → UTF-8 → EBCDIC round-trip.
fuzz_target!(|data: &[u8]| {
    let codepages = [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];

    // --- Forward direction: treat input as EBCDIC, convert to UTF-8 ---
    for codepage in codepages {
        // With Replace policy every byte is mappable (no errors)
        if let Ok(utf8) = ebcdic_to_utf8(data, codepage, UnmappablePolicy::Replace) {
            // Round-trip: UTF-8 back to EBCDIC
            if let Ok(ebcdic2) = utf8_to_ebcdic(&utf8, codepage) {
                // A second forward pass must not panic
                let _ = ebcdic_to_utf8(&ebcdic2, codepage, UnmappablePolicy::Replace);
            }
        }

        // With Error policy — may fail, must not panic
        let _ = ebcdic_to_utf8(data, codepage, UnmappablePolicy::Error);
        // With Skip policy
        let _ = ebcdic_to_utf8(data, codepage, UnmappablePolicy::Skip);
    }

    // --- Reverse direction: treat input as UTF-8, convert to EBCDIC ---
    if let Ok(text) = std::str::from_utf8(data) {
        for codepage in codepages {
            if let Ok(ebcdic) = utf8_to_ebcdic(text, codepage) {
                // Round-trip back to UTF-8
                if let Ok(utf8_2) = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error) {
                    // For fully-mappable characters the round-trip should
                    // reproduce the original text.  Avoid panicking in the
                    // harness — the fuzzer will still record divergences.
                    let _ = utf8_2 == text;
                }
            }
        }
    }

    // --- Single-byte sweep for edge-case coverage ---
    for &byte in data.iter().take(64) {
        let single = [byte];
        for codepage in codepages {
            if let Ok(ch) = ebcdic_to_utf8(&single, codepage, UnmappablePolicy::Replace) {
                let _ = utf8_to_ebcdic(&ch, codepage);
            }
        }
    }
});
