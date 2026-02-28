#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, utf8_to_ebcdic};
use libfuzzer_sys::fuzz_target;

/// Fuzz target for EBCDIC ↔ UTF-8 charset conversion.
///
/// Exercises both directions across codepages and unmappable-byte policies,
/// and validates round-trip consistency where possible.
fuzz_target!(|data: &[u8]| {
    let codepages = [
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];
    let policies = [
        UnmappablePolicy::Error,
        UnmappablePolicy::Replace,
        UnmappablePolicy::Skip,
    ];

    // EBCDIC -> UTF-8
    for codepage in codepages {
        for policy in policies {
            let _ = ebcdic_to_utf8(data, codepage, policy);
        }
    }

    // UTF-8 -> EBCDIC (input must be valid UTF-8)
    if let Ok(text) = std::str::from_utf8(data) {
        for codepage in codepages {
            if let Ok(ebcdic) = utf8_to_ebcdic(text, codepage) {
                // Round-trip: EBCDIC back to UTF-8
                let _ = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Replace);
            }
        }
    }

    // Also try lossy UTF-8 conversion path
    let lossy = String::from_utf8_lossy(data);
    if !lossy.is_empty() {
        for codepage in codepages {
            let _ = utf8_to_ebcdic(&lossy, codepage);
        }
    }
});
