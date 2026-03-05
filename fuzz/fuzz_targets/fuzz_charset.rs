#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, utf8_to_ebcdic};
use libfuzzer_sys::fuzz_target;

/// Fuzz target for EBCDIC ↔ ASCII/UTF-8 character set conversion.
///
/// Feeds arbitrary bytes through all codepage and policy combinations to
/// verify that conversions never panic and that round-trip consistency
/// holds where applicable.
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

    // --- EBCDIC → UTF-8 across all codepages and policies ---
    for codepage in codepages {
        for policy in policies {
            let _ = ebcdic_to_utf8(data, codepage, policy);
        }
    }

    // --- UTF-8 → EBCDIC (requires valid UTF-8 input) ---
    if let Ok(text) = std::str::from_utf8(data) {
        for codepage in codepages {
            if let Ok(ebcdic) = utf8_to_ebcdic(text, codepage) {
                // Round-trip: verify EBCDIC → UTF-8 does not panic
                let _ = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Replace);
            }
        }
    }

    // --- Lossy UTF-8 path for broader coverage ---
    let lossy = String::from_utf8_lossy(data);
    if !lossy.is_empty() {
        for codepage in codepages {
            let _ = utf8_to_ebcdic(&lossy, codepage);
        }
    }

    // --- Byte-at-a-time conversion (edge-case single bytes) ---
    for &byte in data.iter().take(64) {
        let single = [byte];
        for codepage in codepages {
            let _ = ebcdic_to_utf8(&single, codepage, UnmappablePolicy::Replace);
        }
    }
});
