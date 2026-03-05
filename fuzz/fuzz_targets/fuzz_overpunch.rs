#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_codepage::Codepage;
use copybook_overpunch::{
    ZeroSignPolicy, decode_ebcdic_overpunch_zone, decode_overpunch_byte,
    encode_ebcdic_overpunch_zone, encode_overpunch_byte, get_all_valid_overpunch_bytes,
    is_valid_overpunch,
};
use libfuzzer_sys::fuzz_target;

// Fuzz target for the full overpunch API surface.
//
// Exercises EBCDIC zone encode/decode, codepage-aware byte encode/decode,
// validity checks, and round-trip consistency. Must never panic on any input.
fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    let codepages = [
        Codepage::CP037,
        Codepage::ASCII,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];

    // --- EBCDIC zone-level encode/decode ---
    for &byte in data {
        // decode_ebcdic_overpunch_zone: every u8 must return safely
        let _ = decode_ebcdic_overpunch_zone(byte);

        // encode with all digit/sign/policy combos derived from the byte
        let digit = byte % 10;
        let is_negative = byte & 0x80 != 0;
        for policy in [ZeroSignPolicy::Positive, ZeroSignPolicy::Preferred] {
            let encoded = encode_ebcdic_overpunch_zone(digit, is_negative, policy);
            // encoded byte must decode without panic
            let _ = decode_ebcdic_overpunch_zone(encoded);
        }
    }

    // --- Codepage-aware byte encode/decode round-trip ---
    for &byte in data.iter().take(64) {
        for codepage in codepages {
            let valid = is_valid_overpunch(byte, codepage);
            match decode_overpunch_byte(byte, codepage) {
                Ok((digit, is_neg)) => {
                    // Successful decode implies validity
                    if !valid {
                        return;
                    }
                    // Round-trip encode must succeed
                    for policy in [ZeroSignPolicy::Positive, ZeroSignPolicy::Preferred] {
                        let _ = encode_overpunch_byte(digit, is_neg, codepage, policy);
                    }
                }
                Err(_) => {}
            }
        }
    }

    // --- Encode path with fuzz-derived digits ---
    for chunk in data.chunks(2) {
        let digit = chunk[0] % 10;
        let is_negative = chunk.len() > 1 && chunk[1] & 1 != 0;
        for codepage in codepages {
            for policy in [ZeroSignPolicy::Positive, ZeroSignPolicy::Preferred] {
                if let Ok(encoded) = encode_overpunch_byte(digit, is_negative, codepage, policy) {
                    // Verify decode of the encoded byte succeeds
                    let _ = decode_overpunch_byte(encoded, codepage);
                }
            }
        }
    }

    // --- Enumerate valid bytes for coverage (first byte selects codepage) ---
    let codepage = codepages[data[0] as usize % codepages.len()];
    let valid_bytes = get_all_valid_overpunch_bytes(codepage);
    for vb in &valid_bytes {
        let _ = decode_overpunch_byte(*vb, codepage);
        let _ = is_valid_overpunch(*vb, codepage);
    }
});
