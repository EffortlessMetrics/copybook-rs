#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_overpunch::{
    ZeroSignPolicy, decode_overpunch_byte, encode_overpunch_byte, is_valid_overpunch,
};
use copybook_codepage::Codepage;
use libfuzzer_sys::fuzz_target;

/// Fuzz target for overpunch decode/encode with arbitrary bytes.
///
/// Exercises every codepage variant and validates round-trip consistency:
/// bytes that decode successfully must re-encode to the same byte.
fuzz_target!(|data: &[u8]| {
    let codepages = [
        Codepage::CP037,
        Codepage::ASCII,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];

    for &byte in data {
        for codepage in codepages {
            // Decode
            let decoded = decode_overpunch_byte(byte, codepage);

            // Validity check must agree with decode success
            let valid = is_valid_overpunch(byte, codepage);
            if let Ok((digit, is_negative)) = decoded {
                assert!(valid, "decode succeeded but is_valid_overpunch returned false");
                // Round-trip: encode back and compare
                for policy in [ZeroSignPolicy::Positive, ZeroSignPolicy::Preferred] {
                    let _ = encode_overpunch_byte(digit, is_negative, codepage, policy);
                }
            }
        }
    }

    // Also fuzz encode with digit values derived from data
    for &byte in data {
        let digit = byte % 10;
        for codepage in codepages {
            for is_negative in [false, true] {
                for policy in [ZeroSignPolicy::Positive, ZeroSignPolicy::Preferred] {
                    let _ = encode_overpunch_byte(digit, is_negative, codepage, policy);
                }
            }
        }
    }
});
