#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_determinism::{
    DeterminismMode, compare_outputs, compare_outputs_with_limit, find_byte_differences_with_limit,
};
use libfuzzer_sys::fuzz_target;

fn mode_from_byte(byte: u8) -> DeterminismMode {
    match byte % 3 {
        0 => DeterminismMode::DecodeOnly,
        1 => DeterminismMode::EncodeOnly,
        _ => DeterminismMode::RoundTrip,
    }
}

fuzz_target!(|data: &[u8]| {
    if data.len() < 2 {
        return;
    }

    let mode = mode_from_byte(data[0]);
    let limit = usize::from(data[1] % 128);
    let payload = &data[2..];
    let split = payload.len() / 2;

    let round1 = &payload[..split];
    let round2 = &payload[split..];

    let _ = compare_outputs(mode, round1, round2);
    let _ = compare_outputs_with_limit(mode, round1, round2, limit);
    let _ = find_byte_differences_with_limit(round1, round2, limit);
});
