#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_cli_determinism::{render_human_result, render_json_result, truncate_hash};
use copybook_determinism::{ByteDiff, DeterminismMode, DeterminismResult};
use libfuzzer_sys::fuzz_target;

fn hex_from_bytes(data: &[u8]) -> String {
    const HEX: &[u8] = b"0123456789abcdef";
    let mut out = String::new();
    for byte in data {
        out.push(HEX[(byte >> 4) as usize] as char);
        out.push(HEX[(byte & 0x0F) as usize] as char);
    }

    if out.is_empty() {
        out.push_str("00");
    }
    out
}

fn flip_first_hex_digit(mut value: String) -> String {
    let mut bytes = value.into_bytes();
    if let Some(first) = bytes.first_mut() {
        *first = if *first == b'0' { b'f' } else { b'0' };
    }
    String::from_utf8(bytes).expect("hex string remains ascii")
}

fn mode_from_byte(byte: u8) -> DeterminismMode {
    match byte % 3 {
        0 => DeterminismMode::DecodeOnly,
        1 => DeterminismMode::EncodeOnly,
        _ => DeterminismMode::RoundTrip,
    }
}

fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    let hash = hex_from_bytes(data);
    let deterministic = data[0].is_multiple_of(2);
    let mode = mode_from_byte(data[0]);
    let max_diffs = usize::from(data[0] % 64);

    let byte_differences = if deterministic {
        None
    } else {
        let diffs = (0..max_diffs.min(4))
            .map(|idx| ByteDiff {
                offset: idx,
                round1_byte: data.get(idx + 1).copied().unwrap_or(0),
                round2_byte: data.get(idx + 2).copied().unwrap_or(1),
            })
            .collect();
        Some(diffs)
    };

    let result = DeterminismResult {
        mode,
        round1_hash: hash.clone(),
        round2_hash: if deterministic {
            hash
        } else {
            flip_first_hex_digit(hash)
        },
        is_deterministic: deterministic,
        byte_differences,
    };

    let _ = render_human_result(&result, max_diffs);
    let _ = render_json_result(&result);
    let _ = truncate_hash(&result.round1_hash);
    let _ = truncate_hash(&result.round2_hash);
});
