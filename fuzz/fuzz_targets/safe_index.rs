// SPDX-License-Identifier: AGPL-3.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;

use copybook_safe_index::{safe_divide, safe_slice_get};

fuzz_target!(|data: &[u8]| {
    let denominator = data.first().copied().unwrap_or(1) as usize;
    let index = data.get(1).copied().unwrap_or(0) as usize;

    let _ = safe_divide(1_000, denominator, "fuzz-safe-index");
    let _ = safe_slice_get(&[10usize, 20usize, 30usize], index % 5, "fuzz-safe-index");
});
