// SPDX-License-Identifier: AGPL-3.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;
use std::borrow::Cow;

use copybook_safe_text::{
    parse_isize, parse_usize, safe_parse_u16, safe_string_char_at, safe_write, safe_write_str,
};

fuzz_target!(|data: &[u8]| {
    let text = Cow::from(std::str::from_utf8(data).unwrap_or("fuzz"));
    let first = data.first().copied().unwrap_or(0) as usize;
    let second = data.get(1).copied().unwrap_or(0) as usize;
    let third = data.get(2).copied().unwrap_or(0) as usize;

    let _ = parse_usize(&text, "fuzz-safe-text");
    let _ = parse_isize(&text, "fuzz-safe-text");
    let _ = safe_parse_u16(&text, "fuzz-safe-text");

    let mut buffer = String::new();
    let _ = safe_write(&mut buffer, format_args!("len={} index={}", first, second));
    let _ = safe_write_str(&mut buffer, "fuzz");
    let _ = safe_string_char_at("abcdef", third % 6, "fuzz-safe-text");
    let _ = safe_string_char_at("abcdef", third % 7, "fuzz-safe-text");
});
