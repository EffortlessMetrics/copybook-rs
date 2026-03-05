// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-safe-ops.

use copybook_error::ErrorCode;
use copybook_safe_ops::{
    parse_isize, parse_usize, safe_array_bound, safe_divide, safe_parse_u16, safe_slice_get,
    safe_string_char_at, safe_u64_to_u16, safe_u64_to_u32, safe_usize_to_u32, safe_write,
    safe_write_str,
};
use std::panic::catch_unwind;

// ── safe_array_bound ────────────────────────────────────────────────

#[test]
fn array_bound_zero_count_returns_base() {
    assert_eq!(safe_array_bound(10, 0, 5, "ctx").unwrap(), 10);
}

#[test]
fn array_bound_zero_base_returns_product() {
    assert_eq!(safe_array_bound(0, 3, 4, "ctx").unwrap(), 12);
}

#[test]
fn array_bound_all_zeros() {
    assert_eq!(safe_array_bound(0, 0, 0, "ctx").unwrap(), 0);
}

#[test]
fn array_bound_overflow_count_times_item() {
    let err = safe_array_bound(0, usize::MAX, 2, "overflow").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

#[test]
fn array_bound_overflow_add_base() {
    let err = safe_array_bound(usize::MAX, 1, 1, "overflow-add").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

// ── safe_u64_to_u32 ────────────────────────────────────────────────

#[test]
fn u64_to_u32_zero() {
    assert_eq!(safe_u64_to_u32(0, "ctx").unwrap(), 0);
}

#[test]
fn u64_to_u32_max_valid() {
    assert_eq!(
        safe_u64_to_u32(u64::from(u32::MAX), "ctx").unwrap(),
        u32::MAX
    );
}

#[test]
fn u64_to_u32_overflow() {
    let err = safe_u64_to_u32(u64::from(u32::MAX) + 1, "overflow").unwrap_err();
    assert!(err.message.contains("overflow"));
}

// ── safe_u64_to_u16 ────────────────────────────────────────────────

#[test]
fn u64_to_u16_zero() {
    assert_eq!(safe_u64_to_u16(0, "ctx").unwrap(), 0);
}

#[test]
fn u64_to_u16_max_valid() {
    assert_eq!(
        safe_u64_to_u16(u64::from(u16::MAX), "ctx").unwrap(),
        u16::MAX
    );
}

#[test]
fn u64_to_u16_overflow() {
    assert!(safe_u64_to_u16(u64::from(u16::MAX) + 1, "overflow").is_err());
}

// ── safe_usize_to_u32 ──────────────────────────────────────────────

#[test]
fn usize_to_u32_zero() {
    assert_eq!(safe_usize_to_u32(0, "ctx").unwrap(), 0);
}

#[test]
fn usize_to_u32_max_valid() {
    assert_eq!(
        safe_usize_to_u32(u32::MAX as usize, "ctx").unwrap(),
        u32::MAX
    );
}

// ── re-exports work correctly ──────────────────────────────────────

#[test]
fn reexport_safe_divide() {
    assert_eq!(safe_divide(100, 10, "reexport").unwrap(), 10);
    assert!(safe_divide(1, 0, "reexport").is_err());
}

#[test]
fn reexport_safe_slice_get() {
    let data = [10u8, 20, 30];
    assert_eq!(safe_slice_get(&data, 2, "reexport").unwrap(), 30);
    assert!(safe_slice_get(&data, 3, "reexport").is_err());
}

#[test]
fn reexport_parse_usize() {
    assert_eq!(parse_usize("999", "reexport").unwrap(), 999);
}

#[test]
fn reexport_parse_isize() {
    assert_eq!(parse_isize("-100", "reexport").unwrap(), -100);
}

#[test]
fn reexport_safe_parse_u16() {
    assert_eq!(safe_parse_u16("256", "reexport").unwrap(), 256);
}

#[test]
fn reexport_safe_string_char_at() {
    assert_eq!(safe_string_char_at("hello", 4, "reexport").unwrap(), 'o');
}

#[test]
fn reexport_safe_write_and_write_str() {
    let mut buf = String::new();
    safe_write(&mut buf, format_args!("num={}", 42)).unwrap();
    safe_write_str(&mut buf, "!").unwrap();
    assert_eq!(buf, "num=42!");
}

// ── no-panic guarantee ─────────────────────────────────────────────

#[test]
fn array_bound_never_panics_on_overflow() {
    let result = catch_unwind(|| safe_array_bound(usize::MAX, usize::MAX, usize::MAX, "panic"));
    assert!(result.is_ok(), "safe_array_bound must not panic");
    assert!(result.unwrap().is_err());
}

#[test]
fn u64_to_u32_never_panics_on_max() {
    let result = catch_unwind(|| safe_u64_to_u32(u64::MAX, "panic"));
    assert!(result.is_ok(), "safe_u64_to_u32 must not panic");
    assert!(result.unwrap().is_err());
}

#[test]
fn u64_to_u16_never_panics_on_max() {
    let result = catch_unwind(|| safe_u64_to_u16(u64::MAX, "panic"));
    assert!(result.is_ok(), "safe_u64_to_u16 must not panic");
    assert!(result.unwrap().is_err());
}
