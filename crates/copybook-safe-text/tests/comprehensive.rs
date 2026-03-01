// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-safe-text.

use copybook_error::ErrorCode;
use copybook_safe_text::{
    parse_isize, parse_usize, safe_parse_u16, safe_string_char_at, safe_write, safe_write_str,
};
use std::panic::catch_unwind;

// ── parse_usize ─────────────────────────────────────────────────────

#[test]
fn parse_usize_valid() {
    assert_eq!(parse_usize("0", "ctx").unwrap(), 0);
    assert_eq!(parse_usize("12345", "ctx").unwrap(), 12345);
}

#[test]
fn parse_usize_empty_is_error() {
    let err = parse_usize("", "empty-ctx").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn parse_usize_negative_is_error() {
    assert!(parse_usize("-1", "ctx").is_err());
}

#[test]
fn parse_usize_whitespace_is_error() {
    assert!(parse_usize(" 5", "ctx").is_err());
    assert!(parse_usize("5 ", "ctx").is_err());
}

#[test]
fn parse_usize_non_numeric_is_error() {
    assert!(parse_usize("abc", "ctx").is_err());
    assert!(parse_usize("12.5", "ctx").is_err());
}

// ── parse_isize ─────────────────────────────────────────────────────

#[test]
fn parse_isize_positive_and_negative() {
    assert_eq!(parse_isize("0", "ctx").unwrap(), 0);
    assert_eq!(parse_isize("42", "ctx").unwrap(), 42);
    assert_eq!(parse_isize("-42", "ctx").unwrap(), -42);
}

#[test]
fn parse_isize_empty_is_error() {
    assert!(parse_isize("", "ctx").is_err());
}

#[test]
fn parse_isize_float_is_error() {
    assert!(parse_isize("3.14", "ctx").is_err());
}

// ── safe_parse_u16 ──────────────────────────────────────────────────

#[test]
fn parse_u16_boundaries() {
    assert_eq!(safe_parse_u16("0", "ctx").unwrap(), 0);
    assert_eq!(safe_parse_u16("65535", "ctx").unwrap(), u16::MAX);
}

#[test]
fn parse_u16_overflow_is_error() {
    assert!(safe_parse_u16("65536", "ctx").is_err());
    assert!(safe_parse_u16("100000", "ctx").is_err());
}

#[test]
fn parse_u16_negative_is_error() {
    assert!(safe_parse_u16("-1", "ctx").is_err());
}

// ── safe_string_char_at ─────────────────────────────────────────────

#[test]
fn char_at_ascii() {
    assert_eq!(safe_string_char_at("abcde", 0, "ctx").unwrap(), 'a');
    assert_eq!(safe_string_char_at("abcde", 4, "ctx").unwrap(), 'e');
}

#[test]
fn char_at_empty_string_is_error() {
    let err = safe_string_char_at("", 0, "empty-ctx").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn char_at_past_end_is_error() {
    assert!(safe_string_char_at("abc", 3, "ctx").is_err());
    assert!(safe_string_char_at("abc", usize::MAX, "ctx").is_err());
}

#[test]
fn char_at_multibyte_utf8() {
    // Each char is multi-byte in UTF-8 but index is by char position
    let s = "日本語";
    assert_eq!(safe_string_char_at(s, 0, "ctx").unwrap(), '日');
    assert_eq!(safe_string_char_at(s, 1, "ctx").unwrap(), '本');
    assert_eq!(safe_string_char_at(s, 2, "ctx").unwrap(), '語');
    assert!(safe_string_char_at(s, 3, "ctx").is_err());
}

#[test]
fn char_at_emoji() {
    let s = "🦀🐍";
    assert_eq!(safe_string_char_at(s, 0, "ctx").unwrap(), '🦀');
    assert_eq!(safe_string_char_at(s, 1, "ctx").unwrap(), '🐍');
    assert!(safe_string_char_at(s, 2, "ctx").is_err());
}

// ── safe_write ──────────────────────────────────────────────────────

#[test]
fn write_formats_correctly() {
    let mut buf = String::new();
    safe_write(&mut buf, format_args!("val={},ok={}", 123, true)).unwrap();
    assert_eq!(buf, "val=123,ok=true");
}

#[test]
fn write_appends_to_existing() {
    let mut buf = String::from("prefix:");
    safe_write(&mut buf, format_args!("suffix")).unwrap();
    assert_eq!(buf, "prefix:suffix");
}

#[test]
fn write_empty_format() {
    let mut buf = String::new();
    safe_write(&mut buf, format_args!("")).unwrap();
    assert_eq!(buf, "");
}

// ── safe_write_str ──────────────────────────────────────────────────

#[test]
fn write_str_basic() {
    let mut buf = String::new();
    safe_write_str(&mut buf, "hello").unwrap();
    assert_eq!(buf, "hello");
}

#[test]
fn write_str_empty() {
    let mut buf = String::from("unchanged");
    safe_write_str(&mut buf, "").unwrap();
    assert_eq!(buf, "unchanged");
}

#[test]
fn write_str_unicode() {
    let mut buf = String::new();
    safe_write_str(&mut buf, "Ω∑π").unwrap();
    assert_eq!(buf, "Ω∑π");
}

#[test]
fn write_str_multiple_appends() {
    let mut buf = String::new();
    safe_write_str(&mut buf, "a").unwrap();
    safe_write_str(&mut buf, "b").unwrap();
    safe_write_str(&mut buf, "c").unwrap();
    assert_eq!(buf, "abc");
}

// ── no-panic guarantees ─────────────────────────────────────────────

#[test]
fn parse_usize_never_panics() {
    let result = catch_unwind(|| parse_usize("not_a_number", "panic-test"));
    assert!(result.is_ok(), "parse_usize must not panic");
}

#[test]
fn char_at_never_panics_on_empty() {
    let result = catch_unwind(|| safe_string_char_at("", 0, "panic-test"));
    assert!(
        result.is_ok(),
        "safe_string_char_at must not panic on empty string"
    );
    assert!(result.unwrap().is_err());
}

#[test]
fn char_at_never_panics_on_max_index() {
    let result = catch_unwind(|| safe_string_char_at("abc", usize::MAX, "panic-test"));
    assert!(
        result.is_ok(),
        "safe_string_char_at must not panic on usize::MAX"
    );
    assert!(result.unwrap().is_err());
}
