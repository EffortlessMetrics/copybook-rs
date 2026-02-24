// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe parsing and string helper functions.
//!
//! This crate isolates text-oriented "fallible" operations so numeric logic can
//! be delegated to arithmetic-focused crates.

use copybook_error::{Error, ErrorCode};

/// Result type alias using `copybook-error`.
pub type Result<T> = std::result::Result<T, Error>;

/// Safely convert a string to `usize`, returning an error on failure.
///
/// # Errors
///
/// Returns `CBKP001_SYNTAX` if `s` cannot be parsed as `usize`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn parse_usize(s: &str, context: &str) -> Result<usize> {
    s.parse().map_err(|_| {
        Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Invalid numeric value '{s}' in {context}"),
        )
    })
}

/// Safely convert a string to `isize`, returning an error on failure.
///
/// # Errors
///
/// Returns `CBKP001_SYNTAX` if `s` cannot be parsed as `isize`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn parse_isize(s: &str, context: &str) -> Result<isize> {
    s.parse().map_err(|_| {
        Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Invalid signed numeric value '{s}' in {context}"),
        )
    })
}

/// Safely convert `u16` with parse error handling.
///
/// # Errors
///
/// Returns `CBKP001_SYNTAX` if `s` cannot be parsed as `u16`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_parse_u16(s: &str, context: &str) -> Result<u16> {
    s.parse().map_err(|_| {
        Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Invalid u16 value '{s}' in {context}"),
        )
    })
}

/// Safely access a string character with bounds checking.
///
/// # Errors
///
/// Returns `CBKP001_SYNTAX` if `index` is out of bounds.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_string_char_at(s: &str, index: usize, context: &str) -> Result<char> {
    s.chars().nth(index).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!(
                "String character access out of bounds in {context}: index {index} >= length {}",
                s.len()
            ),
        )
    })
}

/// Safely format data into a string buffer for JSON generation.
///
/// # Errors
///
/// Returns `CBKD101_INVALID_FIELD_TYPE` if formatting fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_write(buffer: &mut String, args: std::fmt::Arguments<'_>) -> Result<()> {
    use std::fmt::Write;
    buffer.write_fmt(args).map_err(|e| {
        Error::new(
            ErrorCode::CBKD101_INVALID_FIELD_TYPE,
            format!("String formatting error: {e}"),
        )
    })
}

/// Safely append a string slice to a buffer for JSON field construction.
///
/// # Errors
///
/// Returns `CBKD101_INVALID_FIELD_TYPE` if the write fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_write_str(buffer: &mut String, s: &str) -> Result<()> {
    use std::fmt::Write;
    buffer.write_str(s).map_err(|e| {
        Error::new(
            ErrorCode::CBKD101_INVALID_FIELD_TYPE,
            format!("String write error: {e}"),
        )
    })
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn parse_usize_ok() {
        assert_eq!(parse_usize("123", "test").expect("parse usize"), 123);
    }

    #[test]
    fn parse_usize_err() {
        assert!(matches!(
            parse_usize("invalid", "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn parse_isize_ok() {
        assert_eq!(parse_isize("-42", "test").expect("parse isize"), -42);
    }

    #[test]
    fn safe_parse_u16_ok_and_err() {
        assert_eq!(safe_parse_u16("42", "test").expect("parse u16"), 42);
        assert!(matches!(
            safe_parse_u16("99999", "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn safe_string_char_at_ok() {
        assert_eq!(
            safe_string_char_at("abc", 1, "test").expect("char index"),
            'b'
        );
    }

    #[test]
    fn safe_string_char_at_err() {
        assert!(matches!(
            safe_string_char_at("abc", 3, "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }
}
