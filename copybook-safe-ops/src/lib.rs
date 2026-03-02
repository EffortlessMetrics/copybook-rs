// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe primitive operations used across copybook-rs.

use copybook_error::{Error, ErrorCode, Result};
use std::fmt::Write;

/// Safely convert a string to usize, returning an error on failure
///
/// # Errors
/// Returns an error if the string cannot be parsed as an unsigned integer.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn parse_usize(s: &str, context: &str) -> Result<usize> {
    s.parse().map_err(|_| {
        Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Invalid numeric value '{}' in {}", s, context),
        )
    })
}

/// Safely convert a string to isize, returning an error on failure
///
/// # Errors
/// Returns an error if the string cannot be parsed as a signed integer.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn parse_isize(s: &str, context: &str) -> Result<isize> {
    s.parse().map_err(|_| {
        Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Invalid signed numeric value '{}' in {}", s, context),
        )
    })
}

/// Safely divide two numbers, checking for division by zero
///
/// # Errors
/// Returns an error if the denominator is zero.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_divide(numerator: usize, denominator: usize, context: &str) -> Result<usize> {
    if denominator == 0 {
        return Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Division by zero in {}", context),
        ));
    }
    Ok(numerator / denominator)
}

/// Safely calculate COBOL array bounds with overflow protection
///
/// # Errors
/// Returns overflow-specific errors with detailed context.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_array_bound(
    base: usize,
    count: usize,
    item_size: usize,
    context: &str,
) -> Result<usize> {
    copybook_overflow::safe_array_bound(base, count, item_size, context)
}

/// Safely format data into string buffer.
///
/// # Errors
/// Returns an error if formatting into the buffer fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_write(buffer: &mut String, args: std::fmt::Arguments<'_>) -> Result<()> {
    buffer.write_fmt(args).map_err(|e| {
        Error::new(
            ErrorCode::CBKD101_INVALID_FIELD_TYPE,
            format!("String formatting error: {}", e),
        )
    })
}

/// Safely append string slice to buffer.
///
/// # Errors
/// Returns an error if writing to the buffer fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_write_str(buffer: &mut String, s: &str) -> Result<()> {
    buffer.write_str(s).map_err(|e| {
        Error::new(
            ErrorCode::CBKD101_INVALID_FIELD_TYPE,
            format!("String write error: {}", e),
        )
    })
}

/// Safely convert u64 to u32 with overflow checking.
///
/// # Errors
/// Returns an error if the value exceeds the u32 range.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_u64_to_u32(value: u64, context: &str) -> Result<u32> {
    copybook_overflow::safe_u64_to_u32(value, context)
}

/// Safely convert u64 to u16 with overflow checking.
///
/// # Errors
/// Returns an error if the value exceeds the u16 range.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_u64_to_u16(value: u64, context: &str) -> Result<u16> {
    copybook_overflow::safe_u64_to_u16(value, context)
}

/// Safely convert usize to u32 with overflow checking.
///
/// # Errors
/// Returns an error if the value exceeds the u32 range.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_usize_to_u32(value: usize, context: &str) -> Result<u32> {
    copybook_overflow::safe_usize_to_u32(value, context)
}

/// Access a slice index with explicit bounds checking.
///
/// # Errors
/// Returns an error when the requested index is out of bounds.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_slice_get<T>(slice: &[T], index: usize, context: &str) -> Result<T>
where
    T: Copy,
{
    if index < slice.len() {
        Ok(slice[index])
    } else {
        Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!(
                "Slice bounds violation in {}: index {} >= length {}",
                context,
                index,
                slice.len()
            ),
        ))
    }
}

/// Safely parse string as u16 with context.
///
/// # Errors
/// Returns an error if the string cannot be parsed as a u16.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_parse_u16(s: &str, context: &str) -> Result<u16> {
    s.parse().map_err(|_| {
        Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Invalid u16 value '{}' in {}", s, context),
        )
    })
}

/// Safely access string character with bounds checking.
///
/// # Errors
/// Returns an error if the index is out of bounds for the string.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_string_char_at(s: &str, index: usize, context: &str) -> Result<char> {
    s.chars().nth(index).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!(
                "String character access out of bounds in {}: index {} >= length {}",
                context,
                index,
                s.len()
            ),
        )
    })
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_usize() -> Result<()> {
        assert_eq!(parse_usize("123", "test")?, 123);
        assert!(parse_usize("bad", "test").is_err());
        Ok(())
    }

    #[test]
    fn test_safe_divide() -> Result<()> {
        assert_eq!(safe_divide(10, 2, "test")?, 5);
        assert!(safe_divide(10, 0, "test").is_err());
        Ok(())
    }

    #[test]
    fn test_safe_slice_get() -> Result<()> {
        let values = [1, 2, 3];
        assert_eq!(safe_slice_get(&values, 1, "test")?, 2);
        assert!(safe_slice_get(&values, 5, "test").is_err());
        Ok(())
    }
}
