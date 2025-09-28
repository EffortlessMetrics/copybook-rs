//! Panic-safe utility functions and extension traits
//!
//! This module provides utilities to eliminate panic conditions in the copybook-rs
//! codebase, replacing unwrap() calls with structured error handling.

use crate::Result;
use crate::error::{Error, ErrorCode};

/// Extension trait for `Option<T>` providing panic-safe unwrapping with context
pub trait OptionExt<T> {
    /// Unwrap an option safely, returning a structured error with context if None
    fn ok_or_cbkp_error(self, code: ErrorCode, message: impl Into<String>) -> Result<T>;

    /// Unwrap an option safely with a specific error context
    fn ok_or_error(self, error: Error) -> Result<T>;
}

impl<T> OptionExt<T> for Option<T> {
    fn ok_or_cbkp_error(self, code: ErrorCode, message: impl Into<String>) -> Result<T> {
        self.ok_or_else(|| Error::new(code, message.into()))
    }

    #[inline]
    fn ok_or_error(self, error: Error) -> Result<T> {
        self.ok_or(error)
    }
}

/// Extension trait for `Vec<T>` providing panic-safe access operations
pub trait VecExt<T> {
    /// Pop from vector safely, returning a structured error if empty
    fn pop_or_cbkp_error(&mut self, code: ErrorCode, message: impl Into<String>) -> Result<T>;

    /// Get last element safely, returning a structured error if empty
    fn last_or_cbkp_error(&self, code: ErrorCode, message: impl Into<String>) -> Result<&T>;

    /// Get last mutable element safely, returning a structured error if empty
    fn last_mut_or_cbkp_error(
        &mut self,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T>;
}

impl<T> VecExt<T> for Vec<T> {
    #[inline]
    fn pop_or_cbkp_error(&mut self, code: ErrorCode, message: impl Into<String>) -> Result<T> {
        self.pop().ok_or_cbkp_error(code, message)
    }

    #[inline]
    fn last_or_cbkp_error(&self, code: ErrorCode, message: impl Into<String>) -> Result<&T> {
        self.last().ok_or_cbkp_error(code, message)
    }

    #[inline]
    fn last_mut_or_cbkp_error(
        &mut self,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T> {
        self.last_mut().ok_or_cbkp_error(code, message)
    }
}

/// Extension trait for slice indexing providing panic-safe access
pub trait SliceExt<T> {
    /// Get element at index safely, returning a structured error if out of bounds
    fn get_or_cbkp_error(
        &self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&T>;

    /// Get mutable element at index safely, returning a structured error if out of bounds
    fn get_mut_or_cbkp_error(
        &mut self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T>;
}

impl<T> SliceExt<T> for [T] {
    #[inline]
    fn get_or_cbkp_error(
        &self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&T> {
        self.get(index).ok_or_cbkp_error(code, message)
    }

    #[inline]
    fn get_mut_or_cbkp_error(
        &mut self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T> {
        self.get_mut(index).ok_or_cbkp_error(code, message)
    }
}

/// Utility functions for panic-safe operations
pub mod safe_ops {
    use super::{Error, ErrorCode, Result};
    use std::fmt::Write;

    /// Safely convert a string to usize, returning an error on failure
    #[inline]
    pub fn parse_usize(s: &str, context: &str) -> Result<usize> {
        s.parse().map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("Invalid numeric value '{}' in {}", s, context),
            )
        })
    }

    /// Safely convert a string to isize, returning an error on failure
    #[inline]
    pub fn parse_isize(s: &str, context: &str) -> Result<isize> {
        s.parse().map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("Invalid signed numeric value '{}' in {}", s, context),
            )
        })
    }

    /// Safely divide two numbers, checking for division by zero
    #[inline]
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
    /// Critical for ODO (Occurs Depending On) arrays and OCCURS clauses where
    /// arithmetic overflow could compromise mainframe data processing integrity.
    ///
    /// # Performance
    /// Uses hardware overflow detection for maximum performance on modern CPUs.
    ///
    /// # Errors
    /// Returns `CBKP021_ODO_NOT_TAIL` for overflow conditions with detailed context.
    #[inline]
    pub fn safe_array_bound(
        base: usize,
        count: usize,
        item_size: usize,
        context: &str,
    ) -> Result<usize> {
        let total_size = count.checked_mul(item_size).ok_or_else(|| {
            Error::new(
                ErrorCode::CBKP021_ODO_NOT_TAIL,
                format!(
                    "Array size overflow in {}: {} * {} would overflow",
                    context, count, item_size
                ),
            )
        })?;

        base.checked_add(total_size).ok_or_else(|| {
            Error::new(
                ErrorCode::CBKP021_ODO_NOT_TAIL,
                format!(
                    "Array offset overflow in {}: {} + {} would overflow",
                    context, base, total_size
                ),
            )
        })
    }

    /// Safely format data into string buffer for JSON generation
    ///
    /// Used in high-performance COBOL to JSON conversion where formatting
    /// errors must be handled gracefully without panics.
    ///
    /// # Performance
    /// Zero allocation overhead beyond normal string formatting.
    #[inline]
    pub fn safe_write(buffer: &mut String, args: std::fmt::Arguments<'_>) -> Result<()> {
        buffer.write_fmt(args).map_err(|e| {
            Error::new(
                ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                format!("String formatting error: {}", e),
            )
        })
    }

    /// Safely append string slice to buffer for JSON field construction
    ///
    /// Optimized for high-throughput JSON generation during COBOL data conversion
    /// with comprehensive error handling for enterprise reliability.
    ///
    /// # Performance
    /// Single bounds check and direct memory copy for maximum efficiency.
    #[inline]
    pub fn safe_write_str(buffer: &mut String, s: &str) -> Result<()> {
        buffer.write_str(s).map_err(|e| {
            Error::new(
                ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                format!("String write error: {}", e),
            )
        })
    }

    /// Safely convert u64 to u32 with overflow checking
    ///
    /// Critical for COBOL field offset calculations where overflow could
    /// compromise mainframe data processing accuracy.
    #[inline]
    pub fn safe_u64_to_u32(value: u64, context: &str) -> Result<u32> {
        u32::try_from(value).map_err(|_| {
            Error::new(
                ErrorCode::CBKS141_RECORD_TOO_LARGE,
                format!(
                    "Integer overflow converting u64 to u32 in {}: {} exceeds u32::MAX",
                    context, value
                ),
            )
        })
    }

    /// Safely convert u64 to u16 with overflow checking
    ///
    /// Used for sync padding calculations and small field sizes where
    /// overflow protection is essential for enterprise reliability.
    #[inline]
    pub fn safe_u64_to_u16(value: u64, context: &str) -> Result<u16> {
        u16::try_from(value).map_err(|_| {
            Error::new(
                ErrorCode::CBKS141_RECORD_TOO_LARGE,
                format!(
                    "Integer overflow converting u64 to u16 in {}: {} exceeds u16::MAX",
                    context, value
                ),
            )
        })
    }

    /// Safely convert usize to u32 with overflow checking
    #[inline]
    pub fn safe_usize_to_u32(value: usize, context: &str) -> Result<u32> {
        u32::try_from(value).map_err(|_| {
            Error::new(
                ErrorCode::CBKS141_RECORD_TOO_LARGE,
                format!(
                    "Integer overflow converting usize to u32 in {}: {} exceeds u32::MAX",
                    context, value
                ),
            )
        })
    }

    /// Safely access slice with bounds checking for token streams
    ///
    /// Essential for parser token access where bounds violations could cause
    /// panics during COBOL copybook processing.
    #[inline]
    pub fn safe_slice_get<T>(slice: &[T], index: usize, context: &str) -> Result<T>
    where
        T: Copy,
    {
        slice.get(index).copied().ok_or_else(|| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!(
                    "Slice bounds violation in {}: index {} >= length {}",
                    context,
                    index,
                    slice.len()
                ),
            )
        })
    }

    /// Safely parse string as u16 with context
    ///
    /// Used for COBOL numeric field parsing where invalid digits could cause
    /// parse errors during copybook processing.
    #[inline]
    pub fn safe_parse_u16(s: &str, context: &str) -> Result<u16> {
        s.parse().map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("Invalid u16 value '{}' in {}", s, context),
            )
        })
    }

    /// Safely access string character with bounds checking
    ///
    /// Essential for PIC clause parsing where character access beyond string
    /// bounds could cause panics during COBOL syntax processing.
    #[inline]
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
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::{OptionExt, VecExt, safe_ops};
    use crate::error::ErrorCode;

    #[test]
    fn test_option_ext_some() {
        let opt = Some(42);
        assert_eq!(
            opt.ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test")
                .unwrap(),
            42
        );
    }

    #[test]
    fn test_option_ext_none() {
        let opt: Option<i32> = None;
        let result = opt.ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test error");
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::CBKP001_SYNTAX);
    }

    #[test]
    fn test_vec_ext_pop() {
        let mut vec = vec![1, 2, 3];
        assert_eq!(
            vec.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test")
                .unwrap(),
            3
        );

        let mut empty_vec: Vec<i32> = vec![];
        let result = empty_vec.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test error");
        assert!(result.is_err());
    }

    #[test]
    fn test_safe_parse() {
        assert_eq!(safe_ops::parse_usize("123", "test").unwrap(), 123);

        let result = safe_ops::parse_usize("invalid", "test");
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::CBKP001_SYNTAX);
    }

    #[test]
    fn test_safe_divide() {
        assert_eq!(safe_ops::safe_divide(10, 2, "test").unwrap(), 5);

        let result = safe_ops::safe_divide(10, 0, "test");
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, ErrorCode::CBKP001_SYNTAX);
    }
}
