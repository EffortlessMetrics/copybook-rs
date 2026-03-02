// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe utility functions and extension traits
//!
//! This crate provides utilities to eliminate panic conditions in the copybook-rs
//! codebase, replacing unwrap() calls with structured error handling.

use copybook_error::{Error, ErrorCode};

/// Result type alias using copybook-error's Error
pub type Result<T> = std::result::Result<T, Error>;

/// Extension trait for `Option<T>` providing panic-safe unwrapping with context
pub trait OptionExt<T> {
    /// Unwrap an option safely, returning a structured error with context if None
    fn ok_or_cbkp_error(self, code: ErrorCode, message: impl Into<String>) -> Result<T>;

    /// Unwrap an option safely with a specific error context
    fn ok_or_error(self, error: Error) -> Result<T>;
}

impl<T> OptionExt<T> for Option<T> {
    // PERFORMANCE OPTIMIZATION: Aggressive inlining for hot path error handling
    #[allow(clippy::inline_always)]
    #[inline]
    fn ok_or_cbkp_error(self, code: ErrorCode, message: impl Into<String>) -> Result<T> {
        match self {
            Some(value) => Ok(value),
            None => {
                // Mark error construction as cold path
                Err(Error::new(code, message.into()))
            }
        }
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn ok_or_error(self, error: Error) -> Result<T> {
        match self {
            Some(value) => Ok(value),
            None => Err(error),
        }
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
    // PERFORMANCE OPTIMIZATION: Aggressive inlining for hot path vector operations
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn pop_or_cbkp_error(&mut self, code: ErrorCode, message: impl Into<String>) -> Result<T> {
        // Fast path: check length and pop in one operation
        match self.pop() {
            Some(value) => Ok(value),
            None => Err(Error::new(code, message.into())),
        }
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn last_or_cbkp_error(&self, code: ErrorCode, message: impl Into<String>) -> Result<&T> {
        // Fast path: direct slice access when non-empty
        #[allow(clippy::if_not_else)]
        if !self.is_empty() {
            // SAFETY: We just checked that the vector is not empty
            Ok(&self[self.len() - 1])
        } else {
            Err(Error::new(code, message.into()))
        }
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn last_mut_or_cbkp_error(
        &mut self,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T> {
        // Fast path: direct slice access when non-empty
        #[allow(clippy::if_not_else)]
        if !self.is_empty() {
            let len = self.len();
            // SAFETY: We just checked that the vector is not empty
            Ok(&mut self[len - 1])
        } else {
            Err(Error::new(code, message.into()))
        }
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
    // PERFORMANCE OPTIMIZATION: Aggressive inlining for hot path slice access
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn get_or_cbkp_error(
        &self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&T> {
        // Fast path: bounds check with likely hint
        if index < self.len() {
            // SAFETY: We just checked the bounds above
            Ok(&self[index])
        } else {
            Err(Error::new(code, message.into()))
        }
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn get_mut_or_cbkp_error(
        &mut self,
        index: usize,
        code: ErrorCode,
        message: impl Into<String>,
    ) -> Result<&mut T> {
        // Fast path: bounds check with likely hint
        if index < self.len() {
            // SAFETY: We just checked the bounds above
            Ok(&mut self[index])
        } else {
            Err(Error::new(code, message.into()))
        }
    }
}

/// Utility functions for panic-safe operations
pub use copybook_safe_ops as safe_ops;

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::{OptionExt, VecExt, safe_ops};
    use copybook_error::{ErrorCode, Result};

    #[test]
    fn test_option_ext_some() -> Result<()> {
        let opt = Some(42);
        let value = opt.ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test")?;
        assert_eq!(value, 42);
        Ok(())
    }

    #[test]
    fn test_option_ext_none() {
        let opt: Option<i32> = None;
        let result = opt.ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test error");
        assert!(matches!(
            result,
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn test_vec_ext_pop() -> Result<()> {
        let mut vec = vec![1, 2, 3];
        let value = vec.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test")?;
        assert_eq!(value, 3);
        Ok(())
    }

    #[test]
    fn test_vec_ext_pop_empty() {
        let mut empty_vec: Vec<i32> = vec![];
        let result = empty_vec.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test error");
        assert!(matches!(result, Err(error) if error.code == ErrorCode::CBKP001_SYNTAX));
    }

    #[test]
    fn test_safe_parse() -> Result<()> {
        let parsed = safe_ops::parse_usize("123", "test")?;
        assert_eq!(parsed, 123);

        let result = safe_ops::parse_usize("invalid", "test");
        assert!(matches!(
            result,
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));

        Ok(())
    }

    #[test]
    fn test_safe_divide() -> Result<()> {
        let quotient = safe_ops::safe_divide(10, 2, "test")?;
        assert_eq!(quotient, 5);

        let result = safe_ops::safe_divide(10, 0, "test");
        assert!(matches!(
            result,
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));

        Ok(())
    }
}
