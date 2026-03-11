// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe utility functions and extension traits
//!
//! This crate provides utilities to eliminate panic conditions in the copybook-rs
//! codebase, replacing `unwrap()` calls with structured error handling.

use copybook_error::Error;

/// Result type alias using copybook-error's Error
pub type Result<T> = std::result::Result<T, Error>;

pub use copybook_safe_collections::{OptionExt, SliceExt, VecExt};

pub mod safe_ops {
    pub use copybook_safe_ops::*;
}

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
