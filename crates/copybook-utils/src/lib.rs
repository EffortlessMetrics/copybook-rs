// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe utility crate composing small, focused microcrates.

pub use copybook_safe_access::{OptionExt, Result, SliceExt, VecExt};

/// Utility functions for panic-safe operations.
pub mod safe_ops {
    pub use copybook_safe_ops::*;
}

#[cfg(test)]
#[allow(clippy::expect_used)]
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
    fn test_vec_ext_pop() -> Result<()> {
        let mut vec = vec![1, 2, 3];
        let value = vec.pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "test")?;
        assert_eq!(value, 3);
        Ok(())
    }

    #[test]
    fn test_safe_parse() -> Result<()> {
        let parsed = safe_ops::parse_usize("123", "test")?;
        assert_eq!(parsed, 123);
        Ok(())
    }
}
