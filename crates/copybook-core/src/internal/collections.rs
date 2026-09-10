// SPDX-License-Identifier: AGPL-3.0-or-later
//! Checked stack operations for the parser, owned by `copybook-core`.
//!
//! The one live `VecExt` operation (`pop_or_cbkp_error`, formerly from the
//! `copybook-utils` package) moves here with its callers (#655 Cluster C).
//! The remaining `VecExt`/`OptionExt`/`SliceExt` methods have no production
//! callers and stay behind for the shim-retirement PR.

use crate::{Error, ErrorCode, Result};

/// Pop from a parser stack, returning a structured error on underflow.
///
/// # Errors
/// Returns the caller-supplied `code` with `message` when the stack is empty.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub(crate) fn pop_or_error<T>(
    stack: &mut Vec<T>,
    code: ErrorCode,
    message: impl Into<String>,
) -> Result<T> {
    match stack.pop() {
        Some(value) => Ok(value),
        None => Err(Error::new(code, message.into())),
    }
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn pop_or_error_ok() {
        let mut stack = vec![1, 2, 3];
        assert_eq!(
            pop_or_error(&mut stack, ErrorCode::CBKP001_SYNTAX, "test").unwrap(),
            3
        );
        assert_eq!(stack.len(), 2);
    }

    #[test]
    fn pop_or_error_underflow_keeps_caller_code() {
        let mut stack: Vec<u8> = Vec::new();
        let err = pop_or_error(&mut stack, ErrorCode::CBKP001_SYNTAX, "underflow").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP001_SYNTAX);
    }
}
