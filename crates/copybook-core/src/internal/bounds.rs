// SPDX-License-Identifier: AGPL-3.0-or-later
//! Checked integer narrowing for layout resolution, owned by `copybook-core`.
//!
//! These are the layout call sites formerly served by the `copybook-overflow`
//! package (#655 Cluster C). The stable error identity is unchanged
//! (`CBKS141_RECORD_TOO_LARGE` with identical messages); only the owner
//! moved, so the semantic layer assigns the failure it understands.

use crate::{Error, ErrorCode, Result};

/// Narrow `u64` to `u32` for record offsets and lengths.
///
/// # Errors
/// Returns `CBKS141_RECORD_TOO_LARGE` when `value > u32::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub(crate) fn u64_to_u32(value: u64, context: &str) -> Result<u32> {
    u32::try_from(value).map_err(|_| {
        Error::new(
            ErrorCode::CBKS141_RECORD_TOO_LARGE,
            format!(
                "Integer overflow converting u64 to u32 in {context}: {value} exceeds u32::MAX"
            ),
        )
    })
}

/// Narrow `u64` to `u16` for small quantities such as sync padding.
///
/// # Errors
/// Returns `CBKS141_RECORD_TOO_LARGE` when `value > u16::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub(crate) fn u64_to_u16(value: u64, context: &str) -> Result<u16> {
    u16::try_from(value).map_err(|_| {
        Error::new(
            ErrorCode::CBKS141_RECORD_TOO_LARGE,
            format!(
                "Integer overflow converting u64 to u16 in {context}: {value} exceeds u16::MAX"
            ),
        )
    })
}

/// Narrow `usize` to `u32` for source positions such as line numbers.
///
/// # Errors
/// Returns `CBKS141_RECORD_TOO_LARGE` when `value > u32::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub(crate) fn usize_to_u32(value: usize, context: &str) -> Result<u32> {
    u32::try_from(value).map_err(|_| {
        Error::new(
            ErrorCode::CBKS141_RECORD_TOO_LARGE,
            format!(
                "Integer overflow converting usize to u32 in {context}: {value} exceeds u32::MAX"
            ),
        )
    })
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn u64_to_u32_ok() {
        assert_eq!(u64_to_u32(123, "test").unwrap(), 123);
    }

    #[test]
    fn u64_to_u32_at_max_boundary() {
        assert_eq!(
            u64_to_u32(u64::from(u32::MAX), "boundary").unwrap(),
            u32::MAX
        );
    }

    #[test]
    fn u64_to_u32_overflow() {
        let err = u64_to_u32(u64::from(u32::MAX) + 1, "u64->u32").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[test]
    fn u64_to_u16_ok() {
        assert_eq!(u64_to_u16(123, "test").unwrap(), 123);
    }

    #[test]
    fn u64_to_u16_at_max_boundary() {
        assert_eq!(
            u64_to_u16(u64::from(u16::MAX), "boundary").unwrap(),
            u16::MAX
        );
    }

    #[test]
    fn u64_to_u16_overflow() {
        let err = u64_to_u16(u64::from(u16::MAX) + 1, "u64->u16").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[test]
    fn usize_to_u32_ok() {
        assert_eq!(usize_to_u32(123, "test").unwrap(), 123);
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn usize_to_u32_overflow() {
        let err = usize_to_u32(u32::MAX as usize + 1, "usize->u32").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }
}
