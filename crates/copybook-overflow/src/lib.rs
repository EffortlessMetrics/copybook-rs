// SPDX-License-Identifier: AGPL-3.0-or-later
//! Overflow-safe numeric guards for copybook-rs.
//!
//! This crate isolates checked arithmetic and checked narrowing conversions
//! that are performance-sensitive and correctness-critical.

use copybook_error::{Error, ErrorCode, Result};

/// Safely calculate COBOL array bounds with overflow protection.
///
/// # Errors
/// Returns `CBKP021_ODO_NOT_TAIL` for multiplication/addition overflow.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_array_bound(
    base: usize,
    count: usize,
    item_size: usize,
    context: &str,
) -> Result<usize> {
    let total_size = count.checked_mul(item_size).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKP021_ODO_NOT_TAIL,
            format!("Array size overflow in {context}: {count} * {item_size} would overflow"),
        )
    })?;

    base.checked_add(total_size).ok_or_else(|| {
        Error::new(
            ErrorCode::CBKP021_ODO_NOT_TAIL,
            format!("Array offset overflow in {context}: {base} + {total_size} would overflow"),
        )
    })
}

/// Safely convert `u64` to `u32` with overflow checking.
///
/// # Errors
/// Returns `CBKS141_RECORD_TOO_LARGE` when `value > u32::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_u64_to_u32(value: u64, context: &str) -> Result<u32> {
    u32::try_from(value).map_err(|_| {
        Error::new(
            ErrorCode::CBKS141_RECORD_TOO_LARGE,
            format!(
                "Integer overflow converting u64 to u32 in {context}: {value} exceeds u32::MAX"
            ),
        )
    })
}

/// Safely convert `u64` to `u16` with overflow checking.
///
/// # Errors
/// Returns `CBKS141_RECORD_TOO_LARGE` when `value > u16::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_u64_to_u16(value: u64, context: &str) -> Result<u16> {
    u16::try_from(value).map_err(|_| {
        Error::new(
            ErrorCode::CBKS141_RECORD_TOO_LARGE,
            format!(
                "Integer overflow converting u64 to u16 in {context}: {value} exceeds u16::MAX"
            ),
        )
    })
}

/// Safely convert `usize` to `u32` with overflow checking.
///
/// # Errors
/// Returns `CBKS141_RECORD_TOO_LARGE` when `value > u32::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_usize_to_u32(value: usize, context: &str) -> Result<u32> {
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
    fn safe_array_bound_ok() {
        let value = safe_array_bound(10, 3, 4, "test");
        assert_eq!(value.unwrap(), 22);
    }

    #[test]
    fn safe_array_bound_mul_overflow() {
        let err = safe_array_bound(0, usize::MAX, 2, "mul-overflow").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn safe_array_bound_add_overflow() {
        let err = safe_array_bound(usize::MAX - 1, 1, 2, "add-overflow").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn safe_u64_to_u32_ok() {
        let value = safe_u64_to_u32(123, "test");
        assert_eq!(value.unwrap(), 123);
    }

    #[test]
    fn safe_u64_to_u32_overflow() {
        let err = safe_u64_to_u32(u64::from(u32::MAX) + 1, "u64->u32").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[test]
    fn safe_u64_to_u16_ok() {
        let value = safe_u64_to_u16(123, "test");
        assert_eq!(value.unwrap(), 123);
    }

    #[test]
    fn safe_u64_to_u16_overflow() {
        let err = safe_u64_to_u16(u64::from(u16::MAX) + 1, "u64->u16").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[test]
    fn safe_usize_to_u32_ok() {
        let value = safe_usize_to_u32(123, "test");
        assert_eq!(value.unwrap(), 123);
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn safe_usize_to_u32_overflow() {
        let err = safe_usize_to_u32(u32::MAX as usize + 1, "usize->u32").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }
}
