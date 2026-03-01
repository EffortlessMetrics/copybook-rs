// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe conversion and arithmetic helper functions.
//!
//! This crate centralizes arithmetic-focused helpers and re-exports text-oriented
//! panic-safe operations from `copybook_safe_text`.

use copybook_error::Error;
pub use copybook_safe_index::{safe_divide, safe_slice_get};
pub use copybook_safe_text::{
    parse_isize, parse_usize, safe_parse_u16, safe_string_char_at, safe_write, safe_write_str,
};

/// Result type alias using `copybook-error`.
pub type Result<T> = std::result::Result<T, Error>;

/// Safely calculate COBOL array bounds with overflow protection.
///
/// # Errors
///
/// Returns an error if the computation overflows.
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

/// Safely convert `u64` to `u32` with overflow checking.
///
/// # Errors
///
/// Returns an error if `value` exceeds `u32::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_u64_to_u32(value: u64, context: &str) -> Result<u32> {
    copybook_overflow::safe_u64_to_u32(value, context)
}

/// Safely convert `u64` to `u16` with overflow checking.
///
/// # Errors
///
/// Returns an error if `value` exceeds `u16::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_u64_to_u16(value: u64, context: &str) -> Result<u16> {
    copybook_overflow::safe_u64_to_u16(value, context)
}

/// Safely convert `usize` to `u32` with overflow checking.
///
/// # Errors
///
/// Returns an error if `value` exceeds `u32::MAX`.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_usize_to_u32(value: usize, context: &str) -> Result<u32> {
    copybook_overflow::safe_usize_to_u32(value, context)
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;
    use copybook_error::ErrorCode;
    use proptest::prelude::*;

    #[test]
    fn safe_parse_usize() {
        assert_eq!(parse_usize("123", "test").expect("parse usize"), 123);
    }

    #[test]
    fn safe_parse_usize_invalid() {
        assert!(matches!(
            parse_usize("invalid", "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn safe_parse_isize() {
        assert_eq!(parse_isize("-42", "test").expect("parse isize"), -42);
    }

    #[test]
    fn safe_divide_returns_quotient_or_syntax_error() {
        assert_eq!(safe_divide(10, 2, "test").expect("divide"), 5);
        assert!(matches!(
            safe_divide(10, 0, "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn safe_array_bound_behavior() {
        assert_eq!(safe_array_bound(10, 3, 4, "test").expect("array bound"), 22);

        assert!(matches!(
            safe_array_bound(0, usize::MAX, 2, "overflow"),
            Err(error) if error.code == ErrorCode::CBKP021_ODO_NOT_TAIL
        ));
    }

    #[test]
    fn safe_parse_u16_valid_and_invalid() {
        assert_eq!(safe_parse_u16("42", "test").expect("parse u16"), 42);
        assert!(matches!(
            safe_parse_u16("99999", "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn safe_string_char_at_behavior() {
        assert_eq!(
            safe_string_char_at("abc", 1, "test").expect("char index"),
            'b'
        );
        assert!(matches!(
            safe_string_char_at("abc", 3, "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_lossless)]
    proptest! {
        #[test]
        fn safe_parse_usize_round_trip(value in 0u64..1_000_000u64) {
            let value = value as usize;
            let text = value.to_string();
            let parsed = parse_usize(&text, "prop").expect("roundtrip parse");
            prop_assert_eq!(parsed, value);
        }

        #[test]
        fn safe_slice_get_round_trip(value in 1u32..200u32) {
            let vec: Vec<u32> = (0..value).collect();
            let index = (value as usize) / 2;
            let got = safe_slice_get(&vec, index, "prop").expect("slice index");
            prop_assert_eq!(got, index as u32);
        }

        #[test]
        fn safe_array_bound_matches_reference(
            base in 0u32..1000u32,
            count in 0u32..1000u32,
            item in 1u32..100u32,
        ) {
            let base = base as usize;
            let count = count as usize;
            let item = item as usize;

            let expected = (count as u128)
                .checked_mul(item as u128)
                .and_then(|total| (base as u128).checked_add(total));

            match expected.and_then(|total| usize::try_from(total).ok()) {
                Some(expected) => {
                    prop_assert_eq!(safe_array_bound(base, count, item, "prop").expect("bounded"), expected);
                }
                None => {
                    prop_assert!(safe_array_bound(base, count, item, "prop").is_err());
                }
            }
        }
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_lossless)]
    proptest! {
        #[test]
        fn safe_u64_to_u32_round_trip(value in 0u64..=u32::MAX as u64) {
            prop_assert_eq!(
                safe_u64_to_u32(value, "prop").expect("u64->u32"),
                value as u32
            );
        }

        #[test]
        fn safe_u64_to_u16_round_trip(value in 0u64..=u16::MAX as u64) {
            prop_assert_eq!(
                safe_u64_to_u16(value, "prop").expect("u64->u16"),
                value as u16
            );
        }
    }
}
