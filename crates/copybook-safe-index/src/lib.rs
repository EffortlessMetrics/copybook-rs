// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe helpers for low-level indexing and arithmetic operations.
//!
//! This crate isolates small, single-responsibility helpers that previously lived in
//! other panic-safe utility layers. All errors are translated into
//! `copybook-error` values.

use copybook_error::{Error, ErrorCode};

/// Result type alias using `copybook-error`.
pub type Result<T> = std::result::Result<T, Error>;

/// Safely divide two numbers, checking for division by zero.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn safe_divide(numerator: usize, denominator: usize, context: &str) -> Result<usize> {
    if denominator == 0 {
        return Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Division by zero in {context}"),
        ));
    }
    Ok(numerator / denominator)
}

/// Access a slice index with explicit bounds checking.
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
                "Slice bounds violation in {context}: index {index} >= length {}",
                slice.len()
            ),
        ))
    }
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn safe_divide_ok() {
        assert_eq!(safe_divide(10, 2, "test").expect("divide"), 5);
    }

    #[test]
    fn safe_divide_by_zero_is_error() {
        assert!(matches!(
            safe_divide(10, 0, "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn safe_slice_get_ok() {
        let data = [1u8, 2u8, 3u8];
        assert_eq!(safe_slice_get(&data, 1, "test").expect("index"), 2u8);
    }

    #[test]
    fn safe_slice_get_out_of_range_is_error() {
        let data = [1u8, 2u8, 3u8];
        assert!(matches!(
            safe_slice_get(&data, 99, "test"),
            Err(error) if error.code == ErrorCode::CBKP001_SYNTAX
        ));
    }

    proptest! {
        #[test]
        fn safe_divide_round_trip(
            numerator in 0usize..1_000_000usize,
            denominator in 1usize..1000usize,
        ) {
            prop_assert_eq!(
                safe_divide(numerator, denominator, "prop").expect("safe divide"),
                numerator / denominator
            );
        }

        #[test]
        fn safe_slice_get_round_trip(values in prop::collection::vec(0u8..=255u8, 1..200), index in 0usize..220usize) {
            let normalized_index = index % (values.len() + 1);
            let result = safe_slice_get(&values, normalized_index, "prop");

            if normalized_index < values.len() {
                prop_assert_eq!(result.expect("index in bounds"), values[normalized_index]);
            } else {
                prop_assert!(result.is_err());
            }
        }
    }
}
