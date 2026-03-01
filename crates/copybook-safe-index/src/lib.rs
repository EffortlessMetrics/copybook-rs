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
///
/// # Errors
///
/// Returns `CBKP001_SYNTAX` if `denominator` is zero.
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
///
/// # Errors
///
/// Returns `CBKP001_SYNTAX` if `index` is out of bounds.
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

    // --- safe_divide edge cases ---

    #[test]
    fn safe_divide_zero_numerator() {
        assert_eq!(safe_divide(0, 5, "test").expect("0/5"), 0);
    }

    #[test]
    fn safe_divide_same_values() {
        assert_eq!(safe_divide(7, 7, "test").expect("7/7"), 1);
    }

    #[test]
    fn safe_divide_integer_truncation() {
        assert_eq!(safe_divide(7, 2, "test").expect("7/2"), 3);
    }

    #[test]
    fn safe_divide_large_values() {
        assert_eq!(
            safe_divide(usize::MAX, 1, "test").expect("max/1"),
            usize::MAX
        );
    }

    #[test]
    fn safe_divide_error_message_contains_context() {
        let err = safe_divide(1, 0, "my-context").unwrap_err();
        assert!(
            err.message.contains("my-context"),
            "Error message should contain context"
        );
    }

    // --- safe_slice_get edge cases ---

    #[test]
    fn safe_slice_get_empty_slice() {
        let data: &[u8] = &[];
        assert!(safe_slice_get(data, 0, "test").is_err());
    }

    #[test]
    fn safe_slice_get_first_element() {
        let data = [42u8];
        assert_eq!(safe_slice_get(&data, 0, "test").expect("first"), 42);
    }

    #[test]
    fn safe_slice_get_last_element() {
        let data = [1u8, 2, 3, 4, 5];
        assert_eq!(safe_slice_get(&data, 4, "test").expect("last"), 5);
    }

    #[test]
    fn safe_slice_get_exactly_out_of_bounds() {
        let data = [1u8, 2, 3];
        assert!(safe_slice_get(&data, 3, "test").is_err());
    }

    #[test]
    fn safe_slice_get_with_i32_type() {
        let data = [10i32, 20, 30];
        assert_eq!(safe_slice_get(&data, 2, "test").expect("i32"), 30);
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
