// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for overflow-safe arithmetic crate.
//!
//! Verifies that narrowing never panics, bounds arithmetic stays in range,
//! and results are correct for boundary values.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_overflow::{safe_array_bound, safe_u64_to_u16, safe_u64_to_u32, safe_usize_to_u32};
use proptest::prelude::*;

use super::config::*;

// ---------------------------------------------------------------------------
// Narrowing never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// safe_u64_to_u32 never panics for any u64 input.
    #[test]
    fn prop_u64_to_u32_never_panics(value in any::<u64>()) {
        let result = safe_u64_to_u32(value, "test");
        // Should always return Ok or Err, never panic
        if value <= u64::from(u32::MAX) {
            prop_assert!(result.is_ok());
            prop_assert_eq!(result.unwrap(), value as u32);
        } else {
            prop_assert!(result.is_err());
        }
    }

    /// safe_u64_to_u16 never panics for any u64 input.
    #[test]
    fn prop_u64_to_u16_never_panics(value in any::<u64>()) {
        let result = safe_u64_to_u16(value, "test");
        if value <= u64::from(u16::MAX) {
            prop_assert!(result.is_ok());
            prop_assert_eq!(result.unwrap(), value as u16);
        } else {
            prop_assert!(result.is_err());
        }
    }

    /// safe_usize_to_u32 never panics for any usize input.
    #[test]
    fn prop_usize_to_u32_never_panics(value in any::<usize>()) {
        let result = safe_usize_to_u32(value, "test");
        if value <= u32::MAX as usize {
            prop_assert!(result.is_ok());
            prop_assert_eq!(result.unwrap(), value as u32);
        } else {
            prop_assert!(result.is_err());
        }
    }
}

// ---------------------------------------------------------------------------
// Bounds arithmetic stays in range
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// safe_array_bound never panics regardless of inputs.
    #[test]
    fn prop_array_bound_never_panics(
        base in any::<usize>(),
        count in any::<usize>(),
        item_size in any::<usize>(),
    ) {
        let result = safe_array_bound(base, count, item_size, "test");
        // Should return Ok or Err, never panic
        let _ = result;
    }

    /// When safe_array_bound succeeds, the result equals base + count * item_size.
    #[test]
    fn prop_array_bound_correct_when_ok(
        base in 0usize..=1_000_000,
        count in 0usize..=10_000,
        item_size in 0usize..=10_000,
    ) {
        let result = safe_array_bound(base, count, item_size, "test");
        if let Ok(value) = result {
            let expected = base + count * item_size;
            prop_assert_eq!(value, expected);
        }
    }

    /// Large inputs that would overflow produce an error.
    #[test]
    fn prop_array_bound_overflow_is_error(
        count in (usize::MAX / 2)..=usize::MAX,
        item_size in 2usize..=100,
    ) {
        let result = safe_array_bound(0, count, item_size, "overflow");
        prop_assert!(result.is_err(), "Should overflow: {} * {}", count, item_size);
    }
}

// ---------------------------------------------------------------------------
// Results are correct for boundary values
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Values at or just below u32::MAX succeed, values above fail.
    #[test]
    fn prop_u32_boundary_correct(offset in 0u64..=10) {
        let boundary = u64::from(u32::MAX);

        // At boundary: should succeed
        let at = safe_u64_to_u32(boundary, "boundary");
        prop_assert!(at.is_ok());
        prop_assert_eq!(at.unwrap(), u32::MAX);

        // Above boundary: should fail
        let above = safe_u64_to_u32(boundary + 1 + offset, "above");
        prop_assert!(above.is_err());

        // Below boundary: should succeed
        if boundary >= offset {
            let below = safe_u64_to_u32(boundary - offset, "below");
            prop_assert!(below.is_ok());
        }
    }

    /// Values at or just below u16::MAX succeed, values above fail.
    #[test]
    fn prop_u16_boundary_correct(offset in 0u64..=10) {
        let boundary = u64::from(u16::MAX);

        let at = safe_u64_to_u16(boundary, "boundary");
        prop_assert!(at.is_ok());
        prop_assert_eq!(at.unwrap(), u16::MAX);

        let above = safe_u64_to_u16(boundary + 1 + offset, "above");
        prop_assert!(above.is_err());

        if boundary >= offset {
            let below = safe_u64_to_u16(boundary - offset, "below");
            prop_assert!(below.is_ok());
        }
    }

    /// safe_array_bound with zero count always returns base.
    #[test]
    fn prop_array_bound_zero_count(base in 0usize..=usize::MAX) {
        let result = safe_array_bound(base, 0, 100, "zero-count");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), base);
    }

    /// safe_array_bound with zero item_size always returns base.
    #[test]
    fn prop_array_bound_zero_item_size(
        base in 0usize..=usize::MAX,
        count in 0usize..=usize::MAX,
    ) {
        let result = safe_array_bound(base, count, 0, "zero-item");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), base);
    }
}
