// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for overflow-safe arithmetic and narrowing conversions.
//!
//! Tests that `copybook-overflow` functions never panic, correctly report
//! errors for out-of-range values, and yield correct results at boundary
//! values. Complements `prop_overflow_safety.rs` with additional coverage
//! for i64/u64 extremes, chained operations, and stress scenarios.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_overflow::{safe_array_bound, safe_u64_to_u16, safe_u64_to_u32, safe_usize_to_u32};
use proptest::prelude::*;

use super::config::*;

// ---------------------------------------------------------------------------
// Narrowing: arbitrary i64 cast to u64 then narrow — never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Casting arbitrary i64 to u64 and narrowing to u32 never panics.
    #[test]
    fn prop_i64_to_u32_never_panics(value in any::<i64>()) {
        // Negative i64 → wrapping u64 → always fails narrow
        let as_u64 = value as u64;
        let result = safe_u64_to_u32(as_u64, "i64-test");
        if as_u64 <= u64::from(u32::MAX) {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }

    /// Casting arbitrary i64 to u64 and narrowing to u16 never panics.
    #[test]
    fn prop_i64_to_u16_never_panics(value in any::<i64>()) {
        let as_u64 = value as u64;
        let result = safe_u64_to_u16(as_u64, "i64-test");
        if as_u64 <= u64::from(u16::MAX) {
            prop_assert!(result.is_ok());
        } else {
            prop_assert!(result.is_err());
        }
    }
}

// ---------------------------------------------------------------------------
// Narrowing: specific extremes always caught
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Values in the range (u32::MAX, u64::MAX] always fail u64→u32 narrowing.
    #[test]
    fn prop_u64_above_u32_max_always_error(
        offset in 1u64..=1_000_000,
    ) {
        let value = u64::from(u32::MAX).saturating_add(offset);
        if value > u64::from(u32::MAX) {
            let result = safe_u64_to_u32(value, "above-max");
            prop_assert!(result.is_err());
        }
    }

    /// Values in the range (u16::MAX, u64::MAX] always fail u64→u16 narrowing.
    #[test]
    fn prop_u64_above_u16_max_always_error(
        offset in 1u64..=1_000_000,
    ) {
        let value = u64::from(u16::MAX).saturating_add(offset);
        if value > u64::from(u16::MAX) {
            let result = safe_u64_to_u16(value, "above-max");
            prop_assert!(result.is_err());
        }
    }

    /// Values in [0, u32::MAX] always succeed u64→u32 narrowing.
    #[test]
    fn prop_u64_within_u32_range_always_ok(value in 0u64..=u64::from(u32::MAX)) {
        let result = safe_u64_to_u32(value, "in-range");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), value as u32);
    }

    /// Values in [0, u16::MAX] always succeed u64→u16 narrowing.
    #[test]
    fn prop_u64_within_u16_range_always_ok(value in 0u64..=u64::from(u16::MAX)) {
        let result = safe_u64_to_u16(value, "in-range");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), value as u16);
    }
}

// ---------------------------------------------------------------------------
// safe_usize_to_u32 with arbitrary usize — never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// safe_usize_to_u32 never panics for any usize, and is correct.
    #[test]
    fn prop_usize_to_u32_correct(value in any::<usize>()) {
        let result = safe_usize_to_u32(value, "prop");
        #[allow(clippy::cast_possible_truncation)]
        if value <= u32::MAX as usize {
            prop_assert!(result.is_ok());
            prop_assert_eq!(result.unwrap(), value as u32);
        } else {
            prop_assert!(result.is_err());
        }
    }
}

// ---------------------------------------------------------------------------
// Boundary probing: values near type limits
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Near-boundary probing for u32: values within ±100 of u32::MAX.
    #[test]
    fn prop_u32_boundary_probe(delta in 0u64..=100) {
        let boundary = u64::from(u32::MAX);

        // At boundary - delta: OK
        let below = safe_u64_to_u32(boundary.saturating_sub(delta), "below");
        prop_assert!(below.is_ok());

        // At boundary + 1 + delta: Error
        let above = safe_u64_to_u32(boundary.saturating_add(1).saturating_add(delta), "above");
        prop_assert!(above.is_err());
    }

    /// Near-boundary probing for u16: values within ±100 of u16::MAX.
    #[test]
    fn prop_u16_boundary_probe(delta in 0u64..=100) {
        let boundary = u64::from(u16::MAX);

        let below = safe_u64_to_u16(boundary.saturating_sub(delta), "below");
        prop_assert!(below.is_ok());

        let above = safe_u64_to_u16(boundary.saturating_add(1).saturating_add(delta), "above");
        prop_assert!(above.is_err());
    }
}

// ---------------------------------------------------------------------------
// safe_array_bound: stress with extreme inputs
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// safe_array_bound with entirely random inputs never panics.
    #[test]
    fn prop_array_bound_never_panics_random(
        base in any::<usize>(),
        count in any::<usize>(),
        item_size in any::<usize>(),
    ) {
        // Must not panic regardless of inputs
        let _ = safe_array_bound(base, count, item_size, "random");
    }

    /// When base and product are both small, safe_array_bound always succeeds.
    #[test]
    fn prop_array_bound_small_inputs_succeed(
        base in 0usize..=10_000,
        count in 0usize..=1_000,
        item_size in 0usize..=1_000,
    ) {
        let result = safe_array_bound(base, count, item_size, "small");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), base + count * item_size);
    }

    /// Multiplication overflow is detected: large count × large item_size.
    #[test]
    fn prop_array_bound_detects_mul_overflow(
        count in (1usize << 40)..=usize::MAX,
        item_size in (1usize << 40)..=usize::MAX,
    ) {
        if count.checked_mul(item_size).is_none() {
            let result = safe_array_bound(0, count, item_size, "mul-overflow");
            prop_assert!(result.is_err());
        }
    }

    /// Addition overflow is detected: base near usize::MAX plus nonzero product.
    #[test]
    fn prop_array_bound_detects_add_overflow(
        product in 1usize..=1_000_000,
    ) {
        let base = usize::MAX - product + 1;
        let result = safe_array_bound(base, product, 1, "add-overflow");
        prop_assert!(result.is_err());
    }
}

// ---------------------------------------------------------------------------
// Chained narrowing: u64 → u32 → u16 consistency
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// If u64→u16 succeeds, then u64→u32 must also succeed.
    #[test]
    fn prop_u16_success_implies_u32_success(value in any::<u64>()) {
        let u16_result = safe_u64_to_u16(value, "chain");
        if u16_result.is_ok() {
            let u32_result = safe_u64_to_u32(value, "chain");
            prop_assert!(u32_result.is_ok(), "u16 ok but u32 failed for {}", value);
        }
    }

    /// If u64→u32 fails, then u64→u16 must also fail.
    #[test]
    fn prop_u32_failure_implies_u16_failure(value in any::<u64>()) {
        let u32_result = safe_u64_to_u32(value, "chain");
        if u32_result.is_err() {
            let u16_result = safe_u64_to_u16(value, "chain");
            prop_assert!(u16_result.is_err(), "u32 failed but u16 ok for {}", value);
        }
    }

    /// Successful narrowing preserves the value exactly.
    #[test]
    fn prop_narrowing_preserves_value(value in 0u64..=u64::from(u16::MAX)) {
        let u32_val = safe_u64_to_u32(value, "preserve").expect("u32 ok");
        let u16_val = safe_u64_to_u16(value, "preserve").expect("u16 ok");

        prop_assert_eq!(u64::from(u32_val), value);
        prop_assert_eq!(u64::from(u16_val), value);
    }
}
