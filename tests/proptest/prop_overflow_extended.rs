// SPDX-License-Identifier: AGPL-3.0-or-later
//! Extended property tests for overflow-safe operations.
//!
//! Complements `prop_overflow.rs` and `prop_overflow_safety.rs` with
//! additional properties: commutativity, idempotency, `bounds_check` helpers,
//! and combined narrowing + array-bound chains.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_overflow::{safe_array_bound, safe_u64_to_u16, safe_u64_to_u32, safe_usize_to_u32};
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

// ---------------------------------------------------------------------------
// Idempotency: narrowing a value twice yields the same result
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Narrowing u64→u32 twice is idempotent: result stays the same.
    #[test]
    fn prop_u64_to_u32_idempotent(value in any::<u64>()) {
        let first = safe_u64_to_u32(value, "first");
        if let Ok(v32) = first {
            let second = safe_u64_to_u32(u64::from(v32), "second");
            prop_assert!(second.is_ok());
            prop_assert_eq!(second.unwrap(), v32);
        }
    }

    /// Narrowing u64→u16 twice is idempotent.
    #[test]
    fn prop_u64_to_u16_idempotent(value in any::<u64>()) {
        let first = safe_u64_to_u16(value, "first");
        if let Ok(v16) = first {
            let second = safe_u64_to_u16(u64::from(v16), "second");
            prop_assert!(second.is_ok());
            prop_assert_eq!(second.unwrap(), v16);
        }
    }
}

// ---------------------------------------------------------------------------
// Array bound: commutativity of count and item_size
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// safe_array_bound(base, a, b, _) == safe_array_bound(base, b, a, _)
    /// because multiplication is commutative.
    #[test]
    fn prop_array_bound_commutative(
        base in 0usize..=10_000,
        count in 0usize..=1_000,
        item_size in 0usize..=1_000,
    ) {
        let r1 = safe_array_bound(base, count, item_size, "ab");
        let r2 = safe_array_bound(base, item_size, count, "ba");
        prop_assert_eq!(r1.is_ok(), r2.is_ok());
        if let (Ok(v1), Ok(v2)) = (r1, r2) {
            prop_assert_eq!(v1, v2);
        }
    }

    /// safe_array_bound with count=1 equals base + item_size.
    #[test]
    fn prop_array_bound_count_one(
        base in 0usize..=1_000_000,
        item_size in 0usize..=1_000_000,
    ) {
        let result = safe_array_bound(base, 1, item_size, "one");
        if let Some(expected) = base.checked_add(item_size) {
            prop_assert!(result.is_ok());
            prop_assert_eq!(result.unwrap(), expected);
        } else {
            prop_assert!(result.is_err());
        }
    }

    /// safe_array_bound with item_size=1 equals base + count.
    #[test]
    fn prop_array_bound_item_one(
        base in 0usize..=1_000_000,
        count in 0usize..=1_000_000,
    ) {
        let result = safe_array_bound(base, count, 1, "one");
        if let Some(expected) = base.checked_add(count) {
            prop_assert!(result.is_ok());
            prop_assert_eq!(result.unwrap(), expected);
        } else {
            prop_assert!(result.is_err());
        }
    }
}

// ---------------------------------------------------------------------------
// Combined narrowing + array bound chains
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Narrow a u64 to u32, use it as base for array_bound—never panics.
    #[test]
    fn prop_narrow_then_array_bound_no_panic(
        value in any::<u64>(),
        count in 0usize..=100,
        item_size in 0usize..=100,
    ) {
        if let Ok(base) = safe_u64_to_u32(value, "base") {
            let _ = safe_array_bound(base as usize, count, item_size, "chain");
        }
    }

    /// Narrow a u64 to u16, widen back, then narrow to u32—must succeed.
    #[test]
    fn prop_u16_widened_to_u32_always_succeeds(value in any::<u64>()) {
        if let Ok(v16) = safe_u64_to_u16(value, "step1") {
            let widened = u64::from(v16);
            let result = safe_u64_to_u32(widened, "step2");
            prop_assert!(result.is_ok(), "u16 value must fit in u32");
        }
    }

    /// safe_usize_to_u32 and safe_u64_to_u32 agree on overlapping inputs.
    #[test]
    fn prop_usize_u64_narrow_agree(value in 0u64..=u64::from(u32::MAX) + 1000) {
        let as_usize = value as usize;
        let r_usize = safe_usize_to_u32(as_usize, "usize");
        let r_u64 = safe_u64_to_u32(value, "u64");
        // On 64-bit platforms the two should agree
        #[cfg(target_pointer_width = "64")]
        {
            prop_assert_eq!(r_usize.is_ok(), r_u64.is_ok());
            if let (Ok(a), Ok(b)) = (r_usize, r_u64) {
                prop_assert_eq!(a, b);
            }
        }
    }
}
