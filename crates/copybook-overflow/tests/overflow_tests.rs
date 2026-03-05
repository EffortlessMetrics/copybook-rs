// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for `copybook-overflow`.
//!
//! Covers every public function with normal, boundary, and error cases.

use copybook_error::ErrorCode;
use copybook_overflow::{safe_array_bound, safe_u64_to_u16, safe_u64_to_u32, safe_usize_to_u32};

// ============================================================
// safe_u64_to_u32 – narrowing u64 → u32
// ============================================================

mod safe_u64_to_u32_tests {
    use super::*;

    // --- Normal values ---

    #[test]
    fn zero() {
        assert_eq!(safe_u64_to_u32(0, "ctx").unwrap(), 0);
    }

    #[test]
    fn one() {
        assert_eq!(safe_u64_to_u32(1, "ctx").unwrap(), 1);
    }

    #[test]
    fn typical_value() {
        assert_eq!(safe_u64_to_u32(123_456_789, "ctx").unwrap(), 123_456_789);
    }

    // --- Boundary values ---

    #[test]
    fn at_u32_max() {
        assert_eq!(
            safe_u64_to_u32(u64::from(u32::MAX), "ctx").unwrap(),
            u32::MAX
        );
    }

    #[test]
    fn at_u32_max_minus_one() {
        assert_eq!(
            safe_u64_to_u32(u64::from(u32::MAX) - 1, "ctx").unwrap(),
            u32::MAX - 1
        );
    }

    #[test]
    fn at_u16_max() {
        assert_eq!(
            safe_u64_to_u32(u64::from(u16::MAX), "ctx").unwrap(),
            u32::from(u16::MAX)
        );
    }

    // --- Error: just above u32::MAX ---

    #[test]
    fn just_above_u32_max() {
        let err = safe_u64_to_u32(u64::from(u32::MAX) + 1, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[test]
    fn u64_max_overflows() {
        let err = safe_u64_to_u32(u64::MAX, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[test]
    fn u64_midpoint_overflows() {
        // u64 midpoint is well above u32::MAX
        let err = safe_u64_to_u32(u64::MAX / 2, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    // --- Error message quality ---

    #[test]
    fn error_message_contains_context() {
        let err = safe_u64_to_u32(u64::from(u32::MAX) + 1, "record-length").unwrap_err();
        assert!(err.message.contains("record-length"));
    }

    #[test]
    fn error_message_contains_value() {
        let value = u64::from(u32::MAX) + 42;
        let err = safe_u64_to_u32(value, "ctx").unwrap_err();
        assert!(err.message.contains(&value.to_string()));
    }

    #[test]
    fn error_message_mentions_u32_max() {
        let err = safe_u64_to_u32(u64::from(u32::MAX) + 1, "ctx").unwrap_err();
        assert!(err.message.contains("exceeds u32::MAX"));
    }
}

// ============================================================
// safe_u64_to_u16 – narrowing u64 → u16
// ============================================================

mod safe_u64_to_u16_tests {
    use super::*;

    // --- Normal values ---

    #[test]
    fn zero() {
        assert_eq!(safe_u64_to_u16(0, "ctx").unwrap(), 0);
    }

    #[test]
    fn one() {
        assert_eq!(safe_u64_to_u16(1, "ctx").unwrap(), 1);
    }

    #[test]
    fn typical_value() {
        assert_eq!(safe_u64_to_u16(30_000, "ctx").unwrap(), 30_000);
    }

    // --- Boundary values ---

    #[test]
    fn at_u16_max() {
        assert_eq!(
            safe_u64_to_u16(u64::from(u16::MAX), "ctx").unwrap(),
            u16::MAX
        );
    }

    #[test]
    fn at_u16_max_minus_one() {
        assert_eq!(
            safe_u64_to_u16(u64::from(u16::MAX) - 1, "ctx").unwrap(),
            u16::MAX - 1
        );
    }

    #[test]
    fn at_u8_max() {
        assert_eq!(
            safe_u64_to_u16(u64::from(u8::MAX), "ctx").unwrap(),
            u16::from(u8::MAX)
        );
    }

    // --- Error: just above u16::MAX ---

    #[test]
    fn just_above_u16_max() {
        let err = safe_u64_to_u16(u64::from(u16::MAX) + 1, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[test]
    fn u32_max_overflows() {
        let err = safe_u64_to_u16(u64::from(u32::MAX), "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[test]
    fn u64_max_overflows() {
        let err = safe_u64_to_u16(u64::MAX, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    // --- Error message quality ---

    #[test]
    fn error_message_contains_context() {
        let err = safe_u64_to_u16(70_000, "rdw-length").unwrap_err();
        assert!(err.message.contains("rdw-length"));
    }

    #[test]
    fn error_message_contains_value() {
        let err = safe_u64_to_u16(70_000, "ctx").unwrap_err();
        assert!(err.message.contains("70000"));
    }

    #[test]
    fn error_message_mentions_u16_max() {
        let err = safe_u64_to_u16(70_000, "ctx").unwrap_err();
        assert!(err.message.contains("exceeds u16::MAX"));
    }
}

// ============================================================
// safe_usize_to_u32 – narrowing usize → u32
// ============================================================

mod safe_usize_to_u32_tests {
    use super::*;

    // --- Normal values ---

    #[test]
    fn zero() {
        assert_eq!(safe_usize_to_u32(0, "ctx").unwrap(), 0);
    }

    #[test]
    fn one() {
        assert_eq!(safe_usize_to_u32(1, "ctx").unwrap(), 1);
    }

    #[test]
    fn typical_value() {
        assert_eq!(safe_usize_to_u32(50_000, "ctx").unwrap(), 50_000);
    }

    // --- Boundary values ---

    #[test]
    fn at_u32_max() {
        assert_eq!(
            safe_usize_to_u32(u32::MAX as usize, "ctx").unwrap(),
            u32::MAX
        );
    }

    #[test]
    fn at_u32_max_minus_one() {
        assert_eq!(
            safe_usize_to_u32(u32::MAX as usize - 1, "ctx").unwrap(),
            u32::MAX - 1
        );
    }

    #[test]
    fn at_u16_max() {
        assert_eq!(
            safe_usize_to_u32(u16::MAX as usize, "ctx").unwrap(),
            u32::from(u16::MAX)
        );
    }

    // --- Error: just above u32::MAX (64-bit only) ---

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn just_above_u32_max() {
        let err = safe_usize_to_u32(u32::MAX as usize + 1, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn usize_max_overflows() {
        let err = safe_usize_to_u32(usize::MAX, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn error_message_contains_context() {
        let err = safe_usize_to_u32(u32::MAX as usize + 1, "buf-offset").unwrap_err();
        assert!(err.message.contains("buf-offset"));
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn error_message_mentions_u32_max() {
        let err = safe_usize_to_u32(u32::MAX as usize + 1, "ctx").unwrap_err();
        assert!(err.message.contains("exceeds u32::MAX"));
    }
}

// ============================================================
// safe_array_bound – bounds arithmetic (base + count * item_size)
// ============================================================

mod safe_array_bound_tests {
    use super::*;

    // --- Normal values ---

    #[test]
    fn simple_computation() {
        // 10 + 3 * 4 = 22
        assert_eq!(safe_array_bound(10, 3, 4, "ctx").unwrap(), 22);
    }

    #[test]
    fn zero_base() {
        // 0 + 5 * 8 = 40
        assert_eq!(safe_array_bound(0, 5, 8, "ctx").unwrap(), 40);
    }

    #[test]
    fn single_element() {
        // 100 + 1 * 20 = 120
        assert_eq!(safe_array_bound(100, 1, 20, "ctx").unwrap(), 120);
    }

    #[test]
    fn large_non_overflowing() {
        // 1_000 + 10_000 * 100 = 1_001_000
        assert_eq!(
            safe_array_bound(1_000, 10_000, 100, "ctx").unwrap(),
            1_001_000
        );
    }

    // --- Zero-product identity ---

    #[test]
    fn zero_count_returns_base() {
        assert_eq!(safe_array_bound(42, 0, 999, "ctx").unwrap(), 42);
    }

    #[test]
    fn zero_item_size_returns_base() {
        assert_eq!(safe_array_bound(42, 999, 0, "ctx").unwrap(), 42);
    }

    #[test]
    fn all_zeros() {
        assert_eq!(safe_array_bound(0, 0, 0, "ctx").unwrap(), 0);
    }

    #[test]
    fn zero_count_with_max_base() {
        assert_eq!(
            safe_array_bound(usize::MAX, 0, 100, "ctx").unwrap(),
            usize::MAX
        );
    }

    #[test]
    fn zero_item_size_with_max_base() {
        assert_eq!(
            safe_array_bound(usize::MAX, usize::MAX, 0, "ctx").unwrap(),
            usize::MAX
        );
    }

    // --- Boundary: exact fit ---

    #[test]
    fn exact_usize_max() {
        // usize::MAX - 10 + 1 * 10 = usize::MAX
        assert_eq!(
            safe_array_bound(usize::MAX - 10, 1, 10, "ctx").unwrap(),
            usize::MAX
        );
    }

    #[test]
    fn exact_usize_max_split() {
        // usize::MAX - 100 + 10 * 10 = usize::MAX
        assert_eq!(
            safe_array_bound(usize::MAX - 100, 10, 10, "ctx").unwrap(),
            usize::MAX
        );
    }

    // --- Multiplication overflow ---

    #[test]
    fn mul_overflow_both_large() {
        let err = safe_array_bound(0, usize::MAX, 2, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn mul_overflow_max_times_max() {
        let err = safe_array_bound(0, usize::MAX, usize::MAX, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn mul_overflow_half_max_times_three() {
        let err = safe_array_bound(0, usize::MAX / 2 + 1, 3, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn mul_overflow_error_message() {
        let err = safe_array_bound(0, usize::MAX, 2, "my-field").unwrap_err();
        assert!(err.message.contains("Array size overflow"));
        assert!(err.message.contains("my-field"));
        assert!(err.message.contains("would overflow"));
    }

    // --- Addition overflow ---

    #[test]
    fn add_overflow_base_plus_product() {
        // product = 1 * 2 = 2; base = usize::MAX - 1; sum overflows
        let err = safe_array_bound(usize::MAX - 1, 1, 2, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn add_overflow_max_base_plus_one() {
        let err = safe_array_bound(usize::MAX, 1, 1, "ctx").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn add_overflow_max_base_plus_large_product() {
        let err = safe_array_bound(usize::MAX, 100, 100, "ctx").unwrap_err();
        // This will fail at multiplication or addition
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }

    #[test]
    fn add_overflow_error_message() {
        let err = safe_array_bound(usize::MAX, 1, 1, "odo-calc").unwrap_err();
        assert!(err.message.contains("Array offset overflow"));
        assert!(err.message.contains("odo-calc"));
        assert!(err.message.contains("would overflow"));
    }

    // --- Context propagation ---

    #[test]
    fn context_in_mul_error() {
        let err = safe_array_bound(0, usize::MAX, 2, "TRANSACTIONS").unwrap_err();
        assert!(err.message.contains("TRANSACTIONS"));
    }

    #[test]
    fn context_in_add_error() {
        let err = safe_array_bound(usize::MAX, 1, 1, "ODO-FIELD").unwrap_err();
        assert!(err.message.contains("ODO-FIELD"));
    }

    #[test]
    fn empty_context_does_not_panic() {
        let err = safe_array_bound(usize::MAX, 1, 1, "").unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    }
}

// ============================================================
// Cross-function chaining scenarios
// ============================================================

mod chained_operations {
    use super::*;

    #[test]
    fn array_bound_then_narrow_to_u32() {
        let bound = safe_array_bound(0, 10, 80, "record-size").unwrap();
        let narrow = safe_usize_to_u32(bound, "u32-convert").unwrap();
        assert_eq!(narrow, 800);
    }

    #[test]
    fn array_bound_too_large_for_u32() {
        // On 64-bit, build a bound > u32::MAX
        #[cfg(target_pointer_width = "64")]
        {
            let bound = safe_array_bound(0, u32::MAX as usize + 1, 1, "big-record").unwrap();
            let err = safe_usize_to_u32(bound, "convert").unwrap_err();
            assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
        }
    }

    #[test]
    fn u64_narrow_to_u32_then_widen_back() {
        let original: u64 = 1_000_000;
        let narrow = safe_u64_to_u32(original, "ctx").unwrap();
        assert_eq!(u64::from(narrow), original);
    }

    #[test]
    fn u64_narrow_to_u16_round_trip() {
        let original: u64 = 60_000;
        let narrow = safe_u64_to_u16(original, "ctx").unwrap();
        assert_eq!(u64::from(narrow), original);
    }

    #[test]
    fn multiple_array_bounds_accumulate() {
        let first = safe_array_bound(0, 5, 10, "group-1").unwrap();
        let second = safe_array_bound(first, 3, 20, "group-2").unwrap();
        // 0 + 5*10 = 50; 50 + 3*20 = 110
        assert_eq!(second, 110);
    }
}

// ============================================================
// No-panic guarantee: verify errors instead of panics
// ============================================================

mod no_panic_guarantee {
    use super::*;

    #[test]
    fn u64_to_u32_does_not_panic_on_overflow() {
        let result = safe_u64_to_u32(u64::MAX, "panic-test");
        assert!(result.is_err());
    }

    #[test]
    fn u64_to_u16_does_not_panic_on_overflow() {
        let result = safe_u64_to_u16(u64::MAX, "panic-test");
        assert!(result.is_err());
    }

    #[test]
    fn usize_to_u32_does_not_panic_on_overflow() {
        let result = safe_usize_to_u32(usize::MAX, "panic-test");
        // On 32-bit this succeeds (usize == u32); on 64-bit it's Err.
        #[cfg(target_pointer_width = "64")]
        assert!(result.is_err());
        #[cfg(target_pointer_width = "32")]
        assert!(result.is_ok());
    }

    #[test]
    fn array_bound_does_not_panic_on_mul_overflow() {
        let result = safe_array_bound(0, usize::MAX, usize::MAX, "panic-test");
        assert!(result.is_err());
    }

    #[test]
    fn array_bound_does_not_panic_on_add_overflow() {
        let result = safe_array_bound(usize::MAX, 1, 1, "panic-test");
        assert!(result.is_err());
    }
}
