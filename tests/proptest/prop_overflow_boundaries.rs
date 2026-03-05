// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for overflow-safe arithmetic near signed/unsigned boundaries.
//!
//! Complements `prop_overflow_safety.rs`, `prop_overflow.rs`, and
//! `prop_overflow_extended.rs` with coverage for:
//! - Signed integer boundaries (i8/i16/i32/i64) as they relate to COBOL fields
//! - Powers-of-10 near type limits (PIC 9(n) max values)
//! - Narrowing of COBOL-realistic field widths and offsets
//! - Error code correctness on overflow

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_overflow::{safe_array_bound, safe_u64_to_u16, safe_u64_to_u32, safe_usize_to_u32};
use proptest::prelude::*;

use super::config::*;

// ---------------------------------------------------------------------------
// 1. Powers of 10 near boundaries (COBOL PIC 9(n) max values)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// COBOL PIC 9(n) max values (10^n - 1) that fit in u32 succeed.
    #[test]
    fn prop_powers_of_10_within_u32(exponent in 1u32..=9) {
        let value: u64 = 10u64.pow(exponent) - 1;
        let result = safe_u64_to_u32(value, "pic9");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), value as u32);
    }

    /// COBOL PIC 9(n) max values that exceed u32 fail narrowing.
    #[test]
    fn prop_powers_of_10_exceeding_u32(exponent in 10u32..=18) {
        let value: u64 = 10u64.pow(exponent) - 1;
        if value > u64::from(u32::MAX) {
            let result = safe_u64_to_u32(value, "pic9-large");
            prop_assert!(result.is_err());
        }
    }

    /// PIC 9(n) max values that fit in u16 succeed.
    #[test]
    fn prop_powers_of_10_within_u16(exponent in 1u32..=4) {
        let value: u64 = 10u64.pow(exponent) - 1;
        let result = safe_u64_to_u16(value, "pic9-u16");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), value as u16);
    }

    /// PIC 9(5) = 99999 exceeds u16::MAX (65535).
    #[test]
    fn prop_pic9_5_exceeds_u16(_dummy in Just(())) {
        let value: u64 = 99_999;
        let result = safe_u64_to_u16(value, "pic9-5");
        prop_assert!(result.is_err());
    }
}

// ---------------------------------------------------------------------------
// 2. Signed integer boundaries as COBOL field offsets/lengths
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// i32 positive values converted via u64 always succeed u64→u32.
    #[test]
    fn prop_i32_positive_fits_u32(value in 0i32..=i32::MAX) {
        let as_u64 = value as u64;
        let result = safe_u64_to_u32(as_u64, "i32-pos");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), value as u32);
    }

    /// i16 positive values converted via u64 always succeed u64→u16.
    #[test]
    fn prop_i16_positive_fits_u16(value in 0i16..=i16::MAX) {
        let as_u64 = value as u64;
        let result = safe_u64_to_u16(as_u64, "i16-pos");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), value as u16);
    }

    /// Negative i32 reinterpreted as u64 (wrapping) always fails narrowing.
    #[test]
    fn prop_negative_i32_as_u64_fails(value in i32::MIN..=-1) {
        let as_u64 = value as u64; // wraps to large u64
        let result = safe_u64_to_u32(as_u64, "neg-i32");
        prop_assert!(result.is_err());
    }

    /// Negative i16 reinterpreted as u64 always fails u64→u16.
    #[test]
    fn prop_negative_i16_as_u64_fails(value in i16::MIN..=-1) {
        let as_u64 = value as u64;
        let result = safe_u64_to_u16(as_u64, "neg-i16");
        prop_assert!(result.is_err());
    }
}

// ---------------------------------------------------------------------------
// 3. COBOL-realistic record size calculations
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Typical COBOL record: offset + (occurs × field_size) stays within bounds.
    #[test]
    fn prop_cobol_record_realistic(
        offset in 0usize..=10_000,
        occurs in 0usize..=999,
        field_size in 1usize..=100,
    ) {
        let result = safe_array_bound(offset, occurs, field_size, "cobol-rec");
        // All realistic COBOL values should succeed
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), offset + occurs * field_size);
    }

    /// Maximum COBOL LRECL (32760) fits in u16.
    #[test]
    fn prop_max_cobol_lrecl_fits_u16(lrecl in 1u64..=32760) {
        let result = safe_u64_to_u16(lrecl, "lrecl");
        prop_assert!(result.is_ok());
    }

    /// COBOL record length up to 999,999,999 fits in u32.
    #[test]
    fn prop_large_record_length_fits_u32(len in 0u64..=999_999_999) {
        let result = safe_u64_to_u32(len, "record-len");
        prop_assert!(result.is_ok());
    }
}

// ---------------------------------------------------------------------------
// 4. Narrowing correctness: safe_u64_to_u32 preserves value exactly
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Widening the narrowed value back gives the original.
    #[test]
    fn prop_narrow_u32_widen_roundtrip(value in 0u64..=u64::from(u32::MAX)) {
        let narrowed = safe_u64_to_u32(value, "rt").unwrap();
        prop_assert_eq!(u64::from(narrowed), value);
    }

    /// Widening the narrowed u16 back gives the original.
    #[test]
    fn prop_narrow_u16_widen_roundtrip(value in 0u64..=u64::from(u16::MAX)) {
        let narrowed = safe_u64_to_u16(value, "rt").unwrap();
        prop_assert_eq!(u64::from(narrowed), value);
    }
}

// ---------------------------------------------------------------------------
// 5. Boundary probing: exact values at type limits
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Values at i8 positive max (127) → u64 → u32 always succeed.
    #[test]
    fn prop_i8_positive_max_fits_u32(_dummy in Just(())) {
        let result = safe_u64_to_u32(i8::MAX as u64, "i8-max");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), 127);
    }

    /// Values at i16 positive max (32767) → u64 → u16 always succeed.
    #[test]
    fn prop_i16_positive_max_fits_u16(_dummy in Just(())) {
        let result = safe_u64_to_u16(i16::MAX as u64, "i16-max");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), 32767);
    }

    /// Exact i32::MAX as usize fits in u32.
    #[test]
    fn prop_i32_max_as_usize_fits_u32(_dummy in Just(())) {
        let result = safe_usize_to_u32(i32::MAX as usize, "i32-max");
        prop_assert!(result.is_ok());
        prop_assert_eq!(result.unwrap(), i32::MAX as u32);
    }
}

// ---------------------------------------------------------------------------
// 6. Error code verification: overflow returns correct error codes
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Overflow errors from safe_u64_to_u32 carry CBKS141 error code.
    #[test]
    fn prop_u64_to_u32_error_code(value in (u64::from(u32::MAX) + 1)..=u64::MAX) {
        let err = safe_u64_to_u32(value, "code-check").unwrap_err();
        prop_assert_eq!(
            err.code,
            copybook_error::ErrorCode::CBKS141_RECORD_TOO_LARGE
        );
    }

    /// Overflow errors from safe_u64_to_u16 carry CBKS141 error code.
    #[test]
    fn prop_u64_to_u16_error_code(value in (u64::from(u16::MAX) + 1)..=u64::MAX) {
        let err = safe_u64_to_u16(value, "code-check").unwrap_err();
        prop_assert_eq!(
            err.code,
            copybook_error::ErrorCode::CBKS141_RECORD_TOO_LARGE
        );
    }

    /// Array bound overflow errors carry CBKP021 error code.
    #[test]
    fn prop_array_bound_overflow_error_code(
        count in (usize::MAX / 2)..=usize::MAX,
        item_size in 2usize..=100,
    ) {
        if count.checked_mul(item_size).is_none() {
            let err = safe_array_bound(0, count, item_size, "code-check").unwrap_err();
            prop_assert_eq!(
                err.code,
                copybook_error::ErrorCode::CBKP021_ODO_NOT_TAIL
            );
        }
    }
}

// ---------------------------------------------------------------------------
// 7. Monotonicity: if value fits, value-1 also fits
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// If safe_u64_to_u32(v) succeeds, then safe_u64_to_u32(v-1) also succeeds.
    #[test]
    fn prop_u32_monotonic(value in 1u64..=u64::from(u32::MAX)) {
        let r1 = safe_u64_to_u32(value, "mono");
        let r2 = safe_u64_to_u32(value - 1, "mono-1");
        if r1.is_ok() {
            prop_assert!(r2.is_ok());
        }
    }

    /// If safe_u64_to_u16(v) succeeds, then safe_u64_to_u16(v-1) also succeeds.
    #[test]
    fn prop_u16_monotonic(value in 1u64..=u64::from(u16::MAX)) {
        let r1 = safe_u64_to_u16(value, "mono");
        let r2 = safe_u64_to_u16(value - 1, "mono-1");
        if r1.is_ok() {
            prop_assert!(r2.is_ok());
        }
    }
}
