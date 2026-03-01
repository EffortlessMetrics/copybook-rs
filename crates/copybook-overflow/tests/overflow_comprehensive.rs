// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive supplemental tests for `copybook-overflow` covering
//! COBOL-realistic scenarios, functional composition, idempotency,
//! and additional boundary values not exercised by existing tests.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_overflow::{safe_array_bound, safe_u64_to_u16, safe_u64_to_u32, safe_usize_to_u32};

// ============================================================
// 1. COBOL-realistic array bound calculations
// ============================================================

#[test]
fn cobol_80_byte_records_10000_occurrences() {
    // Typical: offset 20, 10000 OCCURS of 80-byte fields
    let result = safe_array_bound(20, 10_000, 80, "TRANSACTIONS").unwrap();
    assert_eq!(result, 800_020);
}

#[test]
fn cobol_single_byte_counter_field() {
    // PIC 9(3) COMP = 2 bytes, offset 0, 1 occurrence
    let result = safe_array_bound(0, 1, 2, "COUNTER").unwrap();
    assert_eq!(result, 2);
}

#[test]
fn cobol_nested_group_layout() {
    // Simulating: base=100, 5 groups of 200 bytes each
    let first = safe_array_bound(100, 5, 200, "GROUP-1").unwrap();
    assert_eq!(first, 1100);
    // Then 3 more groups of 50 bytes after that
    let second = safe_array_bound(first, 3, 50, "GROUP-2").unwrap();
    assert_eq!(second, 1250);
}

#[test]
fn cobol_maximum_mainframe_record_32760() {
    // IBM mainframe max LRECL for VB = 32760
    let result = safe_array_bound(4, 1, 32756, "VB-RECORD").unwrap();
    assert_eq!(result, 32760);
}

#[test]
fn cobol_odo_zero_occurrences() {
    // ODO field with 0 occurrences: base offset unchanged
    let result = safe_array_bound(500, 0, 80, "ODO-ZERO").unwrap();
    assert_eq!(result, 500);
}

// ============================================================
// 2. Functional composition: chained narrowing
// ============================================================

#[test]
fn chain_u64_to_u32_to_u16_via_separate_calls() {
    let original: u64 = 60_000;
    let as_u32 = safe_u64_to_u32(original, "step-1").unwrap();
    let as_u16 = safe_u64_to_u16(u64::from(as_u32), "step-2").unwrap();
    assert_eq!(as_u16, 60_000u16);
}

#[test]
fn chain_array_bound_to_u32_to_u16() {
    let bound = safe_array_bound(0, 100, 50, "array").unwrap(); // 5000
    let as_u32 = safe_usize_to_u32(bound, "narrow-u32").unwrap();
    assert_eq!(as_u32, 5000);
    let as_u16 = safe_u64_to_u16(u64::from(as_u32), "narrow-u16").unwrap();
    assert_eq!(as_u16, 5000);
}

#[test]
fn chain_fails_at_u16_narrowing() {
    let bound = safe_array_bound(0, 1000, 100, "big-array").unwrap(); // 100_000
    let as_u32 = safe_usize_to_u32(bound, "narrow-u32").unwrap();
    assert_eq!(as_u32, 100_000);
    // 100_000 > u16::MAX, so this should fail
    let err = safe_u64_to_u16(u64::from(as_u32), "narrow-u16").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

// ============================================================
// 3. Idempotency: same input always same output
// ============================================================

#[test]
fn u64_to_u32_idempotent() {
    for value in [0u64, 1, 255, 65535, u64::from(u32::MAX)] {
        let first = safe_u64_to_u32(value, "run-1").unwrap();
        let second = safe_u64_to_u32(value, "run-2").unwrap();
        assert_eq!(first, second, "not idempotent for {value}");
    }
}

#[test]
fn u64_to_u16_idempotent() {
    for value in [0u64, 1, 255, u64::from(u16::MAX)] {
        let first = safe_u64_to_u16(value, "run-1").unwrap();
        let second = safe_u64_to_u16(value, "run-2").unwrap();
        assert_eq!(first, second, "not idempotent for {value}");
    }
}

#[test]
fn array_bound_idempotent() {
    let first = safe_array_bound(10, 20, 30, "ctx").unwrap();
    let second = safe_array_bound(10, 20, 30, "ctx").unwrap();
    assert_eq!(first, second);
}

// ============================================================
// 4. Specific COBOL numeric boundaries
// ============================================================

#[test]
fn narrow_u64_at_signed_i16_max() {
    // 32767 = i16::MAX, common in COBOL COMP fields
    assert_eq!(safe_u64_to_u16(32767, "i16-max").unwrap(), 32767);
}

#[test]
fn narrow_u64_at_signed_i32_max() {
    // 2_147_483_647 = i32::MAX
    assert_eq!(
        safe_u64_to_u32(2_147_483_647, "i32-max").unwrap(),
        2_147_483_647
    );
}

#[test]
fn narrow_usize_at_typical_lrecl_values() {
    for lrecl in [80usize, 132, 256, 1024, 4096, 32760] {
        let result = safe_usize_to_u32(lrecl, "lrecl").unwrap();
        assert_eq!(result, lrecl as u32);
    }
}

// ============================================================
// 5. Error consistency: all functions use expected codes
// ============================================================

#[test]
fn all_narrowing_overflows_use_record_too_large() {
    let err1 = safe_u64_to_u32(u64::from(u32::MAX) + 1, "ctx").unwrap_err();
    let err2 = safe_u64_to_u16(u64::from(u16::MAX) + 1, "ctx").unwrap_err();
    assert_eq!(err1.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
    assert_eq!(err2.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[test]
fn all_array_bound_overflows_use_odo_not_tail() {
    let err1 = safe_array_bound(0, usize::MAX, 2, "mul").unwrap_err();
    let err2 = safe_array_bound(usize::MAX, 1, 1, "add").unwrap_err();
    assert_eq!(err1.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    assert_eq!(err2.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

// ============================================================
// 6. Context string edge cases
// ============================================================

#[test]
fn context_with_special_characters() {
    let err = safe_u64_to_u32(u64::from(u32::MAX) + 1, "FIELD-NAME.CHILD").unwrap_err();
    assert!(err.message.contains("FIELD-NAME.CHILD"));
}

#[test]
fn context_with_empty_string() {
    // Empty context should not cause panic
    let err = safe_u64_to_u16(u64::MAX, "").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[test]
fn context_with_long_field_path() {
    let long_ctx = "RECORD.GROUP-A.GROUP-B.GROUP-C.DETAIL-FIELD";
    let err = safe_u64_to_u32(u64::MAX, long_ctx).unwrap_err();
    assert!(err.message.contains(long_ctx));
}

// ============================================================
// 7. Powers of two narrowing
// ============================================================

#[test]
fn powers_of_two_fit_in_u32() {
    for power in 0..32u32 {
        let value = 1u64 << power;
        let result = safe_u64_to_u32(value, "pow2").unwrap();
        assert_eq!(result, 1u32 << power);
    }
}

#[test]
fn powers_of_two_fit_in_u16() {
    for power in 0..16u32 {
        let value = 1u64 << power;
        let result = safe_u64_to_u16(value, "pow2").unwrap();
        assert_eq!(result, 1u16 << power);
    }
}

#[test]
fn power_of_two_17_overflows_u16() {
    let value = 1u64 << 16; // 65536
    let err = safe_u64_to_u16(value, "pow2-17").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[test]
fn power_of_two_33_overflows_u32() {
    let value = 1u64 << 32; // 4_294_967_296
    let err = safe_u64_to_u32(value, "pow2-33").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}
