// SPDX-License-Identifier: AGPL-3.0-or-later
//! Edge-case tests for copybook-overflow: boundary values, zero, MAX, MAX-1,
//! MAX+1 equivalent, error messages, context propagation, and composition.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_overflow::{safe_array_bound, safe_u64_to_u16, safe_u64_to_u32, safe_usize_to_u32};

// ============================================================================
// 1. safe_u64_to_u32 — edge cases
// ============================================================================

#[test]
fn u64_to_u32_zero() {
    assert_eq!(safe_u64_to_u32(0, "zero").unwrap(), 0);
}

#[test]
fn u64_to_u32_one() {
    assert_eq!(safe_u64_to_u32(1, "one").unwrap(), 1);
}

#[test]
fn u64_to_u32_at_max() {
    assert_eq!(
        safe_u64_to_u32(u64::from(u32::MAX), "max").unwrap(),
        u32::MAX
    );
}

#[test]
fn u64_to_u32_at_max_minus_one() {
    assert_eq!(
        safe_u64_to_u32(u64::from(u32::MAX) - 1, "max-1").unwrap(),
        u32::MAX - 1
    );
}

#[test]
fn u64_to_u32_just_over_max() {
    let err = safe_u64_to_u32(u64::from(u32::MAX) + 1, "over").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[test]
fn u64_to_u32_at_u64_max() {
    let err = safe_u64_to_u32(u64::MAX, "u64max").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[test]
fn u64_to_u32_error_message_contains_context() {
    let err = safe_u64_to_u32(u64::from(u32::MAX) + 1, "MY-FIELD").unwrap_err();
    assert!(err.message.contains("MY-FIELD"), "message: {}", err.message);
    assert!(err.message.contains("u32::MAX"), "message: {}", err.message);
}

// ============================================================================
// 2. safe_u64_to_u16 — edge cases
// ============================================================================

#[test]
fn u64_to_u16_zero() {
    assert_eq!(safe_u64_to_u16(0, "zero").unwrap(), 0);
}

#[test]
fn u64_to_u16_one() {
    assert_eq!(safe_u64_to_u16(1, "one").unwrap(), 1);
}

#[test]
fn u64_to_u16_at_max() {
    assert_eq!(
        safe_u64_to_u16(u64::from(u16::MAX), "max").unwrap(),
        u16::MAX
    );
}

#[test]
fn u64_to_u16_at_max_minus_one() {
    assert_eq!(
        safe_u64_to_u16(u64::from(u16::MAX) - 1, "max-1").unwrap(),
        u16::MAX - 1
    );
}

#[test]
fn u64_to_u16_just_over_max() {
    let err = safe_u64_to_u16(u64::from(u16::MAX) + 1, "over").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[test]
fn u64_to_u16_at_u64_max() {
    let err = safe_u64_to_u16(u64::MAX, "u64max").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[test]
fn u64_to_u16_at_u32_max() {
    let err = safe_u64_to_u16(u64::from(u32::MAX), "u32max").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[test]
fn u64_to_u16_error_message_contains_context() {
    let err = safe_u64_to_u16(70_000, "RDW-LEN").unwrap_err();
    assert!(err.message.contains("RDW-LEN"), "message: {}", err.message);
    assert!(err.message.contains("u16::MAX"), "message: {}", err.message);
}

// ============================================================================
// 3. safe_usize_to_u32 — edge cases
// ============================================================================

#[test]
fn usize_to_u32_zero() {
    assert_eq!(safe_usize_to_u32(0, "zero").unwrap(), 0);
}

#[test]
fn usize_to_u32_one() {
    assert_eq!(safe_usize_to_u32(1, "one").unwrap(), 1);
}

#[test]
fn usize_to_u32_at_max() {
    assert_eq!(
        safe_usize_to_u32(u32::MAX as usize, "max").unwrap(),
        u32::MAX
    );
}

#[cfg(target_pointer_width = "64")]
#[test]
fn usize_to_u32_just_over_max_on_64bit() {
    let err = safe_usize_to_u32(u32::MAX as usize + 1, "over").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

#[cfg(target_pointer_width = "64")]
#[test]
fn usize_to_u32_at_usize_max_on_64bit() {
    let err = safe_usize_to_u32(usize::MAX, "usize-max").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS141_RECORD_TOO_LARGE);
}

// ============================================================================
// 4. safe_array_bound — boundary arithmetic
// ============================================================================

#[test]
fn array_bound_all_zeros() {
    assert_eq!(safe_array_bound(0, 0, 0, "zeros").unwrap(), 0);
}

#[test]
fn array_bound_zero_count_returns_base() {
    assert_eq!(safe_array_bound(42, 0, 100, "no-items").unwrap(), 42);
}

#[test]
fn array_bound_zero_item_size_returns_base() {
    assert_eq!(safe_array_bound(42, 999, 0, "zero-size").unwrap(), 42);
}

#[test]
fn array_bound_single_element() {
    assert_eq!(safe_array_bound(0, 1, 1, "single").unwrap(), 1);
}

#[test]
fn array_bound_typical_cobol_layout() {
    // base=10, 5 records of 80 bytes
    assert_eq!(safe_array_bound(10, 5, 80, "RECORDS").unwrap(), 410);
}

#[test]
fn array_bound_max_base_zero_product() {
    assert_eq!(
        safe_array_bound(usize::MAX, 0, 100, "max-base").unwrap(),
        usize::MAX
    );
}

#[test]
fn array_bound_exactly_at_usize_max() {
    // usize::MAX - 10 + (1 * 10) = usize::MAX
    assert_eq!(
        safe_array_bound(usize::MAX - 10, 1, 10, "exact").unwrap(),
        usize::MAX
    );
}

#[test]
fn array_bound_mul_overflow() {
    let err = safe_array_bound(0, usize::MAX, 2, "mul-overflow").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    assert!(err.message.contains("Array size overflow"));
}

#[test]
fn array_bound_add_overflow() {
    let err = safe_array_bound(usize::MAX, 1, 1, "add-overflow").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
    assert!(err.message.contains("Array offset overflow"));
}

#[test]
fn array_bound_error_message_contains_context() {
    let err = safe_array_bound(0, usize::MAX, 2, "ODO-FIELD").unwrap_err();
    assert!(
        err.message.contains("ODO-FIELD"),
        "message: {}",
        err.message
    );
}

// ============================================================================
// 5. Composition: chained array bound calculations
// ============================================================================

#[test]
fn chained_array_bound_accumulates() {
    let step1 = safe_array_bound(0, 10, 80, "step1").unwrap();
    assert_eq!(step1, 800);
    let step2 = safe_array_bound(step1, 5, 20, "step2").unwrap();
    assert_eq!(step2, 900);
}

// ============================================================================
// 6. Midrange values
// ============================================================================

#[test]
fn u64_to_u32_midrange() {
    assert_eq!(safe_u64_to_u32(1_000_000, "mid").unwrap(), 1_000_000);
}

#[test]
fn u64_to_u16_midrange() {
    assert_eq!(safe_u64_to_u16(30_000, "mid").unwrap(), 30_000);
}

#[test]
fn usize_to_u32_midrange() {
    assert_eq!(safe_usize_to_u32(50_000, "mid").unwrap(), 50_000);
}
