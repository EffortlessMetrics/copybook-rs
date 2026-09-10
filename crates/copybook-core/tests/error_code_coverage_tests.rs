// SPDX-License-Identifier: AGPL-3.0-or-later
//! Error code coverage tests for error codes lacking dedicated test assertions.
//!
//! This module fills gaps identified by the error code audit:
//! - CBKS610_RENAME_MULTIPLE_REDEFINES: Multiple REDEFINES in RENAMES span (R4 flag)
//! - CBKS611_RENAME_PARTIAL_OCCURS: Partial array span in RENAMES (R5 flag)
//!
//! CBKA001_BASELINE_ERROR lives in `copybook-audit`
//! (`crates/copybook-audit/tests/error_codes.rs`) since #656 Phase E moved
//! audit out of the stable core.
//!
//! CBKD302 is vestigial (E2/E3 are now complete). CBKC201 is an I/O error
//! that requires failing writes to trigger.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::error::ErrorCode;

// =============================================================================
// CBKS610_RENAME_MULTIPLE_REDEFINES: Multiple REDEFINES in RENAMES span
// =============================================================================

/// Test CBKS610: RENAMES alias spans multiple REDEFINES alternatives with
/// the RenamesR4R6 feature flag enabled.
///
/// Explicit per-operation flags (#656 Phase D: no process-global state).
#[test]
fn test_cbks610_multiple_redefines_with_r4r6_flag() {
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::RenamesR4R6);

    let copybook = r#"
       01  TRANSACTION-RECORD.
           05  TRANS-TYPE      PIC X(1).
           05  TRANS-DATA      PIC X(20).
           05  CHECK-DATA      REDEFINES TRANS-DATA.
               10  CHECK-NUM  PIC 9(8).
               10  CHECK-AMT  PIC 9(10)V99.
           05  CARD-DATA       REDEFINES TRANS-DATA.
               10  CARD-NUM   PIC 9(16).
               10  CARD-EXP   PIC 9(4).
           66  PAYMENT-INFO RENAMES CHECK-DATA THRU CARD-DATA.
    "#;

    let result = copybook_core::parse_copybook_with_feature_flags(
        copybook,
        &copybook_core::ParseOptions::default(),
        &flags,
    );

    assert!(
        result.is_err(),
        "Expected CBKS610 for multiple REDEFINES in RENAMES span"
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKS610_RENAME_MULTIPLE_REDEFINES,
        "Expected CBKS610_RENAME_MULTIPLE_REDEFINES, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("PAYMENT-INFO"),
        "Error should mention the alias name"
    );
}

/// Test CBKS611: RENAMES alias spans partial array elements with
/// the RenamesR4R6 feature flag enabled.
#[test]
fn test_cbks611_partial_occurs_with_r4r6_flag() {
    // Explicit per-operation flags (#656 Phase D: no process-global state).
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::RenamesR4R6);

    let copybook = r#"
       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  LINE-ITEMS      OCCURS 10 TIMES.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           05  TOTAL-AMT       PIC 9(10).
           66  ORDER-DATA RENAMES LINE-ITEMS THRU TOTAL-AMT.
    "#;

    let result = copybook_core::parse_copybook_with_feature_flags(
        copybook,
        &copybook_core::ParseOptions::default(),
        &flags,
    );

    assert!(
        result.is_err(),
        "Expected CBKS611 for partial OCCURS span in RENAMES"
    );
    let err = result.unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKS611_RENAME_PARTIAL_OCCURS,
        "Expected CBKS611_RENAME_PARTIAL_OCCURS, got {:?}",
        err.code
    );
    assert!(
        err.message.contains("ORDER-DATA"),
        "Error should mention the alias name"
    );
}
