//! Negative tests for Nested ODO (O5) and ODO + REDEFINES (O6) scenarios
//!
//! These tests validate that copybook-rs explicitly rejects unsupported
//! ODO patterns with clear error codes as documented in Issue #164 Phase N1.
//!
//! **Design Contract**: `docs/design/NESTED_ODO_BEHAVIOR.md`
//! **Support Matrix**: `docs/reference/COBOL_SUPPORT_MATRIX.md` (Nested ODO section)
//!
//! ## Test Coverage
//!
//! - **O5 (Nested ODO)**: OCCURS DEPENDING ON inside another OCCURS/ODO → `CBKP022_NESTED_ODO`
//! - **O6 (ODO + REDEFINES)**: OCCURS over REDEFINES group → `CBKP023_ODO_REDEFINES`
//!
//! ## Rationale
//!
//! These scenarios are **rejected by design** in Phase N1:
//! - O5: Schema nesting complexity, counter scoping ambiguity, memory layout challenges
//! - O6: Semantic conflict between fixed overlay (REDEFINES) and variable length (ODO)
//!
//! Future phases may reconsider these based on user demand with concrete use cases.

#![allow(clippy::expect_used)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::needless_raw_string_hashes)]

use copybook_core::{ErrorCode, parse_copybook};

// =============================================================================
// O5: Nested ODO (ODO inside ODO) - CBKP022_NESTED_ODO
// =============================================================================

#[test]
fn test_o5_nested_odo_basic_rejection() {
    // Basic nested ODO: ODO inside dynamic OCCURS (simplified from working pattern)
    let copybook = r#"
       01 OUTER-REC.
          05 OUTER-COUNT PIC 9(2).
          05 OUTER-GROUP OCCURS 1 TO 50 TIMES DEPENDING ON OUTER-COUNT.
             10 INNER-COUNT PIC 9(2).
             10 INNER-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON INNER-COUNT.
                15 DATA-VALUE PIC X(10).
    "#;

    let err = parse_copybook(copybook)
        .expect_err("Nested ODO (O5) must be rejected with CBKP022_NESTED_ODO");

    assert_eq!(
        err.code,
        ErrorCode::CBKP022_NESTED_ODO,
        "Expected CBKP022_NESTED_ODO for nested ODO scenario, got: {:?}",
        err
    );

    // Verify error message mentions nesting
    assert!(
        err.message.to_lowercase().contains("nested")
            || err.message.to_lowercase().contains("inside"),
        "Error message should mention nesting context: {}",
        err.message
    );
}

#[test]
fn test_o5_nested_odo_double_dynamic() {
    // More complex: ODO inside another ODO
    let copybook = r#"
       01 TRANSACTION-REC.
          05 OUTER-COUNT PIC 9(3).
          05 OUTER-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON OUTER-COUNT.
             10 INNER-COUNT PIC 9(2).
             10 INNER-ARRAY OCCURS 1 TO 50 TIMES DEPENDING ON INNER-COUNT.
                15 TRANS-DATA PIC X(20).
    "#;

    let err = parse_copybook(copybook).expect_err("Double dynamic ODO (O5) must be rejected");

    assert_eq!(
        err.code,
        ErrorCode::CBKP022_NESTED_ODO,
        "Expected CBKP022_NESTED_ODO for double dynamic ODO, got: {:?}",
        err
    );
}

#[test]
fn test_o5_nested_odo_deep_nesting() {
    // Deep nesting with groups
    let copybook = r#"
       01 ENTERPRISE-REC.
          05 DEPT-COUNT PIC 9(2).
          05 DEPARTMENTS OCCURS 1 TO 200 TIMES DEPENDING ON DEPT-COUNT.
             10 DEPT-NAME PIC X(30).
             10 EMP-COUNT PIC 9(3).
             10 EMPLOYEES OCCURS 1 TO 100 TIMES DEPENDING ON EMP-COUNT.
                15 EMP-ID PIC 9(6).
                15 EMP-NAME PIC X(40).
    "#;

    let err = parse_copybook(copybook).expect_err("Deep nested ODO (O5) must be rejected");

    assert_eq!(
        err.code,
        ErrorCode::CBKP022_NESTED_ODO,
        "Expected CBKP022_NESTED_ODO for deep nested ODO, got: {:?}",
        err
    );
}

// =============================================================================
// O6: ODO over REDEFINES - CBKP023_ODO_REDEFINES
// =============================================================================

#[test]
fn test_o6_odo_over_redefines_basic() {
    // Basic ODO inside REDEFINES group (child has ODO, parent has REDEFINES)
    let copybook = r#"
       01 TRANSACTION-REC.
          05 TRANS-TYPE PIC X(1).
          05 TRANS-COUNT PIC 9(2).
          05 TRANS-DATA PIC X(100).
          05 TRANS-DETAIL REDEFINES TRANS-DATA.
             10 DETAIL-ITEM OCCURS 1 TO 100 TIMES DEPENDING ON TRANS-COUNT.
                15 DETAIL-FIELD PIC X(10).
    "#;

    let err = parse_copybook(copybook)
        .expect_err("ODO inside REDEFINES (O6) must be rejected with CBKP023_ODO_REDEFINES");

    assert_eq!(
        err.code,
        ErrorCode::CBKP023_ODO_REDEFINES,
        "Expected CBKP023_ODO_REDEFINES for ODO inside REDEFINES, got: {:?}",
        err
    );

    // Verify error message mentions REDEFINES
    assert!(
        err.message.to_uppercase().contains("REDEFINES"),
        "Error message should mention REDEFINES: {}",
        err.message
    );
}

#[test]
fn test_o6_odo_redefines_complex_overlay() {
    // Complex: REDEFINES with nested structure and ODO inside
    let copybook = r#"
       01 PAYMENT-REC.
          05 PAY-TYPE PIC X(2).
          05 ITEM-COUNT PIC 9(2).
          05 PAY-BASE PIC X(50).
          05 PAY-DETAIL REDEFINES PAY-BASE.
             10 DETAIL-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON ITEM-COUNT.
                15 ITEM-CODE PIC X(5).
                15 ITEM-AMT PIC 9(5)V99.
    "#;

    let err =
        parse_copybook(copybook).expect_err("Complex ODO inside REDEFINES (O6) must be rejected");

    assert_eq!(
        err.code,
        ErrorCode::CBKP023_ODO_REDEFINES,
        "Expected CBKP023_ODO_REDEFINES for complex overlay, got: {:?}",
        err
    );
}

#[test]
fn test_o6_odo_redefines_enterprise_scenario() {
    // Enterprise pattern: Multiple REDEFINES with ODO inside
    let copybook = r#"
       01 CLAIM-REC.
          05 CLAIM-TYPE PIC X(1).
          05 LINE-COUNT PIC 9(3).
          05 CLAIM-BASE PIC X(200).
          05 CLAIM-MEDICAL REDEFINES CLAIM-BASE.
             10 PROC-ITEMS OCCURS 1 TO 200 TIMES DEPENDING ON LINE-COUNT.
                15 DIAG-CODE PIC X(10).
                15 PROC-CODE PIC X(7).
                15 PROC-AMT PIC 9(7)V99.
    "#;

    let err = parse_copybook(copybook)
        .expect_err("Enterprise ODO inside REDEFINES (O6) must be rejected");

    assert_eq!(
        err.code,
        ErrorCode::CBKP023_ODO_REDEFINES,
        "Expected CBKP023_ODO_REDEFINES for enterprise scenario, got: {:?}",
        err
    );
}

// =============================================================================
// Combined O5 + O6 Scenarios (should fail on first violation)
// =============================================================================

#[test]
fn test_o5_o6_combined_nested_odo_and_redefines() {
    // Pathological case: nested ODO inside REDEFINES
    // Should fail on first encountered violation (likely O5 or O6 depending on parse order)
    let copybook = r#"
       01 COMPLEX-REC.
          05 OUTER-COUNT PIC 9(2).
          05 BASE-DATA PIC X(500).
          05 OUTER-DETAIL REDEFINES BASE-DATA.
             10 OUTER-ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON OUTER-COUNT.
                15 INNER-COUNT PIC 9(2).
                15 INNER-ITEMS OCCURS 1 TO 200 TIMES DEPENDING ON INNER-COUNT.
                   20 DATA-VALUE PIC X(10).
    "#;

    let err = parse_copybook(copybook).expect_err("Combined O5+O6 violation must be rejected");

    // Accept either error code (depends on validation order)
    assert!(
        err.code == ErrorCode::CBKP022_NESTED_ODO || err.code == ErrorCode::CBKP023_ODO_REDEFINES,
        "Expected CBKP022_NESTED_ODO or CBKP023_ODO_REDEFINES, got: {:?}",
        err
    );
}

// =============================================================================
// Regression Tests: Ensure O1-O4 still work correctly
// =============================================================================

#[test]
fn test_regression_o1_simple_tail_odo_still_works() {
    // O1: Simple tail ODO should still parse successfully
    let copybook = r#"
       01 CUSTOMER-REC.
          05 CUST-ID PIC 9(6).
          05 TRANS-COUNT PIC 9(3).
          05 TRANSACTIONS OCCURS 1 TO 100 TIMES DEPENDING ON TRANS-COUNT.
             10 TRANS-ID PIC 9(10).
             10 TRANS-AMT PIC 9(7)V99.
    "#;

    let result = parse_copybook(copybook);
    assert!(
        result.is_ok(),
        "O1 (simple tail ODO) should still parse successfully, got error: {:?}",
        result.err()
    );
}

#[test]
fn test_regression_o3_group_with_odo_tail_still_works() {
    // O3: Group with ODO at tail should still parse successfully
    let copybook = r#"
       01 ORDER-REC.
          05 ORDER-ID PIC 9(8).
          05 LINE-COUNT PIC 9(2).
          05 ORDER-LINES OCCURS 1 TO 50 TIMES DEPENDING ON LINE-COUNT.
             10 LINE-ITEM.
                15 PRODUCT-ID PIC 9(6).
                15 QUANTITY PIC 9(4).
                15 PRICE PIC 9(7)V99.
    "#;

    let result = parse_copybook(copybook);
    assert!(
        result.is_ok(),
        "O3 (group-with-ODO tail) should still parse successfully, got error: {:?}",
        result.err()
    );
}

#[test]
fn test_regression_o4_odo_with_sibling_still_fails() {
    // O4: ODO with storage sibling should still fail with CBKP021_ODO_NOT_TAIL
    let copybook = r#"
       01 INVALID-REC.
          05 COUNT-FIELD PIC 9(2).
          05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON COUNT-FIELD.
             10 ITEM-VALUE PIC X(10).
          05 TRAILER-FIELD PIC X(5).
    "#;

    let err = parse_copybook(copybook).expect_err("O4 (sibling after ODO) should still fail");

    assert_eq!(
        err.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "O4 should still fail with CBKP021_ODO_NOT_TAIL, got: {:?}",
        err
    );
}
