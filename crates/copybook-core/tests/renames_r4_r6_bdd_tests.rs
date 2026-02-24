// SPDX-License-Identifier: AGPL-3.0-or-later
// BDD-style tests for RENAMES R4-R6 advanced scenarios
//
// This test suite covers:
// - R4: RENAMES + REDEFINES (overlapping storage)
// - R5: RENAMES + OCCURS (array segment aliasing)
// - R6: RENAMES + Level-88 (condition value aliasing)
//
// See docs/RENAMES_R4_R6.md for detailed specification.

use copybook_core::dialect::Dialect;
use copybook_core::error::ErrorCode;
use copybook_core::{ParseOptions, parse_copybook_with_options};

// Default dialect for tests
const TEST_DIALECT: Dialect = Dialect::Normative;

// ============================================================================
// R4: RENAMES + REDEFINES (Feature Flag Disabled)
// ============================================================================

#[test]
fn test_r4_renames_over_redefines_rejected_when_flag_disabled() {
    // Scenario: RENAMES alias spans REDEFINES field(s)
    // Expected: Rejected with CBKS609_RENAME_OVER_REDEFINES

    let copybook = r#"
       01  TRANSACTION-RECORD.
           05  TRANS-TYPE      PIC X(1).
           05  TRANS-DATA      PIC X(20).
           05  CHECK-DATA      REDEFINES TRANS-DATA.
               10  CHECK-NUM  PIC 9(8).
               10  CHECK-AMT  PIC 9(10).
           05  CARD-DATA       REDEFINES TRANS-DATA.
               10  CARD-NUM   PIC 9(16).
               10  CARD-EXP   PIC 9(4).
           66  PAYMENT-INFO RENAMES CHECK-DATA THRU CHECK-DATA.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS609_RENAME_OVER_REDEFINES);
    assert!(
        error
            .message
            .contains("RENAMES alias 'PAYMENT-INFO' spans REDEFINES field(s)")
    );
    assert!(error.message.contains("Enable RenamesR4R6 feature flag"));
}

#[test]
fn test_r4_renames_over_multiple_redefines_rejected() {
    // Scenario: RENAMES alias spans multiple REDEFINES alternatives
    // Expected: Rejected with CBKS609_RENAME_OVER_REDEFINES when flag disabled

    let copybook = r#"
       01  TRANSACTION-RECORD.
           05  TRANS-TYPE      PIC X(1).
           05  TRANS-DATA      PIC X(20).
           05  CHECK-DATA      REDEFINES TRANS-DATA.
               10  CHECK-NUM  PIC 9(8).
               10  CHECK-AMT  PIC 9(10).
           05  CARD-DATA       REDEFINES TRANS-DATA.
               10  CARD-NUM   PIC 9(16).
               10  CARD-EXP   PIC 9(4).
           66  PAYMENT-INFO RENAMES CHECK-DATA THRU CARD-DATA.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS609_RENAME_OVER_REDEFINES);
    assert!(
        error
            .message
            .contains("RENAMES alias 'PAYMENT-INFO' spans REDEFINES field(s)")
    );
}

// ============================================================================
// R5: RENAMES + OCCURS (Feature Flag Disabled)
// ============================================================================

#[test]
fn test_r5_renames_over_occurs_rejected_when_flag_disabled() {
    // Scenario: RENAMES alias spans OCCURS array
    // Expected: Rejected with CBKS607_RENAME_CROSSES_OCCURS

    let copybook = r#"
       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  LINE-ITEMS      OCCURS 10 TIMES.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           66  ORDER-ITEMS RENAMES LINE-ITEMS THRU LINE-ITEMS.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS607_RENAME_CROSSES_OCCURS);
    assert!(
        error
            .message
            .contains("RENAMES alias 'ORDER-ITEMS' crosses OCCURS boundary")
    );
    assert!(error.message.contains("Enable RenamesR4R6 feature flag"));
}

#[test]
fn test_r5_renames_partial_occurs_rejected() {
    // Scenario: RENAMES alias spans partial array elements (FROM != THRU)
    // Expected: Rejected with CBKS607_RENAME_CROSSES_OCCURS when flag disabled

    let copybook = r#"
       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  LINE-ITEMS      OCCURS 10 TIMES.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           05  TOTAL-AMT       PIC 9(10).
           66  ORDER-ITEMS RENAMES LINE-ITEMS THRU TOTAL-AMT.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS607_RENAME_CROSSES_OCCURS);
    assert!(
        error
            .message
            .contains("RENAMES alias 'ORDER-ITEMS' crosses OCCURS boundary")
    );
}

#[test]
fn test_r5_renames_odo_rejected() {
    // Scenario: RENAMES alias spans ODO array
    // Expected: Rejected with CBKS612_RENAME_ODO_NOT_SUPPORTED (even with flag enabled)

    let copybook = r#"
       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  ITEM-COUNT     PIC 9(3).
           05  LINE-ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           66  ORDER-ITEMS RENAMES LINE-ITEMS THRU LINE-ITEMS.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    // ODO arrays are not supported even with feature flag enabled
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS612_RENAME_ODO_NOT_SUPPORTED);
    assert!(
        error
            .message
            .contains("RENAMES alias 'ORDER-ITEMS' spans ODO array")
    );
    assert!(error.message.contains("This pattern is not supported"));
}

// ============================================================================
// R6: RENAMES + Level-88 (Always Supported)
// ============================================================================

#[test]
fn test_r6_renames_with_level_88_accepted() {
    // Scenario: RENAMES alias for field with Level-88 condition values
    // Expected: Accepted (Level-88 fields are non-storage and naturally excluded)

    let copybook = r#"
       01  STATUS-RECORD.
           05  STATUS-CODE     PIC X(1).
               88  STATUS-OK   VALUE 'A'.
               88  STATUS-ERR VALUE 'E'.
           66  STATUS-FLAG RENAMES STATUS-CODE THRU STATUS-CODE.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    // R6 patterns should be accepted
    assert!(result.is_ok());
    let schema = result.unwrap();

    // Verify the RENAMES field is present
    let status_flag = schema
        .find_field_or_alias("STATUS-FLAG")
        .expect("STATUS-FLAG field should exist");

    assert_eq!(status_flag.level, 66);
    assert!(status_flag.resolved_renames.is_some());

    // Level-88 fields should be excluded from members (non-storage)
    let resolved = status_flag.resolved_renames.as_ref().unwrap();
    assert_eq!(resolved.members.len(), 1);
    assert!(resolved.members[0].ends_with("STATUS-CODE"));
}

// ============================================================================
// R4-R6: Valid Patterns (Feature Flag Enabled)
// ============================================================================

#[test]
fn test_r4_single_redefines_accepted_with_feature_flag() {
    // Scenario: RENAMES alias for single REDEFINES alternative (FROM==THRU)
    // Expected: Accepted when feature flag is enabled
    // Note: This test would need feature flag to be enabled to pass
    // For now, we test that it's rejected without the flag

    let copybook = r#"
       01  TRANSACTION-RECORD.
           05  TRANS-TYPE      PIC X(1).
           05  TRANS-DATA      PIC X(20).
           05  CHECK-DATA      REDEFINES TRANS-DATA.
               10  CHECK-NUM  PIC 9(8).
               10  CHECK-AMT  PIC 9(10).
           66  PAYMENT-INFO RENAMES CHECK-DATA THRU CHECK-DATA.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    // Without feature flag, R4 patterns are rejected
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS609_RENAME_OVER_REDEFINES);
}

#[test]
fn test_r5_single_occurs_accepted_with_feature_flag() {
    // Scenario: RENAMES alias for entire OCCURS array (FROM==THRU)
    // Expected: Accepted when feature flag is enabled
    // Note: This test would need feature flag to be enabled to pass
    // For now, we test that it's rejected without the flag

    let copybook = r#"
       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  LINE-ITEMS      OCCURS 10 TIMES.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           66  ORDER-ITEMS RENAMES LINE-ITEMS THRU LINE-ITEMS.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    // Without feature flag, R5 patterns are rejected
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS607_RENAME_CROSSES_OCCURS);
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_r4_r5_r6_combined_scenarios() {
    // Scenario: RENAMES with combinations of R4, R5, R6
    // Expected: Appropriate error based on first detected pattern

    // R4 + R5: RENAMES over REDEFINES + OCCURS
    let copybook_r4_r5 = r#"
       01  COMPLEX-RECORD.
           05  FIELD-A         PIC X(10).
           05  REDEF-B         REDEFINES FIELD-A.
               10  SUB-B1   PIC 9(5).
           05  ARRAY-FIELD    OCCURS 5 TIMES.
               10  SUB-A1   PIC 9(5).
           66  COMPLEX-ALIAS RENAMES REDEF-B THRU ARRAY-FIELD.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook_r4_r5, &options);

    // Should be rejected (REDEFINES takes precedence in error detection)
    assert!(result.is_err());
    let error = result.unwrap_err();
    // Either REDEFINES or OCCURS error is acceptable
    assert!(
        error.code == ErrorCode::CBKS609_RENAME_OVER_REDEFINES
            || error.code == ErrorCode::CBKS607_RENAME_CROSSES_OCCURS
    );
}

#[test]
fn test_r6_renames_with_nested_level_88() {
    // Scenario: RENAMES alias for field with nested Level-88 condition values
    // Expected: Accepted (Level-88 fields are non-storage and naturally excluded)

    let copybook = r#"
       01  STATUS-RECORD.
           05  STATUS-GROUP.
               10  STATUS-CODE  PIC X(1).
                   88  STATUS-OK   VALUE 'A'.
                   88  STATUS-ERR VALUE 'E'.
                   88  STATUS-PEND VALUE 'P'.
           66  STATUS-ALIAS RENAMES STATUS-CODE THRU STATUS-CODE.
    "#;

    let options = ParseOptions {
        dialect: TEST_DIALECT,
        ..ParseOptions::default()
    };

    let result = parse_copybook_with_options(copybook, &options);

    // R6 patterns should be accepted
    assert!(result.is_ok());
    let schema = result.unwrap();

    // Verify the RENAMES field is present
    let status_alias = schema
        .find_field_or_alias("STATUS-ALIAS")
        .expect("STATUS-ALIAS field should exist");

    assert_eq!(status_alias.level, 66);
    assert!(status_alias.resolved_renames.is_some());

    // Level-88 fields should be excluded from members
    let resolved = status_alias.resolved_renames.as_ref().unwrap();
    assert_eq!(resolved.members.len(), 1);
    assert!(resolved.members[0].ends_with("STATUS-CODE"));
}
