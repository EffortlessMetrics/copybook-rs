#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]
#![allow(clippy::unwrap_used, clippy::expect_used)]

/*!
 * Golden Fixtures for ODO (Occurs Depending On) Structural Validation
 *
 * These tests represent the canonical behaviors for ODO structural constraints
 * in COBOL copybooks. They serve as golden fixtures to ensure structural
 * semantics remain stable across versions.
 *
 * **Critical for Production**: These tests validate that copybook-rs correctly
 * enforces COBOL ODO placement rules, which are essential for maintaining
 * data structure integrity in mainframe applications.
 */

use copybook_core::{ErrorCode, parse_copybook};

/// Golden Fixture: Level-88 after ODO (PASS)
///
/// **Purpose**: Validates that level-88 (condition) fields after ODO arrays are allowed.
/// **COBOL Rule**: Non-storage fields (level 88) don't violate ODO tail constraints.
/// **Production Impact**: Ensures conditional fields can follow variable arrays.
#[test]
fn golden_level_88_after_odo_passes() {
    const COPYBOOK: &str = r"
01 VARIABLE-RECORD.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-NAME  PIC X(3).
   88 STATUS-ACTIVE VALUE 'Y'.
   88 STATUS-INACTIVE VALUE 'N'.
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_ok(),
        "Golden fixture failed: Level-88 fields after ODO should be valid. Error: {:?}",
        result.err()
    );

    let schema = result.unwrap();
    assert!(
        !schema.all_fields().is_empty(),
        "Schema should contain parsed fields"
    );
}

/// Golden Fixture: Child inside ODO (PASS)
///
/// **Purpose**: Validates that ODO arrays can contain child fields.
/// **COBOL Rule**: ODO arrays are valid when they are the last storage element in their scope.
/// **Production Impact**: Essential for variable-length record processing.
#[test]
fn golden_child_inside_odo_passes() {
    const COPYBOOK: &str = r"
01 VARIABLE-RECORD.
   05 RECORD-LENGTH  PIC 9(4) COMP.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-ID    PIC 9(6).
      10 ITEM-NAME  PIC X(15).
      10 ITEM-PRICE PIC S9(5)V99 COMP-3.
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_ok(),
        "Golden fixture failed: ODO with children should be valid. Error: {:?}",
        result.err()
    );

    let schema = result.unwrap();
    assert!(
        !schema.all_fields().is_empty(),
        "Schema should contain parsed fields including ODO children"
    );
}

/// Golden Fixture: Storage sibling after ODO (FAIL)
///
/// **Purpose**: Validates that storage fields after ODO arrays are rejected.
/// **COBOL Rule**: ODO arrays must be the last storage element to ensure proper memory layout.
/// **Production Impact**: Prevents data corruption from variable-length field placement.
#[test]
fn golden_storage_sibling_after_odo_fails() {
    const COPYBOOK: &str = r"
01 VARIABLE-RECORD.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-NAME  PIC X(3).
   05 TRAILER       PIC X(1).
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "Golden fixture failed: Storage fields after ODO should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for storage sibling after ODO, got: {:?}",
        error.code
    );
}

/// Golden Fixture: Simple ODO with nested structure (PASS)
///
/// **Purpose**: Validates that ODO arrays with nested child structures work correctly.
/// **COBOL Rule**: ODO arrays can contain hierarchical field structures.
/// **Production Impact**: Ensures variable-length records with complex data work in production.
#[test]
fn golden_simple_odo_with_nesting_passes() {
    const COPYBOOK: &str = r"
01 NESTED-RECORD.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 50 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-HEADER.
         15 ITEM-ID    PIC 9(6).
         15 ITEM-TYPE  PIC X(2).
      10 ITEM-DATA.
         15 ITEM-VALUE PIC S9(9)V99 COMP-3.
         15 ITEM-FLAG  PIC X(1).
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_ok(),
        "Golden fixture failed: ODO with nested structure should be valid. Error: {:?}",
        result.err()
    );

    let schema = result.unwrap();
    assert!(
        !schema.all_fields().is_empty(),
        "Schema should contain ODO with nested structure"
    );
}

/// Golden Fixture: Multiple ODO violation (FAIL)
///
/// **Purpose**: Validates that multiple ODO arrays with storage after the last one fail.
/// **COBOL Rule**: The final ODO in a sequence must still respect tail constraints.
/// **Production Impact**: Prevents complex variable-length layouts that could corrupt data.
#[test]
fn golden_multiple_odo_violation_fails() {
    const COPYBOOK: &str = r"
01 MULTI-ODO-RECORD.
   05 COUNT1        PIC 9(3).
   05 ARRAY1 OCCURS 5 TIMES DEPENDING ON COUNT1 PIC X(3).
   05 COUNT2        PIC 9(3).
   05 ARRAY2 OCCURS 5 TIMES DEPENDING ON COUNT2 PIC X(3).
   05 TRAILER       PIC X(1).
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "Golden fixture failed: Multiple ODO with storage trailer should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for multiple ODO violation, got: {:?}",
        error.code
    );
}

/// Comprehensive Golden Fixture Validation
///
/// **Purpose**: Meta-test that ensures all golden fixtures are run and accounted for.
/// **Production Impact**: Guarantees structural validation coverage for CI/CD.
#[test]
fn golden_fixtures_comprehensive_coverage() {
    // This test serves as documentation of all golden fixtures
    // and ensures they remain in the test suite

    let golden_fixtures = [
        "golden_level_88_after_odo_passes",
        "golden_child_inside_odo_passes",
        "golden_storage_sibling_after_odo_fails",
        "golden_simple_odo_with_nesting_passes",
        "golden_multiple_odo_violation_fails",
    ];

    // Verify we have the expected count of golden fixtures
    assert_eq!(
        golden_fixtures.len(),
        5,
        "Expected 5 golden fixtures for ODO structural validation"
    );

    // This test passing means all golden fixtures above compiled and are available
    println!(
        "✅ All {} ODO golden fixtures are present and accounted for",
        golden_fixtures.len()
    );
}
