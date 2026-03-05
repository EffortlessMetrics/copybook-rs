// SPDX-License-Identifier: AGPL-3.0-or-later
//! Negative tests for level-66 RENAMES resolver validation (Issue #122, Slice-2)
//!
//! This test suite validates resolver error conditions:
//! - CBKS601/CBKS602: Unknown from/thru fields
//! - CBKS603: Non-contiguous ranges
//! - CBKS604: Reversed ranges (from after thru)
//! - CBKS605/CBKS606: Cross-group boundaries
//! - CBKS607: OCCURS boundary violations
//! - CBKS608: Qualified name resolution

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::panic)]

use copybook_core::{error::ErrorCode, parse_copybook};

/// Test CBKS601: RENAMES from field not found
#[test]
fn renames_unknown_from_field() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   05 FIELD-B PIC 9(3).
   66 ALIAS RENAMES UNKNOWN-FIELD THRU FIELD-B.
";
    let result = parse_copybook(cb);
    assert!(result.is_err(), "Should fail with unknown from field");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS601_RENAME_UNKNOWN_FROM);
    assert!(err.to_string().contains("UNKNOWN-FIELD"));
}

/// Test CBKS602: RENAMES thru field not found
#[test]
fn renames_unknown_thru_field() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   05 FIELD-B PIC 9(3).
   66 ALIAS RENAMES FIELD-A THRU UNKNOWN-FIELD.
";
    let result = parse_copybook(cb);
    assert!(result.is_err(), "Should fail with unknown thru field");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS602_RENAME_UNKNOWN_THRU);
    assert!(err.to_string().contains("UNKNOWN-FIELD"));
}

/// Test CBKS604: RENAMES reversed range (from comes after thru)
#[test]
fn renames_reversed_range() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   05 FIELD-B PIC 9(3).
   66 ALIAS RENAMES FIELD-B THRU FIELD-A.
";
    let result = parse_copybook(cb);
    assert!(result.is_err(), "Should fail with reversed range");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS604_RENAME_REVERSED_RANGE);
    assert!(err.to_string().contains("FIELD-B"));
    assert!(err.to_string().contains("FIELD-A"));
}

/// Test CBKS605: RENAMES from field crosses group boundary
#[test]
fn renames_from_crosses_group() {
    let cb = "
01 ROOT-REC.
   05 GROUP-A.
      10 FIELD-A PIC X(5).
      10 FIELD-B PIC 9(3).
   05 FIELD-C PIC X(2).
   66 ALIAS RENAMES GROUP-A THRU FIELD-C.
";
    let result = parse_copybook(cb);
    assert!(result.is_err(), "Should fail when from is a group");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS605_RENAME_FROM_CROSSES_GROUP);
    assert!(err.to_string().contains("GROUP-A"));
}

/// Test CBKS606: RENAMES thru field crosses group boundary
#[test]
fn renames_thru_crosses_group() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   05 GROUP-B.
      10 FIELD-B PIC 9(3).
      10 FIELD-C PIC X(2).
   66 ALIAS RENAMES FIELD-A THRU GROUP-B.
";
    let result = parse_copybook(cb);
    assert!(result.is_err(), "Should fail when thru is a group");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS606_RENAME_THRU_CROSSES_GROUP);
    assert!(err.to_string().contains("GROUP-B"));
}

/// Test CBKS605: RENAMES range spans across group boundary (intermediate group)
#[test]
fn renames_crosses_intermediate_group() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   05 GROUP-MID.
      10 FIELD-MID PIC 9(3).
   05 FIELD-B PIC X(2).
   66 ALIAS RENAMES FIELD-A THRU FIELD-B.
";
    let result = parse_copybook(cb);
    assert!(
        result.is_err(),
        "Should fail when range crosses intermediate group"
    );
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS605_RENAME_FROM_CROSSES_GROUP);
    assert!(err.to_string().contains("GROUP-MID"));
}

/// Test CBKS607: RENAMES range crosses OCCURS boundary
#[test]
fn renames_crosses_occurs_boundary() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   05 ARRAY-FIELD PIC 9(3) OCCURS 5 TIMES.
   05 FIELD-B PIC X(2).
   66 ALIAS RENAMES FIELD-A THRU FIELD-B.
";
    let result = parse_copybook(cb);
    assert!(result.is_err(), "Should fail when range crosses OCCURS");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS607_RENAME_CROSSES_OCCURS);
    assert!(err.to_string().contains("ARRAY-FIELD"));
}

/// Test CBKS607: RENAMES with OCCURS at from position
#[test]
fn renames_from_is_occurs() {
    let cb = "
01 ROOT-REC.
   05 ARRAY-A PIC X(5) OCCURS 3 TIMES.
   05 FIELD-B PIC 9(3).
   66 ALIAS RENAMES ARRAY-A THRU FIELD-B.
";
    let result = parse_copybook(cb);
    assert!(result.is_err(), "Should fail when from field has OCCURS");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS607_RENAME_CROSSES_OCCURS);
}

/// Test CBKS607: RENAMES with OCCURS at thru position
#[test]
fn renames_thru_is_occurs() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   05 ARRAY-B PIC 9(3) OCCURS 3 TIMES.
   66 ALIAS RENAMES FIELD-A THRU ARRAY-B.
";
    let result = parse_copybook(cb);
    assert!(result.is_err(), "Should fail when thru field has OCCURS");
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS607_RENAME_CROSSES_OCCURS);
}

/// Test R4: RENAMES range crosses REDEFINES (flag disabled)
///
/// Note: With RenamesR4R6 disabled, any REDEFINES in the RENAMES span
/// is rejected with CBKS609, regardless of offset contiguity.
#[test]
fn renames_not_contiguous_gap() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   05 FIELD-B REDEFINES FIELD-A PIC X(5).
   05 FIELD-C PIC 9(3).
   66 ALIAS RENAMES FIELD-A THRU FIELD-C.
";
    let result = parse_copybook(cb);
    assert!(
        result.is_err(),
        "REDEFINES in RENAMES span should be rejected"
    );
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS609_RENAME_OVER_REDEFINES);
}

/// Test CBKS603: Non-contiguous range with actual gap
///
/// This creates a scenario where fields are not adjacent in sibling order
/// but we try to RENAMES across them. Due to how the parser works, this
/// is hard to construct without OCCURS or groups, which are caught by other
/// validations. This test documents the contiguity check behavior.
#[test]
fn renames_contiguity_check_with_level88() {
    let cb = "
01 ROOT-REC.
   05 FIELD-A PIC X(5).
   88 CONDITION-A VALUE 'TEST'.
   05 FIELD-B PIC 9(3).
   66 ALIAS RENAMES FIELD-A THRU FIELD-B.
";
    // Level-88 doesn't have storage, so this should succeed
    let result = parse_copybook(cb);
    assert!(
        result.is_ok(),
        "Level-88 condition values don't break contiguity"
    );
}

/// Test nested group validation: RENAMES should work within nested group scope
#[test]
#[ignore = "Blocked on parser/resolver design for nested groups (see Issue #133)"]
fn renames_within_nested_group_valid() {
    // Desired behavior: RENAMES within nested group scope should resolve correctly
    // Current limitation: Parser attaches 66 at level-01, resolver can't find FIELD-A/FIELD-B
    // Resolution: Requires resolver-based semantic approach (see docs/design/RENAMES_NESTED_GROUPS.md)
    let cb = "
01 ROOT-REC.
   05 GROUP-A.
      10 FIELD-A PIC X(5).
      10 FIELD-B PIC 9(3).
      66 ALIAS-A RENAMES FIELD-A THRU FIELD-B.
";
    let result = parse_copybook(cb);
    assert!(
        result.is_ok(),
        "RENAMES within nested group should be valid: {:?}",
        result.err()
    );
}
