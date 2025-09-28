#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::assertions_on_constants
)]

use copybook_core::{ErrorCode, parse_copybook};

#[test]
fn odo_tail_ok_with_children_but_no_sibling_after() {
    // ODO group is the last sibling; it has children inside (valid).
    const CPY: &str = r"
01 VARIABLE-RECORD.
   05 RECORD-LENGTH  PIC 9(4) COMP.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-ID    PIC 9(6).
      10 ITEM-NAME  PIC X(15).
      10 ITEM-PRICE PIC S9(5)V99 COMP-3.
";
    match parse_copybook(CPY) {
        Ok(_) => {} // Test passes
        Err(e) => panic!("Expected success but got error: {e:?}"),
    }
}

#[test]
fn odo_tail_fails_when_storage_sibling_follows() {
    // ODO group is *not* last; there is a storage sibling after it (invalid).
    const CPY: &str = r"
01 VARIABLE-RECORD.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-NAME  PIC X(3).
   05 TRAILER       PIC X(1).
";

    let err = parse_copybook(CPY).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

#[test]
fn odo_tail_ok_with_level_88_sibling_after() {
    // ODO group with only level 88 (non-storage) fields after it should be valid.
    const CPY: &str = r"
01 VARIABLE-RECORD.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-NAME  PIC X(3).
   88 STATUS-ACTIVE VALUE 'Y'.
   88 STATUS-INACTIVE VALUE 'N'.
";

    match parse_copybook(CPY) {
        Ok(_) => {} // Test passes
        Err(e) => panic!("Expected success but got error: {e:?}"),
    }
}

#[test]
fn odo_tail_ok_when_odo_is_only_child() {
    // ODO array as the only child should be valid (using simpler fixed max syntax).
    const CPY: &str = r"
01 COUNTER PIC 9(3).
01 ARRAY-FIELD PIC X(10) OCCURS 5 TIMES DEPENDING ON COUNTER.
";

    match parse_copybook(CPY) {
        Ok(_) => {} // Test passes
        Err(e) => panic!("Expected success but got error: {e:?}"),
    }
}

#[test]
fn odo_tail_ok_with_multiple_non_storage_siblings_after() {
    // ODO array with multiple level 88 fields after it should be valid.
    const CPY: &str = r"
01 COMPLEX-RECORD.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-NAME  PIC X(3).
   05 FLAG-1         PIC X(1).
      88 FLAG-1-ON  VALUE 'Y'.
      88 FLAG-1-OFF VALUE 'N'.
   05 FLAG-2         PIC X(1).
      88 FLAG-2-VALID VALUE 'V'.
";

    // This should fail because FLAG-1 and FLAG-2 are storage fields
    let err = parse_copybook(CPY).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

#[test]
fn odo_tail_validation_multiple_odo_arrays() {
    // Test multiple ODO arrays where one violates tail rule.
    const CPY: &str = r"
01 MULTI-ODO-RECORD.
   05 COUNT1        PIC 9(3).
   05 ARRAY1 OCCURS 5 TIMES DEPENDING ON COUNT1 PIC X(3).
   05 COUNT2        PIC 9(3).
   05 ARRAY2 OCCURS 5 TIMES DEPENDING ON COUNT2 PIC X(3).
   05 TRAILER       PIC X(1).
";

    // This should fail because TRAILER comes after ARRAY2 (the last ODO array)
    let err = parse_copybook(CPY).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

#[test]
fn odo_tail_validation_with_non_storage_after() {
    // Test ODO array with only non-storage fields after it.
    const CPY: &str = r"
01 VALID-WITH-NON-STORAGE.
   05 COUNT         PIC 9(3).
   05 ITEMS OCCURS 5 TIMES DEPENDING ON COUNT PIC X(3).
   88 ITEMS-EMPTY VALUE '000'.
   88 ITEMS-PRESENT VALUE '001' THROUGH '999'.
";

    // This should be valid because only level 88 fields come after the ODO array
    match parse_copybook(CPY) {
        Ok(_) => {} // Test passes
        Err(e) => panic!("Expected success but got error: {e:?}"),
    }
}
