// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep OCCURS clause tests for copybook-core parser.
//!
//! Validates fixed OCCURS, OCCURS with INDEXED BY, OCCURS DEPENDING ON,
//! group-level OCCURS, OCCURS with KEY IS, and nested fixed OCCURS.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use copybook_core::{FieldKind, Occurs, parse_copybook};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn find_field<'a>(schema: &'a copybook_core::Schema, name: &str) -> &'a copybook_core::Field {
    schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("field '{name}' not found in schema"))
}

fn parse(text: &str) -> copybook_core::Schema {
    parse_copybook(text).expect("parse failed")
}

// ===========================================================================
// 1. Fixed OCCURS
// ===========================================================================

#[test]
fn test_fixed_occurs_elementary() {
    let cpy = r"
       01  REC.
           05  ITEMS PIC X(10) OCCURS 5 TIMES.
    ";
    let schema = parse(cpy);
    let items = find_field(&schema, "ITEMS");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 5 })));
    assert_eq!(items.len, 10);
    assert_eq!(items.effective_length(), 50);
    assert_eq!(schema.lrecl_fixed, Some(50));
}

#[test]
fn test_fixed_occurs_group() {
    let cpy = r"
       01  REC.
           05  ENTRIES OCCURS 3 TIMES.
               10  ENT-KEY   PIC X(4).
               10  ENT-DATA  PIC X(8).
    ";
    let schema = parse(cpy);
    let entries = find_field(&schema, "ENTRIES");
    assert!(matches!(entries.occurs, Some(Occurs::Fixed { count: 3 })));
    assert!(entries.is_group());
    assert_eq!(entries.len, 12);
    assert_eq!(entries.effective_length(), 36);
    assert_eq!(schema.lrecl_fixed, Some(36));
}

#[test]
fn test_fixed_occurs_count_1() {
    let cpy = r"
       01  REC.
           05  SINGLE PIC X(5) OCCURS 1 TIMES.
    ";
    let schema = parse(cpy);
    let f = find_field(&schema, "SINGLE");
    assert!(matches!(f.occurs, Some(Occurs::Fixed { count: 1 })));
    assert_eq!(f.effective_length(), 5);
}

#[test]
fn test_fixed_occurs_large_count() {
    let cpy = r"
       01  REC.
           05  BIG-ARRAY PIC X(2) OCCURS 100 TIMES.
    ";
    let schema = parse(cpy);
    let f = find_field(&schema, "BIG-ARRAY");
    assert!(matches!(f.occurs, Some(Occurs::Fixed { count: 100 })));
    assert_eq!(f.effective_length(), 200);
    assert_eq!(schema.lrecl_fixed, Some(200));
}

// ===========================================================================
// 2. OCCURS with INDEXED BY
// ===========================================================================

#[test]
fn test_occurs_indexed_by_elementary() {
    let cpy = r"
       01  REC.
           05  ITEMS PIC X(10) OCCURS 5 TIMES
               INDEXED BY IDX-ITEM.
    ";
    let schema = parse(cpy);
    let items = find_field(&schema, "ITEMS");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 5 })));
    assert_eq!(items.effective_length(), 50);
}

#[test]
fn test_occurs_indexed_by_group() {
    let cpy = r"
       01  REC.
           05  ROWS OCCURS 10 TIMES
               INDEXED BY ROW-IDX.
               10  COL-A PIC X(5).
               10  COL-B PIC 9(3).
    ";
    let schema = parse(cpy);
    let rows = find_field(&schema, "ROWS");
    assert!(matches!(rows.occurs, Some(Occurs::Fixed { count: 10 })));
    assert!(rows.is_group());
    assert_eq!(rows.len, 8);
    assert_eq!(rows.effective_length(), 80);
}

#[test]
fn test_occurs_indexed_by_does_not_affect_lrecl() {
    let cpy = r"
       01  REC.
           05  HDR PIC X(4).
           05  ARR PIC X(6) OCCURS 3 TIMES
               INDEXED BY I1.
    ";
    let schema = parse(cpy);
    // INDEXED BY shouldn't add storage
    assert_eq!(schema.lrecl_fixed, Some(22)); // 4 + 6*3
}

// ===========================================================================
// 3. OCCURS DEPENDING ON (valid tail position)
// ===========================================================================

#[test]
fn test_odo_basic() {
    let cpy = r"
       01  REC.
           05  CNT PIC 9(3).
           05  ITEMS PIC X(10) OCCURS 1 TO 5
               DEPENDING ON CNT.
    ";
    let schema = parse(cpy);
    let items = find_field(&schema, "ITEMS");
    assert!(matches!(
        items.occurs,
        Some(Occurs::ODO { min: 1, max: 5, .. })
    ));
    assert_eq!(items.len, 10);
    // tail_odo should be populated
    assert!(schema.tail_odo.is_some());
    let odo = schema.tail_odo.as_ref().unwrap();
    assert_eq!(odo.min_count, 1);
    assert_eq!(odo.max_count, 5);
}

#[test]
fn test_odo_group_level() {
    let cpy = r"
       01  REC.
           05  NUM-ENTRIES PIC 9(2).
           05  ENTRY OCCURS 1 TO 10
               DEPENDING ON NUM-ENTRIES.
               10  ENTRY-KEY  PIC X(4).
               10  ENTRY-VAL  PIC X(8).
    ";
    let schema = parse(cpy);
    let entry = find_field(&schema, "ENTRY");
    assert!(matches!(
        entry.occurs,
        Some(Occurs::ODO { min: 1, max: 10, .. })
    ));
    assert!(entry.is_group());
    assert_eq!(entry.len, 12);
}

#[test]
fn test_odo_with_fixed_prefix() {
    let cpy = r"
       01  REC.
           05  HEADER PIC X(20).
           05  ITEM-COUNT PIC 9(3).
           05  LINE-ITEMS PIC X(50) OCCURS 0 TO 99
               DEPENDING ON ITEM-COUNT.
    ";
    let schema = parse(cpy);
    let counter = find_field(&schema, "ITEM-COUNT");
    assert_eq!(counter.offset, 20);
    let items = find_field(&schema, "LINE-ITEMS");
    assert!(matches!(
        items.occurs,
        Some(Occurs::ODO { min: 0, max: 99, .. })
    ));
    assert_eq!(items.offset, 23);
}

#[test]
fn test_odo_counter_path_resolved() {
    let cpy = r"
       01  REC.
           05  CTR PIC 9(2).
           05  DATA PIC X(5) OCCURS 1 TO 20
               DEPENDING ON CTR.
    ";
    let schema = parse(cpy);
    if let Some(Occurs::ODO { ref counter_path, .. }) = find_field(&schema, "DATA").occurs {
        assert!(
            counter_path.contains("CTR"),
            "counter_path should reference CTR, got: {counter_path}"
        );
    } else {
        panic!("expected ODO occurs");
    }
}

// ===========================================================================
// 4. Group-level OCCURS
// ===========================================================================

#[test]
fn test_group_occurs_children_accessible() {
    let cpy = r"
       01  REC.
           05  ROWS OCCURS 5 TIMES.
               10  COL1 PIC X(4).
               10  COL2 PIC 9(3).
               10  COL3 PIC X(8).
    ";
    let schema = parse(cpy);
    let rows = find_field(&schema, "ROWS");
    assert!(rows.is_group());
    assert_eq!(rows.children.len(), 3);
    assert_eq!(rows.children[0].name, "COL1");
    assert_eq!(rows.children[1].name, "COL2");
    assert_eq!(rows.children[2].name, "COL3");
    // Single occurrence = 4 + 3 + 8 = 15
    assert_eq!(rows.len, 15);
    assert_eq!(rows.effective_length(), 75);
}

#[test]
fn test_group_occurs_with_subgroup() {
    let cpy = r"
       01  REC.
           05  BLOCKS OCCURS 2 TIMES.
               10  HDR.
                   15  HDR-TYPE PIC X(1).
                   15  HDR-LEN  PIC 9(3).
               10  BODY PIC X(20).
    ";
    let schema = parse(cpy);
    let blocks = find_field(&schema, "BLOCKS");
    assert!(matches!(blocks.occurs, Some(Occurs::Fixed { count: 2 })));
    let hdr = find_field(&schema, "HDR");
    assert!(hdr.is_group());
    assert_eq!(hdr.len, 4);
    // Single = 4 + 20 = 24, total = 48
    assert_eq!(schema.lrecl_fixed, Some(48));
}

#[test]
fn test_multiple_occurs_groups_sequential() {
    let cpy = r"
       01  REC.
           05  PART-A OCCURS 3 TIMES.
               10  A-VAL PIC X(5).
           05  PART-B OCCURS 4 TIMES.
               10  B-VAL PIC X(10).
    ";
    let schema = parse(cpy);
    let a = find_field(&schema, "PART-A");
    assert!(matches!(a.occurs, Some(Occurs::Fixed { count: 3 })));
    assert_eq!(a.effective_length(), 15);
    let b = find_field(&schema, "PART-B");
    assert!(matches!(b.occurs, Some(Occurs::Fixed { count: 4 })));
    assert_eq!(b.effective_length(), 40);
    // B offset = 3 * 5 = 15
    assert_eq!(b.offset, 15);
    assert_eq!(schema.lrecl_fixed, Some(55));
}

// ===========================================================================
// 5. OCCURS with KEY IS clause
// ===========================================================================

#[test]
fn test_occurs_with_ascending_key() {
    let cpy = r"
       01  REC.
           05  ITEMS OCCURS 10 TIMES
               ASCENDING KEY IS ITEM-ID.
               10  ITEM-ID   PIC 9(4).
               10  ITEM-DESC PIC X(20).
    ";
    let schema = parse(cpy);
    let items = find_field(&schema, "ITEMS");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 10 })));
    assert!(items.is_group());
    assert_eq!(items.len, 24);
}

#[test]
fn test_occurs_with_descending_key() {
    let cpy = r"
       01  REC.
           05  ENTRIES OCCURS 5 TIMES
               DESCENDING KEY IS ENTRY-CODE.
               10  ENTRY-CODE  PIC X(3).
               10  ENTRY-VALUE PIC 9(6).
    ";
    let schema = parse(cpy);
    let entries = find_field(&schema, "ENTRIES");
    assert!(matches!(entries.occurs, Some(Occurs::Fixed { count: 5 })));
    assert_eq!(entries.len, 9);
    assert_eq!(entries.effective_length(), 45);
}

#[test]
fn test_occurs_with_key_and_indexed_by() {
    let cpy = r"
       01  REC.
           05  TABLE-ENTRIES OCCURS 8 TIMES
               ASCENDING KEY IS SORT-KEY
               INDEXED BY TBL-IDX.
               10  SORT-KEY PIC 9(5).
               10  TBL-DATA PIC X(15).
    ";
    let schema = parse(cpy);
    let entries = find_field(&schema, "TABLE-ENTRIES");
    assert!(matches!(entries.occurs, Some(Occurs::Fixed { count: 8 })));
    assert_eq!(entries.len, 20);
    assert_eq!(entries.effective_length(), 160);
}

// ===========================================================================
// 6. Nested fixed OCCURS
// ===========================================================================

#[test]
fn test_nested_occurs_group_containing_occurs_field() {
    let cpy = r"
       01  REC.
           05  OUTER OCCURS 3 TIMES.
               10  LABEL PIC X(5).
               10  INNER PIC X(2) OCCURS 4 TIMES.
    ";
    let schema = parse(cpy);
    let outer = find_field(&schema, "OUTER");
    assert!(matches!(outer.occurs, Some(Occurs::Fixed { count: 3 })));
    let inner = find_field(&schema, "INNER");
    assert!(matches!(inner.occurs, Some(Occurs::Fixed { count: 4 })));
    // Single outer = 5 + 2*4 = 13
    assert_eq!(outer.len, 13);
    assert_eq!(outer.effective_length(), 39);
    assert_eq!(schema.lrecl_fixed, Some(39));
}

#[test]
fn test_nested_occurs_group_inside_group() {
    let cpy = r"
       01  REC.
           05  MATRIX OCCURS 3 TIMES.
               10  ROW-HDR PIC X(2).
               10  CELLS OCCURS 4 TIMES.
                   15  CELL-VAL PIC 9(3).
    ";
    let schema = parse(cpy);
    let matrix = find_field(&schema, "MATRIX");
    assert!(matches!(matrix.occurs, Some(Occurs::Fixed { count: 3 })));
    let cells = find_field(&schema, "CELLS");
    assert!(matches!(cells.occurs, Some(Occurs::Fixed { count: 4 })));
    assert!(cells.is_group());
    assert_eq!(cells.len, 3);
    assert_eq!(cells.effective_length(), 12);
    // Single matrix row = 2 + 3*4 = 14
    assert_eq!(matrix.len, 14);
    // Total = 14 * 3 = 42
    assert_eq!(schema.lrecl_fixed, Some(42));
}

#[test]
fn test_nested_occurs_three_levels() {
    let cpy = r"
       01  REC.
           05  L1 OCCURS 2 TIMES.
               10  L1-HDR PIC X(1).
               10  L2 OCCURS 3 TIMES.
                   15  L2-HDR PIC X(1).
                   15  L3 PIC X(4) OCCURS 2 TIMES.
    ";
    let schema = parse(cpy);
    let l1 = find_field(&schema, "L1");
    assert!(matches!(l1.occurs, Some(Occurs::Fixed { count: 2 })));
    let l2 = find_field(&schema, "L2");
    assert!(matches!(l2.occurs, Some(Occurs::Fixed { count: 3 })));
    let l3 = find_field(&schema, "L3");
    assert!(matches!(l3.occurs, Some(Occurs::Fixed { count: 2 })));

    // L2 single = 1 + 4*2 = 9
    assert_eq!(l2.len, 9);
    // L1 single = 1 + 9*3 = 28
    assert_eq!(l1.len, 28);
    // Total = 28 * 2 = 56
    assert_eq!(schema.lrecl_fixed, Some(56));
}

#[test]
fn test_occurs_preserves_child_offsets() {
    let cpy = r"
       01  REC.
           05  ARR OCCURS 5 TIMES.
               10  F1 PIC X(3).
               10  F2 PIC X(7).
    ";
    let schema = parse(cpy);
    // Within the OCCURS group, children have local offsets
    let f1 = find_field(&schema, "F1");
    let f2 = find_field(&schema, "F2");
    assert_eq!(f1.offset, 0);
    assert_eq!(f2.offset, 3);
}

#[test]
fn test_occurs_field_after_occurs_group() {
    let cpy = r"
       01  REC.
           05  REPEATS OCCURS 4 TIMES.
               10  R-DATA PIC X(10).
           05  TRAILER PIC X(5).
    ";
    let schema = parse(cpy);
    let trailer = find_field(&schema, "TRAILER");
    // TRAILER offset = 10 * 4 = 40
    assert_eq!(trailer.offset, 40);
    assert_eq!(schema.lrecl_fixed, Some(45));
}

#[test]
fn test_occurs_with_comp3_children() {
    let cpy = r"
       01  REC.
           05  AMOUNTS OCCURS 6 TIMES.
               10  AMT-TYPE PIC X(1).
               10  AMT-VAL  PIC S9(9)V99 COMP-3.
    ";
    let schema = parse(cpy);
    let amounts = find_field(&schema, "AMOUNTS");
    let amt_val = find_field(&schema, "AMT-VAL");
    assert!(matches!(
        amt_val.kind,
        FieldKind::PackedDecimal { digits: 11, scale: 2, signed: true }
    ));
    // S9(9)V99 COMP-3 = (11+1)/2 = 6 bytes
    assert_eq!(amt_val.len, 6);
    // Single = 1 + 6 = 7
    assert_eq!(amounts.len, 7);
    // Total = 7 * 6 = 42
    assert_eq!(schema.lrecl_fixed, Some(42));
}

#[test]
fn test_odo_with_indexed_by() {
    let cpy = r"
       01  REC.
           05  ITEM-CNT PIC 9(2).
           05  ITEMS OCCURS 1 TO 50
               DEPENDING ON ITEM-CNT
               INDEXED BY ITEM-IDX.
               10  ITEM-CODE PIC X(5).
               10  ITEM-QTY  PIC 9(4).
    ";
    let schema = parse(cpy);
    let items = find_field(&schema, "ITEMS");
    assert!(matches!(
        items.occurs,
        Some(Occurs::ODO { min: 1, max: 50, .. })
    ));
    assert!(items.is_group());
    assert_eq!(items.len, 9);
}
