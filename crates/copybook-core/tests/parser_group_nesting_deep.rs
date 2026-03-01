// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep group nesting tests for copybook-core parser.
//!
//! Validates multi-level nesting (up to 11 levels), FILLER-only groups,
//! empty groups (only subgroups), field count and LRECL calculations,
//! and mixed groups with OCCURS and elementary fields.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use copybook_core::{FieldKind, Occurs, ParseOptions, parse_copybook, parse_copybook_with_options};

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

fn parse_with_filler(text: &str) -> copybook_core::Schema {
    let opts = ParseOptions {
        emit_filler: true,
        ..ParseOptions::default()
    };
    parse_copybook_with_options(text, &opts).expect("parse failed")
}

// ===========================================================================
// 1. Deep nesting — multi-level hierarchy
// ===========================================================================

#[test]
fn test_deep_nesting_3_levels() {
    let cpy = r"
       01  REC.
           05  G1.
               10  F1 PIC X(5).
    ";
    let schema = parse(cpy);
    assert_eq!(schema.lrecl_fixed, Some(5));
    assert!(find_field(&schema, "G1").is_group());
    assert_eq!(find_field(&schema, "F1").offset, 0);
    assert_eq!(find_field(&schema, "F1").len, 5);
}

#[test]
fn test_deep_nesting_4_levels() {
    let cpy = r"
       01  REC.
           05  G1.
               10  G2.
                   15  F1 PIC X(8).
    ";
    let schema = parse(cpy);
    assert_eq!(schema.lrecl_fixed, Some(8));
    assert!(find_field(&schema, "G1").is_group());
    assert!(find_field(&schema, "G2").is_group());
    assert_eq!(find_field(&schema, "F1").len, 8);
}

#[test]
fn test_deep_nesting_5_levels() {
    let cpy = r"
       01  REC.
           05  G1.
               10  G2.
                   15  G3.
                       20  F1 PIC 9(4).
    ";
    let schema = parse(cpy);
    assert_eq!(schema.lrecl_fixed, Some(4));
    let g3 = find_field(&schema, "G3");
    assert!(g3.is_group());
    assert_eq!(g3.len, 4);
}

#[test]
fn test_deep_nesting_7_levels() {
    let cpy = r"
       01  REC.
           05  L1.
               10  L2.
                   15  L3.
                       20  L4.
                           25  L5.
                               30  LEAF PIC X(3).
    ";
    let schema = parse(cpy);
    assert_eq!(schema.lrecl_fixed, Some(3));

    // All intermediate levels are groups
    for name in &["L1", "L2", "L3", "L4", "L5"] {
        assert!(find_field(&schema, name).is_group(), "{name} should be group");
    }

    // Leaf is at the bottom
    assert_eq!(find_field(&schema, "LEAF").len, 3);
    assert_eq!(find_field(&schema, "LEAF").offset, 0);
}

#[test]
fn test_deep_nesting_11_levels() {
    // Levels: 01 → 05 → 10 → 15 → 20 → 25 → 30 → 35 → 40 → 45 → 49
    let cpy = r"
       01  REC.
           05  L05.
               10  L10.
                   15  L15.
                       20  L20.
                           25  L25.
                               30  L30.
                                   35  L35.
                                       40  L40.
                                           45  L45.
                                               49  DEEP-LEAF PIC X(10).
    ";
    let schema = parse(cpy);
    assert_eq!(schema.lrecl_fixed, Some(10));

    // Verify all group levels exist
    for name in &["L05", "L10", "L15", "L20", "L25", "L30", "L35", "L40", "L45"] {
        let f = find_field(&schema, name);
        assert!(f.is_group(), "{name} should be a group");
        assert_eq!(f.len, 10, "{name} should span the leaf");
    }

    assert_eq!(find_field(&schema, "DEEP-LEAF").len, 10);
}

#[test]
fn test_deep_nesting_with_sibling_at_each_level() {
    let cpy = r"
       01  REC.
           05  G1.
               10  G2.
                   15  INNER PIC X(5).
               10  SIB2 PIC X(3).
           05  SIB1 PIC X(7).
    ";
    let schema = parse(cpy);
    // LRECL = 5 + 3 + 7 = 15
    assert_eq!(schema.lrecl_fixed, Some(15));
    assert_eq!(find_field(&schema, "INNER").offset, 0);
    assert_eq!(find_field(&schema, "SIB2").offset, 5);
    assert_eq!(find_field(&schema, "SIB1").offset, 8);
}

#[test]
fn test_deep_nesting_multiple_branches() {
    let cpy = r"
       01  REC.
           05  BRANCH-A.
               10  A1 PIC X(4).
               10  A2 PIC X(6).
           05  BRANCH-B.
               10  B1.
                   15  B1A PIC 9(3).
                   15  B1B PIC 9(2).
               10  B2 PIC X(5).
    ";
    let schema = parse(cpy);
    // LRECL = 4 + 6 + 3 + 2 + 5 = 20
    assert_eq!(schema.lrecl_fixed, Some(20));
    assert_eq!(find_field(&schema, "BRANCH-A").len, 10);
    assert_eq!(find_field(&schema, "BRANCH-B").len, 10);
    assert_eq!(find_field(&schema, "B1").len, 5);
}

// ===========================================================================
// 2. Groups with only FILLER children
// ===========================================================================

#[test]
fn test_group_with_single_filler_child() {
    let cpy = r"
       01  REC.
           05  PADDING.
               10  FILLER PIC X(20).
    ";
    // emit_filler renames FILLER to _filler_* pattern
    let schema = parse_with_filler(cpy);
    let grp = find_field(&schema, "PADDING");
    assert!(grp.is_group());
    assert_eq!(grp.len, 20);
    assert_eq!(grp.children.len(), 1);
    assert!(grp.children[0].name.starts_with("_filler_"));
}

#[test]
fn test_group_with_multiple_filler_children() {
    let cpy = r"
       01  REC.
           05  GAP-BLOCK.
               10  FILLER PIC X(10).
               10  FILLER PIC X(5).
               10  FILLER PIC X(15).
    ";
    let schema = parse_with_filler(cpy);
    let grp = find_field(&schema, "GAP-BLOCK");
    assert!(grp.is_group());
    assert_eq!(grp.len, 30);
    assert_eq!(grp.children.len(), 3);
    for child in &grp.children {
        assert!(child.name.starts_with("_filler_"), "expected _filler_ prefix, got: {}", child.name);
    }
    assert_eq!(schema.lrecl_fixed, Some(30));
}

#[test]
fn test_filler_not_emitted_by_default() {
    let cpy = r"
       01  REC.
           05  A PIC X(5).
           05  FILLER PIC X(10).
           05  B PIC X(5).
    ";
    let schema = parse(cpy);
    // Default: FILLER fields keep name "FILLER" in schema (for offset calc)
    // but LRECL still accounts for them
    assert_eq!(schema.lrecl_fixed, Some(20));
}

#[test]
fn test_filler_emitted_with_option() {
    let cpy = r"
       01  REC.
           05  A PIC X(5).
           05  FILLER PIC X(10).
           05  B PIC X(5).
    ";
    let schema = parse_with_filler(cpy);
    let all = schema.all_fields();
    // emit_filler renames FILLER to _filler_* pattern
    assert!(all.iter().any(|f| f.name.starts_with("_filler_")));
    assert_eq!(schema.lrecl_fixed, Some(20));
}

// ===========================================================================
// 3. Empty groups (group with subgroups only, no direct elementary children)
// ===========================================================================

#[test]
fn test_group_with_only_subgroups() {
    let cpy = r"
       01  REC.
           05  CONTAINER.
               10  SUB-A.
                   15  FA PIC X(3).
               10  SUB-B.
                   15  FB PIC X(7).
    ";
    let schema = parse(cpy);
    let container = find_field(&schema, "CONTAINER");
    assert!(container.is_group());
    assert_eq!(container.len, 10);
    // CONTAINER has 2 children, both groups
    assert_eq!(container.children.len(), 2);
    for child in &container.children {
        assert!(child.is_group());
    }
    assert_eq!(schema.lrecl_fixed, Some(10));
}

#[test]
fn test_deeply_nested_groups_no_leaf_until_bottom() {
    let cpy = r"
       01  REC.
           05  A.
               10  B.
                   15  C.
                       20  D.
                           25  LEAF PIC 9(6).
    ";
    let schema = parse(cpy);
    // Every group spans the single leaf
    for name in &["A", "B", "C", "D"] {
        let g = find_field(&schema, name);
        assert!(g.is_group());
        assert_eq!(g.len, 6, "{name} len should be 6");
        assert_eq!(g.children.len(), 1, "{name} has one child");
    }
    assert_eq!(schema.lrecl_fixed, Some(6));
}

#[test]
fn test_group_containing_mix_of_subgroups_and_elementary() {
    let cpy = r"
       01  REC.
           05  MIXED.
               10  SUB-GRP.
                   15  INNER PIC X(4).
               10  PLAIN PIC X(6).
    ";
    let schema = parse(cpy);
    let mixed = find_field(&schema, "MIXED");
    assert!(mixed.is_group());
    assert_eq!(mixed.len, 10);
    assert_eq!(mixed.children.len(), 2);
    // First child is group, second is elementary
    assert!(mixed.children[0].is_group());
    assert!(!mixed.children[1].is_group());
}

// ===========================================================================
// 4. Field count and LRECL calculations
// ===========================================================================

#[test]
fn test_field_count_flat_record() {
    let cpy = r"
       01  REC.
           05  F1 PIC X(10).
           05  F2 PIC X(10).
           05  F3 PIC X(10).
           05  F4 PIC X(10).
           05  F5 PIC X(10).
    ";
    let schema = parse(cpy);
    // 1 (REC) + 5 leaf fields = 6 total
    assert_eq!(schema.all_fields().len(), 6);
    assert_eq!(schema.lrecl_fixed, Some(50));
}

#[test]
fn test_field_count_with_groups() {
    let cpy = r"
       01  REC.
           05  HEADER.
               10  H1 PIC X(4).
               10  H2 PIC X(4).
           05  BODY.
               10  B1 PIC X(8).
           05  TRAILER PIC X(4).
    ";
    let schema = parse(cpy);
    // REC, HEADER, H1, H2, BODY, B1, TRAILER = 7
    assert_eq!(schema.all_fields().len(), 7);
    assert_eq!(schema.lrecl_fixed, Some(20));
}

#[test]
fn test_lrecl_single_field() {
    let cpy = "       01  REC.\n           05  ONLY PIC X(256).\n";
    let schema = parse(cpy);
    assert_eq!(schema.lrecl_fixed, Some(256));
}

#[test]
fn test_lrecl_with_comp3_and_comp() {
    let cpy = r"
       01  REC.
           05  A PIC X(10).
           05  B PIC S9(5)V99 COMP-3.
           05  C PIC S9(4) COMP.
    ";
    let schema = parse(cpy);
    // X(10) = 10, S9(5)V99 COMP-3 = (7+1)/2 = 4 bytes, S9(4) COMP = 2 bytes
    assert_eq!(schema.lrecl_fixed, Some(16));
}

#[test]
fn test_lrecl_with_occurs() {
    let cpy = r"
       01  REC.
           05  HEADER PIC X(10).
           05  ITEMS OCCURS 5 TIMES.
               10  ITEM-ID PIC X(4).
               10  ITEM-AMT PIC 9(6).
           05  FOOTER PIC X(5).
    ";
    let schema = parse(cpy);
    // 10 + 5*(4+6) + 5 = 65
    assert_eq!(schema.lrecl_fixed, Some(65));
}

// ===========================================================================
// 5. Mixed groups with OCCURS and elementary fields
// ===========================================================================

#[test]
fn test_group_occurs_with_elementary_children() {
    let cpy = r"
       01  REC.
           05  ENTRIES OCCURS 3 TIMES.
               10  CODE PIC X(2).
               10  AMOUNT PIC 9(5).
    ";
    let schema = parse(cpy);
    let entries = find_field(&schema, "ENTRIES");
    assert!(entries.is_group());
    assert!(matches!(entries.occurs, Some(Occurs::Fixed { count: 3 })));
    // Single occurrence size = 2 + 5 = 7
    assert_eq!(entries.len, 7);
    // Effective length = 7 * 3 = 21
    assert_eq!(entries.effective_length(), 21);
    assert_eq!(schema.lrecl_fixed, Some(21));
}

#[test]
fn test_elementary_occurs() {
    let cpy = r"
       01  REC.
           05  TAGS PIC X(8) OCCURS 10 TIMES.
    ";
    let schema = parse(cpy);
    let tags = find_field(&schema, "TAGS");
    assert!(matches!(tags.occurs, Some(Occurs::Fixed { count: 10 })));
    assert_eq!(tags.len, 8);
    assert_eq!(tags.effective_length(), 80);
    assert_eq!(schema.lrecl_fixed, Some(80));
}

#[test]
fn test_occurs_group_with_nested_group() {
    let cpy = r"
       01  REC.
           05  LINE-ITEMS OCCURS 4 TIMES.
               10  ITEM-HDR.
                   15  ITEM-NUM PIC 9(3).
                   15  ITEM-TYPE PIC X(1).
               10  ITEM-AMOUNT PIC S9(7)V99 COMP-3.
    ";
    let schema = parse(cpy);
    let items = find_field(&schema, "LINE-ITEMS");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 4 })));
    let hdr = find_field(&schema, "ITEM-HDR");
    assert!(hdr.is_group());
    assert_eq!(hdr.len, 4);
    // COMP-3: S9(7)V99 = 9 digits + 1 sign = 5 bytes
    assert_eq!(find_field(&schema, "ITEM-AMOUNT").len, 5);
    // Single occurrence = 4 + 5 = 9; total = 9 * 4 = 36
    assert_eq!(schema.lrecl_fixed, Some(36));
}

#[test]
fn test_mixed_occurs_and_plain_fields() {
    let cpy = r"
       01  REC.
           05  PREFIX PIC X(2).
           05  ROWS OCCURS 2 TIMES.
               10  COL1 PIC X(5).
               10  COL2 PIC X(5).
           05  SUFFIX PIC X(3).
    ";
    let schema = parse(cpy);
    // 2 + 2*(5+5) + 3 = 25
    assert_eq!(schema.lrecl_fixed, Some(25));
    assert_eq!(find_field(&schema, "ROWS").effective_length(), 20);
}

#[test]
fn test_occurs_offset_calculation() {
    let cpy = r"
       01  REC.
           05  BEFORE PIC X(6).
           05  ARR PIC 9(4) OCCURS 3 TIMES.
           05  AFTER PIC X(2).
    ";
    let schema = parse(cpy);
    assert_eq!(find_field(&schema, "BEFORE").offset, 0);
    assert_eq!(find_field(&schema, "ARR").offset, 6);
    // AFTER starts at 6 + 4*3 = 18
    assert_eq!(find_field(&schema, "AFTER").offset, 18);
    assert_eq!(schema.lrecl_fixed, Some(20));
}

#[test]
fn test_group_occurs_offset_chain() {
    let cpy = r"
       01  REC.
           05  A PIC X(4).
           05  GRP OCCURS 2 TIMES.
               10  G1 PIC X(3).
               10  G2 PIC X(7).
           05  B PIC X(1).
    ";
    let schema = parse(cpy);
    assert_eq!(find_field(&schema, "A").offset, 0);
    assert_eq!(find_field(&schema, "GRP").offset, 4);
    // B offset = 4 + 2*(3+7) = 24
    assert_eq!(find_field(&schema, "B").offset, 24);
    assert_eq!(schema.lrecl_fixed, Some(25));
}

#[test]
fn test_deep_nesting_offset_propagation() {
    let cpy = r"
       01  REC.
           05  TOP PIC X(2).
           05  MID.
               10  INNER.
                   15  DEEP PIC X(8).
               10  AFTER-INNER PIC X(5).
           05  BOTTOM PIC X(3).
    ";
    let schema = parse(cpy);
    assert_eq!(find_field(&schema, "TOP").offset, 0);
    assert_eq!(find_field(&schema, "MID").offset, 2);
    assert_eq!(find_field(&schema, "INNER").offset, 2);
    assert_eq!(find_field(&schema, "DEEP").offset, 2);
    assert_eq!(find_field(&schema, "AFTER-INNER").offset, 10);
    assert_eq!(find_field(&schema, "BOTTOM").offset, 15);
    assert_eq!(schema.lrecl_fixed, Some(18));
}
