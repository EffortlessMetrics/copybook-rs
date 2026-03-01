// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Comprehensive layout resolution tests.
//!
//! Validates byte offset calculation, field sizing, LRECL computation,
//! alignment/padding, and edge cases for the layout resolution phase.

use copybook_core::feature_flags::{Feature, FeatureFlags};
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

fn enable_sign_separate() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::SignSeparate);
    FeatureFlags::set_global(flags);
}

fn enable_comp_float() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::Comp1);
    flags.enable(Feature::Comp2);
    FeatureFlags::set_global(flags);
}

// ===========================================================================
// 1. Offset calculation — simple sequential fields
// ===========================================================================

#[test]
fn test_offset_simple_sequential_fields() {
    let cpy = r"
       01 REC.
          05 F1 PIC X(5).
          05 F2 PIC X(10).
          05 F3 PIC X(15).
          05 F4 PIC X(20).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "F1").offset, 0);
    assert_eq!(find_field(&schema, "F2").offset, 5);
    assert_eq!(find_field(&schema, "F3").offset, 15);
    assert_eq!(find_field(&schema, "F4").offset, 30);
}

#[test]
fn test_offset_group_starts_at_first_child() {
    let cpy = r"
       01 REC.
          05 HEADER PIC X(8).
          05 BODY.
             10 FIELD-A PIC X(10).
             10 FIELD-B PIC X(20).
          05 TRAILER PIC X(4).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let body = find_field(&schema, "BODY");
    let field_a = find_field(&schema, "FIELD-A");
    assert_eq!(body.offset, 8, "group offset = first child offset");
    assert_eq!(field_a.offset, 8, "first child starts at group offset");
    assert_eq!(body.len, 30, "group len = sum of children");
}

#[test]
fn test_offset_nested_groups_reflect_hierarchy() {
    let cpy = r"
       01 REC.
          05 L1.
             10 L2.
                15 L3.
                   20 DEEP-LEAF PIC X(4).
                15 AFTER-L3 PIC X(6).
             10 AFTER-L2 PIC X(8).
          05 AFTER-L1 PIC X(2).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "L1").offset, 0);
    assert_eq!(find_field(&schema, "L2").offset, 0);
    assert_eq!(find_field(&schema, "L3").offset, 0);
    assert_eq!(find_field(&schema, "DEEP-LEAF").offset, 0);
    assert_eq!(find_field(&schema, "AFTER-L3").offset, 4);
    assert_eq!(find_field(&schema, "AFTER-L2").offset, 10);
    assert_eq!(find_field(&schema, "AFTER-L1").offset, 18);

    assert_eq!(find_field(&schema, "L3").len, 4, "L3 = DEEP-LEAF only");
    assert_eq!(
        find_field(&schema, "L2").len,
        10,
        "L2 = L3(4) + AFTER-L3(6)"
    );
    assert_eq!(
        find_field(&schema, "L1").len,
        18,
        "L1 = L2(10) + AFTER-L2(8)"
    );
}

#[test]
fn test_offset_redefines_same_as_original() {
    let cpy = r"
       01 REC.
          05 ALPHA PIC X(10).
          05 NUMERIC REDEFINES ALPHA PIC 9(10).
          05 NEXT PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "ALPHA").offset, 0);
    assert_eq!(
        find_field(&schema, "NUMERIC").offset,
        0,
        "REDEFINES shares offset"
    );
    assert_eq!(
        find_field(&schema, "NEXT").offset,
        10,
        "NEXT after original"
    );
}

#[test]
fn test_offset_filler_consumes_bytes() {
    let opts = ParseOptions {
        emit_filler: true,
        ..ParseOptions::default()
    };
    let cpy = r"
       01 REC.
          05 BEFORE PIC X(5).
          05 FILLER PIC X(10).
          05 AFTER  PIC X(3).
    ";
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();

    assert_eq!(find_field(&schema, "BEFORE").offset, 0);
    assert_eq!(
        find_field(&schema, "AFTER").offset,
        15,
        "after FILLER (5+10)"
    );
    assert_eq!(schema.lrecl_fixed, Some(18));
}

// ===========================================================================
// 2. Size calculation — PIC X(N), PIC 9(N)
// ===========================================================================

#[test]
fn test_size_pic_x() {
    let cpy = r"
       01 REC.
          05 F1 PIC X.
          05 F2 PIC X(1).
          05 F3 PIC X(50).
          05 F4 PIC X(255).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "F1").len, 1);
    assert_eq!(find_field(&schema, "F2").len, 1);
    assert_eq!(find_field(&schema, "F3").len, 50);
    assert_eq!(find_field(&schema, "F4").len, 255);
}

#[test]
fn test_size_pic_9() {
    let cpy = r"
       01 REC.
          05 F1 PIC 9.
          05 F2 PIC 9(1).
          05 F3 PIC 9(5).
          05 F4 PIC 9(18).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "F1").len, 1);
    assert_eq!(find_field(&schema, "F2").len, 1);
    assert_eq!(find_field(&schema, "F3").len, 5);
    assert_eq!(find_field(&schema, "F4").len, 18);
}

#[test]
fn test_size_pic_s9_overpunch() {
    // S9(N) without SIGN SEPARATE uses overpunch — no extra byte
    let cpy = r"
       01 REC.
          05 F1 PIC S9(5).
          05 F2 PIC S9(9).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(
        find_field(&schema, "F1").len,
        5,
        "overpunch: same as digit count"
    );
    assert_eq!(find_field(&schema, "F2").len, 9);
}

#[test]
fn test_size_sign_leading_separate() {
    enable_sign_separate();
    let cpy = r"
       01 REC.
          05 F1 PIC S9(5) SIGN IS LEADING SEPARATE.
          05 F2 PIC S9(9) SIGN IS LEADING SEPARATE.
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "F1").len, 6, "5 digits + 1 sign byte");
    assert_eq!(find_field(&schema, "F2").len, 10, "9 digits + 1 sign byte");
}

#[test]
fn test_size_sign_trailing_separate() {
    enable_sign_separate();
    let cpy = r"
       01 REC.
          05 F1 PIC S9(5) SIGN IS TRAILING SEPARATE.
          05 F2 PIC S9(7)V99 SIGN TRAILING SEPARATE.
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "F1").len, 6, "5 digits + 1 sign byte");
    assert_eq!(
        find_field(&schema, "F2").len,
        10,
        "9 digit positions + 1 sign byte"
    );
}

#[test]
fn test_size_comp3_packed_decimal() {
    // COMP-3: ceil((digits+1)/2) bytes
    let cpy = r"
       01 REC.
          05 P1 PIC 9(1) COMP-3.
          05 P2 PIC 9(2) COMP-3.
          05 P3 PIC 9(3) COMP-3.
          05 P4 PIC 9(4) COMP-3.
          05 P5 PIC S9(5) COMP-3.
          05 P7 PIC S9(7) COMP-3.
          05 P9 PIC S9(9) COMP-3.
          05 P15 PIC S9(15) COMP-3.
          05 P18 PIC S9(18) COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "P1").len, 1, "ceil((1+1)/2) = 1");
    assert_eq!(find_field(&schema, "P2").len, 2, "ceil((2+1)/2) = 2");
    assert_eq!(find_field(&schema, "P3").len, 2, "ceil((3+1)/2) = 2");
    assert_eq!(find_field(&schema, "P4").len, 3, "ceil((4+1)/2) = 3");
    assert_eq!(find_field(&schema, "P5").len, 3, "ceil((5+1)/2) = 3");
    assert_eq!(find_field(&schema, "P7").len, 4, "ceil((7+1)/2) = 4");
    assert_eq!(find_field(&schema, "P9").len, 5, "ceil((9+1)/2) = 5");
    assert_eq!(find_field(&schema, "P15").len, 8, "ceil((15+1)/2) = 8");
    assert_eq!(find_field(&schema, "P18").len, 10, "ceil((18+1)/2) = 10");
}

#[test]
fn test_size_comp_binary() {
    // COMP: 1-4 digits → 2 bytes, 5-9 → 4 bytes, 10-18 → 8 bytes
    let cpy = r"
       01 REC.
          05 B1  PIC 9(1) COMP.
          05 B4  PIC 9(4) COMP.
          05 B5  PIC 9(5) COMP.
          05 B9  PIC 9(9) COMP.
          05 B10 PIC 9(10) COMP.
          05 B18 PIC 9(18) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "B1").len, 2, "1 digit → 2 bytes");
    assert_eq!(find_field(&schema, "B4").len, 2, "4 digits → 2 bytes");
    assert_eq!(find_field(&schema, "B5").len, 4, "5 digits → 4 bytes");
    assert_eq!(find_field(&schema, "B9").len, 4, "9 digits → 4 bytes");
    assert_eq!(find_field(&schema, "B10").len, 8, "10 digits → 8 bytes");
    assert_eq!(find_field(&schema, "B18").len, 8, "18 digits → 8 bytes");
}

#[test]
fn test_size_comp1_float_single() {
    enable_comp_float();
    let cpy = "01 F1 COMP-1.";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(schema.fields[0].len, 4, "COMP-1 → 4 bytes");
    assert!(matches!(schema.fields[0].kind, FieldKind::FloatSingle));
}

#[test]
fn test_size_comp2_float_double() {
    enable_comp_float();
    let cpy = "01 F1 COMP-2.";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(schema.fields[0].len, 8, "COMP-2 → 8 bytes");
    assert!(matches!(schema.fields[0].kind, FieldKind::FloatDouble));
}

#[test]
fn test_size_implied_decimal_no_extra_bytes() {
    // V (implied decimal) should NOT add storage bytes
    let cpy = r"
       01 REC.
          05 F1 PIC 9(5)V99.
          05 F2 PIC S9(7)V999.
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "F1").len, 7, "5 + 2 digit positions");
    assert_eq!(
        find_field(&schema, "F2").len,
        10,
        "7 + 3 digit positions (overpunch sign)"
    );
}

#[test]
fn test_size_occurs_multiplies_element() {
    let cpy = r"
       01 REC.
          05 ITEMS PIC X(10) OCCURS 7 TIMES.
          05 AFTER PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let items = find_field(&schema, "ITEMS");
    assert_eq!(items.len, 10, "base element size");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 7 })));
    assert_eq!(find_field(&schema, "AFTER").offset, 70, "after 7 × 10");
}

// ===========================================================================
// 3. LRECL calculation
// ===========================================================================

#[test]
fn test_lrecl_fixed_record_sum_of_leaves() {
    let cpy = r"
       01 REC.
          05 A PIC X(10).
          05 B PIC 9(5).
          05 C PIC S9(3)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();

    // A=10, B=5, C=ceil((5+1)/2)=3
    assert_eq!(schema.lrecl_fixed, Some(18));
}

#[test]
fn test_lrecl_multiple_01_records_sequential() {
    // Multiple 01-level records without REDEFINES are laid out sequentially
    let cpy = r"
       01 REC-SMALL PIC X(10).
       01 REC-LARGE PIC X(50).
       01 REC-MEDIUM PIC X(30).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(
        schema.lrecl_fixed,
        Some(90),
        "10 + 50 + 30 = 90 (sequential)"
    );
}

#[test]
fn test_lrecl_odo_uses_max_count() {
    let cpy = r"
       01 REC.
          05 HEADER PIC X(4).
          05 CNT PIC 9(2).
          05 DATA OCCURS 1 TO 20 TIMES
                  DEPENDING ON CNT
                  PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();

    // Variable-length: lrecl_fixed is None
    assert!(schema.lrecl_fixed.is_none(), "ODO → no fixed LRECL");

    // Tail ODO should be detected
    let tail = schema
        .tail_odo
        .as_ref()
        .expect("tail_odo should be present");
    assert_eq!(tail.max_count, 20);
}

#[test]
fn test_lrecl_odo_fixed_when_min_equals_max() {
    // When min == max in ODO, effectively fixed-size
    let cpy = r"
       01 REC.
          05 CNT PIC 9(2).
          05 DATA OCCURS 5 TO 5 TIMES
                  DEPENDING ON CNT
                  PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();

    // min == max → effectively fixed-size
    assert_eq!(schema.lrecl_fixed, Some(42), "2 + (5 × 8) = 42");
}

#[test]
fn test_lrecl_redefines_not_double_counted() {
    let cpy = r"
       01 REC.
          05 FIELD-A PIC X(20).
          05 FIELD-B REDEFINES FIELD-A PIC X(20).
          05 FIELD-C PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(schema.lrecl_fixed, Some(30), "20 (overlaid) + 10");
}

#[test]
fn test_lrecl_redefines_larger_variant_extends() {
    let cpy = r"
       01 A PIC X(10).
       01 B REDEFINES A PIC X(25).
       01 C PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(schema.lrecl_fixed, Some(30), "max(10,25) + 5 = 30");
}

// ===========================================================================
// 4. Alignment / SYNCHRONIZED
// ===========================================================================

#[test]
fn test_sync_binary_halfword_alignment() {
    // PIC 9(1-4) COMP = 2 bytes → halfword (2-byte) alignment when SYNC
    let cpy = r"
       01 REC.
          05 CHAR1 PIC X(1).
          05 BIN2  PIC 9(3) COMP SYNCHRONIZED.
          05 CHAR2 PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let bin2 = find_field(&schema, "BIN2");
    assert_eq!(bin2.offset, 2, "aligned to 2-byte boundary from offset 1");
    assert_eq!(bin2.len, 2);
    assert_eq!(bin2.sync_padding, Some(1), "1 padding byte");
    assert!(bin2.synchronized);

    assert_eq!(find_field(&schema, "CHAR2").offset, 4);
}

#[test]
fn test_sync_binary_fullword_alignment() {
    // PIC 9(5-9) COMP = 4 bytes → fullword (4-byte) alignment when SYNC
    let cpy = r"
       01 REC.
          05 CHAR1 PIC X(1).
          05 BIN4  PIC 9(7) COMP SYNCHRONIZED.
          05 CHAR2 PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let bin4 = find_field(&schema, "BIN4");
    assert_eq!(bin4.offset, 4, "aligned to 4-byte boundary");
    assert_eq!(bin4.len, 4);
    assert_eq!(bin4.sync_padding, Some(3));

    assert_eq!(find_field(&schema, "CHAR2").offset, 8);
}

#[test]
fn test_sync_binary_doubleword_alignment() {
    // PIC 9(10-18) COMP = 8 bytes → doubleword (8-byte) alignment when SYNC
    let cpy = r"
       01 REC.
          05 CHAR1 PIC X(3).
          05 BIN8  PIC 9(15) COMP SYNCHRONIZED.
          05 CHAR2 PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let bin8 = find_field(&schema, "BIN8");
    assert_eq!(bin8.offset, 8, "aligned to 8-byte boundary");
    assert_eq!(bin8.len, 8);
    assert_eq!(bin8.sync_padding, Some(5));

    assert_eq!(find_field(&schema, "CHAR2").offset, 16);
}

#[test]
fn test_no_sync_no_padding() {
    // Without SYNCHRONIZED, binary fields have no alignment padding
    let cpy = r"
       01 REC.
          05 CHAR1 PIC X(1).
          05 BIN4  PIC 9(7) COMP.
          05 CHAR2 PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let bin4 = find_field(&schema, "BIN4");
    assert_eq!(bin4.offset, 1, "no padding without SYNCHRONIZED");
    assert!(bin4.sync_padding.is_none());
    assert!(!bin4.synchronized);

    assert_eq!(find_field(&schema, "CHAR2").offset, 5);
}

#[test]
fn test_sync_already_aligned_no_padding() {
    // When already on a natural boundary, no padding needed
    let cpy = r"
       01 REC.
          05 CHAR4 PIC X(4).
          05 BIN4  PIC 9(7) COMP SYNCHRONIZED.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let bin4 = find_field(&schema, "BIN4");
    assert_eq!(bin4.offset, 4, "already on 4-byte boundary");
    assert!(bin4.sync_padding.is_none(), "no padding needed");
}

#[test]
fn test_sync_multiple_fields_cumulative_padding() {
    let cpy = r"
       01 REC.
          05 C1   PIC X(1).
          05 BIN1 PIC 9(5) COMP SYNCHRONIZED.
          05 C2   PIC X(1).
          05 BIN2 PIC 9(9) COMP SYNCHRONIZED.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let bin1 = find_field(&schema, "BIN1");
    assert_eq!(bin1.offset, 4); // aligned from 1 to 4
    assert_eq!(bin1.sync_padding, Some(3));

    let c2 = find_field(&schema, "C2");
    assert_eq!(c2.offset, 8);

    let bin2 = find_field(&schema, "BIN2");
    assert_eq!(bin2.offset, 12); // aligned from 9 to 12
    assert_eq!(bin2.sync_padding, Some(3));
}

// ===========================================================================
// 5. REDEFINES — advanced scenarios
// ===========================================================================

#[test]
fn test_redefines_group_overlays_original_group() {
    let cpy = r"
       01 REC.
          05 DATE-GROUP.
             10 YYYY PIC 9(4).
             10 MM   PIC 9(2).
             10 DD   PIC 9(2).
          05 DATE-STR REDEFINES DATE-GROUP PIC X(8).
          05 AFTER PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let dg = find_field(&schema, "DATE-GROUP");
    let ds = find_field(&schema, "DATE-STR");
    assert_eq!(dg.offset, ds.offset);
    assert_eq!(dg.len, 8);
    assert_eq!(ds.len, 8);

    assert_eq!(find_field(&schema, "AFTER").offset, 8);
    assert_eq!(schema.lrecl_fixed, Some(13));
}

#[test]
fn test_redefines_multiple_variants_largest_wins() {
    let cpy = r"
       01 ORIG PIC X(20).
       01 V1 REDEFINES ORIG PIC X(10).
       01 V2 REDEFINES ORIG PIC X(30).
       01 V3 REDEFINES ORIG PIC X(15).
       01 NEXT PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    // All redefines at offset 0
    for i in 0..4 {
        assert_eq!(schema.fields[i].offset, 0, "field {} at offset 0", i);
    }

    // NEXT after the largest variant (30 bytes)
    assert_eq!(schema.fields[4].offset, 30);
    assert_eq!(schema.lrecl_fixed, Some(35));
}

// ===========================================================================
// 6. OCCURS — group arrays
// ===========================================================================

#[test]
fn test_occurs_group_total_size() {
    let cpy = r"
       01 REC.
          05 ROWS OCCURS 10 TIMES.
             10 COL-A PIC X(5).
             10 COL-B PIC 9(3).
          05 FOOTER PIC X(2).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let rows = find_field(&schema, "ROWS");
    assert_eq!(rows.len, 8, "base element = 5 + 3");
    assert!(matches!(rows.occurs, Some(Occurs::Fixed { count: 10 })));

    assert_eq!(find_field(&schema, "FOOTER").offset, 80, "10 × 8 = 80");
    assert_eq!(schema.lrecl_fixed, Some(82));
}

#[test]
fn test_occurs_nested_groups() {
    let cpy = r"
       01 REC.
          05 OUTER OCCURS 3 TIMES.
             10 INNER OCCURS 2 TIMES.
                15 LEAF PIC X(4).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let inner = find_field(&schema, "INNER");
    assert_eq!(inner.len, 4, "inner base element");

    let outer = find_field(&schema, "OUTER");
    assert_eq!(outer.len, 8, "outer base = 2 × 4 = 8");

    // Total: 3 × (2 × 4) = 24
    assert_eq!(schema.lrecl_fixed, Some(24));
}

// ===========================================================================
// 7. Mixed types — sequential offset verification
// ===========================================================================

#[test]
fn test_mixed_types_cumulative_offsets() {
    let cpy = r"
       01 REC.
          05 F-ALPHA    PIC X(10).
          05 F-ZONED    PIC 9(5).
          05 F-SZONED   PIC S9(7).
          05 F-PACKED   PIC S9(5)V99 COMP-3.
          05 F-BINARY   PIC 9(9) COMP.
          05 F-TAIL     PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "F-ALPHA").offset, 0);
    assert_eq!(find_field(&schema, "F-ALPHA").len, 10);

    assert_eq!(find_field(&schema, "F-ZONED").offset, 10);
    assert_eq!(find_field(&schema, "F-ZONED").len, 5);

    assert_eq!(find_field(&schema, "F-SZONED").offset, 15);
    assert_eq!(find_field(&schema, "F-SZONED").len, 7);

    assert_eq!(find_field(&schema, "F-PACKED").offset, 22);
    assert_eq!(find_field(&schema, "F-PACKED").len, 4); // ceil((7+1)/2) = 4

    assert_eq!(find_field(&schema, "F-BINARY").offset, 26);
    assert_eq!(find_field(&schema, "F-BINARY").len, 4); // 9 digits → 4 bytes

    assert_eq!(find_field(&schema, "F-TAIL").offset, 30);
    assert_eq!(schema.lrecl_fixed, Some(31));
}

// ===========================================================================
// 8. ODO — variable-length arrays
// ===========================================================================

#[test]
fn test_odo_tail_detected() {
    let cpy = r"
       01 REC.
          05 FIXED-PART PIC X(10).
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 0 TO 50 TIMES
                   DEPENDING ON ITEM-COUNT
                   PIC X(20).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let tail = schema.tail_odo.as_ref().expect("tail ODO");
    assert_eq!(tail.counter_path, "ITEM-COUNT");
    assert_eq!(tail.array_path, "ITEMS");
    assert_eq!(tail.min_count, 0);
    assert_eq!(tail.max_count, 50);

    assert!(
        schema.lrecl_fixed.is_none(),
        "variable-length → no fixed LRECL"
    );
}

#[test]
fn test_odo_lrecl_none_for_variable_length() {
    let cpy = r"
       01 REC.
          05 N PIC 9(2).
          05 ELEMS OCCURS 1 TO 99 TIMES
                   DEPENDING ON N
                   PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert!(schema.lrecl_fixed.is_none());
    assert!(schema.tail_odo.is_some());
}

// ===========================================================================
// 9. Edge cases
// ===========================================================================

#[test]
fn test_edge_level88_no_storage() {
    // Level-88 fields must not consume storage
    let cpy = r"
       01 REC.
          05 STATUS PIC X(1).
             88 ACTIVE   VALUE 'A'.
             88 INACTIVE VALUE 'I'.
          05 DATA PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "STATUS").len, 1);
    assert_eq!(
        find_field(&schema, "DATA").offset,
        1,
        "level-88 does not consume bytes"
    );
    assert_eq!(schema.lrecl_fixed, Some(11));
}

#[test]
fn test_edge_single_field_record() {
    let cpy = "01 ONLY-FIELD PIC X(100).";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(schema.fields[0].offset, 0);
    assert_eq!(schema.fields[0].len, 100);
    assert_eq!(schema.lrecl_fixed, Some(100));
}

#[test]
fn test_edge_very_large_record() {
    // Test record >10000 bytes
    let cpy = r"
       01 REC.
          05 BIG-FIELD PIC X(10000).
          05 EXTRA     PIC X(500).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "BIG-FIELD").len, 10000);
    assert_eq!(find_field(&schema, "EXTRA").offset, 10000);
    assert_eq!(schema.lrecl_fixed, Some(10500));
}

#[test]
fn test_edge_deep_nesting_7_levels() {
    let cpy = r"
       01 REC.
          05 L5.
             10 L10.
                15 L15.
                   20 L20.
                      25 L25.
                         30 L30.
                            35 DEEP PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let deep = find_field(&schema, "DEEP");
    assert_eq!(deep.offset, 0, "deep nested leaf at start");
    assert_eq!(deep.len, 8);

    // All group parents should have len=8
    assert_eq!(find_field(&schema, "L30").len, 8);
    assert_eq!(find_field(&schema, "L25").len, 8);
    assert_eq!(find_field(&schema, "L20").len, 8);
    assert_eq!(find_field(&schema, "L15").len, 8);
    assert_eq!(find_field(&schema, "L10").len, 8);
    assert_eq!(find_field(&schema, "L5").len, 8);

    assert_eq!(schema.lrecl_fixed, Some(8));
}

#[test]
fn test_edge_deep_nesting_with_siblings() {
    let cpy = r"
       01 REC.
          05 L5.
             10 L10.
                15 L15.
                   20 L20.
                      25 LEAF-A PIC X(3).
                      25 LEAF-B PIC X(5).
                   20 SIBLING PIC X(2).
                15 AFTER-L15 PIC X(4).
             10 AFTER-L10 PIC X(6).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "LEAF-A").offset, 0);
    assert_eq!(find_field(&schema, "LEAF-B").offset, 3);
    assert_eq!(find_field(&schema, "SIBLING").offset, 8);
    assert_eq!(find_field(&schema, "AFTER-L15").offset, 10);
    assert_eq!(find_field(&schema, "AFTER-L10").offset, 14);

    assert_eq!(
        find_field(&schema, "L20").len,
        8,
        "L20 = LEAF-A(3) + LEAF-B(5)"
    );
    assert_eq!(
        find_field(&schema, "L15").len,
        10,
        "L15 = L20(8) + SIBLING(2)"
    );
    assert_eq!(
        find_field(&schema, "L10").len,
        14,
        "L10 = L15(10) + AFTER-L15(4)"
    );
    assert_eq!(
        find_field(&schema, "L5").len,
        20,
        "L5 = L10(14) + AFTER-L10(6)"
    );

    assert_eq!(schema.lrecl_fixed, Some(20));
}

#[test]
fn test_edge_occurs_large_array() {
    let cpy = r"
       01 REC.
          05 HEADER PIC X(4).
          05 ITEMS PIC X(10) OCCURS 1000 TIMES.
          05 FOOTER PIC X(4).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "ITEMS").offset, 4);
    assert_eq!(find_field(&schema, "FOOTER").offset, 10004, "4 + 1000×10");
    assert_eq!(schema.lrecl_fixed, Some(10008));
}

#[test]
fn test_edge_comp3_with_implied_decimal() {
    // PIC S9(5)V99 COMP-3 = 7 digits → ceil((7+1)/2) = 4 bytes
    // Implied decimal does NOT add bytes
    let cpy = r"
       01 REC.
          05 AMT PIC S9(5)V99 COMP-3.
          05 AFTER PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "AMT").len, 4);
    assert_eq!(find_field(&schema, "AFTER").offset, 4);
}

#[test]
fn test_edge_multiple_fillers() {
    let opts = ParseOptions {
        emit_filler: true,
        ..ParseOptions::default()
    };
    let cpy = r"
       01 REC.
          05 F1     PIC X(5).
          05 FILLER PIC X(3).
          05 F2     PIC X(10).
          05 FILLER PIC X(7).
          05 F3     PIC X(2).
    ";
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();

    assert_eq!(find_field(&schema, "F1").offset, 0);
    assert_eq!(find_field(&schema, "F2").offset, 8);
    assert_eq!(find_field(&schema, "F3").offset, 25);
    assert_eq!(schema.lrecl_fixed, Some(27));
}

#[test]
fn test_edge_group_containing_only_groups() {
    let cpy = r"
       01 REC.
          05 OUTER.
             10 INNER-A.
                15 LEAF-A PIC X(5).
             10 INNER-B.
                15 LEAF-B PIC X(3).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "OUTER").offset, 0);
    assert_eq!(find_field(&schema, "OUTER").len, 8);
    assert_eq!(find_field(&schema, "INNER-A").offset, 0);
    assert_eq!(find_field(&schema, "INNER-A").len, 5);
    assert_eq!(find_field(&schema, "INNER-B").offset, 5);
    assert_eq!(find_field(&schema, "INNER-B").len, 3);
}

#[test]
fn test_edge_redefines_within_group() {
    // REDEFINES inside a group: both fields share the same offset
    let cpy = r"
       01 REC.
          05 HEADER PIC X(2).
          05 BODY.
             10 RAW-DATA PIC X(20).
             10 PARSED REDEFINES RAW-DATA.
                15 PART-A PIC X(10).
                15 PART-B PIC X(10).
          05 TRAILER PIC X(3).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let raw = find_field(&schema, "RAW-DATA");
    let parsed = find_field(&schema, "PARSED");
    assert_eq!(
        raw.offset, parsed.offset,
        "REDEFINES within group shares offset"
    );
    assert_eq!(raw.offset, 2);
    assert_eq!(raw.len, 20);
}

#[test]
fn test_edge_signed_comp_binary_sizes() {
    // Signed binary should have same byte widths
    let cpy = r"
       01 REC.
          05 SB2 PIC S9(4) COMP.
          05 SB4 PIC S9(9) COMP.
          05 SB8 PIC S9(18) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "SB2").len, 2);
    assert_eq!(find_field(&schema, "SB4").len, 4);
    assert_eq!(find_field(&schema, "SB8").len, 8);
}

#[test]
fn test_fingerprint_generated() {
    let cpy = r"
       01 REC.
          05 A PIC X(10).
          05 B PIC 9(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert!(!schema.fingerprint.is_empty(), "fingerprint should be set");
    assert_eq!(schema.fingerprint.len(), 64, "SHA-256 hex = 64 chars");
}

#[test]
fn test_root_group_length_equals_children_sum() {
    let cpy = r"
       01 REC.
          05 A PIC X(7).
          05 B PIC 9(3).
          05 C PIC X(11).
          05 D PIC S9(5) COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let root = &schema.fields[0];
    // A=7, B=3, C=11, D=ceil((5+1)/2)=3 → total=24
    assert_eq!(root.len, 24, "group len = sum of children");
    assert_eq!(schema.lrecl_fixed, Some(24));
}

#[test]
fn test_occurs_with_packed_elements() {
    let cpy = r"
       01 REC.
          05 AMOUNTS PIC S9(7)V99 COMP-3 OCCURS 5 TIMES.
          05 TOTAL   PIC S9(9)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let amounts = find_field(&schema, "AMOUNTS");
    assert_eq!(amounts.len, 5, "S9(7)V99 = 9 digits → ceil((9+1)/2) = 5");
    assert!(matches!(amounts.occurs, Some(Occurs::Fixed { count: 5 })));

    let total = find_field(&schema, "TOTAL");
    assert_eq!(total.offset, 25, "5 × 5 = 25");
    assert_eq!(total.len, 6, "S9(9)V99 = 11 digits → ceil((11+1)/2) = 6");

    assert_eq!(schema.lrecl_fixed, Some(31));
}
