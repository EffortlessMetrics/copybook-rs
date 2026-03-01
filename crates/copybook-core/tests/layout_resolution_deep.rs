// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Deep layout-resolution tests.
//!
//! Verifies that the parser's layout phase computes correct byte offsets,
//! field lengths, and structural properties for various COBOL constructs.

use copybook_core::{FieldKind, Occurs, ParseOptions, parse_copybook, parse_copybook_with_options};

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

fn find_field<'a>(schema: &'a copybook_core::Schema, name: &str) -> &'a copybook_core::Field {
    schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("field '{name}' not found in schema"))
}

fn enable_sign_separate() {
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::SignSeparate);
    FeatureFlags::set_global(flags);
}

// ===========================================================================
// 1. Simple flat layout — verify offsets
// ===========================================================================

#[test]
fn test_flat_layout_sequential_offsets() {
    let cpy = r"
       01 REC.
          05 F1 PIC X(10).
          05 F2 PIC 9(5).
          05 F3 PIC X(20).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f1 = find_field(&schema, "F1");
    let f2 = find_field(&schema, "F2");
    let f3 = find_field(&schema, "F3");

    assert_eq!(f1.offset, 0);
    assert_eq!(f1.len, 10);

    assert_eq!(f2.offset, 10);
    assert_eq!(f2.len, 5);

    assert_eq!(f3.offset, 15);
    assert_eq!(f3.len, 20);

    // LRECL should be total
    assert_eq!(schema.lrecl_fixed, Some(35));
}

#[test]
fn test_flat_layout_single_field_lrecl() {
    let cpy = "01 REC.\n   05 ONLY PIC X(100).";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(find_field(&schema, "ONLY").offset, 0);
    assert_eq!(schema.lrecl_fixed, Some(100));
}

// ===========================================================================
// 2. Nested group layout — group offsets encompass children
// ===========================================================================

#[test]
fn test_group_offset_and_length() {
    let cpy = r"
       01 REC.
          05 PREFIX PIC X(4).
          05 GRP.
             10 A PIC X(10).
             10 B PIC X(20).
          05 SUFFIX PIC X(6).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let prefix = find_field(&schema, "PREFIX");
    assert_eq!(prefix.offset, 0);
    assert_eq!(prefix.len, 4);

    let grp = find_field(&schema, "GRP");
    assert_eq!(grp.offset, 4, "group starts after PREFIX");
    assert_eq!(grp.len, 30, "group length = 10 + 20");
    assert!(matches!(grp.kind, FieldKind::Group));

    let a = find_field(&schema, "A");
    assert_eq!(a.offset, 4, "first child shares group offset");

    let b = find_field(&schema, "B");
    assert_eq!(b.offset, 14, "second child at 4 + 10");

    let suffix = find_field(&schema, "SUFFIX");
    assert_eq!(suffix.offset, 34, "SUFFIX at 4 + 30");

    assert_eq!(schema.lrecl_fixed, Some(40));
}

#[test]
fn test_multi_level_nesting_offsets() {
    let cpy = r"
       01 REC.
          05 OUTER.
             10 INNER.
                15 LEAF-A PIC X(3).
                15 LEAF-B PIC X(7).
             10 AFTER-INNER PIC X(5).
          05 TAIL PIC X(2).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let outer = find_field(&schema, "OUTER");
    assert_eq!(outer.offset, 0);
    assert_eq!(outer.len, 15, "3 + 7 + 5 = 15");

    let inner = find_field(&schema, "INNER");
    assert_eq!(inner.offset, 0);
    assert_eq!(inner.len, 10, "3 + 7 = 10");

    let leaf_a = find_field(&schema, "LEAF-A");
    assert_eq!(leaf_a.offset, 0);
    let leaf_b = find_field(&schema, "LEAF-B");
    assert_eq!(leaf_b.offset, 3);

    let after = find_field(&schema, "AFTER-INNER");
    assert_eq!(after.offset, 10);

    let tail = find_field(&schema, "TAIL");
    assert_eq!(tail.offset, 15);

    assert_eq!(schema.lrecl_fixed, Some(17));
}

// ===========================================================================
// 3. REDEFINES layout — redefining field shares offset
// ===========================================================================

#[test]
fn test_redefines_shares_offset() {
    let cpy = r"
       01 REC.
          05 HEADER PIC X(4).
          05 ORIG   PIC X(10).
          05 ALT    REDEFINES ORIG PIC 9(10).
          05 TAIL   PIC X(6).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let orig = find_field(&schema, "ORIG");
    let alt = find_field(&schema, "ALT");
    assert_eq!(orig.offset, 4);
    assert_eq!(alt.offset, 4, "REDEFINES must share offset");
    assert_eq!(orig.len, 10);
    assert_eq!(alt.len, 10);

    let tail = find_field(&schema, "TAIL");
    assert_eq!(
        tail.offset, 14,
        "TAIL after original field, not double-counted"
    );
}

#[test]
fn test_redefines_different_sizes_uses_largest() {
    let cpy = r"
       01 ORIG PIC X(20).
       01 SHORT-REDEF REDEFINES ORIG PIC 9(10).
       01 LONG-REDEF REDEFINES ORIG PIC X(30).
       01 NEXT PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(schema.fields[0].offset, 0);
    assert_eq!(schema.fields[1].offset, 0, "SHORT-REDEF at same offset");
    assert_eq!(schema.fields[2].offset, 0, "LONG-REDEF at same offset");

    let next = &schema.fields[3];
    assert_eq!(next.offset, 30, "NEXT after largest variant (30 bytes)");
}

#[test]
fn test_redefines_group_overlays_original() {
    let cpy = r"
       01 REC.
          05 DATE-GROUP.
             10 YYYY PIC 9(4).
             10 MM   PIC 9(2).
             10 DD   PIC 9(2).
          05 DATE-STR REDEFINES DATE-GROUP PIC X(8).
          05 AFTER PIC X(2).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let dg = find_field(&schema, "DATE-GROUP");
    let ds = find_field(&schema, "DATE-STR");
    assert_eq!(dg.offset, ds.offset, "REDEFINES shares offset with group");
    assert_eq!(dg.len, 8);
    assert_eq!(ds.len, 8);

    let after = find_field(&schema, "AFTER");
    assert_eq!(after.offset, 8);
}

// ===========================================================================
// 4. OCCURS — verify array element offsets
// ===========================================================================

#[test]
fn test_occurs_fixed_total_length() {
    let cpy = r"
       01 REC.
          05 PREFIX PIC X(2).
          05 ITEMS  PIC X(10) OCCURS 5 TIMES.
          05 SUFFIX PIC X(3).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let items = find_field(&schema, "ITEMS");
    assert_eq!(items.offset, 2);
    assert_eq!(items.len, 10, "base element length is 10");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 5 })));

    let suffix = find_field(&schema, "SUFFIX");
    // SUFFIX starts after 2 + (10 * 5) = 52
    assert_eq!(suffix.offset, 52);

    assert_eq!(schema.lrecl_fixed, Some(55)); // 2 + 50 + 3
}

#[test]
fn test_occurs_group_with_children_total_length() {
    let cpy = r"
       01 REC.
          05 ENTRIES OCCURS 3 TIMES.
             10 KEY  PIC X(4).
             10 VAL  PIC X(6).
          05 FOOTER PIC X(2).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let entries = find_field(&schema, "ENTRIES");
    assert_eq!(entries.len, 10, "base element = 4 + 6");
    assert!(matches!(entries.occurs, Some(Occurs::Fixed { count: 3 })));

    let footer = find_field(&schema, "FOOTER");
    assert_eq!(footer.offset, 30, "after 3 * 10 = 30");

    assert_eq!(schema.lrecl_fixed, Some(32));
}

// ===========================================================================
// 5. ODO — verify variable-length layout
// ===========================================================================

#[test]
fn test_odo_tail_produces_variable_layout() {
    let cpy = r"
       01 REC.
          05 HDR       PIC X(4).
          05 CNT       PIC 9(2).
          05 ITEMS     OCCURS 1 TO 10 TIMES
                       DEPENDING ON CNT
                       PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let hdr = find_field(&schema, "HDR");
    assert_eq!(hdr.offset, 0);
    assert_eq!(hdr.len, 4);

    let cnt = find_field(&schema, "CNT");
    assert_eq!(cnt.offset, 4);
    assert_eq!(cnt.len, 2);

    let items = find_field(&schema, "ITEMS");
    assert_eq!(items.offset, 6);
    assert_eq!(items.len, 8, "base element size");

    // tail_odo should be set
    let tail = schema.tail_odo.as_ref().expect("tail_odo should be set");
    assert_eq!(tail.counter_path, "CNT");
    assert_eq!(tail.min_count, 1);
    assert_eq!(tail.max_count, 10);
}

#[test]
fn test_odo_min_zero() {
    let cpy = r"
       01 REC.
          05 N    PIC 9(3).
          05 DATA OCCURS 0 TO 100 TIMES
                  DEPENDING ON N
                  PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let tail = schema.tail_odo.as_ref().expect("tail_odo should be set");
    assert_eq!(tail.min_count, 0);
    assert_eq!(tail.max_count, 100);
}

// ===========================================================================
// 6. Mixed COMP types — verify correct byte widths
// ===========================================================================

#[test]
fn test_comp3_byte_widths() {
    // COMP-3 formula: ceil((digits+1)/2)
    let cpy = r"
       01 REC.
          05 P1 PIC 9(1)   COMP-3.
          05 P3 PIC 9(3)   COMP-3.
          05 P5 PIC 9(5)   COMP-3.
          05 P7 PIC S9(7)  COMP-3.
          05 P9 PIC S9(7)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let p1 = find_field(&schema, "P1");
    assert_eq!(p1.len, 1, "ceil((1+1)/2) = 1");

    let p3 = find_field(&schema, "P3");
    assert_eq!(p3.len, 2, "ceil((3+1)/2) = 2");

    let p5 = find_field(&schema, "P5");
    assert_eq!(p5.len, 3, "ceil((5+1)/2) = 3");

    let p7 = find_field(&schema, "P7");
    assert_eq!(p7.len, 4, "ceil((7+1)/2) = 4");

    let p9 = find_field(&schema, "P9");
    // S9(7)V99 = 9 digits → ceil((9+1)/2) = 5
    assert_eq!(p9.len, 5, "ceil((9+1)/2) = 5");
}

#[test]
fn test_comp_binary_byte_widths() {
    // COMP: 1-4 digits → 2 bytes, 5-9 → 4 bytes, 10-18 → 8 bytes
    let cpy = r"
       01 REC.
          05 B2  PIC 9(1)  COMP.
          05 B4A PIC 9(4)  COMP.
          05 B4B PIC 9(5)  COMP.
          05 B4C PIC 9(9)  COMP.
          05 B8A PIC 9(10) COMP.
          05 B8B PIC 9(18) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "B2").len, 2, "1 digit → 2 bytes");
    assert_eq!(find_field(&schema, "B4A").len, 2, "4 digits → 2 bytes");
    assert_eq!(find_field(&schema, "B4B").len, 4, "5 digits → 4 bytes");
    assert_eq!(find_field(&schema, "B4C").len, 4, "9 digits → 4 bytes");
    assert_eq!(find_field(&schema, "B8A").len, 8, "10 digits → 8 bytes");
    assert_eq!(find_field(&schema, "B8B").len, 8, "18 digits → 8 bytes");
}

#[test]
fn test_mixed_comp_types_sequential_offsets() {
    let cpy = r"
       01 REC.
          05 F-ALPHA   PIC X(10).
          05 F-ZONED   PIC 9(5).
          05 F-PACKED  PIC S9(5)V99 COMP-3.
          05 F-BINARY  PIC 9(9) COMP.
          05 F-TAIL    PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let alpha = find_field(&schema, "F-ALPHA");
    assert_eq!(alpha.offset, 0);
    assert_eq!(alpha.len, 10);

    let zoned = find_field(&schema, "F-ZONED");
    assert_eq!(zoned.offset, 10);
    assert_eq!(zoned.len, 5);

    let packed = find_field(&schema, "F-PACKED");
    assert_eq!(packed.offset, 15);
    // S9(5)V99 = 7 digits → ceil((7+1)/2) = 4
    assert_eq!(packed.len, 4);

    let binary = find_field(&schema, "F-BINARY");
    assert_eq!(binary.offset, 19);
    assert_eq!(binary.len, 4); // 9 digits → 4 bytes

    let tail = find_field(&schema, "F-TAIL");
    assert_eq!(tail.offset, 23);
    assert_eq!(tail.len, 1);

    assert_eq!(schema.lrecl_fixed, Some(24));
}

// ===========================================================================
// 7. FILLER field naming convention
// ===========================================================================

#[test]
fn test_filler_naming_convention() {
    let opts = ParseOptions {
        emit_filler: true,
        ..ParseOptions::default()
    };
    let cpy = r"
       01 REC.
          05 F1     PIC X(5).
          05 FILLER PIC X(3).
          05 F2     PIC X(5).
    ";
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();
    let root = &schema.fields[0];

    // Find the filler field
    let filler = root
        .children
        .iter()
        .find(|c| c.name.starts_with("_filler_") || c.name == "FILLER")
        .expect("should find a FILLER/filler field");
    assert_eq!(filler.len, 3, "FILLER should be 3 bytes");
    assert_eq!(filler.offset, 5, "FILLER starts at offset 5");
}

#[test]
fn test_filler_does_not_affect_subsequent_offsets() {
    let opts = ParseOptions {
        emit_filler: true,
        ..ParseOptions::default()
    };
    let cpy = r"
       01 REC.
          05 A      PIC X(10).
          05 FILLER PIC X(5).
          05 B      PIC X(10).
    ";
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();
    let b = find_field(&schema, "B");
    assert_eq!(b.offset, 15, "B at 10 + 5 (FILLER occupies storage)");
    assert_eq!(schema.lrecl_fixed, Some(25));
}

// ===========================================================================
// 8. SIGN SEPARATE adds 1 byte to field length
// ===========================================================================

#[test]
fn test_sign_separate_leading_length() {
    enable_sign_separate();
    let cpy = r"
       01 REC.
          05 AMT PIC S9(5) SIGN IS LEADING SEPARATE.
          05 NEXT PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let amt = find_field(&schema, "AMT");
    assert_eq!(amt.len, 6, "5 digits + 1 sign byte");
    let next = find_field(&schema, "NEXT");
    assert_eq!(next.offset, 6, "next field starts after 6 bytes");
}

#[test]
fn test_sign_separate_trailing_length() {
    enable_sign_separate();
    let cpy = r"
       01 REC.
          05 BAL PIC S9(7)V99 SIGN TRAILING SEPARATE.
          05 NEXT PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let bal = find_field(&schema, "BAL");
    // S9(7)V99 = 9 digit positions + 1 sign byte = 10
    assert_eq!(bal.len, 10, "9 digits + 1 sign byte");
    let next = find_field(&schema, "NEXT");
    assert_eq!(next.offset, 10);
}

#[test]
fn test_sign_separate_versus_embedded_sign() {
    enable_sign_separate();
    let cpy = r"
       01 REC.
          05 EMBEDDED   PIC S9(5).
          05 SEPARATED  PIC S9(5) SIGN LEADING SEPARATE.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let emb = find_field(&schema, "EMBEDDED");
    let sep = find_field(&schema, "SEPARATED");

    assert_eq!(emb.len, 5, "embedded sign: no extra byte");
    assert_eq!(sep.len, 6, "separate sign: +1 byte");
    assert_eq!(sep.offset, 5, "SEPARATED starts after 5-byte EMBEDDED");
}

// ===========================================================================
// Additional layout edge cases
// ===========================================================================

#[test]
fn test_lrecl_with_redefines_does_not_double_count() {
    let cpy = r"
       01 REC.
          05 A PIC X(10).
          05 B REDEFINES A PIC X(10).
          05 C PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(
        schema.lrecl_fixed,
        Some(15),
        "10 (with redefines overlay) + 5"
    );
}

#[test]
fn test_root_group_length_equals_sum_of_children() {
    let cpy = r"
       01 REC.
          05 A PIC X(3).
          05 B PIC 9(5).
          05 C PIC X(12).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.len, 20, "group len = 3 + 5 + 12");
}

#[test]
fn test_occurs_group_does_not_inflate_base_len() {
    // The base element length of a group with OCCURS should be the sum of children,
    // not multiplied by count.
    let cpy = r"
       01 REC.
          05 ROWS OCCURS 4 TIMES.
             10 COL-A PIC X(3).
             10 COL-B PIC X(7).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let rows = find_field(&schema, "ROWS");
    assert_eq!(rows.len, 10, "base element = 3 + 7 = 10");
    assert!(matches!(rows.occurs, Some(Occurs::Fixed { count: 4 })));

    // Total LRECL should account for all occurrences
    assert_eq!(schema.lrecl_fixed, Some(40), "4 * 10 = 40");
}

#[test]
fn test_empty_group_zero_length() {
    // A group with no elementary children that resolve to storage should be zero-length
    // (or just act as a container). Testing with level-88 children only.
    let cpy = r"
       01 REC.
          05 FLAG PIC X(1).
             88 ON-VAL  VALUE 'Y'.
             88 OFF-VAL VALUE 'N'.
          05 DATA PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    // 88-level fields are children of FLAG but consume no storage
    let flag = find_field(&schema, "FLAG");
    assert_eq!(flag.len, 1);
    let data = find_field(&schema, "DATA");
    assert_eq!(data.offset, 1);
    assert_eq!(schema.lrecl_fixed, Some(11));
}
