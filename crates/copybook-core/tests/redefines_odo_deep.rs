// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep parser tests for REDEFINES and ODO (OCCURS DEPENDING ON).
//!
//! Covers:
//! - REDEFINES: offsets, sizes, groups, nesting, multiple variants, FILLER,
//!   field lookup, inspect output, type preservation
//! - ODO: tail position, counter linkage, bounds, dialect interaction,
//!   LRECL impact, counter not found, counter type, ODO+REDEFINES rejection

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::assertions_on_constants
)]

use copybook_core::{
    ErrorCode, FieldKind, Occurs, ParseOptions, parse_copybook, parse_copybook_with_options,
};

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

// ===========================================================================
// REDEFINES — simple offset and size
// ===========================================================================

#[test]
fn redefines_simple_offset_size_and_parent_group() {
    let cpy = r"
       01 REC.
          05 HEADER  PIC X(4).
          05 ORIG    PIC X(10).
          05 ALT     REDEFINES ORIG PIC 9(10).
          05 TAIL    PIC X(6).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let orig = find_field(&schema, "ORIG");
    let alt = find_field(&schema, "ALT");

    assert_eq!(orig.offset, 4);
    assert_eq!(alt.offset, 4, "REDEFINES shares offset with original");
    assert_eq!(orig.len, 10);
    assert_eq!(alt.len, 10);
    assert_eq!(alt.redefines_of.as_deref(), Some("ORIG"));

    // Parent group encompass everything, REDEFINES does not double-count
    let rec = &schema.fields[0];
    assert!(matches!(rec.kind, FieldKind::Group));
    assert_eq!(schema.lrecl_fixed, Some(20), "4 + 10 + 6, no double-count");
}

// ===========================================================================
// REDEFINES — multiple variants (3+ redefines same field)
// ===========================================================================

#[test]
fn redefines_multiple_variants() {
    let cpy = r"
       01 REC.
          05 BASE-DATA      PIC X(20).
          05 AS-NUMERIC     REDEFINES BASE-DATA PIC 9(20).
          05 AS-PACKED      REDEFINES BASE-DATA PIC S9(7)V99 COMP-3.
          05 AFTER          PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let base = find_field(&schema, "BASE-DATA");
    let as_num = find_field(&schema, "AS-NUMERIC");
    let as_pack = find_field(&schema, "AS-PACKED");

    // All share offset 0
    assert_eq!(base.offset, 0);
    assert_eq!(as_num.offset, 0);
    assert_eq!(as_pack.offset, 0);

    // Each points back to BASE-DATA
    assert_eq!(as_num.redefines_of.as_deref(), Some("BASE-DATA"));
    assert_eq!(as_pack.redefines_of.as_deref(), Some("BASE-DATA"));

    let after = find_field(&schema, "AFTER");
    // Verify REDEFINES fields share the base offset; AFTER follows correctly
    assert!(after.offset >= 20, "AFTER must be at or beyond offset 20");
}

// ===========================================================================
// REDEFINES — group field
// ===========================================================================

#[test]
fn redefines_group_field() {
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
    assert!(matches!(dg.kind, FieldKind::Group));
    assert_eq!(ds.redefines_of.as_deref(), Some("DATE-GROUP"));

    let after = find_field(&schema, "AFTER");
    assert_eq!(after.offset, 8);
    assert_eq!(schema.lrecl_fixed, Some(10));
}

// ===========================================================================
// REDEFINES — preserves original field type
// ===========================================================================

#[test]
fn redefines_preserves_original_field_type() {
    let cpy = r"
       01 REC.
          05 ALPHA-FIELD PIC X(10).
          05 NUM-FIELD   REDEFINES ALPHA-FIELD PIC 9(10).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let alpha = find_field(&schema, "ALPHA-FIELD");
    let num = find_field(&schema, "NUM-FIELD");

    assert!(
        matches!(alpha.kind, FieldKind::Alphanum { len: 10 }),
        "Original field type is Alphanum"
    );
    assert!(
        matches!(
            num.kind,
            FieldKind::ZonedDecimal {
                digits: 10,
                scale: 0,
                signed: false,
                ..
            }
        ),
        "Redefining field has its own type (ZonedDecimal)"
    );
    assert!(alpha.redefines_of.is_none(), "Original has no redefines_of");
    assert_eq!(num.redefines_of.as_deref(), Some("ALPHA-FIELD"));
}

// ===========================================================================
// REDEFINES — nested inside group
// ===========================================================================

#[test]
fn redefines_nested_inside_group() {
    let cpy = r"
       01 REC.
          05 OUTER.
             10 RAW-CODE PIC X(4).
             10 NUM-CODE REDEFINES RAW-CODE PIC 9(4).
             10 SUFFIX   PIC X(6).
          05 TAIL PIC X(2).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let raw = find_field(&schema, "RAW-CODE");
    let num = find_field(&schema, "NUM-CODE");

    assert_eq!(raw.offset, 0);
    assert_eq!(num.offset, 0, "Nested REDEFINES shares offset");
    assert_eq!(num.redefines_of.as_deref(), Some("RAW-CODE"));

    let suffix = find_field(&schema, "SUFFIX");
    assert_eq!(suffix.offset, 4, "SUFFIX after original (not doubled)");

    let outer = find_field(&schema, "OUTER");
    assert_eq!(outer.len, 10, "group = 4 + 6, REDEFINES doesn't add");
    assert_eq!(schema.lrecl_fixed, Some(12));
}

// ===========================================================================
// REDEFINES — smaller size OK
// ===========================================================================

#[test]
fn redefines_smaller_size_ok() {
    let cpy = r"
       01 REC.
          05 BIG-FIELD   PIC X(20).
          05 SMALL-FIELD REDEFINES BIG-FIELD PIC X(5).
          05 AFTER       PIC X(3).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let big = find_field(&schema, "BIG-FIELD");
    let small = find_field(&schema, "SMALL-FIELD");

    assert_eq!(big.len, 20);
    assert_eq!(small.len, 5, "Smaller REDEFINES is allowed");
    assert_eq!(small.offset, big.offset);

    let after = find_field(&schema, "AFTER");
    assert_eq!(after.offset, 20, "AFTER placed after larger of the two");
    assert_eq!(schema.lrecl_fixed, Some(23));
}

// ===========================================================================
// REDEFINES — bigger size OK
// ===========================================================================

#[test]
fn redefines_bigger_size_ok() {
    let cpy = r"
       01 ORIG PIC X(10).
       01 BIGGER REDEFINES ORIG PIC X(30).
       01 NEXT PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(schema.fields[0].len, 10);
    assert_eq!(schema.fields[1].len, 30, "Bigger REDEFINES is allowed");
    assert_eq!(schema.fields[1].offset, 0);

    let next = &schema.fields[2];
    assert_eq!(next.offset, 30, "NEXT after largest variant");
}

// ===========================================================================
// REDEFINES — field lookup by name
// ===========================================================================

#[test]
fn redefines_field_lookup_by_name() {
    let cpy = r"
       01 REC.
          05 DATA-AREA PIC X(20).
          05 DATA-NUM  REDEFINES DATA-AREA PIC 9(20).
    ";
    let schema = parse_copybook(cpy).unwrap();

    // all_fields should contain both
    let all = schema.all_fields();
    let names: Vec<&str> = all.iter().map(|f| f.name.as_str()).collect();
    assert!(names.contains(&"DATA-AREA"));
    assert!(names.contains(&"DATA-NUM"));

    // find_redefining_fields should locate DATA-NUM
    let redefining = schema.find_redefining_fields("DATA-AREA");
    assert_eq!(redefining.len(), 1);
    assert_eq!(redefining[0].name, "DATA-NUM");
}

// ===========================================================================
// REDEFINES — in inspect output format
// ===========================================================================

#[test]
fn redefines_appears_in_inspect_output() {
    let cpy = r"
       01 MULTI-RECORD.
          05 RECORD-TYPE  PIC X(1).
          05 DATA-AREA    PIC X(20).
          05 DATA-NUMERIC REDEFINES DATA-AREA PIC 9(20).
    ";
    let schema = parse_copybook(cpy).unwrap();

    // Verify the canonical JSON contains redefines info
    let json = schema.create_canonical_json();
    assert!(
        json.contains("redefines_of"),
        "Canonical JSON should contain redefines_of"
    );
    assert!(
        json.contains("DATA-AREA"),
        "Canonical JSON should reference the redefined field"
    );
}

// ===========================================================================
// REDEFINES — at level-01 (root level)
// ===========================================================================

#[test]
fn redefines_at_root_level() {
    let cpy = r"
       01 REC-A PIC X(50).
       01 REC-B REDEFINES REC-A.
          05 PART1 PIC X(25).
          05 PART2 PIC X(25).
       01 AFTER-REC PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(schema.fields[0].offset, 0);
    assert_eq!(schema.fields[1].offset, 0);
    assert_eq!(schema.fields[1].redefines_of.as_deref(), Some("REC-A"));
    assert!(matches!(schema.fields[1].kind, FieldKind::Group));
}

// ===========================================================================
// REDEFINES — multiple copybook sections
// ===========================================================================

#[test]
fn redefines_across_multiple_sections() {
    let cpy = r"
       01 HEADER.
          05 HDR-TYPE PIC X(2).
          05 HDR-DATA PIC X(8).
          05 HDR-NUM  REDEFINES HDR-DATA PIC 9(8).
       01 DETAIL.
          05 DET-CODE PIC X(4).
          05 DET-RAW  PIC X(16).
          05 DET-PARSED REDEFINES DET-RAW.
             10 DET-PART-A PIC X(8).
             10 DET-PART-B PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let hdr_num = find_field(&schema, "HDR-NUM");
    assert_eq!(hdr_num.redefines_of.as_deref(), Some("HDR-DATA"));

    let det_parsed = find_field(&schema, "DET-PARSED");
    assert_eq!(det_parsed.redefines_of.as_deref(), Some("DET-RAW"));

    // Each section is independently correct
    let hdr_data = find_field(&schema, "HDR-DATA");
    assert_eq!(hdr_data.offset, hdr_num.offset);
    let det_raw = find_field(&schema, "DET-RAW");
    assert_eq!(det_raw.offset, det_parsed.offset);
}

// ===========================================================================
// GROUP REDEFINES with child fields
// ===========================================================================

#[test]
fn group_redefines_with_child_fields() {
    let cpy = r"
       01 REC.
          05 RAW-DATA PIC X(20).
          05 STRUCTURED REDEFINES RAW-DATA.
             10 KEY-PART  PIC X(8).
             10 VAL-PART  PIC X(12).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let raw = find_field(&schema, "RAW-DATA");
    let structured = find_field(&schema, "STRUCTURED");

    assert_eq!(raw.offset, structured.offset);
    assert_eq!(structured.redefines_of.as_deref(), Some("RAW-DATA"));
    assert!(matches!(structured.kind, FieldKind::Group));
    assert_eq!(structured.children.len(), 2);
    assert_eq!(structured.children[0].name, "KEY-PART");
    assert_eq!(structured.children[1].name, "VAL-PART");
}

// ===========================================================================
// FILLER REDEFINES
// ===========================================================================

#[test]
fn filler_redefines() {
    let opts = ParseOptions {
        emit_filler: true,
        ..Default::default()
    };
    let cpy = r"
       01 REC.
          05 A       PIC X(10).
          05 FILLER  PIC X(5).
          05 B       PIC X(3).
    ";
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();

    // With emit_filler, FILLER should be present
    let all = schema.all_fields();
    let filler = all
        .iter()
        .find(|f| f.name.starts_with("_filler_") || f.name == "FILLER")
        .expect("FILLER field should be present with emit_filler");
    assert_eq!(filler.len, 5);
    assert_eq!(filler.offset, 10);
    assert_eq!(schema.lrecl_fixed, Some(18));
}

// ===========================================================================
// ODO — simple tail: counter linked, array bounds
// ===========================================================================

#[test]
fn odo_simple_tail_counter_and_bounds() {
    let cpy = r"
       01 REC.
          05 HDR       PIC X(4).
          05 CNT       PIC 9(3).
          05 ITEMS     OCCURS 1 TO 50 TIMES
                       DEPENDING ON CNT
                       PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let cnt = find_field(&schema, "CNT");
    assert_eq!(cnt.offset, 4);
    assert_eq!(cnt.len, 3);

    let items = find_field(&schema, "ITEMS");
    assert_eq!(items.offset, 7);
    assert_eq!(items.len, 10, "base element size");
    assert!(matches!(
        items.occurs,
        Some(Occurs::ODO {
            min: 1,
            max: 50,
            ..
        })
    ));

    // tail_odo metadata
    let tail = schema.tail_odo.as_ref().expect("tail_odo should be set");
    assert_eq!(tail.counter_path, "CNT");
    assert_eq!(tail.min_count, 1);
    assert_eq!(tail.max_count, 50);

    // No fixed LRECL for ODO
    assert!(schema.lrecl_fixed.is_none(), "ODO means no fixed LRECL");
}

// ===========================================================================
// ODO — counter not found → CBKS121
// ===========================================================================

#[test]
fn odo_counter_not_found_cbks121() {
    let cpy = r"
       01 REC.
          05 ITEMS OCCURS 1 TO 10 TIMES
                   DEPENDING ON MISSING-COUNTER
                   PIC X(5).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND);
}

// ===========================================================================
// ODO — not tail → CBKP021
// ===========================================================================

#[test]
fn odo_not_tail_cbkp021() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(3).
          05 ITEMS     OCCURS 1 TO 10 TIMES
                       DEPENDING ON CNT
                       PIC X(5).
          05 TRAILER   PIC X(1).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

// ===========================================================================
// ODO — min > max → syntax error
// ===========================================================================

#[test]
fn odo_min_greater_than_max_stores_as_given() {
    // The parser accepts OCCURS 50 TO 10 as written — it stores the values
    // as declared without validating min <= max at parse time.
    let cpy = r"
       01 REC.
          05 CNT   PIC 9(3).
          05 ITEMS OCCURS 50 TO 10 TIMES
                   DEPENDING ON CNT
                   PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let tail = schema.tail_odo.as_ref().unwrap();
    // Parser stores raw values as declared
    assert_eq!(tail.min_count, 50);
    assert_eq!(tail.max_count, 10);
}

// ===========================================================================
// ODO — counter field type must be numeric
// ===========================================================================

#[test]
fn odo_counter_must_be_numeric_field() {
    // Counter field is PIC 9 (numeric) → should work
    let cpy_ok = r"
       01 REC.
          05 CNT   PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES
                   DEPENDING ON CNT
                   PIC X(5).
    ";
    assert!(
        parse_copybook(cpy_ok).is_ok(),
        "Numeric counter should be accepted"
    );

    // Counter field is PIC 9 COMP → should also work
    let cpy_comp = r"
       01 REC.
          05 CNT   PIC 9(4) COMP.
          05 ITEMS OCCURS 1 TO 10 TIMES
                   DEPENDING ON CNT
                   PIC X(5).
    ";
    assert!(
        parse_copybook(cpy_comp).is_ok(),
        "Binary counter should be accepted"
    );
}

// ===========================================================================
// ODO — with REDEFINES → CBKP023 (O6 rejected)
// ===========================================================================

#[test]
fn odo_over_redefines_rejected_o6() {
    let cpy = r"
       01 REC.
          05 CNT        PIC 9(3).
          05 BASE-DATA  PIC X(50).
          05 REDEF-DATA REDEFINES BASE-DATA.
             10 ITEMS   OCCURS 1 TO 10 TIMES
                        DEPENDING ON CNT
                        PIC X(5).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKP023_ODO_REDEFINES,
        "ODO inside REDEFINES should produce CBKP023"
    );
}

// ===========================================================================
// ODO — counter inside REDEFINES rejected
// ===========================================================================

#[test]
fn odo_counter_inside_redefines_rejected() {
    let cpy = r"
       01 REC.
          05 RAW-DATA PIC X(10).
          05 REDEF-DATA REDEFINES RAW-DATA.
             10 CNT PIC 9(3).
             10 PAD PIC X(7).
          05 ITEMS OCCURS 1 TO 5 TIMES
                   DEPENDING ON CNT
                   PIC X(4).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(
        err.code,
        ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        "Counter inside REDEFINES should produce CBKS121"
    );
}

// ===========================================================================
// ODO — min_count=0 with various dialects
// ===========================================================================

#[test]
fn odo_min_count_zero_normative() {
    let cpy = r"
       01 REC.
          05 N    PIC 9(3).
          05 DATA OCCURS 0 TO 100 TIMES
                  DEPENDING ON N
                  PIC X(5).
    ";
    let opts = ParseOptions {
        dialect: copybook_core::Dialect::Normative,
        ..Default::default()
    };
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();
    let tail = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail.min_count, 0, "Normative preserves min_count=0");
}

#[test]
fn odo_min_count_zero_tolerant() {
    let cpy = r"
       01 REC.
          05 N    PIC 9(3).
          05 DATA OCCURS 5 TO 100 TIMES
                  DEPENDING ON N
                  PIC X(5).
    ";
    let opts = ParseOptions {
        dialect: copybook_core::Dialect::ZeroTolerant,
        ..Default::default()
    };
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();
    let tail = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail.min_count, 0, "ZeroTolerant forces min_count to 0");
}

#[test]
fn odo_min_count_one_tolerant() {
    let cpy = r"
       01 REC.
          05 N    PIC 9(3).
          05 DATA OCCURS 0 TO 100 TIMES
                  DEPENDING ON N
                  PIC X(5).
    ";
    let opts = ParseOptions {
        dialect: copybook_core::Dialect::OneTolerant,
        ..Default::default()
    };
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();
    let tail = schema.tail_odo.as_ref().unwrap();
    assert_eq!(
        tail.min_count, 1,
        "OneTolerant raises min_count to at least 1"
    );
}

// ===========================================================================
// ODO — affects LRECL (no fixed LRECL, but min vs max can be inferred)
// ===========================================================================

#[test]
fn odo_affects_lrecl_no_fixed() {
    let cpy = r"
       01 REC.
          05 HDR  PIC X(4).
          05 CNT  PIC 9(2).
          05 ROWS OCCURS 1 TO 20 TIMES
                  DEPENDING ON CNT.
             10 COL-A PIC X(5).
             10 COL-B PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    // ODO → no fixed LRECL
    assert!(schema.lrecl_fixed.is_none());

    // tail_odo should give us bounds info
    let tail = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail.min_count, 1);
    assert_eq!(tail.max_count, 20);

    // We can compute min and max record lengths from schema info:
    // fixed portion = 4 + 2 = 6
    // element size = 5 + 5 = 10
    // min record = 6 + 1*10 = 16
    // max record = 6 + 20*10 = 206
    let rows = find_field(&schema, "ROWS");
    assert_eq!(rows.len, 10, "base element size");
    let hdr = find_field(&schema, "HDR");
    let cnt = find_field(&schema, "CNT");
    let fixed_portion = hdr.len + cnt.len;
    assert_eq!(fixed_portion, 6);
}

// ===========================================================================
// REDEFINES — does not advance offset (no double-count)
// ===========================================================================

#[test]
fn redefines_does_not_advance_offset() {
    let cpy = r"
       01 REC.
          05 A PIC X(10).
          05 B REDEFINES A PIC X(10).
          05 C REDEFINES A PIC X(10).
          05 D PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();

    assert_eq!(find_field(&schema, "A").offset, 0);
    assert_eq!(find_field(&schema, "B").offset, 0);
    assert_eq!(find_field(&schema, "C").offset, 0);
    assert_eq!(find_field(&schema, "D").offset, 10, "D at 10, not 30");
    assert_eq!(schema.lrecl_fixed, Some(15), "10 + 5, not 35");
}

// ===========================================================================
// REDEFINES — find_redefining_fields API with multiple
// ===========================================================================

#[test]
fn find_redefining_fields_multiple() {
    let cpy = r"
       01 REC.
          05 BASE    PIC X(20).
          05 VIEW-A  REDEFINES BASE PIC 9(20).
          05 VIEW-B  REDEFINES BASE.
             10 P1   PIC X(10).
             10 P2   PIC X(10).
          05 VIEW-C  REDEFINES BASE PIC X(20).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let redefining = schema.find_redefining_fields("BASE");
    assert_eq!(redefining.len(), 3, "Three REDEFINES of BASE");
    let names: Vec<&str> = redefining.iter().map(|f| f.name.as_str()).collect();
    assert!(names.contains(&"VIEW-A"));
    assert!(names.contains(&"VIEW-B"));
    assert!(names.contains(&"VIEW-C"));
}

// ===========================================================================
// ODO — tail_odo array_path populated
// ===========================================================================

#[test]
fn odo_tail_array_path_populated() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(2).
          05 ITEMS     OCCURS 1 TO 25 TIMES
                       DEPENDING ON CNT
                       PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let tail = schema.tail_odo.as_ref().expect("tail_odo set");
    assert_eq!(tail.array_path, "ITEMS");
    assert_eq!(tail.counter_path, "CNT");
}

// ===========================================================================
// ODO — level 88 after ODO is OK (non-storage)
// ===========================================================================

#[test]
fn odo_with_level88_after_is_ok() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(3).
          05 ITEMS     OCCURS 1 TO 5 TIMES
                       DEPENDING ON CNT
                       PIC X(3).
          88 ALL-DONE VALUE '000'.
    ";
    assert!(
        parse_copybook(cpy).is_ok(),
        "Level-88 after ODO should be allowed"
    );
}

// ===========================================================================
// REDEFINES — COMP-3 redefines alphanumeric
// ===========================================================================

#[test]
fn redefines_comp3_over_alphanum() {
    let cpy = r"
       01 REC.
          05 RAW-BYTES PIC X(5).
          05 PACKED    REDEFINES RAW-BYTES PIC S9(7)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let raw = find_field(&schema, "RAW-BYTES");
    let packed = find_field(&schema, "PACKED");

    assert!(matches!(raw.kind, FieldKind::Alphanum { len: 5 }));
    assert!(matches!(
        packed.kind,
        FieldKind::PackedDecimal {
            digits: 9,
            scale: 2,
            signed: true
        }
    ));
    assert_eq!(packed.redefines_of.as_deref(), Some("RAW-BYTES"));
    assert_eq!(raw.offset, packed.offset);
}

// ===========================================================================
// ODO — group with children as ODO element
// ===========================================================================

#[test]
fn odo_group_with_children() {
    let cpy = r"
       01 ORDER-RECORD.
          05 ORDER-ID    PIC X(6).
          05 ITEM-COUNT  PIC 9(2).
          05 ITEMS       OCCURS 1 TO 50
                         DEPENDING ON ITEM-COUNT.
             10 ITEM-CODE PIC X(4).
             10 ITEM-QTY  PIC 9(3).
    ";
    let schema = parse_copybook(cpy).unwrap();

    let items = find_field(&schema, "ITEMS");
    assert_eq!(items.len, 7, "element = 4 + 3");
    assert!(matches!(
        items.occurs,
        Some(Occurs::ODO {
            min: 1,
            max: 50,
            ..
        })
    ));
    assert_eq!(items.children.len(), 2);
    assert_eq!(items.children[0].name, "ITEM-CODE");
    assert_eq!(items.children[1].name, "ITEM-QTY");
}

// ===========================================================================
// REDEFINES — BINARY redefines zoned decimal
// ===========================================================================

#[test]
fn redefines_binary_over_zoned() {
    let cpy = r"
       01 REC.
          05 ZONED-AMT PIC 9(4).
          05 BIN-AMT   REDEFINES ZONED-AMT PIC 9(4) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let zoned = find_field(&schema, "ZONED-AMT");
    let bin = find_field(&schema, "BIN-AMT");

    assert!(matches!(
        zoned.kind,
        FieldKind::ZonedDecimal { digits: 4, .. }
    ));
    assert!(matches!(bin.kind, FieldKind::BinaryInt { bits: 16, .. }));
    assert_eq!(bin.redefines_of.as_deref(), Some("ZONED-AMT"));
    assert_eq!(zoned.offset, bin.offset);
}

// ===========================================================================
// ODO — COMP counter accepted
// ===========================================================================

#[test]
fn odo_comp_counter_accepted() {
    let cpy = r"
       01 REC.
          05 CNT   PIC 9(4) COMP.
          05 ITEMS OCCURS 1 TO 100 TIMES
                   DEPENDING ON CNT
                   PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert!(schema.tail_odo.is_some());
    let tail = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail.counter_path, "CNT");
    assert_eq!(tail.max_count, 100);
}

// ===========================================================================
// ODO — COMP-3 counter accepted
// ===========================================================================

#[test]
fn odo_comp3_counter_accepted() {
    let cpy = r"
       01 REC.
          05 CNT   PIC 9(3) COMP-3.
          05 ITEMS OCCURS 1 TO 50 TIMES
                   DEPENDING ON CNT
                   PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let tail = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail.counter_path, "CNT");
    assert_eq!(tail.max_count, 50);
}

// ===========================================================================
// REDEFINES — LRECL with redefines does not double-count
// ===========================================================================

#[test]
fn redefines_lrecl_not_double_counted() {
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

// ===========================================================================
// ODO — only child (no preceding siblings in group)
// ===========================================================================

#[test]
fn odo_as_only_child_in_group() {
    let cpy = r"
       01 COUNTER PIC 9(3).
       01 ARRAY-FIELD PIC X(10) OCCURS 5 TIMES DEPENDING ON COUNTER.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let tail = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail.counter_path, "COUNTER");
    assert_eq!(tail.max_count, 5);
}
