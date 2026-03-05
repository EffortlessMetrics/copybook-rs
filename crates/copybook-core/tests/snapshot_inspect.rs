// SPDX-License-Identifier: AGPL-3.0-or-later
//! Snapshot tests for schema layout inspection.
//!
//! These tests verify that parsed copybooks produce stable layout information
//! (offsets, lengths, field hierarchy) by formatting the schema into a
//! human-readable layout string and asserting against expected output.

#![allow(clippy::unwrap_used)]

use copybook_core::{Field, FieldKind, parse_copybook};

// ---------------------------------------------------------------------------
// Helper: render a schema's field layout as a human-readable string
// ---------------------------------------------------------------------------

fn render_layout(fields: &[Field], indent: usize) -> String {
    let mut out = String::new();
    for field in fields {
        let prefix = " ".repeat(indent);
        let kind_label = match &field.kind {
            FieldKind::Alphanum { len } => format!("Alphanum({})", len),
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
                ..
            } => format!(
                "Zoned(digits={}, scale={}, signed={})",
                digits, scale, signed
            ),
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => format!(
                "Packed(digits={}, scale={}, signed={})",
                digits, scale, signed
            ),
            FieldKind::BinaryInt { bits, signed } => {
                format!("Binary(bits={}, signed={})", bits, signed)
            }
            FieldKind::Group => "Group".to_string(),
            FieldKind::Condition { values } => format!("Condition({:?})", values),
            FieldKind::Renames {
                from_field,
                thru_field,
            } => format!("Renames({} THRU {})", from_field, thru_field),
            FieldKind::EditedNumeric {
                pic_string,
                width,
                scale,
                signed,
            } => format!(
                "Edited(pic={}, width={}, scale={}, signed={})",
                pic_string, width, scale, signed
            ),
            FieldKind::FloatSingle => "FloatSingle".to_string(),
            FieldKind::FloatDouble => "FloatDouble".to_string(),
        };

        let occurs_label = match &field.occurs {
            Some(copybook_core::Occurs::Fixed { count }) => format!(" OCCURS {}", count),
            Some(copybook_core::Occurs::ODO {
                min,
                max,
                counter_path,
            }) => format!(" OCCURS {} TO {} DEPENDING ON {}", min, max, counter_path),
            None => String::new(),
        };

        let redefines_label = match &field.redefines_of {
            Some(r) => format!(" REDEFINES {}", r),
            None => String::new(),
        };

        out.push_str(&format!(
            "{}{:02} {:<30} offset={:>4} len={:>4}  {}{}{}\n",
            prefix,
            field.level,
            field.name,
            field.offset,
            field.len,
            kind_label,
            occurs_label,
            redefines_label,
        ));

        if !field.children.is_empty() {
            out.push_str(&render_layout(&field.children, indent + 2));
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Simple flat copybook layout
// ---------------------------------------------------------------------------

#[test]
fn snapshot_inspect_simple_flat_layout() {
    let copybook = "\
       01 EMPLOYEE-RECORD.
          05 EMP-ID      PIC X(6).
          05 EMP-NAME    PIC X(20).
          05 SALARY      PIC 9(7)V99.
";
    let schema = parse_copybook(copybook).unwrap();
    let layout = render_layout(&schema.fields, 0);

    // Verify fixed LRECL
    assert_eq!(schema.lrecl_fixed, Some(35));

    // Verify layout content
    assert!(layout.contains("EMP-ID"), "should contain EMP-ID");
    assert!(layout.contains("EMP-NAME"), "should contain EMP-NAME");
    assert!(layout.contains("SALARY"), "should contain SALARY");
    assert!(layout.contains("offset=   0"), "EMP-ID starts at 0");
    assert!(layout.contains("len=   6"), "EMP-ID is 6 bytes");
    assert!(layout.contains("offset=   6"), "EMP-NAME starts at 6");
    assert!(layout.contains("len=  20"), "EMP-NAME is 20 bytes");
    assert!(layout.contains("offset=  26"), "SALARY starts at 26");
    assert!(
        layout.contains("len=   9"),
        "SALARY is 9 bytes (7+2 digits)"
    );
    assert!(layout.contains("Alphanum(6)"), "EMP-ID is Alphanum");
    assert!(
        layout.contains("Zoned(digits=9, scale=2, signed=false)"),
        "SALARY is unsigned zoned decimal"
    );
}

// ---------------------------------------------------------------------------
// Nested group layout
// ---------------------------------------------------------------------------

#[test]
fn snapshot_inspect_nested_group_layout() {
    let copybook = "\
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID    PIC X(8).
          05 CUSTOMER-NAME.
             10 FIRST-NAME  PIC X(15).
             10 LAST-NAME   PIC X(20).
          05 ACCOUNT-BAL    PIC S9(7)V99 COMP-3.
";
    let schema = parse_copybook(copybook).unwrap();
    let layout = render_layout(&schema.fields, 0);

    // LRECL = 8 + 15 + 20 + 5 = 48
    assert_eq!(schema.lrecl_fixed, Some(48));

    // Group field should appear
    assert!(
        layout.contains("CUSTOMER-NAME"),
        "should contain group name"
    );
    assert!(layout.contains("Group"), "CUSTOMER-NAME is a Group");

    // Children should be indented
    assert!(layout.contains("FIRST-NAME"), "should contain FIRST-NAME");
    assert!(layout.contains("LAST-NAME"), "should contain LAST-NAME");

    // COMP-3 field
    assert!(
        layout.contains("Packed(digits=9, scale=2, signed=true)"),
        "ACCOUNT-BAL should be packed decimal"
    );
}

// ---------------------------------------------------------------------------
// REDEFINES layout
// ---------------------------------------------------------------------------

#[test]
fn snapshot_inspect_redefines_layout() {
    let copybook = "\
       01 MULTI-RECORD.
          05 RECORD-TYPE    PIC X(1).
          05 DATA-AREA      PIC X(20).
          05 DATA-NUMERIC REDEFINES DATA-AREA PIC 9(20).
";
    let schema = parse_copybook(copybook).unwrap();
    let layout = render_layout(&schema.fields, 0);

    // LRECL = 1 + 20 = 21 (REDEFINES doesn't add length)
    assert_eq!(schema.lrecl_fixed, Some(21));

    // Both DATA-AREA and DATA-NUMERIC should share offset
    assert!(
        layout.contains("DATA-AREA"),
        "should contain original field"
    );
    assert!(
        layout.contains("DATA-NUMERIC"),
        "should contain redefining field"
    );
    assert!(
        layout.contains("REDEFINES DATA-AREA"),
        "should show REDEFINES annotation"
    );
}

// ---------------------------------------------------------------------------
// ODO layout
// ---------------------------------------------------------------------------

#[test]
fn snapshot_inspect_odo_layout() {
    let copybook = "\
       01 ORDER-RECORD.
          05 ORDER-ID        PIC X(6).
          05 ITEM-COUNT      PIC 9(2).
          05 ITEMS           OCCURS 1 TO 50
                             DEPENDING ON ITEM-COUNT.
             10 ITEM-CODE    PIC X(4).
             10 ITEM-QTY     PIC 9(3).
";
    let schema = parse_copybook(copybook).unwrap();
    let layout = render_layout(&schema.fields, 0);

    // ODO schemas have no fixed LRECL
    assert!(
        schema.lrecl_fixed.is_none(),
        "ODO should have no fixed LRECL"
    );

    // Tail ODO info
    assert!(schema.tail_odo.is_some());
    let tail = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail.counter_path, "ITEM-COUNT");
    assert_eq!(tail.min_count, 1);
    assert_eq!(tail.max_count, 50);

    // Layout should show OCCURS DEPENDING ON
    assert!(
        layout.contains("OCCURS 1 TO 50 DEPENDING ON ITEM-COUNT"),
        "should show ODO annotation"
    );
    assert!(layout.contains("ITEM-CODE"), "should show ODO children");
    assert!(layout.contains("ITEM-QTY"), "should show ODO children");
}

// ---------------------------------------------------------------------------
// Mixed numeric types layout
// ---------------------------------------------------------------------------

#[test]
fn snapshot_inspect_mixed_numeric_types() {
    let copybook = "\
       01 NUMERIC-RECORD.
          05 ZONED-FIELD     PIC S9(5)V99.
          05 PACKED-FIELD    PIC S9(7)V99 COMP-3.
          05 BINARY-FIELD    PIC S9(4) COMP.
          05 ALPHA-FIELD     PIC X(10).
";
    let schema = parse_copybook(copybook).unwrap();
    let layout = render_layout(&schema.fields, 0);

    // Verify each kind appears
    assert!(
        layout.contains("Zoned(digits=7, scale=2, signed=true)"),
        "should show zoned decimal kind"
    );
    assert!(
        layout.contains("Packed(digits=9, scale=2, signed=true)"),
        "should show packed decimal kind"
    );
    assert!(
        layout.contains("Binary(bits="),
        "should show binary int kind"
    );
    assert!(layout.contains("Alphanum(10)"), "should show alphanum kind");

    // Verify first field starts at offset 0
    assert_eq!(schema.fields[0].offset, 0);
}

// ---------------------------------------------------------------------------
// Level-88 conditions in layout
// ---------------------------------------------------------------------------

#[test]
fn snapshot_inspect_level88_layout() {
    let copybook = "\
       01 STATUS-RECORD.
          05 STATUS-CODE PIC X(1).
             88 ACTIVE   VALUE 'A'.
             88 INACTIVE VALUE 'I'.
          05 DESCRIPTION  PIC X(20).
";
    let schema = parse_copybook(copybook).unwrap();
    let layout = render_layout(&schema.fields, 0);

    // Level-88 fields should appear as children
    assert!(layout.contains("88 ACTIVE"), "should show Level-88 ACTIVE");
    assert!(
        layout.contains("88 INACTIVE"),
        "should show Level-88 INACTIVE"
    );
    assert!(
        layout.contains("Condition"),
        "should show Condition kind for Level-88"
    );

    // Level-88 fields have len=0 (no storage)
    assert!(
        layout.contains("len=   0"),
        "Level-88 fields should have zero length"
    );
}
