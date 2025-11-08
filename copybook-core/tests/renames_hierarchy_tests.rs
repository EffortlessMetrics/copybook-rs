//! Tests for RENAMES (level-66) hierarchy placement
//!
//! Validates that level-66 RENAMES entries are placed as siblings
//! of the aliased fields under the same parent group, not at schema root.

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]

use copybook_core::{FieldKind, parse_copybook};

#[test]
fn renames_is_child_of_record_not_root() {
    let cb = "
01 RECORD-A.
   05 F1 PIC X(3).
   05 F2 PIC 9(2).
   66 ALIAS RENAMES F1 THRU F2.
";
    let schema = parse_copybook(cb).unwrap();

    // Exactly one root (01 RECORD-A)
    assert_eq!(schema.fields.len(), 1, "Should have exactly one root field");
    let rec = &schema.fields[0];
    assert_eq!(rec.level, 1);
    assert_eq!(rec.name, "RECORD-A");

    // ALIAS lives under the record group as a sibling
    let alias = rec
        .children
        .iter()
        .find(|f| f.level == 66 && f.name == "ALIAS")
        .expect("Should find ALIAS as child of RECORD-A");

    assert!(matches!(alias.kind, FieldKind::Renames { .. }));
}

#[test]
fn renames_placement_with_nested_groups() {
    let cb = "
01 ROOT-REC.
   05 GROUP-A.
      10 FIELD-A PIC X(5).
      10 FIELD-B PIC 9(3).
      66 ALIAS-A RENAMES FIELD-A THRU FIELD-B.
   05 GROUP-B.
      10 FIELD-C PIC X(2).
      66 ALIAS-B RENAMES FIELD-C THRU FIELD-C.
";
    let schema = parse_copybook(cb).unwrap();
    let root = &schema.fields[0];

    // Find GROUP-A
    let group_a = root
        .children
        .iter()
        .find(|f| f.name == "GROUP-A")
        .expect("Should find GROUP-A");

    // ALIAS-A should be a child of GROUP-A
    let alias_a = group_a
        .children
        .iter()
        .find(|f| f.level == 66 && f.name == "ALIAS-A")
        .expect("Should find ALIAS-A as child of GROUP-A");

    assert!(matches!(alias_a.kind, FieldKind::Renames { .. }));

    // Find GROUP-B
    let group_b = root
        .children
        .iter()
        .find(|f| f.name == "GROUP-B")
        .expect("Should find GROUP-B");

    // ALIAS-B should be a child of GROUP-B
    let alias_b = group_b
        .children
        .iter()
        .find(|f| f.level == 66 && f.name == "ALIAS-B")
        .expect("Should find ALIAS-B as child of GROUP-B");

    assert!(matches!(alias_b.kind, FieldKind::Renames { .. }));
}

#[test]
fn renames_not_at_schema_root() {
    let cb = "
01 RECORD-A.
   05 F1 PIC X(3).
   05 F2 PIC X(2).
   66 ALIAS RENAMES F1 THRU F2.
";
    let schema = parse_copybook(cb).unwrap();

    // Should have exactly one root field (the 01 level)
    assert_eq!(
        schema.fields.len(),
        1,
        "Level-66 should NOT appear at schema root"
    );

    // The root should be RECORD-A, not ALIAS
    assert_eq!(schema.fields[0].name, "RECORD-A");
    assert_eq!(schema.fields[0].level, 1);
}
