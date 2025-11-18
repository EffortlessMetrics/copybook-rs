#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

#![allow(clippy::unwrap_used, clippy::expect_used)]
use copybook_core::parse_copybook;
use copybook_core::{Field, Schema};

fn record_children(schema: &Schema) -> &Vec<Field> {
    &schema.fields[0].children
}

#[test]
fn renames_basic_thru() {
    let cb = "
01 RECORD-A.
   05 FIELD-1 PIC X(10).
   05 FIELD-2 PIC 9(5).
   05 FIELD-3 PIC X(2).
   66 ALIAS-A RENAMES FIELD-1 THRU FIELD-3.
";
    let schema = parse_copybook(cb).expect("parse ok");
    let kids = record_children(&schema);

    let f1 = &kids[0];
    let f3 = &kids[2];
    let alias = kids
        .iter()
        .find(|f| f.level == 66 && f.name == "ALIAS-A")
        .unwrap();

    let rr = alias.resolved_renames.as_ref().expect("resolved");
    assert_eq!(rr.offset, f1.offset);
    assert_eq!(rr.length, (f3.offset + f3.len) - f1.offset);
    assert_eq!(rr.members.len(), 3);
}

#[test]
fn renames_through_synonym() {
    let cb = "
01 RECORD-A.
   05 A PIC 9(2).
   05 B PIC 9(2).
   66 R RENAMES A THROUGH B.
";
    let schema = parse_copybook(cb).expect("parse ok");
    let kids = record_children(&schema);
    let alias = kids
        .iter()
        .find(|f| f.level == 66 && f.name == "R")
        .unwrap();
    assert_eq!(alias.resolved_renames.as_ref().unwrap().members.len(), 2);
}

#[test]
fn renames_single_field_range() {
    let cb = "
01 RECORD-A.
   05 ITEM PIC 9(4).
   66 ONLY RENAMES ITEM THRU ITEM.
";
    let schema = parse_copybook(cb).expect("parse ok");
    let kids = record_children(&schema);
    let item = kids.iter().find(|f| f.name == "ITEM").unwrap();
    let alias = kids
        .iter()
        .find(|f| f.level == 66 && f.name == "ONLY")
        .unwrap();
    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.offset, item.offset);
    assert_eq!(rr.length, item.len);
    assert_eq!(rr.members.len(), 1);
}

#[test]
fn renames_qualified_names_same_scope() {
    // Qualified QNAMEs; resolver matches by head IDENT in PR A.
    let cb = "
01 RECORD-A.
   05 F1 PIC X(3).
   05 F2 PIC X(2).
   05 F3 PIC X(1).
   66 ALIAS RENAMES F1 OF RECORD-A THRU F3 OF RECORD-A.
";
    let schema = parse_copybook(cb).expect("parse ok");
    let kids = record_children(&schema);
    let alias = kids
        .iter()
        .find(|f| f.level == 66 && f.name == "ALIAS")
        .unwrap();
    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 3);
}
