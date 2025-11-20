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

// ============================================================================
// R2: Same-scope group RENAMES (Phase R2)
// Contract: 66 should attach to the group being renamed, not at level-01
// ============================================================================

#[test]
fn renames_r2_same_scope_group() {
    // From design doc: RENAMES_NESTED_GROUPS.md R2 example
    // Note: COBOL RENAMES syntax requires THRU even for single-field ranges
    let cb = "
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
      10 ADDRESS  PIC X(60).
   66 CUSTOMER-DETAILS RENAMES CUSTOMER-INFO THRU CUSTOMER-INFO.
";
    let schema = parse_copybook(cb).expect("parse ok");
    let record = &schema.fields[0];

    // CUSTOMER-INFO should be a child of CUSTOMER-RECORD
    let customer_info = record
        .children
        .iter()
        .find(|f| f.name == "CUSTOMER-INFO")
        .expect("CUSTOMER-INFO group");

    // CUSTOMER-DETAILS (66) should be attached to CUSTOMER-RECORD, not at level-01
    // It should reference the CUSTOMER-INFO group
    let alias = record
        .children
        .iter()
        .find(|f| f.level == 66 && f.name == "CUSTOMER-DETAILS")
        .expect("CUSTOMER-DETAILS alias");

    let rr = alias.resolved_renames.as_ref().expect("resolved renames");

    // Verify alias points to the correct group
    assert_eq!(rr.offset, customer_info.offset);
    assert_eq!(rr.length, customer_info.len);

    // Members should include NAME and ADDRESS (storage-bearing fields within CUSTOMER-INFO)
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|p| p.contains("NAME")));
    assert!(rr.members.iter().any(|p| p.contains("ADDRESS")));
}

// ============================================================================
// R3: Nested group RENAMES (Phase R2)
// Contract: 66 should attach to closest common ancestor of RENAMES and target
// ============================================================================

#[test]
fn renames_r3_nested_group() {
    // From design doc: RENAMES_NESTED_GROUPS.md R3 example
    // Note: COBOL RENAMES syntax requires THRU even for single-field ranges
    let cb = "
01 POLICY-RECORD.
   05 POLICY-INFO.
      10 POLICY-NUMBER PIC X(10).
      10 POLICY-DATES.
         15 START-DATE PIC X(8).
         15 END-DATE   PIC X(8).
   66 POLICY-PERIOD RENAMES POLICY-DATES THRU POLICY-DATES.
";
    let schema = parse_copybook(cb).expect("parse ok");
    let record = &schema.fields[0];

    // POLICY-INFO should be a child of POLICY-RECORD
    let policy_info = record
        .children
        .iter()
        .find(|f| f.name == "POLICY-INFO")
        .expect("POLICY-INFO group");

    // POLICY-DATES should be nested inside POLICY-INFO
    let policy_dates = policy_info
        .children
        .iter()
        .find(|f| f.name == "POLICY-DATES")
        .expect("POLICY-DATES group");

    // POLICY-PERIOD (66) is attached to POLICY-RECORD (level-01) physically,
    // but semantically references the nested POLICY-DATES subtree (Option B from design).
    // This avoids complex tree surgery while maintaining correct semantics.
    let alias = record
        .children
        .iter()
        .find(|f| f.level == 66 && f.name == "POLICY-PERIOD")
        .expect("POLICY-PERIOD alias");

    let rr = alias.resolved_renames.as_ref().expect("resolved renames");

    // Verify alias points to POLICY-DATES group
    assert_eq!(rr.offset, policy_dates.offset);
    assert_eq!(rr.length, policy_dates.len);

    // Members should include START-DATE and END-DATE
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|p| p.contains("START-DATE")));
    assert!(rr.members.iter().any(|p| p.contains("END-DATE")));
}
