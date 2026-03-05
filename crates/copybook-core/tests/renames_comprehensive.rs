// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive RENAMES (Level-66) tests covering R1–R3 scenarios,
//! Schema API, projection, serialization, and error paths.

#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

use copybook_core::{
    ErrorCode, Field, Schema, parse_copybook, project_schema,
    schema::{FieldKind, ResolvedRenames},
};

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

/// Return the children of the first (level-01) record in the schema.
fn record_children(schema: &Schema) -> &Vec<Field> {
    &schema.fields[0].children
}

// ═══════════════════════════════════════════════════════════════════════════
// R1: RENAMES same-scope elementary fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn r1_same_scope_elementary_fields() {
    let cb = "
       01 RECORD-A.
          05 FIRST-NAME   PIC X(20).
          05 LAST-NAME    PIC X(30).
          05 MIDDLE-INIT  PIC X(1).
       66 FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.level == 66 && f.name == "FULL-NAME")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().expect("resolved");
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|m| m.contains("FIRST-NAME")));
    assert!(rr.members.iter().any(|m| m.contains("LAST-NAME")));
    // Should NOT include MIDDLE-INIT
    assert!(!rr.members.iter().any(|m| m.contains("MIDDLE-INIT")));
}

// ═══════════════════════════════════════════════════════════════════════════
// R2: RENAMES group alias
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn r2_group_alias_preserves_structure() {
    let cb = "
       01 CUSTOMER-RECORD.
          05 CUSTOMER-INFO.
             10 NAME     PIC X(30).
             10 ADDRESS  PIC X(60).
       66 CUSTOMER-DETAILS RENAMES CUSTOMER-INFO THRU CUSTOMER-INFO.
    ";
    let schema = parse_copybook(cb).expect("parse");

    let info = record_children(&schema)
        .iter()
        .find(|f| f.name == "CUSTOMER-INFO")
        .expect("group");

    let alias = record_children(&schema)
        .iter()
        .find(|f| f.level == 66 && f.name == "CUSTOMER-DETAILS")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().expect("resolved");
    assert_eq!(rr.offset, info.offset);
    assert_eq!(rr.length, info.len);
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|m| m.contains("NAME")));
    assert!(rr.members.iter().any(|m| m.contains("ADDRESS")));
}

// ═══════════════════════════════════════════════════════════════════════════
// R3: RENAMES nested group fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn r3_nested_group_fields() {
    let cb = "
       01 POLICY-RECORD.
          05 POLICY-INFO.
             10 POLICY-NUM PIC X(10).
             10 POLICY-DATES.
                15 START-DATE PIC X(8).
                15 END-DATE   PIC X(8).
       66 POLICY-PERIOD RENAMES POLICY-DATES THRU POLICY-DATES.
    ";
    let schema = parse_copybook(cb).expect("parse");

    let alias = record_children(&schema)
        .iter()
        .find(|f| f.level == 66 && f.name == "POLICY-PERIOD")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().expect("resolved");
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|m| m.contains("START-DATE")));
    assert!(rr.members.iter().any(|m| m.contains("END-DATE")));

    // Offset and length should match the nested group
    let policy_info = record_children(&schema)
        .iter()
        .find(|f| f.name == "POLICY-INFO")
        .unwrap();
    let policy_dates = policy_info
        .children
        .iter()
        .find(|f| f.name == "POLICY-DATES")
        .unwrap();
    assert_eq!(rr.offset, policy_dates.offset);
    assert_eq!(rr.length, policy_dates.len);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES with THRU vs THROUGH
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_thru_keyword() {
    let cb = "
       01 REC.
          05 A PIC X(5).
          05 B PIC X(5).
       66 AB RENAMES A THRU B.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "AB")
        .expect("alias");
    assert_eq!(alias.resolved_renames.as_ref().unwrap().members.len(), 2);
}

#[test]
fn renames_through_keyword() {
    let cb = "
       01 REC.
          05 A PIC X(5).
          05 B PIC X(5).
       66 AB RENAMES A THROUGH B.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "AB")
        .expect("alias");
    assert_eq!(alias.resolved_renames.as_ref().unwrap().members.len(), 2);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES single field alias (from == thru)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_single_field_alias() {
    let cb = "
       01 REC.
          05 BALANCE PIC 9(7)V99.
       66 BAL-ALIAS RENAMES BALANCE THRU BALANCE.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "BAL-ALIAS")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 1);
    assert!(rr.members[0].contains("BALANCE"));

    let balance = record_children(&schema)
        .iter()
        .find(|f| f.name == "BALANCE")
        .unwrap();
    assert_eq!(rr.offset, balance.offset);
    assert_eq!(rr.length, balance.len);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES spanning multiple fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_spanning_multiple_fields() {
    let cb = "
       01 REC.
          05 F1 PIC X(3).
          05 F2 PIC X(4).
          05 F3 PIC X(5).
          05 F4 PIC X(6).
       66 MID RENAMES F2 THRU F3.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "MID")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|m| m.contains("F2")));
    assert!(rr.members.iter().any(|m| m.contains("F3")));
    // Should not include F1 or F4
    assert!(!rr.members.iter().any(|m| m.contains("F1")));
    assert!(!rr.members.iter().any(|m| m.contains("F4")));
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES of numeric fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_of_numeric_fields() {
    let cb = "
       01 REC.
          05 AMOUNT     PIC 9(7)V99.
          05 QUANTITY   PIC 9(5).
       66 NUM-RANGE RENAMES AMOUNT THRU QUANTITY.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "NUM-RANGE")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|m| m.contains("AMOUNT")));
    assert!(rr.members.iter().any(|m| m.contains("QUANTITY")));
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES of alphanumeric fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_of_alphanumeric_fields() {
    let cb = "
       01 REC.
          05 FIRST-NAME PIC X(20).
          05 LAST-NAME  PIC X(30).
          05 CITY       PIC X(25).
       66 PERSON-NAME RENAMES FIRST-NAME THRU LAST-NAME.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "PERSON-NAME")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 2);
    assert_eq!(rr.length, 50); // 20 + 30
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES of COMP-3 fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_of_comp3_fields() {
    let cb = "
       01 REC.
          05 PKD-A PIC S9(5)   COMP-3.
          05 PKD-B PIC S9(7)V9 COMP-3.
       66 PACKED-RANGE RENAMES PKD-A THRU PKD-B.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "PACKED-RANGE")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|m| m.contains("PKD-A")));
    assert!(rr.members.iter().any(|m| m.contains("PKD-B")));

    // Verify that COMP-3 field lengths are correct in the alias
    // PKD-A: S9(5) COMP-3 → 3 bytes, PKD-B: S9(7)V9 COMP-3 → 5 bytes
    let pkd_a = record_children(&schema)
        .iter()
        .find(|f| f.name == "PKD-A")
        .unwrap();
    let pkd_b = record_children(&schema)
        .iter()
        .find(|f| f.name == "PKD-B")
        .unwrap();
    assert_eq!(rr.offset, pkd_a.offset);
    assert_eq!(rr.length, pkd_a.len + pkd_b.len);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES with Level-88 after
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_with_level88_conditions_in_range() {
    let cb = "
       01 REC.
          05 STATUS-CODE PIC X(1).
             88 ACTIVE   VALUE 'A'.
             88 INACTIVE VALUE 'I'.
          05 TYPE-CODE   PIC X(2).
       66 STATUS-RANGE RENAMES STATUS-CODE THRU TYPE-CODE.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "STATUS-RANGE")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    // Level-88 conditions are non-storage; only STATUS-CODE and TYPE-CODE counted
    assert_eq!(rr.members.len(), 2);
    assert!(rr.members.iter().any(|m| m.contains("STATUS-CODE")));
    assert!(rr.members.iter().any(|m| m.contains("TYPE-CODE")));
}

// ═══════════════════════════════════════════════════════════════════════════
// Multiple RENAMES in same record
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn multiple_renames_in_same_record() {
    let cb = "
       01 REC.
          05 PART-1 PIC X(10).
          05 PART-2 PIC X(10).
          05 PART-3 PIC X(10).
       66 FIRST-HALF  RENAMES PART-1 THRU PART-2.
       66 SECOND-HALF RENAMES PART-2 THRU PART-3.
       66 FULL-REC    RENAMES PART-1 THRU PART-3.
    ";
    let schema = parse_copybook(cb).expect("parse");

    let renames_fields: Vec<_> = record_children(&schema)
        .iter()
        .filter(|f| f.level == 66)
        .collect();
    assert_eq!(renames_fields.len(), 3);

    // Verify each alias
    let first_half = renames_fields
        .iter()
        .find(|f| f.name == "FIRST-HALF")
        .unwrap();
    assert_eq!(
        first_half.resolved_renames.as_ref().unwrap().members.len(),
        2
    );

    let second_half = renames_fields
        .iter()
        .find(|f| f.name == "SECOND-HALF")
        .unwrap();
    assert_eq!(
        second_half.resolved_renames.as_ref().unwrap().members.len(),
        2
    );

    let full_rec = renames_fields
        .iter()
        .find(|f| f.name == "FULL-REC")
        .unwrap();
    assert_eq!(full_rec.resolved_renames.as_ref().unwrap().members.len(), 3);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES offset calculation
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_offset_calculation() {
    let cb = "
       01 REC.
          05 A PIC X(10).
          05 B PIC X(20).
          05 C PIC X(30).
       66 BC RENAMES B THRU C.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let kids = record_children(&schema);

    let b = kids.iter().find(|f| f.name == "B").unwrap();
    let alias = kids.iter().find(|f| f.name == "BC").unwrap();
    let rr = alias.resolved_renames.as_ref().unwrap();

    // B starts at offset 10 (after A's 10 bytes)
    assert_eq!(b.offset, 10);
    assert_eq!(rr.offset, 10);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES length calculation
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_length_calculation() {
    let cb = "
       01 REC.
          05 A PIC X(10).
          05 B PIC X(20).
          05 C PIC X(30).
       66 BC RENAMES B THRU C.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let kids = record_children(&schema);

    let alias = kids.iter().find(|f| f.name == "BC").unwrap();
    let rr = alias.resolved_renames.as_ref().unwrap();

    // B(20) + C(30) = 50
    assert_eq!(rr.length, 50);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES serialization/deserialization
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_resolved_serde_round_trip() {
    let rr = ResolvedRenames {
        offset: 42,
        length: 100,
        members: vec![
            "REC.FIELD-A".to_string(),
            "REC.FIELD-B".to_string(),
            "REC.FIELD-C".to_string(),
        ],
    };

    let json = serde_json::to_string(&rr).unwrap();
    let de: ResolvedRenames = serde_json::from_str(&json).unwrap();

    assert_eq!(de.offset, 42);
    assert_eq!(de.length, 100);
    assert_eq!(de.members.len(), 3);
    assert_eq!(de.members[0], "REC.FIELD-A");
    assert_eq!(de.members[2], "REC.FIELD-C");
}

#[test]
fn renames_schema_serde_round_trip() {
    let cb = "
       01 REC.
          05 F1 PIC X(5).
          05 F2 PIC X(5).
       66 ALIAS RENAMES F1 THRU F2.
    ";
    let schema = parse_copybook(cb).expect("parse");

    let json = serde_json::to_string(&schema).unwrap();
    let de: Schema = serde_json::from_str(&json).unwrap();

    let alias = de
        .all_fields()
        .into_iter()
        .find(|f| f.name == "ALIAS" && f.level == 66)
        .expect("alias");
    let rr = alias.resolved_renames.as_ref().expect("resolved");
    assert_eq!(rr.members.len(), 2);
}

// ═══════════════════════════════════════════════════════════════════════════
// Error: RENAMES non-existent field (CBKS601/CBKS602)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn error_renames_unknown_from_field() {
    let cb = "
       01 REC.
          05 A PIC X(5).
       66 ALIAS RENAMES BOGUS THRU A.
    ";
    let err = parse_copybook(cb).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS601_RENAME_UNKNOWN_FROM);
    assert!(err.to_string().contains("BOGUS"));
}

#[test]
fn error_renames_unknown_thru_field() {
    let cb = "
       01 REC.
          05 A PIC X(5).
       66 ALIAS RENAMES A THRU MISSING.
    ";
    let err = parse_copybook(cb).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS602_RENAME_UNKNOWN_THRU);
    assert!(err.to_string().contains("MISSING"));
}

// ═══════════════════════════════════════════════════════════════════════════
// Error: RENAMES reversed range (CBKS604)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn error_renames_reversed_range() {
    let cb = "
       01 REC.
          05 A PIC X(5).
          05 B PIC X(5).
       66 ALIAS RENAMES B THRU A.
    ";
    let err = parse_copybook(cb).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS604_RENAME_REVERSED_RANGE);
}

// ═══════════════════════════════════════════════════════════════════════════
// Error: RENAMES across group boundary (CBKS605/CBKS606)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn error_renames_from_crosses_group() {
    let cb = "
       01 REC.
          05 GRP.
             10 GA PIC X(5).
             10 GB PIC X(5).
          05 AFTER PIC X(3).
       66 ALIAS RENAMES GRP THRU AFTER.
    ";
    let err = parse_copybook(cb).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS605_RENAME_FROM_CROSSES_GROUP);
}

#[test]
fn error_renames_thru_crosses_group() {
    let cb = "
       01 REC.
          05 BEFORE PIC X(3).
          05 GRP.
             10 GA PIC X(5).
       66 ALIAS RENAMES BEFORE THRU GRP.
    ";
    let err = parse_copybook(cb).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS606_RENAME_THRU_CROSSES_GROUP);
}

// ═══════════════════════════════════════════════════════════════════════════
// Error: RENAMES across OCCURS boundary (CBKS607)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn error_renames_crosses_occurs() {
    let cb = "
       01 REC.
          05 BEFORE    PIC X(5).
          05 ARR-FIELD PIC X(3) OCCURS 5 TIMES.
          05 AFTER     PIC X(2).
       66 ALIAS RENAMES BEFORE THRU AFTER.
    ";
    let err = parse_copybook(cb).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKS607_RENAME_CROSSES_OCCURS);
}

// ═══════════════════════════════════════════════════════════════════════════
// Schema API: lookup by alias name (find_field_or_alias)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn schema_api_find_field_or_alias() {
    let cb = "
       01 REC.
          05 CUSTOMER-INFO.
             10 NAME    PIC X(30).
             10 ADDRESS PIC X(60).
       66 CUST-ALIAS RENAMES CUSTOMER-INFO THRU CUSTOMER-INFO.
    ";
    let schema = parse_copybook(cb).expect("parse");

    // Alias lookup by name
    let alias = schema
        .find_field_or_alias("CUST-ALIAS")
        .expect("find alias");
    assert_eq!(alias.name, "CUST-ALIAS");
    assert_eq!(alias.level, 66);
    assert!(alias.resolved_renames.is_some());

    // Case-insensitive lookup
    assert!(schema.find_field_or_alias("cust-alias").is_some());
    assert!(schema.find_field_or_alias("Cust-Alias").is_some());

    // Non-existent name returns None
    assert!(schema.find_field_or_alias("DOES-NOT-EXIST").is_none());
}

// ═══════════════════════════════════════════════════════════════════════════
// Schema API: resolve alias to storage fields (resolve_alias_to_target)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn schema_api_resolve_alias_to_target() {
    let cb = "
       01 REC.
          05 FIRST-NAME PIC X(20).
          05 LAST-NAME  PIC X(30).
       66 FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
    ";
    let schema = parse_copybook(cb).expect("parse");

    // Resolving alias returns the first member field
    let target = schema
        .resolve_alias_to_target("FULL-NAME")
        .expect("resolve");
    assert_eq!(target.name, "FIRST-NAME");
    assert_eq!(target.level, 5);
}

#[test]
fn schema_api_resolve_non_alias_falls_back() {
    let cb = "
       01 REC.
          05 AMOUNT PIC 9(5).
    ";
    let schema = parse_copybook(cb).expect("parse");

    // Resolving a regular field path returns that field directly
    let target = schema
        .resolve_alias_to_target("REC.AMOUNT")
        .expect("resolve");
    assert_eq!(target.name, "AMOUNT");
}

// ═══════════════════════════════════════════════════════════════════════════
// Projection: RENAMES alias expansion
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn projection_renames_alias_expands_to_members() {
    let cb = "
       01 REC.
          05 FIRST-NAME     PIC X(20).
          05 LAST-NAME      PIC X(30).
          05 MIDDLE-INITIAL PIC X(1).
       66 FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
       05 ADDRESS           PIC X(50).
    ";
    let schema = parse_copybook(cb).expect("parse");

    let projected = project_schema(&schema, &["FULL-NAME".to_string()]).expect("project");

    let child_names: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();

    assert!(child_names.contains(&"FIRST-NAME"));
    assert!(child_names.contains(&"LAST-NAME"));
    assert!(!child_names.contains(&"MIDDLE-INITIAL"));
    assert!(!child_names.contains(&"ADDRESS"));
    // Alias itself should not appear as a storage field
    assert!(!child_names.contains(&"FULL-NAME"));
}

#[test]
fn projection_renames_alias_with_regular_fields() {
    let cb = "
       01 REC.
          05 FIRST-NAME PIC X(20).
          05 LAST-NAME  PIC X(30).
       66 FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
          05 ID         PIC 9(6).
    ";
    let schema = parse_copybook(cb).expect("parse");

    let projected =
        project_schema(&schema, &["FULL-NAME".to_string(), "ID".to_string()]).expect("project");

    let child_names: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();

    assert_eq!(child_names.len(), 3);
    assert!(child_names.contains(&"FIRST-NAME"));
    assert!(child_names.contains(&"LAST-NAME"));
    assert!(child_names.contains(&"ID"));
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES mixed field types (alphanumeric + numeric + COMP-3)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_mixed_field_types() {
    let cb = "
       01 REC.
          05 NAME     PIC X(20).
          05 AMOUNT   PIC 9(7)V99.
          05 PKD-VAL  PIC S9(5) COMP-3.
       66 ALL-DATA RENAMES NAME THRU PKD-VAL.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "ALL-DATA")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 3);

    // Verify length spans all three fields
    let name_f = record_children(&schema)
        .iter()
        .find(|f| f.name == "NAME")
        .unwrap();
    let pkd_f = record_children(&schema)
        .iter()
        .find(|f| f.name == "PKD-VAL")
        .unwrap();
    assert_eq!(rr.length, (pkd_f.offset + pkd_f.len) - name_f.offset);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES single-field without THRU (parser should treat as FROM==THRU)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_single_field_no_thru() {
    let cb = "
       01 REC.
          05 AMOUNT PIC 9(5).
       66 AMT-ALIAS RENAMES AMOUNT.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "AMT-ALIAS")
        .expect("alias");

    match &alias.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "AMOUNT");
            assert_eq!(thru_field, "AMOUNT");
        }
        other => panic!("Expected Renames, got {:?}", other),
    }

    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 1);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES FieldKind variant inspection
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_field_kind_variant() {
    let cb = "
       01 REC.
          05 A PIC X(10).
          05 B PIC X(10).
       66 ALIAS RENAMES A THRU B.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "ALIAS")
        .expect("alias");

    assert_eq!(alias.level, 66);
    assert!(matches!(
        alias.kind,
        FieldKind::Renames {
            ref from_field,
            ref thru_field,
        } if from_field == "A" && thru_field == "B"
    ));
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES qualified name support
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_qualified_names() {
    let cb = "
       01 REC.
          05 F1 PIC X(3).
          05 F2 PIC X(2).
          05 F3 PIC X(1).
       66 ALIAS RENAMES F1 OF REC THRU F3 OF REC.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "ALIAS")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    assert_eq!(rr.members.len(), 3);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES contiguity: Level-88 does not break range
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_range_ignores_level88() {
    let cb = "
       01 REC.
          05 A PIC X(5).
             88 A-VAL VALUE 'HELLO'.
          05 B PIC 9(3).
       66 AB RENAMES A THRU B.
    ";
    let schema = parse_copybook(cb).expect("parse");
    let alias = record_children(&schema)
        .iter()
        .find(|f| f.name == "AB")
        .expect("alias");

    let rr = alias.resolved_renames.as_ref().unwrap();
    // Only storage-bearing fields: A and B (Level-88 excluded)
    assert_eq!(rr.members.len(), 2);
}

// ═══════════════════════════════════════════════════════════════════════════
// RENAMES does not consume storage
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn renames_does_not_consume_storage() {
    let cb = "
       01 REC.
          05 A PIC X(10).
          05 B PIC X(20).
       66 ALIAS RENAMES A THRU B.
    ";
    let schema = parse_copybook(cb).expect("parse");

    // LRECL should be 30 (10 + 20), not affected by the 66-level alias
    assert_eq!(schema.lrecl_fixed, Some(30));
}
