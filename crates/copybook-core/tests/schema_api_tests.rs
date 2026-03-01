// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive Schema API tests for copybook-core
//!
//! Validates schema construction, field lookup, LRECL, fingerprint,
//! iteration, REDEFINES, OCCURS, ODO, Level-88, RENAMES, serde,
//! display/debug, field offsets, and field type categorization.

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]

use copybook_core::schema::ResolvedRenames;
use copybook_core::{
    Field, FieldKind, Occurs, ParseOptions, Schema, TailODO, parse_copybook,
    parse_copybook_with_options,
};

// ---------------------------------------------------------------------------
// Helper: build a simple field
// ---------------------------------------------------------------------------
fn alphanum_field(name: &str, path: &str, offset: u32, len: u32) -> Field {
    Field {
        path: path.to_string(),
        name: name.to_string(),
        level: 5,
        kind: FieldKind::Alphanum { len },
        offset,
        len,
        redefines_of: None,
        occurs: None,
        sync_padding: None,
        synchronized: false,
        blank_when_zero: false,
        resolved_renames: None,
        children: vec![],
    }
}

// ===========================================================================
// 1. Schema construction from parsed copybook
// ===========================================================================

#[test]
fn test_parse_simple_copybook_produces_schema() {
    let cb = r#"
       01  CUSTOMER-RECORD.
           05  CUST-ID     PIC 9(6).
           05  CUST-NAME   PIC X(30).
    "#;
    let schema = parse_copybook(cb).expect("parse should succeed");
    assert_eq!(schema.fields.len(), 1, "one root-level 01 group");
    assert_eq!(schema.fields[0].name, "CUSTOMER-RECORD");
    assert!(!schema.fingerprint.is_empty(), "fingerprint computed");
}

#[test]
fn test_parse_copybook_populates_lrecl() {
    let cb = r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC X(20).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    // Fixed-record copybook should have LRECL = total bytes of leaf fields
    assert!(schema.lrecl_fixed.is_some(), "lrecl_fixed should be set");
    assert_eq!(schema.lrecl_fixed.unwrap(), 30);
}

#[test]
fn test_schema_from_fields_computes_fingerprint() {
    let fields = vec![alphanum_field("F1", "ROOT.F1", 0, 10)];
    let schema = Schema::from_fields(fields);
    assert!(!schema.fingerprint.is_empty());
    assert_eq!(schema.fingerprint.len(), 64); // SHA-256 hex
}

// ===========================================================================
// 2. Field lookup by path (exact)
// ===========================================================================

#[test]
fn test_find_field_by_full_path() {
    let cb = r#"
       01  REC.
           05  GRP.
               10  LEAF  PIC X(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let found = schema.find_field("REC.GRP.LEAF");
    assert!(found.is_some());
    assert_eq!(found.unwrap().name, "LEAF");
}

#[test]
fn test_find_field_returns_none_for_missing() {
    let schema = Schema::new();
    assert!(schema.find_field("NONEXISTENT").is_none());
}

#[test]
fn test_find_field_returns_none_for_partial_path() {
    let cb = r#"
       01  REC.
           05  GRP.
               10  LEAF  PIC X(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    // Short name should not match (find_field needs full path)
    assert!(schema.find_field("LEAF").is_none());
}

// ===========================================================================
// 3. Field lookup by alias (case-insensitive for RENAMES)
// ===========================================================================

#[test]
fn test_find_field_or_alias_case_insensitive() {
    let cb = r#"
       01  REC.
           05  INFO.
               10  NAME     PIC X(30).
               10  ADDRESS  PIC X(60).
           66  MY-ALIAS RENAMES INFO THRU INFO.
    "#;
    let schema = parse_copybook(cb).expect("parse");
    // Case-insensitive alias lookup
    let found = schema.find_field_or_alias("my-alias");
    assert!(found.is_some());
    assert_eq!(found.unwrap().level, 66);
}

// ===========================================================================
// 4. LRECL calculation – fixed records
// ===========================================================================

#[test]
fn test_lrecl_fixed_single_field() {
    let cb = r#"
       01  REC.
           05  A PIC X(42).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    assert_eq!(schema.lrecl_fixed, Some(42));
}

#[test]
fn test_lrecl_fixed_multiple_fields() {
    let cb = r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC 9(5).
           05  C PIC X(20).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    assert_eq!(schema.lrecl_fixed, Some(35));
}

// ===========================================================================
// 5. LRECL for variable records (ODO)
// ===========================================================================

#[test]
fn test_lrecl_none_for_odo_schema() {
    let cb = r#"
       01  REC.
           05  CNT       PIC 9(3).
           05  ITEMS     OCCURS 1 TO 10 TIMES
                         DEPENDING ON CNT
                         PIC X(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    // Variable-length records should NOT have a fixed LRECL
    assert!(
        schema.lrecl_fixed.is_none(),
        "ODO schema should have no fixed LRECL"
    );
    assert!(schema.tail_odo.is_some(), "tail_odo should be populated");
}

// ===========================================================================
// 6. Schema fingerprint stability
// ===========================================================================

#[test]
fn test_fingerprint_stable_same_input() {
    let cb = r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC 9(5).
    "#;
    let s1 = parse_copybook(cb).expect("parse 1");
    let s2 = parse_copybook(cb).expect("parse 2");
    assert_eq!(
        s1.fingerprint, s2.fingerprint,
        "same input → same fingerprint"
    );
}

// ===========================================================================
// 7. Schema fingerprint sensitivity
// ===========================================================================

#[test]
fn test_fingerprint_differs_for_different_input() {
    let cb1 = r#"
       01  REC.
           05  A PIC X(10).
    "#;
    let cb2 = r#"
       01  REC.
           05  A PIC X(20).
    "#;
    let s1 = parse_copybook(cb1).expect("parse 1");
    let s2 = parse_copybook(cb2).expect("parse 2");
    assert_ne!(
        s1.fingerprint, s2.fingerprint,
        "different input → different fingerprint"
    );
}

// ===========================================================================
// 8. all_fields iteration
// ===========================================================================

#[test]
fn test_all_fields_includes_groups_and_leaves() {
    let cb = r#"
       01  REC.
           05  GRP.
               10  L1 PIC X(5).
               10  L2 PIC X(5).
           05  TOP PIC X(10).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let all = schema.all_fields();
    // REC(group), GRP(group), L1, L2, TOP = 5
    assert!(
        all.len() >= 5,
        "expected at least 5 fields, got {}",
        all.len()
    );
    assert!(all.iter().any(|f| f.name == "REC"));
    assert!(all.iter().any(|f| f.name == "GRP"));
    assert!(all.iter().any(|f| f.name == "L1"));
    assert!(all.iter().any(|f| f.name == "L2"));
    assert!(all.iter().any(|f| f.name == "TOP"));
}

#[test]
fn test_all_fields_preorder_traversal() {
    let cb = r#"
       01  REC.
           05  GRP.
               10  CHILD PIC X(5).
           05  AFTER PIC X(10).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let names: Vec<&str> = schema
        .all_fields()
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    // Pre-order: REC → GRP → CHILD → AFTER
    let grp_idx = names.iter().position(|n| *n == "GRP").unwrap();
    let child_idx = names.iter().position(|n| *n == "CHILD").unwrap();
    let after_idx = names.iter().position(|n| *n == "AFTER").unwrap();
    assert!(grp_idx < child_idx, "GRP before CHILD");
    assert!(child_idx < after_idx, "CHILD before AFTER");
}

// ===========================================================================
// 9. Leaf fields only iteration
// ===========================================================================

#[test]
fn test_leaf_fields_only() {
    let cb = r#"
       01  REC.
           05  GRP.
               10  L1 PIC X(5).
               10  L2 PIC 9(3).
           05  L3 PIC X(10).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let leaves: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.is_scalar())
        .collect();
    // L1, L2, L3 are scalars
    assert!(
        leaves.len() >= 3,
        "expected at least 3 leaves, got {}",
        leaves.len()
    );
    assert!(leaves.iter().all(|f| !f.is_group()));
}

// ===========================================================================
// 10. Group fields only iteration
// ===========================================================================

#[test]
fn test_group_fields_only() {
    let cb = r#"
       01  REC.
           05  GRP.
               10  L1 PIC X(5).
           05  L2 PIC X(10).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let groups: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.is_group())
        .collect();
    // REC and GRP are groups
    assert!(
        groups.len() >= 2,
        "expected at least 2 groups, got {}",
        groups.len()
    );
    assert!(groups.iter().all(|f| f.is_group()));
}

// ===========================================================================
// 11. Schema with REDEFINES
// ===========================================================================

#[test]
fn test_redefines_field_overlay() {
    let cb = r#"
       01  REC.
           05  BASE-FIELD   PIC X(10).
           05  ALT-FIELD    REDEFINES BASE-FIELD PIC 9(10).
    "#;
    let schema = parse_copybook(cb).expect("parse");

    let alt = schema.find_field("REC.ALT-FIELD").expect("find ALT-FIELD");
    assert!(alt.redefines_of.is_some(), "redefines_of populated");

    // REDEFINES occupies the same offset
    let base = schema
        .find_field("REC.BASE-FIELD")
        .expect("find BASE-FIELD");
    assert_eq!(base.offset, alt.offset, "redefines shares offset");
}

#[test]
fn test_find_redefining_fields() {
    let cb = r#"
       01  REC.
           05  BASE-FIELD   PIC X(10).
           05  ALT-FIELD    REDEFINES BASE-FIELD PIC 9(10).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    // The parser stores redefines_of as the bare target name
    let alt = schema.find_field("REC.ALT-FIELD").expect("find ALT-FIELD");
    let target = alt.redefines_of.as_ref().expect("has redefines_of");
    let redefining = schema.find_redefining_fields(target);
    assert_eq!(redefining.len(), 1);
    assert_eq!(redefining[0].name, "ALT-FIELD");
}

// ===========================================================================
// 12. Schema with OCCURS (fixed arrays)
// ===========================================================================

#[test]
fn test_fixed_occurs() {
    let cb = r#"
       01  REC.
           05  ITEMS OCCURS 5 TIMES PIC X(10).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let items = schema.find_field("REC.ITEMS").expect("find ITEMS");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 5 })));
    assert_eq!(items.effective_length(), 50); // 10 * 5
}

#[test]
fn test_fixed_occurs_lrecl() {
    let cb = r#"
       01  REC.
           05  HDR PIC X(2).
           05  ITEMS OCCURS 3 TIMES PIC X(10).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    assert_eq!(schema.lrecl_fixed, Some(32)); // 2 + 3*10
}

// ===========================================================================
// 13. Schema with ODO (variable arrays)
// ===========================================================================

#[test]
fn test_odo_schema_has_tail_odo() {
    let cb = r#"
       01  REC.
           05  CNT       PIC 9(3).
           05  ITEMS     OCCURS 1 TO 10 TIMES
                         DEPENDING ON CNT
                         PIC X(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let tail = schema.tail_odo.as_ref().expect("tail_odo present");
    assert_eq!(tail.min_count, 1);
    assert_eq!(tail.max_count, 10);
    assert!(tail.counter_path.contains("CNT"));
    assert!(tail.array_path.contains("ITEMS"));
}

#[test]
fn test_odo_field_effective_length_uses_max() {
    let cb = r#"
       01  REC.
           05  CNT       PIC 9(3).
           05  ITEMS     OCCURS 1 TO 10 TIMES
                         DEPENDING ON CNT
                         PIC X(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let items = schema.find_field("REC.ITEMS").expect("find ITEMS");
    assert_eq!(items.effective_length(), 50); // 5 * 10 (max)
}

// ===========================================================================
// 14. Schema with Level-88 conditions
// ===========================================================================

#[test]
fn test_level88_condition_values() {
    let cb = r#"
       01  REC.
           05  STATUS PIC X(1).
               88  ACTIVE   VALUE 'A'.
               88  INACTIVE VALUE 'I'.
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let all = schema.all_fields();
    let conditions: Vec<_> = all
        .iter()
        .filter(|f| matches!(f.kind, FieldKind::Condition { .. }))
        .collect();
    assert!(
        conditions.len() >= 2,
        "expected 2 condition fields, got {}",
        conditions.len()
    );

    // Check names
    let cond_names: Vec<&str> = conditions.iter().map(|f| f.name.as_str()).collect();
    assert!(cond_names.contains(&"ACTIVE"));
    assert!(cond_names.contains(&"INACTIVE"));
}

#[test]
fn test_level88_is_not_scalar_storage() {
    let cb = r#"
       01  REC.
           05  STATUS PIC X(1).
               88  ACTIVE VALUE 'A'.
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let all = schema.all_fields();
    let cond = all
        .iter()
        .find(|f| f.name == "ACTIVE")
        .expect("find ACTIVE");
    assert_eq!(cond.level, 88);
    assert_eq!(cond.len, 0, "Level-88 has no storage");
}

// ===========================================================================
// 15. Schema with RENAMES (Level-66)
// ===========================================================================

#[test]
fn test_renames_level66_found_by_alias() {
    let cb = r#"
       01  REC.
           05  INFO.
               10  NAME     PIC X(30).
               10  ADDRESS  PIC X(60).
           66  ALIAS RENAMES INFO THRU INFO.
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let alias = schema.find_field_or_alias("ALIAS").expect("find alias");
    assert_eq!(alias.level, 66);
    assert!(alias.resolved_renames.is_some());
}

#[test]
fn test_resolve_alias_to_target() {
    let cb = r#"
       01  REC.
           05  INFO.
               10  NAME     PIC X(30).
               10  ADDRESS  PIC X(60).
           66  ALIAS RENAMES INFO THRU INFO.
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let target = schema
        .resolve_alias_to_target("ALIAS")
        .expect("resolve alias");
    // Should resolve to the first member of the aliased range
    assert_ne!(target.level, 66, "target should not be the alias itself");
}

// ===========================================================================
// 16. Empty schema edge case
// ===========================================================================

#[test]
fn test_empty_schema() {
    let schema = Schema::new();
    assert!(schema.fields.is_empty());
    assert!(schema.lrecl_fixed.is_none());
    assert!(schema.tail_odo.is_none());
    assert!(schema.fingerprint.is_empty());
    assert!(schema.all_fields().is_empty());
    assert!(schema.find_field("X").is_none());
    assert!(schema.find_field_or_alias("X").is_none());
    assert!(schema.find_redefining_fields("X").is_empty());
}

#[test]
fn test_default_schema_is_empty() {
    let schema = Schema::default();
    assert!(schema.fields.is_empty());
    assert!(schema.fingerprint.is_empty());
}

// ===========================================================================
// 17. Schema serde (JSON roundtrip)
// ===========================================================================

#[test]
fn test_schema_json_roundtrip() {
    let cb = r#"
       01  REC.
           05  ALPHA PIC X(10).
           05  NUM   PIC 9(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let json = serde_json::to_string(&schema).expect("serialize");
    let deser: Schema = serde_json::from_str(&json).expect("deserialize");

    assert_eq!(deser.fields.len(), schema.fields.len());
    assert_eq!(deser.lrecl_fixed, schema.lrecl_fixed);
    assert_eq!(deser.fingerprint, schema.fingerprint);
}

#[test]
fn test_schema_json_roundtrip_with_odo() {
    let cb = r#"
       01  REC.
           05  CNT   PIC 9(3).
           05  ITEMS OCCURS 1 TO 10 TIMES
                     DEPENDING ON CNT
                     PIC X(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let json = serde_json::to_string(&schema).expect("serialize");
    let deser: Schema = serde_json::from_str(&json).expect("deserialize");

    assert!(deser.tail_odo.is_some());
    let tail = deser.tail_odo.as_ref().unwrap();
    assert_eq!(tail.min_count, schema.tail_odo.as_ref().unwrap().min_count);
    assert_eq!(tail.max_count, schema.tail_odo.as_ref().unwrap().max_count);
}

#[test]
fn test_field_kind_json_roundtrip_all_variants() {
    let kinds = vec![
        FieldKind::Alphanum { len: 10 },
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
            sign_separate: None,
        },
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true,
        },
        FieldKind::Group,
        FieldKind::Condition {
            values: vec!["A".to_string(), "B".to_string()],
        },
        FieldKind::Renames {
            from_field: "F1".to_string(),
            thru_field: "F2".to_string(),
        },
        FieldKind::EditedNumeric {
            pic_string: "ZZ9.99".to_string(),
            width: 6,
            scale: 2,
            signed: false,
        },
        FieldKind::FloatSingle,
        FieldKind::FloatDouble,
    ];
    for kind in kinds {
        let json = serde_json::to_string(&kind).expect("serialize");
        let deser: FieldKind = serde_json::from_str(&json).expect("deserialize");
        let re_json = serde_json::to_string(&deser).expect("re-serialize");
        assert_eq!(json, re_json, "roundtrip mismatch for {json}");
    }
}

// ===========================================================================
// 18. Schema display/debug output
// ===========================================================================

#[test]
fn test_schema_debug_not_empty() {
    let schema = Schema::new();
    let dbg = format!("{schema:?}");
    assert!(!dbg.is_empty());
    assert!(dbg.contains("Schema"));
}

#[test]
fn test_field_debug_contains_name() {
    let field = Field::new(5, "MY-FIELD".to_string());
    let dbg = format!("{field:?}");
    assert!(dbg.contains("MY-FIELD"));
}

#[test]
fn test_field_kind_debug_variants() {
    let kind = FieldKind::Alphanum { len: 10 };
    let dbg = format!("{kind:?}");
    assert!(dbg.contains("Alphanum"));

    let kind = FieldKind::PackedDecimal {
        digits: 5,
        scale: 2,
        signed: true,
    };
    let dbg = format!("{kind:?}");
    assert!(dbg.contains("PackedDecimal"));
}

// ===========================================================================
// 19. Field offset validation (sequential, non-overlapping)
// ===========================================================================

#[test]
fn test_leaf_offsets_sequential() {
    let cb = r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC X(20).
           05  C PIC 9(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let all = schema.all_fields();
    let leaves: Vec<_> = all.iter().filter(|f| f.is_scalar()).collect();
    // Verify offsets are sequential and non-overlapping
    for window in leaves.windows(2) {
        let curr = window[0];
        let next = window[1];
        assert!(
            curr.offset + curr.len <= next.offset,
            "field {} (offset={}, len={}) overlaps with {} (offset={})",
            curr.name,
            curr.offset,
            curr.len,
            next.name,
            next.offset,
        );
    }
}

#[test]
fn test_field_offsets_sum_to_lrecl() {
    let cb = r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC 9(5).
           05  C PIC X(20).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let all = schema.all_fields();
    let leaves: Vec<_> = all.iter().filter(|f| f.is_scalar()).collect();
    let total: u32 = leaves.iter().map(|f| f.len).sum();
    assert_eq!(total, schema.lrecl_fixed.unwrap());
}

// ===========================================================================
// 20. Field type categorization
// ===========================================================================

#[test]
fn test_is_group() {
    let field = Field::new(1, "GRP".to_string());
    assert!(field.is_group());
    assert!(!field.is_scalar());
}

#[test]
fn test_is_scalar() {
    let field = Field::with_kind(5, "F".to_string(), FieldKind::Alphanum { len: 10 });
    assert!(field.is_scalar());
    assert!(!field.is_group());
}

#[test]
fn test_is_packed() {
    let field = Field::with_kind(
        5,
        "P".to_string(),
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
        },
    );
    assert!(field.is_packed());
    assert!(!field.is_binary());
}

#[test]
fn test_is_binary() {
    let field = Field::with_kind(
        5,
        "B".to_string(),
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
    );
    assert!(field.is_binary());
    assert!(!field.is_packed());
}

#[test]
fn test_is_filler() {
    let field = Field::new(5, "FILLER".to_string());
    assert!(field.is_filler());

    let field2 = Field::new(5, "filler".to_string());
    assert!(field2.is_filler());

    let field3 = Field::new(5, "NOT-FILLER".to_string());
    assert!(!field3.is_filler());
}

#[test]
fn test_effective_length_no_occurs() {
    let mut field = alphanum_field("F", "F", 0, 10);
    field.occurs = None;
    assert_eq!(field.effective_length(), 10);
}

#[test]
fn test_effective_length_fixed_occurs() {
    let mut field = alphanum_field("F", "F", 0, 10);
    field.occurs = Some(Occurs::Fixed { count: 5 });
    assert_eq!(field.effective_length(), 50);
}

#[test]
fn test_effective_length_odo_uses_max() {
    let mut field = alphanum_field("F", "F", 0, 10);
    field.occurs = Some(Occurs::ODO {
        min: 1,
        max: 100,
        counter_path: "C".to_string(),
    });
    assert_eq!(field.effective_length(), 1000);
}

// ===========================================================================
// Additional: parsed schema field types from real copybook
// ===========================================================================

#[test]
fn test_parsed_field_types_numeric() {
    let cb = r#"
       01  REC.
           05  ZD   PIC S9(5)V99.
           05  PD   PIC S9(5)V99 COMP-3.
           05  BIN  PIC S9(5) COMP.
    "#;
    let schema = parse_copybook(cb).expect("parse");

    let zd = schema.find_field("REC.ZD").expect("find ZD");
    assert!(matches!(zd.kind, FieldKind::ZonedDecimal { .. }));

    let pd = schema.find_field("REC.PD").expect("find PD");
    assert!(matches!(pd.kind, FieldKind::PackedDecimal { .. }));

    let bin = schema.find_field("REC.BIN").expect("find BIN");
    assert!(matches!(bin.kind, FieldKind::BinaryInt { .. }));
}

#[test]
fn test_parsed_field_types_alphanum() {
    let cb = r#"
       01  REC.
           05  NAME PIC X(30).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let name = schema.find_field("REC.NAME").expect("find NAME");
    assert!(matches!(name.kind, FieldKind::Alphanum { len: 30 }));
}

// ===========================================================================
// Additional: canonical JSON for fingerprinting
// ===========================================================================

#[test]
fn test_canonical_json_deterministic() {
    let cb = r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC 9(5).
    "#;
    let s1 = parse_copybook(cb).expect("parse 1");
    let s2 = parse_copybook(cb).expect("parse 2");
    assert_eq!(s1.create_canonical_json(), s2.create_canonical_json());
}

// ===========================================================================
// Additional: schema with nested groups
// ===========================================================================

#[test]
fn test_deeply_nested_groups() {
    let cb = r#"
       01  REC.
           05  L1.
               10  L2.
                   15  L3 PIC X(5).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    let leaf = schema.find_field("REC.L1.L2.L3");
    assert!(leaf.is_some());
    assert_eq!(leaf.unwrap().len, 5);
}

// ===========================================================================
// Additional: REDEFINES does not increase LRECL
// ===========================================================================

#[test]
fn test_redefines_does_not_increase_lrecl() {
    let cb = r#"
       01  REC.
           05  BASE  PIC X(20).
           05  ALT   REDEFINES BASE PIC X(20).
    "#;
    let schema = parse_copybook(cb).expect("parse");
    // LRECL should be 20 (REDEFINES occupies the same space)
    assert_eq!(schema.lrecl_fixed, Some(20));
}

// ===========================================================================
// Additional: parse_copybook_with_options
// ===========================================================================

#[test]
fn test_parse_with_options_default() {
    let cb = r#"
       01  REC.
           05  A PIC X(10).
    "#;
    let opts = ParseOptions::default();
    let schema = parse_copybook_with_options(cb, &opts).expect("parse with options");
    assert_eq!(schema.fields.len(), 1);
}

// ===========================================================================
// Additional: TailODO struct fields
// ===========================================================================

#[test]
fn test_tail_odo_struct_fields() {
    let tail = TailODO {
        counter_path: "REC.CNT".to_string(),
        min_count: 0,
        max_count: 50,
        array_path: "REC.ITEMS".to_string(),
    };
    let json = serde_json::to_string(&tail).expect("serialize");
    let deser: TailODO = serde_json::from_str(&json).expect("deserialize");
    assert_eq!(deser.counter_path, "REC.CNT");
    assert_eq!(deser.min_count, 0);
    assert_eq!(deser.max_count, 50);
    assert_eq!(deser.array_path, "REC.ITEMS");
}

// ===========================================================================
// Additional: ResolvedRenames serde
// ===========================================================================

#[test]
fn test_resolved_renames_json_roundtrip() {
    let renames = ResolvedRenames {
        offset: 10,
        length: 90,
        members: vec!["F1".to_string(), "F2".to_string(), "F3".to_string()],
    };
    let json = serde_json::to_string(&renames).expect("serialize");
    let deser: ResolvedRenames = serde_json::from_str(&json).expect("deserialize");
    assert_eq!(deser.offset, 10);
    assert_eq!(deser.length, 90);
    assert_eq!(deser.members.len(), 3);
}

// ===========================================================================
// Additional: blank_when_zero field attribute
// ===========================================================================

#[test]
fn test_blank_when_zero_attribute() {
    let mut field = alphanum_field("F", "F", 0, 5);
    assert!(!field.blank_when_zero);
    field.blank_when_zero = true;
    assert!(field.blank_when_zero);
}

// ===========================================================================
// Additional: Field::new defaults
// ===========================================================================

#[test]
fn test_field_new_defaults() {
    let field = Field::new(5, "TEST".to_string());
    assert_eq!(field.level, 5);
    assert_eq!(field.name, "TEST");
    assert_eq!(field.path, "TEST");
    assert!(field.is_group()); // default kind is Group
    assert_eq!(field.offset, 0);
    assert_eq!(field.len, 0);
    assert!(field.redefines_of.is_none());
    assert!(field.occurs.is_none());
    assert!(field.sync_padding.is_none());
    assert!(!field.synchronized);
    assert!(!field.blank_when_zero);
    assert!(field.resolved_renames.is_none());
    assert!(field.children.is_empty());
}
