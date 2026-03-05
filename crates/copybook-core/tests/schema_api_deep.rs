// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Deep Schema API tests for copybook-core.
//!
//! Covers field lookup, iteration, fingerprint stability, JSON serde,
//! clone/debug/display, field metadata, and group children access.

use copybook_core::schema::ResolvedRenames;
use copybook_core::{
    Field, FieldKind, Occurs, ParseOptions, Schema, TailODO, parse_copybook,
    parse_copybook_with_options,
};

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

fn parse(cpy: &str) -> Schema {
    parse_copybook(cpy).expect("parse should succeed")
}

fn find_field<'a>(schema: &'a Schema, name: &str) -> &'a Field {
    schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("field '{name}' not found"))
}

// ===========================================================================
// 1. Schema::new() / Schema::default()
// ===========================================================================

#[test]
fn test_schema_new_is_empty() {
    let s = Schema::new();
    assert!(s.fields.is_empty());
    assert!(s.lrecl_fixed.is_none());
    assert!(s.tail_odo.is_none());
    assert!(s.fingerprint.is_empty());
}

#[test]
fn test_schema_default_equals_new() {
    let a = Schema::new();
    let b = Schema::default();
    assert_eq!(a.fields.len(), b.fields.len());
    assert_eq!(a.fingerprint, b.fingerprint);
}

// ===========================================================================
// 2. Schema::from_fields computes fingerprint
// ===========================================================================

#[test]
fn test_from_fields_sets_fingerprint() {
    let f = Field::with_kind(5, "F".to_string(), FieldKind::Alphanum { len: 10 });
    let s = Schema::from_fields(vec![f]);
    assert!(!s.fingerprint.is_empty());
    assert_eq!(s.fingerprint.len(), 64, "SHA-256 hex = 64 chars");
}

#[test]
fn test_from_fields_empty_vec_still_has_fingerprint() {
    let s = Schema::from_fields(vec![]);
    assert!(!s.fingerprint.is_empty());
}

// ===========================================================================
// 3. find_field by full path
// ===========================================================================

#[test]
fn test_find_field_top_level() {
    let s = parse("01 REC.\n   05 LEAF PIC X(5).");
    assert!(s.find_field("REC").is_some());
    assert_eq!(s.find_field("REC").unwrap().name, "REC");
}

#[test]
fn test_find_field_nested() {
    let s = parse(
        r"
       01 REC.
          05 GRP.
             10 LEAF PIC X(5).
    ",
    );
    let f = s
        .find_field("REC.GRP.LEAF")
        .expect("should find nested field");
    assert_eq!(f.name, "LEAF");
    assert_eq!(f.len, 5);
}

#[test]
fn test_find_field_returns_none_for_missing() {
    let s = Schema::new();
    assert!(s.find_field("NOPE").is_none());
}

#[test]
fn test_find_field_requires_full_path() {
    let s = parse(
        r"
       01 REC.
          05 GRP.
             10 LEAF PIC X(5).
    ",
    );
    assert!(
        s.find_field("LEAF").is_none(),
        "short name should not match find_field"
    );
}

// ===========================================================================
// 4. find_field_or_alias
// ===========================================================================

#[test]
fn test_find_field_or_alias_finds_regular_field() {
    let s = parse("01 REC.\n   05 F PIC X(5).");
    let f = s.find_field_or_alias("REC").expect("find by path");
    assert_eq!(f.name, "REC");
}

#[test]
fn test_find_field_or_alias_returns_none_for_unknown() {
    let s = Schema::new();
    assert!(s.find_field_or_alias("GHOST").is_none());
}

#[test]
fn test_find_field_or_alias_finds_renames() {
    let s = parse(
        r"
       01 REC.
          05 INFO.
             10 NAME    PIC X(20).
             10 ADDR    PIC X(40).
          66 ALIAS RENAMES INFO THRU INFO.
    ",
    );
    let alias = s.find_field_or_alias("ALIAS").expect("find alias");
    assert_eq!(alias.level, 66);
}

#[test]
fn test_find_field_or_alias_case_insensitive_renames() {
    let s = parse(
        r"
       01 REC.
          05 INFO.
             10 NAME PIC X(20).
             10 ADDR PIC X(40).
          66 MY-ALIAS RENAMES INFO THRU INFO.
    ",
    );
    assert!(s.find_field_or_alias("my-alias").is_some());
    assert!(s.find_field_or_alias("MY-ALIAS").is_some());
}

// ===========================================================================
// 5. resolve_alias_to_target
// ===========================================================================

#[test]
fn test_resolve_alias_returns_non_66_for_alias() {
    let s = parse(
        r"
       01 REC.
          05 INFO.
             10 NAME PIC X(20).
             10 ADDR PIC X(40).
          66 ALIAS RENAMES INFO THRU INFO.
    ",
    );
    let target = s.resolve_alias_to_target("ALIAS").expect("resolve");
    assert_ne!(target.level, 66, "should resolve to storage field");
}

#[test]
fn test_resolve_alias_returns_field_itself_for_non_alias() {
    let s = parse("01 REC.\n   05 F PIC X(5).");
    let f = s.resolve_alias_to_target("REC").expect("resolve non-alias");
    assert_eq!(f.name, "REC");
}

#[test]
fn test_resolve_alias_returns_none_for_missing() {
    let s = Schema::new();
    assert!(s.resolve_alias_to_target("NOPE").is_none());
}

// ===========================================================================
// 6. find_redefining_fields
// ===========================================================================

#[test]
fn test_find_redefining_fields_found() {
    let s = parse(
        r"
       01 REC.
          05 BASE PIC X(10).
          05 ALT  REDEFINES BASE PIC 9(10).
    ",
    );
    let results = s.find_redefining_fields("BASE");
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "ALT");
}

#[test]
fn test_find_redefining_fields_none() {
    let s = parse("01 REC.\n   05 F PIC X(5).");
    let results = s.find_redefining_fields("F");
    assert!(results.is_empty());
}

#[test]
fn test_find_redefining_fields_empty_schema() {
    let s = Schema::new();
    assert!(s.find_redefining_fields("X").is_empty());
}

// ===========================================================================
// 7. all_fields iteration
// ===========================================================================

#[test]
fn test_all_fields_count_flat() {
    let s = parse(
        r"
       01 REC.
          05 A PIC X(5).
          05 B PIC X(5).
          05 C PIC X(5).
    ",
    );
    // REC + A + B + C = 4
    assert_eq!(s.all_fields().len(), 4);
}

#[test]
fn test_all_fields_count_nested() {
    let s = parse(
        r"
       01 REC.
          05 GRP.
             10 L1 PIC X(5).
             10 L2 PIC X(5).
          05 TOP PIC X(5).
    ",
    );
    // REC + GRP + L1 + L2 + TOP = 5
    assert!(s.all_fields().len() >= 5);
}

#[test]
fn test_all_fields_preorder() {
    let s = parse(
        r"
       01 REC.
          05 GRP.
             10 CHILD PIC X(5).
          05 AFTER PIC X(5).
    ",
    );
    let names: Vec<&str> = s.all_fields().iter().map(|f| f.name.as_str()).collect();
    let grp_pos = names.iter().position(|n| *n == "GRP").unwrap();
    let child_pos = names.iter().position(|n| *n == "CHILD").unwrap();
    let after_pos = names.iter().position(|n| *n == "AFTER").unwrap();
    assert!(grp_pos < child_pos);
    assert!(child_pos < after_pos);
}

#[test]
fn test_all_fields_empty_schema() {
    let s = Schema::new();
    assert!(s.all_fields().is_empty());
}

#[test]
fn test_all_fields_includes_level88() {
    let s = parse(
        r"
       01 REC.
          05 FLAG PIC X(1).
             88 ON-VAL VALUE 'Y'.
    ",
    );
    let names: Vec<&str> = s.all_fields().iter().map(|f| f.name.as_str()).collect();
    assert!(names.contains(&"ON-VAL"));
}

// ===========================================================================
// 8. Fingerprint stability and sensitivity
// ===========================================================================

#[test]
fn test_fingerprint_stable_across_parses() {
    let cpy = "01 REC.\n   05 A PIC X(10).\n   05 B PIC 9(5).";
    let s1 = parse(cpy);
    let s2 = parse(cpy);
    assert_eq!(s1.fingerprint, s2.fingerprint);
}

#[test]
fn test_fingerprint_changes_with_different_field_length() {
    let s1 = parse("01 REC.\n   05 A PIC X(10).");
    let s2 = parse("01 REC.\n   05 A PIC X(11).");
    assert_ne!(s1.fingerprint, s2.fingerprint);
}

#[test]
fn test_fingerprint_changes_with_different_field_name() {
    let s1 = parse("01 REC.\n   05 AAA PIC X(10).");
    let s2 = parse("01 REC.\n   05 BBB PIC X(10).");
    assert_ne!(s1.fingerprint, s2.fingerprint);
}

#[test]
fn test_fingerprint_changes_with_different_field_type() {
    let s1 = parse("01 REC.\n   05 F PIC X(5).");
    let s2 = parse("01 REC.\n   05 F PIC 9(5).");
    assert_ne!(s1.fingerprint, s2.fingerprint);
}

#[test]
fn test_fingerprint_is_64_hex_chars() {
    let s = parse("01 REC.\n   05 F PIC X(1).");
    assert_eq!(s.fingerprint.len(), 64);
    assert!(s.fingerprint.chars().all(|c| c.is_ascii_hexdigit()));
}

// ===========================================================================
// 9. create_canonical_json determinism
// ===========================================================================

#[test]
fn test_canonical_json_deterministic() {
    let cpy = "01 REC.\n   05 A PIC X(10).\n   05 B PIC 9(5).";
    let s1 = parse(cpy);
    let s2 = parse(cpy);
    assert_eq!(s1.create_canonical_json(), s2.create_canonical_json());
}

#[test]
fn test_canonical_json_not_empty() {
    let s = parse("01 REC.\n   05 F PIC X(1).");
    let json = s.create_canonical_json();
    assert!(!json.is_empty());
    assert!(json.contains("fields"));
}

// ===========================================================================
// 10. JSON serde round-trips
// ===========================================================================

#[test]
fn test_schema_json_roundtrip_basic() {
    let s = parse("01 REC.\n   05 A PIC X(10).\n   05 B PIC 9(5).");
    let json = serde_json::to_string(&s).unwrap();
    let back: Schema = serde_json::from_str(&json).unwrap();
    assert_eq!(back.fields.len(), s.fields.len());
    assert_eq!(back.lrecl_fixed, s.lrecl_fixed);
    assert_eq!(back.fingerprint, s.fingerprint);
}

#[test]
fn test_schema_json_roundtrip_with_odo() {
    let s = parse(
        r"
       01 REC.
          05 CNT   PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES
                   DEPENDING ON CNT
                   PIC X(5).
    ",
    );
    let json = serde_json::to_string(&s).unwrap();
    let back: Schema = serde_json::from_str(&json).unwrap();
    assert!(back.tail_odo.is_some());
    let t = back.tail_odo.unwrap();
    assert_eq!(t.min_count, 1);
    assert_eq!(t.max_count, 10);
}

#[test]
fn test_schema_json_roundtrip_with_redefines() {
    let s = parse(
        r"
       01 REC.
          05 BASE PIC X(10).
          05 ALT  REDEFINES BASE PIC 9(10).
    ",
    );
    let json = serde_json::to_string(&s).unwrap();
    let back: Schema = serde_json::from_str(&json).unwrap();
    let alt = back
        .all_fields()
        .into_iter()
        .find(|f| f.name == "ALT")
        .unwrap();
    assert!(alt.redefines_of.is_some());
}

#[test]
fn test_field_kind_serde_alphanum() {
    let k = FieldKind::Alphanum { len: 42 };
    let json = serde_json::to_string(&k).unwrap();
    let back: FieldKind = serde_json::from_str(&json).unwrap();
    assert!(matches!(back, FieldKind::Alphanum { len: 42 }));
}

#[test]
fn test_field_kind_serde_packed_decimal() {
    let k = FieldKind::PackedDecimal {
        digits: 7,
        scale: 2,
        signed: true,
    };
    let json = serde_json::to_string(&k).unwrap();
    let back: FieldKind = serde_json::from_str(&json).unwrap();
    match back {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(digits, 7);
            assert_eq!(scale, 2);
            assert!(signed);
        }
        other => panic!("expected PackedDecimal, got {other:?}"),
    }
}

#[test]
fn test_field_kind_serde_condition() {
    let k = FieldKind::Condition {
        values: vec!["A".into(), "B".into()],
    };
    let json = serde_json::to_string(&k).unwrap();
    let back: FieldKind = serde_json::from_str(&json).unwrap();
    match back {
        FieldKind::Condition { values } => assert_eq!(values.len(), 2),
        other => panic!("expected Condition, got {other:?}"),
    }
}

#[test]
fn test_tail_odo_serde_roundtrip() {
    let t = TailODO {
        counter_path: "REC.CNT".into(),
        min_count: 0,
        max_count: 50,
        array_path: "REC.ITEMS".into(),
    };
    let json = serde_json::to_string(&t).unwrap();
    let back: TailODO = serde_json::from_str(&json).unwrap();
    assert_eq!(back.counter_path, "REC.CNT");
    assert_eq!(back.max_count, 50);
}

#[test]
fn test_resolved_renames_serde_roundtrip() {
    let r = ResolvedRenames {
        offset: 10,
        length: 90,
        members: vec!["F1".into(), "F2".into()],
    };
    let json = serde_json::to_string(&r).unwrap();
    let back: ResolvedRenames = serde_json::from_str(&json).unwrap();
    assert_eq!(back.offset, 10);
    assert_eq!(back.length, 90);
    assert_eq!(back.members, vec!["F1", "F2"]);
}

// ===========================================================================
// 11. Debug / Clone
// ===========================================================================

#[test]
fn test_schema_debug_contains_schema() {
    let s = Schema::new();
    let dbg = format!("{s:?}");
    assert!(dbg.contains("Schema"));
}

#[test]
fn test_schema_clone_equality() {
    let s = parse("01 REC.\n   05 F PIC X(10).");
    let c = s.clone();
    assert_eq!(c.fields.len(), s.fields.len());
    assert_eq!(c.fingerprint, s.fingerprint);
    assert_eq!(c.lrecl_fixed, s.lrecl_fixed);
}

#[test]
fn test_field_debug_contains_name() {
    let f = Field::new(5, "MY-FIELD".into());
    let dbg = format!("{f:?}");
    assert!(dbg.contains("MY-FIELD"));
}

#[test]
fn test_field_clone_preserves_attributes() {
    let mut f = Field::with_kind(5, "F".into(), FieldKind::Alphanum { len: 10 });
    f.offset = 42;
    f.len = 10;
    f.blank_when_zero = true;
    let c = f.clone();
    assert_eq!(c.offset, 42);
    assert_eq!(c.len, 10);
    assert!(c.blank_when_zero);
}

// ===========================================================================
// 12. Field::new / Field::with_kind defaults
// ===========================================================================

#[test]
fn test_field_new_defaults() {
    let f = Field::new(5, "TEST".into());
    assert_eq!(f.level, 5);
    assert_eq!(f.name, "TEST");
    assert_eq!(f.path, "TEST");
    assert!(f.is_group());
    assert_eq!(f.offset, 0);
    assert_eq!(f.len, 0);
    assert!(f.redefines_of.is_none());
    assert!(f.occurs.is_none());
    assert!(f.sync_padding.is_none());
    assert!(!f.synchronized);
    assert!(!f.blank_when_zero);
    assert!(f.resolved_renames.is_none());
    assert!(f.children.is_empty());
}

#[test]
fn test_field_with_kind_sets_kind() {
    let f = Field::with_kind(
        5,
        "BIN".into(),
        FieldKind::BinaryInt {
            bits: 32,
            signed: true,
        },
    );
    assert!(matches!(
        f.kind,
        FieldKind::BinaryInt {
            bits: 32,
            signed: true
        }
    ));
}

// ===========================================================================
// 13. Field type predicates
// ===========================================================================

#[test]
fn test_is_group_and_is_scalar() {
    let grp = Field::new(1, "G".into());
    assert!(grp.is_group());
    assert!(!grp.is_scalar());

    let leaf = Field::with_kind(5, "L".into(), FieldKind::Alphanum { len: 1 });
    assert!(!leaf.is_group());
    assert!(leaf.is_scalar());
}

#[test]
fn test_is_packed() {
    let f = Field::with_kind(
        5,
        "P".into(),
        FieldKind::PackedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
        },
    );
    assert!(f.is_packed());
    assert!(!f.is_binary());
}

#[test]
fn test_is_binary() {
    let f = Field::with_kind(
        5,
        "B".into(),
        FieldKind::BinaryInt {
            bits: 16,
            signed: false,
        },
    );
    assert!(f.is_binary());
    assert!(!f.is_packed());
}

#[test]
fn test_is_filler_case_insensitive() {
    assert!(Field::new(5, "FILLER".into()).is_filler());
    assert!(Field::new(5, "filler".into()).is_filler());
    assert!(Field::new(5, "Filler".into()).is_filler());
    assert!(!Field::new(5, "NOT-FILLER".into()).is_filler());
}

// ===========================================================================
// 14. effective_length
// ===========================================================================

#[test]
fn test_effective_length_no_occurs() {
    let mut f = Field::with_kind(5, "F".into(), FieldKind::Alphanum { len: 10 });
    f.len = 10;
    assert_eq!(f.effective_length(), 10);
}

#[test]
fn test_effective_length_fixed_occurs() {
    let mut f = Field::with_kind(5, "F".into(), FieldKind::Alphanum { len: 10 });
    f.len = 10;
    f.occurs = Some(Occurs::Fixed { count: 5 });
    assert_eq!(f.effective_length(), 50);
}

#[test]
fn test_effective_length_odo_uses_max() {
    let mut f = Field::with_kind(5, "F".into(), FieldKind::Alphanum { len: 8 });
    f.len = 8;
    f.occurs = Some(Occurs::ODO {
        min: 1,
        max: 20,
        counter_path: "C".into(),
    });
    assert_eq!(f.effective_length(), 160);
}

// ===========================================================================
// 15. sign_separate accessor
// ===========================================================================

#[test]
fn test_sign_separate_returns_none_for_non_zoned() {
    let f = Field::with_kind(5, "F".into(), FieldKind::Alphanum { len: 5 });
    assert!(f.sign_separate().is_none());
}

#[test]
fn test_sign_separate_returns_none_when_absent() {
    let f = Field::with_kind(
        5,
        "F".into(),
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 0,
            signed: true,
            sign_separate: None,
        },
    );
    assert!(f.sign_separate().is_none());
}

// ===========================================================================
// 16. LRECL computation edge cases
// ===========================================================================

#[test]
fn test_lrecl_with_occurs() {
    let s = parse(
        r"
       01 REC.
          05 HDR PIC X(2).
          05 ARR PIC X(10) OCCURS 3 TIMES.
    ",
    );
    assert_eq!(s.lrecl_fixed, Some(32)); // 2 + 3*10
}

#[test]
fn test_lrecl_odo_is_none() {
    let s = parse(
        r"
       01 REC.
          05 CNT PIC 9(2).
          05 ARR OCCURS 1 TO 10 TIMES DEPENDING ON CNT PIC X(5).
    ",
    );
    assert!(s.lrecl_fixed.is_none());
}

#[test]
fn test_lrecl_redefines_not_double_counted() {
    let s = parse(
        r"
       01 REC.
          05 A PIC X(10).
          05 B REDEFINES A PIC X(10).
          05 C PIC X(5).
    ",
    );
    assert_eq!(s.lrecl_fixed, Some(15));
}

// ===========================================================================
// 17. Schema from parsed copybook - field metadata access
// ===========================================================================

#[test]
fn test_parsed_field_offset_and_length() {
    let s = parse(
        r"
       01 REC.
          05 A PIC X(10).
          05 B PIC 9(5).
    ",
    );
    let a = s.find_field("REC.A").unwrap();
    assert_eq!(a.offset, 0);
    assert_eq!(a.len, 10);
    assert_eq!(a.level, 5);
    let b = s.find_field("REC.B").unwrap();
    assert_eq!(b.offset, 10);
    assert_eq!(b.len, 5);
}

#[test]
fn test_parsed_group_children_count() {
    let s = parse(
        r"
       01 REC.
          05 A PIC X(5).
          05 B PIC X(5).
          05 C PIC X(5).
    ",
    );
    let root = &s.fields[0];
    assert_eq!(root.name, "REC");
    assert!(root.is_group());
    assert_eq!(root.children.len(), 3);
}

#[test]
fn test_parsed_nested_group_children() {
    let s = parse(
        r"
       01 REC.
          05 GRP.
             10 L1 PIC X(3).
             10 L2 PIC X(7).
          05 TAIL PIC X(2).
    ",
    );
    let grp = find_field(&s, "GRP");
    assert!(grp.is_group());
    assert_eq!(grp.children.len(), 2);
    assert_eq!(grp.children[0].name, "L1");
    assert_eq!(grp.children[1].name, "L2");
}

// ===========================================================================
// 18. Level-88 fields in schema
// ===========================================================================

#[test]
fn test_level88_in_all_fields() {
    let s = parse(
        r"
       01 REC.
          05 STATUS PIC X(1).
             88 ACTIVE   VALUE 'A'.
             88 INACTIVE VALUE 'I'.
    ",
    );
    let conditions: Vec<_> = s
        .all_fields()
        .into_iter()
        .filter(|f| matches!(f.kind, FieldKind::Condition { .. }))
        .collect();
    assert_eq!(conditions.len(), 2);
}

#[test]
fn test_level88_zero_storage() {
    let s = parse(
        r"
       01 REC.
          05 CODE PIC X(1).
             88 YES VALUE 'Y'.
          05 DATA PIC X(10).
    ",
    );
    let yes = find_field(&s, "YES");
    assert_eq!(yes.len, 0, "level-88 has no storage");
    assert_eq!(yes.level, 88);
    let data = find_field(&s, "DATA");
    assert_eq!(data.offset, 1, "DATA right after 1-byte CODE");
}

// ===========================================================================
// 19. Leaf/group filtering
// ===========================================================================

#[test]
fn test_filter_scalar_fields() {
    let s = parse(
        r"
       01 REC.
          05 GRP.
             10 L1 PIC X(5).
             10 L2 PIC 9(3).
          05 L3 PIC X(10).
    ",
    );
    let scalars: Vec<_> = s
        .all_fields()
        .into_iter()
        .filter(|f| f.is_scalar())
        .collect();
    assert!(scalars.len() >= 3);
    assert!(scalars.iter().all(|f| !f.is_group()));
}

#[test]
fn test_filter_group_fields() {
    let s = parse(
        r"
       01 REC.
          05 GRP.
             10 LEAF PIC X(5).
    ",
    );
    let groups: Vec<_> = s
        .all_fields()
        .into_iter()
        .filter(|f| f.is_group())
        .collect();
    assert!(groups.len() >= 2); // REC and GRP
    assert!(groups.iter().all(|f| f.is_group()));
}

// ===========================================================================
// 20. Schema with multiple 01-level records
// ===========================================================================

#[test]
fn test_multiple_01_records_in_schema() {
    let s = parse(
        r"
       01 HDR.
          05 H-TYPE PIC X(4).
       01 DTL.
          05 D-KEY PIC 9(10).
       01 TRL.
          05 T-CNT PIC 9(8).
    ",
    );
    assert_eq!(s.fields.len(), 3);
    assert_eq!(s.fields[0].name, "HDR");
    assert_eq!(s.fields[1].name, "DTL");
    assert_eq!(s.fields[2].name, "TRL");
}

// ===========================================================================
// 21. Schema with OCCURS and children metadata
// ===========================================================================

#[test]
fn test_occurs_field_metadata() {
    let s = parse(
        r"
       01 REC.
          05 ITEMS OCCURS 5 TIMES.
             10 KEY PIC X(4).
             10 VAL PIC X(6).
    ",
    );
    let items = find_field(&s, "ITEMS");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 5 })));
    assert_eq!(items.children.len(), 2);
    assert_eq!(items.effective_length(), 50); // 10 * 5
}

// ===========================================================================
// 22. Schema with parse options
// ===========================================================================

#[test]
fn test_parse_with_emit_filler_option() {
    let opts = ParseOptions {
        emit_filler: true,
        ..ParseOptions::default()
    };
    let s = parse_copybook_with_options(
        r"
       01 REC.
          05 A      PIC X(5).
          05 FILLER PIC X(3).
          05 B      PIC X(5).
    ",
        &opts,
    )
    .unwrap();
    let root = &s.fields[0];
    assert!(root.children.len() >= 3);
}
