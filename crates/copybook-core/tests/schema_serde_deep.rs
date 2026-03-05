// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep schema serialization and API tests for copybook-core
//!
//! Validates JSON serde round-trips for every schema variant (simple,
//! OCCURS, ODO, REDEFINES, Level-88, RENAMES), field metadata
//! preservation, fingerprint determinism, and Schema query API.

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]

use copybook_core::{FieldKind, Occurs, Schema, parse_copybook};

// ---------------------------------------------------------------------------
// Helper: parse a copybook and return the schema
// ---------------------------------------------------------------------------
fn parse(cpy: &str) -> Schema {
    parse_copybook(cpy).expect("parse should succeed")
}

/// Serialize to JSON and deserialize back; return both the JSON string
/// and the reconstructed schema.
fn roundtrip(schema: &Schema) -> (String, Schema) {
    let json = serde_json::to_string_pretty(schema).expect("serialize");
    let back: Schema = serde_json::from_str(&json).expect("deserialize");
    (json, back)
}

// ===========================================================================
// 1. Serde JSON round-trips
// ===========================================================================

#[test]
fn schema_serde_json_roundtrip_simple() {
    let schema = parse(
        r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC 9(5).
    "#,
    );
    let (json, back) = roundtrip(&schema);
    assert!(!json.is_empty());
    assert_eq!(back.fields.len(), schema.fields.len());
    assert_eq!(back.fields[0].name, "REC");
    assert_eq!(back.fields[0].children.len(), 2);
}

#[test]
fn schema_serde_json_roundtrip_with_occurs() {
    let schema = parse(
        r#"
       01  REC.
           05  ITEMS OCCURS 5 TIMES.
               10  ITEM-VAL PIC X(8).
    "#,
    );
    let (_json, back) = roundtrip(&schema);
    let items = back.find_field("REC.ITEMS").expect("ITEMS must exist");
    assert!(
        matches!(items.occurs, Some(Occurs::Fixed { count: 5 })),
        "OCCURS 5 must survive round-trip"
    );
}

#[test]
fn schema_serde_json_roundtrip_with_odo() {
    let schema = parse(
        r#"
       01  REC.
           05  CNT     PIC 9(3).
           05  ITEMS   OCCURS 1 TO 10 DEPENDING ON CNT
                       PIC X(5).
    "#,
    );
    let (_json, back) = roundtrip(&schema);
    let items = back.find_field("REC.ITEMS").expect("ITEMS must exist");
    match &items.occurs {
        Some(Occurs::ODO {
            min,
            max,
            counter_path,
        }) => {
            assert_eq!(*min, 1);
            assert_eq!(*max, 10);
            assert!(
                counter_path.contains("CNT"),
                "counter_path should reference CNT, got {counter_path}"
            );
        }
        other => panic!("expected ODO, got {other:?}"),
    }
}

#[test]
fn schema_serde_json_roundtrip_with_redefines() {
    let schema = parse(
        r#"
       01  REC.
           05  A-NUM   PIC 9(6).
           05  A-TXT   REDEFINES A-NUM PIC X(6).
    "#,
    );
    let (_json, back) = roundtrip(&schema);
    let a_txt = back.find_field("REC.A-TXT").expect("A-TXT must exist");
    assert!(
        a_txt.redefines_of.is_some(),
        "redefines_of must survive round-trip"
    );
    assert!(a_txt.redefines_of.as_ref().unwrap().contains("A-NUM"));
}

#[test]
fn schema_serde_json_roundtrip_with_level88() {
    let schema = parse(
        r#"
       01  REC.
           05  STATUS-CODE PIC X(1).
               88  ACTIVE   VALUE 'A'.
               88  INACTIVE VALUE 'I'.
    "#,
    );
    let (_json, back) = roundtrip(&schema);
    let status = back
        .find_field("REC.STATUS-CODE")
        .expect("STATUS-CODE must exist");
    // Level-88 fields are children of their parent
    let cond_children: Vec<_> = status.children.iter().filter(|f| f.level == 88).collect();
    assert_eq!(cond_children.len(), 2, "two level-88 children");
    assert!(
        matches!(&cond_children[0].kind, FieldKind::Condition { values } if !values.is_empty()),
        "condition values must survive round-trip"
    );
}

#[test]
fn schema_serde_json_roundtrip_with_renames() {
    let schema = parse(
        r#"
       01  REC.
           05  FIRST-NAME  PIC X(10).
           05  LAST-NAME   PIC X(15).
           66  FULL-NAME   RENAMES FIRST-NAME THRU LAST-NAME.
    "#,
    );
    let (_json, back) = roundtrip(&schema);
    let alias = back
        .find_field_or_alias("FULL-NAME")
        .expect("FULL-NAME alias must be found");
    assert_eq!(alias.level, 66);
    assert!(
        matches!(&alias.kind, FieldKind::Renames { from_field, thru_field }
            if from_field.contains("FIRST-NAME") && thru_field.contains("LAST-NAME")),
        "RENAMES from/thru must survive round-trip"
    );
}

// ===========================================================================
// 2. Serde preserves field metadata
// ===========================================================================

#[test]
fn schema_serde_preserves_field_offsets() {
    let schema = parse(
        r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC X(20).
    "#,
    );
    let (_json, back) = roundtrip(&schema);
    let a = back.find_field("REC.A").unwrap();
    let b = back.find_field("REC.B").unwrap();
    assert_eq!(a.offset, 0, "A starts at 0");
    assert_eq!(b.offset, 10, "B starts at 10");
}

#[test]
fn schema_serde_preserves_field_sizes() {
    let schema = parse(
        r#"
       01  REC.
           05  A PIC X(7).
           05  B PIC 9(4)V99 COMP-3.
    "#,
    );
    let (_json, back) = roundtrip(&schema);
    let a = back.find_field("REC.A").unwrap();
    let b = back.find_field("REC.B").unwrap();
    assert_eq!(a.len, 7);
    // COMP-3: (6 digits + 1 sign) / 2 rounded up = 4 bytes
    assert!(b.len > 0, "packed decimal has non-zero length");
    assert_eq!(b.len, schema.find_field("REC.B").unwrap().len);
}

#[test]
fn schema_serde_preserves_field_types() {
    let schema = parse(
        r#"
       01  REC.
           05  A PIC X(5).
           05  B PIC 9(4).
           05  C PIC S9(7) COMP-3.
           05  D PIC S9(4) COMP.
    "#,
    );
    let (_json, back) = roundtrip(&schema);

    assert!(matches!(
        back.find_field("REC.A").unwrap().kind,
        FieldKind::Alphanum { .. }
    ));
    assert!(matches!(
        back.find_field("REC.B").unwrap().kind,
        FieldKind::ZonedDecimal { .. }
    ));
    assert!(matches!(
        back.find_field("REC.C").unwrap().kind,
        FieldKind::PackedDecimal { .. }
    ));
    assert!(matches!(
        back.find_field("REC.D").unwrap().kind,
        FieldKind::BinaryInt { .. }
    ));
}

#[test]
fn schema_serde_preserves_lrecl() {
    let schema = parse(
        r#"
       01  REC.
           05  A PIC X(100).
    "#,
    );
    let (_json, back) = roundtrip(&schema);
    assert_eq!(back.lrecl_fixed, schema.lrecl_fixed);
    assert!(
        back.lrecl_fixed.is_some(),
        "lrecl_fixed should be set for fixed-layout copybook"
    );
}

// ===========================================================================
// 3. Fingerprint
// ===========================================================================

#[test]
fn schema_fingerprint_is_deterministic() {
    let cpy = r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC 9(5).
    "#;
    let s1 = parse(cpy);
    let s2 = parse(cpy);
    assert_eq!(
        s1.fingerprint, s2.fingerprint,
        "same input → same fingerprint"
    );
    assert!(!s1.fingerprint.is_empty());
}

#[test]
fn schema_fingerprint_changes_with_field_changes() {
    let s1 = parse(
        r#"
       01  REC.
           05  A PIC X(10).
    "#,
    );
    let s2 = parse(
        r#"
       01  REC.
           05  A PIC X(20).
    "#,
    );
    assert_ne!(
        s1.fingerprint, s2.fingerprint,
        "different schemas must have different fingerprints"
    );
}

// ===========================================================================
// 4. Field counting and filtering
// ===========================================================================

#[test]
fn schema_field_count_includes_filler() {
    let schema = parse(
        r#"
       01  REC.
           05  A       PIC X(5).
           05  FILLER  PIC X(3).
           05  B       PIC X(7).
    "#,
    );
    let all = schema.all_fields();
    let filler_count = all.iter().filter(|f| f.is_filler()).count();
    assert!(filler_count >= 1, "FILLER must be included in all_fields");
    // REC (group) + A + FILLER + B = 4
    assert_eq!(all.len(), 4, "all_fields includes group + 3 leaf fields");
}

#[test]
fn schema_field_count_excludes_level88() {
    let schema = parse(
        r#"
       01  REC.
           05  CODE PIC X(1).
               88  YES VALUE 'Y'.
               88  NO  VALUE 'N'.
           05  AMT  PIC 9(5).
    "#,
    );
    let all = schema.all_fields();
    // all_fields returns everything including level-88
    let storage_fields: Vec<_> = all
        .iter()
        .filter(|f| !matches!(f.kind, FieldKind::Condition { .. }))
        .collect();
    // REC (group) + CODE + AMT = 3 storage fields
    assert_eq!(
        storage_fields.len(),
        3,
        "storage field count excludes level-88"
    );
    // But all_fields includes level-88 too
    assert!(
        all.len() > storage_fields.len(),
        "all_fields should include level-88 children"
    );
}

// ===========================================================================
// 5. Schema query API
// ===========================================================================

#[test]
fn schema_find_field_by_path() {
    let schema = parse(
        r#"
       01  REC.
           05  CUST-ID   PIC 9(6).
           05  CUST-NAME PIC X(30).
    "#,
    );
    assert!(schema.find_field("REC.CUST-ID").is_some());
    assert!(schema.find_field("REC.CUST-NAME").is_some());
    assert!(
        schema.find_field("REC.NONEXISTENT").is_none(),
        "missing field returns None"
    );
}

#[test]
fn schema_find_field_or_alias_case_insensitive() {
    let schema = parse(
        r#"
       01  REC.
           05  FIRST-NAME  PIC X(10).
           05  LAST-NAME   PIC X(15).
           66  FULL-NAME   RENAMES FIRST-NAME THRU LAST-NAME.
    "#,
    );
    // Alias lookup is case-insensitive for level-66 names
    let upper = schema.find_field_or_alias("FULL-NAME");
    let lower = schema.find_field_or_alias("full-name");
    assert!(upper.is_some(), "upper-case alias found");
    assert!(lower.is_some(), "lower-case alias found");
    assert_eq!(
        upper.unwrap().name,
        lower.unwrap().name,
        "both resolve to same field"
    );
}

#[test]
fn schema_has_field_works() {
    let schema = parse(
        r#"
       01  REC.
           05  ID  PIC 9(6).
           05  VAL PIC X(10).
    "#,
    );
    // has_field expressed as find_field().is_some()
    assert!(schema.find_field("REC.ID").is_some(), "ID exists");
    assert!(schema.find_field("REC.VAL").is_some(), "VAL exists");
    assert!(
        schema.find_field("REC.MISSING").is_none(),
        "MISSING does not exist"
    );
}

#[test]
fn schema_all_fields_iterator_covers_all_fields() {
    let schema = parse(
        r#"
       01  REC.
           05  GRP.
               10  A PIC X(3).
               10  B PIC X(4).
           05  C PIC 9(5).
    "#,
    );
    let all = schema.all_fields();
    let names: Vec<&str> = all.iter().map(|f| f.name.as_str()).collect();
    assert!(names.contains(&"REC"), "root group present");
    assert!(names.contains(&"GRP"), "nested group present");
    assert!(names.contains(&"A"), "nested leaf A present");
    assert!(names.contains(&"B"), "nested leaf B present");
    assert!(names.contains(&"C"), "sibling leaf C present");
    assert_eq!(all.len(), 5, "REC + GRP + A + B + C = 5");
}

// ===========================================================================
// 6. Clone, Debug, Display/canonical
// ===========================================================================

#[test]
fn schema_clone_equals_original() {
    let schema = parse(
        r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC 9(5).
    "#,
    );
    let cloned = schema.clone();
    // Schema doesn't derive PartialEq; compare via serialized JSON
    let orig_json = serde_json::to_string(&schema).unwrap();
    let clone_json = serde_json::to_string(&cloned).unwrap();
    assert_eq!(orig_json, clone_json, "clone must serialize identically");
    assert_eq!(schema.fingerprint, cloned.fingerprint);
}

#[test]
fn schema_debug_format_is_non_empty() {
    let schema = parse(
        r#"
       01  REC.
           05  A PIC X(5).
    "#,
    );
    let debug = format!("{schema:?}");
    assert!(!debug.is_empty(), "Debug format must not be empty");
    assert!(
        debug.contains("Schema"),
        "Debug output should contain type name"
    );
    assert!(
        debug.contains("fields"),
        "Debug output should reference fields"
    );
}

#[test]
fn schema_display_format_is_human_readable() {
    // Schema doesn't implement Display; use create_canonical_json() as the
    // human-readable serialization format.
    let schema = parse(
        r#"
       01  REC.
           05  A PIC X(10).
           05  B PIC 9(5).
    "#,
    );
    let canonical = schema.create_canonical_json();
    assert!(!canonical.is_empty(), "canonical JSON must not be empty");
    // Should be valid JSON
    let parsed: serde_json::Value =
        serde_json::from_str(&canonical).expect("canonical JSON must be valid");
    assert!(parsed.is_object(), "canonical JSON is an object");
    assert!(
        parsed.get("fields").is_some(),
        "canonical JSON contains fields key"
    );
}
