//! Tests for Schema alias lookup methods (find_field_or_alias, resolve_alias_to_target)
//!
//! Validates that RENAMES aliases can be resolved through Schema API methods.

use copybook_core::parse_copybook;

#[test]
fn test_find_field_or_alias_direct_lookup() {
    // Direct field lookup should still work
    let cb = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
      10 ADDRESS  PIC X(60).
"#;

    let schema = parse_copybook(cb).expect("parse");

    // Find by full path
    let field = schema
        .find_field_or_alias("CUSTOMER-RECORD.CUSTOMER-INFO.NAME")
        .expect("find NAME");
    assert_eq!(field.name, "NAME");
    assert_eq!(field.level, 10);

    // Find by short name should not work for direct lookup (needs full path)
    assert!(schema.find_field_or_alias("NAME").is_none());
}

#[test]
fn test_find_field_or_alias_r2_same_scope_group() {
    // R2: Same-scope group RENAMES
    let cb = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
      10 ADDRESS  PIC X(60).
   66 CUSTOMER-DETAILS RENAMES CUSTOMER-INFO THRU CUSTOMER-INFO.
"#;

    let schema = parse_copybook(cb).expect("parse");

    // Find the alias by name
    let alias = schema
        .find_field_or_alias("CUSTOMER-DETAILS")
        .expect("find alias");
    assert_eq!(alias.name, "CUSTOMER-DETAILS");
    assert_eq!(alias.level, 66);

    // Verify resolved_renames metadata
    let resolved = alias.resolved_renames.as_ref().expect("resolved");
    assert_eq!(resolved.members.len(), 2);
    assert!(
        resolved
            .members
            .contains(&"CUSTOMER-RECORD.CUSTOMER-INFO.NAME".to_string())
    );
    assert!(
        resolved
            .members
            .contains(&"CUSTOMER-RECORD.CUSTOMER-INFO.ADDRESS".to_string())
    );
}

#[test]
fn test_find_field_or_alias_r3_nested_group() {
    // R3: Nested group RENAMES
    let cb = r#"
01 POLICY-RECORD.
   05 POLICY-INFO.
      10 POLICY-NUMBER PIC X(10).
      10 POLICY-DATES.
         15 START-DATE PIC X(8).
         15 END-DATE   PIC X(8).
   66 POLICY-PERIOD RENAMES POLICY-DATES THRU POLICY-DATES.
"#;

    let schema = parse_copybook(cb).expect("parse");

    // Find the alias by name
    let alias = schema
        .find_field_or_alias("POLICY-PERIOD")
        .expect("find alias");
    assert_eq!(alias.name, "POLICY-PERIOD");
    assert_eq!(alias.level, 66);

    // Verify resolved_renames metadata for nested target
    let resolved = alias.resolved_renames.as_ref().expect("resolved");
    assert_eq!(resolved.members.len(), 2);
    assert!(
        resolved
            .members
            .contains(&"POLICY-RECORD.POLICY-INFO.POLICY-DATES.START-DATE".to_string())
    );
    assert!(
        resolved
            .members
            .contains(&"POLICY-RECORD.POLICY-INFO.POLICY-DATES.END-DATE".to_string())
    );
}

#[test]
fn test_resolve_alias_to_target_r2() {
    // R2: Resolve alias to target field
    let cb = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
      10 ADDRESS  PIC X(60).
   66 CUSTOMER-DETAILS RENAMES CUSTOMER-INFO THRU CUSTOMER-INFO.
"#;

    let schema = parse_copybook(cb).expect("parse");

    // Resolve alias to first member field
    let target = schema
        .resolve_alias_to_target("CUSTOMER-DETAILS")
        .expect("resolve alias");

    // Should return the first storage member (NAME)
    assert_eq!(target.name, "NAME");
    assert_eq!(target.level, 10);
    assert!(matches!(
        target.kind,
        copybook_core::FieldKind::Alphanum { len: 30 }
    ));
}

#[test]
fn test_resolve_alias_to_target_r3() {
    // R3: Resolve nested alias to target field
    let cb = r#"
01 POLICY-RECORD.
   05 POLICY-INFO.
      10 POLICY-NUMBER PIC X(10).
      10 POLICY-DATES.
         15 START-DATE PIC X(8).
         15 END-DATE   PIC X(8).
   66 POLICY-PERIOD RENAMES POLICY-DATES THRU POLICY-DATES.
"#;

    let schema = parse_copybook(cb).expect("parse");

    // Resolve alias to first member field
    let target = schema
        .resolve_alias_to_target("POLICY-PERIOD")
        .expect("resolve alias");

    // Should return the first storage member (START-DATE)
    assert_eq!(target.name, "START-DATE");
    assert_eq!(target.level, 15);
    assert!(matches!(
        target.kind,
        copybook_core::FieldKind::Alphanum { len: 8 }
    ));
}

#[test]
fn test_resolve_alias_fallback_to_direct_lookup() {
    // If not an alias, resolve_alias_to_target should fall back to direct lookup
    let cb = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
"#;

    let schema = parse_copybook(cb).expect("parse");

    // Resolve non-alias field should work via fallback
    let target = schema
        .resolve_alias_to_target("CUSTOMER-RECORD.CUSTOMER-INFO.NAME")
        .expect("resolve");
    assert_eq!(target.name, "NAME");
    assert_eq!(target.level, 10);
}

#[test]
fn test_find_field_or_alias_case_insensitive() {
    // COBOL is case-insensitive
    let cb = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
   66 CUSTOMER-DETAILS RENAMES CUSTOMER-INFO THRU CUSTOMER-INFO.
"#;

    let schema = parse_copybook(cb).expect("parse");

    // Find alias with different case
    let alias_lower = schema.find_field_or_alias("customer-details");
    let alias_upper = schema.find_field_or_alias("CUSTOMER-DETAILS");
    let alias_mixed = schema.find_field_or_alias("Customer-Details");

    assert!(alias_lower.is_some());
    assert!(alias_upper.is_some());
    assert!(alias_mixed.is_some());

    assert_eq!(alias_lower.unwrap().name, "CUSTOMER-DETAILS");
    assert_eq!(alias_upper.unwrap().name, "CUSTOMER-DETAILS");
    assert_eq!(alias_mixed.unwrap().name, "CUSTOMER-DETAILS");
}
