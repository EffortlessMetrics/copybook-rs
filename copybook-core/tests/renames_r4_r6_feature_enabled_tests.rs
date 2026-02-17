#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::{
    feature_flags::{Feature, FeatureFlags},
    parse_copybook, FieldKind,
};

fn enable_renames_r4_r6_feature() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::RenamesR4R6);
    FeatureFlags::set_global(flags);
}

#[test]
fn test_r4_single_redefines_renames_accepted_with_feature_flag() {
    enable_renames_r4_r6_feature();

    let copybook = r#"
       01  TRANSACTION-RECORD.
           05  TRANS-TYPE      PIC X(1).
           05  TRANS-DATA      PIC X(20).
           05  CHECK-DATA      REDEFINES TRANS-DATA.
               10  CHECK-NUM  PIC 9(8).
               10  CHECK-AMT  PIC 9(10).
           05  CARD-DATA       REDEFINES TRANS-DATA.
               10  CARD-NUM   PIC 9(16).
               10  CARD-EXP   PIC 9(4).
           66  PAYMENT-INFO RENAMES CHECK-DATA THRU CHECK-DATA.
    "#;

    let schema = parse_copybook(copybook).expect("RENAMES over single REDEFINES should resolve when feature enabled");

    let alias = schema
        .find_field_or_alias("PAYMENT-INFO")
        .expect("renames alias should be present");

    assert!(matches!(alias.kind, FieldKind::Renames { .. }));
    let resolved = alias.resolved_renames.as_ref().expect("resolved renames expected");

    assert_eq!(resolved.members.len(), 2);
    assert!(resolved.members.iter().any(|m| m.ends_with("CHECK-NUM")));
    assert!(resolved.members.iter().any(|m| m.ends_with("CHECK-AMT")));
}

#[test]
fn test_r5_occurs_alias_accepted_with_feature_flag() {
    enable_renames_r4_r6_feature();

    let copybook = r#"
       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  LINE-ITEMS      OCCURS 10 TIMES.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           66  ORDER-ITEMS RENAMES LINE-ITEMS THRU LINE-ITEMS.
    "#;

    let schema = parse_copybook(copybook)
        .expect("RENAMES over an entire OCCURS array should resolve when feature enabled");

    let alias = schema
        .find_field_or_alias("ORDER-ITEMS")
        .expect("renames alias should be present");

    assert!(matches!(alias.kind, FieldKind::Renames { .. }));
    let resolved = alias.resolved_renames.as_ref().expect("resolved renames expected");

    assert_eq!(resolved.members.len(), 2);
    assert!(resolved.members.iter().any(|m| m.ends_with("ITEM-CODE")));
    assert!(resolved.members.iter().any(|m| m.ends_with("QUANTITY")));
}
