#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::{
    FieldKind, SignPlacement,
    feature_flags::{Feature, FeatureFlags},
    parse_copybook,
};

fn enable_sign_separate_feature() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::SignSeparate);
    FeatureFlags::set_global(flags);
}

#[test]
fn test_sign_separate_leading_parsed_when_feature_enabled() {
    enable_sign_separate_feature();

    let copybook = "01 SIGNED-FIELD PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("SIGN SEPARATE should parse when feature enabled");

    let field = schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == "SIGNED-FIELD")
        .expect("field should exist");

    match &field.kind {
        FieldKind::ZonedDecimal {
            signed,
            sign_separate: Some(sign),
            ..
        } => {
            assert!(*signed);
            assert_eq!(sign.placement, SignPlacement::Leading);
        }
        _ => panic!("Expected sign-separate zoned decimal"),
    }
}

#[test]
fn test_sign_separate_trailing_parsed_when_feature_enabled() {
    enable_sign_separate_feature();

    let copybook = "01 SIGNED-FIELD PIC S9(5)V99 SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("SIGN SEPARATE should parse when feature enabled");

    let field = schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == "SIGNED-FIELD")
        .expect("field should exist");

    match &field.kind {
        FieldKind::ZonedDecimal {
            signed,
            sign_separate: Some(sign),
            ..
        } => {
            assert!(*signed);
            assert_eq!(sign.placement, SignPlacement::Trailing);
        }
        _ => panic!("Expected sign-separate zoned decimal"),
    }
}
