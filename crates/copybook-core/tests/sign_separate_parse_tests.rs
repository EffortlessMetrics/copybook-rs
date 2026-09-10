#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]

use copybook_core::{FieldKind, SignPlacement, parse_copybook};

#[test]
fn test_sign_separate_leading_parsed_unconditionally() {
    let copybook = "01 SIGNED-FIELD PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema =
        parse_copybook(copybook).expect("SIGN SEPARATE parses unconditionally (#656 Phase C)");

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
fn test_sign_separate_trailing_parsed_unconditionally() {
    let copybook = "01 SIGNED-FIELD PIC S9(5)V99 SIGN TRAILING SEPARATE.";
    let schema =
        parse_copybook(copybook).expect("SIGN SEPARATE parses unconditionally (#656 Phase C)");

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
