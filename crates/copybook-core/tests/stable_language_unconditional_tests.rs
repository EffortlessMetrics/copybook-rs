// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compatibility tests proving SIGN SEPARATE, COMP-1, and COMP-2 parse
//! unconditionally (#656 Phase C, v0.6.0).
//!
//! These clauses are stable, documented COBOL behavior. They must parse with
//! default flags, with every remaining flag disabled, and regardless of any
//! stale `COPYBOOK_FF_SIGN_SEPARATE` / `COPYBOOK_FF_COMP_1` /
//! `COPYBOOK_FF_COMP_2` environment (those names are no longer recognized).

use copybook_core::{
    FieldKind, ParseOptions, feature_flags::FeatureFlags, parse_copybook_with_feature_flags,
};

fn all_flags_disabled() -> FeatureFlags {
    let mut flags = FeatureFlags::default();
    for feature in copybook_core::feature_flags::all_features() {
        flags.disable(feature);
    }
    flags
}

#[test]
fn test_comp1_parses_with_default_flags() {
    let flags = FeatureFlags::default();
    for source in [
        "01 TEMP PIC S9(4) COMP-1.",
        "01 TEMP PIC 9(4) USAGE COMP-1.",
        "01 FIELD-A COMP-1.",
    ] {
        let schema = parse_copybook_with_feature_flags(source, &ParseOptions::default(), &flags);
        let schema = schema
            .unwrap_or_else(|e| panic!("COMP-1 should parse unconditionally ({source}): {e}"));
        assert!(
            matches!(schema.fields[0].kind, FieldKind::FloatSingle),
            "COMP-1 should produce FloatSingle for {source}"
        );
    }
}

#[test]
fn test_comp2_parses_with_default_flags() {
    let flags = FeatureFlags::default();
    for source in [
        "01 TEMP PIC S9(4) COMP-2.",
        "01 TEMP PIC 9(4) USAGE COMP-2.",
        "01 FIELD-B USAGE COMP-2.",
    ] {
        let schema = parse_copybook_with_feature_flags(source, &ParseOptions::default(), &flags);
        let schema = schema
            .unwrap_or_else(|e| panic!("COMP-2 should parse unconditionally ({source}): {e}"));
        assert!(
            matches!(schema.fields[0].kind, FieldKind::FloatDouble),
            "COMP-2 should produce FloatDouble for {source}"
        );
    }
}

#[test]
fn test_stable_clauses_parse_with_all_flags_disabled() {
    let flags = all_flags_disabled();
    for source in [
        "01 TEMP PIC S9(4) COMP-1.",
        "01 TEMP PIC S9(4) COMP-2.",
        "01 AMOUNT PIC S9(5) SIGN IS LEADING SEPARATE.",
        "01 AMOUNT PIC S9(5) SIGN TRAILING SEPARATE.",
    ] {
        let result = parse_copybook_with_feature_flags(source, &ParseOptions::default(), &flags);
        assert!(
            result.is_ok(),
            "stable clause should parse with all flags disabled ({source})"
        );
    }
}
