// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_core::{
    ErrorCode, ParseOptions, feature_flags::FeatureFlags, parse_copybook_with_feature_flags,
};

fn ensure_comp_features_disabled() -> FeatureFlags {
    let mut flags = FeatureFlags::default();
    flags.disable(copybook_core::feature_flags::Feature::Comp1);
    flags.disable(copybook_core::feature_flags::Feature::Comp2);
    flags
}

#[test]
fn test_disabled_comp_features_reject_comp1_comp2_clauses() {
    let flags = ensure_comp_features_disabled();
    assert!(!flags.is_enabled(copybook_core::feature_flags::Feature::Comp1));
    assert!(!flags.is_enabled(copybook_core::feature_flags::Feature::Comp2));

    let cases = [
        (
            "direct-comp-1",
            "01 TEMP PIC S9(4) COMP-1.",
            "COMP-1",
            "comp_1",
        ),
        (
            "direct-comp-2",
            "01 TEMP PIC S9(4) COMP-2.",
            "COMP-2",
            "comp_2",
        ),
        (
            "usage-comp-1",
            "01 TEMP PIC 9(4) USAGE COMP-1.",
            "USAGE COMP-1",
            "comp_1",
        ),
        (
            "usage-comp-2",
            "01 TEMP PIC 9(4) USAGE COMP-2.",
            "USAGE COMP-2",
            "comp_2",
        ),
    ];

    for (_label, source, syntax, token) in cases {
        let result = parse_copybook_with_feature_flags(source, &ParseOptions::default(), &flags);
        let error = result.expect_err("expected gated parser to reject disabled COMP features");
        assert_eq!(error.code, ErrorCode::CBKP011_UNSUPPORTED_CLAUSE);
        assert!(
            error.message.contains(syntax),
            "error message should include disabled syntax '{syntax}', got: {}",
            error.message
        );
        assert!(
            error.message.contains(token),
            "error message should include token '{token}', got: {}",
            error.message
        );
    }
}
