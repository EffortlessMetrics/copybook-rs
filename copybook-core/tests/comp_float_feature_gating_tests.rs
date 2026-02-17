//! Feature gating tests for COMP-1/COMP-2 parsing.
//!
//! Verifies that experimental floating-point features are correctly rejected
//! when feature flags are not enabled.

use copybook_core::{ErrorCode, feature_flags::{Feature, FeatureFlags}, parse_copybook};

fn disable_comp_flags_for_tests() {
    let mut flags = FeatureFlags::default();
    flags.disable(Feature::Comp1);
    flags.disable(Feature::Comp2);
    FeatureFlags::set_global(flags);
}

#[test]
fn test_parser_comp1_without_feature_flag_is_rejected() {
    disable_comp_flags_for_tests();
    let result = parse_copybook("01 FIELD-A COMP-1.");

    assert!(result.is_err(), "COMP-1 should be rejected without feature flag");
    let err = result.unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP011_UNSUPPORTED_CLAUSE);
}

#[test]
fn test_parser_comp2_usage_without_feature_flag_is_rejected() {
    disable_comp_flags_for_tests();
    let result = parse_copybook("01 FIELD-B USAGE COMP-2.");

    assert!(result.is_err(), "USAGE COMP-2 should be rejected without feature flag");
    let err = result.unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP011_UNSUPPORTED_CLAUSE);
}
