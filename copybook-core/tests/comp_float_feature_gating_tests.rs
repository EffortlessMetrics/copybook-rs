// SPDX-License-Identifier: AGPL-3.0-or-later
//! Tests verifying COMP-1/COMP-2 are enabled by default (promoted to stable).
//!
//! These features were previously experimental and required `COPYBOOK_FF_COMP_1`/
//! `COPYBOOK_FF_COMP_2` environment variables. They are now enabled by default.

use copybook_core::{
    feature_flags::{Feature, FeatureFlags},
    parse_copybook,
};

#[test]
fn test_comp1_enabled_by_default() {
    let flags = FeatureFlags::default();
    assert!(
        flags.is_enabled(Feature::Comp1),
        "COMP-1 should be enabled by default (promoted to stable)"
    );
}

#[test]
fn test_comp2_enabled_by_default() {
    let flags = FeatureFlags::default();
    assert!(
        flags.is_enabled(Feature::Comp2),
        "COMP-2 should be enabled by default (promoted to stable)"
    );
}

#[test]
fn test_parser_comp1_accepted_without_env_var() {
    let result = parse_copybook("01 FIELD-A COMP-1.");
    assert!(
        result.is_ok(),
        "COMP-1 should be accepted without any env var (enabled by default)"
    );
}

#[test]
fn test_parser_comp2_accepted_without_env_var() {
    let result = parse_copybook("01 FIELD-B USAGE COMP-2.");
    assert!(
        result.is_ok(),
        "COMP-2 should be accepted without any env var (enabled by default)"
    );
}
