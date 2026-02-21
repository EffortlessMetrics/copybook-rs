// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]

use copybook_governance::{
    Feature, FeatureFlags, FeatureGovernanceState, FeatureGovernanceSummary, FeatureId,
    governance_state_for_support_id, governance_states, runtime_summary,
};

#[test]
fn facade_exposes_runtime_api_surface() {
    let flags = FeatureFlags::default();
    let states: Vec<FeatureGovernanceState> = governance_states(&flags);
    let summary: FeatureGovernanceSummary = runtime_summary(&flags);

    assert!(!states.is_empty());
    assert!(summary.total_support_features >= states.len());

    let sign_state = governance_state_for_support_id(FeatureId::SignSeparate, &flags)
        .expect("sign-separate should remain mapped");
    assert!(sign_state.runtime_enabled);
}

#[test]
fn facade_runtime_summary_tracks_feature_flag_overrides() {
    let flags = FeatureFlags::builder()
        .enable(Feature::RenamesR4R6)
        .disable(Feature::SignSeparate)
        .build();

    let summary = runtime_summary(&flags);
    assert_eq!(summary.runtime_disabled_features, 1);
    assert_eq!(
        summary.runtime_enabled_features,
        summary.total_support_features - 1
    );
}
