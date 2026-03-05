// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_governance::{
    self as governance, Feature, FeatureCategory, FeatureFlags, FeatureId, governance_bindings,
    governance_states, runtime_summary, support_matrix,
};
use cucumber::{given, then, when};

use crate::world::CopybookWorld;

// ---------------------------------------------------------------------------
// Given
// ---------------------------------------------------------------------------

#[given("the default feature flags")]
async fn given_default_feature_flags(world: &mut CopybookWorld) {
    let flags = FeatureFlags::default();
    let json = serde_json::to_string(&flags).expect("serialize default flags");
    world.json_data = Some(json);
}

#[given("feature flags with Enterprise category enabled")]
async fn given_enterprise_enabled(world: &mut CopybookWorld) {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Enterprise)
        .build();
    let json = serde_json::to_string(&flags).expect("serialize enterprise flags");
    world.json_data = Some(json);
}

#[given("feature flags with all categories enabled")]
async fn given_all_categories_enabled(world: &mut CopybookWorld) {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Experimental)
        .enable_category(FeatureCategory::Enterprise)
        .enable_category(FeatureCategory::Performance)
        .enable_category(FeatureCategory::Debug)
        .enable_category(FeatureCategory::Testing)
        .build();
    let json = serde_json::to_string(&flags).expect("serialize all-enabled flags");
    world.json_data = Some(json);
}

#[given("feature flags with Experimental category enabled")]
async fn given_experimental_enabled(world: &mut CopybookWorld) {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Experimental)
        .build();
    let json = serde_json::to_string(&flags).expect("serialize experimental flags");
    world.json_data = Some(json);
}

// ---------------------------------------------------------------------------
// When
// ---------------------------------------------------------------------------

#[when("the flags are serialized to JSON")]
async fn when_flags_serialized(world: &mut CopybookWorld) {
    // json_data already holds the serialized form from the Given step.
    // Re-parse and re-serialize to prove the round-trip path.
    let json = world.json_data.as_ref().expect("flags JSON not set");
    let deserialized: FeatureFlags =
        serde_json::from_str(json).expect("deserialize flags from JSON");
    let re_serialized = serde_json::to_string(&deserialized).expect("re-serialize flags");
    world.verify_report = Some(re_serialized);
}

#[when("runtime state is evaluated")]
async fn when_runtime_state_evaluated(world: &mut CopybookWorld) {
    let json = world.json_data.as_ref().expect("flags JSON not set");
    let flags: FeatureFlags = serde_json::from_str(json).expect("deserialize flags");
    let states = governance_states(&flags);
    let summary = runtime_summary(&flags);
    world.record_count = Some(summary.runtime_enabled_features);
    // Stash the full states as JSON for later assertions.
    let states_json = serde_json::to_string(&states).expect("serialize governance states");
    world.verify_report = Some(states_json);
}

// ---------------------------------------------------------------------------
// Then
// ---------------------------------------------------------------------------

#[then("all stable features should be enabled")]
async fn then_stable_enabled(world: &mut CopybookWorld) {
    let json = world.json_data.as_ref().expect("flags JSON not set");
    let flags: FeatureFlags = serde_json::from_str(json).expect("deserialize flags");

    let stable_features: Vec<Feature> = governance::feature_flags::all_features()
        .into_iter()
        .filter(|f| f.default_enabled())
        .collect();

    assert!(
        !stable_features.is_empty(),
        "There should be at least one stable (default-enabled) feature"
    );

    for feature in &stable_features {
        assert!(
            flags.is_enabled(*feature),
            "Stable feature {feature} should be enabled by default"
        );
    }
}

#[then("experimental features should be disabled")]
async fn then_experimental_disabled(world: &mut CopybookWorld) {
    let json = world.json_data.as_ref().expect("flags JSON not set");
    let flags: FeatureFlags = serde_json::from_str(json).expect("deserialize flags");

    let non_default_experimental: Vec<Feature> =
        FeatureFlags::features_in_category(FeatureCategory::Experimental)
            .into_iter()
            .filter(|f| !f.default_enabled())
            .collect();

    assert!(
        !non_default_experimental.is_empty(),
        "There should be at least one non-default experimental feature"
    );

    for feature in &non_default_experimental {
        assert!(
            !flags.is_enabled(*feature),
            "Non-default experimental feature {feature} should be disabled by default"
        );
    }
}

#[then("enterprise features should be accessible")]
async fn then_enterprise_accessible(world: &mut CopybookWorld) {
    let json = world.json_data.as_ref().expect("flags JSON not set");
    let flags: FeatureFlags = serde_json::from_str(json).expect("deserialize flags");

    let enterprise = FeatureFlags::features_in_category(FeatureCategory::Enterprise);
    assert!(
        !enterprise.is_empty(),
        "Enterprise category should have features"
    );

    for feature in &enterprise {
        assert!(
            flags.is_enabled(*feature),
            "Enterprise feature {feature} should be enabled when category is enabled"
        );
    }
}

#[then("the support matrix should list all governed features")]
async fn then_support_matrix_lists_all(world: &mut CopybookWorld) {
    let bindings = governance_bindings();
    let matrix_features = support_matrix::all_features();

    assert_eq!(
        bindings.len(),
        matrix_features.len(),
        "Governance bindings count should equal support matrix feature count"
    );

    for feature in matrix_features {
        let found = bindings.iter().any(|b| b.support_id == feature.id);
        assert!(
            found,
            "Support matrix feature {:?} should have a governance binding",
            feature.id
        );
    }
}

#[then("the JSON should be valid and round-trip")]
async fn then_json_roundtrips(world: &mut CopybookWorld) {
    let original_json = world
        .json_data
        .as_ref()
        .expect("original flags JSON not set");
    let roundtripped_json = world
        .verify_report
        .as_ref()
        .expect("round-tripped JSON not set");

    let original: FeatureFlags = serde_json::from_str(original_json).expect("deserialize original");
    let roundtripped: FeatureFlags =
        serde_json::from_str(roundtripped_json).expect("deserialize roundtripped");

    // Every feature in the original must match the round-tripped version.
    for feature in governance::feature_flags::all_features() {
        assert_eq!(
            original.is_enabled(feature),
            roundtripped.is_enabled(feature),
            "Feature {feature} mismatch after JSON round-trip"
        );
    }
}

#[then("experimental features should report as enabled")]
async fn then_experimental_runtime_enabled(world: &mut CopybookWorld) {
    let json = world
        .json_data
        .as_ref()
        .expect("flags JSON not set in Given step");
    let flags: FeatureFlags = serde_json::from_str(json).expect("deserialize flags");

    let experimental = FeatureFlags::features_in_category(FeatureCategory::Experimental);
    for feature in &experimental {
        assert!(
            flags.is_enabled(*feature),
            "Experimental feature {feature} should be enabled after enable_category"
        );
    }

    // Also verify via governance runtime: features that require experimental flags
    // should be runtime-enabled.
    let states = governance_states(&flags);
    for state in &states {
        let all_experimental = state
            .required_feature_flags
            .iter()
            .all(|f| f.category() == FeatureCategory::Experimental);
        if all_experimental && !state.required_feature_flags.is_empty() {
            assert!(
                state.runtime_enabled,
                "Governance state {:?} should be runtime-enabled when Experimental category is on",
                state.support_id
            );
        }
    }
}
