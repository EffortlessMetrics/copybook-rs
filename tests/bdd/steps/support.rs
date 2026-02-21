// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_governance as governance;
use copybook_governance_runtime as governance_runtime;
use cucumber::{then, when};

use crate::world::CopybookWorld;

#[when(expr = "the support matrix is queried")]
async fn when_support_matrix_queried(world: &mut CopybookWorld) {
    let features = governance::support_matrix::all_features();
    world.record_count = Some(features.len());
}

#[when(expr = "feature {string} is checked")]
async fn when_feature_checked(world: &mut CopybookWorld, feature_id: String) {
    let feature = governance::support_matrix::find_feature(&feature_id);
    if let Some(f) = feature {
        world.verify_report = Some(format!("{:?}", f.status));
    } else {
        world.error = Some(copybook_core::Error::new(
            copybook_core::ErrorCode::CBKP001_SYNTAX,
            format!("Feature '{feature_id}' not found"),
        ));
    }
}

#[then(expr = "the matrix should include {string} feature")]
async fn then_matrix_includes_feature(world: &mut CopybookWorld, feature_name: String) {
    let features = governance::support_matrix::all_features();
    let found = features.iter().any(|f| {
        f.name.eq_ignore_ascii_case(&feature_name)
            || format!("{:?}", f.id).eq_ignore_ascii_case(&feature_name)
    });
    assert!(
        found,
        "Expected support matrix to include feature '{feature_name}'",
    );
}

#[then(expr = "the feature should have status {string}")]
async fn then_feature_has_status(world: &mut CopybookWorld, expected_status: String) {
    let report = world
        .verify_report
        .as_ref()
        .expect("Feature status not set - call 'feature is checked' first");
    assert!(
        report.contains(&expected_status),
        "Expected feature status to contain '{expected_status}', got '{report}'",
    );
}

#[when(expr = "the governance grid summary is checked")]
async fn when_governance_grid_summary_checked(world: &mut CopybookWorld) {
    let summary = governance_runtime::runtime_summary(governance::FeatureFlags::global());
    world.record_count = Some(summary.total_support_features);
    world.verify_report = Some(format!(
        "mapped:{},linked:{},runtime_enabled:{},runtime_disabled:{}",
        summary.mapped_support_features,
        summary.total_linked_feature_flags,
        summary.runtime_enabled_features,
        summary.runtime_disabled_features,
    ));
}

#[then(expr = "the governance summary should map {int} support entries")]
async fn then_governance_summary_maps_support_entries(
    world: &mut CopybookWorld,
    expected_mapped: i32,
) {
    let report = world
        .verify_report
        .as_ref()
        .expect("Governance summary not set - call summary check first");

    let mapped_value = report
        .split(',')
        .find_map(|entry| entry.strip_prefix("mapped:"))
        .and_then(|value| value.parse::<usize>().ok())
        .expect("Governance summary should report mapped count");

    assert_eq!(
        mapped_value,
        usize::try_from(expected_mapped).expect("Expected mapped value should be non-negative"),
        "Expected governance summary mapped support entries to be {expected_mapped}, got {mapped_value}",
    );
}

#[when(expr = "the governance mapping is checked for feature {string}")]
async fn when_governance_mapping_checked(world: &mut CopybookWorld, feature_id: String) {
    let feature = governance::support_matrix::find_feature(&feature_id);
    if feature.is_none() {
        world.error = Some(copybook_core::Error::new(
            copybook_core::ErrorCode::CBKP001_SYNTAX,
            format!("Feature '{feature_id}' not found in governance support grid"),
        ));
        return;
    }

    let feature = feature.expect("feature exists");
    let bindings = governance::feature_flags_for_support_id(feature.id).unwrap_or(&[]);
    world.verify_report = Some(
        bindings
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join(","),
    );
}

#[then(expr = "the governance mapping should include feature flag {string}")]
async fn then_governance_mapping_includes_flag(world: &mut CopybookWorld, expected_flag: String) {
    let report = world.verify_report.as_ref().expect(
        "Governance mapping not set - call 'the governance mapping is checked for feature' first",
    );

    let mapped = report
        .split(',')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .collect::<Vec<_>>();
    assert!(
        mapped.iter().any(|value| *value == expected_flag),
        "Expected governance mapping to include '{expected_flag}', got '{report}'",
    );
}

#[when(expr = "the support matrix runtime availability is checked")]
async fn when_support_matrix_runtime_available_checked(world: &mut CopybookWorld) {
    let summary = governance_runtime::runtime_summary(governance::FeatureFlags::global());
    world.verify_report = Some(format!(
        "runtime_enabled:{},runtime_disabled:{}",
        summary.runtime_enabled_features, summary.runtime_disabled_features,
    ));
}

#[when(expr = "the support matrix runtime availability is checked with sign-separate disabled")]
async fn when_support_matrix_runtime_available_sign_separate_disabled(world: &mut CopybookWorld) {
    let flags = governance::FeatureFlags::builder()
        .enable(governance::Feature::RenamesR4R6)
        .disable(governance::Feature::SignSeparate)
        .build();
    let summary = governance_runtime::runtime_summary(&flags);
    world.verify_report = Some(format!(
        "runtime_enabled:{},runtime_disabled:{}",
        summary.runtime_enabled_features, summary.runtime_disabled_features,
    ));
}

#[then(
    expr = "the command output should report {int} runtime enabled and {int} runtime disabled support entries"
)]
async fn then_runtime_enabled_counts(
    world: &mut CopybookWorld,
    expected_enabled: i32,
    expected_disabled: i32,
) {
    let report = world
        .verify_report
        .as_ref()
        .expect("runtime summary not set - call runtime check first");

    let enabled = report
        .split(',')
        .find_map(|entry| entry.strip_prefix("runtime_enabled:"))
        .and_then(|value| value.parse::<usize>().ok())
        .expect("runtime summary should report runtime_enabled");
    let disabled = report
        .split(',')
        .find_map(|entry| entry.strip_prefix("runtime_disabled:"))
        .and_then(|value| value.parse::<usize>().ok())
        .expect("runtime summary should report runtime_disabled");

    assert_eq!(
        enabled,
        usize::try_from(expected_enabled).expect("Expected enabled count should be non-negative")
    );
    assert_eq!(
        disabled,
        usize::try_from(expected_disabled).expect("Expected disabled count should be non-negative")
    );
}
