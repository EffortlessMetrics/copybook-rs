// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_core::support_matrix;
use cucumber::{then, when};

use crate::world::CopybookWorld;

#[when(expr = "the support matrix is queried")]
async fn when_support_matrix_queried(world: &mut CopybookWorld) {
    let features = support_matrix::all_features();
    // Store feature count in record_count for later assertions
    world.record_count = Some(features.len());
}

#[when(expr = "feature {string} is checked")]
async fn when_feature_checked(world: &mut CopybookWorld, feature_id: String) {
    let feature = support_matrix::find_feature(&feature_id);
    if let Some(f) = feature {
        world.verify_report = Some(format!("{:?}", f.status));
    } else {
        world.error = Some(copybook_core::Error::new(
            copybook_core::ErrorCode::CBKP001_SYNTAX,
            format!("Feature '{}' not found", feature_id),
        ));
    }
}

#[then(expr = "the matrix should include {string} feature")]
async fn then_matrix_includes_feature(world: &mut CopybookWorld, feature_name: String) {
    let features = support_matrix::all_features();
    let found = features.iter().any(|f| {
        f.name.eq_ignore_ascii_case(&feature_name)
            || format!("{:?}", f.id).eq_ignore_ascii_case(&feature_name)
    });
    assert!(
        found,
        "Expected support matrix to include feature '{}'",
        feature_name
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
        "Expected feature status to contain '{}', got '{}'",
        expected_status,
        report
    );
}
