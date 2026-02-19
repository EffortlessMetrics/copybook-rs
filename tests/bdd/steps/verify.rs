// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::decode_file_to_jsonl;
use cucumber::{then, when};
use std::io::Cursor;

use crate::world::CopybookWorld;

#[when(expr = "the data is verified")]
async fn when_data_verified(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    world.ensure_decode_options();

    // Verify by attempting to decode - if it succeeds, data is valid
    let binary_data = world
        .binary_data
        .as_ref()
        .expect("Binary data not set")
        .clone();
    let mut output = Vec::new();

    match decode_file_to_jsonl(
        world.schema(),
        Cursor::new(&binary_data),
        &mut output,
        world
            .decode_options
            .as_ref()
            .expect("Decode options not set"),
    ) {
        Ok(summary) => {
            world.decoded_output = Some(String::from_utf8(output).unwrap());
            world.verify_error_count = Some(0);
            world.verify_report = Some(format!(
                "Verification passed: {} records processed",
                summary.records_processed
            ));
        }
        Err(e) => {
            world.verify_error_count = Some(1);
            world.verify_report = Some(format!("Verification failed: {}", e));
            world.error = Some(e);
        }
    }
}

#[when(expr = "the data is verified with JSON report")]
async fn when_data_verified_json(world: &mut CopybookWorld) {
    when_data_verified(world).await;
    // Wrap report in JSON format
    if let Some(ref report) = world.verify_report {
        let json_report = serde_json::json!({
            "status": if world.error.is_some() { "failed" } else { "passed" },
            "errors": world.verify_error_count.unwrap_or(0),
            "message": report,
        });
        world.verify_report = Some(serde_json::to_string_pretty(&json_report).unwrap());
    }
}

#[then(expr = "verification should succeed")]
async fn then_verification_succeeds(world: &mut CopybookWorld) {
    assert!(
        world.error.is_none(),
        "Verification should succeed, but got error: {:?}",
        world.error
    );
}

#[then(expr = "verification should fail")]
async fn then_verification_fails(world: &mut CopybookWorld) {
    assert!(world.error.is_some(), "Verification should fail");
}

#[then(expr = "the verify report should contain {int} errors")]
async fn then_verify_report_error_count(world: &mut CopybookWorld, expected: usize) {
    let count = world.verify_error_count.unwrap_or(0);
    assert_eq!(
        count, expected,
        "Expected {} verification errors, got {}",
        expected, count
    );
}

#[then(expr = "the verify report should be valid JSON")]
async fn then_verify_report_valid_json(world: &mut CopybookWorld) {
    let report = world.verify_report.as_ref().expect("Verify report not set");
    let _: serde_json::Value =
        serde_json::from_str(report).expect("Verify report should be valid JSON");
}
