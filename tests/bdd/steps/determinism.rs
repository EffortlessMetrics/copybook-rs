// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::determinism::{
    check_decode_determinism, check_encode_determinism, check_round_trip_determinism,
};
use cucumber::{then, when};
use serde_json::Value;

use crate::world::CopybookWorld;

#[when(expr = "decode determinism is checked")]
async fn when_decode_determinism_checked(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    world.ensure_decode_options();

    let binary_data = world
        .binary_data
        .as_ref()
        .expect("Binary data not set")
        .clone();
    match check_decode_determinism(
        world.schema(),
        &binary_data,
        world
            .decode_options
            .as_ref()
            .expect("Decode options not set"),
    ) {
        Ok(result) => {
            world.determinism_result = Some(result);
        }
        Err(e) => {
            world.error = Some(e);
        }
    }
}

#[when(expr = "encode determinism is checked")]
async fn when_encode_determinism_checked(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    world.ensure_encode_options();

    let json_data = world.json_data.as_ref().expect("JSON data not set").clone();
    let json_value: Value =
        serde_json::from_str(&json_data).expect("JSON data should be valid JSON");

    match check_encode_determinism(
        world.schema(),
        &json_value,
        world
            .encode_options
            .as_ref()
            .expect("Encode options not set"),
    ) {
        Ok(result) => {
            world.determinism_result = Some(result);
        }
        Err(e) => {
            world.error = Some(e);
        }
    }
}

#[when(expr = "round-trip determinism is checked")]
async fn when_roundtrip_determinism_checked(world: &mut CopybookWorld) {
    if !world.ensure_schema_and_return() {
        return;
    }
    world.ensure_decode_options();
    world.ensure_encode_options();

    let binary_data = world
        .binary_data
        .as_ref()
        .expect("Binary data not set")
        .clone();
    match check_round_trip_determinism(
        world.schema(),
        &binary_data,
        world
            .decode_options
            .as_ref()
            .expect("Decode options not set"),
        world
            .encode_options
            .as_ref()
            .expect("Encode options not set"),
    ) {
        Ok(result) => {
            world.determinism_result = Some(result);
        }
        Err(e) => {
            world.error = Some(e);
        }
    }
}

#[then(expr = "determinism should pass")]
async fn then_determinism_passes(world: &mut CopybookWorld) {
    let result = world
        .determinism_result
        .as_ref()
        .expect("Determinism result should be set");

    assert!(
        result.is_deterministic,
        "Expected determinism to pass, but it failed"
    );
}

#[then(expr = "determinism should fail")]
async fn then_determinism_fails(world: &mut CopybookWorld) {
    let result = world
        .determinism_result
        .as_ref()
        .expect("Determinism result should be set");

    assert!(
        !result.is_deterministic,
        "Expected determinism to fail, but it passed"
    );
}

#[then(expr = "the decode should be deterministic")]
async fn then_decode_deterministic(world: &mut CopybookWorld) {
    then_determinism_passes(world).await;
}

#[then(expr = "the encode should be deterministic")]
async fn then_encode_deterministic(world: &mut CopybookWorld) {
    then_determinism_passes(world).await;
}

#[then(expr = "the round-trip should be deterministic")]
async fn then_roundtrip_deterministic(world: &mut CopybookWorld) {
    then_determinism_passes(world).await;
}

#[then(expr = "the round 1 hash should equal to round 2 hash")]
async fn then_hashes_equal(world: &mut CopybookWorld) {
    let result = world
        .determinism_result
        .as_ref()
        .expect("Determinism result not set");
    assert_eq!(
        result.round1_hash, result.round2_hash,
        "Round 1 hash ({}) should equal round 2 hash ({})",
        result.round1_hash, result.round2_hash
    );
}

#[then(expr = "there should be no byte differences")]
async fn then_no_byte_differences(world: &mut CopybookWorld) {
    let result = world
        .determinism_result
        .as_ref()
        .expect("Determinism result not set");
    match &result.byte_differences {
        None => {}
        Some(diffs) => assert!(
            diffs.is_empty(),
            "Expected no byte differences, got {}",
            diffs.len()
        ),
    }
}

#[then(expr = "the JSON should contain {string}")]
async fn then_json_contains(world: &mut CopybookWorld, expected: String) {
    let result = world
        .determinism_result
        .as_ref()
        .expect("Determinism result not set");
    let json = serde_json::to_string(result).expect("Failed to serialize determinism result");
    assert!(
        json.contains(&expected),
        "Expected JSON to contain '{}', got: {}",
        expected,
        json
    );
}

#[then(regex = r#"^the human-readable output should show "(.+)"$"#)]
async fn then_human_readable_shows(world: &mut CopybookWorld, expected: String) {
    let result = world
        .determinism_result
        .as_ref()
        .expect("Determinism result not set");
    let verdict = if result.is_deterministic {
        "DETERMINISTIC"
    } else {
        "NON-DETERMINISTIC"
    };
    assert!(
        verdict.contains(&expected),
        "Expected human-readable to show '{}', got '{}'",
        expected,
        verdict
    );
}

#[then(regex = r#"^the output should contain "(.+)"$"#)]
async fn then_output_contains(world: &mut CopybookWorld, expected: String) {
    let result = world
        .determinism_result
        .as_ref()
        .expect("Determinism result not set");
    let output = format!(
        "Round 1 hash: {}\nRound 2 hash: {}\nDeterministic: {}",
        result.round1_hash, result.round2_hash, result.is_deterministic
    );
    assert!(
        output.contains(&expected),
        "Expected output to contain '{}', got: {}",
        expected,
        output
    );
}
