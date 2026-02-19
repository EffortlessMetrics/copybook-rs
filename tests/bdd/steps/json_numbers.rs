// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::JsonNumberMode;
use cucumber::{given, then};
use serde_json::Value;

use crate::helpers::json_value_for_field;
use crate::world::CopybookWorld;

#[given(expr = "lossless number mode")]
async fn given_lossless_mode(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_json_number_mode(JsonNumberMode::Lossless));
    }
}

#[given(expr = "native number mode")]
async fn given_native_mode(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_json_number_mode(JsonNumberMode::Native));
    }
}

#[then(expr = "the decoded {word} should be a string {string}")]
async fn then_decoded_field_is_string(
    world: &mut CopybookWorld,
    field_name: String,
    expected: String,
) {
    let record = world.first_decoded_record();
    let value = json_value_for_field(&record, &field_name).expect(&format!(
        "Field '{}' not found in decoded output",
        field_name
    ));
    assert!(
        value.is_string(),
        "Expected field '{}' to be a string, got {:?}",
        field_name,
        value
    );
    assert_eq!(
        value.as_str().unwrap(),
        expected,
        "Expected field '{}' to be '{}', got '{}'",
        field_name,
        expected,
        value.as_str().unwrap()
    );
}

#[then(expr = "the decoded {word} should be a number")]
async fn then_decoded_field_is_number(world: &mut CopybookWorld, field_name: String) {
    let record = world.first_decoded_record();
    let value = json_value_for_field(&record, &field_name).expect(&format!(
        "Field '{}' not found in decoded output",
        field_name
    ));
    assert!(
        value.is_number(),
        "Expected field '{}' to be a number, got {:?}",
        field_name,
        value
    );
}
