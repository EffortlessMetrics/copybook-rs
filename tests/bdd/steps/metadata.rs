// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::{given, then};

use crate::world::CopybookWorld;

#[given(expr = "emit_meta is {word}")]
async fn given_emit_meta(world: &mut CopybookWorld, enabled: String) {
    let emit = matches!(enabled.to_lowercase().as_str(), "true" | "on" | "enabled");
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_emit_meta(emit));
    }
}

#[then(expr = "the decoded record_index should be {int}")]
async fn then_decoded_record_index(world: &mut CopybookWorld, expected: usize) {
    let record = world.first_decoded_record();
    let index = record
        .get("record_index")
        .and_then(serde_json::Value::as_u64)
        .expect("record_index not found in decoded output");
    assert_eq!(
        usize::try_from(index).unwrap(),
        expected,
        "Expected record_index {expected}, got {index}",
    );
}
