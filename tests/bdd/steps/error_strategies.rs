// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::{given, then};

use crate::world::CopybookWorld;

#[given(expr = "max errors {int}")]
async fn given_max_errors(world: &mut CopybookWorld, max: u64) {
    world.ensure_decode_options();
    world.ensure_encode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_max_errors(Some(max)));
    }
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_max_errors(Some(max)));
    }
}

#[given(expr = "max errors unlimited")]
async fn given_max_errors_unlimited(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    world.ensure_encode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_max_errors(None));
    }
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_max_errors(None));
    }
}

#[then(expr = "the error count should not exceed {int}")]
async fn then_error_count_not_exceed(world: &mut CopybookWorld, max: usize) {
    let count = world.verify_error_count.unwrap_or(0);
    assert!(
        count <= max,
        "Expected error count <= {}, got {}",
        max,
        count
    );
}
