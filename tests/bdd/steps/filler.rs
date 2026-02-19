// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::given;

use crate::world::CopybookWorld;

#[given(expr = "emit_filler is {word}")]
async fn given_emit_filler(world: &mut CopybookWorld, enabled: String) {
    let emit = matches!(enabled.to_lowercase().as_str(), "true" | "on" | "enabled");
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_emit_filler(emit));
    }
}
