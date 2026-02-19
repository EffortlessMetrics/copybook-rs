// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::given;

use crate::world::CopybookWorld;

#[given(expr = "strict_comments mode")]
async fn given_strict_comments(world: &mut CopybookWorld) {
    let mut options = world.parse_options.clone().unwrap_or_default();
    options.strict = true;
    world.parse_options = Some(options);
}

#[given(expr = "inline comments disabled")]
async fn given_inline_comments_disabled(world: &mut CopybookWorld) {
    let mut options = world.parse_options.clone().unwrap_or_default();
    options.allow_inline_comments = false;
    world.parse_options = Some(options);
}
