// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::then;

use crate::world::CopybookWorld;

#[then(expr = "the error code should start with {string}")]
async fn then_error_code_starts_with(world: &mut CopybookWorld, prefix: String) {
    let error = world.error.as_ref().expect("Error should be set");
    let code_str = format!("{}", error.code);
    assert!(
        code_str.starts_with(&prefix),
        "Expected error code starting with '{}', got '{}'",
        prefix,
        code_str
    );
}

#[then(expr = "the error message should contain context")]
async fn then_error_message_contains_context(world: &mut CopybookWorld) {
    let error = world.error.as_ref().expect("Error should be set");
    let message = format!("{}", error);
    // Error message should be non-trivial (contain meaningful context)
    assert!(
        message.len() > 10,
        "Expected error message with context, got: {}",
        message
    );
}

#[then(expr = "the error display should contain {string}")]
async fn then_error_display_contains(world: &mut CopybookWorld, expected: String) {
    let error = world.error.as_ref().expect("Error should be set");
    let display = format!("{}", error);
    assert!(
        display.contains(&expected),
        "Expected error display to contain '{}', got: {}",
        expected,
        display
    );
}
