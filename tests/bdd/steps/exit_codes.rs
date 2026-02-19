// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::{then, when};

use crate::world::CopybookWorld;

/// Map error code family prefix to exit code integer
fn exit_code_from_error(error: &copybook_core::Error) -> i32 {
    let code_str = format!("{:?}", error.code);
    if code_str.starts_with("CBKD") {
        2 // Data quality failure
    } else if code_str.starts_with("CBKE") {
        3 // Encode/validation failure
    } else if code_str.starts_with("CBKF") || code_str.starts_with("CBKR") {
        4 // Record format / iterator fatal
    } else if code_str.starts_with("CBKI") {
        5 // Internal orchestration error
    } else {
        1 // Unknown / non-taxonomy
    }
}

#[when(expr = "the exit code is computed from the error")]
async fn when_exit_code_computed(world: &mut CopybookWorld) {
    if let Some(ref error) = world.error {
        world.exit_code_result = Some(exit_code_from_error(error));
    } else {
        world.exit_code_result = Some(0);
    }
}

#[then(expr = "the exit code should be {int}")]
async fn then_exit_code_is(world: &mut CopybookWorld, expected: i32) {
    let actual = world.exit_code_result.unwrap_or(0);
    assert_eq!(
        actual, expected,
        "Expected exit code {}, got {}",
        expected, actual
    );
}

#[then(expr = "exit code 0 means success")]
async fn then_exit_code_0_success(world: &mut CopybookWorld) {
    // Exit code 0 always means success — verify no error is present
    let code = world.exit_code_result.unwrap_or(0);
    assert_eq!(code, 0, "Expected exit code 0 (success), got {code}");
}
