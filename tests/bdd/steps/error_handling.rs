// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::then;

use crate::world::CopybookWorld;

#[then(expr = "an error should occur")]
async fn then_error_occurs(world: &mut CopybookWorld) {
    assert!(world.error.is_some(), "An error should have occurred");
}

#[then(expr = "the error message should contain {string}")]
async fn then_error_message_contains(world: &mut CopybookWorld, expected: String) {
    let error = world.error.as_ref().expect("Error should be set");
    let message = format!("{}", error);
    assert!(
        message.contains(&expected),
        "Expected error message to contain '{}', got: {}",
        expected,
        message
    );
}

#[then(expr = "the error code should be {string}")]
async fn then_error_code_is(world: &mut CopybookWorld, expected_code: String) {
    let error = world.error.as_ref().expect("Error should be set");
    let actual_code = format!("{:?}", error.code);
    assert_eq!(
        actual_code, expected_code,
        "Expected error code '{}', got '{}'",
        expected_code, actual_code
    );
}

#[then(expr = "error should contain {string}")]
async fn then_error_contains_alias(world: &mut CopybookWorld, expected: String) {
    let error = world.error.as_ref().expect("Error should be set");
    let message = format!("{}", error);
    assert!(
        message.contains(&expected),
        "Expected error to contain '{}', got: {}",
        expected,
        message
    );
}

#[then(regex = r"^parsing should fail with error code (.+)$")]
async fn then_parsing_fail_with_code(world: &mut CopybookWorld, expected_code: String) {
    assert!(world.error.is_some(), "Parsing should fail");
    let error = world.error.as_ref().unwrap();
    let actual_code = format!("{:?}", error.code);
    let expected_trimmed = expected_code.trim_matches('"');
    assert!(
        actual_code.starts_with(expected_trimmed),
        "Expected error code starting with '{}', got '{}'",
        expected_trimmed,
        actual_code
    );
}
