// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::{given, then, when};

use copybook_core::ErrorCode;
use copybook_safe_ops::{parse_usize, safe_array_bound, safe_divide};

use crate::world::CopybookWorld;

#[given(expr = "the safe-op input is {string}")]
async fn given_safe_op_input(world: &mut CopybookWorld, input: String) {
    world.safe_ops_input = Some(input);
    world.safe_ops_usize = None;
    world.safe_ops_error_code = None;
}

#[when(expr = "safe_ops parses the input as usize")]
async fn when_parse_input_as_usize(world: &mut CopybookWorld) {
    let input = world.safe_ops_input.clone().expect("safe-op input");
    match parse_usize(&input, "bdd-safe-ops") {
        Ok(value) => {
            world.safe_ops_usize = Some(value);
            world.safe_ops_error_code = None;
        }
        Err(error) => {
            world.safe_ops_usize = None;
            world.safe_ops_error_code = Some(error.code);
        }
    }
}

#[then(expr = "the parse result should be {int}")]
async fn then_safe_ops_parse_result(world: &mut CopybookWorld, expected: usize) {
    let actual = world.safe_ops_usize.expect("safe-op parse result");
    assert_eq!(actual, expected);
    assert!(world.safe_ops_error_code.is_none());
}

#[then(expr = "safe_ops should report a syntax error")]
async fn then_safe_ops_parse_error(world: &mut CopybookWorld) {
    assert_eq!(
        world
            .safe_ops_error_code
            .expect("safe-op error for syntax path"),
        ErrorCode::CBKP001_SYNTAX
    );
}

#[when(expr = "safe_array_bound is called with base {int}, count {int}, and item size {int}")]
async fn when_safe_array_bound(world: &mut CopybookWorld, base: usize, count: usize, item_size: usize) {
    match safe_array_bound(base, count, item_size, "bdd-safe-ops") {
        Ok(value) => {
            world.safe_ops_array_bound = Some(value);
            world.safe_ops_error_code = None;
        }
        Err(error) => {
            world.safe_ops_array_bound = None;
            world.safe_ops_error_code = Some(error.code);
        }
    }
}

#[then(expr = "safe_array_bound should return {int}")]
async fn then_safe_array_bound_result(world: &mut CopybookWorld, expected: usize) {
    let actual = world
        .safe_ops_array_bound
        .expect("safe_array_bound result present");
    assert_eq!(actual, expected);
    assert!(world.safe_ops_error_code.is_none());
}

#[when(expr = "safe_divide is called with numerator {int} and denominator {int}")]
async fn when_safe_divide(world: &mut CopybookWorld, numerator: usize, denominator: usize) {
    match safe_divide(numerator, denominator, "bdd-safe-ops") {
        Ok(_) => world.safe_ops_error_code = None,
        Err(error) => world.safe_ops_error_code = Some(error.code),
    }
}
