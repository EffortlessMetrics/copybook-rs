// SPDX-License-Identifier: AGPL-3.0-or-later
use cucumber::{given, then, when};

use copybook_core::ErrorCode;
use copybook_safe_index::{safe_divide, safe_slice_get};
use copybook_safe_ops::{parse_usize, safe_array_bound};
use copybook_safe_text::{parse_isize, safe_parse_u16, safe_string_char_at};

use crate::world::CopybookWorld;

#[given(expr = "the safe-op input is {string}")]
async fn given_safe_op_input(world: &mut CopybookWorld, input: String) {
    world.safe_ops_input = Some(input);
    world.safe_ops_usize = None;
    world.safe_ops_isize = None;
    world.safe_ops_u16 = None;
    world.safe_ops_char = None;
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

#[when(expr = "safe_text parses the input as isize")]
async fn when_parse_input_as_isize(world: &mut CopybookWorld) {
    let input = world.safe_ops_input.clone().expect("safe-op input");
    match parse_isize(&input, "bdd-safe-text") {
        Ok(value) => {
            world.safe_ops_isize = Some(value);
            world.safe_ops_error_code = None;
        }
        Err(error) => {
            world.safe_ops_isize = None;
            world.safe_ops_error_code = Some(error.code);
        }
    }
}

#[when(expr = "safe_text parses the input as u16")]
async fn when_parse_input_as_u16(world: &mut CopybookWorld) {
    let input = world.safe_ops_input.clone().expect("safe-op input");
    match safe_parse_u16(&input, "bdd-safe-text") {
        Ok(value) => {
            world.safe_ops_u16 = Some(value);
            world.safe_ops_error_code = None;
        }
        Err(error) => {
            world.safe_ops_u16 = None;
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

#[then(expr = "the safe-text isize parse result should be {int}")]
async fn then_safe_ops_isize_parse_result(world: &mut CopybookWorld, expected: isize) {
    let actual = world.safe_ops_isize.expect("safe-text parse result");
    assert_eq!(actual, expected);
    assert!(world.safe_ops_error_code.is_none());
}

#[then(expr = "the safe-text u16 parse result should be {int}")]
async fn then_safe_ops_u16_parse_result(world: &mut CopybookWorld, expected: i64) {
    let expected = u16::try_from(expected).expect("expected fits u16");
    let actual = world.safe_ops_u16.expect("safe-text parse result");
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

#[when(expr = "safe_text gets character at index {int}")]
async fn when_safe_text_string_char_at(world: &mut CopybookWorld, index: usize) {
    let input = world.safe_ops_input.clone().expect("safe-op input");
    match safe_string_char_at(&input, index, "bdd-safe-text") {
        Ok(value) => {
            world.safe_ops_char = Some(value);
            world.safe_ops_error_code = None;
        }
        Err(error) => {
            world.safe_ops_char = None;
            world.safe_ops_error_code = Some(error.code);
        }
    }
}

#[then(expr = "the safe-text character should be {string}")]
async fn then_safe_ops_char_result(world: &mut CopybookWorld, expected: String) {
    let expected_char = expected.chars().next().expect("expected single character");
    let actual = world.safe_ops_char.expect("safe-text char result");
    assert_eq!(actual, expected_char);
    assert!(world.safe_ops_error_code.is_none());
}

#[when(expr = "safe_array_bound is called with base {int}, count {int}, and item size {int}")]
async fn when_safe_array_bound(
    world: &mut CopybookWorld,
    base: usize,
    count: usize,
    item_size: usize,
) {
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

#[when(expr = "safe_index gets element at index {int}")]
async fn when_safe_index_get(world: &mut CopybookWorld, index: usize) {
    let sample = [10usize, 20usize, 30usize];
    match safe_slice_get(&sample, index, "bdd-safe-index") {
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

#[then(expr = "safe_index should return {int}")]
async fn then_safe_index_result(world: &mut CopybookWorld, expected: usize) {
    let actual = world.safe_ops_usize.expect("safe-index result");
    assert_eq!(actual, expected);
    assert!(world.safe_ops_error_code.is_none());
}
