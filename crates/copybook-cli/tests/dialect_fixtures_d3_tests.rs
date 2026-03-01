// SPDX-License-Identifier: AGPL-3.0-or-later
//! D3 Dialect Fixtures Tests
//!
//! These tests verify that the CLI dialect flags work correctly.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod test_utils;

use test_utils::{TestResult, bin};

/// Test that "n" (normative) dialect flag is accepted
#[test]
fn test_dialect_flag_n_accepted() -> TestResult<()> {
    let mut cmd = bin();
    cmd.arg("parse")
        .arg(test_utils::test_data_path("test-schema.cpy"))
        .arg("--dialect")
        .arg("n")
        .assert()
        .success();
    Ok(())
}

/// Test that "0" (zero-tolerant) dialect flag is accepted
#[test]
fn test_dialect_flag_0_accepted() -> TestResult<()> {
    let mut cmd = bin();
    cmd.arg("parse")
        .arg(test_utils::test_data_path("test-schema.cpy"))
        .arg("--dialect")
        .arg("0")
        .assert()
        .success();
    Ok(())
}

/// Test that "1" (one-tolerant) dialect flag is accepted
#[test]
fn test_dialect_flag_1_accepted() -> TestResult<()> {
    let mut cmd = bin();
    cmd.arg("parse")
        .arg(test_utils::test_data_path("test-schema.cpy"))
        .arg("--dialect")
        .arg("1")
        .assert()
        .success();
    Ok(())
}

/// Test that COPYBOOK_DIALECT env var is respected
#[test]
fn test_env_var_copybook_dialect() -> TestResult<()> {
    let mut cmd = bin();
    cmd.env("COPYBOOK_DIALECT", "n")
        .arg("parse")
        .arg(test_utils::test_data_path("test-schema.cpy"))
        .assert()
        .success();
    Ok(())
}
