#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use common::{TestResult, bin};

#[test]
#[allow(clippy::unnecessary_wraps)]
fn help_exits_zero() -> TestResult<()> {
    bin().arg("--help").assert().success();
    Ok(())
}

#[test]
#[allow(clippy::unnecessary_wraps)]
fn version_exits_zero() -> TestResult<()> {
    bin().arg("--version").assert().success();
    Ok(())
}
