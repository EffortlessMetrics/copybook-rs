mod common;

use common::{TestResult, bin};

#[test]
fn help_exits_zero() -> TestResult<()> {
    bin().arg("--help").assert().success();
    Ok(())
}

#[test]
fn version_exits_zero() -> TestResult<()> {
    bin().arg("--version").assert().success();
    Ok(())
}
