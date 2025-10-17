mod common;
mod test_utils;

use common::{TestResult, bin};
use serde_json::Value;
use std::str;

#[test]
fn parse_emits_valid_json_on_stdout_only() -> TestResult<()> {
    let copybook_path = test_utils::fixture_path("copybooks/simple.cpy")?;

    let mut cmd = bin()?;
    let assert = cmd.arg("parse").arg(&copybook_path).assert().success();
    let output = assert.get_output();
    let stdout = str::from_utf8(&output.stdout)?;
    let _parsed: Value = serde_json::from_str(stdout)?;
    assert!(
        !stdout.contains("copybook-cli start"),
        "stdout should remain telemetry-free: {stdout}"
    );
    Ok(())
}
