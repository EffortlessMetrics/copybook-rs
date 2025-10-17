mod common;

use common::{TestResult, bin};
use predicates::str::contains;

const CBKE: i32 = 3;

#[test]
fn unknown_flag_maps_to_cbke_exit_code() -> TestResult<()> {
    bin()?
        .arg("--no-such-flag")
        .assert()
        .failure()
        .code(CBKE)
        .stderr(contains("error:"));
    Ok(())
}

#[test]
fn unknown_subcommand_maps_to_cbke_exit_code() -> TestResult<()> {
    bin()?
        .arg("nope")
        .assert()
        .failure()
        .code(CBKE)
        .stderr(contains("Usage:"));
    Ok(())
}

#[test]
fn missing_required_argument_maps_to_cbke_exit_code() -> TestResult<()> {
    bin()?
        .arg("decode")
        .assert()
        .failure()
        .code(CBKE)
        .stderr(contains("Usage:"));
    Ok(())
}
