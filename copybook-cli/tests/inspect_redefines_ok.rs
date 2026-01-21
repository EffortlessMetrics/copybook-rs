#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod test_utils;

use assert_cmd::cargo::cargo_bin_cmd;
use assert_fs::prelude::*;
use test_utils::{TestResult, path_to_str};

#[test]
fn redefines_views_load() -> TestResult<()> {
    // Two views at same level share bytes
    let cpy = r"
01 REC.
   05 A      PIC X(3).
   05 B REDEFINES A PIC 9(3).
";

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("redefines_ok.cpy");
    f.write_str(cpy)?;

    let copybook_str = path_to_str(f.path())?;

    { let mut cmd = cargo_bin_cmd!("copybook"); cmd.env("NO_COLOR", "1"); cmd }
        .args(["inspect", copybook_str])
        .assert()
        .success();

    Ok(())
}
