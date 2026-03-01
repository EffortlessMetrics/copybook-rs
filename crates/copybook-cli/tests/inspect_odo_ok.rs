#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]

mod test_utils;

use assert_cmd::cargo::cargo_bin_cmd;
use assert_fs::prelude::*;
use test_utils::{TestResult, path_to_str};

#[test]
fn odo_copybook_loads_in_both_modes() -> TestResult<()> {
    // Counter precedes array; simple 0..3 bound
    let cpy = r"
01 REC.
   05 CNT           PIC 9(1).
   05 ARR OCCURS 0 TO 3 DEPENDING ON CNT.
      10 A         PIC X.
";

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("odo_ok.cpy");
    f.write_str(cpy)?;

    let copybook_str = path_to_str(f.path())?;

    // Test that ODO copybook loads successfully (lenient mode)
    cargo_bin_cmd!("copybook")
        .args(["inspect", copybook_str])
        .assert()
        .success();

    // strict
    cargo_bin_cmd!("copybook")
        .args(["inspect", "--strict", copybook_str])
        .assert()
        .success();

    Ok(())
}
