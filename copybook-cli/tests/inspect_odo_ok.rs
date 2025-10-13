mod test_utils;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::process::Command;
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
    Command::cargo_bin("copybook")?
        .args(["inspect", copybook_str])
        .assert()
        .success();

    // strict
    Command::cargo_bin("copybook")?
        .args(["inspect", "--strict", copybook_str])
        .assert()
        .success();

    Ok(())
}
