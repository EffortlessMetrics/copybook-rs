#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod test_utils;

use assert_cmd::cargo::cargo_bin_cmd;
use assert_fs::prelude::*;
use test_utils::{TestResult, path_to_str};

#[test]
fn edited_pic_is_a_hard_error() -> TestResult<()> {
    // Edited picture (unsupported): ZZ9.99
    let cpy = r"
01 REC.
   05 AMT PIC ZZ9.99.
";

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("edited_pic.cpy");
    f.write_str(cpy)?;

    let copybook_str = path_to_str(f.path())?;

    let output = cargo_bin_cmd!("copybook")
        .args(["inspect", copybook_str])
        .output()?;

    assert!(!output.status.success());
    let mut all = String::new();
    all.push_str(&String::from_utf8_lossy(&output.stdout));
    all.push_str(&String::from_utf8_lossy(&output.stderr));
    assert!(all.contains("CBKP051") && all.contains("edited PIC"));

    Ok(())
}
