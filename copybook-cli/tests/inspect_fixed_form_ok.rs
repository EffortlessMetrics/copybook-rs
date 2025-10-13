mod test_utils;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::process::Command;
use test_utils::{TestResult, path_to_str};

#[test]
fn fixed_form_sequence_and_continuation_are_handled() -> TestResult<()> {
    // Columns: 1-6 seq, 7 indicator, 8-72 code, 73-80 id/comment
    // Includes column-7 continuation and trailing id area numbers.
    let cpy = "\
123456 01 RECORD-NAME.                                              12345678
123456-     05 VERY-LONG-FIELD-NAME-THAT-NEEDS-                     12345678
123456-        CONTINUATION            PIC X(10).                    12345678
";

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("fixed_form_ok.cpy");
    f.write_str(cpy)?;

    let copybook_str = path_to_str(f.path())?;

    Command::cargo_bin("copybook")?
        .args(["inspect", copybook_str])
        .assert()
        .success();

    Ok(())
}
