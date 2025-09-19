use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use predicates::str::contains;
use std::process::Command;

#[test]
fn edited_pic_is_a_hard_error() -> Result<(), Box<dyn std::error::Error>> {
    // Edited picture (unsupported): ZZ9.99
    let cpy = r#"
01 REC.
   05 AMT PIC ZZ9.99.
"#;

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("edited_pic.cpy");
    f.write_str(cpy)?;

    Command::cargo_bin("copybook")?
        .args(["inspect", f.path().to_str().unwrap()])
        .assert()
        .failure()
        .stdout(contains("CBKP051").and(contains("edited PIC")));

    Ok(())
}
