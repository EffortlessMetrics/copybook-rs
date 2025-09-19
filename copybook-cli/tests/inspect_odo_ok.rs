use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::process::Command;

#[test]
fn odo_copybook_loads_in_both_modes() -> Result<(), Box<dyn std::error::Error>> {
    // Counter precedes array; simple 0..3 bound
    let cpy = r#"
01 REC.
   05 CNT           PIC 9(1).
   05 ARR OCCURS 0 TO 3 DEPENDING ON CNT.
      10 A         PIC X.
"#;

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("odo_ok.cpy");
    f.write_str(cpy)?;

    // Test that ODO copybook loads successfully (lenient mode)
    Command::cargo_bin("copybook")?
        .args(["inspect", f.path().to_str().unwrap()])
        .assert()
        .success();

    // strict
    Command::cargo_bin("copybook")?
        .args(["inspect", "--strict", f.path().to_str().unwrap()])
        .assert()
        .success();

    Ok(())
}
