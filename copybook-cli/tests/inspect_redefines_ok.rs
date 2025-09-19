use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::process::Command;

#[test]
fn redefines_views_load() -> Result<(), Box<dyn std::error::Error>> {
    // Two views at same level share bytes
    let cpy = r#"
01 REC.
   05 A      PIC X(3).
   05 B REDEFINES A PIC 9(3).
"#;

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("redefines_ok.cpy");
    f.write_str(cpy)?;

    Command::cargo_bin("copybook")?
        .args(["inspect", f.path().to_str().unwrap()])
        .assert()
        .success();

    Ok(())
}
