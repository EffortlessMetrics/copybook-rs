use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::process::Command;

#[test]
fn edited_pic_is_a_hard_error() -> Result<(), Box<dyn std::error::Error>> {
    // Edited picture (unsupported): ZZ9.99
    let cpy = r"
01 REC.
   05 AMT PIC ZZ9.99.
";

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("edited_pic.cpy");
    f.write_str(cpy)?;

    let output = Command::cargo_bin("copybook")?
        .args(["inspect", f.path().to_str().unwrap()])
        .output()?;

    assert!(!output.status.success());
    let mut all = String::new();
    all.push_str(&String::from_utf8_lossy(&output.stdout));
    all.push_str(&String::from_utf8_lossy(&output.stderr));
    assert!(all.contains("CBKP051") && all.contains("edited PIC"));

    Ok(())
}
