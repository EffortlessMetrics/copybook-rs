#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]

mod test_utils;

use assert_cmd::cargo::cargo_bin_cmd;
use assert_fs::prelude::*;
use test_utils::{TestResult, path_to_str};

/// Test that edited PICs are now supported and can be inspected successfully
/// Phase E1: Edited PICs parse into schema with EditedNumeric field kind
#[test]
fn edited_pic_inspects_successfully() -> TestResult<()> {
    // Edited picture (supported in E1/E2): ZZZZ (simple zero suppression)
    let cpy = r"
01 REC.
   05 AMT PIC ZZZZ.
";

    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("edited_pic.cpy");
    f.write_str(cpy)?;

    let copybook_str = path_to_str(f.path())?;

    let output = cargo_bin_cmd!("copybook")
        .args(["inspect", copybook_str])
        .output()?;

    // Should succeed now that edited PICs are supported (Phase E1)
    assert!(
        output.status.success(),
        "inspect should succeed for edited PIC"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify the output shows EDITED field kind (human-readable format)
    assert!(
        stdout.contains("EDITED"),
        "Should show EDITED field kind in output"
    );
    assert!(stdout.contains("ZZZZ"), "Should show the PIC pattern");

    Ok(())
}
