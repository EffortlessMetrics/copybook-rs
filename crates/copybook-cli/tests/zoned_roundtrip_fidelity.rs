// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI byte-fidelity witness for ASCII zoned decimal preservation (issues #48, #1176).

use std::error::Error;
use std::fs;
use std::io;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn test_cli_roundtrip_cmp_validation() -> Result<(), Box<dyn Error>> {
    let temp_dir = TempDir::new()?;
    let copybook_path = temp_dir.path().join("test.cpy");
    let original_data_path = temp_dir.path().join("original.bin");
    let json_path = temp_dir.path().join("intermediate.jsonl");
    let roundtrip_data_path = temp_dir.path().join("roundtrip.bin");
    let original_data = b"12345";
    fs::write(&copybook_path, "01 ZONED-FIELD PIC 9(5).")?;
    fs::write(&original_data_path, original_data)?;

    // Cargo supplies this package's just-built binary, including custom target directories.
    let binary = env!("CARGO_BIN_EXE_copybook");
    let decode = Command::new(binary)
        .args([
            "decode",
            "--preserve-zoned-encoding",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&json_path)
        .arg(&copybook_path)
        .arg(&original_data_path)
        .output()?;
    if !decode.status.success() {
        return Err(io::Error::other(format!(
            "decode failed ({}): {}",
            decode.status,
            String::from_utf8_lossy(&decode.stderr)
        ))
        .into());
    }

    let encode = Command::new(binary)
        .args([
            "encode",
            "--zoned-encoding-override",
            "ascii",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&roundtrip_data_path)
        .arg(&copybook_path)
        .arg(&json_path)
        .output()?;
    if !encode.status.success() {
        return Err(io::Error::other(format!(
            "encode failed ({}): {}",
            encode.status,
            String::from_utf8_lossy(&encode.stderr)
        ))
        .into());
    }

    let roundtrip_data = fs::read(&roundtrip_data_path)?;
    if roundtrip_data != original_data {
        return Err(io::Error::other(format!(
            "ASCII zoned round-trip changed bytes: expected {original_data:?}, got {roundtrip_data:?}"
        ))
        .into());
    }
    Ok(())
}
