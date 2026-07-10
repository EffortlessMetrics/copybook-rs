#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::panic)]
use assert_cmd::Command;
use assert_cmd::cargo::cargo_bin_cmd;
use std::error::Error;
use std::fs;
use std::path::Path;

pub type TestResult<T> = Result<T, Box<dyn Error>>;

#[must_use]
#[inline]
#[allow(dead_code)]
pub fn bin() -> Command {
    cargo_bin_cmd!("copybook")
}

#[inline]
#[allow(dead_code)]
pub fn write_file(path: &Path, contents: impl AsRef<[u8]>) -> TestResult<()> {
    fs::write(path, contents.as_ref())?;
    Ok(())
}
