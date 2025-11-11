#![allow(clippy::missing_errors_doc)]
#![allow(clippy::panic)]
use assert_cmd::Command;
use assert_cmd::cargo::cargo_bin_cmd;
use std::error::Error;
use std::fs;
use std::path::Path;

pub type TestResult<T> = Result<T, Box<dyn Error>>;

#[allow(dead_code)] // shared test helper: some suites only rely on write_file
pub fn bin() -> TestResult<Command> {
    Ok(cargo_bin_cmd!("copybook"))
}

#[allow(dead_code)] // shared test helper: silences per-binary unused warnings
pub fn write_file(path: &Path, contents: impl AsRef<[u8]>) -> TestResult<()> {
    fs::write(path, contents.as_ref())?;
    Ok(())
}
