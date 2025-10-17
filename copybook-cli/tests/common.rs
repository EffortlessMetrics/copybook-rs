use assert_cmd::Command;
use std::error::Error;
use std::fs;
use std::path::Path;

pub type TestResult<T> = Result<T, Box<dyn Error>>;

pub fn bin() -> TestResult<Command> {
    Ok(Command::cargo_bin("copybook")?)
}

pub fn write_file(path: &Path, contents: impl AsRef<[u8]>) -> TestResult<()> {
    fs::write(path, contents.as_ref())?;
    Ok(())
}
