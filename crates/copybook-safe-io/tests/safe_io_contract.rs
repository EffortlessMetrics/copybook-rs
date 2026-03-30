use copybook_safe_io::{extract_panic_message, panic_caused_by_std_pipe};

#[test]
fn detects_broken_pipe_stdout_panic_message() {
    let payload = "thread panicked: failed printing to stdout: Broken pipe (os error 32)";
    assert!(panic_caused_by_std_pipe(&payload));
}

#[test]
fn ignores_non_stdio_panic_messages() {
    let payload = "something else failed";
    assert!(!panic_caused_by_std_pipe(&payload));
}

#[test]
fn extracts_string_payloads() {
    let payload = String::from("boom");
    assert_eq!(extract_panic_message(&payload), "boom");
}

use std::io::{self, Write};

struct BrokenPipeWriter;

impl Write for BrokenPipeWriter {
    fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
        Err(io::Error::new(io::ErrorKind::BrokenPipe, "broken"))
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[test]
fn write_all_ignoring_closed_swallows_broken_pipe() {
    let mut writer = BrokenPipeWriter;
    let result = copybook_safe_io::write_all_ignoring_closed(&mut writer, b"data");
    assert!(result.is_ok());
}
