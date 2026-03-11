// SPDX-License-Identifier: AGPL-3.0-or-later
//! Panic-safe stdio helpers.

use std::any::Any;
use std::borrow::Cow;
use std::io::{self, ErrorKind, Write};

/// Return true when an `io::Error` indicates the downstream consumer closed.
#[must_use]
pub fn is_consumer_closed(err: &io::Error) -> bool {
    matches!(err.kind(), ErrorKind::BrokenPipe | ErrorKind::WriteZero)
        || err.raw_os_error() == Some(109)
        || err.raw_os_error() == Some(232)
}

/// Write bytes while treating consumer-close conditions as success.
pub fn write_all_ignoring_closed<W: Write>(writer: &mut W, bytes: &[u8]) -> io::Result<()> {
    match writer.write_all(bytes) {
        Ok(()) => Ok(()),
        Err(err) if is_consumer_closed(&err) => Ok(()),
        Err(err) => Err(err),
    }
}

/// Return true when the panic payload indicates stdout/stderr pipeline shutdown.
#[must_use]
pub fn panic_caused_by_std_pipe(panic_payload: &dyn Any) -> bool {
    let message = if let Some(&msg) = panic_payload.downcast_ref::<&str>() {
        msg
    } else if let Some(msg) = panic_payload.downcast_ref::<String>() {
        msg.as_str()
    } else {
        return false;
    };

    let lower = message.to_ascii_lowercase();
    let is_std_stream =
        lower.contains("failed printing to stdout") || lower.contains("failed printing to stderr");
    if !is_std_stream {
        return false;
    }

    let is_broken_pipe = lower.contains("broken pipe")
        || lower.contains("os error 32")
        || lower.contains("error_broken_pipe")
        || lower.contains("error_no_data");
    let is_write_zero = lower.contains("write zero") || lower.contains("writezero");

    is_broken_pipe || is_write_zero
}

/// Extract a panic payload as a user-displayable string.
#[must_use]
pub fn extract_panic_message(panic_payload: &dyn Any) -> Cow<'_, str> {
    if let Some(&msg) = panic_payload.downcast_ref::<&str>() {
        return Cow::Borrowed(msg);
    }
    if let Some(msg) = panic_payload.downcast_ref::<String>() {
        return Cow::Borrowed(msg.as_str());
    }
    Cow::Borrowed("unknown panic")
}
