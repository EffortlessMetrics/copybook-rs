// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::Codepage;
use copybook_codec::numeric::decode_zoned_decimal;
use std::io::{self, Write};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
struct CaptureWriter(Arc<Mutex<Vec<u8>>>);

impl Write for CaptureWriter {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(bytes);
        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[test]
fn cbkd412_zoned_blank_is_zero_warning_is_emitted() {
    let captured = Arc::new(Mutex::new(Vec::new()));
    let subscriber = tracing_subscriber::fmt()
        .with_ansi(false)
        .with_writer({
            let captured = Arc::clone(&captured);
            move || CaptureWriter(Arc::clone(&captured))
        })
        .finish();
    let _guard = tracing::subscriber::set_default(subscriber);

    let value = decode_zoned_decimal(b"   ", 3, 0, false, Codepage::ASCII, true)
        .expect("blank when zero should decode successfully");
    assert_eq!(value.to_string(), "0");
    drop(_guard);

    let log = String::from_utf8(captured.lock().unwrap().clone()).unwrap();
    assert!(log.contains("CBKD412_ZONED_BLANK_IS_ZERO"), "{log}");
}
