// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::edited_pic::{decode_edited_numeric, tokenize_edited_pic};
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
fn cbkd423_edited_pic_blank_when_zero_warning_is_emitted() {
    let captured = Arc::new(Mutex::new(Vec::new()));
    let subscriber = tracing_subscriber::fmt()
        .with_ansi(false)
        .with_writer({
            let captured = Arc::clone(&captured);
            move || CaptureWriter(Arc::clone(&captured))
        })
        .finish();
    let _guard = tracing::subscriber::set_default(subscriber);

    let pattern = tokenize_edited_pic("ZZZ9").expect("edited PIC should tokenize");
    let value = decode_edited_numeric("    ", &pattern, 0, true)
        .expect("blank when zero should decode successfully");
    assert_eq!(value.to_decimal_string(), "0");
    drop(_guard);

    let log = String::from_utf8(captured.lock().unwrap().clone()).unwrap();
    assert!(log.contains("CBKD423_EDITED_PIC_BLANK_WHEN_ZERO"), "{log}");
}
