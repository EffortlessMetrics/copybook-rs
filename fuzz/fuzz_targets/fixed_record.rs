#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use libfuzzer_sys::fuzz_target;
use std::io::Cursor;

fuzz_target!(|data: &[u8]| {
    let lrecl = if data.is_empty() {
        1u32
    } else {
        (u32::from(data[0]) % 128) + 1
    };

    let mut encoded = Vec::new();
    if let Ok(mut writer) = FixedRecordWriter::new(&mut encoded, Some(lrecl)) {
        let max_payload = (lrecl as usize).saturating_add(8);
        let payload_len = data.len().saturating_sub(1).min(max_payload);
        let end = 1 + payload_len;
        let payload = if data.len() > 1 { &data[1..end] } else { &[] };
        let _ = writer.write_record(payload);
        let _ = writer.flush();
    }

    if let Ok(mut reader) = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)) {
        while let Ok(Some(record)) = reader.read_record() {
            let _ = record.len();
        }
    }

    if let Ok(mut raw_reader) = FixedRecordReader::new(Cursor::new(data), Some(lrecl)) {
        while let Ok(Some(record)) = raw_reader.read_record() {
            let _ = record.len();
        }
    }
});
