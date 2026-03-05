#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_rdw::{RDWRecordReader, RDWRecordWriter, RdwHeader, RDW_HEADER_LEN};
use libfuzzer_sys::fuzz_target;
use std::io::Cursor;

/// Fuzz target for RDW record framing.
///
/// Feeds arbitrary bytes through the RDW reader/writer pipeline to verify
/// that malformed or adversarial input never causes panics. Exercises both
/// strict and lenient modes, and validates that successfully read records
/// can be written back.
fuzz_target!(|data: &[u8]| {
    // --- Reader path (lenient mode) ---
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let mut records = Vec::new();
    loop {
        match reader.read_record() {
            Ok(Some(record)) => {
                let _ = record.length();
                let _ = record.reserved();
                let _ = record.as_bytes();
                records.push(record);
                // Limit to prevent OOM on crafted input
                if records.len() >= 256 {
                    break;
                }
            }
            Ok(None) => break,
            Err(_) => break,
        }
    }

    // --- Reader path (strict mode) ---
    let mut strict_reader = RDWRecordReader::new(Cursor::new(data), true);
    loop {
        match strict_reader.read_record() {
            Ok(Some(record)) => {
                let _ = record.length();
                if records.len() >= 256 {
                    break;
                }
            }
            Ok(None) => break,
            Err(_) => break,
        }
    }

    // --- Writer round-trip for successfully parsed records ---
    if !records.is_empty() {
        let mut output = Vec::new();
        let mut writer = RDWRecordWriter::new(&mut output);
        for record in &records {
            let _ = writer.write_record(record);
        }
        let _ = writer.flush();
    }

    // --- Header parsing at various offsets ---
    for offset in 0..data.len().min(32) {
        let slice = &data[offset..];
        if slice.len() >= RDW_HEADER_LEN {
            let mut bytes = [0u8; RDW_HEADER_LEN];
            bytes.copy_from_slice(&slice[..RDW_HEADER_LEN]);
            let header = RdwHeader::from_bytes(bytes);
            let _ = header.length();
            let _ = header.reserved();
            let _ = header.looks_ascii_corrupt();
        }
    }
});
