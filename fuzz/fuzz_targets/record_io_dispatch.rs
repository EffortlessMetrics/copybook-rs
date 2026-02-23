#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_options::RecordFormat;
use copybook_record_io::{read_record, write_record};
use libfuzzer_sys::fuzz_target;
use std::io::Cursor;

fuzz_target!(|data: &[u8]| {
    let format = if data.first().is_some_and(|b| (b & 1) == 1) {
        RecordFormat::RDW
    } else {
        RecordFormat::Fixed
    };

    let lrecl = data
        .get(1)
        .map(|b| u32::from(*b % 128) + 1)
        .unwrap_or(1u32);

    let payload = if data.len() > 2 { &data[2..] } else { &[] };

    let mut encoded = Vec::new();
    let _ = write_record(&mut encoded, payload, format);

    let mut encoded_cursor = Cursor::new(encoded.as_slice());
    let _ = read_record(&mut encoded_cursor, format, Some(lrecl));
    let _ = read_record(&mut encoded_cursor, format, Some(lrecl));

    let mut raw_cursor = Cursor::new(data);
    let _ = read_record(&mut raw_cursor, format, Some(lrecl));

    if format == RecordFormat::Fixed {
        // Exercise fixed-format contract validation paths.
        let mut fixed_cursor = Cursor::new(payload);
        let _ = read_record(&mut fixed_cursor, RecordFormat::Fixed, None);
        let _ = read_record(&mut fixed_cursor, RecordFormat::Fixed, Some(0));
    }
});
