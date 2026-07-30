// SPDX-License-Identifier: AGPL-3.0-or-later
//! Clean-room proof for direct copybook-fixed consumers.

use copybook_error::ErrorCode;
use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::Cursor;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::with_lrecl(&mut encoded, 8)?;
        writer.write_record(b"ABCD")?;
        writer.write_record(b"EFGH1234")?;
        writer.flush()?;
        assert_eq!(writer.record_count(), 2);
    }
    assert_eq!(encoded, b"ABCD\0\0\0\0EFGH1234");

    let mut reader = FixedRecordReader::with_lrecl(Cursor::new(encoded), 8)?;
    assert_eq!(reader.read_record()?.as_deref(), Some(&b"ABCD\0\0\0\0"[..]));
    assert_eq!(reader.read_record()?.as_deref(), Some(&b"EFGH1234"[..]));
    assert!(reader.read_record()?.is_none());
    assert_eq!(reader.record_count(), 2);

    let mut oversized = FixedRecordWriter::with_lrecl(Vec::new(), 4)?;
    let error = oversized.write_record(b"TOO-LONG").unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);

    let mut truncated = FixedRecordReader::with_lrecl(Cursor::new(b"COMPLETE!tail"), 9)?;
    assert_eq!(truncated.read_record()?.as_deref(), Some(&b"COMPLETE!"[..]));
    let error = truncated.read_record().unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);

    Ok(())
}
