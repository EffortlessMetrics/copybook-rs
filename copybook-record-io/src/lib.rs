#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Record-format dispatch and facade re-exports for framing microcrates.
//!
//! This crate deliberately owns one concern: route legacy single-record I/O
//! calls to either fixed-LRECL or RDW framing implementations.

use copybook_error::{Error, ErrorCode, Result};
use copybook_options::RecordFormat;
use std::io::{self, Read, Write};
use std::path::Path;
use tempfile::NamedTempFile;

pub use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
pub use copybook_rdw::{RDWRecord, RDWRecordReader, RDWRecordWriter};

/// Read one record from input using the selected record format.
///
/// # Errors
/// Returns an error when the delegated fixed/RDW framing read fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn read_record(
    input: &mut impl Read,
    format: RecordFormat,
    lrecl: Option<u32>,
) -> Result<Option<Vec<u8>>> {
    match format {
        RecordFormat::Fixed => read_fixed_record(input, lrecl),
        RecordFormat::RDW => read_rdw_record(input),
    }
}

#[inline]
fn read_fixed_record(input: &mut impl Read, lrecl: Option<u32>) -> Result<Option<Vec<u8>>> {
    let mut reader = FixedRecordReader::new(input, lrecl)?;
    reader.read_record()
}

#[inline]
fn read_rdw_record(input: &mut impl Read) -> Result<Option<Vec<u8>>> {
    let mut reader = RDWRecordReader::new(input, false);
    match reader.read_record()? {
        Some(record) => Ok(Some(record.payload)),
        None => Ok(None),
    }
}

/// Write one record to output using the selected record format.
///
/// # Errors
/// Returns an error when the delegated fixed/RDW framing write fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn write_record(output: &mut impl Write, data: &[u8], format: RecordFormat) -> Result<()> {
    match format {
        RecordFormat::Fixed => {
            output.write_all(data).map_err(|e| {
                Error::new(
                    ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                    format!("Write error: {e}"),
                )
            })?;
            Ok(())
        }
        RecordFormat::RDW => {
            let mut writer = RDWRecordWriter::new(output);
            writer.write_record_from_payload(data, None)
        }
    }
}

/// Atomically write data to a file using temporary file + rename.
///
/// This ensures the target path is either fully written or left untouched.
///
/// # Errors
/// Returns an error if creating/writing/flushing/persisting the temporary file fails.
pub fn atomic_write<P: AsRef<Path>, F>(path: P, write_fn: F) -> io::Result<()>
where
    F: FnOnce(&mut dyn Write) -> io::Result<()>,
{
    let path = path.as_ref();
    let temp_dir = path.parent().unwrap_or_else(|| Path::new("."));
    let mut temp_file = NamedTempFile::new_in(temp_dir)?;

    write_fn(&mut temp_file)?;
    temp_file.flush()?;
    temp_file.as_file().sync_all()?;
    temp_file.persist(path)?;

    Ok(())
}

/// Read UTF-8 text from `path`, or from stdin when `path` is `"-"`.
///
/// # Errors
/// Returns an error if file/stdin reading fails.
pub fn read_text_file_or_stdin<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let path = path.as_ref();

    if path == Path::new("-") {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        Ok(buffer)
    } else {
        std::fs::read_to_string(path)
    }
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
mod tests {
    use super::*;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use std::io::Cursor;
    use tempfile::tempdir;

    #[test]
    fn read_fixed_record_delegates_to_fixed_microcrate() {
        let mut cursor = Cursor::new(b"LOCKTEST".to_vec());
        let record = read_record(&mut cursor, RecordFormat::Fixed, Some(8))
            .unwrap()
            .unwrap();
        assert_eq!(record, b"LOCKTEST");
    }

    #[test]
    fn read_record_fixed_requires_lrecl() {
        let mut cursor = Cursor::new(b"LOCKTEST".to_vec());
        let err = read_record(&mut cursor, RecordFormat::Fixed, None).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
    }

    #[test]
    fn read_rdw_record_returns_payload_only() {
        let mut cursor = Cursor::new(vec![0x00, 0x04, 0x00, 0x00, b'T', b'E', b'S', b'T']);
        let record = read_record(&mut cursor, RecordFormat::RDW, None)
            .unwrap()
            .unwrap();
        assert_eq!(record, b"TEST");
    }

    #[test]
    fn write_fixed_record_passthrough() {
        let mut output = Vec::new();
        write_record(&mut output, b"LOCK", RecordFormat::Fixed).unwrap();
        assert_eq!(output, b"LOCK");
    }

    #[test]
    fn write_rdw_record_emits_header_and_payload() {
        let mut output = Vec::new();
        write_record(&mut output, b"ABCD", RecordFormat::RDW).unwrap();
        assert_eq!(output, vec![0x00, 0x04, 0x00, 0x00, b'A', b'B', b'C', b'D']);
    }

    #[test]
    fn write_rdw_record_rejects_oversized_payload() {
        let mut output = Vec::new();
        let oversized = vec![0u8; usize::from(u16::MAX) + 1];
        let err = write_record(&mut output, &oversized, RecordFormat::RDW).unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    }

    #[test]
    fn atomic_write_writes_file() {
        let temp_dir = tempdir().unwrap();
        let target_path = temp_dir.path().join("atomic.txt");

        atomic_write(&target_path, |writer| writer.write_all(b"hello")).unwrap();

        assert_eq!(std::fs::read_to_string(target_path).unwrap(), "hello");
    }

    #[test]
    fn atomic_write_failure_does_not_create_target() {
        let temp_dir = tempdir().unwrap();
        let target_path = temp_dir.path().join("atomic.txt");

        let result = atomic_write(&target_path, |_writer| Err(io::Error::other("boom")));

        assert!(result.is_err());
        assert!(!target_path.exists());
    }

    #[test]
    fn read_text_file_or_stdin_reads_file() {
        let temp_dir = tempdir().unwrap();
        let target_path = temp_dir.path().join("input.txt");
        std::fs::write(&target_path, "abc").unwrap();

        let got = read_text_file_or_stdin(&target_path).unwrap();
        assert_eq!(got, "abc");
    }

    proptest! {
        #[test]
        fn prop_fixed_roundtrip_when_lrecl_equals_payload_len(payload in vec(any::<u8>(), 1..=512)) {
            let mut encoded = Vec::new();
            write_record(&mut encoded, &payload, RecordFormat::Fixed).unwrap();

            let mut cursor = Cursor::new(encoded);
            let decoded = read_record(
                &mut cursor,
                RecordFormat::Fixed,
                Some(u32::try_from(payload.len()).unwrap()),
            )
            .unwrap()
            .unwrap();

            prop_assert_eq!(decoded.as_slice(), payload.as_slice());
            prop_assert!(read_record(
                &mut cursor,
                RecordFormat::Fixed,
                Some(u32::try_from(payload.len()).unwrap()),
            ).unwrap().is_none());
        }

        #[test]
        fn prop_rdw_roundtrip_preserves_payload(payload in vec(any::<u8>(), 0..=1024)) {
            let mut encoded = Vec::new();
            write_record(&mut encoded, &payload, RecordFormat::RDW).unwrap();

            let mut cursor = Cursor::new(encoded);
            let decoded = read_record(&mut cursor, RecordFormat::RDW, None).unwrap().unwrap();
            prop_assert_eq!(decoded, payload);
            prop_assert!(read_record(&mut cursor, RecordFormat::RDW, None).unwrap().is_none());
        }
    }
}
