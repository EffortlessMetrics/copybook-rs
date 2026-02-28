#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Fixed-length record framing primitives.
//!
//! This crate intentionally focuses on one concern:
//! reading and writing LRECL-framed records with deterministic padding and
//! structured error mapping.
//!
//! Use [`FixedRecordReader`] to consume fixed-length records from a byte stream
//! and [`FixedRecordWriter`] to produce them with automatic null-byte padding.

use copybook_core::Schema;
use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use std::convert::TryFrom;
use std::io::{ErrorKind, Read, Write};
use tracing::{debug, warn};

/// Fixed record reader for processing fixed-length records.
#[derive(Debug)]
pub struct FixedRecordReader<R: Read> {
    input: R,
    lrecl: u32,
    record_count: u64,
}

impl<R: Read> FixedRecordReader<R> {
    /// Create a new fixed record reader.
    ///
    /// # Errors
    /// Returns an error if no LRECL is provided or if it is zero.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn new(input: R, lrecl: Option<u32>) -> Result<Self> {
        let lrecl = lrecl.ok_or_else(|| {
            Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                "Fixed format requires LRECL",
            )
        })?;

        if lrecl == 0 {
            return Err(Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                "LRECL must be greater than zero",
            ));
        }

        Ok(Self {
            input,
            lrecl,
            record_count: 0,
        })
    }

    /// Read the next record.
    ///
    /// # Errors
    /// Returns an error if the record cannot be read due to I/O errors.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn read_record(&mut self) -> Result<Option<Vec<u8>>> {
        // Read one byte first so true EOF can be treated as `Ok(None)`.
        let mut first_byte = [0u8; 1];
        match self.input.read_exact(&mut first_byte) {
            Ok(()) => {
                let lrecl_len = self.lrecl_usize()?;
                let mut buffer = vec![0u8; lrecl_len];
                buffer[0] = first_byte[0];

                if lrecl_len > 1 {
                    match self.input.read_exact(&mut buffer[1..]) {
                        Ok(()) => {
                            self.record_count += 1;
                            debug!(
                                "Read fixed record {} of {} bytes",
                                self.record_count, self.lrecl
                            );
                            Ok(Some(buffer))
                        }
                        Err(e) if e.kind() == ErrorKind::UnexpectedEof => Err(Error::new(
                            ErrorCode::CBKF221_RDW_UNDERFLOW,
                            format!(
                                "Incomplete record at end of file: expected {} bytes",
                                self.lrecl
                            ),
                        )
                        .with_context(ErrorContext {
                            record_index: Some(self.record_count + 1),
                            field_path: None,
                            byte_offset: None,
                            line_number: None,
                            details: Some("File ends with partial record".to_string()),
                        })),
                        Err(e) => Err(Error::new(
                            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                            format!("I/O error reading record: {e}"),
                        )
                        .with_context(ErrorContext {
                            record_index: Some(self.record_count + 1),
                            field_path: None,
                            byte_offset: None,
                            line_number: None,
                            details: None,
                        })),
                    }
                } else {
                    self.record_count += 1;
                    debug!(
                        "Read fixed record {} of {} bytes",
                        self.record_count, self.lrecl
                    );
                    Ok(Some(buffer))
                }
            }
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                debug!("Reached EOF after {} records", self.record_count);
                Ok(None)
            }
            Err(e) => Err(Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error reading record: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: None,
            })),
        }
    }

    /// Validate record length against schema expectations.
    ///
    /// # Errors
    /// Returns an error if the record length does not match configured LRECL.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn validate_record_length(&self, schema: &Schema, record_data: &[u8]) -> Result<()> {
        let lrecl_len = self.lrecl_usize()?;
        if record_data.len() != lrecl_len {
            return Err(Error::new(
                ErrorCode::CBKF221_RDW_UNDERFLOW,
                format!(
                    "Record length mismatch: expected {}, got {}",
                    self.lrecl,
                    record_data.len()
                ),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("Fixed record length validation failed".to_string()),
            }));
        }

        if let Some(schema_lrecl) = schema.lrecl_fixed
            && self.lrecl != schema_lrecl
        {
            warn!(
                "LRECL mismatch: reader configured for {}, schema expects {}",
                self.lrecl, schema_lrecl
            );
        }

        if schema.tail_odo.is_some() {
            debug!("Record has ODO tail, variable length within fixed LRECL is expected");
        }

        Ok(())
    }

    /// Get the current record count.
    #[must_use]
    #[inline]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Get the configured LRECL.
    #[must_use]
    #[inline]
    pub fn lrecl(&self) -> u32 {
        self.lrecl
    }

    #[inline]
    fn lrecl_usize(&self) -> Result<usize> {
        usize::try_from(self.lrecl).map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "LRECL exceeds platform addressable size",
            )
        })
    }
}

/// Fixed record writer for writing fixed-length records.
#[derive(Debug)]
pub struct FixedRecordWriter<W: Write> {
    output: W,
    lrecl: u32,
    record_count: u64,
}

impl<W: Write> FixedRecordWriter<W> {
    /// Create a new fixed record writer.
    ///
    /// # Errors
    /// Returns an error if no LRECL is provided or if it is zero.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn new(output: W, lrecl: Option<u32>) -> Result<Self> {
        let lrecl = lrecl.ok_or_else(|| {
            Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                "Fixed format requires LRECL",
            )
        })?;

        if lrecl == 0 {
            return Err(Error::new(
                ErrorCode::CBKI001_INVALID_STATE,
                "LRECL must be greater than zero",
            ));
        }

        Ok(Self {
            output,
            lrecl,
            record_count: 0,
        })
    }

    /// Write a record and pad with `0x00` to LRECL.
    ///
    /// # Errors
    /// Returns an error if the record is longer than LRECL or I/O fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record(&mut self, data: &[u8]) -> Result<()> {
        let data_len = data.len();
        let lrecl = self.lrecl_usize()?;

        if data_len > lrecl {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Record too long: {data_len} bytes exceeds LRECL of {lrecl}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: Some("Record exceeds fixed length".to_string()),
            }));
        }

        self.output.write_all(data).map_err(|e| {
            Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error writing record: {e}"),
            )
            .with_context(ErrorContext {
                record_index: Some(self.record_count + 1),
                field_path: None,
                byte_offset: None,
                line_number: None,
                details: None,
            })
        })?;

        if data_len < lrecl {
            let padding = vec![0u8; lrecl - data_len];
            self.output.write_all(&padding).map_err(|e| {
                Error::new(
                    ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                    format!("I/O error writing padding: {e}"),
                )
                .with_context(ErrorContext {
                    record_index: Some(self.record_count + 1),
                    field_path: None,
                    byte_offset: Some(u64::try_from(data_len).unwrap_or(u64::MAX)),
                    line_number: None,
                    details: Some("Error writing record padding".to_string()),
                })
            })?;
        }

        self.record_count += 1;
        debug!(
            "Wrote fixed record {} of {} bytes (data: {}, padding: {})",
            self.record_count,
            lrecl,
            data_len,
            lrecl - data_len
        );
        Ok(())
    }

    /// Flush the output.
    ///
    /// # Errors
    /// Returns an error if the flush operation fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn flush(&mut self) -> Result<()> {
        self.output.flush().map_err(|e| {
            Error::new(
                ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
                format!("I/O error flushing output: {e}"),
            )
        })
    }

    /// Get the current record count.
    #[must_use]
    #[inline]
    pub fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Get the configured LRECL.
    #[must_use]
    #[inline]
    pub fn lrecl(&self) -> u32 {
        self.lrecl
    }

    #[inline]
    fn lrecl_usize(&self) -> Result<usize> {
        usize::try_from(self.lrecl).map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "LRECL exceeds platform addressable size",
            )
        })
    }
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
mod tests {
    use super::*;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use std::io::Cursor;

    #[test]
    fn fixed_record_reader_basic() {
        let data = b"ABCD1234EFGH5678";
        let mut reader = FixedRecordReader::new(Cursor::new(data), Some(8)).unwrap();

        let record1 = reader.read_record().unwrap().unwrap();
        assert_eq!(record1, b"ABCD1234");
        assert_eq!(reader.record_count(), 1);

        let record2 = reader.read_record().unwrap().unwrap();
        assert_eq!(record2, b"EFGH5678");
        assert_eq!(reader.record_count(), 2);

        let record3 = reader.read_record().unwrap();
        assert!(record3.is_none());
    }

    #[test]
    fn fixed_record_reader_partial_record_is_underflow() {
        let data = b"ABCD123";
        let mut reader = FixedRecordReader::new(Cursor::new(data), Some(8)).unwrap();

        let error = reader.read_record().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    }

    #[test]
    fn fixed_record_reader_zero_lrecl_is_invalid_state() {
        let data = b"test";
        let error = FixedRecordReader::new(Cursor::new(data), Some(0)).unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
    }

    #[test]
    fn fixed_record_reader_missing_lrecl_is_invalid_state() {
        let data = b"test";
        let error = FixedRecordReader::new(Cursor::new(data), None).unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
    }

    #[test]
    fn fixed_record_writer_basic() {
        let mut output = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut output, Some(8)).unwrap();

        writer.write_record(b"ABCD1234").unwrap();
        writer.write_record(b"XYZ").unwrap();
        writer.flush().unwrap();

        assert_eq!(writer.record_count(), 2);
        assert_eq!(output, b"ABCD1234XYZ\x00\x00\x00\x00\x00");
    }

    #[test]
    fn fixed_record_writer_too_long_is_cbke501() {
        let mut output = Vec::new();
        let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();

        let error = writer.write_record(b"ABCDEFGH").unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    }

    #[test]
    fn fixed_record_writer_zero_lrecl_is_invalid_state() {
        let mut output = Vec::new();
        let error = FixedRecordWriter::new(&mut output, Some(0)).unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
    }

    #[test]
    fn fixed_record_writer_missing_lrecl_is_invalid_state() {
        let mut output = Vec::new();
        let error = FixedRecordWriter::new(&mut output, None).unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
    }

    #[test]
    fn validate_record_length_ok() {
        let data = b"ABCD1234";
        let mut reader = FixedRecordReader::new(Cursor::new(data), Some(8)).unwrap();
        let record = reader.read_record().unwrap().unwrap();

        let schema = Schema::new();
        reader.validate_record_length(&schema, &record).unwrap();
    }

    #[test]
    fn validate_record_length_mismatch_is_underflow() {
        let data = b"ABCD1234";
        let mut reader = FixedRecordReader::new(Cursor::new(data), Some(8)).unwrap();
        let _ = reader.read_record().unwrap().unwrap();

        let schema = Schema::new();
        let error = reader.validate_record_length(&schema, b"ABC").unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    }

    proptest! {
        #[test]
        fn prop_fixed_writer_reader_roundtrip(
            lrecl in 1u16..=512u16,
            payload in vec(any::<u8>(), 0..=512),
        ) {
            prop_assume!(payload.len() <= usize::from(lrecl));
            let mut encoded = Vec::new();
            let mut writer = FixedRecordWriter::new(&mut encoded, Some(u32::from(lrecl))).unwrap();
            writer.write_record(&payload).unwrap();
            writer.flush().unwrap();
            prop_assert_eq!(encoded.len(), usize::from(lrecl));
            prop_assert_eq!(&encoded[..payload.len()], payload.as_slice());

            let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(u32::from(lrecl))).unwrap();
            let decoded = reader.read_record().unwrap().unwrap();
            prop_assert_eq!(decoded, encoded.as_slice());
            prop_assert_eq!(reader.read_record().unwrap(), None);
        }

        #[test]
        fn prop_fixed_writer_rejects_oversize_payload(
            lrecl in 1u16..=128u16,
            extra in 1usize..=64usize,
        ) {
            let mut output = Vec::new();
            let mut writer = FixedRecordWriter::new(&mut output, Some(u32::from(lrecl))).unwrap();
            let payload = vec![0x41; usize::from(lrecl) + extra];
            let error = writer.write_record(&payload).unwrap_err();
            prop_assert_eq!(error.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
        }
    }
}
