// SPDX-License-Identifier: AGPL-3.0-or-later
//! Record iterator for streaming access to decoded records.

use crate::options::DecodeOptions;
use copybook_core::{Result, Schema};
use copybook_record_iterator::{RawRecordReader, raw_records, raw_records_from_file};
use serde_json::Value;
use std::io::Read;

/// Iterator over records in a data file, yielding decoded JSON values.
pub struct RecordIterator<R: Read> {
    raw_reader: RawRecordReader<R>,
}

impl<R: Read> RecordIterator<R> {
    /// Create a new record iterator.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn new(reader: R, schema: &Schema, options: &DecodeOptions) -> Result<Self> {
        Ok(Self {
            raw_reader: RawRecordReader::new(reader, schema, options)?,
        })
    }

    /// Get the current record index (1-based).
    #[inline]
    #[must_use]
    pub fn current_record_index(&self) -> u64 {
        self.raw_reader.current_record_index()
    }

    /// Check if the iterator has reached the end of the file.
    #[inline]
    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.raw_reader.is_eof()
    }

    /// Get a reference to the schema being used.
    #[inline]
    #[must_use]
    pub fn schema(&self) -> &Schema {
        self.raw_reader.schema()
    }

    /// Get a reference to the decode options being used.
    #[inline]
    #[must_use]
    pub fn options(&self) -> &DecodeOptions {
        self.raw_reader.options()
    }

    /// Read the next record without decoding it.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn read_raw_record(&mut self) -> Result<Option<Vec<u8>>> {
        self.raw_reader.read_raw_record()
    }

    #[inline]
    fn decode_next_record(&mut self) -> Result<Option<Value>> {
        match self.raw_reader.read_raw_record()? {
            Some(record_bytes) => {
                let json_value = crate::decode_record(
                    self.raw_reader.schema(),
                    &record_bytes,
                    self.raw_reader.options(),
                )?;
                Ok(Some(json_value))
            }
            None => Ok(None),
        }
    }
}

impl<R: Read> Iterator for RecordIterator<R> {
    type Item = Result<Value>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.raw_reader.is_eof() {
            return None;
        }

        match self.decode_next_record() {
            Ok(Some(value)) => Some(Ok(value)),
            Ok(None) => None,
            Err(error) => Some(Err(error)),
        }
    }
}

/// Convenience function to create a record iterator from a file path.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn iter_records_from_file<P: AsRef<std::path::Path>>(
    file_path: P,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RecordIterator<std::fs::File>> {
    Ok(RecordIterator {
        raw_reader: raw_records_from_file(file_path, schema, options)?,
    })
}

/// Convenience function to create a record iterator from any readable source.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn iter_records<R: Read>(
    reader: R,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RecordIterator<R>> {
    Ok(RecordIterator {
        raw_reader: raw_records(reader, schema, options)?,
    })
}
