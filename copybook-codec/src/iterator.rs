//! Record iterator for streaming access to decoded records
//!
//! This module provides iterator-based access to records for programmatic processing,
//! allowing users to process records one at a time without loading entire files into memory.

use crate::options::{DecodeOptions, RecordFormat};
use copybook_core::{Error, ErrorCode, Result, Schema};
use serde_json::Value;
use std::io::{BufReader, Read};

const FIXED_FORMAT_LRECL_MISSING: &str = "Fixed format requires a fixed record length (LRECL). \
     Set `schema.lrecl_fixed` or use `RecordFormat::Variable`.";

/// Iterator over records in a data file, yielding decoded JSON values
///
/// This iterator provides streaming access to records, processing them one at a time
/// to maintain bounded memory usage even for very large files.
///
/// # Examples
///
/// ```rust
/// use copybook_codec::{RecordIterator, DecodeOptions};
/// use copybook_core::parse_copybook;
/// use std::fs::File;
///
/// # fn example() -> copybook_core::Result<()> {
/// let copybook_text = "01 RECORD.\n   05 ID PIC 9(5).\n   05 NAME PIC X(20).";
/// let schema = parse_copybook(copybook_text)?;
/// let file = File::open("data.bin")?;
/// let options = DecodeOptions::default();
///
/// let mut iterator = RecordIterator::new(file, &schema, &options)?;
///
/// for (record_index, result) in iterator.enumerate() {
///     match result {
///         Ok(json_value) => {
///             println!("Record {}: {}", record_index + 1, json_value);
///         }
///         Err(error) => {
///             eprintln!("Error in record {}: {}", record_index + 1, error);
///         }
///     }
/// }
/// # Ok(())
/// # }
/// ```
pub struct RecordIterator<R: Read> {
    /// The buffered reader
    reader: BufReader<R>,
    /// The schema for decoding records
    schema: Schema,
    /// Decoding options
    options: DecodeOptions,
    /// Current record index (1-based)
    record_index: u64,
    /// Whether the iterator has reached EOF
    eof_reached: bool,
    /// Buffer for reading record data
    buffer: Vec<u8>,
}

impl<R: Read> RecordIterator<R> {
    /// Create a new record iterator
    ///
    /// # Arguments
    ///
    /// * `reader` - The input stream to read from
    /// * `schema` - The parsed copybook schema
    /// * `options` - Decoding options
    ///
    /// # Errors
    ///
    /// Returns an error if the record format is incompatible with the schema
    #[inline]
    #[must_use = "Handle iterator construction failures"]
    pub fn new(reader: R, schema: &Schema, options: &DecodeOptions) -> Result<Self> {
        Ok(Self {
            reader: BufReader::new(reader),
            schema: schema.clone(),
            options: options.clone(),
            record_index: 0,
            eof_reached: false,
            buffer: Vec::new(),
        })
    }

    /// Get the current record index (1-based)
    ///
    /// This returns the index of the last record that was successfully read,
    /// or 0 if no records have been read yet.
    #[inline]
    #[must_use]
    pub fn current_record_index(&self) -> u64 {
        self.record_index
    }

    /// Check if the iterator has reached the end of the file
    #[inline]
    #[must_use]
    pub fn is_eof(&self) -> bool {
        self.eof_reached
    }

    /// Get a reference to the schema being used
    #[inline]
    #[must_use]
    pub fn schema(&self) -> &Schema {
        &self.schema
    }

    /// Get a reference to the decode options being used
    #[inline]
    #[must_use]
    pub fn options(&self) -> &DecodeOptions {
        &self.options
    }

    /// Read the next record without decoding it
    ///
    /// This method reads the raw bytes of the next record without performing
    /// JSON decoding. Useful for applications that need access to raw record data.
    ///
    /// # Returns
    ///
    /// * `Ok(Some(bytes))` - The raw record bytes
    /// * `Ok(None)` - End of file reached
    /// * `Err(error)` - An error occurred while reading
    ///
    /// # Errors
    ///
    /// Returns an error if underlying I/O operations fail or the record format is invalid.
    #[inline]
    #[must_use = "Consume raw record reads to advance the iterator"]
    pub fn read_raw_record(&mut self) -> Result<Option<Vec<u8>>> {
        if self.eof_reached {
            return Ok(None);
        }

        self.buffer.clear();

        let record_data = match self.options.format {
            RecordFormat::Fixed => {
                let lrecl = self.schema.lrecl_fixed.ok_or_else(|| {
                    Error::new(ErrorCode::CBKI001_INVALID_STATE, FIXED_FORMAT_LRECL_MISSING)
                })? as usize;
                self.buffer.resize(lrecl, 0);

                match self.reader.read_exact(&mut self.buffer) {
                    Ok(()) => {
                        self.record_index += 1;
                        Some(self.buffer.clone())
                    }
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                        self.eof_reached = true;
                        return Ok(None);
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKD301_RECORD_TOO_SHORT,
                            format!("Failed to read fixed record: {e}"),
                        ));
                    }
                }
            }
            RecordFormat::RDW => {
                // Read RDW header
                let mut rdw_header = [0u8; 4];
                match self.reader.read_exact(&mut rdw_header) {
                    Ok(()) => {}
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                        self.eof_reached = true;
                        return Ok(None);
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKR221_RDW_UNDERFLOW,
                            format!("Failed to read RDW header: {e}"),
                        ));
                    }
                }

                // Parse length (payload bytes only)
                let length = u16::from_be_bytes([rdw_header[0], rdw_header[1]]) as usize;

                // Read payload
                self.buffer.resize(length, 0);
                match self.reader.read_exact(&mut self.buffer) {
                    Ok(()) => {
                        self.record_index += 1;
                        Some(self.buffer.clone())
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKR221_RDW_UNDERFLOW,
                            format!("Failed to read RDW payload: {e}"),
                        ));
                    }
                }
            }
        };

        Ok(record_data)
    }

    /// Decode the next record to JSON
    ///
    /// This is the main method used by the Iterator implementation.
    /// It reads and decodes the next record in one operation.
    #[inline]
    fn decode_next_record(&mut self) -> Result<Option<Value>> {
        match self.read_raw_record()? {
            Some(record_bytes) => {
                let json_value = crate::decode_record(&self.schema, &record_bytes, &self.options)?;
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
        if self.eof_reached {
            return None;
        }

        match self.decode_next_record() {
            Ok(Some(value)) => Some(Ok(value)),
            Ok(None) => {
                self.eof_reached = true;
                None
            }
            Err(error) => {
                // On error, we still advance the record index if we were able to read something
                Some(Err(error))
            }
        }
    }
}

/// Convenience function to create a record iterator from a file path
///
/// # Arguments
///
/// * `file_path` - Path to the data file
/// * `schema` - The parsed copybook schema  
/// * `options` - Decoding options
///
/// # Errors
///
/// Returns an error if the file cannot be opened or the iterator cannot be created
#[inline]
#[must_use = "Handle iterator creation failures"]
pub fn iter_records_from_file<P: AsRef<std::path::Path>>(
    file_path: P,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RecordIterator<std::fs::File>> {
    let file = std::fs::File::open(file_path)
        .map_err(|e| Error::new(ErrorCode::CBKF104_RDW_SUSPECT_ASCII, e.to_string()))?;

    RecordIterator::new(file, schema, options)
}

/// Convenience function to create a record iterator from any readable source
///
/// # Arguments
///
/// * `reader` - Any type implementing Read
/// * `schema` - The parsed copybook schema
/// * `options` - Decoding options
///
/// # Errors
///
/// Returns an error if the iterator cannot be created
#[inline]
#[must_use = "Handle iterator creation failures"]
pub fn iter_records<R: Read>(
    reader: R,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RecordIterator<R>> {
    RecordIterator::new(reader, schema, options)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use copybook_core::parse_copybook;
    use std::io::Cursor;

    #[test]
    fn test_record_iterator_basic() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text).unwrap();

        // Create test data: two 8-byte fixed records
        let test_data = b"001ALICE002BOB  ";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            ..DecodeOptions::default()
        };

        let iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Just test that the iterator can be created successfully
        assert_eq!(iterator.current_record_index(), 0);
        assert!(!iterator.is_eof());
    }

    #[test]
    fn test_record_iterator_rdw() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text).unwrap();

        // Create RDW test data:
        // Record 1: length=8, reserved=0, data="001ALICE"
        // Record 2: length=6, reserved=0, data="002BOB"
        let test_data = vec![
            0x00, 0x08, 0x00, 0x00, // RDW header: length=8, reserved=0
            b'0', b'0', b'1', b'A', b'L', b'I', b'C', b'E', // Record 1 data
            0x00, 0x06, 0x00, 0x00, // RDW header: length=6, reserved=0
            b'0', b'0', b'2', b'B', b'O', b'B', // Record 2 data
        ];

        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::RDW,
            ..DecodeOptions::default()
        };

        let iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Just test that the iterator can be created successfully
        assert_eq!(iterator.current_record_index(), 0);
        assert!(!iterator.is_eof());
    }

    #[test]
    fn test_raw_record_reading() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text).unwrap();

        let test_data = b"001ALICE";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            ..DecodeOptions::default()
        };

        let mut iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Read raw record
        let raw_record = iterator.read_raw_record().unwrap().unwrap();
        assert_eq!(raw_record, b"001ALICE");
        assert_eq!(iterator.current_record_index(), 1);

        // End of file
        assert!(iterator.read_raw_record().unwrap().is_none());
    }

    #[test]
    fn test_iterator_error_handling() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text).unwrap();

        // Create incomplete record (only 4 bytes instead of 8)
        let test_data = b"001A";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            ..DecodeOptions::default()
        };

        let mut iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Should yield EOF (Ok(None)) when encountering truncated fixed-length data
        assert!(iterator.next().is_none());
    }

    #[test]
    fn test_iterator_fixed_format_missing_lrecl_errors_on_next() {
        // A schema without a fixed record length
        let copybook_text = "01 SOME-GROUP. 05 SOME-FIELD PIC X(1).";
        let mut schema = parse_copybook(copybook_text).unwrap();
        schema.lrecl_fixed = None; // Ensure it's None

        let test_data = b"";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            ..DecodeOptions::default()
        };

        let mut iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        let first = iterator.next().unwrap();
        assert!(first.is_err());
        if let Err(e) = first {
            assert_eq!(e.code, ErrorCode::CBKI001_INVALID_STATE);
            assert_eq!(e.message, FIXED_FORMAT_LRECL_MISSING);
        }
    }
}
