// SPDX-License-Identifier: AGPL-3.0-or-later
//! Record iterator for streaming access to decoded records
//!
//! This module provides iterator-based access to records for programmatic processing,
//! allowing users to process records one at a time without loading entire files into memory.
//!
//! # Overview
//!
//! The iterator module implements streaming record processing with bounded memory usage.
//! It provides low-level iterator primitives for reading COBOL data files sequentially,
//! supporting both fixed-length and RDW (Record Descriptor Word) variable-length formats.
//!
//! Key capabilities:
//!
//! 1. **Streaming iteration** ([`RecordIterator`]) - Process records one at a time
//! 2. **Format flexibility** - Handle both fixed-length and RDW variable-length records
//! 3. **Raw access** ([`RecordIterator::read_raw_record`]) - Access undecoded record bytes
//! 4. **Convenience functions** ([`iter_records_from_file`], [`iter_records`]) - Simplified creation
//!
//! # Performance Characteristics
//!
//! The iterator uses buffered I/O and maintains bounded memory usage:
//! - **Memory**: One record buffer (typically <32 KiB per record)
//! - **Throughput**: Depends on decode complexity (DISPLAY vs COMP-3)
//! - **Latency**: Sequential I/O optimized with `BufReader`
//!
//! For high-throughput parallel processing, consider using [`crate::decode_file_to_jsonl`]
//! which provides parallel worker pools and streaming output.
//!
//! # Examples
//!
//! ## Basic Fixed-Length Record Iteration
//!
//! ```rust
//! use copybook_codec::{iter_records_from_file, DecodeOptions, Codepage, RecordFormat};
//! use copybook_core::parse_copybook;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Parse copybook schema
//! let copybook_text = r#"
//!     01 CUSTOMER-RECORD.
//!        05 CUSTOMER-ID    PIC 9(5).
//!        05 CUSTOMER-NAME  PIC X(20).
//!        05 BALANCE        PIC S9(7)V99 COMP-3.
//! "#;
//! let schema = parse_copybook(copybook_text)?;
//!
//! // Configure decoding options
//! let options = DecodeOptions::new()
//!     .with_codepage(Codepage::CP037)
//!     .with_format(RecordFormat::Fixed);
//!
//! // Create iterator from file
//! # #[cfg(not(test))]
//! let iterator = iter_records_from_file("customers.bin", &schema, &options)?;
//!
//! // Process records one at a time
//! # #[cfg(not(test))]
//! for (index, result) in iterator.enumerate() {
//!     match result {
//!         Ok(json_value) => {
//!             println!("Record {}: {}", index + 1, json_value);
//!         }
//!         Err(error) => {
//!             eprintln!("Error in record {}: {}", index + 1, error);
//!             break; // Stop on first error
//!         }
//!     }
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ## RDW Variable-Length Records
//!
//! ```rust
//! use copybook_codec::{RecordIterator, DecodeOptions, RecordFormat};
//! use copybook_core::parse_copybook;
//! use std::fs::File;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let copybook_text = r#"
//!     01 TRANSACTION.
//!        05 TRAN-ID       PIC 9(10).
//!        05 TRAN-AMOUNT   PIC S9(9)V99 COMP-3.
//!        05 TRAN-DESC     PIC X(100).
//! "#;
//! let schema = parse_copybook(copybook_text)?;
//!
//! let options = DecodeOptions::new()
//!     .with_format(RecordFormat::RDW);  // RDW variable-length format
//!
//! # #[cfg(not(test))]
//! let file = File::open("transactions.dat")?;
//! # #[cfg(test)]
//! # let file = std::io::Cursor::new(vec![]);
//! let mut iterator = RecordIterator::new(file, &schema, &options)?;
//!
//! // Process with error recovery
//! let mut processed = 0;
//! let mut errors = 0;
//!
//! for (index, result) in iterator.enumerate() {
//!     match result {
//!         Ok(json_value) => {
//!             processed += 1;
//!             // Process record...
//!         }
//!         Err(error) => {
//!             errors += 1;
//!             eprintln!("Record {}: {}", index + 1, error);
//!
//!             if errors > 10 {
//!                 eprintln!("Too many errors, stopping");
//!                 break;
//!             }
//!         }
//!     }
//! }
//!
//! println!("Processed: {}, Errors: {}", processed, errors);
//! # Ok(())
//! # }
//! ```
//!
//! ## Raw Record Access (No Decoding)
//!
//! ```rust
//! use copybook_codec::{RecordIterator, DecodeOptions, RecordFormat};
//! use copybook_core::parse_copybook;
//! use std::io::Cursor;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let copybook_text = "01 RECORD.\n   05 DATA PIC X(10).";
//! let schema = parse_copybook(copybook_text)?;
//!
//! let options = DecodeOptions::new()
//!     .with_format(RecordFormat::Fixed);
//!
//! let data = b"RECORD0001RECORD0002";
//! let mut iterator = RecordIterator::new(Cursor::new(data), &schema, &options)?;
//!
//! // Read raw bytes without JSON decoding
//! while let Some(raw_bytes) = iterator.read_raw_record()? {
//!     println!("Raw record {}: {} bytes",
//!              iterator.current_record_index(),
//!              raw_bytes.len());
//!
//!     // Process raw bytes directly...
//!     // (useful for binary analysis, checksums, etc.)
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ## Collecting Records into a Vec
//!
//! ```rust
//! use copybook_codec::{iter_records, DecodeOptions};
//! use copybook_core::parse_copybook;
//! use serde_json::Value;
//! use std::io::Cursor;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let copybook_text = "01 RECORD.\n   05 ID PIC 9(5).";
//! let schema = parse_copybook(copybook_text)?;
//! let options = DecodeOptions::default();
//!
//! let data = b"0000100002";
//! let iterator = iter_records(Cursor::new(data), &schema, &options)?;
//!
//! // Collect all successful records
//! let records: Vec<Value> = iterator
//!     .filter_map(Result::ok)  // Skip errors
//!     .collect();
//!
//! println!("Collected {} records", records.len());
//! # Ok(())
//! # }
//! ```
//!
//! ## Using with `DecodeOptions` and Metadata
//!
//! ```rust
//! use copybook_codec::{iter_records_from_file, DecodeOptions, Codepage, JsonNumberMode};
//! use copybook_core::parse_copybook;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let copybook_text = r#"
//!     01 RECORD.
//!        05 AMOUNT PIC S9(9)V99 COMP-3.
//! "#;
//! let schema = parse_copybook(copybook_text)?;
//!
//! // Configure with lossless numbers and metadata
//! let options = DecodeOptions::new()
//!     .with_codepage(Codepage::CP037)
//!     .with_json_number_mode(JsonNumberMode::Lossless)
//!     .with_emit_meta(true);  // Include field metadata
//!
//! # #[cfg(not(test))]
//! let iterator = iter_records_from_file("data.bin", &schema, &options)?;
//!
//! # #[cfg(not(test))]
//! for result in iterator {
//!     let json_value = result?;
//!     // JSON includes metadata: {"AMOUNT": "123.45", "_meta": {...}}
//!     println!("{}", serde_json::to_string_pretty(&json_value)?);
//! }
//! # Ok(())
//! # }
//! ```

use crate::lib_api::decode_record_with_raw_data;
use crate::options::{DecodeOptions, RecordFormat};
use copybook_core::{Error, ErrorCode, ErrorContext, Result, Schema};
use copybook_rdw::RdwHeader;
use serde_json::Value;
use std::io::{BufReader, Read};

/// Outcome of reading a whole record-sized unit at a record boundary.
enum BoundaryRead {
    /// The buffer was filled.
    Complete,
    /// The reader was already exhausted; nothing was consumed.
    CleanEof,
    /// The reader ran out mid-unit after this many bytes.
    Partial(usize),
}

/// Fill `buffer` from `reader`, distinguishing a clean end of file from a
/// truncated final unit.
///
/// [`Read::read_exact`] reports [`std::io::ErrorKind::UnexpectedEof`] for both
/// cases and consumes the bytes it did read, so it cannot tell "the file ended
/// on a record boundary" from "the file ends with a partial record". Treating
/// the two alike silently discards the trailing bytes.
fn fill_at_record_boundary<R: Read>(
    reader: &mut R,
    buffer: &mut [u8],
) -> std::io::Result<BoundaryRead> {
    let mut filled = 0;
    while filled < buffer.len() {
        match reader.read(&mut buffer[filled..]) {
            Ok(0) => break,
            Ok(read) => filled += read,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {}
            Err(e) => return Err(e),
        }
    }

    if filled == buffer.len() {
        Ok(BoundaryRead::Complete)
    } else if filled == 0 {
        Ok(BoundaryRead::CleanEof)
    } else {
        Ok(BoundaryRead::Partial(filled))
    }
}

/// Iterator over records in a data file, yielding decoded JSON values
///
/// This iterator provides streaming access to records, processing them one at a time
/// to maintain bounded memory usage even for very large files.
///
/// # Examples
///
/// ```rust,no_run
/// use copybook_codec::{RecordIterator, DecodeOptions};
/// use copybook_core::parse_copybook;
/// # use std::io::Cursor;
///
/// # fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let copybook_text = "01 RECORD.\n   05 ID PIC 9(5).\n   05 NAME PIC X(20).";
/// let mut schema = parse_copybook(copybook_text)?;
/// schema.lrecl_fixed = Some(25);
/// let options = DecodeOptions::default();
/// # let record_bytes = b"00001ALICE               ";
/// # let file = Cursor::new(&record_bytes[..]);
/// // let file = std::fs::File::open("data.bin")?;
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
    /// Complete RDW frame for `RawMode::RecordRDW` envelope capture.
    raw_data_with_header: Option<Vec<u8>>,
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
    /// Returns an error if the record format is incompatible with the schema.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn new(reader: R, schema: &Schema, options: &DecodeOptions) -> Result<Self> {
        Ok(Self {
            reader: BufReader::new(reader),
            schema: schema.clone(),
            options: options.clone(),
            record_index: 0,
            eof_reached: false,
            buffer: Vec::new(),
            raw_data_with_header: None,
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
    /// JSON decoding. Useful for applications that need access to raw record data
    /// for binary analysis, checksums, or custom processing.
    ///
    /// # Returns
    ///
    /// * `Ok(Some(bytes))` - The raw record bytes
    /// * `Ok(None)` - End of file reached
    /// * `Err(error)` - An error occurred while reading
    ///
    /// # Errors
    /// Returns an error if underlying I/O operations fail or the record format is invalid.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::{RecordIterator, DecodeOptions, RecordFormat};
    /// use copybook_core::parse_copybook;
    /// use std::io::Cursor;
    ///
    /// # fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// let copybook_text = "01 RECORD.\n   05 DATA PIC X(8).";
    /// let schema = parse_copybook(copybook_text)?;
    ///
    /// let options = DecodeOptions::new()
    ///     .with_format(RecordFormat::Fixed);
    ///
    /// let data = b"RECORD01RECORD02";
    /// let mut iterator = RecordIterator::new(Cursor::new(data), &schema, &options)?;
    ///
    /// // Read raw bytes
    /// if let Some(raw_bytes) = iterator.read_raw_record()? {
    ///     assert_eq!(raw_bytes, b"RECORD01");
    ///     assert_eq!(iterator.current_record_index(), 1);
    /// }
    ///
    /// if let Some(raw_bytes) = iterator.read_raw_record()? {
    ///     assert_eq!(raw_bytes, b"RECORD02");
    ///     assert_eq!(iterator.current_record_index(), 2);
    /// }
    ///
    /// // End of file
    /// assert!(iterator.read_raw_record()?.is_none());
    /// assert!(iterator.is_eof());
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn read_raw_record(&mut self) -> Result<Option<Vec<u8>>> {
        if self.eof_reached {
            return Ok(None);
        }

        self.buffer.clear();
        self.raw_data_with_header = None;

        let record_data = match self.options.format {
            RecordFormat::Fixed => {
                let lrecl = crate::file::fixed::lrecl(&self.schema)? as usize;
                self.buffer.resize(lrecl, 0);

                match fill_at_record_boundary(&mut self.reader, &mut self.buffer) {
                    Ok(BoundaryRead::Complete) => {
                        self.record_index += 1;
                        Some(self.buffer.clone())
                    }
                    Ok(BoundaryRead::CleanEof) => {
                        self.eof_reached = true;
                        return Ok(None);
                    }
                    Ok(BoundaryRead::Partial(read)) => {
                        self.eof_reached = true;
                        return Err(Error::new(
                            ErrorCode::CBKR101_FIXED_RECORD_ERROR,
                            format!("Incomplete record at end of file: expected {lrecl} bytes"),
                        )
                        .with_context(ErrorContext {
                            record_index: Some(self.record_index + 1),
                            field_path: None,
                            byte_offset: None,
                            line_number: None,
                            details: Some(format!(
                                "File ends with partial record ({read} of {lrecl} bytes)"
                            )),
                        }));
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKR101_FIXED_RECORD_ERROR,
                            format!("Failed to read fixed record: {e}"),
                        ));
                    }
                }
            }
            RecordFormat::RDW => {
                // Read RDW header
                let mut rdw_header = [0u8; 4];
                match fill_at_record_boundary(&mut self.reader, &mut rdw_header) {
                    Ok(BoundaryRead::Complete) => {}
                    Ok(BoundaryRead::CleanEof) => {
                        self.eof_reached = true;
                        return Ok(None);
                    }
                    Ok(BoundaryRead::Partial(read)) => {
                        self.eof_reached = true;
                        return Err(Error::new(
                            ErrorCode::CBKF221_RDW_UNDERFLOW,
                            "Incomplete RDW header at end of file: expected 4 bytes".to_string(),
                        )
                        .with_context(ErrorContext {
                            record_index: Some(self.record_index + 1),
                            field_path: None,
                            byte_offset: None,
                            line_number: None,
                            details: Some(format!(
                                "File ends with partial RDW header ({read} of 4 bytes)"
                            )),
                        }));
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKF221_RDW_UNDERFLOW,
                            format!("Failed to read RDW header: {e}"),
                        ));
                    }
                }

                // Parse length (payload bytes only)
                let length = usize::from(RdwHeader::from_bytes(rdw_header).length());

                // Read payload
                self.buffer.resize(length, 0);
                match self.reader.read_exact(&mut self.buffer) {
                    Ok(()) => {
                        let mut raw_data_with_header = Vec::with_capacity(4 + length);
                        raw_data_with_header.extend_from_slice(&rdw_header);
                        raw_data_with_header.extend_from_slice(&self.buffer);
                        self.raw_data_with_header = Some(raw_data_with_header);
                        self.record_index += 1;
                        Some(self.buffer.clone())
                    }
                    Err(e) => {
                        return Err(Error::new(
                            ErrorCode::CBKF221_RDW_UNDERFLOW,
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
                let raw_data_with_header = self.raw_data_with_header.take();
                let json_value = decode_record_with_raw_data(
                    &self.schema,
                    &record_bytes,
                    &self.options,
                    raw_data_with_header.as_deref(),
                    self.record_index,
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
/// This is the most common way to create an iterator for processing COBOL data files.
/// It handles file opening and iterator creation in a single call.
///
/// # Arguments
///
/// * `file_path` - Path to the data file
/// * `schema` - The parsed copybook schema
/// * `options` - Decoding options
///
/// # Errors
/// Returns an error if the file cannot be opened or the iterator cannot be created.
///
/// # Examples
///
/// ## Basic Usage with Fixed-Length Records
///
/// ```rust,no_run
/// use copybook_codec::{iter_records_from_file, DecodeOptions, Codepage, RecordFormat};
/// use copybook_core::parse_copybook;
///
/// # fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let copybook_text = r#"
///     01 EMPLOYEE-RECORD.
///        05 EMP-ID        PIC 9(6).
///        05 EMP-NAME      PIC X(30).
///        05 EMP-SALARY    PIC S9(7)V99 COMP-3.
/// "#;
/// let schema = parse_copybook(copybook_text)?;
///
/// let options = DecodeOptions::new()
///     .with_codepage(Codepage::CP037)
///     .with_format(RecordFormat::Fixed);
///
/// let iterator = iter_records_from_file("employees.dat", &schema, &options)?;
///
/// for (index, result) in iterator.enumerate() {
///     match result {
///         Ok(employee) => println!("Employee {}: {}", index + 1, employee),
///         Err(e) => eprintln!("Error at record {}: {}", index + 1, e),
///     }
/// }
/// # Ok(())
/// # }
/// ```
///
/// ## Processing with Error Limits
///
/// ```rust,no_run
/// use copybook_codec::{iter_records_from_file, DecodeOptions};
/// use copybook_core::parse_copybook;
///
/// # fn example() -> Result<(), Box<dyn std::error::Error>> {
/// # let schema = parse_copybook("01 R.\n   05 F PIC X(1).")?;
/// # let options = DecodeOptions::default();
/// let iterator = iter_records_from_file("data.bin", &schema, &options)?;
///
/// let mut success_count = 0;
/// let mut error_count = 0;
/// const MAX_ERRORS: usize = 100;
///
/// for result in iterator {
///     match result {
///         Ok(_) => success_count += 1,
///         Err(e) => {
///             error_count += 1;
///             eprintln!("Error: {}", e);
///
///             if error_count >= MAX_ERRORS {
///                 eprintln!("Too many errors, aborting");
///                 break;
///             }
///         }
///     }
/// }
///
/// println!("Success: {}, Errors: {}", success_count, error_count);
/// # Ok(())
/// # }
/// ```
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn iter_records_from_file<P: AsRef<std::path::Path>>(
    file_path: P,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RecordIterator<std::fs::File>> {
    let file = std::fs::File::open(file_path).map_err(|e| {
        Error::new(
            ErrorCode::CBKR201_RDW_READ_ERROR,
            format!("failed to open input file: {e}"),
        )
    })?;

    RecordIterator::new(file, schema, options)
}

/// Convenience function to create a record iterator from any readable source
///
/// This function provides maximum flexibility by accepting any type that implements
/// the `Read` trait, including files, cursors, network streams, or custom readers.
///
/// # Arguments
///
/// * `reader` - Any type implementing Read (File, Cursor, `TcpStream`, etc.)
/// * `schema` - The parsed copybook schema
/// * `options` - Decoding options
///
/// # Errors
/// Returns an error if the iterator cannot be created.
///
/// # Examples
///
/// ## Using with In-Memory Data (Cursor)
///
/// ```rust
/// use copybook_codec::{iter_records, DecodeOptions, RecordFormat};
/// use copybook_core::parse_copybook;
/// use std::io::Cursor;
///
/// # fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let copybook_text = "01 RECORD.\n   05 ID PIC 9(3).\n   05 NAME PIC X(5).";
/// let schema = parse_copybook(copybook_text)?;
///
/// let options = DecodeOptions::new()
///     .with_format(RecordFormat::Fixed);
///
/// // Create iterator from in-memory data
/// let data = b"001ALICE002BOB  003CAROL";
/// let iterator = iter_records(Cursor::new(data), &schema, &options)?;
///
/// let records: Vec<_> = iterator.collect::<Result<Vec<_>, _>>()?;
/// assert_eq!(records.len(), 3);
/// # Ok(())
/// # }
/// ```
///
/// ## Using with File
///
/// ```rust,no_run
/// use copybook_codec::{iter_records, DecodeOptions};
/// use copybook_core::parse_copybook;
/// use std::fs::File;
///
/// # fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let schema = parse_copybook("01 RECORD.\n   05 DATA PIC X(10).")?;
/// let options = DecodeOptions::default();
///
/// let file = File::open("data.bin")?;
/// let iterator = iter_records(file, &schema, &options)?;
///
/// for result in iterator {
///     let record = result?;
///     println!("{}", record);
/// }
/// # Ok(())
/// # }
/// ```
///
/// ## Using with Compressed Data
///
/// ```text
/// use copybook_codec::{iter_records, DecodeOptions};
/// use copybook_core::parse_copybook;
/// use std::fs::File;
/// use flate2::read::GzDecoder;
///
/// # fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let schema = parse_copybook("01 RECORD.\n   05 DATA PIC X(10).")?;
/// let options = DecodeOptions::default();
///
/// // Read from gzipped file
/// let file = File::open("data.bin.gz")?;
/// let decoder = GzDecoder::new(file);
/// let iterator = iter_records(decoder, &schema, &options)?;
///
/// for result in iterator {
///     let record = result?;
///     // Process decompressed record...
/// }
/// # Ok(())
/// # }
/// ```
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn iter_records<R: Read>(
    reader: R,
    schema: &Schema,
    options: &DecodeOptions,
) -> Result<RecordIterator<R>> {
    RecordIterator::new(reader, schema, options)
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use crate::Codepage;
    use copybook_core::parse_copybook;
    use std::io::{self, Cursor, Read};

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

        // A truncated final record is reported, not silently dropped: returning
        // EOF here would discard the four trailing bytes with no signal to the
        // caller.
        let error = iterator
            .next()
            .expect("truncated data yields an item")
            .expect_err("truncated data is an error");
        assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);
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
            assert_eq!(e.message, crate::file::fixed::FIXED_FORMAT_LRECL_MISSING);
        }
    }

    #[test]
    fn test_iterator_fixed_format_zero_lrecl_errors_on_next() {
        let copybook_text = "01 SOME-GROUP. 05 SOME-FIELD PIC X(1).";
        let mut schema = parse_copybook(copybook_text).unwrap();
        schema.lrecl_fixed = Some(0);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            ..DecodeOptions::default()
        };

        let mut iterator = RecordIterator::new(Cursor::new(b"DATA"), &schema, &options).unwrap();

        let error = iterator.next().unwrap().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
        assert_eq!(error.message, "LRECL must be greater than zero");
    }

    #[test]
    fn test_iterator_schema_and_options_accessors() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let mut schema = parse_copybook(copybook_text).unwrap();
        schema.lrecl_fixed = Some(8);
        let test_data = b"001ALICE";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::ASCII,
            ..DecodeOptions::default()
        };

        let iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Test schema accessor
        assert_eq!(iterator.schema().fields[0].name, "RECORD");

        // Test options accessor
        assert_eq!(iterator.options().format, RecordFormat::Fixed);
    }

    #[test]
    fn test_iterator_multiple_fixed_records() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let mut schema = parse_copybook(copybook_text).unwrap();
        schema.lrecl_fixed = Some(8);

        // Create test data: three 8-byte fixed records
        let test_data = b"001ALICE002BOB  003CAROL";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::ASCII,
            ..DecodeOptions::default()
        };

        let mut iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Read all records
        let mut count = 0;
        for result in iterator.by_ref() {
            assert!(result.is_ok(), "Record {count} should decode successfully");
            count += 1;
        }

        assert_eq!(count, 3);
        assert_eq!(iterator.current_record_index(), 3);
        assert!(iterator.is_eof());
    }

    #[test]
    fn test_iterator_rdw_multiple_records() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text).unwrap();

        // Create RDW test data with three records
        let test_data = vec![
            // Record 1
            0x00, 0x08, 0x00, 0x00, // RDW header: length=8
            b'0', b'0', b'1', b'A', b'L', b'I', b'C', b'E', // Record 2
            0x00, 0x06, 0x00, 0x00, // RDW header: length=6
            b'0', b'0', b'2', b'B', b'O', b'B', // Record 3
            0x00, 0x08, 0x00, 0x00, // RDW header: length=8
            b'0', b'0', b'3', b'C', b'A', b'R', b'O', b'L',
        ];

        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::RDW,
            codepage: Codepage::ASCII,
            ..DecodeOptions::default()
        };

        let mut iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Read all records
        let mut count = 0;
        for result in iterator.by_ref() {
            assert!(result.is_ok(), "Record {count} should decode successfully");
            count += 1;
        }

        assert_eq!(count, 3);
        assert_eq!(iterator.current_record_index(), 3);
        assert!(iterator.is_eof());
    }

    #[test]
    fn test_iter_records_convenience() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text).unwrap();

        let test_data = b"001ALICE002BOB  ";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            ..DecodeOptions::default()
        };

        let iterator = iter_records(cursor, &schema, &options).unwrap();

        assert_eq!(iterator.current_record_index(), 0);
        assert!(!iterator.is_eof());
    }

    #[test]
    fn test_iterator_with_empty_data() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let mut schema = parse_copybook(copybook_text).unwrap();
        schema.lrecl_fixed = Some(8);

        let test_data = b"";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            ..DecodeOptions::default()
        };

        let mut iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Should immediately return None for empty data
        assert!(iterator.next().is_none());
        assert!(iterator.is_eof());
        assert_eq!(iterator.current_record_index(), 0);
    }

    #[test]
    fn test_iterator_raw_record_eof() {
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

        // Read first record
        assert!(iterator.read_raw_record().unwrap().is_some());
        assert_eq!(iterator.current_record_index(), 1);

        // Read second record (should be None)
        assert!(iterator.read_raw_record().unwrap().is_none());
        assert!(iterator.is_eof());
    }

    #[test]
    fn test_iterator_collect_results() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let mut schema = parse_copybook(copybook_text).unwrap();
        schema.lrecl_fixed = Some(8);

        let test_data = b"001ALICE002BOB  003CAROL";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::ASCII,
            ..DecodeOptions::default()
        };

        let iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // Collect all results
        let results: Vec<Result<Value>> = iterator.collect();

        assert_eq!(results.len(), 3);
        for result in results {
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_iterator_with_decode_error() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let mut schema = parse_copybook(copybook_text).unwrap();
        schema.lrecl_fixed = Some(8);

        // Create data that will decode successfully for first record
        let test_data = b"001ALICE";
        let cursor = Cursor::new(test_data);

        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::ASCII,
            ..DecodeOptions::default()
        };

        let mut iterator = RecordIterator::new(cursor, &schema, &options).unwrap();

        // First record should decode successfully
        let first = iterator.next();
        assert!(first.is_some());
        assert!(first.unwrap().is_ok());

        // Second call should return None (EOF)
        assert!(iterator.next().is_none());
    }

    #[derive(Default)]
    struct FailingReader {
        fail: bool,
    }

    impl Read for FailingReader {
        fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
            if self.fail {
                Ok(0)
            } else {
                self.fail = true;
                Err(io::Error::other("forced read error"))
            }
        }
    }

    #[test]
    fn test_iterator_fixed_format_read_error_code() {
        let copybook_text = r"
            01 RECORD.
               05 ID PIC 9(3).
               05 NAME PIC X(5).
        ";

        let schema = parse_copybook(copybook_text).unwrap();

        let mut schema = schema;
        schema.lrecl_fixed = Some(8);

        let mut iterator =
            RecordIterator::new(FailingReader::default(), &schema, &DecodeOptions::default())
                .unwrap();

        let error = iterator.read_raw_record().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);
    }

    /// Reader that hands out one byte per call, so `read_exact` cannot fill the
    /// buffer in a single call and the boundary logic is exercised for real.
    struct DribbleReader {
        data: Vec<u8>,
        position: usize,
    }

    impl Read for DribbleReader {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            if self.position >= self.data.len() || buf.is_empty() {
                return Ok(0);
            }
            buf[0] = self.data[self.position];
            self.position += 1;
            Ok(1)
        }
    }

    fn eight_byte_schema() -> Schema {
        let mut schema = parse_copybook("01 RECORD.\n   05 NAME PIC X(8).").unwrap();
        schema.lrecl_fixed = Some(8);
        schema
    }

    #[test]
    fn fixed_trailing_partial_record_is_reported_not_discarded() {
        // Twelve bytes for an 8-byte LRECL: one whole record and a 4-byte tail.
        let data = b"RECORD01HALF".to_vec();
        let schema = eight_byte_schema();
        let options = DecodeOptions::default().with_codepage(Codepage::ASCII);
        let mut iterator = RecordIterator::new(Cursor::new(data), &schema, &options).unwrap();

        assert_eq!(iterator.read_raw_record().unwrap().unwrap(), b"RECORD01");

        let error = iterator.read_raw_record().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);
        let context = error.context.expect("partial record reports context");
        assert_eq!(context.record_index, Some(2));
        assert!(
            context.details.unwrap_or_default().contains("4 of 8 bytes"),
            "details should name the short count"
        );
    }

    #[test]
    fn fixed_partial_record_terminates_iteration_after_one_error() {
        let schema = eight_byte_schema();
        let options = DecodeOptions::default().with_codepage(Codepage::ASCII);
        let mut iterator =
            RecordIterator::new(Cursor::new(b"RECORD01HALF".to_vec()), &schema, &options).unwrap();

        assert!(iterator.next().unwrap().is_ok());
        assert!(iterator.next().unwrap().is_err());
        assert!(
            iterator.next().is_none(),
            "iteration must stop after the partial-record error"
        );
    }

    #[test]
    fn fixed_file_ending_on_a_record_boundary_is_still_clean_eof() {
        let schema = eight_byte_schema();
        let options = DecodeOptions::default().with_codepage(Codepage::ASCII);
        let mut iterator =
            RecordIterator::new(Cursor::new(b"RECORD01".to_vec()), &schema, &options).unwrap();

        assert_eq!(iterator.read_raw_record().unwrap().unwrap(), b"RECORD01");
        assert!(iterator.read_raw_record().unwrap().is_none());
        assert!(iterator.is_eof());
    }

    #[test]
    fn fixed_partial_record_detected_when_reads_return_one_byte_at_a_time() {
        // A short `read` is not end of file. The boundary helper must keep
        // reading until the buffer is full or the reader is genuinely dry.
        let schema = eight_byte_schema();
        let options = DecodeOptions::default().with_codepage(Codepage::ASCII);
        let reader = DribbleReader {
            data: b"RECORD01HALF".to_vec(),
            position: 0,
        };
        let mut iterator = RecordIterator::new(reader, &schema, &options).unwrap();

        assert_eq!(iterator.read_raw_record().unwrap().unwrap(), b"RECORD01");
        assert_eq!(
            iterator.read_raw_record().unwrap_err().code,
            ErrorCode::CBKR101_FIXED_RECORD_ERROR
        );
    }

    #[test]
    fn rdw_trailing_partial_header_is_reported_not_discarded() {
        // One 8-byte RDW record followed by a 2-byte stub of a second header.
        let mut data = vec![0x00, 0x08, 0x00, 0x00];
        data.extend_from_slice(b"RECORD01");
        data.extend_from_slice(&[0x00, 0x08]);

        let schema = eight_byte_schema();
        let options = DecodeOptions::default()
            .with_format(RecordFormat::RDW)
            .with_codepage(Codepage::ASCII);
        let mut iterator = RecordIterator::new(Cursor::new(data), &schema, &options).unwrap();

        assert_eq!(iterator.read_raw_record().unwrap().unwrap(), b"RECORD01");

        let error = iterator.read_raw_record().unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    }

    #[test]
    fn rdw_file_ending_on_a_record_boundary_is_still_clean_eof() {
        let mut data = vec![0x00, 0x08, 0x00, 0x00];
        data.extend_from_slice(b"RECORD01");

        let schema = eight_byte_schema();
        let options = DecodeOptions::default()
            .with_format(RecordFormat::RDW)
            .with_codepage(Codepage::ASCII);
        let mut iterator = RecordIterator::new(Cursor::new(data), &schema, &options).unwrap();

        assert_eq!(iterator.read_raw_record().unwrap().unwrap(), b"RECORD01");
        assert!(iterator.read_raw_record().unwrap().is_none());
        assert!(iterator.is_eof());
    }
}
