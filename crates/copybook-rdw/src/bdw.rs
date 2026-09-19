// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_error::{Error, ErrorCode, ErrorContext, Result};
use std::io::{BufReader, Read, Write};
use tracing::{debug, warn};

/// Size of a BDW (Block Descriptor Word) header in bytes.
pub const BDW_HEADER_LEN: usize = 4;

/// Maximum VB block length in bytes, header included.
pub const BDW_MAX_BLOCK_LEN: usize = 32760;

/// Maximum RDW length inside a VB block, header included.
pub const VB_MAX_RECORD_LEN: usize = BDW_MAX_BLOCK_LEN - BDW_HEADER_LEN;

/// Parsed BDW header (`block length + reserved`).
///
/// A 4-byte Block Descriptor Word containing a 2-byte big-endian block
/// length that **includes** these 4 header bytes, plus 2 reserved bytes
/// that must be zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BdwHeader {
    bytes: [u8; BDW_HEADER_LEN],
}

impl BdwHeader {
    /// Construct from raw 4-byte header bytes.
    #[must_use]
    #[inline]
    pub const fn from_bytes(bytes: [u8; BDW_HEADER_LEN]) -> Self {
        Self { bytes }
    }

    /// Construct from a block length, validating the 0.7 wire bounds.
    ///
    /// # Errors
    /// Returns `CBKF222_BDW_LENGTH_INVALID` when `block_len` is below the
    /// header size or above the 32760-byte maximum.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn from_block_len(block_len: usize) -> Result<Self> {
        if !(BDW_HEADER_LEN..=BDW_MAX_BLOCK_LEN).contains(&block_len) {
            return Err(Error::new(
                ErrorCode::CBKF222_BDW_LENGTH_INVALID,
                format!(
                    "BDW block length out of bounds: {block_len} bytes (valid {BDW_HEADER_LEN}..={BDW_MAX_BLOCK_LEN})"
                ),
            ));
        }
        let len = u16::try_from(block_len).map_err(|_| {
            Error::new(
                ErrorCode::CBKF222_BDW_LENGTH_INVALID,
                format!("BDW block length out of bounds: {block_len} bytes"),
            )
        })?;
        let len_bytes = len.to_be_bytes();
        Ok(Self {
            bytes: [len_bytes[0], len_bytes[1], 0, 0],
        })
    }

    /// Return raw bytes.
    #[must_use]
    #[inline]
    pub const fn bytes(self) -> [u8; BDW_HEADER_LEN] {
        self.bytes
    }

    /// Extract block length, BDW header included.
    #[must_use]
    #[inline]
    pub const fn length(self) -> u16 {
        u16::from_be_bytes([self.bytes[0], self.bytes[1]])
    }

    /// Extract reserved bytes.
    #[must_use]
    #[inline]
    pub const fn reserved(self) -> u16 {
        u16::from_be_bytes([self.bytes[2], self.bytes[3]])
    }
}

/// One record framed inside a VB block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VbRecord {
    /// Record payload with framing headers removed.
    pub payload: Vec<u8>,
    /// Original 4-byte RDW header (length includes the header).
    pub rdw: [u8; 4],
    /// Reserved bytes of the record RDW header.
    pub rdw_reserved: u16,
    /// Zero-based index of the containing block.
    pub block_index: u64,
    /// Zero-based index of the record inside its block.
    pub record_index_in_block: u32,
    /// Physical byte offset of the containing BDW.
    pub block_offset: u64,
    /// Physical byte offset of the record RDW.
    pub physical_offset: u64,
    /// Declared block length, BDW header included.
    pub block_len: usize,
}

/// Bounded VB block reader yielding framed records.
///
/// Lengths are validated before any allocation or extraction. An RDW length
/// inside a block includes its own 4-byte header (mainframe LL convention),
/// unlike the bare-RDW payload-length convention.
#[derive(Debug)]
pub struct VbBlockReader<R: Read> {
    input: BufReader<R>,
    strict_mode: bool,
    max_record_length: Option<u64>,
    block_index: u64,
    record_count: u64,
    physical_offset: u64,
    block_remaining: usize,
    block_offset: u64,
    block_len: usize,
    block_record_index: u32,
    in_block: bool,
}

impl<R: Read> VbBlockReader<R> {
    /// Create a new VB block reader.
    #[inline]
    #[must_use]
    pub fn new(input: R, strict_mode: bool) -> Self {
        Self {
            input: BufReader::with_capacity(BDW_MAX_BLOCK_LEN, input),
            strict_mode,
            max_record_length: None,
            block_index: 0,
            record_count: 0,
            physical_offset: 0,
            block_remaining: 0,
            block_offset: 0,
            block_len: 0,
            block_record_index: 0,
            in_block: false,
        }
    }

    /// Cap the nested logical record payload bytes accepted per record.
    ///
    /// A nested payload longer than the cap fails with
    /// `CBKF226_RECORD_BOUND_EXCEEDED` before any payload allocation or
    /// read. Block bounds still apply independently: one valid block may
    /// hold many individually valid records. `None` (the default) keeps
    /// architectural behavior.
    #[inline]
    #[must_use]
    pub fn with_max_record_length(mut self, cap: Option<u64>) -> Self {
        self.max_record_length = cap;
        self
    }

    /// Read the next framed record, or `None` on clean EOF.
    ///
    /// # Errors
    /// Returns `CBKF222`/`CBKF223`/`CBKF224`/`CBKF225` framing failures with
    /// block index, physical offsets, and claimed versus available lengths.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn read_record(&mut self) -> Result<Option<VbRecord>> {
        loop {
            if !self.in_block {
                if !self.open_block()? {
                    return Ok(None);
                }
                if self.block_remaining == 0 {
                    // Valid empty block: close it and advance.
                    self.in_block = false;
                    self.block_index += 1;
                    continue;
                }
            }
            return self.read_block_record().map(Some);
        }
    }

    /// Open the next block; `Ok(false)` means clean EOF before any header byte.
    ///
    /// The first byte is read alone so a short `fill_buf` result from a
    /// streaming source is never mistaken for EOF: only zero bytes means
    /// clean EOF, 1-3 trailing bytes are truncated input.
    fn open_block(&mut self) -> Result<bool> {
        let mut first = [0u8; 1];
        let read = self.input.read(&mut first).map_err(|error| {
            Error::new(
                ErrorCode::CBKR201_RDW_READ_ERROR,
                format!("I/O error probing VB input: {error}"),
            )
        })?;
        if read == 0 {
            debug!("Reached EOF after {} VB blocks", self.block_index);
            return Ok(false);
        }
        let mut header = [0u8; BDW_HEADER_LEN];
        header[0] = first[0];
        let mut have = 1usize;
        while have < BDW_HEADER_LEN {
            match self.input.read(&mut header[have..]) {
                Ok(0) => break,
                Ok(read_now) => have += read_now,
                Err(error) => {
                    return Err(Error::new(
                        ErrorCode::CBKR201_RDW_READ_ERROR,
                        format!("I/O error reading BDW header: {error}"),
                    )
                    .with_context(self.block_context("Unable to read BDW header")));
                }
            }
        }
        if have < BDW_HEADER_LEN {
            return self.short_block_header(have);
        }
        let parsed = BdwHeader::from_bytes(header);
        let block_len = usize::from(parsed.length());
        if !(BDW_HEADER_LEN..=BDW_MAX_BLOCK_LEN).contains(&block_len) {
            return Err(Error::new(
                ErrorCode::CBKF222_BDW_LENGTH_INVALID,
                format!(
                    "BDW block length out of bounds: {block_len} bytes (valid {BDW_HEADER_LEN}..={BDW_MAX_BLOCK_LEN})"
                ),
            )
            .with_context(self.block_context("Invalid BDW block length")));
        }
        self.check_bdw_reserved(parsed.reserved())?;
        self.block_offset = self.physical_offset;
        self.physical_offset += BDW_HEADER_LEN as u64;
        self.block_len = block_len;
        self.block_remaining = block_len - BDW_HEADER_LEN;
        self.block_record_index = 0;
        self.in_block = true;
        debug!("Opened VB block {}: length={block_len}", self.block_index);
        Ok(true)
    }

    /// Fewer than 4 bytes remain: clean stop under lenient, fatal under strict.
    fn short_block_header(&mut self, available: usize) -> Result<bool> {
        if self.strict_mode {
            return Err(Error::new(
                ErrorCode::CBKF223_BDW_UNDERFLOW,
                format!("Truncated BDW header: {available} trailing bytes cannot form a block"),
            )
            .with_context(self.block_context("Trailing bytes after final VB block")));
        }
        warn!("Ignoring {available} trailing bytes after final VB block");
        Ok(false)
    }

    fn check_bdw_reserved(&self, reserved: u16) -> Result<()> {
        if reserved == 0 {
            return Ok(());
        }
        let error = Error::new(
            ErrorCode::CBKF225_BDW_RESERVED_NONZERO,
            format!("BDW reserved bytes are non-zero: {reserved:04X}"),
        )
        .with_context(ErrorContext {
            record_index: None,
            field_path: None,
            byte_offset: Some(self.physical_offset + 2),
            line_number: None,
            details: Some(format!("Expected 0000, got {reserved:04X}")),
        });
        if self.strict_mode {
            return Err(error);
        }
        warn!(
            "BDW reserved bytes non-zero (block {}): {:04X}",
            self.block_index, reserved
        );
        Ok(())
    }

    fn read_block_record(&mut self) -> Result<VbRecord> {
        let rdw_offset = self.physical_offset;
        // A declared block that ends with 1-3 stray bytes holds no RDW
        // header. Reading here would consume the next block's BDW, so
        // reject before touching the stream.
        if self.block_remaining < 4 {
            return Err(Error::new(
                ErrorCode::CBKF223_BDW_UNDERFLOW,
                format!(
                    "VB block {} ends with {} stray bytes; cannot form an RDW header",
                    self.block_index, self.block_remaining
                ),
            )
            .with_context(self.record_context("Block ends mid-RDW header")));
        }
        let mut rdw = [0u8; 4];
        self.input.read_exact(&mut rdw).map_err(|_| {
            Error::new(
                ErrorCode::CBKF223_BDW_UNDERFLOW,
                format!(
                    "Truncated RDW inside VB block {}: {} block bytes remain",
                    self.block_index, self.block_remaining
                ),
            )
            .with_context(self.record_context("Block ends mid-RDW header"))
        })?;
        let record_len = usize::from(u16::from_be_bytes([rdw[0], rdw[1]]));
        if !(4..=VB_MAX_RECORD_LEN).contains(&record_len) {
            return Err(Error::new(
                ErrorCode::CBKF222_BDW_LENGTH_INVALID,
                format!("RDW length out of bounds inside VB block: {record_len} bytes"),
            )
            .with_context(self.record_context("Invalid nested RDW length")));
        }
        if record_len > self.block_remaining {
            return Err(Error::new(
                ErrorCode::CBKF224_RDW_BEYOND_BLOCK,
                format!(
                    "RDW of {record_len} bytes escapes VB block {} ({} bytes remain)",
                    self.block_index, self.block_remaining
                ),
            )
            .with_context(self.record_context("Nested RDW beyond block end")));
        }
        let payload_len = record_len - 4;
        if let Some(cap) = self.max_record_length
            && u64::try_from(payload_len).is_ok_and(|declared| declared > cap)
        {
            return Err(Error::new(
                ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED,
                format!(
                    "VB block {} record {} declares {payload_len} payload bytes, exceeding the reviewed bound of {cap}",
                    self.block_index, self.record_count + 1,
                ),
            )
            .with_context(self.record_context("Nested RDW exceeds reviewed bound")));
        }
        let mut payload = vec![0u8; payload_len];
        self.input.read_exact(&mut payload).map_err(|_| {
            Error::new(
                ErrorCode::CBKF223_BDW_UNDERFLOW,
                format!("Truncated RDW payload inside VB block {}", self.block_index),
            )
            .with_context(self.record_context("Block ends mid-RDW payload"))
        })?;
        self.block_remaining -= record_len;
        self.physical_offset += record_len as u64;
        let record = VbRecord {
            payload,
            rdw,
            rdw_reserved: u16::from_be_bytes([rdw[2], rdw[3]]),
            block_index: self.block_index,
            record_index_in_block: self.block_record_index,
            block_offset: self.block_offset,
            physical_offset: rdw_offset,
            block_len: self.block_len,
        };
        self.block_record_index += 1;
        self.record_count += 1;
        if self.block_remaining == 0 {
            self.in_block = false;
            self.block_index += 1;
        }
        Ok(record)
    }

    fn block_context(&self, details: impl Into<String>) -> ErrorContext {
        ErrorContext {
            record_index: None,
            field_path: None,
            byte_offset: Some(self.physical_offset),
            line_number: None,
            details: Some(details.into()),
        }
    }

    fn record_context(&self, details: impl Into<String>) -> ErrorContext {
        ErrorContext {
            record_index: Some(self.record_count + 1),
            field_path: None,
            byte_offset: Some(self.physical_offset),
            line_number: None,
            details: Some(details.into()),
        }
    }

    /// Number of blocks fully consumed.
    #[inline]
    #[must_use]
    pub const fn block_count(&self) -> u64 {
        self.block_index
    }

    /// Number of records yielded.
    #[inline]
    #[must_use]
    pub const fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Physical input bytes consumed, headers included.
    #[inline]
    #[must_use]
    pub const fn physical_bytes(&self) -> u64 {
        self.physical_offset
    }
}

/// Deterministic VB block writer packing records in order.
///
/// Records fill the current block while they fit within 32760 bytes; a new
/// block starts otherwise. Reserved bytes are zero.
#[derive(Debug)]
pub struct VbBlockWriter<W: Write> {
    output: W,
    pending: Vec<u8>,
    block_count: u64,
    record_count: u64,
    physical_bytes: u64,
}

impl<W: Write> VbBlockWriter<W> {
    /// Create a new VB block writer.
    #[inline]
    #[must_use]
    pub fn new(output: W) -> Self {
        Self {
            output,
            pending: Vec::new(),
            block_count: 0,
            record_count: 0,
            physical_bytes: 0,
        }
    }

    /// Buffer one payload with an RDW header (length includes the header).
    ///
    /// # Errors
    /// Returns `CBKF222_BDW_LENGTH_INVALID` when the framed record exceeds
    /// the 32756-byte maximum, before any allocation beyond the payload.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn write_record_from_payload(&mut self, payload: &[u8], reserved: u16) -> Result<()> {
        let record_len = payload
            .len()
            .checked_add(4)
            .filter(|len| *len <= VB_MAX_RECORD_LEN)
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKF222_BDW_LENGTH_INVALID,
                    format!(
                        "VB record too large: {} payload bytes exceed maximum of {VB_MAX_RECORD_LEN}",
                        payload.len()
                    ),
                )
            })?;
        if !self.pending.is_empty()
            && self.pending.len() + BDW_HEADER_LEN + record_len > BDW_MAX_BLOCK_LEN
        {
            self.flush_block()?;
        }
        let len_bytes = u16::try_from(record_len)
            .map_err(|_| {
                Error::new(
                    ErrorCode::CBKF222_BDW_LENGTH_INVALID,
                    format!("VB record length out of range: {record_len} bytes"),
                )
            })?
            .to_be_bytes();
        let reserved_bytes = reserved.to_be_bytes();
        self.pending.extend_from_slice(&[
            len_bytes[0],
            len_bytes[1],
            reserved_bytes[0],
            reserved_bytes[1],
        ]);
        self.pending.extend_from_slice(payload);
        self.record_count += 1;
        Ok(())
    }

    /// Flush any buffered block.
    ///
    /// # Errors
    /// Returns an I/O error when the underlying writer fails.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn finish(&mut self) -> Result<()> {
        if !self.pending.is_empty() {
            self.flush_block()?;
        }
        self.output.flush().map_err(|error| {
            Error::new(
                ErrorCode::CBKR202_RDW_WRITE_ERROR,
                format!("I/O error finishing VB blocks: {error}"),
            )
        })
    }

    fn flush_block(&mut self) -> Result<()> {
        let block_len = self.pending.len() + BDW_HEADER_LEN;
        let header = BdwHeader::from_block_len(block_len)?;
        self.output.write_all(&header.bytes()).map_err(|error| {
            Error::new(
                ErrorCode::CBKR202_RDW_WRITE_ERROR,
                format!("I/O error writing VB block header: {error}"),
            )
        })?;
        self.output.write_all(&self.pending).map_err(|error| {
            Error::new(
                ErrorCode::CBKR202_RDW_WRITE_ERROR,
                format!("I/O error writing VB block body: {error}"),
            )
        })?;
        self.physical_bytes += block_len as u64;
        self.block_count += 1;
        self.pending.clear();
        Ok(())
    }

    /// Number of blocks flushed.
    #[inline]
    #[must_use]
    pub const fn block_count(&self) -> u64 {
        self.block_count
    }

    /// Number of records buffered or written.
    #[inline]
    #[must_use]
    pub const fn record_count(&self) -> u64 {
        self.record_count
    }

    /// Physical bytes written, headers included.
    #[inline]
    #[must_use]
    pub const fn physical_bytes(&self) -> u64 {
        self.physical_bytes
    }
}
