// SPDX-License-Identifier: AGPL-3.0-or-later
//! Configuration options for Arrow/Parquet output

/// Options for COBOL-to-Arrow conversion.
#[derive(Debug, Clone)]
#[allow(clippy::struct_excessive_bools)]
pub struct ArrowOptions {
    /// Number of records per Arrow `RecordBatch` (default: 8192)
    pub batch_size: usize,
    /// Flatten group fields to top-level columns (default: true)
    pub flatten_groups: bool,
    /// How to represent edited PIC fields in Arrow
    pub edited_pic_as: EditedPicRepresentation,
    /// Include FILLER fields in output (default: false)
    pub emit_filler: bool,
    /// Include metadata column with record info (default: false)
    pub emit_meta: bool,
    /// Parquet compression codec
    pub compression: Compression,
    /// Row group size for Parquet output
    pub row_group_size: usize,
    /// Embed copybook text in Parquet metadata
    pub embed_copybook: bool,
    /// Codepage for character conversion
    pub codepage: copybook_codec::Codepage,
    /// Floating-point representation for COMP-1/COMP-2 fields
    pub float_format: copybook_codec::FloatFormat,
}

/// How edited PIC fields are represented in Arrow
#[derive(Debug, Clone, Copy, Default)]
pub enum EditedPicRepresentation {
    /// Store as Decimal128 (extract numeric value)
    #[default]
    Decimal,
    /// Store as Utf8 string (preserve formatting)
    String,
}

/// Compression codec for Parquet files
#[derive(Debug, Clone, Copy, Default)]
pub enum Compression {
    /// No compression
    None,
    /// Snappy compression
    Snappy,
    /// Gzip compression
    Gzip,
    /// LZ4 compression
    Lz4,
    /// Zstd compression (default)
    #[default]
    Zstd,
}

impl Default for ArrowOptions {
    #[inline]
    fn default() -> Self {
        Self {
            batch_size: 8192,
            flatten_groups: true,
            edited_pic_as: EditedPicRepresentation::default(),
            emit_filler: false,
            emit_meta: false,
            compression: Compression::default(),
            row_group_size: 1_000_000,
            embed_copybook: false,
            codepage: copybook_codec::Codepage::CP037,
            float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        }
    }
}
