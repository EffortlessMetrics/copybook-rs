//! Configuration options for encoding and decoding operations

use serde::{Deserialize, Serialize};

/// Options for decoding operations
#[derive(Debug, Clone)]
pub struct DecodeOptions {
    /// Record format
    pub format: RecordFormat,
    /// Character encoding
    pub codepage: Codepage,
    /// JSON number representation
    pub json_number_mode: JsonNumberMode,
    /// Whether to emit FILLER fields
    pub emit_filler: bool,
    /// Whether to emit metadata
    pub emit_meta: bool,
    /// Raw data capture mode
    pub emit_raw: RawMode,
    /// Error handling mode
    pub strict_mode: bool,
    /// Maximum errors before stopping
    pub max_errors: Option<u64>,
    /// Policy for unmappable characters
    pub on_decode_unmappable: UnmappablePolicy,
}

/// Options for encoding operations
#[derive(Debug, Clone)]
pub struct EncodeOptions {
    /// Record format
    pub format: RecordFormat,
    /// Character encoding
    pub codepage: Codepage,
    /// Whether to use raw data when available
    pub use_raw: bool,
    /// BLANK WHEN ZERO encoding policy
    pub bwz_encode: bool,
    /// Error handling mode
    pub strict_mode: bool,
    /// Maximum errors before stopping
    pub max_errors: Option<u64>,
}

/// Record format specification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum RecordFormat {
    /// Fixed-length records
    Fixed,
    /// Variable-length records with RDW
    RDW,
}

/// Character encoding specification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum Codepage {
    /// ASCII encoding
    ASCII,
    /// EBCDIC Code Page 037 (US/Canada)
    CP037,
    /// EBCDIC Code Page 273 (Germany/Austria)
    CP273,
    /// EBCDIC Code Page 500 (International)
    CP500,
    /// EBCDIC Code Page 1047 (Open Systems)
    CP1047,
    /// EBCDIC Code Page 1140 (US/Canada with Euro)
    CP1140,
}

/// JSON number representation mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum JsonNumberMode {
    /// Lossless string representation for decimals
    Lossless,
    /// Native JSON numbers where possible
    Native,
}

/// Raw data capture mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum RawMode {
    /// No raw data capture
    Off,
    /// Capture record-level raw data
    Record,
    /// Capture field-level raw data
    Field,
    /// Capture record and RDW header
    #[value(name = "record+rdw")]
    RecordRDW,
}

/// Policy for handling unmappable characters during decode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum UnmappablePolicy {
    /// Error on unmappable characters
    Error,
    /// Replace with U+FFFD
    Replace,
    /// Skip unmappable characters
    Skip,
}

impl Default for DecodeOptions {
    fn default() -> Self {
        Self {
            format: RecordFormat::Fixed,
            codepage: Codepage::CP037,
            json_number_mode: JsonNumberMode::Lossless,
            emit_filler: false,
            emit_meta: false,
            emit_raw: RawMode::Off,
            strict_mode: false,
            max_errors: None,
            on_decode_unmappable: UnmappablePolicy::Error,
        }
    }
}

impl Default for EncodeOptions {
    fn default() -> Self {
        Self {
            format: RecordFormat::Fixed,
            codepage: Codepage::CP037,
            use_raw: false,
            bwz_encode: false,
            strict_mode: false,
            max_errors: None,
        }
    }
}
