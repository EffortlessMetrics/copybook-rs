//! Configuration options for encoding and decoding operations

use serde::{Deserialize, Serialize};
use std::fmt;

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
    /// Number of threads for parallel processing
    pub threads: usize,
}

/// Options for encoding operations
#[derive(Debug, Clone)]
#[allow(clippy::struct_excessive_bools)]
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
    /// Number of threads for parallel processing
    pub threads: usize,
    /// Whether to coerce non-string JSON numbers to strings before encoding
    pub coerce_numbers: bool,
}

/// Record format specification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum RecordFormat {
    /// Fixed-length records
    Fixed,
    /// Variable-length records with RDW
    RDW,
}

impl RecordFormat {
    /// Check if this is a fixed-length record format
    #[must_use]
    pub const fn is_fixed(self) -> bool {
        matches!(self, Self::Fixed)
    }

    /// Check if this is a variable-length record format
    #[must_use]
    pub const fn is_variable(self) -> bool {
        matches!(self, Self::RDW)
    }

    /// Get a human-readable description of the format
    #[must_use]
    pub const fn description(self) -> &'static str {
        match self {
            Self::Fixed => "Fixed-length records",
            Self::RDW => "Variable-length records with Record Descriptor Word",
        }
    }
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

impl Codepage {
    /// Check if this is an ASCII codepage
    #[must_use]
    pub const fn is_ascii(self) -> bool {
        matches!(self, Self::ASCII)
    }

    /// Check if this is an EBCDIC codepage
    #[must_use]
    pub const fn is_ebcdic(self) -> bool {
        !self.is_ascii()
    }

    /// Get the numeric code page identifier
    #[must_use]
    pub const fn code_page_number(self) -> Option<u16> {
        match self {
            Self::ASCII => None,
            Self::CP037 => Some(37),
            Self::CP273 => Some(273),
            Self::CP500 => Some(500),
            Self::CP1047 => Some(1047),
            Self::CP1140 => Some(1140),
        }
    }

    /// Get a human-readable description of the codepage
    #[must_use]
    pub const fn description(self) -> &'static str {
        match self {
            Self::ASCII => "ASCII encoding",
            Self::CP037 => "EBCDIC Code Page 037 (US/Canada)",
            Self::CP273 => "EBCDIC Code Page 273 (Germany/Austria)",
            Self::CP500 => "EBCDIC Code Page 500 (International)",
            Self::CP1047 => "EBCDIC Code Page 1047 (Open Systems)",
            Self::CP1140 => "EBCDIC Code Page 1140 (US/Canada with Euro)",
        }
    }
}

/// JSON number representation mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum JsonNumberMode {
    /// Lossless string representation for decimals
    Lossless,
    /// Native JSON numbers where possible
    Native,
}

impl JsonNumberMode {
    /// Check if this mode uses lossless string representation
    #[must_use]
    pub const fn is_lossless(self) -> bool {
        matches!(self, Self::Lossless)
    }

    /// Check if this mode uses native JSON numbers
    #[must_use]
    pub const fn is_native(self) -> bool {
        matches!(self, Self::Native)
    }

    /// Get a human-readable description of the mode
    #[must_use]
    pub const fn description(self) -> &'static str {
        match self {
            Self::Lossless => "Lossless string representation for decimals",
            Self::Native => "Native JSON numbers where possible",
        }
    }
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
            threads: 1,
        }
    }
}

impl DecodeOptions {
    /// Create new decode options with default values
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the record format
    #[must_use]
    pub fn with_format(mut self, format: RecordFormat) -> Self {
        self.format = format;
        self
    }

    /// Set the codepage
    #[must_use]
    pub fn with_codepage(mut self, codepage: Codepage) -> Self {
        self.codepage = codepage;
        self
    }

    /// Set the JSON number mode
    #[must_use]
    pub fn with_json_number_mode(mut self, mode: JsonNumberMode) -> Self {
        self.json_number_mode = mode;
        self
    }

    /// Enable or disable FILLER field emission
    #[must_use]
    pub fn with_emit_filler(mut self, emit_filler: bool) -> Self {
        self.emit_filler = emit_filler;
        self
    }

    /// Enable or disable metadata emission
    #[must_use]
    pub fn with_emit_meta(mut self, emit_meta: bool) -> Self {
        self.emit_meta = emit_meta;
        self
    }

    /// Set the raw data capture mode
    #[must_use]
    pub fn with_emit_raw(mut self, emit_raw: RawMode) -> Self {
        self.emit_raw = emit_raw;
        self
    }

    /// Enable or disable strict mode
    #[must_use]
    pub fn with_strict_mode(mut self, strict_mode: bool) -> Self {
        self.strict_mode = strict_mode;
        self
    }

    /// Set the maximum number of errors before stopping
    #[must_use]
    pub fn with_max_errors(mut self, max_errors: Option<u64>) -> Self {
        self.max_errors = max_errors;
        self
    }

    /// Set the policy for unmappable characters
    #[must_use]
    pub fn with_unmappable_policy(mut self, policy: UnmappablePolicy) -> Self {
        self.on_decode_unmappable = policy;
        self
    }

    /// Set the number of threads for parallel processing
    #[must_use]
    pub fn with_threads(mut self, threads: usize) -> Self {
        self.threads = threads;
        self
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
            threads: 1,
            coerce_numbers: false,
        }
    }
}

impl EncodeOptions {
    /// Create new encode options with default values
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the record format
    #[must_use]
    pub fn with_format(mut self, format: RecordFormat) -> Self {
        self.format = format;
        self
    }

    /// Set the codepage
    #[must_use]
    pub fn with_codepage(mut self, codepage: Codepage) -> Self {
        self.codepage = codepage;
        self
    }

    /// Enable or disable raw data usage
    #[must_use]
    pub fn with_use_raw(mut self, use_raw: bool) -> Self {
        self.use_raw = use_raw;
        self
    }

    /// Enable or disable BLANK WHEN ZERO encoding
    #[must_use]
    pub fn with_bwz_encode(mut self, bwz_encode: bool) -> Self {
        self.bwz_encode = bwz_encode;
        self
    }

    /// Enable or disable strict mode
    #[must_use]
    pub fn with_strict_mode(mut self, strict_mode: bool) -> Self {
        self.strict_mode = strict_mode;
        self
    }

    /// Set the maximum number of errors before stopping
    #[must_use]
    pub fn with_max_errors(mut self, max_errors: Option<u64>) -> Self {
        self.max_errors = max_errors;
        self
    }

    /// Set the number of threads for parallel processing
    #[must_use]
    pub fn with_threads(mut self, threads: usize) -> Self {
        self.threads = threads;
        self
    }

    /// Enable or disable number coercion
    #[must_use]
    pub fn with_coerce_numbers(mut self, coerce_numbers: bool) -> Self {
        self.coerce_numbers = coerce_numbers;
        self
    }
}
impl fmt::Display for RecordFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fixed => write!(f, "fixed"),
            Self::RDW => write!(f, "rdw"),
        }
    }
}

impl fmt::Display for Codepage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ASCII => write!(f, "ascii"),
            Self::CP037 => write!(f, "cp037"),
            Self::CP273 => write!(f, "cp273"),
            Self::CP500 => write!(f, "cp500"),
            Self::CP1047 => write!(f, "cp1047"),
            Self::CP1140 => write!(f, "cp1140"),
        }
    }
}

impl fmt::Display for JsonNumberMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lossless => write!(f, "lossless"),
            Self::Native => write!(f, "native"),
        }
    }
}

impl fmt::Display for RawMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Off => write!(f, "off"),
            Self::Record => write!(f, "record"),
            Self::Field => write!(f, "field"),
            Self::RecordRDW => write!(f, "record+rdw"),
        }
    }
}

impl fmt::Display for UnmappablePolicy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Replace => write!(f, "replace"),
            Self::Skip => write!(f, "skip"),
        }
    }
}
