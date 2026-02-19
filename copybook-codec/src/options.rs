//! Configuration options for encoding and decoding operations
#![allow(clippy::missing_inline_in_public_items)]

use serde::{Deserialize, Serialize};
use std::fmt;

/// Zone nibble constants for zoned decimal encoding detection
mod zone_constants {
    /// ASCII digit zone nibble (0x30-0x39 range)
    pub const ASCII_ZONE: u8 = 0x3;
    /// EBCDIC digit zone nibble (0xF0-0xF9 range)
    pub const EBCDIC_ZONE: u8 = 0xF;
    /// Zone nibble mask for extracting upper 4 bits
    pub const ZONE_MASK: u8 = 0x0F;
}

/// Zoned decimal encoding format specification for round-trip fidelity
///
/// This enum controls how zoned decimal fields are encoded and decoded,
/// enabling preservation of the original encoding format during round-trip
/// operations for enterprise data consistency.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum, Default)]
pub enum ZonedEncodingFormat {
    /// ASCII digit zones (0x30-0x39) - Used in ASCII environments
    Ascii,
    /// EBCDIC digit zones (0xF0-0xF9) - Standard mainframe encoding
    Ebcdic,
    /// Automatic detection based on zone nibbles - Analyzes data to determine format
    #[default]
    Auto,
}

impl ZonedEncodingFormat {
    /// Check if this is ASCII encoding
    #[must_use]
    #[inline]
    pub const fn is_ascii(self) -> bool {
        matches!(self, Self::Ascii)
    }

    /// Check if this is EBCDIC encoding
    #[must_use]
    #[inline]
    pub const fn is_ebcdic(self) -> bool {
        matches!(self, Self::Ebcdic)
    }

    /// Check if this is auto-detection mode
    #[must_use]
    #[inline]
    pub const fn is_auto(self) -> bool {
        matches!(self, Self::Auto)
    }

    /// Get a human-readable description of the encoding format
    #[must_use]
    #[inline]
    pub const fn description(self) -> &'static str {
        match self {
            Self::Ascii => "ASCII digit zones (0x30-0x39)",
            Self::Ebcdic => "EBCDIC digit zones (0xF0-0xF9)",
            Self::Auto => "Automatic detection based on zone nibbles",
        }
    }

    /// Detect encoding format from a single byte of zoned decimal data
    ///
    /// Examines the zone nibble (upper 4 bits) to determine the encoding format.
    /// Returns `None` for invalid zone values that don't match standard patterns.
    ///
    /// # Zone Analysis
    /// - `0x3`: ASCII digit zone (0x30-0x39 range)
    /// - `0xF`: EBCDIC digit zone (0xF0-0xF9 range)
    /// - Others: Invalid or non-standard zones
    #[must_use]
    #[inline]
    pub fn detect_from_byte(byte: u8) -> Option<Self> {
        use zone_constants::{ASCII_ZONE, EBCDIC_ZONE, ZONE_MASK};

        let zone_nibble = (byte >> 4) & ZONE_MASK;
        match zone_nibble {
            ASCII_ZONE => Some(Self::Ascii),
            EBCDIC_ZONE => Some(Self::Ebcdic),
            _ => None, // Invalid or mixed zone
        }
    }
}

impl fmt::Display for ZonedEncodingFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ascii => write!(f, "ascii"),
            Self::Ebcdic => write!(f, "ebcdic"),
            Self::Auto => write!(f, "auto"),
        }
    }
}

/// Floating-point binary format for COMP-1/COMP-2 fields.
///
/// Copybooks define field usage but not the compiler's concrete floating-point
/// representation. This option makes the decode/encode interpretation explicit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum, Default)]
pub enum FloatFormat {
    /// IEEE-754 big-endian binary format.
    #[default]
    #[value(name = "ieee-be", alias = "ieee", alias = "ieee-big-endian")]
    IeeeBigEndian,
    /// IBM hexadecimal floating-point format.
    #[value(name = "ibm-hex", alias = "ibm")]
    IbmHex,
}

/// Options for decoding operations
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::struct_excessive_bools)] // Many boolean options are needed for decode configuration
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
    /// Enable zoned decimal encoding preservation for binary round-trip consistency
    ///
    /// When enabled, the decoder captures the original encoding format (ASCII vs EBCDIC)
    /// and includes it in metadata for use during re-encoding to maintain byte-level
    /// fidelity in encode/decode cycles.
    pub preserve_zoned_encoding: bool,
    /// Preferred encoding format when auto-detection is ambiguous
    ///
    /// Used as fallback when `ZonedEncodingFormat::Auto` cannot determine the format
    /// from the data (e.g., all-zero fields, mixed encodings).
    pub preferred_zoned_encoding: ZonedEncodingFormat,
    /// Floating-point representation for COMP-1/COMP-2 fields.
    #[serde(default)]
    pub float_format: FloatFormat,
}

/// Options for encoding operations
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::struct_excessive_bools)]
pub struct EncodeOptions {
    /// Record format
    pub format: RecordFormat,
    /// Character encoding
    pub codepage: Codepage,
    /// Fallback zoned decimal encoding format when no override or metadata applies
    pub preferred_zoned_encoding: ZonedEncodingFormat,
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
    /// Policy for unmappable characters during encoding
    pub on_encode_unmappable: UnmappablePolicy,
    /// JSON number representation mode (used when round-tripping)
    pub json_number_mode: JsonNumberMode,
    /// Explicit zoned decimal encoding format override
    ///
    /// When specified, forces all zoned decimal fields to use this encoding format,
    /// overriding any preserved format from decode operations. This provides the
    /// highest precedence in the format selection hierarchy:
    /// 1. Explicit override (this field)
    /// 2. Preserved format from decode metadata
    /// 3. EBCDIC default for mainframe compatibility
    pub zoned_encoding_override: Option<ZonedEncodingFormat>,
    /// Floating-point representation for COMP-1/COMP-2 fields.
    #[serde(default)]
    pub float_format: FloatFormat,
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
    #[inline]
    pub const fn is_ascii(self) -> bool {
        matches!(self, Self::ASCII)
    }

    /// Check if this is an EBCDIC codepage
    #[must_use]
    #[inline]
    pub const fn is_ebcdic(self) -> bool {
        !self.is_ascii()
    }

    /// Get the numeric code page identifier
    #[must_use]
    #[inline]
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
    #[inline]
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
    #[inline]
    pub const fn is_lossless(self) -> bool {
        matches!(self, Self::Lossless)
    }

    /// Check if this mode uses native JSON numbers
    #[must_use]
    #[inline]
    pub const fn is_native(self) -> bool {
        matches!(self, Self::Native)
    }

    /// Get a human-readable description of the mode
    #[must_use]
    #[inline]
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
            preserve_zoned_encoding: false,
            preferred_zoned_encoding: ZonedEncodingFormat::Auto,
            float_format: FloatFormat::IeeeBigEndian,
        }
    }
}

impl DecodeOptions {
    /// Create new decode options with default values
    #[must_use]
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the record format
    #[must_use]
    #[inline]
    pub fn with_format(mut self, format: RecordFormat) -> Self {
        self.format = format;
        self
    }

    /// Set the codepage
    #[must_use]
    #[inline]
    pub fn with_codepage(mut self, codepage: Codepage) -> Self {
        self.codepage = codepage;
        self
    }

    /// Set the JSON number mode
    #[must_use]
    #[inline]
    pub fn with_json_number_mode(mut self, mode: JsonNumberMode) -> Self {
        self.json_number_mode = mode;
        self
    }

    /// Enable or disable FILLER field emission
    #[must_use]
    #[inline]
    pub fn with_emit_filler(mut self, emit_filler: bool) -> Self {
        self.emit_filler = emit_filler;
        self
    }

    /// Enable or disable metadata emission
    #[must_use]
    #[inline]
    pub fn with_emit_meta(mut self, emit_meta: bool) -> Self {
        self.emit_meta = emit_meta;
        self
    }

    /// Set the raw data capture mode
    #[must_use]
    #[inline]
    pub fn with_emit_raw(mut self, emit_raw: RawMode) -> Self {
        self.emit_raw = emit_raw;
        self
    }

    /// Enable or disable strict mode
    #[must_use]
    #[inline]
    pub fn with_strict_mode(mut self, strict_mode: bool) -> Self {
        self.strict_mode = strict_mode;
        self
    }

    /// Set the maximum number of errors before stopping
    #[must_use]
    #[inline]
    pub fn with_max_errors(mut self, max_errors: Option<u64>) -> Self {
        self.max_errors = max_errors;
        self
    }

    /// Set the policy for unmappable characters
    #[must_use]
    #[inline]
    pub fn with_unmappable_policy(mut self, policy: UnmappablePolicy) -> Self {
        self.on_decode_unmappable = policy;
        self
    }

    /// Set the number of threads for parallel processing
    #[must_use]
    #[inline]
    pub fn with_threads(mut self, threads: usize) -> Self {
        self.threads = threads;
        self
    }

    // === Zoned Decimal Encoding Configuration ===

    /// Enable zoned decimal encoding preservation for round-trip fidelity
    ///
    /// When enabled, the decoder will detect and preserve the original encoding
    /// format (ASCII vs EBCDIC) for use during subsequent encoding operations.
    /// This ensures byte-level consistency in encode/decode cycles.
    #[must_use]
    #[inline]
    pub fn with_preserve_zoned_encoding(mut self, preserve_zoned_encoding: bool) -> Self {
        self.preserve_zoned_encoding = preserve_zoned_encoding;
        self
    }

    /// Set the preferred zoned encoding format for ambiguous detection
    ///
    /// This format is used as a fallback when auto-detection cannot determine
    /// the encoding from the data (e.g., all-zero fields, mixed encodings).
    #[must_use]
    #[inline]
    pub fn with_preferred_zoned_encoding(
        mut self,
        preferred_zoned_encoding: ZonedEncodingFormat,
    ) -> Self {
        self.preferred_zoned_encoding = preferred_zoned_encoding;
        self
    }

    /// Set floating-point representation for COMP-1/COMP-2 fields
    #[must_use]
    #[inline]
    pub fn with_float_format(mut self, float_format: FloatFormat) -> Self {
        self.float_format = float_format;
        self
    }
}

impl Default for EncodeOptions {
    fn default() -> Self {
        Self {
            format: RecordFormat::Fixed,
            codepage: Codepage::CP037,
            preferred_zoned_encoding: ZonedEncodingFormat::Auto,
            use_raw: false,
            bwz_encode: false,
            strict_mode: false,
            max_errors: None,
            threads: 1,
            coerce_numbers: false,
            on_encode_unmappable: UnmappablePolicy::Error,
            json_number_mode: JsonNumberMode::Lossless,
            zoned_encoding_override: None,
            float_format: FloatFormat::IeeeBigEndian,
        }
    }
}

impl EncodeOptions {
    /// Create new encode options with default values
    #[must_use]
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the record format
    #[must_use]
    #[inline]
    pub fn with_format(mut self, format: RecordFormat) -> Self {
        self.format = format;
        self
    }

    /// Set the codepage
    #[must_use]
    #[inline]
    pub fn with_codepage(mut self, codepage: Codepage) -> Self {
        self.codepage = codepage;
        self
    }

    /// Enable or disable raw data usage
    #[must_use]
    #[inline]
    pub fn with_use_raw(mut self, use_raw: bool) -> Self {
        self.use_raw = use_raw;
        self
    }

    /// Enable or disable BLANK WHEN ZERO encoding
    #[must_use]
    #[inline]
    pub fn with_bwz_encode(mut self, bwz_encode: bool) -> Self {
        self.bwz_encode = bwz_encode;
        self
    }

    /// Set the preferred zoned encoding fallback when overrides and preserved formats are absent
    #[must_use]
    #[inline]
    pub fn with_preferred_zoned_encoding(
        mut self,
        preferred_zoned_encoding: ZonedEncodingFormat,
    ) -> Self {
        self.preferred_zoned_encoding = preferred_zoned_encoding;
        self
    }

    /// Enable or disable strict mode
    #[must_use]
    #[inline]
    pub fn with_strict_mode(mut self, strict_mode: bool) -> Self {
        self.strict_mode = strict_mode;
        self
    }

    /// Set the maximum number of errors before stopping
    #[must_use]
    #[inline]
    pub fn with_max_errors(mut self, max_errors: Option<u64>) -> Self {
        self.max_errors = max_errors;
        self
    }

    /// Set the number of threads for parallel processing
    #[must_use]
    #[inline]
    pub fn with_threads(mut self, threads: usize) -> Self {
        self.threads = threads;
        self
    }

    /// Enable or disable number coercion
    #[must_use]
    #[inline]
    pub fn with_coerce_numbers(mut self, coerce_numbers: bool) -> Self {
        self.coerce_numbers = coerce_numbers;
        self
    }

    /// Set the policy for unmappable characters during encoding
    #[must_use]
    #[inline]
    pub fn with_unmappable_policy(mut self, policy: UnmappablePolicy) -> Self {
        self.on_encode_unmappable = policy;
        self
    }

    /// Set the JSON number mode
    #[must_use]
    #[inline]
    pub fn with_json_number_mode(mut self, mode: JsonNumberMode) -> Self {
        self.json_number_mode = mode;
        self
    }

    /// Set explicit zoned decimal encoding format override
    ///
    /// Forces all zoned decimal fields to use the specified encoding format,
    /// overriding any preserved format from decode operations. Use `None` to
    /// disable override and respect preserved formats.
    #[must_use]
    #[inline]
    pub fn with_zoned_encoding_override(
        mut self,
        zoned_encoding_override: Option<ZonedEncodingFormat>,
    ) -> Self {
        self.zoned_encoding_override = zoned_encoding_override;
        self
    }

    /// Convenience method to set explicit zoned encoding format
    ///
    /// Equivalent to `with_zoned_encoding_override(Some(format))`.
    #[must_use]
    #[inline]
    pub fn with_zoned_encoding_format(mut self, format: ZonedEncodingFormat) -> Self {
        self.zoned_encoding_override = Some(format);
        self
    }

    /// Set floating-point representation for COMP-1/COMP-2 fields
    #[must_use]
    #[inline]
    pub fn with_float_format(mut self, float_format: FloatFormat) -> Self {
        self.float_format = float_format;
        self
    }
}
impl fmt::Display for RecordFormat {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fixed => write!(f, "fixed"),
            Self::RDW => write!(f, "rdw"),
        }
    }
}

impl fmt::Display for Codepage {
    #[inline]
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
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lossless => write!(f, "lossless"),
            Self::Native => write!(f, "native"),
        }
    }
}

impl fmt::Display for RawMode {
    #[inline]
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
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Replace => write!(f, "replace"),
            Self::Skip => write!(f, "skip"),
        }
    }
}

impl fmt::Display for FloatFormat {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IeeeBigEndian => write!(f, "ieee-be"),
            Self::IbmHex => write!(f, "ibm-hex"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zoned_encoding_format_is_ascii() {
        assert!(ZonedEncodingFormat::Ascii.is_ascii());
        assert!(!ZonedEncodingFormat::Ebcdic.is_ascii());
        assert!(!ZonedEncodingFormat::Auto.is_ascii());
    }

    #[test]
    fn test_zoned_encoding_format_is_ebcdic() {
        assert!(!ZonedEncodingFormat::Ascii.is_ebcdic());
        assert!(ZonedEncodingFormat::Ebcdic.is_ebcdic());
        assert!(!ZonedEncodingFormat::Auto.is_ebcdic());
    }

    #[test]
    fn test_zoned_encoding_format_is_auto() {
        assert!(!ZonedEncodingFormat::Ascii.is_auto());
        assert!(!ZonedEncodingFormat::Ebcdic.is_auto());
        assert!(ZonedEncodingFormat::Auto.is_auto());
    }

    #[test]
    fn test_zoned_encoding_format_description() {
        assert_eq!(
            ZonedEncodingFormat::Ascii.description(),
            "ASCII digit zones (0x30-0x39)"
        );
        assert_eq!(
            ZonedEncodingFormat::Ebcdic.description(),
            "EBCDIC digit zones (0xF0-0xF9)"
        );
        assert_eq!(
            ZonedEncodingFormat::Auto.description(),
            "Automatic detection based on zone nibbles"
        );
    }

    #[test]
    fn test_zoned_encoding_format_detect_from_byte() {
        // ASCII zone nibble (0x30)
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0x35),
            Some(ZonedEncodingFormat::Ascii)
        );
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0x30),
            Some(ZonedEncodingFormat::Ascii)
        );
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0x39),
            Some(ZonedEncodingFormat::Ascii)
        );

        // EBCDIC zone nibble (0xF0)
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0xF5),
            Some(ZonedEncodingFormat::Ebcdic)
        );
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0xF0),
            Some(ZonedEncodingFormat::Ebcdic)
        );
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0xF9),
            Some(ZonedEncodingFormat::Ebcdic)
        );

        // Invalid zone nibbles (0x00, 0x50)
        assert_eq!(ZonedEncodingFormat::detect_from_byte(0x00), None);
        assert_eq!(ZonedEncodingFormat::detect_from_byte(0x50), None);
        // Note: 0xFF matches EBCDIC_ZONE (0x0F), so it returns Some(Ebcdic)
        assert_eq!(
            ZonedEncodingFormat::detect_from_byte(0xFF),
            Some(ZonedEncodingFormat::Ebcdic)
        );
    }

    #[test]
    fn test_zoned_encoding_format_display() {
        assert_eq!(format!("{}", ZonedEncodingFormat::Ascii), "ascii");
        assert_eq!(format!("{}", ZonedEncodingFormat::Ebcdic), "ebcdic");
        assert_eq!(format!("{}", ZonedEncodingFormat::Auto), "auto");
    }

    #[test]
    fn test_decode_options_default() {
        let options = DecodeOptions::default();
        assert_eq!(options.format, RecordFormat::Fixed);
        assert_eq!(options.codepage, Codepage::CP037);
        assert_eq!(options.json_number_mode, JsonNumberMode::Lossless);
        assert!(!options.emit_filler);
        assert!(!options.emit_meta);
        assert_eq!(options.emit_raw, RawMode::Off);
        assert!(!options.strict_mode);
        assert!(options.max_errors.is_none());
        assert_eq!(options.on_decode_unmappable, UnmappablePolicy::Error);
        assert_eq!(options.threads, 1);
        assert!(!options.preserve_zoned_encoding);
        assert_eq!(options.preferred_zoned_encoding, ZonedEncodingFormat::Auto);
        assert_eq!(options.float_format, FloatFormat::IeeeBigEndian);
    }

    #[test]
    fn test_encode_options_default() {
        let options = EncodeOptions::default();
        assert_eq!(options.format, RecordFormat::Fixed);
        assert_eq!(options.codepage, Codepage::CP037);
        assert_eq!(options.preferred_zoned_encoding, ZonedEncodingFormat::Auto);
        assert!(!options.use_raw);
        assert!(!options.bwz_encode);
        assert!(!options.strict_mode);
        assert_eq!(options.on_encode_unmappable, UnmappablePolicy::Error);
        assert_eq!(options.json_number_mode, JsonNumberMode::Lossless);
        assert_eq!(options.float_format, FloatFormat::IeeeBigEndian);
    }

    #[test]
    fn test_record_format_display() {
        assert_eq!(format!("{}", RecordFormat::Fixed), "fixed");
        assert_eq!(format!("{}", RecordFormat::RDW), "rdw");
    }

    #[test]
    fn test_codepage_display() {
        assert_eq!(format!("{}", Codepage::CP037), "cp037");
        assert_eq!(format!("{}", Codepage::CP273), "cp273");
        assert_eq!(format!("{}", Codepage::CP500), "cp500");
        assert_eq!(format!("{}", Codepage::CP1047), "cp1047");
        assert_eq!(format!("{}", Codepage::CP1140), "cp1140");
    }

    #[test]
    fn test_json_number_mode_display() {
        assert_eq!(format!("{}", JsonNumberMode::Lossless), "lossless");
        assert_eq!(format!("{}", JsonNumberMode::Native), "native");
    }

    #[test]
    fn test_raw_mode_display() {
        assert_eq!(format!("{}", RawMode::Off), "off");
        assert_eq!(format!("{}", RawMode::Record), "record");
        assert_eq!(format!("{}", RawMode::Field), "field");
        assert_eq!(format!("{}", RawMode::RecordRDW), "record+rdw");
    }

    #[test]
    fn test_unmappable_policy_display() {
        assert_eq!(format!("{}", UnmappablePolicy::Error), "error");
        assert_eq!(format!("{}", UnmappablePolicy::Replace), "replace");
        assert_eq!(format!("{}", UnmappablePolicy::Skip), "skip");
    }

    #[test]
    fn test_decode_options_serialization() {
        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::CP037,
            json_number_mode: JsonNumberMode::Lossless,
            emit_filler: true,
            emit_meta: true,
            emit_raw: RawMode::Record,
            strict_mode: true,
            max_errors: Some(100),
            on_decode_unmappable: UnmappablePolicy::Replace,
            threads: 4,
            preserve_zoned_encoding: true,
            preferred_zoned_encoding: ZonedEncodingFormat::Ebcdic,
            float_format: FloatFormat::IbmHex,
        };

        let serialized = serde_json::to_string(&options).unwrap();
        let deserialized: DecodeOptions = serde_json::from_str(&serialized).unwrap();

        assert_eq!(deserialized.format, RecordFormat::Fixed);
        assert_eq!(deserialized.codepage, Codepage::CP037);
        assert!(deserialized.emit_filler);
        assert!(deserialized.emit_meta);
        assert_eq!(deserialized.emit_raw, RawMode::Record);
        assert!(deserialized.strict_mode);
        assert_eq!(deserialized.max_errors, Some(100));
        assert_eq!(deserialized.on_decode_unmappable, UnmappablePolicy::Replace);
        assert_eq!(deserialized.threads, 4);
        assert!(deserialized.preserve_zoned_encoding);
        assert_eq!(
            deserialized.preferred_zoned_encoding,
            ZonedEncodingFormat::Ebcdic
        );
        assert_eq!(deserialized.float_format, FloatFormat::IbmHex);
    }

    #[test]
    fn test_decode_options_deserialize_missing_float_format_defaults() {
        let options = DecodeOptions::default();
        let mut value = serde_json::to_value(options).unwrap();
        value.as_object_mut().unwrap().remove("float_format");
        let deserialized: DecodeOptions = serde_json::from_value(value).unwrap();
        assert_eq!(deserialized.float_format, FloatFormat::IeeeBigEndian);
    }

    #[test]
    fn test_encode_options_deserialize_missing_float_format_defaults() {
        let options = EncodeOptions::default();
        let mut value = serde_json::to_value(options).unwrap();
        value.as_object_mut().unwrap().remove("float_format");
        let deserialized: EncodeOptions = serde_json::from_value(value).unwrap();
        assert_eq!(deserialized.float_format, FloatFormat::IeeeBigEndian);
    }
}
