// SPDX-License-Identifier: AGPL-3.0-or-later
//! # Numeric Type Codecs for COBOL Data
//!
//! This module provides encoding and decoding functions for the three main COBOL numeric
//! data types:
//!
//! - **Zoned Decimal** (`PIC 9` with optional `SIGN SEPARATE`): External decimal format
//!   where each digit is stored in a byte with a zone nibble and a digit nibble.
//!   The sign may be encoded via overpunch or stored in a separate byte.
//!
//! - **Packed Decimal** (`COMP-3`): Compact binary format where each byte contains
//!   two decimal digits (nibbles), with the last nibble containing the sign.
//!
//! - **Binary Integer** (`COMP-4`, `COMP-5`, `BINARY`): Standard binary integer
//!   encoding in big-endian byte order.
//!
//! ## Module Organization
//!
//! The module is organized into three main categories:
//!
//! ### Decoding Functions
//! - [`decode_zoned_decimal`] - Decode zoned decimal fields
//! - [`decode_zoned_decimal_sign_separate`] - Decode SIGN SEPARATE zoned decimals
//! - [`decode_zoned_decimal_with_encoding`] - Decode with encoding detection
//! - [`decode_packed_decimal`] - Decode COMP-3 packed decimals
//! - [`decode_binary_int`] - Decode binary integer fields
//!
//! ### Encoding Functions
//! - [`encode_zoned_decimal`] - Encode zoned decimal fields
//! - [`encode_zoned_decimal_with_format`] - Encode with explicit encoding format
//! - [`encode_zoned_decimal_with_format_and_policy`] - Encode with format and policy
//! - [`encode_zoned_decimal_with_bwz`] - Encode with BLANK WHEN ZERO support
//! - [`encode_packed_decimal`] - Encode COMP-3 packed decimals
//! - [`encode_binary_int`] - Encode binary integer fields
//!
//! ### Utility Functions
//! - [`get_binary_width_from_digits`] - Map digit count to binary width
//! - [`validate_explicit_binary_width`] - Validate explicit BINARY(n) widths
//! - [`should_encode_as_blank_when_zero`] - Check BLANK WHEN ZERO policy
//!
//! ## Performance Considerations
//!
//! This module is optimized for high-throughput enterprise data processing:
//!
//! - **Hot path optimization**: Common cases (1-5 byte COMP-3, ASCII zoned) use
//!   specialized fast paths
//! - **Branch prediction**: Manual hints mark error paths as unlikely
//! - **Zero-allocation**: Scratch buffer variants avoid repeated allocations in loops
//! - **Saturating arithmetic**: Prevents panics while maintaining correctness
//!
//! ## Encoding Formats
//!
//! ### Zoned Decimal Encoding
//!
//! Zoned decimals support two primary encoding formats:
//!
//! | Format | Zone Nibble | Example Digits | Sign Encoding |
//! |---------|--------------|----------------|---------------|
//! | ASCII | `0x3` | `0x30`-`0x39` | Overpunch or separate |
//! | EBCDIC | `0xF` | `0xF0`-`0xF9` | Overpunch or separate |
//!
//! ### Packed Decimal Sign Nibbles
//!
//! COMP-3 uses the last nibble for sign encoding:
//!
//! | Sign | Nibble | Description |
//! |------|---------|-------------|
//! | Positive | `0xC`, `0xA`, `0xE`, `0xF` | Positive values |
//! | Negative | `0xB`, `0xD` | Negative values |
//! | Unsigned | `0xF` | Unsigned fields only |
//!
//! ### Binary Integer Widths
//!
//! Binary integers use the following width mappings:
//!
//! | Digits | Width | Bits | Range (signed) |
//! |---------|--------|-------|----------------|
//! | 1-4 | 2 bytes | 16 | -32,768 to 32,767 |
//! | 5-9 | 4 bytes | 32 | -2,147,483,648 to 2,147,483,647 |
//! | 10-18 | 8 bytes | 64 | -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 |
//!
//! ## Examples
//!
//! ### Decoding a Zoned Decimal
//!
//! ```no_run
//! use copybook_codec::numeric::{decode_zoned_decimal};
//! use copybook_codec::options::Codepage;
//!
//! // ASCII zoned decimal: "123" = [0x31, 0x32, 0x33]
//! let data = b"123";
//! let result = decode_zoned_decimal(data, 3, 0, false, Codepage::ASCII, false)?;
//! assert_eq!(result.to_string(), "123");
//! # Ok::<(), copybook_core::Error>(())
//! ```
//!
//! ### Encoding a Packed Decimal
//!
//! ```no_run
//! use copybook_codec::numeric::{encode_packed_decimal};
//!
//! // Encode "123.45" as 7-digit COMP-3 with 2 decimal places
//! let encoded = encode_packed_decimal("123.45", 7, 2, true)?;
//! // Result: [0x12, 0x34, 0x5C] (12345 positive)
//! # Ok::<(), copybook_core::Error>(())
//! ```
//!
//! ## See Also
//!
//! - [`crate::zoned_overpunch`] - Zoned decimal overpunch encoding/decoding
//! - [`crate::SmallDecimal`] - Decimal representation without floating-point precision loss
//! - [`crate::memory::ScratchBuffers`] - Reusable buffers for zero-allocation processing

use crate::memory::ScratchBuffers;
use crate::options::{Codepage, FloatFormat, ZonedEncodingFormat};
use crate::zoned_overpunch::{ZeroSignPolicy, encode_overpunch_byte};
use copybook_core::{Error, ErrorCode, Result, SignPlacement, SignSeparateInfo};
use std::convert::TryFrom;
use std::fmt::Write;
use tracing::warn;

/// Nibble zones for ASCII/EBCDIC digits (high bits in zoned bytes).
const ASCII_DIGIT_ZONE: u8 = 0x3; // ASCII '0'..'9' => 0x30..0x39
const EBCDIC_DIGIT_ZONE: u8 = 0xF; // EBCDIC '0'..'9' => 0xF0..0xF9

/// Branch prediction hint for likely-true conditions
///
/// Provides a manual branch prediction hint to the compiler that the condition
/// is likely to be true. This optimization helps keep hot paths efficient by
/// marking the false case as cold.
///
/// # Arguments
/// * `b` - Boolean condition to evaluate
///
/// # Returns
/// The input boolean value unchanged
///
/// # Performance
/// This function is critical for COBOL data decoding hot paths where valid
/// data is the common case and errors are exceptional.
///
/// # Examples
/// ```text
/// use copybook_codec::numeric::likely;
///
/// let valid_data = true;
/// if likely(valid_data) {
///     // Hot path - optimized for this case
/// }
/// ```
#[inline]
pub(crate) fn likely(b: bool) -> bool {
    // CRITICAL PERFORMANCE OPTIMIZATION: Manual branch prediction optimization
    // The true case is expected to be taken most of the time (likely path)
    // Mark the false case as cold to optimize for the common true case
    if b {
        true
    } else {
        cold_branch_hint();
        false
    }
}

/// Branch prediction hint for unlikely-true conditions
///
/// Provides a manual branch prediction hint to the compiler that the condition
/// is unlikely to be true. This optimization keeps error paths cold and hot
/// paths optimized.
///
/// # Arguments
/// * `b` - Boolean condition to evaluate
///
/// # Returns
/// The input boolean value unchanged
///
/// # Performance
/// Critical for error handling in COBOL numeric decoding where validation
/// failures are exceptional cases.
///
/// # Examples
/// ```text
/// use copybook_codec::numeric::unlikely;
///
/// let error_condition = false;
/// if unlikely(error_condition) {
///     // Cold path - marked as unlikely
/// }
/// ```
#[inline]
pub(crate) fn unlikely(b: bool) -> bool {
    // CRITICAL PERFORMANCE OPTIMIZATION: Manual branch prediction optimization
    // Use explicit cold annotation to hint that error paths are unlikely
    // This provides significant speedup by keeping hot paths optimized
    if b {
        // Cold path: mark as unlikely taken
        cold_branch_hint();
        true
    } else {
        false
    }
}

/// Manual branch prediction hint for cold paths
///
/// This function serves as a branch prediction hint that the calling path is cold/unlikely.
/// The `#[cold]` attribute tells the compiler this is an unlikely execution path, and
/// `#[inline(never)]` ensures the cold path doesn't bloat the hot path.
#[cold]
#[inline(never)]
fn cold_branch_hint() {
    // This function serves as a branch prediction hint that the calling path is cold/unlikely
    // The #[cold] attribute tells the compiler this is an unlikely execution path
}

/// Create a normalized `SmallDecimal` from raw components
///
/// Constructs a `SmallDecimal` from value, scale, and sign components, then
/// normalizes it to ensure -0 becomes 0.
///
/// # Arguments
/// * `value` - The unscaled integer value
/// * `scale` - Number of decimal places (positive for fractions, 0 for integers)
/// * `is_negative` - Whether the value is negative
///
/// # Returns
/// A normalized `SmallDecimal` instance
///
/// # Performance
/// This inline function is optimized for hot paths in packed decimal decoding
/// where decimals are frequently constructed from parsed nibbles.
///
/// # Examples
/// ```
/// use copybook_codec::numeric::SmallDecimal;
///
/// // Create 123.45 (value=12345, scale=2)
/// let decimal = SmallDecimal::new(12345, 2, false);
/// assert_eq!(decimal.to_string(), "123.45");
/// ```
#[inline]
fn create_normalized_decimal(value: i64, scale: i16, is_negative: bool) -> SmallDecimal {
    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize();
    decimal
}

/// Statistics collector for encoding analysis
///
/// Tracks the distribution of encoding formats within a zoned decimal field
/// to determine the overall format and detect mixed encoding patterns.
#[derive(Debug, Default)]
#[allow(clippy::struct_field_names)] // Fields are descriptive counters
struct EncodingAnalysisStats {
    ascii_count: usize,
    ebcdic_count: usize,
    invalid_count: usize,
}

impl EncodingAnalysisStats {
    /// Create a new statistics collector
    fn new() -> Self {
        Self::default()
    }

    /// Record a detected format in the statistics
    fn record_format(&mut self, format: Option<ZonedEncodingFormat>) {
        match format {
            Some(ZonedEncodingFormat::Ascii) => self.ascii_count += 1,
            Some(ZonedEncodingFormat::Ebcdic) => self.ebcdic_count += 1,
            Some(ZonedEncodingFormat::Auto) => { /* Should not occur in detection */ }
            None => self.invalid_count += 1,
        }
    }

    /// Determine the overall format and mixed encoding status from collected statistics
    ///
    /// # Logic
    /// - If invalid zones exist: Auto format with mixed encoding flag
    /// - If both ASCII and EBCDIC zones exist: Auto format with mixed encoding flag
    /// - If only ASCII zones: ASCII format, no mixing
    /// - If only EBCDIC zones: EBCDIC format, no mixing
    /// - If no valid zones: Auto format, no mixing (empty or all invalid)
    fn determine_overall_format(&self) -> (ZonedEncodingFormat, bool) {
        if self.invalid_count > 0 {
            // Invalid zones detected - cannot determine format reliably
            (ZonedEncodingFormat::Auto, true)
        } else if self.ascii_count > 0 && self.ebcdic_count > 0 {
            // Mixed ASCII and EBCDIC zones within the same field
            (ZonedEncodingFormat::Auto, true)
        } else if self.ascii_count > 0 {
            // Consistent ASCII encoding throughout the field
            (ZonedEncodingFormat::Ascii, false)
        } else if self.ebcdic_count > 0 {
            // Consistent EBCDIC encoding throughout the field
            (ZonedEncodingFormat::Ebcdic, false)
        } else {
            // No valid zones found (empty field or all invalid)
            (ZonedEncodingFormat::Auto, false)
        }
    }
}

/// Comprehensive encoding detection result for zoned decimal fields
///
/// Provides detailed analysis of zoned decimal encoding patterns within a field,
/// enabling detection of mixed encodings and validation of data consistency.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZonedEncodingInfo {
    /// Overall detected encoding format for the field
    pub detected_format: ZonedEncodingFormat,
    /// True if mixed ASCII/EBCDIC encoding was detected within the field
    pub has_mixed_encoding: bool,
    /// Per-byte encoding detection results for detailed analysis
    pub byte_formats: Vec<Option<ZonedEncodingFormat>>,
}

impl ZonedEncodingInfo {
    /// Create new encoding info with the specified format and mixed encoding status
    ///
    /// # Arguments
    /// * `detected_format` - The overall encoding format determined for the field
    /// * `has_mixed_encoding` - Whether mixed encoding patterns were detected
    #[inline]
    #[must_use]
    pub fn new(detected_format: ZonedEncodingFormat, has_mixed_encoding: bool) -> Self {
        Self {
            detected_format,
            has_mixed_encoding,
            byte_formats: Vec::new(),
        }
    }

    /// Analyze zoned decimal data bytes to detect encoding patterns
    ///
    /// Analyze bytes to identify the zoned encoding mix for downstream encode.
    ///
    /// # Errors
    /// Returns an error if detection fails. In this wave, the implementation
    /// returns `Ok` for supported byte patterns and uses a deterministic fallback
    /// for empty data.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn detect_from_data(data: &[u8]) -> Result<Self> {
        if data.is_empty() {
            return Ok(Self::new(ZonedEncodingFormat::Auto, false));
        }

        let mut byte_formats = Vec::with_capacity(data.len());
        let mut encoding_stats = EncodingAnalysisStats::new();

        // Analyze each byte's zone nibble for encoding patterns
        for &byte in data {
            let format = Self::analyze_zone_nibble(byte);
            byte_formats.push(format);
            encoding_stats.record_format(format);
        }

        // Determine overall encoding and mixed status from statistics
        let (detected_format, has_mixed_encoding) = encoding_stats.determine_overall_format();

        Ok(Self {
            detected_format,
            has_mixed_encoding,
            byte_formats,
        })
    }

    /// Analyze a single byte's zone nibble to determine its encoding format
    ///
    /// # Zone Nibble Analysis
    /// - `0x3`: ASCII digit zone (0x30-0x39 range)
    /// - `0xF`: EBCDIC digit zone (0xF0-0xF9 range)
    /// - ASCII overpunch characters: 0x4X, 0x7B, 0x7D (sign encoding)
    /// - Others: Invalid or non-standard zones
    fn analyze_zone_nibble(byte: u8) -> Option<ZonedEncodingFormat> {
        const ASCII_ZONE: u8 = 0x3;
        const EBCDIC_ZONE: u8 = 0xF;
        const ZONE_MASK: u8 = 0x0F;

        // Check for specific ASCII overpunch characters first
        match byte {
            // ASCII overpunch sign bytes and overpunch characters (A-I, J-R)
            0x7B | 0x7D | 0x41..=0x52 => return Some(ZonedEncodingFormat::Ascii),
            _ => {}
        }

        let zone_nibble = (byte >> 4) & ZONE_MASK;
        match zone_nibble {
            ASCII_ZONE => Some(ZonedEncodingFormat::Ascii),
            EBCDIC_ZONE | 0xC | 0xD => Some(ZonedEncodingFormat::Ebcdic),
            _ => None, // Invalid or mixed zone
        }
    }
}

/// Small decimal structure for parsing/formatting without floats
/// This avoids floating-point precision issues for financial data
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SmallDecimal {
    /// The integer value (unscaled)
    pub value: i64,
    /// The scale (number of decimal places)
    pub scale: i16,
    /// Whether the value is negative
    pub negative: bool,
}

impl SmallDecimal {
    /// Create a new `SmallDecimal`.
    #[inline]
    #[must_use]
    pub fn new(value: i64, scale: i16, negative: bool) -> Self {
        Self {
            value,
            scale,
            negative,
        }
    }

    /// Create a zero value with the given scale
    #[inline]
    #[must_use]
    pub fn zero(scale: i16) -> Self {
        Self {
            value: 0,
            scale,
            negative: false,
        }
    }

    /// Normalize -0 to 0 (NORMATIVE)
    #[inline]
    pub fn normalize(&mut self) {
        if self.value == 0 {
            self.negative = false;
        }
    }

    /// Format as string with fixed scale (NORMATIVE)
    ///
    /// Always render with exactly `scale` digits after decimal point.
    #[allow(clippy::inherent_to_string)] // Intentional - this is a specific numeric formatting
    #[inline]
    #[must_use = "Use the formatted string output"]
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        self.append_sign_if_negative(&mut result);
        self.append_formatted_value(&mut result);
        result
    }

    /// Check if this decimal represents a zero value
    fn is_zero_value(&self) -> bool {
        self.value == 0
    }

    /// Append negative sign to the result if the value is negative and non-zero
    fn append_sign_if_negative(&self, result: &mut String) {
        if self.negative && !self.is_zero_value() {
            result.push('-');
        }
    }

    /// Append the formatted numeric value (integer or decimal) to the result
    fn append_formatted_value(&self, result: &mut String) {
        if self.scale <= 0 {
            self.append_integer_format(result);
        } else {
            self.append_decimal_format(result);
        }
    }

    /// Append integer format or scale extension to the result
    fn append_integer_format(&self, result: &mut String) {
        let scaled_value = if self.scale < 0 {
            // Scale extension: multiply by 10^(-scale)
            self.value * 10_i64.pow(scale_abs_to_u32(self.scale))
        } else {
            // Normal integer format (scale = 0)
            self.value
        };
        // Writing to String should not fail here; keep the deterministic fallback
        // path for non-panicking behavior if it ever occurs.
        if write!(result, "{scaled_value}").is_err() {
            // Deterministic fallback path: emit the explicit "ERR" marker.
            result.push_str("ERR");
        }
    }

    /// Append decimal format with exactly `scale` digits after decimal point
    fn append_decimal_format(&self, result: &mut String) {
        let divisor = 10_i64.pow(scale_abs_to_u32(self.scale));
        let integer_part = self.value / divisor;
        let fractional_part = self.value % divisor;

        // Writing to String should not fail here; keep the deterministic fallback
        // path for non-panicking behavior if it ever occurs.
        let width = usize::try_from(self.scale).unwrap_or_else(|_| {
            debug_assert!(false, "scale should be positive when formatting decimal");
            0
        });

        if write!(result, "{integer_part}.{fractional_part:0width$}").is_err() {
            // Deterministic fallback path: emit the explicit "ERR" marker.
            result.push_str("ERR");
        }
    }

    /// Parse a decimal string into a `SmallDecimal` using the expected scale.
    ///
    /// # Errors
    /// Returns an error when the text violates the expected numeric format.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn from_str(s: &str, expected_scale: i16) -> Result<Self> {
        let trimmed = s.trim();
        if trimmed.is_empty() {
            return Ok(Self::zero(expected_scale));
        }

        let (negative, numeric_part) = Self::extract_sign(trimmed);

        if let Some(dot_pos) = numeric_part.find('.') {
            Self::parse_decimal_format(numeric_part, dot_pos, expected_scale, negative)
        } else {
            Self::parse_integer_format(numeric_part, expected_scale, negative)
        }
    }

    /// Extract sign information from the numeric string
    ///
    /// Returns (`is_negative`, `numeric_part_without_sign`).
    fn extract_sign(s: &str) -> (bool, &str) {
        if let Some(without_minus) = s.strip_prefix('-') {
            (true, without_minus)
        } else {
            (false, s)
        }
    }

    /// Parse decimal format string (contains decimal point)
    fn parse_decimal_format(
        numeric_part: &str,
        dot_pos: usize,
        expected_scale: i16,
        negative: bool,
    ) -> Result<Self> {
        let integer_part = &numeric_part[..dot_pos];
        let fractional_part = &numeric_part[dot_pos + 1..];

        // Validate scale matches exactly (NORMATIVE)
        let expected_len = usize::try_from(expected_scale).map_err(|_| {
            Error::new(
                ErrorCode::CBKE505_SCALE_MISMATCH,
                format!(
                    "Scale mismatch: expected {expected_scale} decimal places, got {}",
                    fractional_part.len()
                ),
            )
        })?;

        if fractional_part.len() != expected_len {
            return Err(Error::new(
                ErrorCode::CBKE505_SCALE_MISMATCH,
                format!(
                    "Scale mismatch: expected {expected_scale} decimal places, got {}",
                    fractional_part.len()
                ),
            ));
        }

        let integer_value = Self::parse_integer_component(integer_part)?;
        let fractional_value = Self::parse_integer_component(fractional_part)?;
        let total_value =
            Self::combine_integer_and_fractional(integer_value, fractional_value, expected_scale)?;

        let mut result = Self::new(total_value, expected_scale, negative);
        result.normalize();
        Ok(result)
    }

    /// Parse integer format string (no decimal point)
    fn parse_integer_format(
        numeric_part: &str,
        expected_scale: i16,
        negative: bool,
    ) -> Result<Self> {
        // Defense-in-depth: accept bare "0" for scaled fields (e.g., user-provided JSON)
        if expected_scale != 0 {
            let value = Self::parse_integer_component(numeric_part)?;
            if value == 0 {
                let mut result = Self::new(0, expected_scale, negative);
                result.normalize();
                return Ok(result);
            }
            return Err(Error::new(
                ErrorCode::CBKE505_SCALE_MISMATCH,
                format!("Scale mismatch: expected {expected_scale} decimal places, got integer"),
            ));
        }

        let value = Self::parse_integer_component(numeric_part)?;
        let mut result = Self::new(value, expected_scale, negative);
        result.normalize();
        Ok(result)
    }

    /// Parse a string component as an integer with error handling
    fn parse_integer_component(s: &str) -> Result<i64> {
        s.parse().map_err(|_| {
            Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Invalid numeric component: '{s}'"),
            )
        })
    }

    /// Combine integer and fractional parts into a scaled value
    fn combine_integer_and_fractional(
        integer_value: i64,
        fractional_value: i64,
        scale: i16,
    ) -> Result<i64> {
        let divisor = 10_i64.pow(scale_abs_to_u32(scale));
        integer_value
            .checked_mul(divisor)
            .and_then(|v| v.checked_add(fractional_value))
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKE510_NUMERIC_OVERFLOW,
                    "Numeric value too large - would cause overflow",
                )
            })
    }

    /// Format as string with fixed scale (NORMATIVE)
    /// Always render with exactly `scale` digits after decimal
    #[inline]
    #[must_use]
    pub fn to_fixed_scale_string(&self, scale: i16) -> String {
        let mut result = String::new();

        if self.negative && self.value != 0 {
            result.push('-');
        }

        if scale <= 0 {
            // Integer format (scale=0) or scale extension
            let scaled_value = if scale < 0 {
                self.value * 10_i64.pow(scale_abs_to_u32(scale))
            } else {
                self.value
            };
            // Writing to String should never fail, but handle gracefully for panic elimination
            if write!(result, "{scaled_value}").is_err() {
                result.push_str("ERR");
            }
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(scale_abs_to_u32(scale));
            let integer_part = self.value / divisor;
            let fractional_part = self.value % divisor;

            // Writing to String should never fail, but handle gracefully for panic elimination
            let width = usize::try_from(scale).unwrap_or_else(|_| {
                debug_assert!(false, "scale should be positive in decimal formatting");
                0
            });

            if write!(result, "{integer_part}.{fractional_part:0width$}").is_err() {
                result.push_str("ERR");
            }
        }

        result
    }

    /// High-performance format using scratch buffer (zero-allocation optimization)
    /// CRITICAL for COMP-3 JSON conversion performance
    #[inline]
    pub fn format_to_scratch_buffer(&self, scale: i16, scratch_buffer: &mut String) {
        scratch_buffer.clear();

        if self.negative && self.value != 0 {
            scratch_buffer.push('-');
        }

        if scale <= 0 {
            // Integer format (scale=0) or scale extension
            let scaled_value = if scale < 0 {
                self.value * 10_i64.pow(scale_abs_to_u32(scale))
            } else {
                self.value
            };
            // CRITICAL OPTIMIZATION: Manual integer formatting to avoid write!() overhead
            Self::format_integer_manual(scaled_value, scratch_buffer);
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(scale_abs_to_u32(scale));
            let integer_part = self.value / divisor;
            let fractional_part = self.value % divisor;

            // CRITICAL OPTIMIZATION: Manual decimal formatting to avoid write!() overhead
            Self::format_integer_manual(integer_part, scratch_buffer);
            scratch_buffer.push('.');
            Self::format_integer_with_leading_zeros(
                fractional_part,
                scale_abs_to_u32(scale),
                scratch_buffer,
            );
        }
    }

    /// Ultra-fast manual integer formatting to avoid write!() macro overhead
    #[inline]
    fn format_integer_manual(mut value: i64, buffer: &mut String) {
        if value == 0 {
            buffer.push('0');
            return;
        }

        // Optimized formatting with fewer divisions for common cases
        if value < 100 {
            // Fast path for 1-2 digit numbers (very common in COMP-3)
            if value < 10 {
                push_digit(buffer, value);
            } else {
                let tens = value / 10;
                let ones = value % 10;
                push_digit(buffer, tens);
                push_digit(buffer, ones);
            }
            return;
        }

        // Use a small stack buffer for digits for larger numbers
        let mut digits = [0u8; 20]; // More than enough for i64::MAX
        let mut count = 0;

        // Safe: i64::MAX has 19 digits, array has 20 elements
        while value > 0 && count < 20 {
            digits[count] = digit_from_value(value % 10);
            value /= 10;
            count += 1;
        }

        // Add digits in reverse order
        for i in (0..count).rev() {
            buffer.push(char::from(b'0' + digits[i]));
        }
    }

    /// Ultra-fast manual integer formatting with leading zeros
    #[inline]
    fn format_integer_with_leading_zeros(mut value: i64, width: u32, buffer: &mut String) {
        // Optimized for common small widths (most COMP-3 scales are 0-4)
        if width <= 4 && value < 10000 {
            match width {
                1 => {
                    push_digit(buffer, value);
                }
                2 => {
                    push_digit(buffer, value / 10);
                    push_digit(buffer, value % 10);
                }
                3 => {
                    push_digit(buffer, value / 100);
                    push_digit(buffer, (value / 10) % 10);
                    push_digit(buffer, value % 10);
                }
                4 => {
                    push_digit(buffer, value / 1000);
                    push_digit(buffer, (value / 100) % 10);
                    push_digit(buffer, (value / 10) % 10);
                    push_digit(buffer, value % 10);
                }
                _ => {}
            }
            return;
        }

        // General case for larger widths
        let mut digits = [0u8; 20]; // More than enough for i64::MAX
        let mut count = 0;
        // Clamp target_width to array size to prevent out-of-bounds access
        let target_width = usize::try_from(width).unwrap_or(usize::MAX).min(20);

        // Extract digits (safe: i64::MAX has at most 19 digits, array has 20 elements)
        loop {
            digits[count] = digit_from_value(value % 10);
            value /= 10;
            count += 1;
            // Safety: count is bounded by min(19, target_width) where target_width <= 20
            if value == 0 && count >= target_width {
                break;
            }
            if count >= 20 {
                // Defensive: should never happen for i64, but prevents any overflow
                break;
            }
        }

        // Pad with leading zeros if needed (count is guaranteed <= 20)
        while count < target_width {
            digits[count] = 0;
            count += 1;
        }

        // Add digits in reverse order
        for i in (0..count).rev() {
            buffer.push(char::from(b'0' + digits[i]));
        }
    }

    /// Get the scale of this decimal
    #[inline]
    #[must_use]
    pub fn scale(&self) -> i16 {
        self.scale
    }

    /// Check if this decimal is negative
    #[inline]
    #[must_use]
    pub fn is_negative(&self) -> bool {
        self.negative && self.value != 0
    }

    /// Get the total number of digits in this decimal
    #[inline]
    #[must_use]
    pub fn total_digits(&self) -> u16 {
        if self.value == 0 {
            return 1;
        }

        let mut count = 0;
        let mut val = self.value.abs();
        while val > 0 {
            count += 1;
            val /= 10;
        }
        count
    }
}

/// Convert an integer value to a single digit (0-9)
///
/// Validates that the value is in the range 0-9 and converts it to a u8 digit.
/// Returns 0 for invalid inputs (with debug assertion).
///
/// # Arguments
/// * `value` - Integer value to convert (expected to be 0-9)
///
/// # Returns
/// A single digit as u8, or 0 if the value is out of range
///
/// # Safety
/// This function includes a debug assertion that fires if the value is out of
/// range. In release builds, invalid values return 0.
///
/// # Performance
/// Used in hot paths for packed decimal encoding where digits are extracted
/// from numeric values.
#[inline]
fn digit_from_value(value: i64) -> u8 {
    match u8::try_from(value) {
        Ok(digit) if digit <= 9 => digit,
        _ => {
            debug_assert!(false, "digit out of range: {value}");
            0
        }
    }
}

/// Push a single digit character to a string buffer
///
/// Converts an integer digit (0-9) to its ASCII character representation and
/// appends it to the buffer.
///
/// # Arguments
/// * `buffer` - String buffer to append to
/// * `digit` - Integer digit value (0-9)
///
/// # Performance
/// Inline function optimized for decimal formatting hot paths in COMP-3 decoding.
#[inline]
fn push_digit(buffer: &mut String, digit: i64) {
    buffer.push(char::from(b'0' + digit_from_value(digit)));
}

/// Convert absolute scale value to u32 for power calculations
///
/// Takes the absolute value of a scale (i16) and converts it to u32 for use
/// in power-of-10 calculations.
///
/// # Arguments
/// * `scale` - Scale value (can be negative for integer extensions)
///
/// # Returns
/// Absolute value of scale as u32
///
/// # Examples
/// ```
/// # fn scale_abs_to_u32(scale: i16) -> u32 { u32::from(scale.unsigned_abs()) }
/// assert_eq!(scale_abs_to_u32(2), 2);
/// assert_eq!(scale_abs_to_u32(-2), 2);
/// assert_eq!(scale_abs_to_u32(0), 0);
/// ```
#[inline]
fn scale_abs_to_u32(scale: i16) -> u32 {
    u32::from(scale.unsigned_abs())
}

/// Decode a zoned decimal using the configured code page with detailed error context.
///
/// Decodes zoned decimal (PIC 9) fields where each digit is stored in a byte
/// with a zone nibble and a digit nibble. The sign may be encoded via overpunch
/// in the last byte's zone nibble.
///
/// # Arguments
/// * `data` - Raw byte data containing the zoned decimal
/// * `digits` - Number of digit characters (field length)
/// * `scale` - Number of decimal places (can be negative for scaling)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
/// * `codepage` - Character encoding (ASCII or EBCDIC variant)
/// * `blank_when_zero` - If true, all-space fields decode as zero
///
/// # Returns
/// A `SmallDecimal` containing the decoded value
///
/// # Policy
/// Applies the codec default: ASCII uses `ZeroSignPolicy::Positive`; EBCDIC zeros normalize via `ZeroSignPolicy::Preferred`.
///
/// # Errors
/// Returns an error if the zoned decimal data is invalid or contains bad sign zones.
/// All errors include proper context information (`record_index`, `field_path`, `byte_offset`).
///
/// # Examples
///
/// ## ASCII Zoned Decimal
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // ASCII "123" = [0x31, 0x32, 0x33]
/// let data = b"123";
/// let result = decode_zoned_decimal(data, 3, 0, false, Codepage::ASCII, false)?;
/// assert_eq!(result.to_string(), "123");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Signed ASCII Zoned Decimal (Overpunch)
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // ASCII "-123" with overpunch: [0x31, 0x32, 0x4D] (M = 3 with negative sign)
/// let data = [0x31, 0x32, 0x4D];
/// let result = decode_zoned_decimal(&data, 3, 0, true, Codepage::ASCII, false)?;
/// assert_eq!(result.to_string(), "-123");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## EBCDIC Zoned Decimal
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // EBCDIC "123" = [0xF1, 0xF2, 0xF3]
/// let data = [0xF1, 0xF2, 0xF3];
/// let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::CP037, false)?;
/// assert_eq!(result.to_string(), "123");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Decimal Scale
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // "12.34" with 2 decimal places
/// let data = b"1234";
/// let result = decode_zoned_decimal(data, 4, 2, false, Codepage::ASCII, false)?;
/// assert_eq!(result.to_string(), "12.34");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## BLANK WHEN ZERO
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // All spaces decode as zero when blank_when_zero is true
/// let data = b"   ";
/// let result = decode_zoned_decimal(data, 3, 0, false, Codepage::ASCII, true)?;
/// assert_eq!(result.to_string(), "0");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`decode_zoned_decimal_sign_separate`] - For SIGN SEPARATE fields
/// * [`decode_zoned_decimal_with_encoding`] - For encoding detection
/// * [`encode_zoned_decimal`] - For encoding zoned decimals
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_zoned_decimal(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
) -> Result<SmallDecimal> {
    if unlikely(data.len() != usize::from(digits)) {
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "Zoned decimal data length mismatch".to_string(),
        ));
    }

    // Check for BLANK WHEN ZERO (all spaces)
    let is_all_spaces = data.iter().all(|&b| {
        match codepage {
            Codepage::ASCII => b == b' ',
            _ => b == 0x40, // EBCDIC space
        }
    });

    if is_all_spaces {
        if blank_when_zero {
            warn!("CBKD412_ZONED_BLANK_IS_ZERO: Zoned field is blank, decoding as zero");
            // Track this warning in RunSummary
            crate::lib_api::increment_warning_counter();
            return Ok(SmallDecimal::zero(scale));
        }
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "Zoned field contains all spaces but BLANK WHEN ZERO not specified",
        ));
    }

    let mut value = 0i64;
    let mut is_negative = false;
    let expected_zone = match codepage {
        Codepage::ASCII => ASCII_DIGIT_ZONE,
        _ => EBCDIC_DIGIT_ZONE,
    };

    for (i, &byte) in data.iter().enumerate() {
        if i == data.len() - 1 {
            let (digit, negative) = crate::zoned_overpunch::decode_overpunch_byte(byte, codepage)?;

            if signed {
                is_negative = negative;
            } else {
                let zone = (byte >> 4) & 0x0F;
                let zone_label = match codepage {
                    Codepage::ASCII => "ASCII",
                    _ => "EBCDIC",
                };
                if zone != expected_zone {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        format!(
                            "Unsigned {zone_label} zoned decimal cannot contain sign zone 0x{zone:X} in last byte"
                        ),
                    ));
                }
                if negative {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        "Unsigned zoned decimal contains negative overpunch",
                    ));
                }
            }

            value = value.saturating_mul(10).saturating_add(i64::from(digit));
        } else {
            let zone = (byte >> 4) & 0x0F;
            let digit = byte & 0x0F;

            if digit > 9 {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid digit nibble 0x{digit:X} at position {i}"),
                ));
            }

            if zone != expected_zone {
                let zone_label = match codepage {
                    Codepage::ASCII => "ASCII",
                    _ => "EBCDIC",
                };
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!(
                        "Invalid {zone_label} zone 0x{zone:X} at position {i}, expected 0x{expected_zone:X}"
                    ),
                ));
            }

            value = value.saturating_mul(10).saturating_add(i64::from(digit));
        }
    }

    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize(); // Normalize -0 → 0 (NORMATIVE)
    Ok(decimal)
}

/// Decode a zoned decimal field with SIGN SEPARATE clause
///
/// SIGN SEPARATE stores the sign in a separate byte rather than overpunching
/// it in the zone portion of the last digit. The sign byte can be leading
/// (before digits) or trailing (after digits).
///
/// # Arguments
/// * `data` - Raw byte data (includes sign byte + digit bytes)
/// * `digits` - Number of digit characters (not including sign byte)
/// * `scale` - Decimal places (can be negative for scaling)
/// * `sign_separate` - SIGN SEPARATE clause information (placement)
/// * `codepage` - Character encoding (ASCII or EBCDIC)
///
/// # Returns
/// A `SmallDecimal` containing the decoded value
///
/// # Errors
/// Returns an error if data length is incorrect or sign byte is invalid.
///
/// # Examples
///
/// ## Leading Sign (ASCII)
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal_sign_separate};
/// use copybook_codec::options::Codepage;
/// use copybook_core::SignPlacement;
/// use copybook_core::SignSeparateInfo;
///
/// // "+123" with leading sign: [0x2B, 0x31, 0x32, 0x33]
/// let sign_info = SignSeparateInfo { placement: SignPlacement::Leading };
/// let data = [b'+', b'1', b'2', b'3'];
/// let result = decode_zoned_decimal_sign_separate(&data, 3, 0, &sign_info, Codepage::ASCII)?;
/// assert_eq!(result.to_string(), "123");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Trailing Sign (ASCII)
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal_sign_separate};
/// use copybook_codec::options::Codepage;
/// use copybook_core::SignPlacement;
/// use copybook_core::SignSeparateInfo;
///
/// // "-456" with trailing sign: [0x34, 0x35, 0x36, 0x2D]
/// let sign_info = SignSeparateInfo { placement: SignPlacement::Trailing };
/// let data = [b'4', b'5', b'6', b'-'];
/// let result = decode_zoned_decimal_sign_separate(&data, 3, 0, &sign_info, Codepage::ASCII)?;
/// assert_eq!(result.to_string(), "-456");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## EBCDIC Leading Sign
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal_sign_separate};
/// use copybook_codec::options::Codepage;
/// use copybook_core::SignPlacement;
/// use copybook_core::SignSeparateInfo;
///
/// // "+789" with leading EBCDIC sign: [0x4E, 0xF7, 0xF8, 0xF9]
/// let sign_info = SignSeparateInfo { placement: SignPlacement::Leading };
/// let data = [0x4E, 0xF7, 0xF8, 0xF9];
/// let result = decode_zoned_decimal_sign_separate(&data, 3, 0, &sign_info, Codepage::CP037)?;
/// assert_eq!(result.to_string(), "789");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`decode_zoned_decimal`] - For overpunch-encoded zoned decimals
/// * [`decode_zoned_decimal_with_encoding`] - For encoding detection
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_zoned_decimal_sign_separate(
    data: &[u8],
    digits: u16,
    scale: i16,
    sign_separate: &SignSeparateInfo,
    codepage: Codepage,
) -> Result<SmallDecimal> {
    // SIGN SEPARATE adds 1 byte for the sign
    let expected_len = usize::from(digits) + 1;

    if unlikely(data.len() != expected_len) {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!(
                "SIGN SEPARATE zoned decimal data length mismatch: expected {} bytes, got {}",
                expected_len,
                data.len()
            ),
        ));
    }

    // Determine sign byte and digit bytes based on placement
    let (sign_byte, digit_bytes) = match sign_separate.placement {
        SignPlacement::Leading => {
            // Sign byte is first, digits follow
            if data.is_empty() {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    "SIGN SEPARATE field is empty",
                ));
            }
            (data[0], &data[1..])
        }
        SignPlacement::Trailing => {
            // Digits are first, sign byte is last
            if data.is_empty() {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    "SIGN SEPARATE field is empty",
                ));
            }
            (data[data.len() - 1], &data[..data.len() - 1])
        }
    };

    // Decode sign byte

    let is_negative = if codepage.is_ascii() {
        match sign_byte {
            b'-' => true,

            b'+' | b' ' | b'0' => false, // Space or zero means positive/unsigned

            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid sign byte in SIGN SEPARATE field: 0x{sign_byte:02X} (ASCII)"),
                ));
            }
        }
    } else {
        // EBCDIC codepage (CP037, CP273, CP500, CP1047, CP1140)

        match sign_byte {
            0x60 => true, // EBCDIC '-'

            0x4E | 0x40 | 0xF0 => false, // Space or zero means positive/unsigned

            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid sign byte in SIGN SEPARATE field: 0x{sign_byte:02X} (EBCDIC)"),
                ));
            }
        }
    };

    // Decode digit bytes

    let mut value: i64 = 0;

    for &byte in digit_bytes {
        let digit = if codepage.is_ascii() {
            if !byte.is_ascii_digit() {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    format!("Invalid digit byte in SIGN SEPARATE field: 0x{byte:02X} (ASCII)"),
                ));
            }

            byte - b'0'
        } else {
            // EBCDIC digits are 0xF0-0xF9

            if !(0xF0..=0xF9).contains(&byte) {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    format!("Invalid digit byte in SIGN SEPARATE field: 0x{byte:02X} (EBCDIC)"),
                ));
            }

            byte - 0xF0
        };

        value = value
            .checked_mul(10)
            .and_then(|v| v.checked_add(i64::from(digit)))
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKD410_ZONED_OVERFLOW,
                    format!("SIGN SEPARATE zoned decimal value overflow for {digits} digits"),
                )
            })?;
    }

    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize(); // Normalize -0 → 0 (NORMATIVE)
    Ok(decimal)
}

/// Encode a zoned decimal value with SIGN SEPARATE clause.
///
/// The SIGN SEPARATE clause places the sign character in a separate byte
/// (leading or trailing) rather than overpunching the last digit.
///
/// Total encoded length = digits + 1 (for the separate sign byte).
///
/// # Arguments
/// * `value` - String representation of the numeric value (e.g., "123", "-456.78")
/// * `digits` - Number of digit positions in the field
/// * `scale` - Number of implied decimal places
/// * `sign_separate` - Sign placement information (leading or trailing)
/// * `codepage` - Character encoding (determines sign byte encoding)
/// * `buffer` - Output buffer (must be at least digits + 1 bytes)
///
/// # Errors
/// Returns `CBKE530_SIGN_SEPARATE_ENCODE_ERROR` if the value cannot be encoded.
#[inline]
pub fn encode_zoned_decimal_sign_separate(
    value: &str,
    digits: u16,
    scale: i16,
    sign_separate: &SignSeparateInfo,
    codepage: Codepage,
    buffer: &mut [u8],
) -> Result<()> {
    let expected_len = usize::from(digits) + 1;
    if buffer.len() < expected_len {
        return Err(Error::new(
            ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
            format!(
                "SIGN SEPARATE encode buffer too small: need {expected_len} bytes, got {}",
                buffer.len()
            ),
        ));
    }

    // Parse value string to determine sign and digit characters
    let trimmed = value.trim();
    let (is_negative, abs_str) = if let Some(rest) = trimmed.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = trimmed.strip_prefix('+') {
        (false, rest)
    } else {
        (false, trimmed)
    };

    // Validate input characters before scaling
    for ch in abs_str.chars() {
        if !ch.is_ascii_digit() && ch != '.' {
            return Err(Error::new(
                ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
                format!("Unexpected character '{ch}' in numeric value '{value}'"),
            ));
        }
    }

    // Structural validation: reject ambiguous/empty numeric input
    let dot_count = abs_str.chars().filter(|&c| c == '.').count();
    let digit_count = abs_str.chars().filter(char::is_ascii_digit).count();

    if digit_count == 0 {
        return Err(Error::new(
            ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
            format!("No digits found in numeric value '{value}'"),
        ));
    }
    if dot_count > 1 {
        return Err(Error::new(
            ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
            format!("Multiple decimal points in numeric value '{value}'"),
        ));
    }
    if scale <= 0 && dot_count == 1 {
        return Err(Error::new(
            ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
            format!("Unexpected decimal point for scale {scale} in value '{value}'"),
        ));
    }

    // Build the scaled digit string
    let scaled = build_scaled_digit_string(abs_str, scale);

    // Pad with leading zeros or truncate to match digit count
    let digits_usize = usize::from(digits);
    let padded = match scaled.len().cmp(&digits_usize) {
        std::cmp::Ordering::Less => {
            format!("{scaled:0>digits_usize$}")
        }
        std::cmp::Ordering::Greater => {
            return Err(Error::new(
                ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
                format!(
                    "SIGN SEPARATE overflow: value requires {} digits but field allows {}",
                    scaled.len(),
                    digits_usize
                ),
            ));
        }
        std::cmp::Ordering::Equal => scaled,
    };

    // Determine sign byte and digit encoding based on codepage
    let (sign_byte, digit_base): (u8, u8) = if codepage.is_ascii() {
        (if is_negative { b'-' } else { b'+' }, b'0')
    } else {
        // EBCDIC codepages
        (if is_negative { 0x60 } else { 0x4E }, 0xF0)
    };

    // Write digits to buffer
    let digit_offset = match sign_separate.placement {
        SignPlacement::Leading => {
            buffer[0] = sign_byte;
            1
        }
        SignPlacement::Trailing => 0,
    };

    for (i, byte) in padded.bytes().enumerate() {
        let digit = byte.wrapping_sub(b'0');
        if digit > 9 {
            return Err(Error::new(
                ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR,
                format!("Invalid digit byte 0x{byte:02X} in value"),
            ));
        }
        buffer[digit_offset + i] = digit_base + digit;
    }

    if matches!(sign_separate.placement, SignPlacement::Trailing) {
        buffer[digits_usize] = sign_byte;
    }

    Ok(())
}

/// Build a digit string scaled to the given number of decimal places.
///
/// Splits the absolute value string at the decimal point (if present),
/// pads or truncates the fractional part to `scale` digits, and concatenates.
fn build_scaled_digit_string(abs_str: &str, scale: i16) -> String {
    // Extract only digit characters (ignoring other formatting)
    let digit_str: String = abs_str.chars().filter(char::is_ascii_digit).collect();

    if scale <= 0 {
        return digit_str;
    }

    let scale_usize = usize::try_from(scale).unwrap_or(0);
    let (integer_part, fractional_part) = if let Some(pos) = abs_str.find('.') {
        (&abs_str[..pos], &abs_str[pos + 1..])
    } else {
        (abs_str, "")
    };

    let int_digits: String = integer_part.chars().filter(char::is_ascii_digit).collect();
    let frac_digits: String = fractional_part
        .chars()
        .filter(char::is_ascii_digit)
        .collect();

    // Pad or truncate fractional part to match scale
    let padded_frac = if frac_digits.len() >= scale_usize {
        frac_digits[..scale_usize].to_string()
    } else {
        format!("{frac_digits:0<scale_usize$}")
    };
    format!("{int_digits}{padded_frac}")
}

/// Decode zoned decimal field with encoding detection and preservation
///
/// Returns both the decoded decimal and encoding information for preservation.
/// When `preserve_encoding` is true, analyzes the input data to detect
/// whether it uses ASCII or EBCDIC encoding, and whether mixed encodings
/// are present within the field.
///
/// # Arguments
/// * `data` - Raw byte data containing the zoned decimal
/// * `digits` - Number of digit characters (field length)
/// * `scale` - Number of decimal places (can be negative for scaling)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
/// * `codepage` - Character encoding (ASCII or EBCDIC variant)
/// * `blank_when_zero` - If true, all-space fields decode as zero
/// * `preserve_encoding` - If true, detect and return encoding information
///
/// # Returns
/// A tuple of (`SmallDecimal`, `Option<ZonedEncodingInfo>`) containing:
/// - The decoded decimal value
/// - Encoding information (if `preserve_encoding` was true)
///
/// # Policy
/// Mirrors [`decode_zoned_decimal`], defaulting to preferred-zero handling for EBCDIC unless a preserved format dictates otherwise.
///
/// # Errors
/// Returns an error if the zoned decimal data is invalid or contains bad sign zones.
/// All errors include proper context information (`record_index`, `field_path`, `byte_offset`).
///
/// # Examples
///
/// ## Basic Decoding Without Preservation
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal_with_encoding};
/// use copybook_codec::options::Codepage;
///
/// let data = b"123";
/// let (decimal, encoding_info) = decode_zoned_decimal_with_encoding(
///     data, 3, 0, false, Codepage::ASCII, false, false
/// )?;
/// assert_eq!(decimal.to_string(), "123");
/// assert!(encoding_info.is_none());
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Encoding Detection
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal_with_encoding};
/// use copybook_codec::options::Codepage;
/// use copybook_codec::options::ZonedEncodingFormat;
///
/// let data = b"123";
/// let (decimal, encoding_info) = decode_zoned_decimal_with_encoding(
///     data, 3, 0, false, Codepage::ASCII, false, true
/// )?;
/// assert_eq!(decimal.to_string(), "123");
/// let info = encoding_info.unwrap();
/// assert_eq!(info.detected_format, ZonedEncodingFormat::Ascii);
/// assert!(!info.has_mixed_encoding);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`decode_zoned_decimal`] - For basic zoned decimal decoding
/// * [`ZonedEncodingInfo`] - For encoding detection results
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_zoned_decimal_with_encoding(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
    preserve_encoding: bool,
) -> Result<(SmallDecimal, Option<ZonedEncodingInfo>)> {
    if data.len() != usize::from(digits) {
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            format!(
                "Zoned decimal data length {} doesn't match digits {}",
                data.len(),
                digits
            ),
        ));
    }

    // Check for BLANK WHEN ZERO (all spaces)
    let is_all_spaces = data.iter().all(|&b| {
        match codepage {
            Codepage::ASCII => b == b' ',
            _ => b == 0x40, // EBCDIC space
        }
    });

    if is_all_spaces {
        if blank_when_zero {
            warn!("CBKD412_ZONED_BLANK_IS_ZERO: Zoned field is blank, decoding as zero");
            crate::lib_api::increment_warning_counter();
            return Ok((SmallDecimal::zero(scale), None));
        }
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "Zoned field contains all spaces but BLANK WHEN ZERO not specified",
        ));
    }

    // Detect encoding if preservation is enabled
    let encoding_info = if preserve_encoding {
        Some(ZonedEncodingInfo::detect_from_data(data)?)
    } else {
        None
    };

    // Check for mixed encoding error
    if let Some(ref info) = encoding_info
        && info.has_mixed_encoding
    {
        return Err(Error::new(
            ErrorCode::CBKD414_ZONED_MIXED_ENCODING,
            "Mixed ASCII/EBCDIC encoding detected within zoned decimal field",
        ));
    }

    let (value, is_negative) =
        zoned_decode_digits_with_encoding(data, signed, codepage, preserve_encoding)?;

    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize(); // Normalize -0 → 0 (NORMATIVE)
    Ok((decimal, encoding_info))
}

/// Internal helper to decode zoned decimal digits with encoding detection
///
/// This function handles the core logic of iterating through zoned decimal bytes,
/// accumulating the numeric value, and optionally detecting/validating the encoding.
///
/// # Arguments
/// * `data` - Raw byte data containing the zoned decimal
/// * `signed` - Whether the field is signed
/// * `codepage` - Character encoding (ASCII or EBCDIC variant)
/// * `preserve_encoding` - If true, validate consistent encoding throughout the field
///
/// # Returns
/// A tuple of (`accumulated_value`, `is_negative`)
///
/// # Errors
/// Returns an error if an invalid digit or zone nibble is encountered.
#[inline]
fn zoned_decode_digits_with_encoding(
    data: &[u8],
    signed: bool,
    codepage: Codepage,
    preserve_encoding: bool,
) -> Result<(i64, bool)> {
    let mut value = 0i64;
    let mut is_negative = false;

    for (index, &byte) in data.iter().enumerate() {
        let zone = (byte >> 4) & 0x0F;

        if index == data.len() - 1 {
            let (digit, negative) = crate::zoned_overpunch::decode_overpunch_byte(byte, codepage)?;

            if signed {
                is_negative = negative;
            } else {
                let zone_valid = if preserve_encoding {
                    matches!(zone, 0x3 | 0xF)
                } else {
                    match codepage {
                        Codepage::ASCII => zone == 0x3,
                        _ => zone == 0xF,
                    }
                };

                if !zone_valid {
                    let message = if preserve_encoding {
                        format!(
                            "Invalid zone 0x{zone:X} in unsigned zoned decimal, expected 0x3 (ASCII) or 0xF (EBCDIC)"
                        )
                    } else {
                        let zone_label = zoned_zone_label(codepage);
                        format!(
                            "Unsigned {zone_label} zoned decimal cannot contain sign zone 0x{zone:X} in last byte"
                        )
                    };
                    let code = if preserve_encoding {
                        ErrorCode::CBKD413_ZONED_INVALID_ENCODING
                    } else {
                        ErrorCode::CBKD411_ZONED_BAD_SIGN
                    };
                    return Err(Error::new(code, message));
                }

                if negative {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        "Unsigned zoned decimal contains negative overpunch",
                    ));
                }
            }

            value = value.saturating_mul(10).saturating_add(i64::from(digit));
        } else {
            let digit = byte & 0x0F;
            if digit > 9 {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid digit nibble 0x{digit:X} at position {index}"),
                ));
            }

            if preserve_encoding {
                match zone {
                    0x3 | 0xF => {}
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKD413_ZONED_INVALID_ENCODING,
                            format!(
                                "Invalid zone 0x{zone:X} at position {index}, expected 0x3 (ASCII) or 0xF (EBCDIC)"
                            ),
                        ));
                    }
                }
            } else {
                match codepage {
                    Codepage::ASCII => {
                        if zone != 0x3 {
                            return Err(Error::new(
                                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                                format!(
                                    "Invalid ASCII zone 0x{zone:X} at position {index}, expected 0x3"
                                ),
                            ));
                        }
                    }
                    _ => {
                        if zone != 0xF {
                            return Err(Error::new(
                                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                                format!(
                                    "Invalid EBCDIC zone 0x{zone:X} at position {index}, expected 0xF"
                                ),
                            ));
                        }
                    }
                }
            }

            value = value.saturating_mul(10).saturating_add(i64::from(digit));
        }
    }

    Ok((value, is_negative))
}

/// Decode packed decimal (COMP-3) field with comprehensive error context
///
/// Decodes COMP-3 packed decimal format where each byte contains two decimal
/// digits (nibbles), with the last nibble containing the sign. This function
/// uses optimized fast paths for common enterprise data patterns.
///
/// # Arguments
/// * `data` - Raw byte data containing the packed decimal
/// * `digits` - Number of decimal digits in the field (1-18 supported)
/// * `scale` - Number of decimal places (can be negative for scaling)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
///
/// # Returns
/// A `SmallDecimal` containing the decoded value
///
/// # Errors
/// Returns an error if the packed decimal data contains invalid nibbles.
/// All errors include proper context information (`record_index`, `field_path`, `byte_offset`).
///
/// # Performance
/// This function uses specialized fast paths for common cases:
/// - 1-5 byte fields: Direct decoding with minimal validation
/// - Empty data: Immediate zero return
/// - Digits > 18: Error (maximum supported precision)
///
/// # Examples
///
/// ## Basic Positive Value
///
/// ```no_run
/// use copybook_codec::numeric::{decode_packed_decimal};
///
/// // "123" as COMP-3: [0x12, 0x3C] (12 positive, 3C = positive sign)
/// let data = [0x12, 0x3C];
/// let result = decode_packed_decimal(&data, 3, 0, true)?;
/// assert_eq!(result.to_string(), "123");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Negative Value
///
/// ```no_run
/// use copybook_codec::numeric::{decode_packed_decimal};
///
/// // "-456" as COMP-3: [0x04, 0x56, 0xD] (456 negative)
/// let data = [0x04, 0x56, 0xD];
/// let result = decode_packed_decimal(&data, 3, 0, true)?;
/// assert_eq!(result.to_string(), "-456");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Decimal Scale
///
/// ```no_run
/// use copybook_codec::numeric::{decode_packed_decimal};
///
/// // "12.34" with 2 decimal places: [0x12, 0x34, 0xC]
/// let data = [0x12, 0x34, 0xC];
/// let result = decode_packed_decimal(&data, 4, 2, true)?;
/// assert_eq!(result.to_string(), "12.34");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Unsigned Field
///
/// ```no_run
/// use copybook_codec::numeric::{decode_packed_decimal};
///
/// // Unsigned "789": [0x07, 0x89, 0xF] (F = unsigned sign)
/// let data = [0x07, 0x89, 0xF];
/// let result = decode_packed_decimal(&data, 3, 0, false)?;
/// assert_eq!(result.to_string(), "789");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Zero Value
///
/// ```no_run
/// use copybook_codec::numeric::{decode_packed_decimal};
///
/// // Zero: [0x00, 0x0C]
/// let data = [0x00, 0x0C];
/// let result = decode_packed_decimal(&data, 2, 0, true)?;
/// assert_eq!(result.to_string(), "0");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`encode_packed_decimal`] - For encoding packed decimals
/// * [`decode_packed_decimal_with_scratch`] - For zero-allocation decoding
/// * [`decode_packed_decimal_to_string_with_scratch`] - For direct string output
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_packed_decimal(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    // CRITICAL PERFORMANCE OPTIMIZATION: Ultra-fast path with minimal safety overhead
    let expected_bytes = usize::from((digits + 1).div_ceil(2));
    // PERFORMANCE CRITICAL: Single branch validation optimized for happy path
    if likely(data.len() == expected_bytes && !data.is_empty() && digits <= 18) {
        // ULTRA-FAST PATH: Most common enterprise cases with minimal validation
        return decode_packed_decimal_fast_path(data, digits, scale, signed);
    }

    // FALLBACK PATH: Full validation for edge cases
    if data.len() != expected_bytes {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            "Packed decimal data length mismatch".to_string(),
        ));
    }

    if data.is_empty() {
        return Ok(SmallDecimal::zero(scale));
    }

    if digits > 18 {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            format!(
                "COMP-3 field with {digits} digits exceeds maximum supported precision (18 digits max for current implementation)"
            ),
        ));
    }

    // Delegate to ultra-fast path
    decode_packed_decimal_fast_path(data, digits, scale, signed)
}

/// Ultra-optimized COMP-3 decoder for hot path performance
///
/// This function is highly optimized for the 95% case of enterprise COBOL processing
/// where COMP-3 fields are 1-5 bytes and well-formed. It selects the appropriate
/// specialized decoder based on the data length.
#[inline]
fn decode_packed_decimal_fast_path(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    match data.len() {
        1 => decode_packed_fast_len1(data[0], digits, scale, signed),
        2 => decode_packed_fast_len2(data, digits, scale, signed),
        3 => decode_packed_fast_len3(data, scale, signed),
        _ => decode_packed_fast_general(data, digits, scale, signed),
    }
}

/// Specialized COMP-3 decoder for 1-byte fields (1 digit)
///
/// # Arguments
/// * `byte` - The single byte of packed decimal data
/// * `digits` - Number of digits (should be 1)
/// * `scale` - Decimal scale
/// * `signed` - Whether the field is signed
#[inline]
fn decode_packed_fast_len1(
    byte: u8,
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    // Wave-3 regression guard: 1-byte COMP-3 fields should only represent 1-digit values.
    debug_assert!(digits == 1);
    let high_nibble = (byte >> 4) & 0x0F;
    let low_nibble = byte & 0x0F;
    let mut value = 0i64;

    if !digits.is_multiple_of(2) {
        if unlikely(high_nibble > 9) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                "Invalid digit nibble in packed decimal".to_string(),
            ));
        }
        value = i64::from(high_nibble);
    }

    if signed {
        let is_negative = match low_nibble {
            0xA | 0xC | 0xE | 0xF => false,
            0xB | 0xD => true,
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid sign nibble in packed decimal".to_string(),
                ));
            }
        };
        return Ok(create_normalized_decimal(value, scale, is_negative));
    }

    if unlikely(low_nibble != 0xF) {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            "Invalid unsigned sign nibble, expected 0xF".to_string(),
        ));
    }

    Ok(create_normalized_decimal(value, scale, false))
}

/// Specialized COMP-3 decoder for 2-byte fields (2-3 digits)
///
/// # Arguments
/// * `data` - The 2 bytes of packed decimal data
/// * `digits` - Number of digits (2 or 3)
/// * `scale` - Decimal scale
/// * `signed` - Whether the field is signed
#[inline]
fn decode_packed_fast_len2(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    // Wave-3 regression guard: 2-byte COMP-3 payloads cover only 2- or 3-digit values.
    debug_assert!(digits == 2 || digits == 3);
    let byte0 = data[0];
    let byte1 = data[1];

    let d1 = (byte0 >> 4) & 0x0F;
    let d2 = byte0 & 0x0F;
    let d3 = (byte1 >> 4) & 0x0F;
    let sign_nibble = byte1 & 0x0F;

    let value = if digits == 2 {
        if unlikely(d1 != 0) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Expected padding nibble 0 for 2-digit field, got 0x{d1:X}"),
            ));
        }

        if unlikely(d2 > 9 || d3 > 9) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                "Invalid digit in 2-digit COMP-3 field".to_string(),
            ));
        }

        i64::from(d2) * 10 + i64::from(d3)
    } else {
        if unlikely(d1 > 9 || d2 > 9 || d3 > 9) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                "Invalid digit in 3-digit COMP-3 field".to_string(),
            ));
        }

        i64::from(d1) * 100 + i64::from(d2) * 10 + i64::from(d3)
    };

    let is_negative = if signed {
        match sign_nibble {
            0xA | 0xC | 0xE | 0xF => false,
            0xB | 0xD => true,
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid sign nibble in packed decimal".to_string(),
                ));
            }
        }
    } else {
        if unlikely(sign_nibble != 0xF) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                "Invalid unsigned sign nibble, expected 0xF".to_string(),
            ));
        }
        false
    };

    Ok(create_normalized_decimal(value, scale, is_negative))
}

/// Specialized COMP-3 decoder for 3-byte fields (4-5 digits)
///
/// # Arguments
/// * `data` - The 3 bytes of packed decimal data
/// * `scale` - Decimal scale
/// * `signed` - Whether the field is signed
#[inline]
fn decode_packed_fast_len3(data: &[u8], scale: i16, signed: bool) -> Result<SmallDecimal> {
    // Wave-3 regression guard: 3-byte COMP-3 payloads cover 4- or 5-digit values.
    debug_assert!(data.len() == 3);
    let byte0 = data[0];
    let byte1 = data[1];
    let byte2 = data[2];

    let d1 = (byte0 >> 4) & 0x0F;
    let d2 = byte0 & 0x0F;
    let d3 = (byte1 >> 4) & 0x0F;
    let d4 = byte1 & 0x0F;
    let d5 = (byte2 >> 4) & 0x0F;
    let sign_nibble = byte2 & 0x0F;

    if unlikely(d1 > 9 || d2 > 9 || d3 > 9 || d4 > 9 || d5 > 9) {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            "Invalid digit in 3-byte COMP-3 field".to_string(),
        ));
    }

    let value = i64::from(d1) * 10000
        + i64::from(d2) * 1000
        + i64::from(d3) * 100
        + i64::from(d4) * 10
        + i64::from(d5);

    let is_negative = if signed {
        match sign_nibble {
            0xA | 0xC | 0xE | 0xF => false,
            0xB | 0xD => true,
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid sign nibble in packed decimal".to_string(),
                ));
            }
        }
    } else {
        if unlikely(sign_nibble != 0xF) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                "Invalid unsigned sign nibble, expected 0xF".to_string(),
            ));
        }
        false
    };

    Ok(create_normalized_decimal(value, scale, is_negative))
}

/// General-purpose COMP-3 decoder for fields longer than 3 bytes
///
/// Handles multi-byte packed decimal decoding with support for padding
/// nibbles and variable digit counts.
///
/// # Arguments
/// * `data` - The packed decimal data bytes
/// * `digits` - Number of decimal digits in the field
/// * `scale` - Decimal scale
/// * `signed` - Whether the field is signed
#[inline]
fn decode_packed_fast_general(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    let total_nibbles = digits + 1;
    let has_padding = (total_nibbles & 1) == 1;
    let digit_count = usize::from(digits);

    let Some((last_byte, prefix_bytes)) = data.split_last() else {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            "Packed decimal data is empty".to_string(),
        ));
    };
    let mut value = 0i64;
    let mut digit_pos = 0;

    for &byte in prefix_bytes {
        let high_nibble = (byte >> 4) & 0x0F;
        let low_nibble = byte & 0x0F;

        if likely(!(digit_pos == 0 && has_padding)) {
            if unlikely(high_nibble > 9) {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid digit nibble".to_string(),
                ));
            }
            value = value * 10 + i64::from(high_nibble);
            digit_pos += 1;
        }

        if unlikely(low_nibble > 9) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                "Invalid digit nibble".to_string(),
            ));
        }
        value = value * 10 + i64::from(low_nibble);
        digit_pos += 1;
    }

    let last_high = (*last_byte >> 4) & 0x0F;
    let sign_nibble = *last_byte & 0x0F;

    if likely(digit_pos < digit_count) {
        if unlikely(last_high > 9) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                "Invalid digit nibble".to_string(),
            ));
        }
        value = value * 10 + i64::from(last_high);
    }

    let is_negative = if signed {
        match sign_nibble {
            0xA | 0xC | 0xE | 0xF => false,
            0xB | 0xD => true,
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid sign nibble".to_string(),
                ));
            }
        }
    } else {
        if unlikely(sign_nibble != 0xF) {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                "Invalid unsigned sign nibble".to_string(),
            ));
        }
        false
    };

    Ok(create_normalized_decimal(value, scale, is_negative))
}

/// Decode binary integer field (COMP-4, COMP-5, BINARY)
///
/// Decodes big-endian binary integer fields. Supports 16-bit, 32-bit, and 64-bit
/// widths. All binary integers use big-endian byte order as per COBOL specification.
///
/// # Arguments
/// * `data` - Raw byte data containing the binary integer
/// * `bits` - Bit width of the field (16, 32, or 64)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
///
/// # Returns
/// The decoded integer value as `i64`
///
/// # Errors
/// Returns an error if the binary data is invalid or the field size is unsupported.
///
/// # Examples
///
/// ## 16-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{decode_binary_int};
///
/// // 16-bit signed: -12345 = [0xCF, 0xC7] (big-endian)
/// let data = [0xCF, 0xC7];
/// let result = decode_binary_int(&data, 16, true)?;
/// assert_eq!(result, -12345);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 16-bit Unsigned Integer
///
/// ```no_run
/// use copybook_codec::numeric::{decode_binary_int};
///
/// // 16-bit unsigned: 54321 = [0xD4, 0x31]
/// let data = [0xD4, 0x31];
/// let result = decode_binary_int(&data, 16, false)?;
/// assert_eq!(result, 54321);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 32-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{decode_binary_int};
///
/// // 32-bit signed: -987654321 = [0xC5, 0x7D, 0x3C, 0x21]
/// let data = [0xC5, 0x7D, 0x3C, 0x21];
/// let result = decode_binary_int(&data, 32, true)?;
/// assert_eq!(result, -987654321);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 64-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{decode_binary_int};
///
/// // 64-bit signed: 9223372036854775807 = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]
/// let data = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07];
/// let result = decode_binary_int(&data, 64, true)?;
/// assert_eq!(result, 9223372036854775807);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`encode_binary_int`] - For encoding binary integers
/// * [`get_binary_width_from_digits`] - For mapping digit count to width
/// * [`validate_explicit_binary_width`] - For validating explicit BINARY(n) widths
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_binary_int(data: &[u8], bits: u16, signed: bool) -> Result<i64> {
    let expected_bytes = usize::from(bits / 8);
    if data.len() != expected_bytes {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE, // Reusing error code for binary validation
            format!(
                "Binary data length {} doesn't match expected {} bytes for {} bits",
                data.len(),
                expected_bytes,
                bits
            ),
        ));
    }

    match bits {
        16 => {
            if data.len() != 2 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "16-bit binary field requires exactly 2 bytes",
                ));
            }
            let value = u16::from_be_bytes([data[0], data[1]]);
            if signed {
                Ok(i64::from(i16::from_be_bytes([data[0], data[1]])))
            } else {
                Ok(i64::from(value))
            }
        }
        32 => {
            if data.len() != 4 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "32-bit binary field requires exactly 4 bytes",
                ));
            }
            let value = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
            if signed {
                Ok(i64::from(i32::from_be_bytes([
                    data[0], data[1], data[2], data[3],
                ])))
            } else {
                Ok(i64::from(value))
            }
        }
        64 => {
            if data.len() != 8 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "64-bit binary field requires exactly 8 bytes",
                ));
            }
            let bytes: [u8; 8] = data.try_into().map_err(|_| {
                Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Failed to convert data to 8-byte array",
                )
            })?;
            if signed {
                Ok(i64::from_be_bytes(bytes))
            } else {
                // For unsigned 64-bit, we need to be careful about overflow
                let value = u64::from_be_bytes(bytes);
                let max_i64 = u64::try_from(i64::MAX).unwrap_or(u64::MAX);
                if value > max_i64 {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    ));
                }
                i64::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    )
                })
            }
        }
        _ => Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            format!("Unsupported binary field width: {bits} bits"),
        )),
    }
}

/// Encode a zoned decimal using the configured code page defaults.
///
/// Encodes decimal values to zoned decimal format (PIC 9) where each digit is stored
/// in a byte with a zone nibble and a digit nibble. For signed fields, the
/// last byte uses overpunch encoding for the sign.
///
/// # Arguments
/// * `value` - String representation of the decimal value to encode
/// * `digits` - Number of digit characters (field length)
/// * `scale` - Number of decimal places (can be negative for scaling)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
/// * `codepage` - Character encoding (ASCII or EBCDIC variant)
///
/// # Returns
/// A vector of bytes containing the encoded zoned decimal
///
/// # Policy
/// Applies `ZeroSignPolicy::Positive` for ASCII and `ZeroSignPolicy::Preferred` for EBCDIC when no overrides are provided.
///
/// # Errors
/// Returns an error if the value cannot be encoded as a zoned decimal with the specified parameters.
///
/// # Examples
///
/// ## Basic ASCII Encoding
///
/// ```no_run
/// use copybook_codec::numeric::{encode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // Encode "123" as ASCII zoned decimal
/// let encoded = encode_zoned_decimal("123", 3, 0, false, Codepage::ASCII)?;
/// assert_eq!(encoded, b"123"); // [0x31, 0x32, 0x33]
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Signed ASCII Encoding (Overpunch)
///
/// ```no_run
/// use copybook_codec::numeric::{encode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // Encode "-456" with overpunch sign
/// let encoded = encode_zoned_decimal("-456", 3, 0, true, Codepage::ASCII)?;
/// // Last byte 0x4D = 'M' = digit 3 with negative sign
/// assert_eq!(encoded, [0x34, 0x35, 0x4D]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## EBCDIC Encoding
///
/// ```no_run
/// use copybook_codec::numeric::{encode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // Encode "789" as EBCDIC zoned decimal
/// let encoded = encode_zoned_decimal("789", 3, 0, false, Codepage::CP037)?;
/// assert_eq!(encoded, [0xF7, 0xF8, 0xF9]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Decimal Scale
///
/// ```no_run
/// use copybook_codec::numeric::{encode_zoned_decimal};
/// use copybook_codec::options::Codepage;
///
/// // Encode "12.34" with 2 decimal places
/// let encoded = encode_zoned_decimal("12.34", 4, 2, false, Codepage::ASCII)?;
/// assert_eq!(encoded, b"1234"); // [0x31, 0x32, 0x33, 0x34]
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`encode_zoned_decimal_with_format`] - For encoding with explicit format
/// * [`encode_zoned_decimal_with_format_and_policy`] - For encoding with format and policy
/// * [`encode_zoned_decimal_with_bwz`] - For encoding with BLANK WHEN ZERO support
/// * [`decode_zoned_decimal`] - For decoding zoned decimals
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_zoned_decimal(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
) -> Result<Vec<u8>> {
    let zero_policy = if codepage.is_ascii() {
        ZeroSignPolicy::Positive
    } else {
        ZeroSignPolicy::Preferred
    };

    encode_zoned_decimal_with_format_and_policy(
        value,
        digits,
        scale,
        signed,
        codepage,
        None,
        zero_policy,
    )
}

/// Encode a zoned decimal using an explicit encoding override when supplied.
///
/// Encodes zoned decimal values with an explicit encoding format (ASCII or EBCDIC).
/// When `encoding_override` is provided, it takes precedence over the codepage default.
/// When `Auto` is specified, the codepage default is used.
///
/// # Arguments
/// * `value` - String representation of the decimal value to encode
/// * `digits` - Number of digit characters (field length)
/// * `scale` - Number of decimal places (can be negative for scaling)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
/// * `codepage` - Character encoding (ASCII or EBCDIC variant)
/// * `encoding_override` - Optional explicit encoding format (ASCII/EBCDIC/Auto)
///
/// # Returns
/// A vector of bytes containing the encoded zoned decimal
///
/// # Policy
/// Resolves `ZeroSignPolicy` from `encoding_override` first; when unset or `Auto`, falls back to the code page defaults.
///
/// # Errors
/// Returns an error if the value cannot be encoded as a zoned decimal with the specified parameters.
///
/// # Examples
///
/// ## ASCII Encoding (Explicit)
///
/// ```no_run
/// use copybook_codec::numeric::{encode_zoned_decimal_with_format};
/// use copybook_codec::options::Codepage;
/// use copybook_codec::options::ZonedEncodingFormat;
///
/// // Encode "123" with explicit ASCII encoding
/// let encoded = encode_zoned_decimal_with_format(
///     "123", 3, 0, false, Codepage::ASCII, Some(ZonedEncodingFormat::Ascii)
/// )?;
/// assert_eq!(encoded, b"123");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## EBCDIC Encoding (Explicit)
///
/// ```no_run
/// use copybook_codec::numeric::{encode_zoned_decimal_with_format};
/// use copybook_codec::options::Codepage;
/// use copybook_codec::options::ZonedEncodingFormat;
///
/// // Encode "789" with explicit EBCDIC encoding
/// let encoded = encode_zoned_decimal_with_format(
///     "789", 3, 0, false, Codepage::CP037, Some(ZonedEncodingFormat::Ebcdic)
/// )?;
/// assert_eq!(encoded, [0xF7, 0xF8, 0xF9]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Auto Encoding (Codepage Default)
///
/// ```no_run
/// use copybook_codec::numeric::{encode_zoned_decimal_with_format};
/// use copybook_codec::options::Codepage;
/// use copybook_codec::options::ZonedEncodingFormat;
///
/// // Encode "456" with Auto encoding (uses EBCDIC default for CP037)
/// let encoded = encode_zoned_decimal_with_format(
///     "456", 3, 0, false, Codepage::CP037, Some(ZonedEncodingFormat::Auto)
/// )?;
/// assert_eq!(encoded, [0xF4, 0xF5, 0xF6]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`encode_zoned_decimal`] - For encoding with codepage defaults
/// * [`encode_zoned_decimal_with_format_and_policy`] - For encoding with format and policy
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_zoned_decimal_with_format(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    encoding_override: Option<ZonedEncodingFormat>,
) -> Result<Vec<u8>> {
    let zero_policy = match encoding_override {
        Some(ZonedEncodingFormat::Ascii) => ZeroSignPolicy::Positive,
        Some(ZonedEncodingFormat::Ebcdic) => ZeroSignPolicy::Preferred,
        Some(ZonedEncodingFormat::Auto) | None => {
            if codepage.is_ascii() {
                ZeroSignPolicy::Positive
            } else {
                ZeroSignPolicy::Preferred
            }
        }
    };

    encode_zoned_decimal_with_format_and_policy(
        value,
        digits,
        scale,
        signed,
        codepage,
        encoding_override,
        zero_policy,
    )
}

/// Encode a zoned decimal using a caller-resolved format and zero-sign policy.
///
/// This is the lowest-level zoned decimal encoder. The caller supplies both the
/// encoding format override (ASCII vs EBCDIC) and the zero-sign policy, which
/// together govern how the sign nibble of the last byte is produced.  Higher-level
/// wrappers such as [`encode_zoned_decimal`] and [`encode_zoned_decimal_with_format`]
/// resolve these parameters from codec defaults and then delegate here.
///
/// # Arguments
/// * `value` - String representation of the decimal value to encode (e.g. `"123"`, `"-45.67"`)
/// * `digits` - Number of digit positions in the COBOL field (PIC digit count)
/// * `scale` - Number of implied decimal places (can be negative for scaling)
/// * `signed` - Whether the field carries a sign (PIC S9 vs PIC 9)
/// * `codepage` - Target character encoding (ASCII or EBCDIC variant)
/// * `encoding_override` - Explicit format override; `None` falls back to codepage default
/// * `zero_policy` - How the sign nibble is encoded for zero values
///
/// # Returns
/// A vector of bytes containing the encoded zoned decimal in the target encoding.
///
/// # Policy
/// Callers provide the resolved policy in precedence order:
/// override → preserved metadata → preferred for the target code page.
///
/// # Errors
/// * `CBKE510_NUMERIC_OVERFLOW` - if the value is too large for the digit count
/// * `CBKE501_JSON_TYPE_MISMATCH` - if the input contains non-digit characters
///
/// # See Also
/// * [`encode_zoned_decimal`] - Convenience wrapper that resolves policy from the codepage
/// * [`encode_zoned_decimal_with_format`] - Accepts a format override without an explicit policy
/// * [`encode_zoned_decimal_with_bwz`] - Adds BLANK WHEN ZERO support
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_zoned_decimal_with_format_and_policy(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    encoding_override: Option<ZonedEncodingFormat>,
    zero_policy: ZeroSignPolicy,
) -> Result<Vec<u8>> {
    // Parse the input value with scale validation (NORMATIVE)
    let decimal = SmallDecimal::from_str(value, scale)?;

    // Convert to string representation of digits
    let abs_value = decimal.value.abs();
    let width = usize::from(digits);
    let digit_str = format!("{abs_value:0width$}");

    if digit_str.len() > width {
        return Err(Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            format!("Value too large for {digits} digits"),
        ));
    }

    // Determine the encoding format to use
    // Precedence: explicit override > codepage default
    let mut target_format = encoding_override.unwrap_or(match codepage {
        Codepage::ASCII => ZonedEncodingFormat::Ascii,
        _ => ZonedEncodingFormat::Ebcdic,
    });
    if target_format == ZonedEncodingFormat::Auto {
        target_format = if codepage.is_ascii() {
            ZonedEncodingFormat::Ascii
        } else {
            ZonedEncodingFormat::Ebcdic
        };
    }

    let mut result = Vec::with_capacity(width);
    let digit_bytes = digit_str.as_bytes();

    // Encode each digit
    for (i, &ascii_digit) in digit_bytes.iter().enumerate() {
        let digit = ascii_digit - b'0';
        if digit > 9 {
            return Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Invalid digit character: {}", ascii_digit as char),
            ));
        }

        if i == digit_bytes.len() - 1 && signed {
            if target_format == ZonedEncodingFormat::Ascii {
                let overpunch_byte = encode_overpunch_byte(
                    digit,
                    decimal.negative,
                    Codepage::ASCII,
                    ZeroSignPolicy::Positive,
                )?;
                result.push(overpunch_byte);
            } else {
                let encode_codepage = if codepage == Codepage::ASCII {
                    Codepage::CP037
                } else {
                    codepage
                };
                let overpunch_byte =
                    encode_overpunch_byte(digit, decimal.negative, encode_codepage, zero_policy)?;
                result.push(overpunch_byte);
            }
        } else {
            let zone = match target_format {
                ZonedEncodingFormat::Ascii => ASCII_DIGIT_ZONE,
                _ => EBCDIC_DIGIT_ZONE,
            };
            result.push((zone << 4) | digit);
        }
    }

    Ok(result)
}

/// Encode packed decimal (COMP-3) field
///
/// Encodes decimal values to COMP-3 packed decimal format where each byte contains
/// two decimal digits (nibbles), with the last nibble containing the sign.
/// This function is optimized for high-throughput enterprise data processing.
///
/// # Arguments
/// * `value` - String representation of the decimal value to encode
/// * `digits` - Number of decimal digits in the field (1-18 supported)
/// * `scale` - Number of decimal places (can be negative for scaling)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
///
/// # Returns
/// A vector of bytes containing the encoded packed decimal
///
/// # Errors
/// Returns an error if the value cannot be encoded as a packed decimal with the specified parameters.
///
/// # Performance
/// This function uses optimized digit extraction to avoid `format!()` allocation overhead.
///
/// # Examples
///
/// ## Basic Positive Value
///
/// ```no_run
/// use copybook_codec::numeric::{encode_packed_decimal};
///
/// // Encode "123" as COMP-3: [0x12, 0x3C] (12 positive, 3C = positive sign)
/// let encoded = encode_packed_decimal("123", 3, 0, true)?;
/// assert_eq!(encoded, [0x12, 0x3C]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Negative Value
///
/// ```no_run
/// use copybook_codec::numeric::{encode_packed_decimal};
///
/// // Encode "-456" as COMP-3: [0x04, 0x56, 0xD] (456 negative)
/// let encoded = encode_packed_decimal("-456", 3, 0, true)?;
/// assert_eq!(encoded, [0x04, 0x56, 0xD]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Decimal Scale
///
/// ```no_run
/// use copybook_codec::numeric::{encode_packed_decimal};
///
/// // Encode "12.34" with 2 decimal places: [0x12, 0x34, 0xC]
/// let encoded = encode_packed_decimal("12.34", 4, 2, true)?;
/// assert_eq!(encoded, [0x12, 0x34, 0xC]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Unsigned Field
///
/// ```no_run
/// use copybook_codec::numeric::{encode_packed_decimal};
///
/// // Unsigned "789": [0x07, 0x89, 0xF] (F = unsigned sign)
/// let encoded = encode_packed_decimal("789", 3, 0, false)?;
/// assert_eq!(encoded, [0x07, 0x89, 0xF]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## Zero Value
///
/// ```no_run
/// use copybook_codec::numeric::{encode_packed_decimal};
///
/// // Zero: [0x00, 0x0C]
/// let encoded = encode_packed_decimal("0", 2, 0, true)?;
/// assert_eq!(encoded, [0x00, 0x0C]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`decode_packed_decimal`] - For decoding packed decimals
/// * [`encode_packed_decimal_with_scratch`] - For zero-allocation encoding
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_packed_decimal(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<Vec<u8>> {
    // Parse the input value with scale validation (NORMATIVE)
    let decimal = SmallDecimal::from_str(value, scale)?;

    // CRITICAL PERFORMANCE OPTIMIZATION: Avoid format!() allocation
    // Direct integer-to-digits conversion for massive speedup
    let abs_value = decimal.value.abs();

    // Fast path for zero
    if abs_value == 0 {
        let expected_bytes = usize::from((digits + 1).div_ceil(2));
        let mut result = vec![0u8; expected_bytes];
        // Set sign in last byte
        let sign_nibble = if signed {
            if decimal.negative { 0x0D } else { 0x0C }
        } else {
            0x0F
        };
        result[expected_bytes - 1] = sign_nibble;
        return Ok(result);
    }

    // Pre-allocate digit buffer on stack for speed (up to 18 digits for i64::MAX)
    let mut digit_buffer: [u8; 20] = [0; 20];
    let mut digit_count = 0;
    let mut temp_value = abs_value;

    // Extract digits in reverse order using fast division
    while temp_value > 0 {
        digit_buffer[digit_count] = digit_from_value(temp_value % 10);
        temp_value /= 10;
        digit_count += 1;
    }

    // Validate digit count
    let digits_usize = usize::from(digits);
    if unlikely(digit_count > digits_usize) {
        return Err(Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            format!("Value too large for {digits} digits"),
        ));
    }

    let expected_bytes = usize::from((digits + 1).div_ceil(2));
    let mut result = Vec::with_capacity(expected_bytes);

    // CRITICAL FIX: Handle digit positioning correctly for even/odd digit counts
    // For packed decimal, we have:
    // - Total nibbles needed: digits + 1 (for sign)
    // - If digits is even: first nibble is padding (0), then digits, then sign
    // - If digits is odd: no padding, digits fill completely, then sign

    let has_padding = digits.is_multiple_of(2); // Even digit count requires padding
    let total_nibbles = digits_usize + 1 + usize::from(has_padding);

    for byte_idx in 0..expected_bytes {
        let mut byte_val = 0u8;

        // Calculate which nibbles belong to this byte
        let nibble_offset = byte_idx * 2;

        // High nibble
        let high_nibble_idx = nibble_offset;
        if high_nibble_idx < total_nibbles - 1 {
            // Not the sign nibble
            if has_padding && high_nibble_idx == 0 {
                // First nibble is padding for even digit count
                byte_val |= 0x00 << 4;
            } else {
                // Calculate which digit this represents
                let digit_idx = if has_padding {
                    high_nibble_idx - 1
                } else {
                    high_nibble_idx
                };

                // CRITICAL FIX: Right-align digits in COMP-3 field (leading zeros, not trailing)
                // For field width of 'digits', actual digits should occupy the rightmost positions
                if digit_idx >= (digits_usize - digit_count) {
                    // This position should contain an actual digit
                    let actual_digit_idx = digit_idx - (digits_usize - digit_count);
                    if actual_digit_idx < digit_count {
                        // Digits are stored in reverse order (least significant first)
                        let digit_pos_from_right = digit_count - 1 - actual_digit_idx;
                        let digit = digit_buffer[digit_pos_from_right];
                        byte_val |= digit << 4;
                    }
                }
                // else: leading zero for large digit field (byte_val already initialized to 0)
            }
        }

        // Low nibble
        let low_nibble_idx = nibble_offset + 1;
        if low_nibble_idx == total_nibbles - 1 {
            // This is the sign nibble
            byte_val |= if signed {
                if decimal.negative { 0x0D } else { 0x0C }
            } else {
                0x0F
            };
        } else if low_nibble_idx < total_nibbles - 1 {
            // Calculate which digit this represents
            let digit_idx = if has_padding {
                low_nibble_idx - 1
            } else {
                low_nibble_idx
            };

            // CRITICAL FIX: Right-align digits in COMP-3 field (leading zeros, not trailing)
            // For field width of 'digits', actual digits should occupy the rightmost positions
            if digit_idx >= (digits_usize - digit_count) {
                // This position should contain an actual digit
                let actual_digit_idx = digit_idx - (digits_usize - digit_count);
                if actual_digit_idx < digit_count {
                    // Digits are stored in reverse order (least significant first)
                    let digit_pos_from_right = digit_count - 1 - actual_digit_idx;
                    let digit = digit_buffer[digit_pos_from_right];
                    byte_val |= digit;
                }
            }
            // else: leading zero for large digit field (byte_val already initialized to 0)
        }

        result.push(byte_val);
    }

    Ok(result)
}

/// Encode binary integer field (COMP-4, COMP-5, BINARY)
///
/// Encodes integer values to big-endian binary format. Supports 16-bit, 32-bit, and 64-bit
/// widths. All binary integers use big-endian byte order as per COBOL specification.
///
/// # Arguments
/// * `value` - Integer value to encode
/// * `bits` - Bit width of field (16, 32, or 64)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
///
/// # Returns
/// A vector of bytes containing the encoded binary integer
///
/// # Errors
/// Returns an error if the value is out of range for the specified bit width.
///
/// # Examples
///
/// ## 16-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{encode_binary_int};
///
/// // Encode -12345 as 16-bit signed
/// let encoded = encode_binary_int(-12345, 16, true)?;
/// assert_eq!(encoded, [0xCF, 0xC7]); // Big-endian
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 16-bit Unsigned Integer
///
/// ```no_run
/// use copybook_codec::numeric::{encode_binary_int};
///
/// // Encode 54321 as 16-bit unsigned
/// let encoded = encode_binary_int(54321, 16, false)?;
/// assert_eq!(encoded, [0xD4, 0x31]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 32-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{encode_binary_int};
///
/// // Encode -987654321 as 32-bit signed
/// let encoded = encode_binary_int(-987654321, 32, true)?;
/// assert_eq!(encoded, [0xC5, 0x7D, 0x3C, 0x21]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## 64-bit Signed Integer
///
/// ```no_run
/// use copybook_codec::numeric::{encode_binary_int};
///
/// // Encode 9223372036854775807 as 64-bit signed
/// let encoded = encode_binary_int(9223372036854775807, 64, true)?;
/// assert_eq!(encoded, [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07]);
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`decode_binary_int`] - For decoding binary integers
/// * [`get_binary_width_from_digits`] - For mapping digit count to width
/// * [`validate_explicit_binary_width`] - For validating explicit BINARY(n) widths
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_binary_int(value: i64, bits: u16, signed: bool) -> Result<Vec<u8>> {
    match bits {
        16 => {
            if signed {
                let int_value = i16::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for signed 16-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            } else {
                let int_value = u16::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for unsigned 16-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            }
        }
        32 => {
            if signed {
                let int_value = i32::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for signed 32-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            } else {
                let int_value = u32::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for unsigned 32-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            }
        }
        64 => {
            if signed {
                Ok(value.to_be_bytes().to_vec())
            } else {
                let int_value = u64::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} cannot be negative for unsigned 64-bit integer"),
                    )
                })?;
                Ok(int_value.to_be_bytes().to_vec())
            }
        }
        _ => Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Unsupported binary field width: {bits} bits"),
        )),
    }
}

/// Encode an alphanumeric (PIC X) field with right-padding to the declared length.
///
/// Converts the UTF-8 input string to the target codepage encoding and then
/// right-pads with space characters (ASCII `0x20` or EBCDIC `0x40`) to fill
/// the declared field length.
///
/// # Arguments
/// * `text` - UTF-8 string value to encode
/// * `field_len` - Declared byte length of the COBOL field
/// * `codepage` - Target character encoding (ASCII or EBCDIC variant)
///
/// # Returns
/// A vector of exactly `field_len` bytes containing the encoded and padded text.
///
/// # Errors
/// * `CBKE501_JSON_TYPE_MISMATCH` - if the encoded text exceeds `field_len` bytes
///
/// # See Also
/// * [`crate::charset::utf8_to_ebcdic`] - Underlying character conversion
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_alphanumeric(text: &str, field_len: usize, codepage: Codepage) -> Result<Vec<u8>> {
    // Convert UTF-8 to target encoding
    let encoded_bytes = crate::charset::utf8_to_ebcdic(text, codepage)?;

    if encoded_bytes.len() > field_len {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!(
                "Text length {} exceeds field length {}",
                encoded_bytes.len(),
                field_len
            ),
        ));
    }

    // Pad with spaces to field length (NORMATIVE)
    let mut result = encoded_bytes;
    let space_byte = match codepage {
        Codepage::ASCII => b' ',
        _ => 0x40, // EBCDIC space
    };

    result.resize(field_len, space_byte);
    Ok(result)
}

/// Determine whether a value should be encoded as all spaces under the
/// COBOL `BLANK WHEN ZERO` clause.
///
/// Returns `true` when `bwz_encode` is enabled **and** the string value
/// represents zero (including decimal zeros such as `"0.00"`).  Callers
/// use this check before encoding to decide whether to emit a
/// space-filled field instead of the normal numeric encoding.
///
/// # Arguments
/// * `value` - String representation of the numeric value to test
/// * `bwz_encode` - Whether the BLANK WHEN ZERO clause is active for this field
///
/// # Returns
/// `true` if the field should be encoded as all spaces; `false` otherwise.
///
/// # See Also
/// * [`encode_zoned_decimal_with_bwz`] - Uses this function to apply the BWZ policy
#[inline]
#[must_use]
pub fn should_encode_as_blank_when_zero(value: &str, bwz_encode: bool) -> bool {
    if !bwz_encode {
        return false;
    }

    // Check if value is zero (with any scale)
    let trimmed = value.trim();
    if trimmed.is_empty() || trimmed == "0" {
        return true;
    }

    // Check for decimal zero (0.00, 0.000, etc.)
    if let Some(dot_pos) = trimmed.find('.') {
        let integer_part = &trimmed[..dot_pos];
        let fractional_part = &trimmed[dot_pos + 1..];

        if integer_part == "0" && fractional_part.chars().all(|c| c == '0') {
            return true;
        }
    }

    false
}

/// Encode a zoned decimal with COBOL `BLANK WHEN ZERO` support.
///
/// When `bwz_encode` is `true` and the value is zero (including decimal zeros
/// like `"0.00"`), the entire field is filled with space bytes (ASCII `0x20`
/// or EBCDIC `0x40`).  Otherwise, encoding delegates to [`encode_zoned_decimal`].
///
/// # Arguments
/// * `value` - String representation of the decimal value to encode
/// * `digits` - Number of digit positions in the COBOL field
/// * `scale` - Number of implied decimal places
/// * `signed` - Whether the field carries a sign
/// * `codepage` - Target character encoding
/// * `bwz_encode` - Whether the BLANK WHEN ZERO clause is active
///
/// # Returns
/// A vector of bytes containing the encoded zoned decimal, or all-space bytes
/// when the BWZ policy triggers.
///
/// # Errors
/// Returns an error if the value cannot be represented in the target zoned
/// decimal format (delegates error handling to [`encode_zoned_decimal`]).
///
/// # See Also
/// * [`should_encode_as_blank_when_zero`] - The predicate used to test zero values
/// * [`encode_zoned_decimal`] - Non-BWZ zoned decimal encoding
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_zoned_decimal_with_bwz(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    bwz_encode: bool,
) -> Result<Vec<u8>> {
    // Check BWZ policy first
    if should_encode_as_blank_when_zero(value, bwz_encode) {
        let space_byte = match codepage {
            Codepage::ASCII => b' ',
            _ => 0x40, // EBCDIC space
        };
        return Ok(vec![space_byte; usize::from(digits)]);
    }

    encode_zoned_decimal(value, digits, scale, signed, codepage)
}

/// Map a COBOL PIC digit count to the corresponding USAGE BINARY storage width
/// in bits (NORMATIVE).
///
/// Follows the COBOL standard mapping:
/// - 1-4 digits  -> 16 bits (2 bytes, halfword)
/// - 5-9 digits  -> 32 bits (4 bytes, fullword)
/// - 10-18 digits -> 64 bits (8 bytes, doubleword)
///
/// # Arguments
/// * `digits` - Number of digit positions declared in the PIC clause
///
/// # Returns
/// The storage width in bits: 16, 32, or 64.
///
/// # See Also
/// * [`validate_explicit_binary_width`] - For explicit `USAGE BINARY(n)` declarations
/// * [`decode_binary_int`] - Decodes binary integer data at the determined width
#[inline]
#[must_use]
pub fn get_binary_width_from_digits(digits: u16) -> u16 {
    match digits {
        1..=4 => 16, // 2 bytes
        5..=9 => 32, // 4 bytes
        _ => 64,     // 8 bytes for larger values
    }
}

/// Validate an explicit `USAGE BINARY(n)` byte-width declaration and return
/// the equivalent bit width (NORMATIVE).
///
/// Only the widths 1, 2, 4, and 8 bytes are valid.  Each is converted to
/// the corresponding bit count (8, 16, 32, 64) for downstream codec use.
///
/// # Arguments
/// * `width_bytes` - The explicit byte width from the copybook (1, 2, 4, or 8)
///
/// # Returns
/// The equivalent width in bits: 8, 16, 32, or 64.
///
/// # Errors
/// * `CBKE501_JSON_TYPE_MISMATCH` - if `width_bytes` is not 1, 2, 4, or 8
///
/// # See Also
/// * [`get_binary_width_from_digits`] - Infers width from PIC digit count
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn validate_explicit_binary_width(width_bytes: u8) -> Result<u16> {
    match width_bytes {
        1 => Ok(8),  // 1 byte = 8 bits
        2 => Ok(16), // 2 bytes = 16 bits
        4 => Ok(32), // 4 bytes = 32 bits
        8 => Ok(64), // 8 bytes = 64 bits
        _ => Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Invalid explicit binary width: {width_bytes} bytes. Must be 1, 2, 4, or 8"),
        )),
    }
}

/// Get the space byte value for a given codepage
///
/// Returns the appropriate space character byte for ASCII or EBCDIC codepages.
///
/// # Arguments
/// * `codepage` - The target codepage
///
/// # Returns
/// * `0x20` (ASCII space) for ASCII codepage
/// * `0x40` (EBCDIC space) for EBCDIC codepages
///
/// # Examples
/// ```
/// use copybook_codec::options::Codepage;
/// # fn zoned_space_byte(codepage: Codepage) -> u8 {
/// #     match codepage {
/// #         Codepage::ASCII => b' ',
/// #         _ => 0x40,
/// #     }
/// # }
///
/// assert_eq!(zoned_space_byte(Codepage::ASCII), b' ');
/// assert_eq!(zoned_space_byte(Codepage::CP037), 0x40);
/// ```
#[inline]
const fn zoned_space_byte(codepage: Codepage) -> u8 {
    match codepage {
        Codepage::ASCII => b' ',
        _ => 0x40,
    }
}

/// Get the expected zone nibble for valid digits
///
/// Returns the zone nibble value expected for digit bytes in zoned decimal
/// encoding for the given codepage.
///
/// # Arguments
/// * `codepage` - The target codepage
///
/// # Returns
/// * `0x3` for ASCII (digits 0x30-0x39)
/// * `0xF` for EBCDIC (digits 0xF0-0xF9)
///
/// # Examples
/// ```
/// use copybook_codec::options::Codepage;
/// # const ASCII_DIGIT_ZONE: u8 = 0x3;
/// # const EBCDIC_DIGIT_ZONE: u8 = 0xF;
/// # fn zoned_expected_zone(codepage: Codepage) -> u8 {
/// #     match codepage {
/// #         Codepage::ASCII => ASCII_DIGIT_ZONE,
/// #         _ => EBCDIC_DIGIT_ZONE,
/// #     }
/// # }
///
/// assert_eq!(zoned_expected_zone(Codepage::ASCII), 0x3);
/// assert_eq!(zoned_expected_zone(Codepage::CP037), 0xF);
/// ```
#[inline]
const fn zoned_expected_zone(codepage: Codepage) -> u8 {
    match codepage {
        Codepage::ASCII => ASCII_DIGIT_ZONE,
        _ => EBCDIC_DIGIT_ZONE,
    }
}

/// Get a human-readable label for the encoding zone type
///
/// Returns a string label describing the encoding zone type for error messages.
///
/// # Arguments
/// * `codepage` - The target codepage
///
/// # Returns
/// * `"ASCII"` for ASCII codepage
/// * `"EBCDIC"` for EBCDIC codepages
///
/// # Examples
/// ```
/// use copybook_codec::options::Codepage;
/// # fn zoned_zone_label(codepage: Codepage) -> &'static str {
/// #     match codepage {
/// #         Codepage::ASCII => "ASCII",
/// #         _ => "EBCDIC",
/// #     }
/// # }
///
/// assert_eq!(zoned_zone_label(Codepage::ASCII), "ASCII");
/// assert_eq!(zoned_zone_label(Codepage::CP037), "EBCDIC");
/// ```
#[inline]
const fn zoned_zone_label(codepage: Codepage) -> &'static str {
    match codepage {
        Codepage::ASCII => "ASCII",
        _ => "EBCDIC",
    }
}

/// Validate a non-final byte in a zoned decimal field
///
/// Checks that the byte contains a valid digit nibble (0-9) and the expected
/// zone nibble for the codepage. Non-final bytes should not contain sign information.
///
/// # Arguments
/// * `byte` - The byte to validate
/// * `index` - Position of the byte in the field (for error messages)
/// * `expected_zone` - Expected zone nibble value (0x3 for ASCII, 0xF for EBCDIC)
/// * `codepage` - Target codepage for zone validation
///
/// # Returns
/// The digit nibble value (0-9) extracted from the byte
///
/// # Errors
/// * `CBKD411_ZONED_BAD_SIGN` - Invalid digit nibble or mismatched zone
///
/// # Examples
/// ```text
/// // ASCII '5' is 0x35 (zone 0x3, digit 0x5)
/// let digit = zoned_validate_non_final_byte(0x35, 0, 0x3, Codepage::ASCII)?;
/// assert_eq!(digit, 5);
/// ```
#[inline]
fn zoned_validate_non_final_byte(
    byte: u8,
    index: usize,
    expected_zone: u8,
    codepage: Codepage,
) -> Result<u8> {
    let zone = (byte >> 4) & 0x0F;
    let digit = byte & 0x0F;

    if digit > 9 {
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            format!("Invalid digit nibble 0x{digit:X} at position {index}"),
        ));
    }

    if zone != expected_zone {
        let zone_label = zoned_zone_label(codepage);
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            format!(
                "Invalid {zone_label} zone 0x{zone:X} at position {index}, expected 0x{expected_zone:X}"
            ),
        ));
    }

    Ok(digit)
}

/// Process all non-final digits in a zoned decimal field
///
/// Validates each byte's zone and digit nibbles, accumulates the numeric value,
/// and stores digits in the scratch buffer for verification.
///
/// # Arguments
/// * `data` - Non-final bytes of the zoned decimal field
/// * `expected_zone` - Expected zone nibble (0x3 for ASCII, 0xF for EBCDIC)
/// * `codepage` - Target codepage
/// * `scratch` - Scratch buffers for digit accumulation
///
/// # Returns
/// Accumulated integer value from non-final digits
///
/// # Errors
/// * `CBKD411_ZONED_BAD_SIGN` - Invalid zone or digit nibble encountered
///
/// # Performance
/// Uses saturating arithmetic to prevent overflow panics while accumulating
/// the numeric value.
#[inline]
fn zoned_process_non_final_digits(
    data: &[u8],
    expected_zone: u8,
    codepage: Codepage,
    scratch: &mut ScratchBuffers,
) -> Result<i64> {
    let mut value = 0i64;

    for (index, &byte) in data.iter().enumerate() {
        let digit = zoned_validate_non_final_byte(byte, index, expected_zone, codepage)?;
        scratch.digit_buffer.push(digit);
        value = value.saturating_mul(10).saturating_add(i64::from(digit));
    }

    Ok(value)
}

/// Decode the last byte of a zoned decimal field
///
/// The last byte contains both a digit and sign information encoded as an
/// overpunch character. Delegates to the overpunch decoder for extraction.
///
/// # Arguments
/// * `byte` - The final byte of the zoned decimal field
/// * `codepage` - Target codepage for overpunch interpretation
///
/// # Returns
/// Tuple of (digit, `is_negative`) extracted from the overpunch byte
///
/// # Errors
/// * `CBKD411_ZONED_BAD_SIGN` - Invalid overpunch encoding
///
/// # See Also
/// * `zoned_overpunch::decode_overpunch_byte` - Underlying overpunch decoder
#[inline]
fn zoned_decode_last_byte(byte: u8, codepage: Codepage) -> Result<(u8, bool)> {
    crate::zoned_overpunch::decode_overpunch_byte(byte, codepage)
}

/// Ensure unsigned zoned decimal has no sign information
///
/// Validates that an unsigned zoned decimal field contains only unsigned zone
/// nibbles and no negative overpunch encoding.
///
/// # Arguments
/// * `last_byte` - The final byte of the field
/// * `expected_zone` - Expected unsigned zone (0x3 for ASCII, 0xF for EBCDIC)
/// * `codepage` - Target codepage
/// * `negative` - Whether overpunch decoding detected a negative sign
///
/// # Returns
/// Always returns `Ok(false)` for valid unsigned fields
///
/// # Errors
/// * `CBKD411_ZONED_BAD_SIGN` - Sign zone or negative overpunch in unsigned field
///
/// # Examples
/// ```text
/// // Valid unsigned ASCII zoned decimal ends with zone 0x3
/// let result = zoned_ensure_unsigned(0x35, 0x3, Codepage::ASCII, false)?;
/// assert_eq!(result, false);
/// ```
#[inline]
fn zoned_ensure_unsigned(
    last_byte: u8,
    expected_zone: u8,
    codepage: Codepage,
    negative: bool,
) -> Result<bool> {
    let zone = (last_byte >> 4) & 0x0F;
    if zone != expected_zone {
        let zone_label = zoned_zone_label(codepage);
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            format!(
                "Unsigned {zone_label} zoned decimal cannot contain sign zone 0x{zone:X} in last byte"
            ),
        ));
    }

    if negative {
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "Unsigned zoned decimal contains negative overpunch",
        ));
    }

    Ok(false)
}

/// Decode a zoned decimal using the configured code page and policy while reusing scratch buffers.
///
/// Decodes zoned decimal fields while reusing scratch buffers to avoid repeated allocations.
/// This is optimized for high-throughput processing where the same scratch buffers
/// are used across multiple decode operations.
///
/// # Arguments
/// * `data` - Raw byte data containing the zoned decimal
/// * `digits` - Number of digit characters (field length)
/// * `scale` - Number of decimal places (can be negative for scaling)
/// * `signed` - Whether the field is signed (true) or unsigned (false)
/// * `codepage` - Character encoding (ASCII or EBCDIC variant)
/// * `blank_when_zero` - If true, all-space fields decode as zero
/// * `scratch` - Mutable reference to scratch buffers for reuse
///
/// # Returns
/// A `SmallDecimal` containing of decoded value
///
/// # Policy
/// Defaults to *preferred zero sign* (`ZeroSignPolicy::Preferred`) for EBCDIC zeros unless
/// `preserve_zoned_encoding` captured an explicit format at decode.
///
/// # Errors
/// Returns an error if zone nibbles or the last-byte overpunch are invalid.
///
/// # Performance
/// This function avoids allocations by reusing scratch buffers across decode operations.
/// Use this for processing multiple zoned decimal fields in a loop.
///
/// # Examples
///
/// ## Basic Decoding
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal_with_scratch};
/// use copybook_codec::memory::ScratchBuffers;
/// use copybook_codec::options::Codepage;
///
/// let mut scratch = ScratchBuffers::new();
/// let data = b"123";
/// let result = decode_zoned_decimal_with_scratch(data, 3, 0, false, Codepage::ASCII, false, &mut scratch)?;
/// assert_eq!(result.to_string(), "123");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// ## With BLANK WHEN ZERO
///
/// ```no_run
/// use copybook_codec::numeric::{decode_zoned_decimal_with_scratch};
/// use copybook_codec::memory::ScratchBuffers;
/// use copybook_codec::options::Codepage;
///
/// let mut scratch = ScratchBuffers::new();
/// let data = b"   "; // 3 ASCII spaces
/// let result = decode_zoned_decimal_with_scratch(data, 3, 0, false, Codepage::ASCII, true, &mut scratch)?;
/// assert_eq!(result.to_string(), "0");
/// # Ok::<(), copybook_core::Error>(())
/// ```
///
/// # See Also
/// * [`decode_zoned_decimal`] - For basic zoned decimal decoding
/// * [`ScratchBuffers`] - For scratch buffer management
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_zoned_decimal_with_scratch(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
    scratch: &mut ScratchBuffers,
) -> Result<SmallDecimal> {
    if data.len() != usize::from(digits) {
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            format!(
                "Zoned decimal data length {} doesn't match digits {}",
                data.len(),
                digits
            ),
        ));
    }

    // Check for BLANK WHEN ZERO (all spaces) - optimized check
    let space_byte = zoned_space_byte(codepage);

    let is_all_spaces = data.iter().all(|&b| b == space_byte);
    if is_all_spaces {
        if blank_when_zero {
            warn!("CBKD412_ZONED_BLANK_IS_ZERO: Zoned field is blank, decoding as zero");
            crate::lib_api::increment_warning_counter();
            return Ok(SmallDecimal::zero(scale));
        }
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "Zoned field contains all spaces but BLANK WHEN ZERO not specified",
        ));
    }

    // Clear and prepare digit buffer for reuse
    scratch.digit_buffer.clear();
    scratch.digit_buffer.reserve(usize::from(digits));

    let expected_zone = zoned_expected_zone(codepage);
    let Some((&last_byte, non_final)) = data.split_last() else {
        return Err(Error::new(
            ErrorCode::CBKD411_ZONED_BAD_SIGN,
            "Zoned decimal field is empty",
        ));
    };
    let partial_value =
        zoned_process_non_final_digits(non_final, expected_zone, codepage, scratch)?;
    let (last_digit, negative) = zoned_decode_last_byte(last_byte, codepage)?;
    scratch.digit_buffer.push(last_digit);
    let value = partial_value
        .saturating_mul(10)
        .saturating_add(i64::from(last_digit));
    let is_negative = if signed {
        negative
    } else {
        zoned_ensure_unsigned(last_byte, expected_zone, codepage, negative)?
    };
    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize();

    debug_assert!(
        scratch.digit_buffer.iter().all(|&d| d <= 9),
        "scratch digit buffer must contain only logical digits"
    );
    Ok(decimal)
}

/// Decode a single-byte packed decimal value
///
/// Handles the special case where the entire packed decimal fits in one byte.
/// For 1-digit fields, the high nibble contains the digit and low nibble contains
/// the sign. For 0-digit fields (just sign), only the low nibble is significant.
///
/// # Arguments
/// * `byte` - The packed decimal byte
/// * `digits` - Number of digits (0 or 1 for single byte)
/// * `scale` - Decimal scale
/// * `signed` - Whether the field is signed
///
/// # Returns
/// Decoded `SmallDecimal` value
///
/// # Errors
/// * `CBKD401_COMP3_INVALID_NIBBLE` - Invalid digit or sign nibble
///
/// # Format
/// Single-byte packed decimals:
/// - 1 digit: `[digit][sign]` (e.g., 0x5C = 5 positive)
/// - 0 digits: `[0][sign]` (just sign, high nibble must be 0)
///
/// Valid sign nibbles:
/// - Positive: 0xA, 0xC, 0xE, 0xF
/// - Negative: 0xB, 0xD
/// - Unsigned: 0xF only
#[inline]
fn packed_decode_single_byte(
    byte: u8,
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    let high_nibble = (byte >> 4) & 0x0F;
    let low_nibble = byte & 0x0F;
    let mut value = 0i64;

    if digits == 1 {
        if high_nibble > 9 {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid digit nibble 0x{high_nibble:X}"),
            ));
        }
        value = i64::from(high_nibble);
    }

    let is_negative = if signed {
        match low_nibble {
            0xA | 0xC | 0xE | 0xF => false,
            0xB | 0xD => true,
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!("Invalid sign nibble 0x{low_nibble:X}"),
                ));
            }
        }
    } else {
        if low_nibble != 0xF {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid unsigned sign nibble 0x{low_nibble:X}, expected 0xF"),
            ));
        }
        false
    };

    Ok(create_normalized_decimal(value, scale, is_negative))
}

/// Add a digit to the accumulating packed decimal value
///
/// Multiplies the current value by 10 and adds the new digit, with overflow checking.
///
/// # Arguments
/// * `value` - Mutable reference to the accumulating value
/// * `digit` - Digit to add (0-9)
///
/// # Returns
/// `Ok(())` on success
///
/// # Errors
/// * `CBKD411_ZONED_BAD_SIGN` - Numeric overflow during accumulation
///
/// # Performance
/// Uses checked arithmetic to prevent panics while detecting overflow conditions.
#[inline]
fn packed_push_digit(value: &mut i64, digit: u8) -> Result<()> {
    *value = value
        .checked_mul(10)
        .and_then(|v| v.checked_add(i64::from(digit)))
        .ok_or_else(|| {
            Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                "Numeric overflow during zoned decimal conversion",
            )
        })?;
    Ok(())
}

/// Process non-final bytes of a multi-byte packed decimal
///
/// Extracts digit nibbles from all bytes before the last one, handling padding
/// if the digit count is odd. Accumulates the numeric value and counts digits.
///
/// # Arguments
/// * `bytes` - Non-final bytes of the packed decimal
/// * `digits` - Total number of digits in the field
/// * `has_padding` - Whether the first nibble is padding (odd total nibbles)
///
/// # Returns
/// Tuple of (`accumulated_value`, `digit_count`)
///
/// # Errors
/// * `CBKD401_COMP3_INVALID_NIBBLE` - Invalid digit or padding nibble
///
/// # Format
/// Packed decimal nibble layout:
/// - Even digits: `[pad=0][d1][d2][d3]...[sign]`
/// - Odd digits: `[d1][d2][d3]...[sign]` (no padding)
#[inline]
fn packed_process_non_last_bytes(
    bytes: &[u8],
    digits: u16,
    has_padding: bool,
) -> Result<(i64, u16)> {
    let mut value = 0i64;
    let mut digit_count: u16 = 0;

    for (index, &byte) in bytes.iter().enumerate() {
        let high_nibble = (byte >> 4) & 0x0F;
        let low_nibble = byte & 0x0F;

        if index == 0 && has_padding {
            if high_nibble != 0 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!("Expected padding nibble 0, got 0x{high_nibble:X}"),
                ));
            }
        } else {
            if high_nibble > 9 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!("Invalid digit nibble 0x{high_nibble:X}"),
                ));
            }
            packed_push_digit(&mut value, high_nibble)?;
            digit_count += 1;
        }

        if digit_count >= digits {
            break;
        }

        if low_nibble > 9 {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid digit nibble 0x{low_nibble:X}"),
            ));
        }
        packed_push_digit(&mut value, low_nibble)?;
        digit_count += 1;

        if digit_count >= digits {
            break;
        }
    }

    Ok((value, digit_count))
}

/// Process the last byte of a packed decimal field
///
/// Extracts the final digit (if needed) and sign nibble from the last byte.
/// Creates the final normalized `SmallDecimal` value.
///
/// # Arguments
/// * `value` - Accumulated value from previous bytes
/// * `last_byte` - The final byte containing digit and sign
/// * `digits` - Total number of digits expected
/// * `digit_count` - Number of digits already processed
/// * `scale` - Decimal scale
/// * `signed` - Whether the field is signed
///
/// # Returns
/// Decoded and normalized `SmallDecimal`
///
/// # Errors
/// * `CBKD401_COMP3_INVALID_NIBBLE` - Invalid digit or sign nibble
///
/// # Format
/// Last byte always ends with sign nibble:
/// - If `digit_count` < digits: `[digit][sign]`
/// - If `digit_count` == digits: `[unused][sign]` (high nibble ignored)
#[inline]
fn packed_finish_last_byte(
    mut value: i64,
    last_byte: u8,
    digits: u16,
    digit_count: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    let high_nibble = (last_byte >> 4) & 0x0F;
    let low_nibble = last_byte & 0x0F;

    if digit_count < digits {
        if high_nibble > 9 {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid digit nibble 0x{high_nibble:X}"),
            ));
        }
        packed_push_digit(&mut value, high_nibble)?;
    }

    let is_negative = if signed {
        match low_nibble {
            0xA | 0xC | 0xE | 0xF => false,
            0xB | 0xD => true,
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!("Invalid sign nibble 0x{low_nibble:X}"),
                ));
            }
        }
    } else {
        if low_nibble != 0xF {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid unsigned sign nibble 0x{low_nibble:X}, expected 0xF"),
            ));
        }
        false
    };

    Ok(create_normalized_decimal(value, scale, is_negative))
}

/// Decode a multi-byte packed decimal value
///
/// Orchestrates the decoding of packed decimals that span multiple bytes by
/// processing non-final bytes and the final byte separately.
///
/// # Arguments
/// * `data` - Complete packed decimal byte array
/// * `digits` - Number of digits in the field
/// * `scale` - Decimal scale
/// * `signed` - Whether the field is signed
///
/// # Returns
/// Decoded `SmallDecimal` value
///
/// # Errors
/// * `CBKD401_COMP3_INVALID_NIBBLE` - Invalid nibbles or empty input
///
/// # Algorithm
/// 1. Calculate if padding nibble is present (odd total nibbles)
/// 2. Process all non-final bytes to extract digits
/// 3. Process final byte to extract last digit and sign
/// 4. Construct normalized `SmallDecimal`
#[inline]
fn packed_decode_multi_byte(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    let total_nibbles = digits + 1;
    let has_padding = (total_nibbles & 1) == 1;
    let Some((&last_byte, non_last)) = data.split_last() else {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            "Packed decimal input is empty",
        ));
    };
    let (value, digit_count) = packed_process_non_last_bytes(non_last, digits, has_padding)?;
    packed_finish_last_byte(value, last_byte, digits, digit_count, scale, signed)
}

/// Optimized packed decimal decoder using scratch buffers
/// Minimizes allocations by reusing digit buffer
///
/// # Errors
/// Returns an error when the packed decimal data has an invalid length or contains bad digit/sign nibbles.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_packed_decimal_with_scratch(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    scratch: &mut ScratchBuffers,
) -> Result<SmallDecimal> {
    let expected_bytes = usize::from((digits + 1).div_ceil(2));
    if data.len() != expected_bytes {
        return Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            format!(
                "Packed decimal data length {} doesn't match expected {} bytes for {} digits",
                data.len(),
                expected_bytes,
                digits
            ),
        ));
    }

    if data.is_empty() {
        return Ok(SmallDecimal::zero(scale));
    }

    // Use the original implementation - the "optimized" path actually hurts performance
    // Clear and prepare digit buffer for reuse
    scratch.digit_buffer.clear();
    scratch.digit_buffer.reserve(usize::from(digits));

    // Optimized nibble processing - unify handling for multi-byte cases
    let decimal = if data.len() == 1 {
        packed_decode_single_byte(data[0], digits, scale, signed)?
    } else {
        packed_decode_multi_byte(data, digits, scale, signed)?
    };

    debug_assert!(
        scratch.digit_buffer.iter().all(|&d| d <= 9),
        "scratch digit buffer must contain only logical digits"
    );

    Ok(decimal)
}

/// Decode a COBOL binary integer (USAGE BINARY / COMP) with optimized fast
/// paths for 16-, 32-, and 64-bit widths.
///
/// For the three common widths this function reads the big-endian bytes
/// directly into the native integer type, avoiding the generic loop in
/// [`decode_binary_int`].  Uncommon widths fall back to that generic
/// implementation.
///
/// # Arguments
/// * `data` - Raw big-endian byte data containing the binary integer
/// * `bits` - Expected field width in bits (16, 32, or 64 for fast path)
/// * `signed` - Whether the field is signed (PIC S9 COMP) or unsigned (PIC 9 COMP)
///
/// # Returns
/// The decoded integer value as `i64`.
///
/// # Errors
/// * `CBKD401_COMP3_INVALID_NIBBLE` - if an unsigned 64-bit value exceeds `i64::MAX`
/// * Delegates to [`decode_binary_int`] for unsupported widths, which may
///   return its own errors.
///
/// # See Also
/// * [`decode_binary_int`] - General-purpose binary integer decoder
/// * [`encode_binary_int`] - Binary integer encoder
/// * [`format_binary_int_to_string_with_scratch`] - Formats the decoded value to a string
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_binary_int_fast(data: &[u8], bits: u16, signed: bool) -> Result<i64> {
    // Optimized paths for common binary widths
    match (bits, data.len()) {
        (16, 2) => {
            // 16-bit integer - most common case
            let bytes = [data[0], data[1]];
            if signed {
                Ok(i64::from(i16::from_be_bytes(bytes)))
            } else {
                Ok(i64::from(u16::from_be_bytes(bytes)))
            }
        }
        (32, 4) => {
            // 32-bit integer - common case
            let bytes = [data[0], data[1], data[2], data[3]];
            if signed {
                Ok(i64::from(i32::from_be_bytes(bytes)))
            } else {
                Ok(i64::from(u32::from_be_bytes(bytes)))
            }
        }
        (64, 8) => {
            // 64-bit integer
            let bytes = [
                data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            ];
            if signed {
                Ok(i64::from_be_bytes(bytes))
            } else {
                let value = u64::from_be_bytes(bytes);
                let max_i64 = u64::try_from(i64::MAX).unwrap_or(u64::MAX);
                if value > max_i64 {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    ));
                }
                i64::try_from(value).map_err(|_| {
                    Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    )
                })
            }
        }
        _ => {
            // Fallback to general implementation
            decode_binary_int(data, bits, signed)
        }
    }
}

/// Encode a zoned decimal while reusing caller-owned scratch buffers to avoid
/// per-call heap allocations on the hot path.
///
/// Converts the pre-parsed [`SmallDecimal`] to its string representation using
/// the scratch buffer and then delegates to [`encode_zoned_decimal`].  The
/// `_bwz_encode` parameter is intentionally a no-op in this wave; it is reserved
/// for future BLANK WHEN ZERO integration.
///
/// # Arguments
/// * `decimal` - Pre-parsed decimal value to encode
/// * `digits` - Number of digit positions in the COBOL field
/// * `signed` - Whether the field carries a sign
/// * `codepage` - Target character encoding (ASCII or EBCDIC variant)
/// * `_bwz_encode` - Reserved for BLANK WHEN ZERO support (currently a no-op).
/// * `scratch` - Reusable scratch buffers for zero-allocation string processing
///
/// # Returns
/// A vector of bytes containing the encoded zoned decimal.
///
/// # Policy
/// Callers typically resolve policy using `zoned_encoding_override` → preserved
/// metadata → `preferred_zoned_encoding`, matching the documented library
/// behavior for zoned decimals.
///
/// # Errors
/// Returns an error when the decimal value cannot be represented with the
/// requested digit count or encoding format.
///
/// # See Also
/// * [`encode_zoned_decimal`] - Underlying encoder
/// * [`encode_packed_decimal_with_scratch`] - Scratch-based packed decimal encoder
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_zoned_decimal_with_scratch(
    decimal: &SmallDecimal,
    digits: u16,
    signed: bool,
    codepage: Codepage,
    _bwz_encode: bool,
    scratch: &mut ScratchBuffers,
) -> Result<Vec<u8>> {
    // Clear and prepare buffers
    scratch.digit_buffer.clear();
    scratch.byte_buffer.clear();
    scratch.byte_buffer.reserve(usize::from(digits));

    // Convert decimal to string using scratch buffer
    scratch.string_buffer.clear();
    scratch.string_buffer.push_str(&decimal.to_string());

    // Use the standard encode function, retaining current deterministic output
    // while keeping the scratch-buffer path for future AC9-like optimization work.
    encode_zoned_decimal(
        &scratch.string_buffer,
        digits,
        decimal.scale,
        signed,
        codepage,
    )
}

/// Encode a packed decimal (COMP-3) while reusing caller-owned scratch buffers
/// to minimize per-call allocations.
///
/// Encodes a pre-parsed [`SmallDecimal`] directly into packed bytes, avoiding any
/// intermediate `String` allocations. Intended for use on codec hot paths where
/// many records are encoded sequentially with the same [`ScratchBuffers`] instance.
///
/// # Arguments
/// * `decimal` - Pre-parsed decimal value to encode
/// * `digits` - Number of decimal digits in the field (1-18)
/// * `signed` - Whether the field is signed (`true`) or unsigned (`false`)
/// * `scratch` - Reusable scratch buffers for zero-allocation string processing
///
/// # Returns
/// A vector of bytes containing the encoded packed decimal (COMP-3 format).
///
/// # Errors
/// Returns an error when the decimal value cannot be encoded into the
/// requested packed representation.
///
/// # See Also
/// * [`encode_packed_decimal`] - Underlying packed decimal encoder
/// * [`encode_zoned_decimal_with_scratch`] - Scratch-based zoned decimal encoder
/// * [`decode_packed_decimal_to_string_with_scratch`] - Scratch-based decoder
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_packed_decimal_with_scratch(
    decimal: &SmallDecimal,
    digits: u16,
    signed: bool,
    scratch: &mut ScratchBuffers,
) -> Result<Vec<u8>> {
    // Clear and prepare buffers
    debug_assert!(digits > 0, "Packed decimal fields must have at least one digit");
    scratch.digit_buffer.clear();
    scratch.byte_buffer.clear();
    scratch.digit_buffer.reserve(usize::from(digits));
    let expected_bytes = usize::from((digits + 1).div_ceil(2));
    debug_assert!(expected_bytes > 0, "Packed decimal fields must occupy at least one byte");
    scratch.byte_buffer.resize(expected_bytes, 0u8);

    let is_negative = decimal.negative && decimal.value != 0;
    debug_assert!(
        signed || !is_negative,
        "Negative packed decimal provided to unsigned field"
    );
    let abs_value = decimal.value.abs();

    if abs_value == 0 {
        // Fast path for zero value: sign-only in final nibble
        let sign_nibble = if signed {
            if is_negative { 0x0D } else { 0x0C }
        } else {
            0x0F
        };
        if let Some(last_byte) = scratch.byte_buffer.last_mut() {
            *last_byte = sign_nibble;
        }
        return Ok(std::mem::take(&mut scratch.byte_buffer));
    }

    let mut temp_value = abs_value;
    while temp_value > 0 {
        scratch
            .digit_buffer
            .push(digit_from_value(temp_value % 10));
        temp_value /= 10;
    }

    let digit_count = scratch.digit_buffer.len();
    let digits_usize = usize::from(digits);
    if digit_count > digits_usize {
        return Err(Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            format!("Value too large for {digits} digits"),
        ));
    }

    let has_padding = digits.is_multiple_of(2); // Even digit count requires padding
    let total_nibbles = digits_usize + 1 + usize::from(has_padding);

    for byte_idx in 0..expected_bytes {
        let mut byte_val = 0u8;

        // High nibble handling
        let high_nibble_idx = byte_idx * 2;
        if high_nibble_idx < total_nibbles - 1 {
            if has_padding && high_nibble_idx == 0 {
                // Leading padding for even digit counts
            } else {
                let digit_idx = if has_padding {
                    high_nibble_idx - 1
                } else {
                    high_nibble_idx
                };
                if digit_idx >= digits_usize.saturating_sub(digit_count) {
                    let actual_digit_idx = digit_idx - (digits_usize - digit_count);
                    if actual_digit_idx < digit_count {
                        let digit_pos_from_right = digit_count - 1 - actual_digit_idx;
                        let digit = scratch.digit_buffer[digit_pos_from_right];
                        byte_val |= digit << 4;
                    }
                }
            }
        }

        // Low nibble handling
        let low_nibble_idx = high_nibble_idx + 1;
        if low_nibble_idx == total_nibbles - 1 {
            byte_val |= if signed {
                if is_negative { 0x0D } else { 0x0C }
            } else {
                0x0F
            };
        } else if low_nibble_idx < total_nibbles - 1 {
            let digit_idx = if has_padding {
                low_nibble_idx - 1
            } else {
                low_nibble_idx
            };
            if digit_idx >= digits_usize.saturating_sub(digit_count) {
                let actual_digit_idx = digit_idx - (digits_usize - digit_count);
                if actual_digit_idx < digit_count {
                    let digit_pos_from_right = digit_count - 1 - actual_digit_idx;
                    let digit = scratch.digit_buffer[digit_pos_from_right];
                    byte_val |= digit;
                }
            }
        }

        scratch.byte_buffer[byte_idx] = byte_val;
    }

    Ok(std::mem::take(&mut scratch.byte_buffer))
}

/// Decode a packed decimal (COMP-3) directly to a `String`, bypassing the
/// intermediate [`SmallDecimal`] allocation.
///
/// This is a critical performance optimization for COMP-3 JSON conversion.
/// By decoding nibbles and formatting the result in a single pass using the
/// caller-owned scratch buffer, it avoids the `SmallDecimal` -> `String`
/// allocation overhead that caused 94-96% throughput regression in COMP-3
/// processing benchmarks.
///
/// # Arguments
/// * `data` - Raw byte data containing the packed decimal (BCD with trailing sign nibble)
/// * `digits` - Number of decimal digits in the field (1-18)
/// * `scale` - Number of implied decimal places (can be negative for scaling)
/// * `signed` - Whether the field is signed (`true`) or unsigned (`false`)
/// * `scratch` - Reusable scratch buffers; the `string_buffer` is consumed via
///   `std::mem::take` and returned as the result string.
///
/// # Returns
/// The decoded value formatted as a string (e.g. `"123"`, `"-45.67"`, `"0"`).
///
/// # Errors
/// * `CBKD401_COMP3_INVALID_NIBBLE` - if any data nibble is > 9 or the sign
///   nibble is invalid
///
/// # Performance
/// Includes a fast path for single-digit packed decimals (1 byte) and falls
/// back to [`decode_packed_decimal_with_scratch`] plus
/// [`SmallDecimal::format_to_scratch_buffer`] for larger values.
///
/// # See Also
/// * [`decode_packed_decimal`] - Returns a `SmallDecimal` instead of a string
/// * [`decode_packed_decimal_with_scratch`] - Scratch-based decoder returning `SmallDecimal`
/// * [`encode_packed_decimal_with_scratch`] - Scratch-based packed decimal encoder
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_packed_decimal_to_string_with_scratch(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    scratch: &mut ScratchBuffers,
) -> Result<String> {
    // SIMD-friendly sign lookup table for faster branch-free sign detection
    // Index by nibble value: 0=invalid, 1=positive, 2=negative
    const SIGN_TABLE: [u8; 16] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x0-0x9: invalid
        1, 2, 1, 2, 1, 1, // 0xA=pos, 0xB=neg, 0xC=pos, 0xD=neg, 0xE=pos, 0xF=pos
    ];

    // CRITICAL OPTIMIZATION: Direct decode-to-string path to avoid SmallDecimal allocation
    if data.is_empty() {
        scratch.string_buffer.clear();
        scratch.string_buffer.push('0');
        return Ok(std::mem::take(&mut scratch.string_buffer));
    }

    // Fast path for common single-digit packed decimals
    if data.len() == 1 && digits == 1 {
        let byte = data[0];
        let high_nibble = (byte >> 4) & 0x0F;
        let low_nibble = byte & 0x0F;

        let mut is_negative = false;

        // Single digit: high nibble is unused (should be 0), low nibble is sign
        if high_nibble > 9 {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid digit nibble 0x{high_nibble:X}"),
            ));
        }
        let value = i64::from(high_nibble);

        if signed {
            // SIMD-friendly branch-free sign detection
            let sign_code = SIGN_TABLE[usize::from(low_nibble)];
            if sign_code == 0 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!("Invalid sign nibble 0x{low_nibble:X}"),
                ));
            }
            is_negative = sign_code == 2;
        } else if low_nibble != 0xF {
            return Err(Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!("Invalid unsigned sign nibble 0x{low_nibble:X}, expected 0xF"),
            ));
        }

        // Format directly to string without SmallDecimal
        scratch.string_buffer.clear();
        if is_negative && value != 0 {
            scratch.string_buffer.push('-');
        }

        if scale <= 0 {
            // Integer format
            let scaled_value = if scale < 0 {
                value * 10_i64.pow(scale_abs_to_u32(scale))
            } else {
                value
            };
            format_integer_to_buffer(scaled_value, &mut scratch.string_buffer);
        } else {
            // Decimal format
            let divisor = 10_i64.pow(scale_abs_to_u32(scale));
            let integer_part = value / divisor;
            let fractional_part = value % divisor;

            format_integer_to_buffer(integer_part, &mut scratch.string_buffer);
            scratch.string_buffer.push('.');
            format_integer_with_leading_zeros_to_buffer(
                fractional_part,
                scale_abs_to_u32(scale),
                &mut scratch.string_buffer,
            );
        }

        // CRITICAL OPTIMIZATION: Move string content without cloning
        let result = std::mem::take(&mut scratch.string_buffer);
        return Ok(result);
    }

    // Fall back to general case for larger packed decimals
    let decimal = decode_packed_decimal_with_scratch(data, digits, scale, signed, scratch)?;

    // Now format to string using the optimized scratch buffer method
    decimal.format_to_scratch_buffer(scale, &mut scratch.string_buffer);

    // CRITICAL OPTIMIZATION: Move string content without cloning
    let result = std::mem::take(&mut scratch.string_buffer);
    Ok(result)
}

/// Format a binary integer into the caller-owned scratch buffer.
///
/// ## Why scratch?
/// Avoids hot-path allocations in codec routes that emit integers frequently
/// (zoned/packed/binary). This writes into `scratch` and returns that buffer,
/// so callers must reuse the same `ScratchBuffers` instance across a walk.
///
/// ## Contract
/// - No allocations on the hot path
/// - Returns the scratch-backed `String` (valid until next reuse/clear)
#[inline]
#[must_use = "Use the formatted string or continue mutating the scratch buffer"]
pub fn format_binary_int_to_string_with_scratch(
    value: i64,
    scratch: &mut ScratchBuffers,
) -> String {
    scratch.string_buffer.clear();

    if value < 0 {
        scratch.string_buffer.push('-');
        if value == i64::MIN {
            // Avoid overflow when negating i64::MIN
            scratch.string_buffer.push_str("9223372036854775808");
            return std::mem::take(&mut scratch.string_buffer);
        }
        format_integer_to_buffer(-value, &mut scratch.string_buffer);
    } else {
        format_integer_to_buffer(value, &mut scratch.string_buffer);
    }

    std::mem::take(&mut scratch.string_buffer)
}

/// Format an integer to a string buffer with optimized performance
///
/// Provides ultra-fast integer-to-string conversion optimized for COBOL numeric
/// decoding hot paths. Uses manual digit extraction to avoid format macro overhead.
///
/// # Arguments
/// * `value` - Integer value to format
/// * `buffer` - String buffer to append digits to
///
/// # Performance
/// Critical optimization for COMP-3 and zoned decimal JSON conversion. Avoids
/// the overhead of Rust's standard formatting macros through manual digit extraction.
///
/// # Examples
/// ```text
/// let mut buffer = String::new();
/// format_integer_to_buffer(12345, &mut buffer);
/// assert_eq!(buffer, "12345");
/// ```
#[inline]
fn format_integer_to_buffer(value: i64, buffer: &mut String) {
    SmallDecimal::format_integer_manual(value, buffer);
}

/// Format an integer with leading zeros to a string buffer
///
/// Formats an integer with exactly `width` digits, padding with leading zeros
/// if necessary. Optimized for decimal formatting where fractional parts must
/// maintain precise digit counts.
///
/// # Arguments
/// * `value` - Integer value to format
/// * `width` - Number of digits in output (with leading zeros)
/// * `buffer` - String buffer to append formatted digits to
///
/// # Performance
/// Optimized for common COBOL scales (0-4 decimal places) with specialized
/// fast paths. Critical for maintaining COMP-3 decimal precision.
///
/// # Examples
/// ```text
/// let mut buffer = String::new();
/// format_integer_with_leading_zeros_to_buffer(45, 4, &mut buffer);
/// assert_eq!(buffer, "0045");
/// ```
#[inline]
fn format_integer_with_leading_zeros_to_buffer(value: i64, width: u32, buffer: &mut String) {
    SmallDecimal::format_integer_with_leading_zeros(value, width, buffer);
}

/// Decode a zoned decimal directly to a `String`, bypassing the intermediate
/// [`SmallDecimal`] allocation.
///
/// Analogous to [`decode_packed_decimal_to_string_with_scratch`] but for zoned
/// decimal (PIC 9 / PIC S9) fields.  Decodes via
/// [`decode_zoned_decimal_with_scratch`] and then formats the result into the
/// scratch string buffer, avoiding a separate heap allocation.
///
/// # Arguments
/// * `data` - Raw byte data containing the zoned decimal
/// * `digits` - Number of digit characters (field length)
/// * `scale` - Number of implied decimal places (can be negative for scaling)
/// * `signed` - Whether the field carries a sign (overpunch in last byte)
/// * `codepage` - Character encoding (ASCII or EBCDIC variant)
/// * `blank_when_zero` - If `true`, all-space fields decode as `"0"`
/// * `scratch` - Reusable scratch buffers; the `string_buffer` is consumed via
///   `std::mem::take` and returned as the result string.
///
/// # Returns
/// The decoded value formatted as a string (e.g. `"123"`, `"-45.67"`, `"0"`).
///
/// # Policy
/// Mirrors [`decode_zoned_decimal_with_scratch`], inheriting its default
/// preferred-zero handling for EBCDIC data.
///
/// # Errors
/// * `CBKD411_ZONED_BAD_SIGN` - if the zone nibbles or sign are invalid
///
/// # See Also
/// * [`decode_zoned_decimal`] - Returns a `SmallDecimal` instead of a string
/// * [`decode_zoned_decimal_with_scratch`] - Scratch-based decoder returning `SmallDecimal`
/// * [`decode_packed_decimal_to_string_with_scratch`] - Equivalent for packed decimals
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_zoned_decimal_to_string_with_scratch(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
    scratch: &mut ScratchBuffers,
) -> Result<String> {
    // First decode to SmallDecimal using existing optimized decoder
    let decimal = decode_zoned_decimal_with_scratch(
        data,
        digits,
        scale,
        signed,
        codepage,
        blank_when_zero,
        scratch,
    )?;

    // Special-case integer zoned decimals for digit padding consistency
    if scale == 0 && !blank_when_zero {
        if decimal.value == 0 {
            scratch.string_buffer.clear();
            scratch.string_buffer.push('0');
        } else {
            scratch.string_buffer.clear();
            if decimal.negative && decimal.value != 0 {
                scratch.string_buffer.push('-');
            }

            let magnitude = if decimal.scale < 0 {
                decimal.value * 10_i64.pow(scale_abs_to_u32(decimal.scale))
            } else {
                decimal.value
            };

            SmallDecimal::format_integer_with_leading_zeros(
                magnitude,
                u32::from(digits),
                &mut scratch.string_buffer,
            );
        }

        return Ok(std::mem::take(&mut scratch.string_buffer));
    }

    // Fallback to general fixed-scale formatting using scratch buffer
    decimal.format_to_scratch_buffer(scale, &mut scratch.string_buffer);
    Ok(std::mem::take(&mut scratch.string_buffer))
}

// =============================================================================
// Floating-Point Codecs (COMP-1 / COMP-2)
// =============================================================================

const IBM_HEX_EXPONENT_BIAS: i32 = 64;
const IBM_HEX_FRACTION_MIN: f64 = 1.0 / 16.0;
const IBM_HEX_SINGLE_FRACTION_BITS: u32 = 24;
const IBM_HEX_DOUBLE_FRACTION_BITS: u32 = 56;

#[inline]
fn validate_float_buffer_len(data: &[u8], required: usize, usage: &str) -> Result<()> {
    if data.len() < required {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!("{usage} requires {required} bytes, got {}", data.len()),
        ));
    }
    Ok(())
}

#[inline]
fn validate_float_encode_buffer_len(buffer: &[u8], required: usize, usage: &str) -> Result<()> {
    if buffer.len() < required {
        return Err(Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            format!(
                "{usage} requires {required} bytes, buffer has {}",
                buffer.len()
            ),
        ));
    }
    Ok(())
}

#[inline]
#[allow(clippy::cast_precision_loss)]
fn decode_ibm_hex_to_f64(sign: bool, exponent_raw: u8, fraction_bits: u64, bits: u32) -> f64 {
    if exponent_raw == 0 && fraction_bits == 0 {
        return if sign { -0.0 } else { 0.0 };
    }

    let exponent = i32::from(exponent_raw) - IBM_HEX_EXPONENT_BIAS;
    let divisor = 2_f64.powi(i32::try_from(bits).unwrap_or(0));
    let fraction = (fraction_bits as f64) / divisor;
    let magnitude = fraction * 16_f64.powi(exponent);
    if sign { -magnitude } else { magnitude }
}

#[inline]
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn encode_f64_to_ibm_hex_parts(value: f64, bits: u32) -> Result<(u8, u64)> {
    if !value.is_finite() {
        return Err(Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            "IBM hex float encoding requires finite values",
        ));
    }

    if value == 0.0 {
        return Ok((0, 0));
    }

    let mut exponent = IBM_HEX_EXPONENT_BIAS;
    let mut fraction = value.abs();

    while fraction < IBM_HEX_FRACTION_MIN {
        fraction *= 16.0;
        exponent -= 1;
        if exponent <= 0 {
            return Ok((0, 0));
        }
    }

    while fraction >= 1.0 {
        fraction /= 16.0;
        exponent += 1;
        if exponent >= 128 {
            return Err(Error::new(
                ErrorCode::CBKE510_NUMERIC_OVERFLOW,
                "IBM hex float exponent overflow",
            ));
        }
    }

    let scale = 2_f64.powi(i32::try_from(bits).unwrap_or(0));
    let mut fraction_bits = (fraction * scale).round() as u64;
    let full_scale = 1_u64 << bits;
    if fraction_bits >= full_scale {
        // Carry from rounding: re-normalize to 0x1... and bump exponent.
        fraction_bits = 1_u64 << (bits - 4);
        exponent += 1;
        if exponent >= 128 {
            return Err(Error::new(
                ErrorCode::CBKE510_NUMERIC_OVERFLOW,
                "IBM hex float exponent overflow",
            ));
        }
    }

    Ok((u8::try_from(exponent).unwrap_or(0), fraction_bits))
}

/// Decode a COMP-1 field in IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 4 bytes.
#[inline]
pub fn decode_float_single_ieee_be(data: &[u8]) -> Result<f32> {
    validate_float_buffer_len(data, 4, "COMP-1")?;
    let bytes: [u8; 4] = [data[0], data[1], data[2], data[3]];
    Ok(f32::from_be_bytes(bytes))
}

/// Decode a COMP-2 field in IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 8 bytes.
#[inline]
pub fn decode_float_double_ieee_be(data: &[u8]) -> Result<f64> {
    validate_float_buffer_len(data, 8, "COMP-2")?;
    let bytes: [u8; 8] = [
        data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
    ];
    Ok(f64::from_be_bytes(bytes))
}

/// Decode a COMP-1 field in IBM hexadecimal floating-point format.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 4 bytes.
#[inline]
pub fn decode_float_single_ibm_hex(data: &[u8]) -> Result<f32> {
    validate_float_buffer_len(data, 4, "COMP-1")?;
    let word = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
    let sign = (word & 0x8000_0000) != 0;
    let exponent_raw = ((word >> 24) & 0x7F) as u8;
    let fraction_bits = u64::from(word & 0x00FF_FFFF);
    let value = decode_ibm_hex_to_f64(
        sign,
        exponent_raw,
        fraction_bits,
        IBM_HEX_SINGLE_FRACTION_BITS,
    );
    #[allow(clippy::cast_possible_truncation)]
    {
        Ok(value as f32)
    }
}

/// Decode a COMP-2 field in IBM hexadecimal floating-point format.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 8 bytes.
#[inline]
pub fn decode_float_double_ibm_hex(data: &[u8]) -> Result<f64> {
    validate_float_buffer_len(data, 8, "COMP-2")?;
    let word = u64::from_be_bytes([
        data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
    ]);
    let sign = (word & 0x8000_0000_0000_0000) != 0;
    let exponent_raw = ((word >> 56) & 0x7F) as u8;
    let fraction_bits = word & 0x00FF_FFFF_FFFF_FFFF;
    Ok(decode_ibm_hex_to_f64(
        sign,
        exponent_raw,
        fraction_bits,
        IBM_HEX_DOUBLE_FRACTION_BITS,
    ))
}

/// Decode a COMP-1 float with explicit format selection.
///
/// # Errors
/// Returns format-specific decode errors.
#[inline]
pub fn decode_float_single_with_format(data: &[u8], format: FloatFormat) -> Result<f32> {
    match format {
        FloatFormat::IeeeBigEndian => decode_float_single_ieee_be(data),
        FloatFormat::IbmHex => decode_float_single_ibm_hex(data),
    }
}

/// Decode a COMP-2 float with explicit format selection.
///
/// # Errors
/// Returns format-specific decode errors.
#[inline]
pub fn decode_float_double_with_format(data: &[u8], format: FloatFormat) -> Result<f64> {
    match format {
        FloatFormat::IeeeBigEndian => decode_float_double_ieee_be(data),
        FloatFormat::IbmHex => decode_float_double_ibm_hex(data),
    }
}

/// Decode a COMP-1 float with default IEEE-754 big-endian interpretation.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 4 bytes.
#[inline]
pub fn decode_float_single(data: &[u8]) -> Result<f32> {
    decode_float_single_ieee_be(data)
}

/// Decode a COMP-2 float with default IEEE-754 big-endian interpretation.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 8 bytes.
#[inline]
pub fn decode_float_double(data: &[u8]) -> Result<f64> {
    decode_float_double_ieee_be(data)
}

/// Encode a COMP-1 value in IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKE510_NUMERIC_OVERFLOW` if the buffer has fewer than 4 bytes.
#[inline]
pub fn encode_float_single_ieee_be(value: f32, buffer: &mut [u8]) -> Result<()> {
    validate_float_encode_buffer_len(buffer, 4, "COMP-1")?;
    let bytes = value.to_be_bytes();
    buffer[..4].copy_from_slice(&bytes);
    Ok(())
}

/// Encode a COMP-2 value in IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKE510_NUMERIC_OVERFLOW` if the buffer has fewer than 8 bytes.
#[inline]
pub fn encode_float_double_ieee_be(value: f64, buffer: &mut [u8]) -> Result<()> {
    validate_float_encode_buffer_len(buffer, 8, "COMP-2")?;
    let bytes = value.to_be_bytes();
    buffer[..8].copy_from_slice(&bytes);
    Ok(())
}

/// Encode a COMP-1 value in IBM hexadecimal floating-point format.
///
/// # Errors
/// Returns:
/// - `CBKE510_NUMERIC_OVERFLOW` if the buffer is too small
/// - `CBKE510_NUMERIC_OVERFLOW` for non-finite values or exponent overflow
#[inline]
pub fn encode_float_single_ibm_hex(value: f32, buffer: &mut [u8]) -> Result<()> {
    validate_float_encode_buffer_len(buffer, 4, "COMP-1")?;
    let sign = value.is_sign_negative();
    let (exponent_raw, fraction_bits) =
        encode_f64_to_ibm_hex_parts(f64::from(value), IBM_HEX_SINGLE_FRACTION_BITS)?;
    let sign_bit = if sign { 0x8000_0000 } else { 0 };
    let fraction_low = u32::try_from(fraction_bits & 0x00FF_FFFF).map_err(|_| {
        Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            "IBM hex fraction overflow for COMP-1",
        )
    })?;
    let word = sign_bit | (u32::from(exponent_raw) << 24) | fraction_low;
    buffer[..4].copy_from_slice(&word.to_be_bytes());
    Ok(())
}

/// Encode a COMP-2 value in IBM hexadecimal floating-point format.
///
/// # Errors
/// Returns:
/// - `CBKE510_NUMERIC_OVERFLOW` if the buffer is too small
/// - `CBKE510_NUMERIC_OVERFLOW` for non-finite values or exponent overflow
#[inline]
pub fn encode_float_double_ibm_hex(value: f64, buffer: &mut [u8]) -> Result<()> {
    validate_float_encode_buffer_len(buffer, 8, "COMP-2")?;
    let sign = value.is_sign_negative();
    let (exponent_raw, fraction_bits) =
        encode_f64_to_ibm_hex_parts(value, IBM_HEX_DOUBLE_FRACTION_BITS)?;
    let sign_bit = if sign { 0x8000_0000_0000_0000 } else { 0 };
    let word = sign_bit | (u64::from(exponent_raw) << 56) | (fraction_bits & 0x00FF_FFFF_FFFF_FFFF);
    buffer[..8].copy_from_slice(&word.to_be_bytes());
    Ok(())
}

/// Encode a COMP-1 float with explicit format selection.
///
/// # Errors
/// Returns format-specific encode errors.
#[inline]
pub fn encode_float_single_with_format(
    value: f32,
    buffer: &mut [u8],
    format: FloatFormat,
) -> Result<()> {
    match format {
        FloatFormat::IeeeBigEndian => encode_float_single_ieee_be(value, buffer),
        FloatFormat::IbmHex => encode_float_single_ibm_hex(value, buffer),
    }
}

/// Encode a COMP-2 float with explicit format selection.
///
/// # Errors
/// Returns format-specific encode errors.
#[inline]
pub fn encode_float_double_with_format(
    value: f64,
    buffer: &mut [u8],
    format: FloatFormat,
) -> Result<()> {
    match format {
        FloatFormat::IeeeBigEndian => encode_float_double_ieee_be(value, buffer),
        FloatFormat::IbmHex => encode_float_double_ibm_hex(value, buffer),
    }
}

/// Encode a COMP-1 float with default IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKE510_NUMERIC_OVERFLOW` if the buffer has fewer than 4 bytes.
#[inline]
pub fn encode_float_single(value: f32, buffer: &mut [u8]) -> Result<()> {
    encode_float_single_ieee_be(value, buffer)
}

/// Encode a COMP-2 float with default IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKE510_NUMERIC_OVERFLOW` if the buffer has fewer than 8 bytes.
#[inline]
pub fn encode_float_double(value: f64, buffer: &mut [u8]) -> Result<()> {
    encode_float_double_ieee_be(value, buffer)
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use crate::zoned_overpunch::{ZeroSignPolicy, encode_overpunch_byte, is_valid_overpunch};
    use proptest::prelude::*;
    use proptest::test_runner::RngSeed;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    fn proptest_case_count() -> u32 {
        option_env!("PROPTEST_CASES")
            .and_then(|s| s.parse().ok())
            .unwrap_or(256)
    }

    fn numeric_proptest_config() -> ProptestConfig {
        let mut cfg = ProptestConfig {
            cases: proptest_case_count(),
            max_shrink_time: 0,
            ..ProptestConfig::default()
        };

        if let Ok(seed_value) = std::env::var("PROPTEST_SEED")
            && !seed_value.is_empty()
        {
            let parsed_seed = seed_value.parse::<u64>().unwrap_or_else(|_| {
                let mut hasher = DefaultHasher::new();
                seed_value.hash(&mut hasher);
                hasher.finish()
            });
            cfg.rng_seed = RngSeed::Fixed(parsed_seed);
        }

        cfg
    }

    #[test]
    fn test_small_decimal_normalization() {
        let mut decimal = SmallDecimal::new(0, 2, true);
        decimal.normalize();
        assert!(!decimal.negative); // -0 should become 0
    }

    #[test]
    fn test_small_decimal_formatting() {
        // Integer format (scale=0)
        let decimal = SmallDecimal::new(123, 0, false);
        assert_eq!(decimal.to_string(), "123");

        // Decimal format with fixed scale
        let decimal = SmallDecimal::new(12345, 2, false);
        assert_eq!(decimal.to_string(), "123.45");

        // Negative decimal
        let decimal = SmallDecimal::new(12345, 2, true);
        assert_eq!(decimal.to_string(), "-123.45");
    }

    #[test]
    fn test_zero_with_scale_preserves_decimal_places() {
        // Zero with scale=2 must produce "0.00" (not "0")
        let decimal = SmallDecimal::new(0, 2, false);
        assert_eq!(decimal.to_string(), "0.00");

        // Zero with scale=1
        let decimal = SmallDecimal::new(0, 1, false);
        assert_eq!(decimal.to_string(), "0.0");

        // Zero with scale=4 and negative flag (normalizes sign away)
        let decimal = SmallDecimal::new(0, 4, true);
        assert_eq!(decimal.to_string(), "0.0000");
    }

    proptest! {
        #![proptest_config(numeric_proptest_config())]
        #[test]
        fn prop_zoned_digit_buffer_contains_only_digits(
            digits_vec in prop::collection::vec(0u8..=9, 1..=12),
            signed in any::<bool>(),
            allow_negative in any::<bool>(),
            codepage in prop_oneof![
                Just(Codepage::ASCII),
                Just(Codepage::CP037),
                Just(Codepage::CP273),
                Just(Codepage::CP500),
                Just(Codepage::CP1047),
                Just(Codepage::CP1140),
            ],
            policy in prop_oneof![Just(ZeroSignPolicy::Positive), Just(ZeroSignPolicy::Preferred)],
        ) {
            let digit_count = u16::try_from(digits_vec.len()).expect("vector length <= 12");
            let mut bytes = Vec::with_capacity(digits_vec.len());

            for digit in digits_vec.iter().take(digits_vec.len().saturating_sub(1)) {
                let byte = if codepage.is_ascii() {
                    0x30 + digit
                } else {
                    0xF0 + digit
                };
                bytes.push(byte);
            }

            let is_negative = signed && allow_negative;
            let last_digit = *digits_vec.last().expect("vector is non-empty");
            let last_byte = if signed {
                let encoded = encode_overpunch_byte(last_digit, is_negative, codepage, policy)
                    .expect("valid overpunch for digit 0-9");
                prop_assume!(is_valid_overpunch(encoded, codepage));
                encoded
            } else if codepage.is_ascii() {
                0x30 + last_digit
            } else {
                0xF0 + last_digit
            };
            bytes.push(last_byte);

            let mut scratch = ScratchBuffers::new();
            let _ = decode_zoned_decimal_with_scratch(
                &bytes,
                digit_count,
                0,
                signed,
                codepage,
                false,
                &mut scratch,
            ).expect("decoding constructed zoned bytes should succeed");

            prop_assert_eq!(scratch.digit_buffer.len(), digits_vec.len());
            prop_assert!(scratch.digit_buffer.iter().all(|&d| d <= 9));
            prop_assert_eq!(&scratch.digit_buffer[..], &digits_vec[..]);
        }
    }

    #[test]
    fn test_zoned_decimal_blank_when_zero() {
        // EBCDIC spaces (0x40)
        let data = vec![0x40, 0x40, 0x40];
        let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::CP037, true).unwrap();
        assert_eq!(result.to_string(), "0");

        // ASCII spaces
        let data = vec![b' ', b' ', b' '];
        let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result.to_string(), "0");
    }

    #[test]
    fn test_packed_decimal_signs() {
        // Positive packed decimal: 123C (123 positive)
        let data = vec![0x12, 0x3C];
        let result = decode_packed_decimal(&data, 3, 0, true).unwrap();
        assert_eq!(result.to_string(), "123");

        // Negative packed decimal: 123D (123 negative)
        let data = vec![0x12, 0x3D];
        let result = decode_packed_decimal(&data, 3, 0, true).unwrap();
        assert_eq!(result.to_string(), "-123");

        // Test the failing case from property tests: -11 (2 digits)
        // Test that round-trip encoding/decoding preserves the sign
        let encoded = encode_packed_decimal("-11", 2, 0, true).unwrap();
        let result = decode_packed_decimal(&encoded, 2, 0, true).unwrap();
        assert_eq!(
            result.to_string(),
            "-11",
            "Failed to round-trip -11 correctly"
        );

        // Test that the old buggy format is now rejected
        let data = vec![0x11, 0xDD]; // Invalid format with sign in both nibbles
        let result = decode_packed_decimal(&data, 2, 0, true);
        assert!(
            result.is_err(),
            "Should reject invalid format with sign in both nibbles"
        );
    }

    #[test]
    fn test_binary_int_big_endian() {
        // 16-bit big-endian: 0x0123 = 291
        let data = vec![0x01, 0x23];
        let result = decode_binary_int(&data, 16, false).unwrap();
        assert_eq!(result, 291);

        // 32-bit big-endian: 0x01234567 = 19088743
        let data = vec![0x01, 0x23, 0x45, 0x67];
        let result = decode_binary_int(&data, 32, false).unwrap();
        assert_eq!(result, 19_088_743);
    }

    #[test]
    fn test_alphanumeric_encoding() {
        // ASCII encoding with padding
        let result = encode_alphanumeric("HELLO", 10, Codepage::ASCII).unwrap();
        assert_eq!(result, b"HELLO     ");

        // Over-length should error
        let result = encode_alphanumeric("HELLO WORLD", 5, Codepage::ASCII);
        assert!(result.is_err());
    }

    #[test]
    fn test_bwz_policy() {
        // Zero values should trigger BWZ
        assert!(should_encode_as_blank_when_zero("0", true));
        assert!(should_encode_as_blank_when_zero("0.00", true));
        assert!(should_encode_as_blank_when_zero("0.000", true));

        // Non-zero values should not trigger BWZ
        assert!(!should_encode_as_blank_when_zero("1", true));
        assert!(!should_encode_as_blank_when_zero("0.01", true));

        // BWZ disabled should never trigger
        assert!(!should_encode_as_blank_when_zero("0", false));
    }

    #[test]
    fn test_binary_width_mapping() {
        // Test digit-to-width mapping (NORMATIVE)
        assert_eq!(get_binary_width_from_digits(1), 16); // ≤4 → 2B
        assert_eq!(get_binary_width_from_digits(4), 16); // ≤4 → 2B
        assert_eq!(get_binary_width_from_digits(5), 32); // 5-9 → 4B
        assert_eq!(get_binary_width_from_digits(9), 32); // 5-9 → 4B
        assert_eq!(get_binary_width_from_digits(10), 64); // 10-18 → 8B
        assert_eq!(get_binary_width_from_digits(18), 64); // 10-18 → 8B
    }

    #[test]
    fn test_explicit_binary_width_validation() {
        // Valid explicit widths
        assert_eq!(validate_explicit_binary_width(1).unwrap(), 8);
        assert_eq!(validate_explicit_binary_width(2).unwrap(), 16);
        assert_eq!(validate_explicit_binary_width(4).unwrap(), 32);
        assert_eq!(validate_explicit_binary_width(8).unwrap(), 64);

        // Invalid explicit widths
        assert!(validate_explicit_binary_width(3).is_err());
        assert!(validate_explicit_binary_width(16).is_err());
    }

    #[test]
    fn test_zoned_decimal_with_bwz() {
        // BWZ enabled with zero value should return spaces
        let result =
            encode_zoned_decimal_with_bwz("0", 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result, vec![b' ', b' ', b' ']);

        // BWZ disabled with zero value should return normal encoding
        let result =
            encode_zoned_decimal_with_bwz("0", 3, 0, false, Codepage::ASCII, false).unwrap();
        assert_eq!(result, vec![0x30, 0x30, 0x30]); // ASCII "000"

        // Non-zero value should return normal encoding regardless of BWZ
        let result =
            encode_zoned_decimal_with_bwz("123", 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result, vec![0x31, 0x32, 0x33]); // ASCII "123"
    }

    #[test]
    fn test_error_handling_invalid_numeric_inputs() {
        // Test packed decimal with invalid input - should return specific CBKD error
        let invalid_data = vec![0xFF]; // Invalid packed decimal
        let result = decode_packed_decimal(&invalid_data, 2, 0, false);
        assert!(
            result.is_err(),
            "Invalid packed decimal should return error"
        );

        let error = result.unwrap_err();
        assert!(
            error.to_string().contains("CBKD"),
            "Error should be CBKD code"
        );

        // Test binary int with insufficient data
        let short_data = vec![0x01]; // Only 1 byte for 4-byte int
        let result = decode_binary_int(&short_data, 32, false);
        assert!(
            result.is_err(),
            "Insufficient binary data should return error"
        );

        // Test zoned decimal with invalid characters
        let invalid_zoned = b"12X"; // Contains non-digit
        let result = decode_zoned_decimal(invalid_zoned, 3, 0, false, Codepage::ASCII, false);
        assert!(result.is_err(), "Invalid zoned decimal should return error");

        // Test alphanumeric encoding with oversized input
        let result = encode_alphanumeric("TOOLONGFORFIELD", 5, Codepage::ASCII);
        assert!(
            result.is_err(),
            "Oversized alphanumeric should return error"
        );

        let error = result.unwrap_err();
        assert!(
            error.to_string().contains("CBKE"),
            "Error should be CBKE code"
        );
    }

    #[test]
    fn test_boundary_conditions_numeric_operations() {
        // Test maximum values for different data types

        // Test maximum packed decimal
        let max_packed_bytes = vec![0x99, 0x9C]; // 999 positive (3 digits)
        let result = decode_packed_decimal(&max_packed_bytes, 3, 0, true);
        assert!(
            result.is_ok(),
            "Valid maximum packed decimal should succeed"
        );

        // Test zero packed decimal
        let zero_packed = vec![0x00, 0x0C]; // 00 positive
        let result = decode_packed_decimal(&zero_packed, 2, 0, true);
        assert!(result.is_ok(), "Zero packed decimal should succeed");

        // Test edge case with maximum binary values
        let max_u16_bytes = vec![0xFF, 0xFF];
        let result = decode_binary_int(&max_u16_bytes, 16, false);
        assert!(result.is_ok(), "Maximum unsigned 16-bit should succeed");

        let max_signed_16_bytes = vec![0x7F, 0xFF];
        let result = decode_binary_int(&max_signed_16_bytes, 16, true);
        assert!(result.is_ok(), "Maximum signed 16-bit should succeed");

        // Test edge case with minimum signed values
        let min_i16_bytes = vec![0x80, 0x00];
        let result = decode_binary_int(&min_i16_bytes, 16, true);
        assert!(result.is_ok(), "Minimum signed 16-bit should succeed");
    }

    #[test]
    fn test_comp3_decimal_scale_fix() {
        // Test case for PIC S9(7)V99 COMP-3 with decimal positioning fix
        let input_value = "123.45";
        let digits = 9; // 7 integer + 2 decimal = 9 total digits
        let scale = 2; // 2 decimal places
        let signed = true;

        // Test round-trip encoding/decoding
        let encoded_data = encode_packed_decimal(input_value, digits, scale, signed).unwrap();
        let decoded = decode_packed_decimal(&encoded_data, digits, scale, signed).unwrap();

        assert_eq!(decoded.to_string(), "123.45", "COMP-3 round-trip failed");

        // Test negative case
        let negative_value = "-999.99";
        let encoded_neg = encode_packed_decimal(negative_value, digits, scale, signed).unwrap();
        let decoded_neg = decode_packed_decimal(&encoded_neg, digits, scale, signed).unwrap();

        assert_eq!(
            decoded_neg.to_string(),
            "-999.99",
            "Negative COMP-3 round-trip failed"
        );
    }

    #[test]
    fn test_error_path_coverage_arithmetic_operations() {
        // Test SmallDecimal creation and basic operations
        let decimal = SmallDecimal::new(i64::MAX, 0, false);
        assert_eq!(decimal.value, i64::MAX);
        assert_eq!(decimal.scale, 0);
        assert!(!decimal.negative);

        // Test boundary conditions for large values
        let large_decimal = SmallDecimal::new(999_999_999, 0, false);
        assert_eq!(large_decimal.value, 999_999_999);

        // Test boundary conditions for scale normalization
        let mut small_decimal = SmallDecimal::new(1, 10, false);
        small_decimal.normalize(); // Should handle high scale
        assert!(small_decimal.scale >= 0);

        // Test signed/unsigned conversions with boundary values
        let negative_decimal = SmallDecimal::new(-1, 0, true);
        assert!(
            negative_decimal.is_negative(),
            "Signed negative should be negative"
        );

        let positive_decimal = SmallDecimal::new(1, 0, false);
        assert!(
            !positive_decimal.is_negative(),
            "Unsigned should not be negative"
        );
    }
}
