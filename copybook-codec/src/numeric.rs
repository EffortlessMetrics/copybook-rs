//! Numeric type codecs for COBOL data
//!
//! This module implements encoding and decoding for zoned decimal,
//! packed decimal, and binary integer types.

use crate::memory::ScratchBuffers;
use crate::options::{Codepage, ZonedEncodingFormat};
use crate::zoned_overpunch::{ZeroSignPolicy, encode_overpunch_byte};
use copybook_core::{Error, ErrorCode, Result};
use std::convert::TryFrom;
use tracing::warn;

/// Nibble zones for ASCII/EBCDIC digits (high bits in zoned bytes).
const ASCII_DIGIT_ZONE: u8 = 0x3; // ASCII '0'..'9' => 0x30..0x39
const EBCDIC_DIGIT_ZONE: u8 = 0xF; // EBCDIC '0'..'9' => 0xF0..0xF9

/// Lookup table for 2-digit pairs to speed up integer formatting
/// "00", "01", ..., "99" stored as 2 bytes each
const DIGIT_PAIRS: &[u8; 200] = b"\
00010203040506070809\
10111213141516171819\
20212223242526272829\
30313233343536373839\
40414243444546474849\
50515253545556575859\
60616263646566676869\
70717273747576777879\
80818283848586878889\
90919293949596979899";

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
/// ```ignore
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
/// ```ignore
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
    /// Returns an error if detection fails (currently never fails).
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
    /// Special case: zero values with scale > 0 are normalized to "0" (no decimal places)
    /// to address packed decimal zero representation inconsistency.
    #[allow(clippy::inherent_to_string)] // Intentional - this is a specific numeric formatting
    #[inline]
    #[must_use = "Use the formatted string output"]
    pub fn to_string(&self) -> String {
        // Handle zero normalization special case first
        if self.is_zero_value() && self.scale > 0 {
            return "0".to_string();
        }

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
        // Optimization: Use manual formatting to avoid write! overhead
        Self::format_integer_manual(scaled_value, result);
    }

    /// Append decimal format with exactly `scale` digits after decimal point
    fn append_decimal_format(&self, result: &mut String) {
        let divisor = 10_i64.pow(scale_abs_to_u32(self.scale));
        let integer_part = self.value / divisor;
        let fractional_part = self.value % divisor;

        // Optimization: Use manual formatting to avoid write! overhead
        Self::format_integer_manual(integer_part, result);
        result.push('.');
        Self::format_integer_with_leading_zeros(
            fractional_part,
            scale_abs_to_u32(self.scale),
            result,
        );
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
        // Integer format - validate scale is 0 (NORMATIVE)
        if expected_scale != 0 {
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
            // Optimization: Use manual formatting to avoid write! overhead
            Self::format_integer_manual(scaled_value, &mut result);
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(scale_abs_to_u32(scale));
            let integer_part = self.value / divisor;
            let fractional_part = self.value % divisor;

            // Optimization: Use manual formatting to avoid write! overhead
            Self::format_integer_manual(integer_part, &mut result);
            result.push('.');
            Self::format_integer_with_leading_zeros(
                fractional_part,
                scale_abs_to_u32(scale),
                &mut result,
            );
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
    fn format_integer_manual(value: i64, buffer: &mut String) {
        if value == 0 {
            buffer.push('0');
            return;
        }

        let mut value = value.unsigned_abs();

        // Optimized formatting with fewer divisions for common cases
        if value < 100 {
            // Fast path for 1-2 digit numbers (very common in COMP-3)
            if value < 10 {
                buffer.push(char::from(b'0' + value as u8));
            } else {
                let offset = (value as usize) * 2;
                buffer.push(char::from(DIGIT_PAIRS[offset]));
                buffer.push(char::from(DIGIT_PAIRS[offset + 1]));
            }
            return;
        }

        // Use a small stack buffer for digits for larger numbers
        let mut digits = [0u8; 20]; // More than enough for i64::MAX
        let mut count = 0;

        // Process 2 digits at a time to reduce divisions
        while value >= 100 {
            let rem = (value % 100) as usize;
            value /= 100;

            let offset = rem * 2;
            // Push digits in reverse order for later reversal
            digits[count] = DIGIT_PAIRS[offset + 1];
            digits[count + 1] = DIGIT_PAIRS[offset];
            count += 2;
        }

        if value > 0 {
            if value < 10 {
                digits[count] = b'0' + (value as u8);
                count += 1;
            } else {
                let offset = (value as usize) * 2;
                digits[count] = DIGIT_PAIRS[offset + 1];
                digits[count + 1] = DIGIT_PAIRS[offset];
                count += 2;
            }
        }

        // Add digits in reverse order
        for i in (0..count).rev() {
            buffer.push(char::from(digits[i]));
        }
    }

    /// Ultra-fast manual integer formatting with leading zeros
    #[inline]
    fn format_integer_with_leading_zeros(value: i64, width: u32, buffer: &mut String) {
        let mut value = value.unsigned_abs();

        // Optimized for common small widths (most COMP-3 scales are 0-4)
        if width <= 4 && value < 10000 {
            match width {
                1 => {
                    buffer.push(char::from(b'0' + value as u8));
                }
                2 => {
                    let offset = (value as usize) * 2;
                    buffer.push(char::from(DIGIT_PAIRS[offset]));
                    buffer.push(char::from(DIGIT_PAIRS[offset + 1]));
                }
                3 => {
                    let hundreds = value / 100;
                    let rem = (value % 100) as usize;
                    buffer.push(char::from(b'0' + hundreds as u8));

                    let offset = rem * 2;
                    buffer.push(char::from(DIGIT_PAIRS[offset]));
                    buffer.push(char::from(DIGIT_PAIRS[offset + 1]));
                }
                4 => {
                    let hundreds = (value / 100) as usize;
                    let rem = (value % 100) as usize;

                    let offset1 = hundreds * 2;
                    buffer.push(char::from(DIGIT_PAIRS[offset1]));
                    buffer.push(char::from(DIGIT_PAIRS[offset1 + 1]));

                    let offset2 = rem * 2;
                    buffer.push(char::from(DIGIT_PAIRS[offset2]));
                    buffer.push(char::from(DIGIT_PAIRS[offset2 + 1]));
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

        // Process 2 digits at a time to reduce divisions
        while value >= 100 {
            let rem = (value % 100) as usize;
            value /= 100;

            let offset = rem * 2;
            digits[count] = DIGIT_PAIRS[offset + 1];
            digits[count + 1] = DIGIT_PAIRS[offset];
            count += 2;

            if value == 0 && count >= target_width {
                break;
            }
            if count >= 20 {
                break;
            }
        }

        // Handle remaining < 100 part
        if count < target_width || value > 0 {
             if value < 10 {
                digits[count] = b'0' + (value as u8);
                count += 1;
            } else {
                let offset = (value as usize) * 2;
                digits[count] = DIGIT_PAIRS[offset + 1];
                digits[count + 1] = DIGIT_PAIRS[offset];
                count += 2;
            }
        }

        // Pad with leading zeros if needed (count is guaranteed <= 20)
        while count < target_width {
            digits[count] = b'0';
            count += 1;
        }

        // Add digits in reverse order
        for i in (0..count).rev() {
            buffer.push(char::from(digits[i]));
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
/// # Policy
/// Applies the codec default: ASCII uses `ZeroSignPolicy::Positive`; EBCDIC zeros normalize via `ZeroSignPolicy::Preferred`.
///
/// # Errors
/// Returns an error if the zoned decimal data is invalid or contains bad sign zones.
/// All errors include proper context information (`record_index`, `field_path`, `byte_offset`).
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

/// Decode zoned decimal field with encoding detection and preservation
///
/// Returns both the decoded decimal and encoding information for preservation.
///
/// # Policy
/// Mirrors [`decode_zoned_decimal`], defaulting to preferred-zero handling for EBCDIC unless a preserved format dictates otherwise.
///
/// # Errors
/// Returns an error if the zoned decimal data is invalid or contains bad sign zones.
/// All errors include proper context information (`record_index`, `field_path`, `byte_offset`).
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

/// Decode packed decimal field with comprehensive error context
///
/// # Errors
/// Returns an error if the packed decimal data contains invalid nibbles.
/// All errors include proper context information (`record_index`, `field_path`, `byte_offset`).
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
/// where COMP-3 fields are 1-5 bytes and well-formed.
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

#[inline]
fn decode_packed_fast_len1(
    byte: u8,
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
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

#[inline]
fn decode_packed_fast_len2(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
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

#[inline]
fn decode_packed_fast_len3(data: &[u8], scale: i16, signed: bool) -> Result<SmallDecimal> {
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

/// Decode binary integer field
///
/// # Errors
/// Returns an error if the binary data is invalid or the field size is unsupported.
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
/// # Policy
/// Applies `ZeroSignPolicy::Positive` for ASCII and `ZeroSignPolicy::Preferred` for EBCDIC when no overrides are provided.
///
/// # Errors
/// Returns an error if the value cannot be encoded as a zoned decimal with the specified parameters.
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
/// # Policy
/// Resolves `ZeroSignPolicy` from `encoding_override` first; when unset or `Auto`, falls back to the code page defaults.
///
/// # Errors
/// Returns an error if the value cannot be encoded as a zoned decimal with the specified parameters.
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
/// # Policy
/// Callers provide the resolved policy in precedence order: override → preserved → preferred for the target code page.
///
/// # Errors
/// Returns an error if the value cannot be encoded as a zoned decimal with the specified parameters.
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

/// Encode packed decimal field
///
/// # Errors
/// Returns an error if the value cannot be encoded as a packed decimal with the specified parameters
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

/// Encode binary integer field
///
/// # Errors
/// Returns an error if the value is out of range for the specified bit width.
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

/// Encode alphanumeric field with space padding
///
/// # Errors
/// Returns an error if the text is too long for the field.
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

/// Apply BLANK WHEN ZERO encoding policy
///
/// Returns true if the value should be encoded as spaces instead of zeros
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

/// Encode zoned decimal with BWZ policy
///
/// # Errors
/// Returns an error if the value cannot be encoded.
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

/// Get binary width mapping based on PIC digits (NORMATIVE)
///
/// Maps digits to width: ≤4→2B, 5-9→4B, 10-18→8B
#[inline]
#[must_use]
pub fn get_binary_width_from_digits(digits: u16) -> u16 {
    match digits {
        1..=4 => 16, // 2 bytes
        5..=9 => 32, // 4 bytes
        _ => 64,     // 8 bytes for larger values
    }
}

/// Validate explicit USAGE BINARY(n) width (NORMATIVE)
///
/// Accept explicit USAGE BINARY(n) for n ∈ {1,2,4,8}
///
/// # Errors
/// Returns an error when the requested width is not one of the supported values.
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
/// ```ignore
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
/// Tuple of (digit, is_negative) extracted from the overpunch byte
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
/// ```ignore
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
/// # Policy
/// Defaults to *preferred zero sign* (`ZeroSignPolicy::Preferred`) for EBCDIC zeros unless
/// `preserve_zoned_encoding` captured an explicit format at decode.
///
/// # Errors
/// Returns an error if zone nibbles or the last-byte overpunch are invalid.
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
/// Tuple of (accumulated_value, digit_count)
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
/// Creates the final normalized SmallDecimal value.
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
/// - If digit_count < digits: `[digit][sign]`
/// - If digit_count == digits: `[unused][sign]` (high nibble ignored)
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
/// 4. Construct normalized SmallDecimal
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

/// Fast binary integer decoder with optimized paths for common widths
///
/// # Errors
/// Returns an error when the provided data exceeds the supported range for the requested width.
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

/// Encode a zoned decimal using the configured code page and a caller-resolved policy while
/// reusing scratch buffers.
///
/// # Policy
/// Callers typically resolve policy using `zoned_encoding_override` → preserved metadata →
/// `preferred_zoned_encoding`, matching the documented library behavior for zoned decimals.
///
/// # Errors
/// Returns an error when the decimal value cannot be represented with the requested digits or encoding.
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

    // Use the standard encode function but with optimized digit processing
    // This is a placeholder for now - the actual optimization would involve
    // rewriting the encode logic to use the scratch buffers
    encode_zoned_decimal(
        &scratch.string_buffer,
        digits,
        decimal.scale,
        signed,
        codepage,
    )
}

/// Optimized packed decimal encoder using scratch buffers
/// Minimizes allocations by reusing digit buffer
///
/// # Errors
/// Returns an error when the decimal value cannot be encoded into the requested packed representation.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_packed_decimal_with_scratch(
    decimal: &SmallDecimal,
    digits: u16,
    signed: bool,
    scratch: &mut ScratchBuffers,
) -> Result<Vec<u8>> {
    // Clear and prepare buffers
    scratch.digit_buffer.clear();
    scratch.byte_buffer.clear();
    let expected_bytes = usize::from((digits + 1).div_ceil(2));
    scratch.byte_buffer.reserve(expected_bytes);

    // Convert decimal to string using scratch buffer
    scratch.string_buffer.clear();
    scratch.string_buffer.push_str(&decimal.to_string());

    // Use the standard encode function but with optimized nibble processing
    // This is a placeholder for now - the actual optimization would involve
    // rewriting the encode logic to use the scratch buffers
    encode_packed_decimal(&scratch.string_buffer, digits, decimal.scale, signed)
}

/// Optimized packed decimal decoder that formats to string using scratch buffer
/// CRITICAL PERFORMANCE OPTIMIZATION for COMP-3 JSON conversion
///
/// This avoids the `SmallDecimal` -> String allocation overhead that was causing
/// the 94-96% performance regression in COMP-3 processing.
///
/// # Errors
/// Returns an error when the packed decimal bytes are malformed for the requested digits or scale.
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
        return Ok("0".to_string());
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
/// ```ignore
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
/// ```ignore
/// let mut buffer = String::new();
/// format_integer_with_leading_zeros_to_buffer(45, 4, &mut buffer);
/// assert_eq!(buffer, "0045");
/// ```
#[inline]
fn format_integer_with_leading_zeros_to_buffer(value: i64, width: u32, buffer: &mut String) {
    SmallDecimal::format_integer_with_leading_zeros(value, width, buffer);
}

/// Optimized zoned decimal decoder that formats to string using scratch buffer
/// CRITICAL PERFORMANCE OPTIMIZATION for zoned decimal JSON conversion
///
/// # Policy
/// Mirrors [`decode_zoned_decimal_with_scratch`], inheriting its default preferred-zero handling for EBCDIC data.
///
/// # Errors
/// Returns an error when the zoned decimal bytes are invalid for the requested digits or scale.
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
