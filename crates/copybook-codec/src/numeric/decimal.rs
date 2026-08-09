// SPDX-License-Identifier: AGPL-3.0-or-later
//! Decimal value and zoned-encoding metadata helpers for numeric codecs.

use super::zoned::ZonedEncodingFormat;
use copybook_core::{Error, ErrorCode, Result};
use std::fmt::Write;

pub(super) fn create_normalized_decimal(value: i64, scale: i16, is_negative: bool) -> SmallDecimal {
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
    /// - If invalid zones exist without both known formats: Auto format without a mixed flag
    /// - If both ASCII and EBCDIC zones exist: Auto format with mixed encoding flag
    /// - If only ASCII zones: ASCII format, no mixing
    /// - If only EBCDIC zones: EBCDIC format, no mixing
    /// - If no valid zones: Auto format, no mixing (empty or all invalid)
    fn determine_overall_format(&self) -> (ZonedEncodingFormat, bool) {
        if self.ascii_count > 0 && self.ebcdic_count > 0 {
            // Mixed ASCII and EBCDIC zones within the same field
            (ZonedEncodingFormat::Auto, true)
        } else if self.invalid_count > 0 {
            // No unambiguous format can be determined from the invalid zones
            (ZonedEncodingFormat::Auto, false)
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
        // Writing to String should never fail, but handle gracefully for panic elimination
        if write!(result, "{scaled_value}").is_err() {
            // Fallback: append a placeholder if formatting somehow fails
            result.push_str("ERR");
        }
    }

    /// Append decimal format with exactly `scale` digits after decimal point
    fn append_decimal_format(&self, result: &mut String) {
        let divisor = 10_i64.pow(scale_abs_to_u32(self.scale));
        let integer_part = self.value / divisor;
        let fractional_part = self.value % divisor;

        // Writing to String should never fail, but handle gracefully for panic elimination
        let width = usize::try_from(self.scale).unwrap_or_else(|_| {
            debug_assert!(false, "scale should be positive when formatting decimal");
            0
        });

        if write!(result, "{integer_part}.{fractional_part:0width$}").is_err() {
            // Fallback: append a placeholder if formatting somehow fails
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

    /// Format as string with the given fixed scale.
    ///
    /// Always renders with exactly `scale` digits after the decimal point,
    /// independent of the `SmallDecimal`'s own scale. Used for lossless JSON output.
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

    /// Format the decimal into a caller-owned string buffer to avoid allocation.
    ///
    /// This is the hot-path formatter used in COMP-3 JSON conversion. The buffer
    /// is cleared before writing.
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
    pub(super) fn format_integer_manual(mut value: i64, buffer: &mut String) {
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
    pub(super) fn format_integer_with_leading_zeros(
        mut value: i64,
        width: u32,
        buffer: &mut String,
    ) {
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
pub(super) fn digit_from_value(value: i64) -> u8 {
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
pub(super) fn scale_abs_to_u32(scale: i16) -> u32 {
    u32::from(scale.unsigned_abs())
}
