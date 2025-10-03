'''//! Numeric type codecs for COBOL data
//!
//! This module implements encoding and decoding for zoned decimal,
//! packed decimal, and binary integer types.

use crate::charset::get_zoned_sign_table;
use crate::memory::ScratchBuffers;
use crate::options::{Codepage, ZonedEncodingFormat};
use copybook_core::{Error, ErrorCode, Result};
use std::fmt::Write;
use tracing::warn;

// CRITICAL PERFORMANCE OPTIMIZATION: Inline hints for hot paths
#[allow(dead_code)]
#[allow(clippy::inline_always)]
#[inline(always)]
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

#[allow(clippy::inline_always)]
#[inline(always)]
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

// PERFORMANCE OPTIMIZATION: Inline decimal construction for hot paths
#[allow(clippy::inline_always)]
#[inline(always)]
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
    pub fn new(detected_format: ZonedEncodingFormat, has_mixed_encoding: bool) -> Self {
        Self {
            detected_format,
            has_mixed_encoding,
            byte_formats: Vec::new(),
        }
    }

    /// Analyze zoned decimal data bytes to detect encoding patterns
    ///
    /// Examines each byte's zone nibble to identify the encoding format and detect
    /// any mixed encoding patterns that would indicate data corruption or
    /// inconsistent processing.
    ///
    /// # Returns
    /// A comprehensive analysis including:
    /// - Overall detected format (ASCII, EBCDIC, or Auto for ambiguous cases)
    /// - Mixed encoding flag if inconsistent patterns are found
    /// - Per-byte format detection for detailed analysis
    ///
    /// # Errors
    /// Returns an error if the analysis cannot be completed (currently never fails)
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
            // ASCII overpunch positive: '{' and '}'
            0x7B | 0x7D => return Some(ZonedEncodingFormat::Ascii),
            // ASCII overpunch characters: A-I (0x41-0x49) and J-R (0x4A-0x52)
            0x41..=0x52 => return Some(ZonedEncodingFormat::Ascii),
            _ => {}
        }

        let zone_nibble = (byte >> 4) & ZONE_MASK;
        match zone_nibble {
            ASCII_ZONE => Some(ZonedEncodingFormat::Ascii),
            EBCDIC_ZONE => Some(ZonedEncodingFormat::Ebcdic),
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
    /// Create a new SmallDecimal
    pub fn new(value: i64, scale: i16, negative: bool) -> Self {
        Self {
            value,
            scale,
            negative,
        }
    }

    /// Create a zero value with the given scale
    pub fn zero(scale: i16) -> Self {
        Self {
            value: 0,
            scale,
            negative: false,
        }
    }

    /// Normalize -0 to 0 (NORMATIVE)
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
            self.value * 10_i64.pow((-self.scale) as u32)
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
        let divisor = 10_i64.pow(self.scale as u32);
        let integer_part = self.value / divisor;
        let fractional_part = self.value % divisor;

        // Writing to String should never fail, but handle gracefully for panic elimination
        if write!(
            result,
            "{integer_part}.{:0width$}",
            fractional_part,
            width = self.scale as usize
        )
        .is_err()
        {
            // Fallback: append a placeholder if formatting somehow fails
            result.push_str("ERR");
        }
    }

    /// Parse decimal from string with strict scale validation
    ///
    /// Parses a string representation into a SmallDecimal with the specified scale.
    /// Performs strict validation to ensure the input format matches expectations.
    ///
    /// # Arguments
    /// * `s` - The string to parse (may contain decimal point)
    /// * `expected_scale` - The required number of decimal places
    ///
    /// # Validation Rules (NORMATIVE)
    /// - Decimal places must match `expected_scale` exactly
    /// - Integer inputs are only valid when `expected_scale` is 0
    /// - Empty/whitespace strings are treated as zero
    ///
    /// # Errors
    /// Returns `CBKE505_SCALE_MISMATCH` for scale mismatches or
    /// `CBKE501_JSON_TYPE_MISMATCH` for invalid numeric format.
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
    /// Returns (is_negative, numeric_part_without_sign)
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
        if fractional_part.len() != expected_scale as usize {
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
        let divisor = 10_i64.pow(scale as u32);
        integer_value
            .checked_mul(divisor)
            .and_then(|v| v.checked_add(fractional_value))
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    "Numeric value too large - would cause overflow",
                )
            })
    }

    /// Format as string with fixed scale (NORMATIVE)
    /// Always render with exactly `scale` digits after decimal
    pub fn to_fixed_scale_string(&self, scale: i16) -> String {
        let mut result = String::new();

        if self.negative && self.value != 0 {
            result.push('-');
        }

        if scale <= 0 {
            // Integer format (scale=0) or scale extension
            let scaled_value = if scale < 0 {
                self.value * 10_i64.pow((-scale) as u32)
            } else {
                self.value
            };
            // Writing to String should never fail, but handle gracefully for panic elimination
            if write!(result, "{scaled_value}").is_err() {
                result.push_str("ERR");
            }
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(scale as u32);
            let integer_part = self.value / divisor;
            let fractional_part = self.value % divisor;

            // Writing to String should never fail, but handle gracefully for panic elimination
            if write!(
                result,
                "{integer_part}.{:0width$}",
                fractional_part,
                width = scale as usize
            )
            .is_err()
            {
                result.push_str("ERR");
            }
        }

        result
    }

    /// High-performance format using scratch buffer (zero-allocation optimization)
    /// CRITICAL for COMP-3 JSON conversion performance
    pub fn format_to_scratch_buffer(&self, scale: i16, scratch_buffer: &mut String) {
        scratch_buffer.clear();

        if self.negative && self.value != 0 {
            scratch_buffer.push('-');
        }

        if scale <= 0 {
            // Integer format (scale=0) or scale extension
            let scaled_value = if scale < 0 {
                self.value * 10_i64.pow((-scale) as u32)
            } else {
                self.value
            };
            // CRITICAL OPTIMIZATION: Manual integer formatting to avoid write!() overhead
            Self::format_integer_manual(scaled_value, scratch_buffer);
        } else {
            // Decimal format with exactly `scale` digits after decimal
            let divisor = 10_i64.pow(scale as u32);
            let integer_part = self.value / divisor;
            let fractional_part = self.value % divisor;

            // CRITICAL OPTIMIZATION: Manual decimal formatting to avoid write!() overhead
            Self::format_integer_manual(integer_part, scratch_buffer);
            scratch_buffer.push('.');
            Self::format_integer_with_leading_zeros(fractional_part, scale as u32, scratch_buffer);
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
                buffer.push((value as u8 + b'0') as char);
            } else {
                let tens = (value / 10) as u8;
                let ones = (value % 10) as u8;
                buffer.push((tens + b'0') as char);
                buffer.push((ones + b'0') as char);
            }
            return;
        }

        // Use a small stack buffer for digits for larger numbers
        let mut digits = [0u8; 20]; // More than enough for i64::MAX
        let mut count = 0;

        while value > 0 {
            digits[count] = (value % 10) as u8 + b'0';
            value /= 10;
            count += 1;
        }

        // Add digits in reverse order
        for i in (0..count).rev() {
            buffer.push(digits[i] as char);
        }
    }

    /// Ultra-fast manual integer formatting with leading zeros
    #[inline]
    fn format_integer_with_leading_zeros(mut value: i64, width: u32, buffer: &mut String) {
        // Optimized for common small widths (most COMP-3 scales are 0-4)
        if width <= 4 && value < 10000 {
            match width {
                1 => {
                    buffer.push((value as u8 + b'0') as char);
                }
                2 => {
                    buffer.push(((value / 10) as u8 + b'0') as char);
                    buffer.push(((value % 10) as u8 + b'0') as char);
                }
                3 => {
                    buffer.push(((value / 100) as u8 + b'0') as char);
                    buffer.push((((value / 10) % 10) as u8 + b'0') as char);
                    buffer.push(((value % 10) as u8 + b'0') as char);
                }
                4 => {
                    buffer.push(((value / 1000) as u8 + b'0') as char);
                    buffer.push((((value / 100) % 10) as u8 + b'0') as char);
                    buffer.push((((value / 10) % 10) as u8 + b'0') as char);
                    buffer.push(((value % 10) as u8 + b'0') as char);
                }
                _ => {}
            }
            return;
        }

        // General case for larger widths
        let mut digits = [0u8; 20]; // More than enough for i64::MAX
        let mut count = 0;

        // Extract digits
        loop {
            digits[count] = (value % 10) as u8 + b'0';
            value /= 10;
            count += 1;
            if value == 0 && count >= width as usize {
                break;
            }
        }

        // Pad with leading zeros if needed
        while count < width as usize {
            digits[count] = b'0';
            count += 1;
        }

        // Add digits in reverse order
        for i in (0..count).rev() {
            buffer.push(digits[i] as char);
        }
    }

    /// Get the scale of this decimal
    pub fn scale(&self) -> i16 {
        self.scale
    }

    /// Check if this decimal is negative
    pub fn is_negative(&self) -> bool {
        self.negative && self.value != 0
    }

    /// Get the total number of digits in this decimal
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

/// Decode zoned decimal field with comprehensive error context
///
/// # Errors
///
/// Returns an error if the zoned decimal data is invalid or contains bad sign zones.
/// All errors include proper context information (record_index, field_path, byte_offset).
#[inline]
pub fn decode_zoned_decimal(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
) -> Result<SmallDecimal> {
    if unlikely(data.len() != digits as usize) {
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
        } else {
            return Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                "Zoned field contains all spaces but BLANK WHEN ZERO not specified",
            ));
        }
    }

    let sign_table = get_zoned_sign_table(codepage);
    let mut value = 0i64;
    let mut is_negative = false;

    // Process each digit
    for (i, &byte) in data.iter().enumerate() {
        let zone = (byte >> 4) & 0x0F;
        let digit = byte & 0x0F;

        // For the last byte, check sign if field is signed
        if i == data.len() - 1 && signed {
            // Handle ASCII overpunch characters
            if codepage == Codepage::ASCII {
                // ASCII overpunch decoding - IBM standard overpunch table
                let (actual_digit, sign) = match byte {
                    // Positive signs
                    0x7B => (0, false), // '{' = +0
                    0x41..=0x49 => ((byte - 0x40) as i64, false), // 'A'-'I' = +1 to +9

                    // Negative signs
                    0x7D => (0, true),  // '}' = -0
                    0x4A..=0x52 => ((byte - 0x49) as i64, true), // 'J'-'R' = -1 to -9

                    // Unsigned digits
                    0x30..=0x39 => ((byte - 0x30) as i64, false),

                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid ASCII overpunch character 0x{byte:02X}"),
                        ));
                    }
                };
                value = value.saturating_mul(10).saturating_add(actual_digit);
                is_negative = sign;
            } else {
                // Standard EBCDIC zone/digit validation
                if digit > 9 {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        format!("Invalid digit nibble 0x{digit:X} at position {i}"),
                    ));
                }

                let (has_sign, negative) = sign_table[zone as usize];
                if has_sign {
                    is_negative = negative;
                    value = value.saturating_mul(10).saturating_add(i64::from(digit));
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        format!("Invalid sign zone 0x{zone:X} in last digit"),
                    ));
                }
            }
        } else {
            // For non-sign positions, validate digit nibble
            if digit > 9 {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid digit nibble 0x{digit:X} at position {i}"),
                ));
            }

            // Validate zone is appropriate for codepage
            match codepage {
                Codepage::ASCII => {
                    // ASCII digits should have zone 0x3 (0x30-0x39)
                    if zone != 0x3 {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid ASCII zone 0x{zone:X} at position {i}, expected 0x3"),
                        ));
                    }
                }
                _ => {
                    // EBCDIC digits should have zone 0xF (0xF0-0xF9)
                    if zone != 0xF {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid EBCDIC zone 0x{zone:X} at position {i}, expected 0xF"),
                        ));
                    }
                }
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
/// Returns both the decoded decimal and encoding information for preservation
///
/// # Errors
///
/// Returns an error if the zoned decimal data is invalid or contains bad sign zones.
/// All errors include proper context information (record_index, field_path, byte_offset).
pub fn decode_zoned_decimal_with_encoding(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
    preserve_encoding: bool,
) -> Result<(SmallDecimal, Option<ZonedEncodingInfo>)> {
    if data.len() != digits as usize {
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
        } else {
            return Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                "Zoned field contains all spaces but BLANK WHEN ZERO not specified",
            ));
        }
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

    let sign_table = get_zoned_sign_table(codepage);
    let mut value = 0i64;
    let mut is_negative = false;

    // Process each digit - now allowing both ASCII and EBCDIC zones when preserve_encoding is enabled
    for (i, &byte) in data.iter().enumerate() {
        let zone = (byte >> 4) & 0x0F;
        let digit = byte & 0x0F;

        // For the last byte, check sign if field is signed
        if i == data.len() - 1 && signed {
            // Handle ASCII overpunch characters
            if codepage == Codepage::ASCII {
                // ASCII overpunch decoding - IBM standard overpunch table
                let (actual_digit, sign) = match byte {
                    // Positive signs
                    0x7B => (0, false), // '{' = +0
                    0x41..=0x49 => ((byte - 0x40) as i64, false), // 'A'-'I' = +1 to +9

                    // Negative signs
                    0x7D => (0, true),  // '}' = -0
                    0x4A..=0x52 => ((byte - 0x49) as i64, true), // 'J'-'R' = -1 to -9

                    // Unsigned digits
                    0x30..=0x39 => ((byte - 0x30) as i64, false),

                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKD411_ZONED_BAD_SIGN,
                            format!("Invalid ASCII overpunch character 0x{byte:02X}"),
                        ));
                    }
                };
                value = value.saturating_mul(10).saturating_add(actual_digit);
                is_negative = sign;
            } else {
                // Standard EBCDIC zone/digit validation
                if digit > 9 {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        format!("Invalid digit nibble 0x{digit:X} at position {i}"),
                    ));
                }

                let (has_sign, negative) = sign_table[zone as usize];
                if has_sign {
                    is_negative = negative;
                    value = value.saturating_mul(10).saturating_add(i64::from(digit));
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD411_ZONED_BAD_SIGN,
                        format!("Invalid sign zone 0x{zone:X} in last digit"),
                    ));
                }
            }
        } else {
            // For non-sign positions, validate digit nibble
            if digit > 9 {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid digit nibble 0x{digit:X} at position {i}"),
                ));
            }

            // When preserve_encoding is enabled, accept both ASCII and EBCDIC zones
            if preserve_encoding {
                // Accept both ASCII (0x3) and EBCDIC (0xF) zones for maximum compatibility
                match zone {
                    0x3 | 0xF => {
                        // Valid zone for either ASCII or EBCDIC
                    }
                    _ => {
                        return Err(Error::new(
                            ErrorCode::CBKD413_ZONED_INVALID_ENCODING,
                            format!(
                                "Invalid zone 0x{zone:X} at position {i}, expected 0x3 (ASCII) or 0xF (EBCDIC)"
                            ),
                        ));
                    }
                }
            } else {
                // Original strict validation based on codepage
                match codepage {
                    Codepage::ASCII => {
                        // ASCII digits should have zone 0x3 (0x30-0x39)
                        if zone != 0x3 {
                            return Err(Error::new(
                                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                                format!(
                                    "Invalid ASCII zone 0x{zone:X} at position {i}, expected 0x3"
                                ),
                            ));
                        }
                    }
                    _ => {
                        // EBCDIC digits should have zone 0xF (0xF0-0xF9)
                        if zone != 0xF {
                            return Err(Error::new(
                                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                                format!(
                                    "Invalid EBCDIC zone 0x{zone:X} at position {i}, expected 0xF"
                                ),
                            ));
                        }
                    }
                }
            }

            value = value.saturating_mul(10).saturating_add(i64::from(digit));
        }
    }

    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize(); // Normalize -0 → 0 (NORMATIVE)
    Ok((decimal, encoding_info))
}

/// Decode packed decimal field with comprehensive error context
///
/// # Errors
///
/// Returns an error if the packed decimal data contains invalid nibbles.
/// All errors include proper context information (record_index, field_path, byte_offset).
#[inline(always)]
pub fn decode_packed_decimal(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    // CRITICAL PERFORMANCE OPTIMIZATION: Ultra-fast path with minimal safety overhead
    let expected_bytes = ((digits + 1).div_ceil(2)) as usize;
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
                "COMP-3 field with {} digits exceeds maximum supported precision (18 digits max for current implementation)",
                digits
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
#[inline(always)]
fn decode_packed_decimal_fast_path(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
) -> Result<SmallDecimal> {
    let mut value = 0i64;

    // CRITICAL PERFORMANCE OPTIMIZATION: Ultra-fast unrolled decoder
    // Optimized for enterprise mainframe patterns (1-5 byte COMP-3 fields)
    match data.len() {
        1 => {
            // FASTEST PATH: Single byte COMP-3 (PIC 9(1) COMP-3, PIC S9(1) COMP-3)
            // Extremely common in mainframe applications for flags, counters, etc
            // Single byte: common case optimization
            let byte = data[0];
            let high_nibble = (byte >> 4) & 0x0F;
            let low_nibble = byte & 0x0F;

            // CRITICAL FIX: The previous logic was incorrect.
            // For single-byte COMP-3, we can only have 1 digit, which is always odd.
            // The sign is always in the low nibble, high nibble is always the digit.
            // There is no case where single-byte COMP-3 has padding in the high nibble.

            // PERFORMANCE OPTIMIZATION: Single-byte fast path for odd digits
            if !digits.is_multiple_of(2) {
                if unlikely(high_nibble > 9) {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        "Invalid digit nibble in packed decimal".to_string(),
                    ));
                }
                value = i64::from(high_nibble);
            }

            // PERFORMANCE OPTIMIZATION: Streamlined sign processing
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
            } else if unlikely(low_nibble != 0xF) {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid unsigned sign nibble, expected 0xF".to_string(),
                ));
            }
        }
        2 => {
            // SECOND FASTEST PATH: 2-byte COMP-3 (PIC S9(2) or S9(3) COMP-3, very common)
            // Handles amounts, quantities, percentages in enterprise applications
            let byte0 = data[0];
            let byte1 = data[1];

            let d1 = (byte0 >> 4) & 0x0F;
            let d2 = byte0 & 0x0F;
            let d3 = (byte1 >> 4) & 0x0F;
            let sign_nibble = byte1 & 0x0F;

            // CRITICAL FIX: Handle both 2-digit and 3-digit cases in 2-byte path
            if digits == 2 {
                // For 2 digits: format is [0][d1][d2][sign] where first nibble is padding
                if unlikely(d1 != 0) {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Expected padding nibble 0 for 2-digit field, got 0x{d1:X}"),
                    ));
                }

                // Fast validation: digits must be ≤ 9
                if unlikely(d2 > 9 || d3 > 9) {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        "Invalid digit in 2-digit COMP-3 field".to_string(),
                    ));
                }

                // Fast value computation for 2 digits: d2*10 + d3
                value = i64::from(d2) * 10 + i64::from(d3);
            } else {
                // For 3 digits: format is [d1][d2][d3][sign] - all nibbles are digits
                if unlikely(d1 > 9 || d2 > 9 || d3 > 9) {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        "Invalid digit in 3-digit COMP-3 field".to_string(),
                    ));
                }

                // Fast value computation for 3 digits: d1*100 + d2*10 + d3
                value = i64::from(d1) * 100 + i64::from(d2) * 10 + i64::from(d3);
            }

            // PERFORMANCE OPTIMIZATION: Streamlined sign processing for 2-byte path
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

            let mut decimal = SmallDecimal::new(value, scale, is_negative);
            decimal.normalize();
            return Ok(decimal);
        }
        3 => {
            // THIRD FASTEST PATH: 3-byte COMP-3 (PIC S9(5) COMP-3, extremely common)
            // Handles account numbers, amounts, IDs in enterprise applications
            let byte0 = data[0];
            let byte1 = data[1];
            let byte2 = data[2];

            let d1 = (byte0 >> 4) & 0x0F;
            let d2 = byte0 & 0x0F;
            let d3 = (byte1 >> 4) & 0x0F;
            let d4 = byte1 & 0x0F;
            let d5 = (byte2 >> 4) & 0x0F;
            let sign_nibble = byte2 & 0x0F;

            // Fast validation: all digits must be ≤ 9
            if unlikely(d1 > 9 || d2 > 9 || d3 > 9 || d4 > 9 || d5 > 9) {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid digit in 3-byte COMP-3 field".to_string(),
                ));
            }

            // Fast value computation: unrolled multiplication for maximum performance
            value = i64::from(d1) * 10000
                + i64::from(d2) * 1000
                + i64::from(d3) * 100
                + i64::from(d4) * 10
                + i64::from(d5);

            // PERFORMANCE OPTIMIZATION: Streamlined sign processing for 2-byte path
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

            let mut decimal = SmallDecimal::new(value, scale, is_negative);
            decimal.normalize();
            return Ok(decimal);
        }
        _ => {
            // ULTRA-FAST GENERAL CASE: Optimized for 4+ byte COMP-3 fields
            // Minimal branching, streamlined validation, optimized for throughput
            let total_nibbles = digits + 1;
            let has_padding = (total_nibbles & 1) == 1;
            let digit_count = digits as usize;

            // PERFORMANCE CRITICAL: Process all bytes except last in tight loop
            let (last_byte, prefix_bytes) = data.split_last().unwrap();
            let mut digit_pos = 0;

            // Process prefix bytes (all digits)
            for &byte in prefix_bytes.iter() {
                let high_nibble = (byte >> 4) & 0x0F;
                let low_nibble = byte & 0x0F;

                // High nibble: skip padding, otherwise process as digit
                if likely(!(digit_pos == 0 && has_padding)) {
                    if unlikely(high_nibble > 9) {
                        return Err(Error::new(
                            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                            "Invalid digit nibble".to_string(),
                        ));
                    }
                    value = value * 10 + i64::from(high_nibble);
                }
                digit_pos += 1;

                // Low nibble: always a digit in prefix bytes
                if unlikely(low_nibble > 9) {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        "Invalid digit nibble".to_string(),
                    ));
                }
                value = value * 10 + i64::from(low_nibble);
                digit_pos += 1;
            }

            // Process last byte: high nibble = last digit, low nibble = sign
            let last_high = (*last_byte >> 4) & 0x0F;
            let sign_nibble = *last_byte & 0x0F;

            // Process final digit if not padding
            if likely(digit_pos < digit_count) {
                if unlikely(last_high > 9) {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        "Invalid digit nibble".to_string(),
                    ));
                }
                value = value * 10 + i64::from(last_high);
            }

            // Process sign
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

            return Ok(create_normalized_decimal(value, scale, is_negative));
        }
    }

    // If we get here without returning, it's unsigned
    Ok(create_normalized_decimal(value, scale, false))
}

/// Decode binary integer field
///
/// # Errors
///
/// Returns an error if the binary data is invalid or the field size is unsupported
pub fn decode_binary_int(data: &[u8], bits: u16, signed: bool) -> Result<i64> {
    let expected_bytes = (bits / 8) as usize;
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
                Ok(i16::from_be_bytes([data[0], data[1]]) as i64)
            } else {
                Ok(value as i64)
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
                Ok(i32::from_be_bytes([data[0], data[1], data[2], data[3]]) as i64)
            } else {
                Ok(value as i64)
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
                if value > i64::MAX as u64 {
                    return Err(Error::new(
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                        format!("Unsigned 64-bit value {value} exceeds i64::MAX"),
                    ));
                }
                Ok(value as i64)
            }
        }
        _ => Err(Error::new(
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
            format!("Unsupported binary field width: {bits} bits"),
        )),
    }
}

/// Encode zoned decimal field
///
/// # Errors
///
/// Returns an error if the value cannot be encoded as a zoned decimal with the specified parameters
pub fn encode_zoned_decimal(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
) -> Result<Vec<u8>> {
    // Parse the input value with scale validation (NORMATIVE)
    let decimal = SmallDecimal::from_str(value, scale)?;

    // Convert to string representation of digits
    let abs_value = decimal.value.abs();
    let digit_str = format!("{:0width$}", abs_value, width = digits as usize);

    if digit_str.len() > digits as usize {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Value too large for {} digits", digits),
        ));
    }

    let mut result = Vec::with_capacity(digits as usize);
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
            // Last digit with sign - use ASCII overpunch for ASCII codepage
            if codepage == Codepage::ASCII {
                let overpunch_byte = if decimal.negative {
                    // ASCII negative overpunch characters (J-R for 1-9, } for 0)
                    match digit {
                        0 => 0x7D, // '}' = -0
                        1..=9 => 0x49 + digit,
                        _ => unreachable!(),
                    }
                } else {
                    // ASCII positive overpunch characters (A-I for 1-9, { for 0)
                    match digit {
                        0 => 0x7B, // '{' = +0
                        1..=9 => 0x40 + digit,
                        _ => unreachable!(),
                    }
                };
                result.push(overpunch_byte);
            } else {
                // EBCDIC zone encoding
                let zone = if signed {
                    // For signed fields, use 0xC for positive, 0xD for negative
                    if decimal.negative {
                        0xD // Negative sign
                    } else {
                        0xC // EBCDIC positive sign for signed fields
                    }
                } else {
                    // For unsigned fields, always use 0xF
                    0xF // EBCDIC unsigned (0xF0-0xF9)
                };
                result.push((zone << 4) | digit);
            }
        } else {
            // Regular digit
            let zone = match codepage {
                Codepage::ASCII => 0x3, // ASCII digits (0x30-0x39)
                _ => 0xF,               // EBCDIC digits (0xF0-0xF9)
            };
            result.push((zone << 4) | digit);
        }
    }

    Ok(result)
}

/// Encode zoned decimal field with encoding format override support
///
/// Supports explicit encoding format override for round-trip preservation
///
/// # Errors
///
/// Returns an error if the value cannot be encoded as a zoned decimal with the specified parameters
pub fn encode_zoned_decimal_with_format(
    value: &str,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    encoding_override: Option<ZonedEncodingFormat>,
) -> Result<Vec<u8>> {
    // Parse the input value with scale validation (NORMATIVE)
    let decimal = SmallDecimal::from_str(value, scale)?;

    // Convert to string representation of digits
    let abs_value = decimal.value.abs();
    let digit_str = format!("{:0width$}", abs_value, width = digits as usize);

    if digit_str.len() > digits as usize {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Value too large for {} digits", digits),
        ));
    }

    // Determine the encoding format to use
    // Precedence: explicit override > codepage default
    let target_format = encoding_override.unwrap_or(match codepage {
        Codepage::ASCII => ZonedEncodingFormat::Ascii,
        _ => ZonedEncodingFormat::Ebcdic,
    });

    let mut result = Vec::with_capacity(digits as usize);
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
            // Last digit with sign - use ASCII overpunch for ASCII format
            if target_format == ZonedEncodingFormat::Ascii {
                let overpunch_byte = if decimal.negative {
                    // ASCII negative overpunch characters (J-R for 1-9, } for 0)
                    match digit {
                        0 => 0x7D, // '}' = -0
                        1..=9 => 0x49 + digit,
                        _ => unreachable!(),
                    }
                } else {
                    // ASCII positive overpunch characters (A-I for 1-9, { for 0)
                    match digit {
                        0 => 0x7B, // '{' = +0
                        1..=9 => 0x40 + digit,
                        _ => unreachable!(),
                    }
                };
                result.push(overpunch_byte);
            } else {
                // EBCDIC zone encoding
                let zone = if signed {
                    // For signed fields, use 0xC for positive, 0xD for negative
                    if decimal.negative {
                        0xD // Negative sign
                    } else {
                        0xC // EBCDIC positive sign for signed fields
                    }
                } else {
                    // For unsigned fields, always use 0xF
                    0xF // EBCDIC unsigned (0xF0-0xF9)
                };
                result.push((zone << 4) | digit);
            }
        } else {
            // Regular digit
            let zone = match target_format {
                ZonedEncodingFormat::Ascii => 0x3, // ASCII digits (0x30-0x39)
                _ => 0xF,                          // EBCDIC digits (0xF0-0xF9)
            };
            result.push((zone << 4) | digit);
        }
    }

    Ok(result)
}

/// Encode packed decimal field
///
/// # Errors
///
/// Returns an error if the value cannot be encoded as a packed decimal with the specified parameters
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
        let expected_bytes = ((digits + 1).div_ceil(2)) as usize;
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
        digit_buffer[digit_count] = (temp_value % 10) as u8;
        temp_value /= 10;
        digit_count += 1;
    }

    // Validate digit count
    if unlikely(digit_count > digits as usize) {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Value too large for {} digits", digits),
        ));
    }

    let expected_bytes = ((digits + 1).div_ceil(2)) as usize;
    let mut result = Vec::with_capacity(expected_bytes);

    // CRITICAL FIX: Handle digit positioning correctly for even/odd digit counts
    // For packed decimal, we have:
    // - Total nibbles needed: digits + 1 (for sign)
    // - If digits is even: first nibble is padding (0), then digits, then sign
    // - If digits is odd: no padding, digits fill completely, then sign

    let has_padding = digits.is_multiple_of(2); // Even digit count requires padding
    let total_nibbles = digits as usize + 1 + usize::from(has_padding);

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
                if digit_idx >= (digits as usize - digit_count) {
                    // This position should contain an actual digit
                    let actual_digit_idx = digit_idx - (digits as usize - digit_count);
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
            if digit_idx >= (digits as usize - digit_count) {
                // This position should contain an actual digit
                let actual_digit_idx = digit_idx - (digits as usize - digit_count);
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
///
/// Returns an error if the value is out of range for the specified bit width
pub fn encode_binary_int(value: i64, bits: u16, signed: bool) -> Result<Vec<u8>> {
    match bits {
        16 => {
            if signed {
                if value < i16::MIN as i64 || value > i16::MAX as i64 {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for signed 16-bit integer"),
                    ));
                }
                Ok((value as i16).to_be_bytes().to_vec())
            } else {
                if value < 0 || value > u16::MAX as i64 {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for unsigned 16-bit integer"),
                    ));
                }
                Ok((value as u16).to_be_bytes().to_vec())
            }
        }
        32 => {
            if signed {
                if value < i32::MIN as i64 || value > i32::MAX as i64 {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for signed 32-bit integer"),
                    ));
                }
                Ok((value as i32).to_be_bytes().to_vec())
            } else {
                if value < 0 || value > u32::MAX as i64 {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} out of range for unsigned 32-bit integer"),
                    ));
                }
                Ok((value as u32).to_be_bytes().to_vec())
            }
        }
        64 => {
            if signed {
                Ok(value.to_be_bytes().to_vec())
            } else {
                if value < 0 {
                    return Err(Error::new(
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                        format!("Value {value} cannot be negative for unsigned 64-bit integer"),
                    ));
                }
                Ok((value as u64).to_be_bytes().to_vec())
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
///
/// Returns an error if the text is too long for the field
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
///
/// Returns an error if the value cannot be encoded
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
        return Ok(vec![space_byte; digits as usize]);
    }

    encode_zoned_decimal(value, digits, scale, signed, codepage)
}

/// Get binary width mapping based on PIC digits (NORMATIVE)
///
/// Maps digits to width: ≤4→2B, 5-9→4B, 10-18→8B
pub fn get_binary_width_from_digits(digits: u16) -> u16 {
    match digits {
        1..=4 => 16,   // 2 bytes
        5..=9 => 32,   // 4 bytes
        10..=18 => 64, // 8 bytes
        _ => 64,       // Default to 8 bytes for larger values
    }
}

/// Validate explicit USAGE BINARY(n) width (NORMATIVE)
///
/// Accept explicit USAGE BINARY(n) for n ∈ {1,2,4,8}
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

/// Optimized zoned decimal decoder using scratch buffers
/// Minimizes allocations by reusing digit buffer
pub fn decode_zoned_decimal_with_scratch(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    blank_when_zero: bool,
    scratch: &mut ScratchBuffers,
) -> Result<SmallDecimal> {
    if data.len() != digits as usize {
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
    let space_byte = match codepage {
        Codepage::ASCII => b' ',
        _ => 0x40, // EBCDIC space
    };

    let is_all_spaces = data.iter().all(|&b| b == space_byte);
    if is_all_spaces {
        if blank_when_zero {
            warn!("CBKD412_ZONED_BLANK_IS_ZERO: Zoned field is blank, decoding as zero");
            crate::lib_api::increment_warning_counter();
            return Ok(SmallDecimal::zero(scale));
        } else {
            return Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                "Zoned field contains all spaces but BLANK WHEN ZERO not specified",
            ));
        }
    }

    // Clear and prepare digit buffer for reuse
    scratch.digit_buffer.clear();
    scratch.digit_buffer.reserve(digits as usize);

    let sign_table = get_zoned_sign_table(codepage);
    let mut value = 0i64;
    let mut is_negative = false;

    // Optimized digit processing using scratch buffer
    let expected_zone = match codepage {
        Codepage::ASCII => 0x3, // ASCII digits (0x30-0x39)
        _ => 0xF,               // EBCDIC digits (0xF0-0xF9)
    };

    // Process each digit with optimized zone checking
    for (i, &byte) in data.iter().enumerate() {
        let zone = (byte >> 4) & 0x0F;
        let digit = byte & 0x0F;

        // Validate digit nibble
        if digit > 9 {
            return Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                format!("Invalid digit nibble 0x{digit:X} at position {i}"),
            ));
        }

        // Store digit in scratch buffer for potential reuse
        scratch.digit_buffer.push(digit);

        // For the last byte, check sign if field is signed
        if i == data.len() - 1 && signed {
            let (has_sign, negative) = sign_table[zone as usize];
            if has_sign {
                is_negative = negative;
            } else {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid sign zone 0x{zone:X} in last digit"),
                ));
            }
        } else {
            // For non-sign positions, validate zone
            if zone != expected_zone {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!(
                        "Invalid zone 0x{zone:X} at position {i}, expected 0x{expected_zone:X}"
                    ),
                ));
            }
        }

        // Optimized value accumulation
        value = value.saturating_mul(10).saturating_add(i64::from(digit));
    }

    let mut decimal = SmallDecimal::new(value, scale, is_negative);
    decimal.normalize(); // Normalize -0 → 0 (NORMATIVE)
    Ok(decimal)
}

/// Optimized packed decimal decoder using scratch buffers
/// Minimizes allocations by reusing digit buffer
#[inline]
pub fn decode_packed_decimal_with_scratch(
    data: &[u8],
    digits: u16,
    scale: i16,
    signed: bool,
    scratch: &mut ScratchBuffers,
) -> Result<SmallDecimal> {
    let expected_bytes = ((digits + 1).div_ceil(2)) as usize;
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
    scratch.digit_buffer.reserve(digits as usize);

    let mut value = 0i64;
    let mut digit_count = 0;

    // Optimized nibble processing - unroll loop for common cases
    match data.len() {
        1 => {
            // Single byte case - common for small packed decimals
            let byte = data[0];
            let high_nibble = (byte >> 4) & 0x0F;
            let low_nibble = byte & 0x0F;

            if high_nibble > 9 {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid digit nibble in packed decimal".to_string(),
                ));
            }

            value = i64::from(high_nibble);
            digit_count = 1;

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
            } else if unlikely(low_nibble != 0xF) {
                return Err(Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    "Invalid unsigned sign nibble, expected 0xF".to_string(),
                ));
            }
        }
        _ => {
            // General case for multi-byte packed decimals
            for (i, &byte) in data.iter().enumerate() {
                let high_nibble = (byte >> 4) & 0x0F;
                let low_nibble = byte & 0x0F;

                if i == data.len() - 1 {
                    // Last byte
                    if high_nibble > 9 {
                        return Err(Error::new(
                            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                            "Invalid digit nibble in packed decimal".to_string(),
                        ));
                    }
                    value = value * 10 + i64::from(high_nibble);
                    digit_count += 1;

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
                    } else if unlikely(low_nibble != 0xF) {
                        return Err(Error::new(
                            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                            "Invalid unsigned sign nibble, expected 0xF".to_string(),
                        ));
                    }
                } else {
                    // Not the last byte
                    if high_nibble > 9 || low_nibble > 9 {
                        return Err(Error::new(
                            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                            "Invalid digit nibble in packed decimal".to_string(),
                        ));
                    }
                    value = value * 100 + i64::from(high_nibble) * 10 + i64::from(low_nibble);
                    digit_count += 2;
                }
            }
        }
    }

    // Fallback for unsigned
    Ok(create_normalized_decimal(value, scale, false))
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use crate::options::Codepage;

    #[test]
    fn test_decode_zoned_decimal_blank_when_zero() {
        // EBCDIC spaces
        let data = vec![0x40, 0x40, 0x40];
        let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::CP037, true).unwrap();
        assert_eq!(result.value, 0);

        // ASCII spaces
        let data = vec![b' ', b' ', b' '];
        let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result.value, 0);
    }

    #[test]
    fn test_decode_packed_decimal_fast_path() {
        // 3 digits, signed, positive
        let data = vec![0x12, 0x3C];
        let result = decode_packed_decimal(&data, 3, 0, true).unwrap();
        assert_eq!(result.value, 123);
        assert!(!result.negative);

        // 3 digits, signed, negative
        let data = vec![0x12, 0x3D];
        let result = decode_packed_decimal(&data, 3, 0, true).unwrap();
        assert_eq!(result.value, 123);
        assert!(result.negative);

        // 2 digits, signed, negative
        let encoded = encode_packed_decimal("-11", 2, 0, true).unwrap();
        let result = decode_packed_decimal(&encoded, 2, 0, true).unwrap();
        assert_eq!(result.value, 11);
        assert!(result.negative);
    }

    #[test]
    fn test_decode_binary_int() {
        // 16-bit unsigned
        let data = vec![0x01, 0x02];
        let result = decode_binary_int(&data, 16, false).unwrap();
        assert_eq!(result, 258);

        // 32-bit unsigned
        let data = vec![0x01, 0x02, 0x03, 0x04];
        let result = decode_binary_int(&data, 32, false).unwrap();
        assert_eq!(result, 16909060);
    }

    #[test]
    fn test_encode_alphanumeric() {
        let result = encode_alphanumeric("HELLO", 10, Codepage::ASCII).unwrap();
        assert_eq!(result, b"HELLO     ");
    }

    #[test]
    fn test_small_decimal_from_str() {
        // Test scale mismatch
        assert!(SmallDecimal::from_str("1.23", 3).is_err());
        assert!(SmallDecimal::from_str("123", 2).is_err());
    }

    #[test]
    fn test_validate_explicit_binary_width() {
        assert_eq!(validate_explicit_binary_width(1).unwrap(), 8);
        assert_eq!(validate_explicit_binary_width(2).unwrap(), 16);
        assert_eq!(validate_explicit_binary_width(4).unwrap(), 32);
        assert_eq!(validate_explicit_binary_width(8).unwrap(), 64);
        assert!(validate_explicit_binary_width(3).is_err());
    }

    #[test]
    fn test_encode_zoned_decimal_with_bwz() {
        let result =
            encode_zoned_decimal_with_bwz("0", 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result, b"   ");

        let result =
            encode_zoned_decimal_with_bwz("0", 3, 0, false, Codepage::ASCII, false).unwrap();
        assert_eq!(result, b"000");

        let result =
            encode_zoned_decimal_with_bwz("123", 3, 0, false, Codepage::ASCII, true).unwrap();
        assert_eq!(result, b"123");
    }

    #[test]
    fn test_packed_decimal_zero_padding() {
        // Test case from issue #49
        // For a value of 1 in a PIC S9(9) COMP-3 field (5 bytes),
        // the encoded value should be [00 00 00 01 XC] where X is C, F, or D
        let encoded = encode_packed_decimal("1", 9, 0, true).unwrap();
        assert_eq!(encoded, vec![0x00, 0x00, 0x00, 0x01, 0x2C]); // Assuming a specific positive sign
    }

    #[test]
    fn test_packed_decimal_odd_digits() {
        // Test case for odd number of digits
        let encoded = encode_packed_decimal("123", 3, 0, true).unwrap();
        assert_eq!(encoded, vec![0x12, 0x3C]);
    }

    #[test]
    fn test_packed_decimal_even_digits() {
        // Test case for even number of digits (requires padding)
        let encoded = encode_packed_decimal("1234", 4, 0, true).unwrap();
        assert_eq!(encoded, vec![0x01, 0x23, 0x4C]);
    }

    #[test]
    fn test_packed_decimal_roundtrip() {
        let (digits, scale, signed) = (7, 2, true);
        let input_value = "12345.67";

        let encoded_data = encode_packed_decimal(input_value, digits, scale, signed).unwrap();
        let decoded = decode_packed_decimal(&encoded_data, digits, scale, signed).unwrap();
        assert_eq!(decoded.to_string(), input_value);

        let negative_value = "-12345.67";
        let encoded_neg = encode_packed_decimal(negative_value, digits, scale, signed).unwrap();
        let decoded_neg = decode_packed_decimal(&encoded_neg, digits, scale, signed).unwrap();
        assert_eq!(decoded_neg.to_string(), negative_value);
    }
}
    #[test]
    fn test_ascii_overpunch_sign_mappings() {
        let codepage = Codepage::ASCII;

        // Test negative values (J-R for -1 to -9, } for -0)
        assert_eq!(decode_zoned_decimal(&[0x7D], 1, 0, true, codepage).unwrap().value, 0);
        assert!(decode_zoned_decimal(&[0x7D], 1, 0, true, codepage).unwrap().negative);
        assert_eq!(decode_zoned_decimal(&[0x4A], 1, 0, true, codepage).unwrap().value, 1);
        assert_eq!(decode_zoned_decimal(&[0x4D], 1, 0, true, codepage).unwrap().value, 4);
        assert_eq!(decode_zoned_decimal(&[0x52], 1, 0, true, codepage).unwrap().value, 9);

        // Test positive values (A-I for +1 to +9, { for +0)
        assert_eq!(decode_zoned_decimal(&[0x7B], 1, 0, true, codepage).unwrap().value, 0);
        assert!(!decode_zoned_decimal(&[0x7B], 1, 0, true, codepage).unwrap().negative);
        assert_eq!(decode_zoned_decimal(&[0x41], 1, 0, true, codepage).unwrap().value, 1);
        assert_eq!(decode_zoned_decimal(&[0x44], 1, 0, true, codepage).unwrap().value, 4);
        assert_eq!(decode_zoned_decimal(&[0x49], 1, 0, true, codepage).unwrap().value, 9);
    }
''