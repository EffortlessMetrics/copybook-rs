//! Edited PIC (Phase E2) decode support
//!
//! This module implements decode for edited numeric PICTURE clauses following IBM COBOL specifications.
//! Edited PICs include formatting symbols like Z (zero suppression), $ (currency), comma, decimal point,
//! and sign editing (+, -, CR, DB).
//!
//! The decode algorithm walks the input string and PIC pattern in lockstep, extracting numeric digits
//! and validating formatting symbols.

use copybook_core::{Error, ErrorCode, Result};
use tracing::warn;

/// Pattern tokens for edited PIC clauses
#[derive(Debug, Clone, PartialEq)]
pub enum PicToken {
    /// Numeric digit (9) - always displays
    Digit,
    /// Zero suppression (Z) - displays space if leading zero
    ZeroSuppress,
    /// Zero insert (0) - always displays '0'
    ZeroInsert,
    /// Asterisk fill (*) - displays '*' for leading zeros
    AsteriskFill,
    /// Blank space (B)
    Space,
    /// Literal comma
    Comma,
    /// Literal slash
    Slash,
    /// Decimal point
    DecimalPoint,
    /// Currency symbol ($)
    Currency,
    /// Leading plus sign
    LeadingPlus,
    /// Leading minus sign
    LeadingMinus,
    /// Trailing plus sign
    TrailingPlus,
    /// Trailing minus sign
    TrailingMinus,
    /// Credit (CR) - two characters
    Credit,
    /// Debit (DB) - two characters
    Debit,
}

impl PicToken {
    /// Returns true if this token represents a numeric position
    #[inline]
    #[allow(dead_code)] // May be used in future optimizations
    const fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Digit | Self::ZeroSuppress | Self::ZeroInsert | Self::AsteriskFill
        )
    }

    /// Returns true if this token is a sign editing symbol
    #[inline]
    #[allow(dead_code)] // May be used in future optimizations
    const fn is_sign(&self) -> bool {
        matches!(
            self,
            Self::LeadingPlus
                | Self::LeadingMinus
                | Self::TrailingPlus
                | Self::TrailingMinus
                | Self::Credit
                | Self::Debit
        )
    }
}

impl std::fmt::Display for PicToken {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Digit => write!(f, "9"),
            Self::ZeroSuppress => write!(f, "Z"),
            Self::ZeroInsert => write!(f, "0"),
            Self::AsteriskFill => write!(f, "*"),
            Self::Space => write!(f, "B"),
            Self::Comma => write!(f, ","),
            Self::Slash => write!(f, "/"),
            Self::DecimalPoint => write!(f, "."),
            Self::Currency => write!(f, "$"),
            Self::LeadingPlus | Self::TrailingPlus => write!(f, "+"),
            Self::LeadingMinus | Self::TrailingMinus => write!(f, "-"),
            Self::Credit => write!(f, "CR"),
            Self::Debit => write!(f, "DB"),
        }
    }
}

/// Sign extracted from edited PIC
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Sign {
    /// Positive or unsigned
    Positive,
    /// Negative
    Negative,
}

/// Tokenize an edited PIC pattern into tokens
///
/// # Errors
/// Returns error if the PIC pattern is malformed
#[inline]
#[allow(clippy::too_many_lines)]
pub fn tokenize_edited_pic(pic_str: &str) -> Result<Vec<PicToken>> {
    let mut tokens = Vec::new();
    let mut chars = pic_str.chars().peekable();
    let mut found_decimal = false;

    // Skip leading 'S' if present (sign is handled separately via sign editing symbols)
    if chars.peek() == Some(&'S') || chars.peek() == Some(&'s') {
        chars.next();
    }

    while let Some(ch) = chars.next() {
        match ch.to_ascii_uppercase() {
            '9' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::Digit);
                }
            }
            'Z' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::ZeroSuppress);
                }
            }
            '0' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::ZeroInsert);
                }
            }
            '*' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::AsteriskFill);
                }
            }
            'B' => {
                let count = parse_repetition(&mut chars)?;
                for _ in 0..count {
                    tokens.push(PicToken::Space);
                }
            }
            ',' => tokens.push(PicToken::Comma),
            '/' => tokens.push(PicToken::Slash),
            '.' => {
                if found_decimal {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Multiple decimal points in edited PIC: {pic_str}"),
                    ));
                }
                found_decimal = true;
                tokens.push(PicToken::DecimalPoint);
            }
            '$' => tokens.push(PicToken::Currency),
            '+' => {
                // Check if at beginning (leading) or end (trailing)
                if tokens.is_empty() {
                    tokens.push(PicToken::LeadingPlus);
                } else {
                    tokens.push(PicToken::TrailingPlus);
                }
            }
            '-' => {
                // Check if at beginning (leading) or end (trailing)
                if tokens.is_empty() {
                    tokens.push(PicToken::LeadingMinus);
                } else {
                    tokens.push(PicToken::TrailingMinus);
                }
            }
            'C' => {
                // Check for CR
                if let Some(&next_ch) = chars.peek()
                    && (next_ch == 'R' || next_ch == 'r')
                {
                    chars.next(); // consume 'R'
                    tokens.push(PicToken::Credit);
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Invalid character 'C' in edited PIC: {pic_str}"),
                    ));
                }
            }
            'D' => {
                // Check for DB
                if let Some(&next_ch) = chars.peek()
                    && (next_ch == 'B' || next_ch == 'b')
                {
                    chars.next(); // consume 'B'
                    tokens.push(PicToken::Debit);
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Invalid character 'D' in edited PIC: {pic_str}"),
                    ));
                }
            }
            _ => {
                // Skip implicit decimal point markers (V), whitespace, or unknown characters
            }
        }
    }

    if tokens.is_empty() {
        return Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Empty or invalid edited PIC pattern: {pic_str}"),
        ));
    }

    Ok(tokens)
}

/// Parse repetition count from chars like (5)
fn parse_repetition<I>(chars: &mut std::iter::Peekable<I>) -> Result<usize>
where
    I: Iterator<Item = char>,
{
    if chars.peek() == Some(&'(') {
        chars.next(); // consume '('
        let mut count_str = String::new();
        while let Some(&ch) = chars.peek() {
            if ch == ')' {
                chars.next(); // consume ')'
                break;
            } else if ch.is_ascii_digit() {
                count_str.push(ch);
                chars.next();
            } else {
                return Err(Error::new(
                    ErrorCode::CBKP001_SYNTAX,
                    format!("Invalid repetition count: {count_str}"),
                ));
            }
        }
        count_str.parse::<usize>().map_err(|_| {
            Error::new(
                ErrorCode::CBKP001_SYNTAX,
                format!("Invalid repetition count: {count_str}"),
            )
        })
    } else {
        Ok(1)
    }
}

/// Decoded numeric value from edited PIC
#[derive(Debug, Clone, PartialEq)]
pub struct NumericValue {
    /// Sign of the number
    pub sign: Sign,
    /// Digits without decimal point (e.g., "12345" for 123.45 with scale=2)
    pub digits: String,
    /// Number of decimal places
    pub scale: u16,
}

impl NumericValue {
    /// Format as decimal string for JSON output
    #[must_use]
    #[inline]
    pub fn to_decimal_string(&self) -> String {
        if self.digits.is_empty() || self.digits.chars().all(|c| c == '0') {
            return "0".to_string();
        }

        let sign_prefix = match self.sign {
            Sign::Positive => "",
            Sign::Negative => "-",
        };

        if self.scale == 0 {
            // Integer - just return digits with sign
            format!("{sign_prefix}{}", self.digits)
        } else {
            // Decimal - insert decimal point
            let scale = self.scale as usize;
            let digits_len = self.digits.len();

            if scale >= digits_len {
                // Need leading zeros (e.g., 0.0123)
                let zeros = "0".repeat(scale - digits_len);
                format!("{sign_prefix}0.{zeros}{}", self.digits)
            } else {
                // Split at decimal point
                let (int_part, frac_part) = self.digits.split_at(digits_len - scale);
                if int_part.is_empty() {
                    format!("{sign_prefix}0.{frac_part}")
                } else {
                    format!("{sign_prefix}{int_part}.{frac_part}")
                }
            }
        }
    }
}

/// Decode edited numeric string according to PICTURE pattern
///
/// # Errors
/// Returns error if the input doesn't match the pattern
#[inline]
#[allow(clippy::too_many_lines)]
pub fn decode_edited_numeric(
    input: &str,
    pattern: &[PicToken],
    scale: u16,
    blank_when_zero: bool,
) -> Result<NumericValue> {
    // Check for BLANK WHEN ZERO
    if blank_when_zero && input.chars().all(|c| c == ' ') {
        warn!("CBKD423_EDITED_PIC_BLANK_WHEN_ZERO: Edited PIC field is blank, decoding as zero");
        crate::lib_api::increment_warning_counter();
        return Ok(NumericValue {
            sign: Sign::Positive,
            digits: "0".to_string(),
            scale,
        });
    }

    let input_chars: Vec<char> = input.chars().collect();
    let mut pattern_idx = 0;
    let mut input_idx = 0;
    let mut digits = String::new();
    let mut sign = Sign::Positive;
    let mut found_non_zero = false;

    // Extract sign from pattern and input
    while pattern_idx < pattern.len() {
        let token = &pattern[pattern_idx];

        if input_idx >= input_chars.len() {
            return Err(Error::new(
                ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                format!(
                    "Input too short for edited PIC pattern (expected {} characters, got {})",
                    pattern.len(),
                    input.len()
                ),
            ));
        }

        let input_char = input_chars[input_idx];

        match token {
            PicToken::Digit
            | PicToken::ZeroSuppress
            | PicToken::ZeroInsert
            | PicToken::AsteriskFill => {
                // Expect digit or space or asterisk
                if input_char.is_ascii_digit() {
                    let digit_val = input_char;
                    if digit_val != '0' {
                        found_non_zero = true;
                    }
                    if found_non_zero || matches!(token, PicToken::Digit | PicToken::ZeroInsert) {
                        digits.push(digit_val);
                    } else {
                        // Leading zero suppression - push 0 to maintain position
                        digits.push('0');
                    }
                } else if input_char == ' ' {
                    // Space for zero suppression
                    if matches!(token, PicToken::Digit) {
                        // Required digit position cannot be space
                        return Err(Error::new(
                            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                            format!("Expected digit but found space at position {input_idx}"),
                        ));
                    }
                    digits.push('0');
                } else if input_char == '*' {
                    // Asterisk fill for check protection
                    if !matches!(token, PicToken::AsteriskFill) {
                        return Err(Error::new(
                            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                            format!("Unexpected asterisk at position {input_idx}"),
                        ));
                    }
                    digits.push('0');
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!(
                            "Expected digit, space, or asterisk but found '{input_char}' at position {input_idx}"
                        ),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Space => {
                if input_char != ' ' && input_char != 'B' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!("Expected space but found '{input_char}' at position {input_idx}"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Comma => {
                if input_char != ',' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!("Expected comma but found '{input_char}' at position {input_idx}"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Slash => {
                if input_char != '/' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!("Expected slash but found '{input_char}' at position {input_idx}"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::DecimalPoint => {
                if input_char != '.' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!(
                            "Expected decimal point but found '{input_char}' at position {input_idx}"
                        ),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Currency => {
                if input_char != '$' && input_char != ' ' {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        format!(
                            "Expected currency symbol but found '{input_char}' at position {input_idx}"
                        ),
                    ));
                }
                input_idx += 1;
            }
            PicToken::LeadingPlus => {
                if input_char == '+' || input_char == ' ' {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected '+' for leading plus but found '{input_char}'"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::LeadingMinus => {
                if input_char == '-' {
                    sign = Sign::Negative;
                } else if input_char == ' ' {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected '-' for leading minus but found '{input_char}'"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::TrailingPlus => {
                if input_char == '+' || input_char == ' ' {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected '+' for trailing plus but found '{input_char}'"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::TrailingMinus => {
                if input_char == '-' {
                    sign = Sign::Negative;
                } else if input_char == ' ' {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected '-' for trailing minus but found '{input_char}'"),
                    ));
                }
                input_idx += 1;
            }
            PicToken::Credit => {
                // CR requires two characters
                if input_idx + 1 >= input_chars.len() {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        "Input too short for CR symbol".to_string(),
                    ));
                }
                let cr_str: String = input_chars[input_idx..input_idx + 2].iter().collect();
                if cr_str == "CR" {
                    sign = Sign::Negative;
                } else if cr_str == "  " {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected 'CR' or spaces but found '{cr_str}'"),
                    ));
                }
                input_idx += 2;
            }
            PicToken::Debit => {
                // DB requires two characters
                if input_idx + 1 >= input_chars.len() {
                    return Err(Error::new(
                        ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                        "Input too short for DB symbol".to_string(),
                    ));
                }
                let db_str: String = input_chars[input_idx..input_idx + 2].iter().collect();
                if db_str == "DB" {
                    sign = Sign::Negative;
                } else if db_str == "  " {
                    sign = Sign::Positive;
                } else {
                    return Err(Error::new(
                        ErrorCode::CBKD422_EDITED_PIC_SIGN_MISMATCH,
                        format!("Expected 'DB' or spaces but found '{db_str}'"),
                    ));
                }
                input_idx += 2;
            }
        }

        pattern_idx += 1;
    }

    // Check if we consumed all input
    if input_idx != input_chars.len() {
        return Err(Error::new(
            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
            format!(
                "Input longer than expected (pattern consumed {} chars, input has {} chars)",
                input_idx,
                input_chars.len()
            ),
        ));
    }

    // Clean up digits - remove leading zeros but preserve at least one digit
    let digits = digits.trim_start_matches('0');
    let digits = if digits.is_empty() {
        "0".to_string()
    } else {
        digits.to_string()
    };

    // If all zeros, force positive sign
    if digits == "0" {
        sign = Sign::Positive;
    }

    Ok(NumericValue {
        sign,
        digits,
        scale,
    })
}

/// Parsed numeric value for encoding
#[derive(Debug, Clone)]
struct ParsedNumeric {
    /// Sign of the number
    sign: Sign,
    /// All digits without decimal point (e.g., "12345" for 123.45)
    digits: Vec<u8>,
    /// Position of decimal point from right (0 for integers, 2 for 2 decimal places)
    decimal_places: usize,
}

/// Parse a numeric string into its components for encoding
fn parse_numeric_value(value: &str) -> Result<ParsedNumeric> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Err(Error::new(
            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
            "Empty numeric value",
        ));
    }

    let mut chars = trimmed.chars().peekable();
    let sign = if chars.peek() == Some(&'-') {
        chars.next();
        Sign::Negative
    } else if chars.peek() == Some(&'+') {
        chars.next();
        Sign::Positive
    } else {
        Sign::Positive
    };

    let mut digits = Vec::new();
    let mut found_decimal = false;
    let mut decimal_places = 0;
    let mut found_digit = false;

    for ch in chars {
        if ch.is_ascii_digit() {
            digits.push(ch as u8 - b'0');
            if found_decimal {
                decimal_places += 1;
            }
            found_digit = true;
        } else if ch == '.' {
            if found_decimal {
                return Err(Error::new(
                    ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                    format!("Multiple decimal points in value: {value}"),
                ));
            }
            found_decimal = true;
        } else {
            return Err(Error::new(
                ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
                format!("Invalid character '{ch}' in numeric value: {value}"),
            ));
        }
    }

    if !found_digit {
        return Err(Error::new(
            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
            format!("No digits found in value: {value}"),
        ));
    }

    Ok(ParsedNumeric {
        sign,
        digits,
        decimal_places,
    })
}

/// Encode a numeric value to an edited PIC string
///
/// # Errors
/// Returns error if the value cannot be encoded to the pattern
#[inline]
#[allow(clippy::too_many_lines)]
pub fn encode_edited_numeric(
    value: &str,
    pattern: &[PicToken],
    scale: u16,
    _blank_when_zero: bool,
) -> Result<String> {
    // Parse the input value
    let parsed = parse_numeric_value(value)?;

    // Check for unsupported tokens (E3.2 supports trailing signs)
    for token in pattern {
        match token {
            PicToken::Digit
            | PicToken::ZeroSuppress
            | PicToken::ZeroInsert
            | PicToken::DecimalPoint
            | PicToken::LeadingPlus
            | PicToken::LeadingMinus
            | PicToken::TrailingPlus
            | PicToken::TrailingMinus => {}
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
                    format!("Edited PIC token not supported in E3.2: {token:?}"),
                ));
            }
        }
    }

    // Check if value is all zeros (force positive sign)
    let is_zero = parsed.digits.iter().all(|&d| d == 0);
    let effective_sign = if is_zero { Sign::Positive } else { parsed.sign };

    // Count numeric positions and decimal point in pattern
    let mut has_decimal = false;
    for token in pattern {
        if *token == PicToken::DecimalPoint {
            has_decimal = true;
        }
    }

    // Calculate expected decimal places from pattern
    let _pattern_decimal_places = if has_decimal {
        // Count numeric positions after decimal point
        let mut after_decimal = 0;
        let mut found = false;
        for token in pattern {
            if *token == PicToken::DecimalPoint {
                found = true;
            } else if found
                && matches!(
                    token,
                    PicToken::Digit | PicToken::ZeroSuppress | PicToken::ZeroInsert
                )
            {
                after_decimal += 1;
            }
        }
        after_decimal
    } else {
        0
    };

    // Adjust digits to match pattern scale
    let scale = scale as usize;
    let mut adjusted_digits = parsed.digits.clone();

    // Pad or truncate to match scale
    if scale > parsed.decimal_places {
        // Need to add trailing zeros
        let to_add = scale - parsed.decimal_places;
        adjusted_digits.extend(std::iter::repeat_n(0, to_add));
    } else if scale < parsed.decimal_places {
        // Need to truncate (round down for now)
        let to_remove = parsed.decimal_places - scale;
        for _ in 0..to_remove {
            adjusted_digits.pop();
        }
    }

    // Calculate integer and fractional parts
    let decimal_places = scale;
    let total_digits = adjusted_digits.len();
    let int_digits = total_digits.saturating_sub(decimal_places);

    // Count integer and fractional positions in pattern
    let mut int_positions = 0;
    let mut frac_positions = 0;
    let mut after_decimal = false;
    for token in pattern {
        match token {
            PicToken::Digit | PicToken::ZeroSuppress | PicToken::ZeroInsert => {
                if after_decimal {
                    frac_positions += 1;
                } else {
                    int_positions += 1;
                }
            }
            PicToken::DecimalPoint => {
                after_decimal = true;
            }
            _ => {}
        }
    }

    // Check if value fits in pattern
    if int_digits > int_positions || decimal_places > frac_positions {
        return Err(Error::new(
            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT,
            format!(
                "Value too long for pattern (pattern has {int_positions} integer positions, value has {int_digits} digits)"
            ),
        ));
    }

    // Build output string by filling from right to left
    let mut result: Vec<char> = vec![' '; pattern.len()];
    let mut int_digit_idx = int_digits; // Start from the end
    let mut frac_digit_idx = decimal_places; // Start from the end

    // Fill from right to left
    for (i, token) in pattern.iter().enumerate().rev() {
        match token {
            PicToken::Digit => {
                let digit = if i < pattern.len() && pattern[i] == PicToken::DecimalPoint {
                    // Should not happen
                    '0'
                } else {
                    // Check if this position is before or after decimal
                    let is_after_decimal = pattern[..i].contains(&PicToken::DecimalPoint);
                    if is_after_decimal && frac_digit_idx > 0 {
                        frac_digit_idx -= 1;
                        char::from_digit(
                            u32::from(adjusted_digits[int_digits + frac_digit_idx]),
                            10,
                        )
                        .unwrap_or('0')
                    } else if !is_after_decimal && int_digit_idx > 0 {
                        int_digit_idx -= 1;
                        char::from_digit(u32::from(adjusted_digits[int_digit_idx]), 10)
                            .unwrap_or('0')
                    } else {
                        '0'
                    }
                };
                result[i] = digit;
            }
            PicToken::ZeroSuppress => {
                let is_after_decimal = pattern[..i].contains(&PicToken::DecimalPoint);
                if is_after_decimal && frac_digit_idx > 0 {
                    frac_digit_idx -= 1;
                    let d = adjusted_digits[int_digits + frac_digit_idx];
                    result[i] = char::from_digit(u32::from(d), 10).unwrap_or('0');
                } else if !is_after_decimal && int_digit_idx > 0 {
                    int_digit_idx -= 1;
                    let d = adjusted_digits[int_digit_idx];
                    result[i] = char::from_digit(u32::from(d), 10).unwrap_or('0');
                } else {
                    result[i] = ' ';
                }
            }
            PicToken::ZeroInsert => {
                let is_after_decimal = pattern[..i].contains(&PicToken::DecimalPoint);
                if is_after_decimal && frac_digit_idx > 0 {
                    frac_digit_idx -= 1;
                    let d = adjusted_digits[int_digits + frac_digit_idx];
                    result[i] = char::from_digit(u32::from(d), 10).unwrap_or('0');
                } else if !is_after_decimal && int_digit_idx > 0 {
                    int_digit_idx -= 1;
                    let d = adjusted_digits[int_digit_idx];
                    result[i] = char::from_digit(u32::from(d), 10).unwrap_or('0');
                } else {
                    result[i] = '0';
                }
            }
            PicToken::DecimalPoint => {
                result[i] = '.';
            }
            PicToken::LeadingPlus | PicToken::TrailingPlus => {
                result[i] = match effective_sign {
                    Sign::Positive => '+',
                    Sign::Negative => '-',
                };
            }
            PicToken::LeadingMinus | PicToken::TrailingMinus => {
                result[i] = match effective_sign {
                    Sign::Positive => ' ',
                    Sign::Negative => '-',
                };
            }
            _ => {
                // Should have been caught by unsupported check
                return Err(Error::new(
                    ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED,
                    format!("Edited PIC token not supported: {token:?}"),
                ));
            }
        }
    }

    Ok(result.into_iter().collect())
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple_z() {
        let tokens = tokenize_edited_pic("ZZZ9").unwrap();
        assert_eq!(
            tokens,
            vec![
                PicToken::ZeroSuppress,
                PicToken::ZeroSuppress,
                PicToken::ZeroSuppress,
                PicToken::Digit
            ]
        );
    }

    #[test]
    fn test_tokenize_with_decimal() {
        let tokens = tokenize_edited_pic("ZZZ9.99").unwrap();
        assert_eq!(
            tokens,
            vec![
                PicToken::ZeroSuppress,
                PicToken::ZeroSuppress,
                PicToken::ZeroSuppress,
                PicToken::Digit,
                PicToken::DecimalPoint,
                PicToken::Digit,
                PicToken::Digit
            ]
        );
    }

    #[test]
    fn test_tokenize_currency() {
        let tokens = tokenize_edited_pic("$ZZ,ZZZ.99").unwrap();
        assert_eq!(tokens[0], PicToken::Currency);
        assert!(tokens.contains(&PicToken::Comma));
        assert!(tokens.contains(&PicToken::DecimalPoint));
    }

    #[test]
    fn test_decode_simple() {
        let pattern = tokenize_edited_pic("ZZZ9").unwrap();
        let result = decode_edited_numeric("  12", &pattern, 0, false).unwrap();
        assert_eq!(result.sign, Sign::Positive);
        assert_eq!(result.digits, "12");
        assert_eq!(result.to_decimal_string(), "12");
    }

    #[test]
    fn test_decode_with_decimal() {
        let pattern = tokenize_edited_pic("ZZZ9.99").unwrap();
        let result = decode_edited_numeric("  12.34", &pattern, 2, false).unwrap();
        assert_eq!(result.sign, Sign::Positive);
        assert_eq!(result.digits, "1234");
        assert_eq!(result.scale, 2);
        assert_eq!(result.to_decimal_string(), "12.34");
    }

    #[test]
    fn test_decode_blank_when_zero() {
        let pattern = tokenize_edited_pic("ZZZ9").unwrap();
        let result = decode_edited_numeric("    ", &pattern, 0, true).unwrap();
        assert_eq!(result.to_decimal_string(), "0");
    }

    #[test]
    fn test_decode_with_currency() {
        let pattern = tokenize_edited_pic("$ZZZ.99").unwrap();
        let result = decode_edited_numeric("$ 12.34", &pattern, 2, false).unwrap();
        assert_eq!(result.to_decimal_string(), "12.34");
    }

    #[test]
    fn test_decode_trailing_cr() {
        let pattern = tokenize_edited_pic("ZZZ9CR").unwrap();
        let result = decode_edited_numeric("  12CR", &pattern, 0, false).unwrap();
        assert_eq!(result.sign, Sign::Negative);
        assert_eq!(result.to_decimal_string(), "-12");
    }

    #[test]
    fn test_decode_trailing_db() {
        let pattern = tokenize_edited_pic("ZZZ9DB").unwrap();
        let result = decode_edited_numeric("  12DB", &pattern, 0, false).unwrap();
        assert_eq!(result.sign, Sign::Negative);
        assert_eq!(result.to_decimal_string(), "-12");
    }

    // ===== E3.1 Encode Tests =====

    #[test]
    fn test_encode_basic_digits() {
        let pattern = tokenize_edited_pic("9999").unwrap();
        let result = encode_edited_numeric("1234", &pattern, 0, false).unwrap();
        assert_eq!(result, "1234");
    }

    #[test]
    fn test_encode_zero_with_zero_insert() {
        let pattern = tokenize_edited_pic("9999").unwrap();
        let result = encode_edited_numeric("0", &pattern, 0, false).unwrap();
        assert_eq!(result, "0000");
    }

    #[test]
    fn test_encode_zero_suppression() {
        let pattern = tokenize_edited_pic("ZZZ9").unwrap();
        let result = encode_edited_numeric("123", &pattern, 0, false).unwrap();
        assert_eq!(result, " 123");
    }

    #[test]
    fn test_encode_zero_suppression_zero() {
        let pattern = tokenize_edited_pic("ZZZ9").unwrap();
        let result = encode_edited_numeric("0", &pattern, 0, false).unwrap();
        assert_eq!(result, "   0");
    }

    #[test]
    fn test_encode_zero_suppression_single_digit() {
        let pattern = tokenize_edited_pic("ZZZ9").unwrap();
        let result = encode_edited_numeric("1", &pattern, 0, false).unwrap();
        assert_eq!(result, "   1");
    }

    #[test]
    fn test_encode_zero_insert() {
        let pattern = tokenize_edited_pic("0009").unwrap();
        let result = encode_edited_numeric("123", &pattern, 0, false).unwrap();
        assert_eq!(result, "0123");
    }

    #[test]
    fn test_encode_zero_insert_all_zeros() {
        let pattern = tokenize_edited_pic("0009").unwrap();
        let result = encode_edited_numeric("0", &pattern, 0, false).unwrap();
        assert_eq!(result, "0000");
    }

    #[test]
    fn test_encode_decimal_point() {
        let pattern = tokenize_edited_pic("99.99").unwrap();
        let result = encode_edited_numeric("12.34", &pattern, 2, false).unwrap();
        assert_eq!(result, "12.34");
    }

    #[test]
    fn test_encode_zero_decimal() {
        let pattern = tokenize_edited_pic("99.99").unwrap();
        let result = encode_edited_numeric("0.00", &pattern, 2, false).unwrap();
        assert_eq!(result, "00.00");
    }

    #[test]
    fn test_encode_leading_plus_positive() {
        let pattern = tokenize_edited_pic("+999").unwrap();
        let result = encode_edited_numeric("123", &pattern, 0, false).unwrap();
        assert_eq!(result, "+123");
    }

    #[test]
    fn test_encode_leading_plus_negative() {
        let pattern = tokenize_edited_pic("+999").unwrap();
        let result = encode_edited_numeric("-123", &pattern, 0, false).unwrap();
        assert_eq!(result, "-123");
    }

    #[test]
    fn test_encode_leading_minus_positive() {
        let pattern = tokenize_edited_pic("-999").unwrap();
        let result = encode_edited_numeric("123", &pattern, 0, false).unwrap();
        assert_eq!(result, " 123");
    }

    #[test]
    fn test_encode_leading_minus_negative() {
        let pattern = tokenize_edited_pic("-999").unwrap();
        let result = encode_edited_numeric("-123", &pattern, 0, false).unwrap();
        assert_eq!(result, "-123");
    }

    #[test]
    fn test_encode_leading_plus_with_decimal() {
        let pattern = tokenize_edited_pic("+99.99").unwrap();
        let result = encode_edited_numeric("12.34", &pattern, 2, false).unwrap();
        assert_eq!(result, "+12.34");
    }

    #[test]
    fn test_encode_leading_minus_with_decimal() {
        let pattern = tokenize_edited_pic("-99.99").unwrap();
        let result = encode_edited_numeric("-12.34", &pattern, 2, false).unwrap();
        assert_eq!(result, "-12.34");
    }

    #[test]
    fn test_encode_negative_zero_forces_positive() {
        let pattern = tokenize_edited_pic("-999").unwrap();
        let result = encode_edited_numeric("-0", &pattern, 0, false).unwrap();
        assert_eq!(result, " 000");
    }

    #[test]
    fn test_encode_value_too_long() {
        let pattern = tokenize_edited_pic("999").unwrap();
        let result = encode_edited_numeric("1234", &pattern, 0, false);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKD421_EDITED_PIC_INVALID_FORMAT
        ));
    }

    #[test]
    fn test_encode_unsupported_token() {
        let pattern = tokenize_edited_pic("999CR").unwrap();
        let result = encode_edited_numeric("123", &pattern, 0, false);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKD302_EDITED_PIC_NOT_IMPLEMENTED
        ));
    }

    #[test]
    fn test_encode_empty_value() {
        let pattern = tokenize_edited_pic("999").unwrap();
        let result = encode_edited_numeric("", &pattern, 0, false);
        assert!(result.is_err());
    }

    #[test]
    fn test_encode_invalid_character() {
        let pattern = tokenize_edited_pic("999").unwrap();
        let result = encode_edited_numeric("12a", &pattern, 0, false);
        assert!(result.is_err());
    }
}
