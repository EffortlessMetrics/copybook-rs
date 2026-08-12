// SPDX-License-Identifier: AGPL-3.0-or-later
//! PIC clause parsing and validation for COBOL data types
//!
//! This module handles parsing of COBOL PICTURE clauses, which define the format
//! and storage characteristics of data items. COBOL PIC clauses specify:
//!
//! - **Data Type**: Alphanumeric (X), Numeric (9), or Alphabetic (A)
//! - **Size**: Number of characters or digits
//! - **Decimal Places**: Position of implied decimal point (V)
//! - **Sign**: Whether numeric values can be signed (S)
//!
//! ## Common COBOL PIC Examples
//!
//! ```cobol
//! PIC X(10)        -- 10 alphanumeric characters
//! PIC 9(5)         -- 5-digit unsigned integer
//! PIC S9(5)        -- 5-digit signed integer
//! PIC 9(7)V99      -- 7 digits with 2 decimal places (e.g. 12345.67)
//! PIC S9(5)V9(2)   -- Signed decimal with 5 integer and 2 fractional digits
//! ```
//!
//! This module validates supported formats and rejects edited pictures
//! (currency symbols, etc.) which require separate handling.

use crate::error::ErrorCode;
use crate::{Error, Result};
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

/// Parsed PIC clause information
///
/// Represents the result of parsing a COBOL PICTURE clause, capturing the
/// data type, sign, digit count, and decimal scale.
#[derive(Debug, Clone, PartialEq)]
pub struct PicClause {
    /// The data type category of this PIC clause.
    pub kind: PicKind,
    /// Whether this field is signed (`S` prefix in PIC clause).
    pub signed: bool,
    /// Total number of digit or character positions.
    pub digits: u16,
    /// Number of decimal places (digits after `V`). Negative for implied scaling.
    pub scale: i16,
}

/// Types of PIC clauses
///
/// # Examples
///
/// ```
/// use copybook_core::pic::PicKind;
///
/// let kind = PicKind::Alphanumeric;
/// assert_eq!(kind, PicKind::Alphanumeric);
///
/// let numeric = PicKind::NumericDisplay;
/// assert_ne!(numeric, PicKind::Alphanumeric);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum PicKind {
    /// Alphanumeric field (`PIC X`).
    Alphanumeric,
    /// Numeric display field (`PIC 9`).
    NumericDisplay,
    /// Edited numeric picture (e.g., `PIC ZZ,ZZZ.99`). Handled by Phase E1/E2/E3.
    Edited,
}

impl PicClause {
    /// Parse a PIC clause string
    ///
    /// # Errors
    /// Returns an error if the PIC clause is invalid or uses unsupported features.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn parse(pic_str: &str) -> Result<Self> {
        let pic_str = pic_str.trim();
        let pic_upper = pic_str.to_ascii_uppercase();

        // SIGN clauses are not yet supported in edited PIC decoding
        if pic_upper.contains("SIGN") {
            return Err(Error::new(
                ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                format!("SIGN clause is not supported: {pic_str}"),
            ));
        }

        // Check for edited PIC patterns first (Phase E2: parse with scale computation)
        if is_edited_pic(pic_str) {
            let width = compute_edited_pic_width(pic_str)?;
            let signed = has_sign_editing(pic_str);
            let scale = compute_edited_pic_scale(pic_str)?;
            return Ok(PicClause {
                kind: PicKind::Edited,
                signed,
                digits: width,
                scale,
            });
        }

        let mut chars = pic_str.chars().peekable();
        let mut signed = false;
        let mut digits = 0u16;
        let mut scale = 0i16;
        let mut kind = None;
        let mut found_v = false;
        let mut repetition_eligible = false;

        // Check for leading S (signed)
        if chars.peek() == Some(&'S') || chars.peek() == Some(&'s') {
            signed = true;
            chars.next();
        }

        while let Some(ch) = chars.next() {
            match ch.to_ascii_uppercase() {
                'X' => {
                    if kind.is_some() && kind != Some(PicKind::Alphanumeric) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Mixed PIC types not allowed: {}", pic_str),
                        ));
                    }
                    kind = Some(PicKind::Alphanumeric);
                    digits = checked_add_u16(digits, 1, pic_str, "PIC width")?;
                    repetition_eligible = true;
                }
                '9' => {
                    if kind.is_some() && kind != Some(PicKind::NumericDisplay) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Mixed PIC types not allowed: {}", pic_str),
                        ));
                    }
                    kind = Some(PicKind::NumericDisplay);
                    digits = checked_add_u16(digits, 1, pic_str, "PIC width")?;
                    if found_v {
                        scale = checked_add_i16(scale, 1, pic_str, "PIC scale")?;
                    }
                    repetition_eligible = true;
                }
                'V' => {
                    if found_v {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Multiple V positions not allowed: {}", pic_str),
                        ));
                    }
                    if kind != Some(PicKind::NumericDisplay) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("V only allowed in numeric PIC: {}", pic_str),
                        ));
                    }
                    found_v = true;
                    repetition_eligible = false;
                }
                '(' => {
                    if !repetition_eligible {
                        return Err(invalid_pic(
                            pic_str,
                            "repetition count must follow `X` or `9`",
                        ));
                    }
                    let count = parse_repetition_count(&mut chars, pic_str)?;

                    // Subtract 1 because we already counted the character before '('
                    let prefix = digits.checked_sub(1).ok_or_else(|| {
                        invalid_pic(pic_str, "repetition count has no preceding PIC symbol")
                    })?;
                    digits = checked_add_u16(prefix, count, pic_str, "PIC width")?;
                    if found_v {
                        let count = i16::try_from(count)
                            .map_err(|_| invalid_pic(pic_str, "PIC scale exceeds i16::MAX"))?;
                        let prefix = scale.checked_sub(1).ok_or_else(|| {
                            invalid_pic(pic_str, "repetition count has no fractional PIC symbol")
                        })?;
                        scale = checked_add_i16(prefix, count, pic_str, "PIC scale")?;
                    }
                    repetition_eligible = false;
                }
                ' ' | '\t' => {
                    // Skip whitespace
                    repetition_eligible = false;
                }
                _ => {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Invalid character in PIC clause: {}", ch),
                    ));
                }
            }
        }

        let kind = kind
            .ok_or_else(|| Error::new(ErrorCode::CBKP001_SYNTAX, "Empty PIC clause".to_string()))?;

        // Validate constraints
        if digits == 0 {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "PIC clause must have at least one digit/character".to_string(),
            ));
        }

        // Note: digits is u16, so max value is already 65535 - no need to check

        // Alphanumeric fields cannot be signed
        if signed && kind == PicKind::Alphanumeric {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "Alphanumeric PIC cannot be signed".to_string(),
            ));
        }

        Ok(PicClause {
            kind,
            signed,
            digits,
            scale,
        })
    }

    /// Get the byte length of this field when stored in a record.
    ///
    /// For `Edited` PIC kinds, returns 0 because the display width is used instead.
    pub fn byte_length(&self) -> u32 {
        match self.kind {
            PicKind::Alphanumeric => self.digits as u32,
            PicKind::NumericDisplay => self.digits as u32,
            PicKind::Edited => 0, // Should never reach here
        }
    }
}

impl fmt::Display for PicClause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sign_prefix = if self.signed { "S" } else { "" };

        match self.kind {
            PicKind::Alphanumeric => {
                if self.digits == 1 {
                    write!(f, "{}X", sign_prefix)
                } else {
                    write!(f, "{}X({})", sign_prefix, self.digits)
                }
            }
            PicKind::NumericDisplay => {
                if self.scale == 0 {
                    if self.digits == 1 {
                        write!(f, "{}9", sign_prefix)
                    } else {
                        write!(f, "{}9({})", sign_prefix, self.digits)
                    }
                } else {
                    let integer_digits = self.digits - self.scale as u16;
                    if integer_digits == 1 && self.scale == 1 {
                        write!(f, "{sign_prefix}9V9")
                    } else if integer_digits == 1 {
                        write!(f, "{}9V9({})", sign_prefix, self.scale)
                    } else if self.scale == 1 {
                        write!(f, "{sign_prefix}9({integer_digits})V9")
                    } else {
                        write!(f, "{}9({})V9({})", sign_prefix, integer_digits, self.scale)
                    }
                }
            }
            PicKind::Edited => write!(f, "EDITED"),
        }
    }
}

/// Check if a PIC string contains edited picture characters
fn is_edited_pic(pic_str: &str) -> bool {
    // Edited PIC characters: Z, /, comma, $, +, -, *
    // Note: '0' (zero insertion) is handled separately to avoid false positives
    // from count specifiers like 9(10) which contain '0' inside parentheses
    let edited_chars = ['Z', 'z', '/', ',', '$', '+', '-', '*'];

    // Track parentheses depth to distinguish count digits from edited characters
    let mut paren_depth: u32 = 0;
    for ch in pic_str.chars() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '0' => {
                // Zero is only an edited character when outside parentheses
                // (zero insertion like PIC 0999, not count like PIC 9(10))
                if paren_depth == 0 {
                    return true;
                }
            }
            _ => {
                if paren_depth == 0 && edited_chars.contains(&ch) {
                    return true;
                }
            }
        }
    }

    // Check for multi-character edited symbols
    pic_str.contains("CR")
        || pic_str.contains("DB")
        || pic_str.contains("cr")
        || pic_str.contains("db")
}

fn invalid_pic(pic_str: &str, reason: &str) -> Error {
    Error::new(
        ErrorCode::CBKP001_SYNTAX,
        format!("Invalid PIC clause `{pic_str}`: {reason}"),
    )
}

fn parse_repetition_count(chars: &mut Peekable<Chars<'_>>, pic_str: &str) -> Result<u16> {
    let mut count_str = String::new();
    let mut closed = false;
    while let Some(&ch) = chars.peek() {
        if ch == ')' {
            let _ = chars.next();
            closed = true;
            break;
        }
        if !ch.is_ascii_digit() {
            return Err(invalid_pic(
                pic_str,
                "repetition count must contain only digits",
            ));
        }
        count_str.push(ch);
        let _ = chars.next();
    }

    if !closed {
        return Err(invalid_pic(
            pic_str,
            "repetition count is missing closing `)`",
        ));
    }
    if count_str.is_empty() {
        return Err(invalid_pic(pic_str, "repetition count cannot be empty"));
    }

    let count = count_str
        .parse::<u16>()
        .map_err(|_| invalid_pic(pic_str, "repetition count exceeds u16::MAX"))?;
    if count == 0 {
        return Err(invalid_pic(pic_str, "repetition count cannot be zero"));
    }
    Ok(count)
}

fn checked_add_u16(value: u16, increment: u16, pic_str: &str, name: &str) -> Result<u16> {
    value
        .checked_add(increment)
        .ok_or_else(|| invalid_pic(pic_str, &format!("{name} exceeds u16::MAX")))
}

fn checked_add_i16(value: i16, increment: i16, pic_str: &str, name: &str) -> Result<i16> {
    value
        .checked_add(increment)
        .ok_or_else(|| invalid_pic(pic_str, &format!("{name} exceeds i16::MAX")))
}

/// Compute display width from edited PIC string
/// Example: "ZZ,ZZZ.99" → 8 (including comma and decimal point)
fn compute_edited_pic_width(pic_str: &str) -> Result<u16> {
    let mut width = 0u16;
    let mut chars = pic_str.chars().peekable();
    let mut repetition_eligible = false;

    while let Some(ch) = chars.next() {
        match ch.to_ascii_uppercase() {
            // Digit positions (including zero insertion)
            '9' | 'Z' | '*' | '0' => {
                // Check for repetition count
                if chars.peek() == Some(&'(') {
                    chars.next(); // consume '('
                    let count = parse_repetition_count(&mut chars, pic_str)?;
                    width = checked_add_u16(width, count, pic_str, "edited PIC width")?;
                    repetition_eligible = false;
                } else {
                    width = checked_add_u16(width, 1, pic_str, "edited PIC width")?;
                    repetition_eligible = true;
                }
            }
            // Insertion characters (commas, slashes, etc.)
            ',' | '/' | '.' => {
                width = checked_add_u16(width, 1, pic_str, "edited PIC width")?;
                repetition_eligible = false;
            }
            // Currency symbol
            '$' => {
                width = checked_add_u16(width, 1, pic_str, "edited PIC width")?;
                repetition_eligible = false;
            }
            // Sign symbols
            '+' | '-' => {
                width = checked_add_u16(width, 1, pic_str, "edited PIC width")?;
                repetition_eligible = false;
            }
            // CR/DB handling (2 characters)
            'C' | 'D' => {
                if let Some(&next_ch) = chars.peek()
                    && ((ch == 'C' && (next_ch == 'R' || next_ch == 'r'))
                        || (ch == 'D' && (next_ch == 'B' || next_ch == 'b')))
                {
                    chars.next(); // consume second character
                    width = checked_add_u16(width, 2, pic_str, "edited PIC width")?;
                }
                repetition_eligible = false;
            }
            // V is non-display (implied decimal)
            'V' => {
                // Don't add to width
                repetition_eligible = false;
            }
            // S prefix is non-display
            'S' => {
                // Don't add to width
                repetition_eligible = false;
            }
            // Whitespace
            ' ' | '\t' => {
                // Skip
                repetition_eligible = false;
            }
            '(' | ')' => {
                let reason = if ch == '(' && !repetition_eligible {
                    "repetition count must follow `9`, `Z`, `*`, or `0`"
                } else {
                    "malformed repetition delimiter"
                };
                return Err(invalid_pic(pic_str, reason));
            }
            _ => {
                // Unknown character - for now, just skip it
            }
        }
    }

    if width == 0 {
        return Err(Error::new(
            ErrorCode::CBKP001_SYNTAX,
            format!("Edited PIC has zero display width: {}", pic_str),
        ));
    }

    Ok(width)
}

/// Check if edited PIC has sign editing (CR, DB, +, -)
fn has_sign_editing(pic_str: &str) -> bool {
    pic_str.contains("CR")
        || pic_str.contains("DB")
        || pic_str.contains("cr")
        || pic_str.contains("db")
        || pic_str.contains('+')
        || pic_str.contains('-')
}

/// Compute decimal scale (number of digits after decimal point) for edited PIC
/// Examples:
/// - "ZZZ9" → 0
/// - "ZZZ9.99" → 2
/// - "$ZZ,ZZZ.99" → 2
/// - "9(5)V99" → 2 (V is implicit decimal, . is explicit)
fn compute_edited_pic_scale(pic_str: &str) -> Result<i16> {
    let mut chars = pic_str.chars().peekable();
    let mut found_decimal = false;
    let mut scale = 0i16;
    let mut repetition_eligible = false;

    // Skip leading 'S' if present
    if chars.peek() == Some(&'S') || chars.peek() == Some(&'s') {
        chars.next();
    }

    while let Some(ch) = chars.next() {
        match ch.to_ascii_uppercase() {
            '.' => {
                if found_decimal {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Multiple decimal points in edited PIC: {pic_str}"),
                    ));
                }
                found_decimal = true;
                repetition_eligible = false;
            }
            'V' => {
                // Implicit decimal point
                if found_decimal {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Both V and . in edited PIC: {pic_str}"),
                    ));
                }
                found_decimal = true;
                repetition_eligible = false;
            }
            '9' | 'Z' | '*' | '0' => {
                // Check for repetition count
                let repeated = chars.peek() == Some(&'(');
                let count = if repeated {
                    chars.next(); // consume '('
                    parse_repetition_count(&mut chars, pic_str)?
                } else {
                    1
                };

                if found_decimal {
                    let count = i16::try_from(count)
                        .map_err(|_| invalid_pic(pic_str, "PIC scale exceeds i16::MAX"))?;
                    scale = checked_add_i16(scale, count, pic_str, "PIC scale")?;
                }
                repetition_eligible = !repeated;
            }
            '(' | ')' => {
                let reason = if ch == '(' && !repetition_eligible {
                    "repetition count must follow `9`, `Z`, `*`, or `0`"
                } else {
                    "malformed repetition delimiter"
                };
                return Err(invalid_pic(pic_str, reason));
            }
            // Other characters (comma, slash, $, +, -, etc.) don't affect scale
            _ => repetition_eligible = false,
        }
    }

    Ok(scale)
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_alphanumeric_pic() {
        let pic = PicClause::parse("X(10)").unwrap();
        assert_eq!(pic.kind, PicKind::Alphanumeric);
        assert!(!pic.signed);
        assert_eq!(pic.digits, 10);
        assert_eq!(pic.scale, 0);
        assert_eq!(pic.byte_length(), 10);
    }

    #[test]
    fn test_numeric_display_pic() {
        let pic = PicClause::parse("9(5)").unwrap();
        assert_eq!(pic.kind, PicKind::NumericDisplay);
        assert!(!pic.signed);
        assert_eq!(pic.digits, 5);
        assert_eq!(pic.scale, 0);
    }

    #[test]
    fn test_signed_numeric_pic() {
        let pic = PicClause::parse("S9(7)V99").unwrap();
        assert_eq!(pic.kind, PicKind::NumericDisplay);
        assert!(pic.signed);
        assert_eq!(pic.digits, 9);
        assert_eq!(pic.scale, 2);
    }

    #[test]
    fn test_edited_pic_parses() {
        // Phase E1: edited PIC should now parse successfully
        let result = PicClause::parse("ZZ,ZZZ.99");
        assert!(result.is_ok());
        let pic = result.unwrap();
        assert_eq!(pic.kind, PicKind::Edited);
        assert_eq!(pic.digits, 9); // ZZ,ZZZ.99 = 2 + 1 + 3 + 1 + 2 = 9 display positions
        assert!(!pic.signed); // No sign editing
    }

    #[test]
    fn test_sign_clause_rejected() {
        let result = PicClause::parse("S9(5) SIGN LEADING");
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC);
    }

    #[test]
    fn test_mixed_types_error() {
        let result = PicClause::parse("X9");
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn test_signed_alphanumeric_error() {
        let result = PicClause::parse("SX(10)");
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().code,
            ErrorCode::CBKP001_SYNTAX
        ));
    }

    #[test]
    fn test_pic_display() {
        assert_eq!(PicClause::parse("X(10)").unwrap().to_string(), "X(10)");
        assert_eq!(PicClause::parse("9(5)").unwrap().to_string(), "9(5)");
        assert_eq!(
            PicClause::parse("S9(7)V99").unwrap().to_string(),
            "S9(7)V9(2)"
        );
        assert_eq!(PicClause::parse("9V9").unwrap().to_string(), "9V9");
    }

    #[test]
    fn repetition_counts_require_positive_closed_u16_values() {
        for invalid in [
            "9(10",
            "9()",
            "9(0)",
            "9(65536)",
            "9(999999999999999999999999999999)",
            "9((2))",
            "9 (2)",
            "X\t(2)",
            "9( 2)",
            "9(2 )",
            "9(2)(3)",
            "9V(2)",
            "S(2)9",
            "9)",
            "Z(10",
            "Z()",
            "Z(0)",
            "Z(65536)",
            "Z (2)",
            "*\t(2)",
            "Z( 2)",
            "Z(2 )",
            "Z(2)(3)",
            "ZV(2)",
            "Z)",
        ] {
            let error = PicClause::parse(invalid).unwrap_err();
            assert_eq!(error.code, ErrorCode::CBKP001_SYNTAX, "{invalid}");
        }
    }

    #[test]
    fn repetition_arithmetic_preserves_maximum_boundaries() {
        let plain = PicClause::parse("9(65535)").unwrap();
        assert_eq!(plain.digits, u16::MAX);
        assert_eq!(plain.scale, 0);

        let edited = PicClause::parse("Z(65535)").unwrap();
        assert_eq!(edited.digits, u16::MAX);
        assert_eq!(edited.scale, 0);

        let fractional = PicClause::parse("ZV9(32767)").unwrap();
        assert_eq!(fractional.scale, i16::MAX);

        for invalid in ["9(65535)9", "Z(65535)9", "ZV9(32767)9"] {
            let error = PicClause::parse(invalid).unwrap_err();
            assert_eq!(error.code, ErrorCode::CBKP001_SYNTAX, "{invalid}");
        }

        let error = PicClause::parse("Z(65535)V9(32768)").unwrap_err();
        assert!(error.message.contains("edited PIC width"));
    }

    #[test]
    fn ordinary_pic_whitespace_remains_supported_without_detached_repetition() {
        assert_eq!(PicClause::parse("9 9").unwrap().digits, 2);
        assert_eq!(PicClause::parse("Z Z").unwrap().digits, 2);
    }

    #[test]
    fn mixed_type_validation_precedes_later_repetition_overflow() {
        let error = PicClause::parse("9X(65536)").unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP001_SYNTAX);
        assert!(error.message.contains("Mixed PIC types"));
    }

    #[test]
    fn parser_maps_hostile_repetitions_to_invalid_pic() {
        for copybook in [
            "01 ROOT.\n   05 FIELD PIC 9(10",
            "01 ROOT.\n   05 FIELD PIC 9(65535)9.",
            "01 ROOT.\n   05 FIELD PIC Z(10",
            "01 ROOT.\n   05 FIELD PIC Z(65535)9.",
        ] {
            let error = crate::parse_copybook(copybook).unwrap_err();
            assert_eq!(error.code, ErrorCode::CBKP101_INVALID_PIC, "{copybook}");
        }

        for copybook in [
            "01 ROOT.\n   05 FIELD PIC 9 (2).",
            "01 ROOT.\n   05 FIELD PIC Z\t(2).",
            "01 ROOT.\n   05 FIELD PIC 9( 2).",
            "01 ROOT.\n   05 FIELD PIC 9(\t2).",
            "01 ROOT.\n   05 FIELD PIC Z( 2).",
            "01 ROOT.\n   05 FIELD PIC Z(\t2).",
            "01 ROOT.\n   05 FIELD PIC 9(2 ).",
            "01 ROOT.\n   05 FIELD PIC 9(2\t).",
            "01 ROOT.\n   05 FIELD PIC Z(2 ).",
            "01 ROOT.\n   05 FIELD PIC Z(2\t).",
        ] {
            let error = crate::parse_copybook(copybook).unwrap_err();
            assert_eq!(error.code, ErrorCode::CBKP101_INVALID_PIC, "{copybook}");
            assert!(error.message.contains("must be attached"), "{copybook}");
        }

        for copybook in [
            "01 ROOT.\n   05 FIELD PIC 9(\n2).",
            "01 ROOT.\n   05 FIELD PIC Z(2\n).",
        ] {
            let error = crate::parse_copybook(copybook).unwrap_err();
            assert_eq!(error.code, ErrorCode::CBKP101_INVALID_PIC, "{copybook}");
        }
    }

    #[test]
    fn parser_preserves_immediate_repetitions_and_ordinary_pic_whitespace() {
        assert_eq!(PicClause::parse("9(2)9(3)").unwrap().digits, 5);
        assert_eq!(PicClause::parse("Z(2)Z(3)").unwrap().digits, 5);

        let repeated = crate::parse_copybook("01 ROOT.\n   05 FIELD PIC 9(2)9(3).").unwrap();
        let field = repeated.find_field("ROOT.FIELD").unwrap();
        assert!(
            matches!(
                field.kind,
                crate::schema::FieldKind::ZonedDecimal { digits: 5, .. }
            ),
            "{:?}",
            field.kind
        );

        let edited = crate::parse_copybook("01 ROOT.\n   05 FIELD PIC Z(2)Z(3).").unwrap();
        let field = edited.find_field("ROOT.FIELD").unwrap();
        assert!(
            matches!(
                field.kind,
                crate::schema::FieldKind::EditedNumeric { width: 5, .. }
            ),
            "{:?}",
            field.kind
        );

        for copybook in [
            "01 ROOT.\n   05 FIELD PIC 9(2) 9(3).",
            "01 ROOT.\n   05 FIELD PIC Z(2)\tZ(3).",
        ] {
            let error = crate::parse_copybook(copybook).unwrap_err();
            assert_eq!(error.code, ErrorCode::CBKP101_INVALID_PIC, "{copybook}");
            assert!(error.message.contains("must be attached"), "{copybook}");
        }

        for copybook in [
            "01 ROOT.\n   05 FIELD PIC 9 9.",
            "01 ROOT.\n   05 FIELD PIC Z Z.",
        ] {
            assert!(crate::parse_copybook(copybook).is_ok(), "{copybook}");
        }
    }
}
