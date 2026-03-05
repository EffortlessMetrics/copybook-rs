// SPDX-License-Identifier: AGPL-3.0-or-later
//! PIC clause parsing and validation for COBOL data types.

#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

use copybook_error::{Error, ErrorCode, Result};
use std::fmt;

/// Parsed PIC clause information
#[derive(Debug, Clone, PartialEq)]
pub struct PicClause {
    pub kind: PicKind,
    pub signed: bool,
    pub digits: u16,
    pub scale: i16,
}

/// Types of PIC clauses
#[derive(Debug, Clone, PartialEq)]
pub enum PicKind {
    /// Alphanumeric field (X)
    Alphanumeric,
    /// Numeric display field (9)
    NumericDisplay,
    /// Edited picture
    Edited,
}

impl PicClause {
    /// Parse a PIC clause string.
    ///
    /// # Errors
    /// Returns an error if the PIC clause is invalid or uses unsupported features.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn parse(pic_str: &str) -> Result<Self> {
        let pic_str = pic_str.trim();
        let pic_upper = pic_str.to_ascii_uppercase();

        if pic_upper.contains("SIGN") {
            return Err(Error::new(
                ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC,
                format!("SIGN clause is not supported: {pic_str}"),
            ));
        }

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
                    digits += 1;
                }
                '9' => {
                    if kind.is_some() && kind != Some(PicKind::NumericDisplay) {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Mixed PIC types not allowed: {}", pic_str),
                        ));
                    }
                    kind = Some(PicKind::NumericDisplay);
                    digits += 1;
                    if found_v {
                        scale += 1;
                    }
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
                }
                '(' => {
                    let mut count_str = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == ')' {
                            chars.next();
                            break;
                        }
                        if ch.is_ascii_digit() {
                            if let Some(digit_char) = chars.next() {
                                count_str.push(digit_char);
                            } else {
                                return Err(Error::new(
                                    ErrorCode::CBKP001_SYNTAX,
                                    format!("Unexpected end of digit sequence in PIC: {}", pic_str),
                                ));
                            }
                        } else {
                            return Err(Error::new(
                                ErrorCode::CBKP001_SYNTAX,
                                format!("Invalid repetition count: {}", pic_str),
                            ));
                        }
                    }

                    let count: u16 = count_str.parse().map_err(|_| {
                        Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Invalid repetition count: {}", count_str),
                        )
                    })?;

                    if count == 0 {
                        return Err(Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            "Repetition count cannot be zero".to_string(),
                        ));
                    }

                    digits = digits.saturating_sub(1) + count;
                    if found_v {
                        scale = scale.saturating_sub(1) + count as i16;
                    }
                }
                ' ' | '\t' => {}
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

        if digits == 0 {
            return Err(Error::new(
                ErrorCode::CBKP001_SYNTAX,
                "PIC clause must have at least one digit/character".to_string(),
            ));
        }

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

    /// Get byte length when stored.
    #[must_use]
    pub fn byte_length(&self) -> u32 {
        match self.kind {
            PicKind::Alphanumeric => self.digits as u32,
            PicKind::NumericDisplay => self.digits as u32,
            PicKind::Edited => 0,
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

fn is_edited_pic(pic_str: &str) -> bool {
    let edited_chars = ['Z', 'z', '/', ',', '$', '+', '-', '*'];

    let mut paren_depth: u32 = 0;
    for ch in pic_str.chars() {
        match ch {
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '0' => {
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

    pic_str.contains("CR")
        || pic_str.contains("DB")
        || pic_str.contains("cr")
        || pic_str.contains("db")
}

fn compute_edited_pic_width(pic_str: &str) -> Result<u16> {
    let mut width = 0u16;
    let mut chars = pic_str.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch.to_ascii_uppercase() {
            '9' | 'Z' | '*' | '0' => {
                if chars.peek() == Some(&'(') {
                    chars.next();
                    let mut count_str = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == ')' {
                            chars.next();
                            break;
                        } else if ch.is_ascii_digit() {
                            count_str.push(ch);
                            chars.next();
                        } else {
                            return Err(Error::new(
                                ErrorCode::CBKP001_SYNTAX,
                                format!("Invalid repetition count in edited PIC: {}", pic_str),
                            ));
                        }
                    }
                    let count: u16 = count_str.parse().map_err(|_| {
                        Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Invalid repetition count: {}", count_str),
                        )
                    })?;
                    width = width.saturating_add(count);
                } else {
                    width = width.saturating_add(1);
                }
            }
            ',' | '/' | '.' => width = width.saturating_add(1),
            '$' => width = width.saturating_add(1),
            '+' | '-' => width = width.saturating_add(1),
            'C' | 'D' => {
                if let Some(&next_ch) = chars.peek()
                    && ((ch == 'C' && (next_ch == 'R' || next_ch == 'r'))
                        || (ch == 'D' && (next_ch == 'B' || next_ch == 'b')))
                {
                    chars.next();
                    width = width.saturating_add(2);
                }
            }
            'V' | 'S' | ' ' | '\t' => {}
            _ => {}
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

fn has_sign_editing(pic_str: &str) -> bool {
    pic_str.contains("CR")
        || pic_str.contains("DB")
        || pic_str.contains("cr")
        || pic_str.contains("db")
        || pic_str.contains('+')
        || pic_str.contains('-')
}

fn compute_edited_pic_scale(pic_str: &str) -> Result<i16> {
    let mut chars = pic_str.chars().peekable();
    let mut found_decimal = false;
    let mut scale = 0i16;

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
            }
            'V' => {
                if found_decimal {
                    return Err(Error::new(
                        ErrorCode::CBKP001_SYNTAX,
                        format!("Both V and . in edited PIC: {pic_str}"),
                    ));
                }
                found_decimal = true;
            }
            '9' | 'Z' | '*' | '0' => {
                let count = if chars.peek() == Some(&'(') {
                    chars.next();
                    let mut count_str = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == ')' {
                            chars.next();
                            break;
                        } else if ch.is_ascii_digit() {
                            count_str.push(ch);
                            chars.next();
                        } else {
                            return Err(Error::new(
                                ErrorCode::CBKP001_SYNTAX,
                                format!("Invalid repetition count in edited PIC: {pic_str}"),
                            ));
                        }
                    }
                    count_str.parse::<i16>().map_err(|_| {
                        Error::new(
                            ErrorCode::CBKP001_SYNTAX,
                            format!("Invalid repetition count: {count_str}"),
                        )
                    })?
                } else {
                    1
                };

                if found_decimal {
                    scale = scale.saturating_add(count);
                }
            }
            _ => {}
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
        let result = PicClause::parse("ZZ,ZZZ.99");
        assert!(result.is_ok());
        let pic = result.unwrap();
        assert_eq!(pic.kind, PicKind::Edited);
        assert_eq!(pic.digits, 9);
        assert!(!pic.signed);
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
}
