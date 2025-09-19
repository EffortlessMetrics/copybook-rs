//! COBOL copybook lexical analysis
//!
//! This module implements tokenization for COBOL copybooks, supporting both
//! fixed-form and free-form formats with proper continuation handling.

use logos::Logos;
use std::fmt;

/// COBOL copybook tokens
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\f]+")]
pub enum Token {
    // Level numbers (01-49) - highest priority
    #[regex(r"0[1-9]|[1-4][0-9]", priority = 5, callback = |lex| lex.slice().parse::<u8>().ok())]
    Level(u8),

    // Special level numbers (66, 77, 88) - highest priority
    #[token("66", priority = 6)]
    Level66,
    #[token("77", priority = 6)]
    Level77,
    #[token("88", priority = 6)]
    Level88,

    // Keywords
    #[token("PIC", ignore(case))]
    #[token("PICTURE", ignore(case))]
    Pic,

    #[token("USAGE", ignore(case))]
    Usage,

    #[token("DISPLAY", ignore(case))]
    Display,

    #[token("COMP", ignore(case))]
    #[token("COMPUTATIONAL", ignore(case))]
    Comp,

    #[token("COMP-3", ignore(case))]
    #[token("COMPUTATIONAL-3", ignore(case))]
    Comp3,

    #[token("BINARY", ignore(case))]
    Binary,

    #[token("REDEFINES", ignore(case))]
    Redefines,

    #[token("OCCURS", ignore(case))]
    Occurs,

    #[token("DEPENDING", ignore(case))]
    Depending,

    #[token("ON", ignore(case))]
    On,

    #[token("TO", ignore(case))]
    To,

    #[token("TIMES", ignore(case))]
    Times,

    #[token("SYNCHRONIZED", ignore(case))]
    #[token("SYNC", ignore(case))]
    Synchronized,

    #[token("VALUE", ignore(case))]
    Value,

    #[token("SIGN", ignore(case))]
    Sign,

    #[token("LEADING", ignore(case))]
    Leading,

    #[token("TRAILING", ignore(case))]
    Trailing,

    #[token("SEPARATE", ignore(case))]
    Separate,

    #[token("BLANK", ignore(case))]
    Blank,

    #[token("WHEN", ignore(case))]
    When,

    #[token("ZERO", ignore(case))]
    #[token("ZEROS", ignore(case))]
    #[token("ZEROES", ignore(case))]
    Zero,

    // PIC clauses - basic patterns (higher priority than identifiers)
    #[regex(r"S?X+", priority = 3, callback = |lex| lex.slice().to_string())]
    #[regex(r"S?X\([0-9]+\)", priority = 3, callback = |lex| lex.slice().to_string())]
    #[regex(r"S?9+", priority = 3, callback = |lex| lex.slice().to_string())]
    #[regex(r"S?9\([0-9]+\)", priority = 3, callback = |lex| lex.slice().to_string())]
    #[regex(r"S?9+V9+", priority = 3, callback = |lex| lex.slice().to_string())]
    #[regex(r"S?9\([0-9]+\)V9+", priority = 3, callback = |lex| lex.slice().to_string())]
    #[regex(r"S?9+V9\([0-9]+\)", priority = 3, callback = |lex| lex.slice().to_string())]
    #[regex(r"S?9\([0-9]+\)V9\([0-9]+\)", priority = 3, callback = |lex| lex.slice().to_string())]
    PicClause(String),

    // Edited PIC patterns (to be rejected) - higher priority than identifiers
    #[regex(r"[Z9]*[/,\$\+\-\*]+[Z9]*", priority = 3, callback = |lex| lex.slice().to_string())]
    EditedPic(String),

    // Numbers - higher priority than PIC clauses
    #[regex(r"[0-9]+", priority = 4, callback = |lex| lex.slice().parse::<u32>().ok())]
    Number(u32),

    // Identifiers and names - lowest priority
    #[regex(r"[A-Za-z][A-Za-z0-9\-]*", priority = 1, callback = |lex| lex.slice().to_string())]
    Identifier(String),

    // String literals
    #[regex(r#""[^"]*""#, callback = |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    #[regex(r"'[^']*'", callback = |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    StringLiteral(String),

    // Punctuation
    #[token(".")]
    Period,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    // Comments
    #[regex(r"\*>.*", callback = |lex| lex.slice()[2..].trim().to_string())]
    InlineComment(String),

    // Newlines (important for line tracking)
    #[token("\n")]
    #[token("\r\n")]
    Newline,

    // End of input
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Level(n) => write!(f, "{:02}", n),
            Token::Level66 => write!(f, "66"),
            Token::Level77 => write!(f, "77"),
            Token::Level88 => write!(f, "88"),
            Token::Pic => write!(f, "PIC"),
            Token::Usage => write!(f, "USAGE"),
            Token::Display => write!(f, "DISPLAY"),
            Token::Comp => write!(f, "COMP"),
            Token::Comp3 => write!(f, "COMP-3"),
            Token::Binary => write!(f, "BINARY"),
            Token::Redefines => write!(f, "REDEFINES"),
            Token::Occurs => write!(f, "OCCURS"),
            Token::Depending => write!(f, "DEPENDING"),
            Token::On => write!(f, "ON"),
            Token::To => write!(f, "TO"),
            Token::Times => write!(f, "TIMES"),
            Token::Synchronized => write!(f, "SYNCHRONIZED"),
            Token::Value => write!(f, "VALUE"),
            Token::Sign => write!(f, "SIGN"),
            Token::Leading => write!(f, "LEADING"),
            Token::Trailing => write!(f, "TRAILING"),
            Token::Separate => write!(f, "SEPARATE"),
            Token::Blank => write!(f, "BLANK"),
            Token::When => write!(f, "WHEN"),
            Token::Zero => write!(f, "ZERO"),
            Token::PicClause(s) => write!(f, "{}", s),
            Token::EditedPic(s) => write!(f, "{}", s),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::Period => write!(f, "."),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::InlineComment(s) => write!(f, "*> {}", s),
            Token::Newline => write!(f, "\\n"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

/// Position information for tokens
#[derive(Debug, Clone, PartialEq)]
pub struct TokenPos {
    pub token: Token,
    pub line: usize,
    pub column: usize,
    pub span: std::ops::Range<usize>,
}

/// COBOL format detection
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CobolFormat {
    Fixed,
    Free,
}

/// Lexer for COBOL copybooks
pub struct Lexer<'a> {
    _input: &'a str,
    format: CobolFormat,
    lines: Vec<ProcessedLine<'a>>,
    _current_line: usize,
    _current_pos: usize,
}

/// A processed line after format-specific handling
#[derive(Debug, Clone)]
struct ProcessedLine<'a> {
    content: &'a str,
    _original_line: usize,
    is_comment: bool,
    is_continuation: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given input
    pub fn new(input: &'a str) -> Self {
        let format = detect_format(input);
        let lines = preprocess_lines(input, format);

        Self {
            _input: input,
            format,
            lines,
            _current_line: 0,
            _current_pos: 0,
        }
    }

    /// Get the detected format
    pub fn format(&self) -> CobolFormat {
        self.format
    }

    /// Tokenize the input and return all tokens with positions
    pub fn tokenize(&mut self) -> Vec<TokenPos> {
        let mut tokens = Vec::new();
        let processed_text = self.build_processed_text();

        let mut lexer = Token::lexer(&processed_text);
        let mut line = 1;
        let mut column = 1;

        while let Some(result) = lexer.next() {
            let span = lexer.span();
            let token = if let Ok(token) = result {
                token
            } else {
                // Handle lexer errors - create an identifier token for unknown text
                let text = &processed_text[span.clone()];
                Token::Identifier(text.to_string())
            };

            // Update line/column tracking
            if token == Token::Newline {
                line += 1;
                column = 1;
            } else {
                column += span.len();
            }

            tokens.push(TokenPos {
                token,
                line,
                column: column - span.len(),
                span,
            });
        }

        // Add EOF token
        tokens.push(TokenPos {
            token: Token::Eof,
            line,
            column,
            span: processed_text.len()..processed_text.len(),
        });

        tokens
    }

    /// Build the processed text from all non-comment lines
    fn build_processed_text(&self) -> String {
        let mut result = String::new();
        let mut i = 0;

        while i < self.lines.len() {
            let line = &self.lines[i];

            if line.is_comment {
                i += 1;
                continue;
            }

            if line.is_continuation && i > 0 {
                // This is a continuation line - join with previous line
                // Remove the last newline from result and append continuation content
                if result.ends_with('\n') {
                    result.pop(); // Remove newline
                }

                // Handle continuation properly:
                // 1. Remove trailing whitespace from previous line
                // 2. For column-7 continuation, preserve hyphens if they're part of the content
                // 3. Add appropriate spacing between continued parts

                let mut trimmed_result = result.trim_end().to_string();
                let continuation_content = line.content.trim();

                // Join the content with appropriate spacing
                if !trimmed_result.is_empty() && !continuation_content.is_empty() {
                    // Check if we need a space or direct join
                    if trimmed_result.ends_with('-') && !continuation_content.starts_with('-') {
                        // Hyphenated word continuation - don't add space
                        trimmed_result.push_str(continuation_content);
                    } else {
                        // Add space for separate tokens - always add space for continuation
                        // unless it's a direct hyphenated join
                        trimmed_result.push(' ');
                        trimmed_result.push_str(continuation_content);
                    }
                } else if !continuation_content.is_empty() {
                    trimmed_result.push_str(continuation_content);
                }

                result = trimmed_result;
                result.push('\n');
            } else {
                // Regular line
                result.push_str(line.content);
                result.push('\n');
            }

            i += 1;
        }

        result
    }
}

/// Detect whether the input is fixed-form or free-form COBOL
fn detect_format(input: &str) -> CobolFormat {
    let lines: Vec<&str> = input.lines().collect();
    let mut fixed_form_indicators = 0;
    let mut total_content_lines = 0;

    for line in &lines {
        if line.trim().is_empty() || line.trim_start().starts_with('*') {
            continue; // Skip empty lines and comments
        }

        total_content_lines += 1;

        // Strong indicators of fixed-form:
        // 1. Line starts with 6+ spaces followed by content (sequence area)
        // 2. Line has content starting exactly at column 8 (after 7 chars)
        // 3. Line is exactly 72 or 80 characters (traditional fixed-form length)

        if line.len() >= 8 {
            // Check if first 6 characters are spaces/digits (sequence area)
            let first_six = &line[0..6];
            let col_7 = line.chars().nth(6).unwrap_or(' ');
            let col_8_onwards = &line[7..];

            // Fixed-form indicators:
            // - First 6 chars are spaces or digits (sequence numbers)
            // - Column 7 is space, *, -, or /
            // - Content starts at column 8
            if (first_six.chars().all(|c| c.is_ascii_digit() || c == ' '))
                && (col_7 == ' ' || col_7 == '*' || col_7 == '-' || col_7 == '/')
                && !col_8_onwards.trim().is_empty()
            {
                fixed_form_indicators += 1;
            }
        }

        // Also check for traditional fixed-form line lengths
        if line.len() == 72 || line.len() == 80 {
            fixed_form_indicators += 1;
        }
    }

    // If ≥50% of content lines show fixed-form indicators, assume fixed-form
    // Use a lower threshold since we have multiple indicators
    if total_content_lines > 0 && (fixed_form_indicators * 100 / total_content_lines) >= 50 {
        CobolFormat::Fixed
    } else {
        CobolFormat::Free
    }
}

/// Preprocess lines according to the detected format
fn preprocess_lines(input: &str, format: CobolFormat) -> Vec<ProcessedLine<'_>> {
    let mut result = Vec::new();

    for (line_num, line) in input.lines().enumerate() {
        let processed = match format {
            CobolFormat::Fixed => process_fixed_form_line(line, line_num + 1),
            CobolFormat::Free => process_free_form_line(line, line_num + 1),
        };
        result.push(processed);
    }

    result
}

/// Process a fixed-form COBOL line
fn process_fixed_form_line(line: &str, line_num: usize) -> ProcessedLine<'_> {
    if line.is_empty() {
        return ProcessedLine {
            content: "",
            _original_line: line_num,
            is_comment: false,
            is_continuation: false,
        };
    }

    // Check for comment line (* in column 1)
    if line.starts_with('*') {
        return ProcessedLine {
            content: line,
            _original_line: line_num,
            is_comment: true,
            is_continuation: false,
        };
    }

    // Check for continuation (- in column 7)
    let is_continuation = line.len() > 6 && line.chars().nth(6) == Some('-');

    // Extract content from columns 8-72 (or to end if shorter)
    let content = if line.len() > 7 {
        let end_col = if line.len() > 72 { 72 } else { line.len() };
        &line[7..end_col]
    } else {
        ""
    };

    ProcessedLine {
        content,
        _original_line: line_num,
        is_comment: false,
        is_continuation,
    }
}

/// Process a free-form COBOL line
fn process_free_form_line(line: &str, line_num: usize) -> ProcessedLine<'_> {
    let trimmed = line.trim_start();

    // Check for full comment lines (* at column 1)
    if trimmed.starts_with('*') {
        return ProcessedLine {
            content: line,
            _original_line: line_num,
            is_comment: true,
            is_continuation: false,
        };
    }

    // Handle inline comments (*> anywhere) - strip comment part
    let content = if let Some(comment_pos) = line.find("*>") {
        line[..comment_pos].trim_end()
    } else {
        line
    };

    ProcessedLine {
        content,
        _original_line: line_num,
        is_comment: false,
        is_continuation: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_detection_fixed() {
        let input = r#"      * This is a comment
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID     PIC X(10).
           05  CUSTOMER-NAME   PIC X(30).
"#;
        assert_eq!(detect_format(input), CobolFormat::Fixed);
    }

    #[test]
    fn test_format_detection_free() {
        let input = r#"*> This is a comment
01 CUSTOMER-RECORD.
  05 CUSTOMER-ID PIC X(10).
  05 CUSTOMER-NAME PIC X(30).
"#;
        assert_eq!(detect_format(input), CobolFormat::Free);
    }

    #[test]
    fn test_basic_tokenization() {
        let input = "01 CUSTOMER-ID PIC X(10).";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].token, Token::Level(1));
        assert_eq!(
            tokens[1].token,
            Token::Identifier("CUSTOMER-ID".to_string())
        );
        assert_eq!(tokens[2].token, Token::Pic);
        assert_eq!(tokens[3].token, Token::PicClause("X(10)".to_string()));
        assert_eq!(tokens[4].token, Token::Period);
    }

    #[test]
    fn test_continuation_handling() {
        let input = r#"       01  VERY-LONG-FIELD-NAME
      -        PIC X(50).
"#;
        let lexer = Lexer::new(input);
        let processed = lexer.build_processed_text();

        // Should join the continuation properly
        assert!(processed.contains("VERY-LONG-FIELD-NAME PIC X(50)"));
    }

    #[test]
    fn test_edited_pic_detection() {
        let input = "01 AMOUNT PIC ZZ,ZZZ.99.";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        // Should detect edited PIC
        let pic_token = tokens
            .iter()
            .find(|t| matches!(t.token, Token::EditedPic(_)));
        assert!(pic_token.is_some());
    }
}
