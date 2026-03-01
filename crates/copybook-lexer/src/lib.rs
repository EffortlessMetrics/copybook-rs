#![allow(clippy::must_use_candidate)]

//! COBOL lexer microcrate.
//!
//! Provides tokenization for COBOL copybooks with fixed/free form preprocessing,
//! continuation handling, and keyword/picture token recognition.

use std::fmt;

use logos::Logos;

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

    #[token("COMP-1", ignore(case))]
    #[token("COMPUTATIONAL-1", ignore(case))]
    Comp1,

    #[token("COMP-2", ignore(case))]
    #[token("COMPUTATIONAL-2", ignore(case))]
    Comp2,

    #[token("BINARY", ignore(case))]
    Binary,

    #[token("REDEFINES", ignore(case))]
    Redefines,

    #[token("RENAMES", ignore(case))]
    Renames,

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

    #[token("THRU", ignore(case))]
    Thru,

    #[token("THROUGH", ignore(case))]
    Through,

    #[token("SIGN", ignore(case))]
    Sign,

    #[token("LEADING", ignore(case))]
    Leading,

    #[token("IS", ignore(case))]
    Is,

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
    // Zero insertion patterns (e.g., "0009") - higher priority than level numbers
    #[regex(r"0{2,}[0-9]+", priority = 5, callback = |lex| lex.slice().to_string())]
    #[regex(r"[0Z9]+", priority = 3, callback = |lex| lex.slice().to_string())]
    #[regex(r"[Z9]*[/,\$\+\-\*]+[Z9]*", priority = 3, callback = |lex| lex.slice().to_string())]
    EditedPic(String),

    // Numbers - higher priority than PIC clauses but lower than special levels
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

    // Comma must have higher priority than EditedPic so 88 VALUE lists with commas parse (see Issue #86).
    #[token(",", priority = 4)]
    Comma,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    // Comments – priority must exceed EditedPic (3/5) so `*>` is never
    // consumed as an asterisk-fill pattern before the comment is recognised.
    #[regex(
        r"\*>[^\r\n]*",
        priority = 6,
        callback = |lex| lex.slice()[2..].trim().to_string(),
        allow_greedy = true
    )]
    InlineComment(String),

    // Newlines (important for line tracking)
    #[token("\n")]
    #[token("\r\n")]
    Newline,

    // End of input
    Eof,
}

impl fmt::Display for Token {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Level(n) => write!(f, "{n:02}"),
            Token::Level66 => write!(f, "66"),
            Token::Level77 => write!(f, "77"),
            Token::Level88 => write!(f, "88"),
            Token::Pic => write!(f, "PIC"),
            Token::Usage => write!(f, "USAGE"),
            Token::Display => write!(f, "DISPLAY"),
            Token::Comp => write!(f, "COMP"),
            Token::Comp3 => write!(f, "COMP-3"),
            Token::Comp1 => write!(f, "COMP-1"),
            Token::Comp2 => write!(f, "COMP-2"),
            Token::Binary => write!(f, "BINARY"),
            Token::Redefines => write!(f, "REDEFINES"),
            Token::Renames => write!(f, "RENAMES"),
            Token::Occurs => write!(f, "OCCURS"),
            Token::Depending => write!(f, "DEPENDING"),
            Token::On => write!(f, "ON"),
            Token::To => write!(f, "TO"),
            Token::Times => write!(f, "TIMES"),
            Token::Synchronized => write!(f, "SYNCHRONIZED"),
            Token::Value => write!(f, "VALUE"),
            Token::Thru => write!(f, "THRU"),
            Token::Through => write!(f, "THROUGH"),
            Token::Sign => write!(f, "SIGN"),
            Token::Is => write!(f, "IS"),
            Token::Leading => write!(f, "LEADING"),
            Token::Trailing => write!(f, "TRAILING"),
            Token::Separate => write!(f, "SEPARATE"),
            Token::Blank => write!(f, "BLANK"),
            Token::When => write!(f, "WHEN"),
            Token::Zero => write!(f, "ZERO"),
            Token::PicClause(s) | Token::EditedPic(s) | Token::Identifier(s) => write!(f, "{s}"),
            Token::Number(n) => write!(f, "{n}"),
            Token::StringLiteral(s) => write!(f, "\"{s}\""),
            Token::Period => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::InlineComment(s) => write!(f, "*> {s}"),
            Token::Newline => write!(f, "\\n"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

/// Position information for tokens
#[derive(Debug, Clone, PartialEq)]
pub struct TokenPos {
    /// The token value.
    pub token: Token,
    /// 1-based line number where the token starts.
    pub line: usize,
    /// 1-based column number where the token starts.
    pub column: usize,
    /// Byte range within the source text.
    pub span: std::ops::Range<usize>,
}

/// COBOL format detection
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CobolFormat {
    /// Traditional fixed-format (columns 1–6 sequence, 7 indicator, 8–72 code).
    Fixed,
    /// Free-format source (no column restrictions).
    Free,
}

/// Configuration options for the COBOL lexer.
#[derive(Debug, Clone, Copy)]
pub struct LexerOptions {
    /// When `true`, COBOL-2002 inline comments (`*>`) are recognised.
    pub allow_inline_comments: bool,
    /// When `true`, only column-7 indicators are treated as comment markers.
    pub strict_comments: bool,
}

impl Default for LexerOptions {
    #[inline]
    fn default() -> Self {
        Self {
            allow_inline_comments: true,
            strict_comments: false,
        }
    }
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
    #[inline]
    pub fn new(input: &'a str) -> Self {
        Self::new_with_options(input, LexerOptions::default())
    }

    /// Create a new lexer for the given input with specific options
    #[inline]
    pub fn new_with_options(input: &'a str, options: LexerOptions) -> Self {
        let format = detect_format(input);
        let lines = preprocess_lines(input, format, options);

        Self {
            _input: input,
            format,
            lines,
            _current_line: 0,
            _current_pos: 0,
        }
    }

    /// Get the detected format
    #[inline]
    pub fn format(&self) -> CobolFormat {
        self.format
    }

    /// Tokenize the input and return all tokens with positions
    #[inline]
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
                let text = &processed_text[span.clone()];
                Token::Identifier(text.to_string())
            };
            let start_column = column;

            if token == Token::Newline {
                line += 1;
                column = 1;
            } else {
                column += span.len();
            }

            tokens.push(TokenPos {
                token,
                line,
                column: start_column,
                span,
            });
        }

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
                if result.ends_with('\n') {
                    result.pop();
                }

                let mut trimmed_result = result.trim_end().to_string();
                let continuation_content = line.content.trim();

                if !trimmed_result.is_empty() && !continuation_content.is_empty() {
                    if trimmed_result.ends_with('-') {
                        if let Some(stripped) = continuation_content.strip_prefix('-') {
                            trimmed_result.push_str(stripped);
                        } else {
                            trimmed_result.push_str(continuation_content);
                        }
                    } else {
                        trimmed_result.push(' ');
                        trimmed_result.push_str(continuation_content);
                    }
                } else if !continuation_content.is_empty() {
                    trimmed_result.push_str(continuation_content);
                }

                result = trimmed_result;
                result.push('\n');
            } else {
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
            continue;
        }

        total_content_lines += 1;

        if line.len() >= 8 {
            let first_six = &line[0..6];
            let col_7 = line.chars().nth(6).unwrap_or(' ');
            let col_8_onwards = &line[7..];

            if (first_six.chars().all(|c| c.is_ascii_digit() || c == ' '))
                && (col_7 == ' ' || col_7 == '*' || col_7 == '-' || col_7 == '/')
                && !col_8_onwards.trim().is_empty()
            {
                fixed_form_indicators += 1;
            }
        }

        if line.len() == 72 || line.len() == 80 {
            fixed_form_indicators += 1;
        }
    }

    if total_content_lines > 0 && (fixed_form_indicators * 100 / total_content_lines) >= 50 {
        CobolFormat::Fixed
    } else {
        CobolFormat::Free
    }
}

/// Preprocess lines according to the detected format
fn preprocess_lines(
    input: &str,
    format: CobolFormat,
    options: LexerOptions,
) -> Vec<ProcessedLine<'_>> {
    let mut result = Vec::new();

    for (line_num, line) in input.lines().enumerate() {
        let processed = match format {
            CobolFormat::Fixed => process_fixed_form_line(line, line_num + 1),
            CobolFormat::Free => process_free_form_line(line, line_num + 1, options),
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

    if line.starts_with('*') {
        return ProcessedLine {
            content: line,
            _original_line: line_num,
            is_comment: true,
            is_continuation: false,
        };
    }

    let is_continuation = line.len() > 6 && line.chars().nth(6) == Some('-');
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
fn process_free_form_line(line: &str, line_num: usize, options: LexerOptions) -> ProcessedLine<'_> {
    let trimmed = line.trim_start();

    if trimmed.starts_with('*') {
        return ProcessedLine {
            content: line,
            _original_line: line_num,
            is_comment: true,
            is_continuation: false,
        };
    }

    let content = if options.allow_inline_comments && !options.strict_comments {
        if let Some(comment_pos) = line.find("*>") {
            line[..comment_pos].trim_end()
        } else {
            line
        }
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
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_format_detection_fixed() {
        let input = r"      * This is a comment
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID     PIC X(10).
           05  CUSTOMER-NAME   PIC X(30).
";
        assert_eq!(detect_format(input), CobolFormat::Fixed);
    }

    #[test]
    fn test_format_detection_free() {
        let input = r"*> This is a comment
01 CUSTOMER-RECORD.
  05 CUSTOMER-ID PIC X(10).
  05 CUSTOMER-NAME PIC X(30).
";
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
        let input = r"       01  VERY-LONG-FIELD-NAME
      -        PIC X(50).";
        let lexer = Lexer::new(input);
        let processed = lexer.build_processed_text();

        assert!(processed.contains("VERY-LONG-FIELD-NAME PIC X(50)"));
    }

    #[test]
    fn test_edited_pic_detection() {
        let input = "01 AMOUNT PIC ZZ,ZZZ.99.";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        let pic_token = tokens
            .iter()
            .find(|t| matches!(t.token, Token::EditedPic(_)));
        assert!(pic_token.is_some());
    }

    #[test]
    fn test_comma_tokenization_priority() {
        let input = ",";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].token, Token::Comma);
    }

    #[test]
    fn test_comma_in_level88_value_clause() {
        let input = r#"88 IS-VALID VALUE "A", "B", "C"."#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        let comma_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(t.token, Token::Comma))
            .collect();
        assert_eq!(comma_tokens.len(), 2);

        let edited_pic_commas: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(&t.token, Token::EditedPic(s) if s == ","))
            .collect();
        assert_eq!(edited_pic_commas.len(), 0);
    }

    #[test]
    fn test_edited_pic_still_detected_after_comma_fix() {
        let input = "PIC Z,ZZZ.99";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        let edited_pic = tokens
            .iter()
            .find(|t| matches!(t.token, Token::EditedPic(_)));
        assert!(edited_pic.is_some());

        if let Some(token_pos) = edited_pic
            && let Token::EditedPic(pattern) = &token_pos.token
        {
            assert!(pattern.contains(','));
        }
    }

    #[test]
    fn test_comma_vs_edited_pic_disambiguation() {
        let mut lexer1 = Lexer::new(",");
        let tokens1 = lexer1.tokenize();
        assert!(matches!(tokens1[0].token, Token::Comma));

        let mut lexer2 = Lexer::new("Z,ZZZ");
        let tokens2 = lexer2.tokenize();
        assert!(matches!(tokens2[0].token, Token::EditedPic(_)));

        // Comma inside a string literal is NOT tokenized as Comma
        let mut lexer3 = Lexer::new(r#""A,B""#);
        let tokens3 = lexer3.tokenize();
        assert!(!tokens3.iter().any(|t| matches!(t.token, Token::Comma)));
    }

    #[test]
    fn test_commas_with_spaces_realistic_cobol() {
        let input = r#"VALUE "A", "B", "C""#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        let comma_count = tokens
            .iter()
            .filter(|t| matches!(t.token, Token::Comma))
            .count();
        assert_eq!(comma_count, 2);
    }

    #[test]
    fn test_comma_inside_string_literal_not_tokenized() {
        let mut lx = Lexer::new(r#""A,B""#);
        let toks = lx.tokenize();

        assert!(!toks.iter().any(|t| matches!(t.token, Token::Comma)));
        let string_tokens: Vec<_> = toks
            .iter()
            .filter(|t| matches!(&t.token, Token::StringLiteral(s) if s == "A,B"))
            .collect();
        assert_eq!(string_tokens.len(), 1);
    }

    // ── Additional tests ─────────────────────────────────────────────

    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        let tokens = lexer.tokenize();
        assert_eq!(tokens.last().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_whitespace_only_input() {
        let mut lexer = Lexer::new("   \t  ");
        let tokens = lexer.tokenize();
        // Should get newline and Eof
        assert!(tokens.iter().any(|t| t.token == Token::Eof));
    }

    #[test]
    fn test_level_numbers_01_to_49() {
        for level in 1..=49u8 {
            let input = format!("{level:02} FIELD PIC X.");
            let mut lexer = Lexer::new(&input);
            let tokens = lexer.tokenize();
            assert_eq!(tokens[0].token, Token::Level(level), "level {level:02}");
        }
    }

    #[test]
    fn test_level_66() {
        let input = "66 ALIAS-FIELD RENAMES ORIG-FIELD.";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_eq!(tokens[0].token, Token::Level66);
        assert_eq!(tokens[2].token, Token::Renames);
    }

    #[test]
    fn test_level_77() {
        let input = "77 STANDALONE-FIELD PIC 9(5).";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_eq!(tokens[0].token, Token::Level77);
    }

    #[test]
    fn test_level_88() {
        let input = r#"88 IS-TRUE VALUE "Y"."#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_eq!(tokens[0].token, Token::Level88);
        assert_eq!(tokens[2].token, Token::Value);
    }

    #[test]
    fn test_pic_keyword_case_insensitive() {
        for kw in &["PIC", "pic", "Pic", "PICTURE", "picture"] {
            let input = format!("{kw} X(5)");
            let mut lexer = Lexer::new(&input);
            let tokens = lexer.tokenize();
            assert!(
                tokens.iter().any(|t| t.token == Token::Pic),
                "failed for keyword: {kw}"
            );
        }
    }

    #[test]
    fn test_comp_variants() {
        let mut lx = Lexer::new("COMP");
        assert!(lx.tokenize().iter().any(|t| t.token == Token::Comp));

        let mut lx = Lexer::new("COMP-1");
        assert!(lx.tokenize().iter().any(|t| t.token == Token::Comp1));

        let mut lx = Lexer::new("COMP-2");
        assert!(lx.tokenize().iter().any(|t| t.token == Token::Comp2));

        let mut lx = Lexer::new("COMP-3");
        assert!(lx.tokenize().iter().any(|t| t.token == Token::Comp3));
    }

    #[test]
    fn test_computational_variants() {
        let mut lx = Lexer::new("COMPUTATIONAL");
        assert!(lx.tokenize().iter().any(|t| t.token == Token::Comp));

        let mut lx = Lexer::new("COMPUTATIONAL-3");
        assert!(lx.tokenize().iter().any(|t| t.token == Token::Comp3));
    }

    #[test]
    fn test_keyword_tokens() {
        let cases = vec![
            ("USAGE", Token::Usage),
            ("DISPLAY", Token::Display),
            ("BINARY", Token::Binary),
            ("REDEFINES", Token::Redefines),
            ("OCCURS", Token::Occurs),
            ("DEPENDING", Token::Depending),
            ("ON", Token::On),
            ("TO", Token::To),
            ("TIMES", Token::Times),
            ("SYNCHRONIZED", Token::Synchronized),
            ("SYNC", Token::Synchronized),
            ("VALUE", Token::Value),
            ("THRU", Token::Thru),
            ("THROUGH", Token::Through),
            ("SIGN", Token::Sign),
            ("LEADING", Token::Leading),
            ("IS", Token::Is),
            ("TRAILING", Token::Trailing),
            ("SEPARATE", Token::Separate),
            ("BLANK", Token::Blank),
            ("WHEN", Token::When),
            ("ZERO", Token::Zero),
            ("ZEROS", Token::Zero),
            ("ZEROES", Token::Zero),
        ];

        for (input, expected) in cases {
            let mut lx = Lexer::new(input);
            let tokens = lx.tokenize();
            assert!(
                tokens.iter().any(|t| t.token == expected),
                "keyword {input} not matched"
            );
        }
    }

    #[test]
    fn test_string_literal_double_quotes() {
        let mut lx = Lexer::new(r#""HELLO WORLD""#);
        let tokens = lx.tokenize();
        assert_eq!(
            tokens[0].token,
            Token::StringLiteral("HELLO WORLD".to_string())
        );
    }

    #[test]
    fn test_string_literal_single_quotes() {
        let mut lx = Lexer::new("'HELLO WORLD'");
        let tokens = lx.tokenize();
        assert_eq!(
            tokens[0].token,
            Token::StringLiteral("HELLO WORLD".to_string())
        );
    }

    #[test]
    fn test_number_token() {
        let mut lx = Lexer::new("OCCURS 100 TIMES");
        let tokens = lx.tokenize();
        assert_eq!(tokens[1].token, Token::Number(100));
        assert_eq!(tokens[2].token, Token::Times);
    }

    #[test]
    fn test_parentheses() {
        let mut lx = Lexer::new("(50)");
        let tokens = lx.tokenize();
        assert_eq!(tokens[0].token, Token::LeftParen);
        assert_eq!(tokens[1].token, Token::Number(50));
        assert_eq!(tokens[2].token, Token::RightParen);
    }

    #[test]
    fn test_period_token() {
        let mut lx = Lexer::new("FIELD-NAME.");
        let tokens = lx.tokenize();
        let last_non_eof = tokens
            .iter()
            .rev()
            .find(|t| t.token != Token::Eof && t.token != Token::Newline)
            .unwrap();
        assert_eq!(last_non_eof.token, Token::Period);
    }

    #[test]
    fn test_pic_clause_patterns() {
        let patterns = vec![
            ("X(10)", "X(10)"),
            ("9(5)", "9(5)"),
            ("S9(5)V9(2)", "S9(5)V9(2)"),
            ("XXX", "XXX"),
            ("S999V99", "S999V99"),
        ];

        for (input, expected) in patterns {
            let full = format!("PIC {input}");
            let mut lx = Lexer::new(&full);
            let tokens = lx.tokenize();
            let pic_clause = tokens
                .iter()
                .find(|t| matches!(&t.token, Token::PicClause(_)));
            assert!(pic_clause.is_some(), "no PicClause for pattern: {input}");
            if let Some(tp) = pic_clause {
                assert_eq!(tp.token, Token::PicClause(expected.to_string()));
            }
        }
    }

    #[test]
    fn test_identifier_with_hyphens() {
        let mut lx = Lexer::new("CUSTOMER-RECORD-ID");
        let tokens = lx.tokenize();
        assert_eq!(
            tokens[0].token,
            Token::Identifier("CUSTOMER-RECORD-ID".to_string())
        );
    }

    #[test]
    fn test_inline_comment_in_free_form() {
        // Inline comments are stripped during preprocessing in free form.
        // Verify that content before the comment is preserved.
        let input = "01 FIELD PIC X. *> this is a comment";
        let mut lx = Lexer::new(input);
        let tokens = lx.tokenize();
        // The comment should be stripped; the field definition should parse
        assert!(tokens.iter().any(|t| t.token == Token::Level(1)));
        assert!(tokens.iter().any(|t| t.token == Token::Period));
    }

    #[test]
    fn test_token_display_trait() {
        assert_eq!(format!("{}", Token::Level(5)), "05");
        assert_eq!(format!("{}", Token::Level66), "66");
        assert_eq!(format!("{}", Token::Level77), "77");
        assert_eq!(format!("{}", Token::Level88), "88");
        assert_eq!(format!("{}", Token::Pic), "PIC");
        assert_eq!(format!("{}", Token::Comp3), "COMP-3");
        assert_eq!(format!("{}", Token::Period), ".");
        assert_eq!(format!("{}", Token::Comma), ",");
        assert_eq!(format!("{}", Token::LeftParen), "(");
        assert_eq!(format!("{}", Token::RightParen), ")");
        assert_eq!(format!("{}", Token::Eof), "EOF");
        assert_eq!(format!("{}", Token::Newline), "\\n");
        assert_eq!(format!("{}", Token::Number(42)), "42");
        assert_eq!(
            format!("{}", Token::StringLiteral("test".to_string())),
            "\"test\""
        );
        assert_eq!(
            format!("{}", Token::InlineComment("comment".to_string())),
            "*> comment"
        );
    }

    #[test]
    fn test_lexer_options_default() {
        let opts = LexerOptions::default();
        assert!(opts.allow_inline_comments);
        assert!(!opts.strict_comments);
    }

    #[test]
    fn test_lexer_format_accessor() {
        let lexer = Lexer::new("01 FIELD PIC X.");
        // Free form since it's a short line
        assert_eq!(lexer.format(), CobolFormat::Free);
    }

    #[test]
    fn test_cobol_format_eq() {
        assert_eq!(CobolFormat::Fixed, CobolFormat::Fixed);
        assert_eq!(CobolFormat::Free, CobolFormat::Free);
        assert_ne!(CobolFormat::Fixed, CobolFormat::Free);
    }

    #[test]
    fn test_token_last_is_always_eof() {
        for input in &["01 X PIC X.", "", "OCCURS 5 TIMES.", "  "] {
            let mut lx = Lexer::new(input);
            let tokens = lx.tokenize();
            assert_eq!(tokens.last().unwrap().token, Token::Eof);
        }
    }

    #[test]
    fn test_tokenpos_has_position_info() {
        let mut lx = Lexer::new("01 FIELD PIC X.");
        let tokens = lx.tokenize();
        let first = &tokens[0];
        assert_eq!(first.line, 1);
        assert!(first.column >= 1);
        assert!(!first.span.is_empty());
    }

    #[test]
    fn test_occurs_depending_on_clause() {
        let input = "OCCURS 1 TO 10 DEPENDING ON COUNTER";
        let mut lx = Lexer::new(input);
        let tokens = lx.tokenize();
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token).collect();
        assert!(token_types.contains(&&Token::Occurs));
        assert!(token_types.contains(&&Token::To));
        assert!(token_types.contains(&&Token::Depending));
        assert!(token_types.contains(&&Token::On));
    }

    #[test]
    fn test_value_thru_clause() {
        let input = "VALUE 1 THRU 100";
        let mut lx = Lexer::new(input);
        let tokens = lx.tokenize();
        assert!(tokens.iter().any(|t| t.token == Token::Value));
        assert!(tokens.iter().any(|t| t.token == Token::Thru));
    }
}
