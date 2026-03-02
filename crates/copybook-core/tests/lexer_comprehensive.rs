// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Comprehensive lexer tests for copybook-core.
//!
//! Validates all token types, whitespace handling, comment detection,
//! continuation lines, and edge cases.

use copybook_core::ParseOptions;
use copybook_core::lexer::{CobolFormat, Lexer, Token};

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

fn tokens(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    lexer
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect()
}

fn tokens_with_opts(input: &str, opts: &ParseOptions) -> Vec<Token> {
    let mut lexer = Lexer::new_with_options(input, opts);
    lexer
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect()
}

// ===========================================================================
// 1. Level tokens
// ===========================================================================

#[test]
fn test_level_01() {
    let toks = tokens("01");
    assert!(toks.contains(&Token::Level(1)));
}

#[test]
fn test_level_05() {
    let toks = tokens("05");
    assert!(toks.contains(&Token::Level(5)));
}

#[test]
fn test_level_49() {
    let toks = tokens("49");
    assert!(toks.contains(&Token::Level(49)));
}

#[test]
fn test_level_66() {
    let toks = tokens("66");
    assert!(toks.contains(&Token::Level66));
}

#[test]
fn test_level_77() {
    let toks = tokens("77");
    assert!(toks.contains(&Token::Level77));
}

#[test]
fn test_level_88() {
    let toks = tokens("88");
    assert!(toks.contains(&Token::Level88));
}

// ===========================================================================
// 2. Keyword tokens
// ===========================================================================

#[test]
fn test_pic_keyword() {
    let toks = tokens("PIC");
    assert!(toks.contains(&Token::Pic));
}

#[test]
fn test_picture_keyword_alias() {
    let toks = tokens("PICTURE");
    assert!(toks.contains(&Token::Pic), "PICTURE should map to Pic");
}

#[test]
fn test_occurs_keyword() {
    let toks = tokens("OCCURS");
    assert!(toks.contains(&Token::Occurs));
}

#[test]
fn test_redefines_keyword() {
    let toks = tokens("REDEFINES");
    assert!(toks.contains(&Token::Redefines));
}

#[test]
fn test_renames_keyword() {
    let toks = tokens("RENAMES");
    assert!(toks.contains(&Token::Renames));
}

#[test]
fn test_depending_on_keywords() {
    let toks = tokens("DEPENDING ON");
    assert!(toks.contains(&Token::Depending));
    assert!(toks.contains(&Token::On));
}

#[test]
fn test_to_keyword() {
    let toks = tokens("TO");
    assert!(toks.contains(&Token::To));
}

#[test]
fn test_times_keyword() {
    let toks = tokens("TIMES");
    assert!(toks.contains(&Token::Times));
}

#[test]
fn test_value_keyword() {
    let toks = tokens("VALUE");
    assert!(toks.contains(&Token::Value));
}

#[test]
fn test_comp_keyword() {
    let toks = tokens("COMP");
    assert!(toks.contains(&Token::Comp));
}

#[test]
fn test_comp3_keyword() {
    let toks = tokens("COMP-3");
    assert!(toks.contains(&Token::Comp3));
}

#[test]
fn test_comp1_keyword() {
    let toks = tokens("COMP-1");
    assert!(toks.contains(&Token::Comp1));
}

#[test]
fn test_comp2_keyword() {
    let toks = tokens("COMP-2");
    assert!(toks.contains(&Token::Comp2));
}

#[test]
fn test_binary_keyword() {
    let toks = tokens("BINARY");
    assert!(toks.contains(&Token::Binary));
}

#[test]
fn test_sign_leading_trailing_separate_keywords() {
    let toks = tokens("SIGN LEADING TRAILING SEPARATE");
    assert!(toks.contains(&Token::Sign));
    assert!(toks.contains(&Token::Leading));
    assert!(toks.contains(&Token::Trailing));
    assert!(toks.contains(&Token::Separate));
}

#[test]
fn test_blank_when_zero_keywords() {
    let toks = tokens("BLANK WHEN ZERO");
    assert!(toks.contains(&Token::Blank));
    assert!(toks.contains(&Token::When));
    assert!(toks.contains(&Token::Zero));
}

#[test]
fn test_thru_through_keywords() {
    let toks = tokens("THRU THROUGH");
    assert!(toks.contains(&Token::Thru));
    assert!(toks.contains(&Token::Through));
}

#[test]
fn test_synchronized_keyword() {
    let toks = tokens("SYNCHRONIZED");
    assert!(toks.contains(&Token::Synchronized));
}

#[test]
fn test_sync_keyword_alias() {
    let toks = tokens("SYNC");
    assert!(toks.contains(&Token::Synchronized));
}

#[test]
fn test_usage_display_keywords() {
    let toks = tokens("USAGE DISPLAY");
    assert!(toks.contains(&Token::Usage));
    assert!(toks.contains(&Token::Display));
}

#[test]
fn test_is_keyword() {
    let toks = tokens("IS");
    assert!(toks.contains(&Token::Is));
}

// ===========================================================================
// 3. Case insensitivity
// ===========================================================================

#[test]
fn test_keywords_case_insensitive() {
    let toks = tokens("pic Pic PIC");
    let pic_count = toks.iter().filter(|t| matches!(t, Token::Pic)).count();
    assert_eq!(pic_count, 3, "All case variants should lex as Pic");
}

#[test]
fn test_comp3_case_insensitive() {
    let toks = tokens("comp-3 Comp-3 COMP-3");
    let count = toks.iter().filter(|t| matches!(t, Token::Comp3)).count();
    assert_eq!(count, 3);
}

#[test]
fn test_computational_full_word() {
    let toks = tokens("COMPUTATIONAL");
    assert!(toks.contains(&Token::Comp));
}

#[test]
fn test_computational_3_full_word() {
    let toks = tokens("COMPUTATIONAL-3");
    assert!(toks.contains(&Token::Comp3));
}

// ===========================================================================
// 4. PIC clause patterns
// ===========================================================================

#[test]
fn test_pic_clause_x_pattern() {
    let toks = tokens("PIC X(10)");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::PicClause(s) if s.contains('X')))
    );
}

#[test]
fn test_pic_clause_9_with_v() {
    let toks = tokens("PIC S9(5)V9(2)");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::PicClause(s) if s.contains('V')))
    );
}

// ===========================================================================
// 5. Punctuation and literals
// ===========================================================================

#[test]
fn test_period_token() {
    let toks = tokens(".");
    assert!(toks.contains(&Token::Period));
}

#[test]
fn test_parentheses_tokens() {
    let toks = tokens("( )");
    assert!(toks.contains(&Token::LeftParen));
    assert!(toks.contains(&Token::RightParen));
}

#[test]
fn test_number_literal() {
    let toks = tokens("42");
    assert!(toks.contains(&Token::Number(42)));
}

#[test]
fn test_string_literal_single_quotes() {
    let toks = tokens("'HELLO'");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::StringLiteral(s) if s == "HELLO"))
    );
}

#[test]
fn test_string_literal_double_quotes() {
    let toks = tokens(r#""WORLD""#);
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::StringLiteral(s) if s == "WORLD"))
    );
}

#[test]
fn test_identifier_token() {
    let toks = tokens("MY-FIELD-NAME");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::Identifier(s) if s == "MY-FIELD-NAME"))
    );
}

// ===========================================================================
// 6. Comment detection
// ===========================================================================

#[test]
fn test_fixed_form_comment_line_col7_asterisk() {
    // Column 7 asterisk in fixed-form should be treated as a comment
    let input = "      * This is a comment\n       01 REC PIC X.\n";
    let mut lexer = Lexer::new(input);
    let toks: Vec<Token> = lexer.tokenize().into_iter().map(|tp| tp.token).collect();
    // The comment line should be skipped; we should still see the 01 level
    assert!(
        toks.iter().any(|t| matches!(t, Token::Level(1))),
        "Should parse past comment line"
    );
}

#[test]
fn test_inline_comment_stripped_by_default() {
    let input = "01 FIELD PIC X(4). *> inline comment\n";
    let opts = ParseOptions {
        allow_inline_comments: true,
        strict_comments: false,
        ..ParseOptions::default()
    };
    let toks = tokens_with_opts(input, &opts);
    let comment_count = toks
        .iter()
        .filter(|t| matches!(t, Token::InlineComment(_)))
        .count();
    assert_eq!(comment_count, 0, "Non-strict mode strips inline comments");
}

#[test]
fn test_inline_comment_preserved_in_strict_mode() {
    let input = "01 FIELD PIC X(4). *> inline comment\n";
    let opts = ParseOptions {
        allow_inline_comments: true,
        strict_comments: true,
        ..ParseOptions::default()
    };
    let toks = tokens_with_opts(input, &opts);
    let comment_count = toks
        .iter()
        .filter(|t| matches!(t, Token::InlineComment(_)))
        .count();
    assert!(
        comment_count > 0,
        "Strict mode should preserve inline comments"
    );
}

// ===========================================================================
// 7. Format detection
// ===========================================================================

#[test]
fn test_free_form_detection() {
    let mut lexer = Lexer::new("01 FOO PIC X.\n");
    assert_eq!(lexer.format(), CobolFormat::Free);
    let _ = lexer.tokenize();
}

#[test]
fn test_fixed_form_detection() {
    let input = "       01 REC.\n           05 F1 PIC X(10).\n";
    let mut lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
    let _ = lexer.tokenize();
}

// ===========================================================================
// 8. Whitespace handling
// ===========================================================================

#[test]
fn test_multiple_spaces_between_tokens() {
    let toks = tokens("01     FIELD     PIC     X(10).");
    assert!(toks.iter().any(|t| matches!(t, Token::Level(1))));
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::Identifier(s) if s == "FIELD"))
    );
    assert!(toks.contains(&Token::Pic));
}

#[test]
fn test_tabs_in_input() {
    let toks = tokens("01\tFIELD\tPIC\tX(10).");
    assert!(toks.iter().any(|t| matches!(t, Token::Level(1))));
    assert!(toks.contains(&Token::Pic));
}

// ===========================================================================
// 9. Continuation lines (fixed-form)
// ===========================================================================

#[test]
fn test_continuation_line_in_fixed_form() {
    // Column 7 dash indicates continuation
    let input = "       01 FIXED-RECORD.\n      -    PIC X(10).\n";
    let mut lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
    let toks = lexer.tokenize();
    assert!(
        toks.iter().any(|t| t.token == Token::Pic),
        "Continuation line content should be tokenized"
    );
}

// ===========================================================================
// 10. Edge cases
// ===========================================================================

#[test]
fn test_empty_input() {
    let mut lexer = Lexer::new("");
    let toks = lexer.tokenize();
    assert!(
        toks.iter().any(|t| t.token == Token::Eof),
        "Empty input should still produce EOF"
    );
}

#[test]
fn test_whitespace_only_input() {
    let mut lexer = Lexer::new("   \n\n   \n");
    let toks = lexer.tokenize();
    let non_ws: Vec<_> = toks
        .iter()
        .filter(|t| !matches!(t.token, Token::Newline | Token::Eof))
        .collect();
    assert!(
        non_ws.is_empty(),
        "Whitespace-only should produce no content tokens"
    );
}

#[test]
fn test_single_period() {
    let toks = tokens(".");
    assert_eq!(toks, vec![Token::Period]);
}

#[test]
fn test_eof_always_present() {
    let mut lexer = Lexer::new("01 FOO.");
    let toks = lexer.tokenize();
    assert_eq!(
        toks.last().unwrap().token,
        Token::Eof,
        "Last token must be EOF"
    );
}

#[test]
fn test_zeros_zeroes_zero_all_map_to_zero_token() {
    let toks = tokens("ZERO ZEROS ZEROES");
    let count = toks.iter().filter(|t| matches!(t, Token::Zero)).count();
    assert_eq!(count, 3, "All three spellings should lex as Zero");
}

#[test]
fn test_large_number_literal() {
    let toks = tokens("99999");
    assert!(toks.contains(&Token::Number(99999)));
}

#[test]
fn test_multiple_statements_single_line() {
    let toks = tokens("01 A. 05 B PIC X.");
    let levels: Vec<_> = toks
        .iter()
        .filter(|t| matches!(t, Token::Level(_)))
        .collect();
    assert_eq!(levels.len(), 2, "Should find two level tokens");
}

#[test]
fn test_line_position_tracking() {
    let input = "01 FOO.\n05 BAR PIC X.\n";
    let mut lexer = Lexer::new(input);
    let toks = lexer.tokenize();
    let first_level = toks
        .iter()
        .find(|t| matches!(t.token, Token::Level(1)))
        .unwrap();
    let second_level = toks
        .iter()
        .find(|t| matches!(t.token, Token::Level(5)))
        .unwrap();
    assert!(
        first_level.line < second_level.line || first_level.line == second_level.line,
        "Position should be tracked"
    );
}

#[test]
fn test_comma_token() {
    let toks = tokens(",");
    assert!(toks.contains(&Token::Comma));
}
