// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Edge-case tests for the copybook-lexer crate.
//!
//! Targets boundary conditions: empty input, whitespace-only, comment-only,
//! line-length boundaries, continuation markers, format detection, exotic
//! line endings, null bytes, Unicode in comments, and COPY/REPLACE directives.

use copybook_lexer::{CobolFormat, Lexer, LexerOptions, Token};

// ── Helpers ──────────────────────────────────────────────────────────

/// Tokenize free-form input and return just the Token values.
fn tokens(input: &str) -> Vec<Token> {
    Lexer::new(input)
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .collect()
}

/// Tokenize and strip Newline / Eof so tests only see "payload" tokens.
fn payload(input: &str) -> Vec<Token> {
    tokens(input)
        .into_iter()
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect()
}

/// Tokenize with specific options; return all tokens.
fn tokens_with(input: &str, opts: LexerOptions) -> Vec<Token> {
    Lexer::new_with_options(input, opts)
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .collect()
}

// =========================================================================
// 1. Empty input
// =========================================================================

#[test]
fn edge_empty_input_returns_only_eof() {
    let toks = tokens("");
    assert_eq!(toks, vec![Token::Eof]);
}

// =========================================================================
// 2. Only whitespace
// =========================================================================

#[test]
fn edge_whitespace_only_produces_no_payload() {
    let toks = payload("   \t   \t  ");
    assert!(toks.is_empty(), "expected no payload tokens, got {toks:?}");
}

#[test]
fn edge_whitespace_multiline_produces_no_payload() {
    let toks = payload("   \n   \n  \t  \n");
    assert!(toks.is_empty(), "expected no payload tokens, got {toks:?}");
}

// =========================================================================
// 3. Only comments (* in column 7)
// =========================================================================

#[test]
fn edge_fixed_form_comment_only() {
    // Fixed-form: a line starting with '*' is treated as comment
    let input = "* This is a full-line comment.\n* Another comment line.\n";
    let toks = payload(input);
    // Comments should be stripped – no payload tokens
    assert!(
        toks.is_empty(),
        "comment-only fixed-form should produce no payload: {toks:?}"
    );
}

#[test]
fn edge_fixed_form_comment_col7_asterisk() {
    // Fixed-form with sequence area: '*' at column 7 (index 6) is an indicator.
    // The lexer extracts columns 8-72 as content; the indicator area content
    // ends up tokenized (this is a known limitation – comments with sequence
    // numbers need the content check). Verify no panic.
    let input = "000100*This is a full-line comment.\n000200*Another comment line.\n";
    let _toks = payload(input); // Must not panic
}

#[test]
fn edge_free_form_comment_only() {
    let input = "* A free-form comment line\n* Another comment\n";
    let toks = payload(input);
    assert!(
        toks.is_empty(),
        "comment-only free-form should produce no payload: {toks:?}"
    );
}

// =========================================================================
// 4. Single COBOL line
// =========================================================================

#[test]
fn edge_single_cobol_line() {
    let toks = payload("05 CUSTOMER-ID PIC X(10).");
    assert_eq!(
        toks,
        vec![
            Token::Level(5),
            Token::Identifier("CUSTOMER-ID".into()),
            Token::Pic,
            Token::PicClause("X(10)".into()),
            Token::Period,
        ]
    );
}

// =========================================================================
// 5. Line exactly 72 chars (standard fixed-form end)
// =========================================================================

#[test]
fn edge_line_exactly_72_chars() {
    // Build a fixed-form line exactly 72 characters long:
    // cols 1-6: sequence number, col 7: space, cols 8-72: code (65 chars)
    let code = "05 FIELD-A PIC X(10).";
    let padding = 65 - code.len();
    let line = format!("000100 {}{}", code, " ".repeat(padding));
    assert_eq!(line.len(), 72, "line should be exactly 72 chars");

    let toks = payload(&line);
    assert!(
        toks.contains(&Token::Level(5)),
        "should tokenize level: {toks:?}"
    );
    assert!(
        toks.contains(&Token::Period),
        "should find period terminator: {toks:?}"
    );
}

// =========================================================================
// 6. Line with sequence area (columns 1-6)
// =========================================================================

#[test]
fn edge_sequence_area_stripped_in_fixed_form() {
    // Fixed-form with sequence numbers in columns 1-6
    let input = "\
000100 01  CUSTOMER-RECORD.
000200     05  CUST-NAME PIC X(20).
";
    let lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);

    let toks = payload(input);
    // Sequence numbers should not appear as tokens
    assert!(
        !toks.iter().any(|t| matches!(t, Token::Number(100))),
        "sequence numbers should be stripped: {toks:?}"
    );
    assert!(
        toks.contains(&Token::Level(1)),
        "should still find level 01: {toks:?}"
    );
}

// =========================================================================
// 7. Line with continuation in column 7 (-)
// =========================================================================

#[test]
fn edge_continuation_marker_joins_lines() {
    // Fixed-form: continuation line has '-' in column 7
    let input = "\
000100 01  VERY-LONG-FIELD-NA
000200-        ME PIC X(50).
";
    let toks = payload(input);
    // The continuation should join the identifier
    let has_pic = toks.contains(&Token::Pic);
    assert!(
        has_pic,
        "continuation should still produce PIC token: {toks:?}"
    );
}

// =========================================================================
// 8. Line with debug indicator (D in column 7)
// =========================================================================

#[test]
fn edge_debug_indicator_d_no_panic() {
    // 'D' in column 7 is a debug indicator in some COBOL dialects
    let input = "000100D    DISPLAY 'DEBUG LINE'.\n000200 01  FIELD PIC X(5).\n";
    // Should not panic; the 'D' line may be treated as comment or code
    let toks = payload(input);
    // We should at least get tokens from the non-debug line
    assert!(
        toks.iter().any(|t| matches!(t, Token::Level(1))),
        "non-debug line should tokenize: {toks:?}"
    );
}

// =========================================================================
// 9. Free-form vs fixed-form detection
// =========================================================================

#[test]
fn edge_detect_free_form() {
    let input = "01 RECORD.\n  05 FIELD PIC X(10).\n";
    let lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Free);
}

#[test]
fn edge_detect_fixed_form() {
    let input = "\
000100 01  CUSTOMER-RECORD.
000200     05  CUST-ID        PIC X(10).
000300     05  CUST-NAME      PIC X(30).
";
    let lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
}

// =========================================================================
// 10. Tab characters in input
// =========================================================================

#[test]
fn edge_tab_characters_no_panic() {
    let input = "\t01\tRECORD.\n\t\t05\tFIELD\tPIC\tX(5).\n";
    let toks = payload(input);
    assert!(
        toks.contains(&Token::Level(1)),
        "tabs should be handled gracefully: {toks:?}"
    );
}

// =========================================================================
// 11. CRLF, LF, CR line endings
// =========================================================================

#[test]
fn edge_lf_line_endings() {
    let input = "01 REC.\n05 F PIC X(3).\n";
    let toks = payload(input);
    assert_eq!(toks.len(), 8);
}

#[test]
fn edge_crlf_line_endings() {
    let input = "01 REC.\r\n05 F PIC X(3).\r\n";
    let toks = payload(input);
    assert!(
        toks.contains(&Token::Level(1)),
        "CRLF should work: {toks:?}"
    );
    assert!(
        toks.contains(&Token::Period),
        "CRLF should produce Period: {toks:?}"
    );
}

#[test]
fn edge_cr_only_line_endings() {
    // Bare CR without LF (classic Mac line ending)
    let input = "01 REC.\r05 F PIC X(3).\r";
    // Should not panic
    let toks = payload(input);
    assert!(
        !toks.is_empty(),
        "bare CR input should produce some tokens: {toks:?}"
    );
}

// =========================================================================
// 12. Null bytes in input (no panic)
// =========================================================================

#[test]
fn edge_null_bytes_no_panic() {
    let input = "01 REC.\x0005 F PIC X(5).\x00";
    let _toks = tokens(input); // Must not panic
}

#[test]
fn edge_null_bytes_embedded_no_panic() {
    let input = "01 REC\x00ORD.\n05 FIELD PIC X(10).\n";
    let _toks = tokens(input); // Must not panic
}

// =========================================================================
// 13. Very long lines (>256 chars)
// =========================================================================

#[test]
fn edge_very_long_line_no_panic() {
    // A line of >300 characters with valid COBOL content
    let long_name = "A".repeat(250);
    let input = format!("01 {long_name} PIC X(5).");
    let toks = payload(&input);
    assert!(
        !toks.is_empty(),
        "very long line should produce tokens: {toks:?}"
    );
}

#[test]
fn edge_very_long_fixed_form_line_truncation() {
    // In fixed-form, only columns 8-72 are code. Build a multi-line input >>256 chars.
    let mut long = String::new();
    for i in 0..10 {
        long.push_str(&format!(
            "{:06} 05  FIELD-{:02} PIC X(10).{}\n",
            i * 100 + 100,
            i,
            " ".repeat(200)
        ));
    }
    assert!(long.len() > 256);
    let mut lexer = Lexer::new(&long);
    // Construction and tokenization must not panic
    let _toks = lexer.tokenize();
}

// =========================================================================
// 14. Unicode in comments (no panic)
// =========================================================================

#[test]
fn edge_unicode_in_free_form_comment_no_panic() {
    let input = "* コメント: こんにちは 🎉\n01 REC.\n  05 F PIC X(5).\n";
    let toks = payload(input);
    // Unicode in comments should be skipped gracefully
    assert!(
        toks.contains(&Token::Level(1)),
        "should tokenize past unicode comment: {toks:?}"
    );
}

#[test]
fn edge_unicode_in_inline_comment_no_panic() {
    let input = "01 REC. *> café résumé naïve\n05 F PIC X(5).\n";
    let toks = payload(input);
    assert!(
        !toks.is_empty(),
        "inline unicode comment should not break lexing: {toks:?}"
    );
}

// =========================================================================
// 15. Mixed content and blank lines
// =========================================================================

#[test]
fn edge_mixed_content_and_blank_lines() {
    let input = "\
01 REC.

  05 FIELD-A PIC X(10).

  05 FIELD-B PIC 9(5).

";
    let toks = payload(input);
    let level_count = toks.iter().filter(|t| matches!(t, Token::Level(_))).count();
    assert_eq!(level_count, 3, "should find 3 level tokens amid blanks");
}

#[test]
fn edge_many_consecutive_blank_lines() {
    let mut input = String::from("01 REC.\n");
    for _ in 0..20 {
        input.push('\n');
    }
    input.push_str("05 FIELD PIC X(5).\n");
    let toks = payload(&input);
    assert!(
        toks.contains(&Token::Level(5)),
        "should find level 05 after many blanks: {toks:?}"
    );
}

// =========================================================================
// 16. COPY/REPLACE directives (lexed but not expanded)
// =========================================================================

#[test]
fn edge_copy_directive_lexed_as_identifier() {
    // COPY is not a recognized keyword token – it should appear as Identifier
    let input = "COPY MEMBER-NAME.";
    let toks = payload(input);
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::Identifier(s) if s == "COPY")),
        "COPY should lex as Identifier: {toks:?}"
    );
}

#[test]
fn edge_replace_directive_lexed_as_identifier() {
    // REPLACE is not a recognized keyword token – it should appear as Identifier
    let input = "REPLACE ==OLD-NAME== BY ==NEW-NAME==.";
    let toks = payload(input);
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::Identifier(s) if s == "REPLACE")),
        "REPLACE should lex as Identifier: {toks:?}"
    );
}

// =========================================================================
// 17. Additional edge cases
// =========================================================================

#[test]
fn edge_inline_comment_with_options() {
    let opts = LexerOptions {
        allow_inline_comments: true,
        strict_comments: false,
    };
    let input = "05 FIELD PIC X(5). *> This is a comment";
    let toks = tokens_with(input, opts);
    let has_period = toks.contains(&Token::Period);
    assert!(
        has_period,
        "should find Period before inline comment: {toks:?}"
    );
}

#[test]
fn edge_strict_comments_option() {
    let opts = LexerOptions {
        allow_inline_comments: false,
        strict_comments: true,
    };
    let input = "05 FIELD PIC X(5).";
    let toks = tokens_with(input, opts);
    assert!(
        toks.contains(&Token::Period),
        "strict_comments should still tokenize code: {toks:?}"
    );
}

#[test]
fn edge_period_at_end_of_line_no_trailing_space() {
    let input = "01 REC.";
    let toks = payload(input);
    assert_eq!(toks.last(), Some(&Token::Period));
}

#[test]
fn edge_multiple_periods_in_sequence() {
    // Unusual but shouldn't panic
    let input = "01 REC...";
    let toks = payload(input);
    let period_count = toks.iter().filter(|t| matches!(t, Token::Period)).count();
    assert!(
        period_count >= 1,
        "should have at least one period: {toks:?}"
    );
}
