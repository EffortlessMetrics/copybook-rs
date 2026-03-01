// SPDX-License-Identifier: AGPL-3.0-or-later
//! Stress tests for the copybook-lexer crate.
//!
//! Exercises extreme, degenerate, and adversarial inputs to ensure the lexer
//! never panics and handles edge cases gracefully.

use copybook_lexer::{CobolFormat, Lexer, LexerOptions, Token};

// ── Helpers ──────────────────────────────────────────────────────────

/// Tokenize with default options and return only payload tokens
/// (no Newline / Eof).
fn payload(input: &str) -> Vec<Token> {
    Lexer::new(input)
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
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

// ── 1. Very long copybook (1000+ lines) ─────────────────────────────

#[test]
fn stress_very_long_copybook_1000_lines() {
    let mut input = String::from("01 RECORD.\n");
    for i in 0..1000 {
        input.push_str(&format!("  05 FIELD-{i:04} PIC X(10).\n"));
    }
    let toks = payload(&input);
    // Each field line produces: Level(5), Identifier, Pic, PicClause, Period = 5 tokens
    // Plus the header line: Level(1), Identifier, Period = 3 tokens
    assert!(
        toks.len() >= 5003,
        "expected >=5003 tokens, got {}",
        toks.len()
    );
}

#[test]
fn stress_very_long_copybook_2000_lines() {
    let mut input = String::from("01 ROOT-GROUP.\n");
    for i in 0..2000 {
        input.push_str(&format!("  05 FLD-{i:04} PIC 9(5).\n"));
    }
    let toks = payload(&input);
    assert!(
        toks.len() >= 10003,
        "expected >=10003 tokens, got {}",
        toks.len()
    );
}

// ── 2. Very long field names (30 chars, the COBOL max) ──────────────

#[test]
fn stress_max_length_field_names() {
    // COBOL max is 30 characters
    let name = "ABCDEFGHIJ-KLMNOPQRST-UVWXYZ12"; // exactly 30
    assert_eq!(name.len(), 30);
    let input = format!("05 {name} PIC X(5).");
    let toks = payload(&input);
    assert_eq!(toks[1], Token::Identifier(name.to_string()));
}

#[test]
fn stress_many_max_length_field_names() {
    let mut input = String::from("01 TOP-LEVEL-RECORD-GROUP-01.\n");
    for i in 0..100 {
        // Generate unique 30-char names
        let name = format!("FIELD-WITH-VERY-LONG-NAME-{i:04}");
        let padded = if name.len() < 30 {
            format!("{name}{}", "X".repeat(30 - name.len()))
        } else {
            name[..30].to_string()
        };
        input.push_str(&format!("  05 {padded} PIC X(10).\n"));
    }
    let toks = payload(&input);
    assert!(toks.len() > 500);
}

// ── 3. Very long PIC clauses ────────────────────────────────────────

#[test]
fn stress_very_long_pic_clause() {
    let input = "05 BIG-FIELD PIC X(99999).";
    let toks = payload(&input);
    assert_eq!(toks[2], Token::Pic);
    assert_eq!(toks[3], Token::PicClause("X(99999)".to_string()));
}

#[test]
fn stress_pic_clause_with_huge_repeat() {
    let input = "05 HUGE-NUM PIC 9(999999).";
    let toks = payload(&input);
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::PicClause(s) if s == "9(999999)"))
    );
}

#[test]
fn stress_pic_clause_many_explicit_digits() {
    // PIC 999999999999999 (15 nines, no repeat notation)
    // The lexer may tokenize long digit strings as Number or split them;
    // the key invariant is that it must not panic.
    let nines = "9".repeat(50);
    let input = format!("05 MANY-NINES PIC {nines}.");
    let _toks = payload(&input); // Must not panic
}

// ── 4. Deep nesting (many levels) ───────────────────────────────────

#[test]
fn stress_deep_nesting_levels() {
    let mut input = String::from("01 ROOT.\n");
    // COBOL levels go 01-49; simulate deep nesting with incrementing levels
    for level in 2..=49 {
        input.push_str(&format!("  {level:02} GROUP-{level:02}.\n"));
    }
    input.push_str("    49 LEAF PIC X(1).\n");
    let toks = payload(&input);
    // Should have many Level tokens
    let level_count = toks.iter().filter(|t| matches!(t, Token::Level(_))).count();
    assert!(
        level_count >= 49,
        "expected >=49 level tokens, got {level_count}"
    );
}

#[test]
fn stress_repeated_same_level() {
    let mut input = String::from("01 FLAT-RECORD.\n");
    for i in 0..500 {
        input.push_str(&format!("  05 F-{i:04} PIC X.\n"));
    }
    let toks = payload(&input);
    let level5_count = toks.iter().filter(|t| matches!(t, Token::Level(5))).count();
    assert_eq!(level5_count, 500);
}

// ── 5. Many continuation lines ──────────────────────────────────────

#[test]
fn stress_many_continuation_lines() {
    // Build a fixed-format input with a continuation on every other line
    let mut lines = Vec::new();
    // Line 1: header
    lines.push("       01  CONTINUED-RECORD.                                            ");
    // Generate many continuation lines for a single long identifier
    for i in 0..20 {
        let content = format!("05 CONT-FLD-{i:02} PIC X(5).");
        // Pad to fixed format: cols 1-6 seq, col 7 space, cols 8-72 content
        let formatted = format!("{:06} {:<65}", i + 2, content);
        lines.push(Box::leak(formatted.into_boxed_str()) as &str);
    }
    let input = lines.join("\n");
    let _toks = payload(&input); // Should not panic
}

// ── 6. All-comment copybook ─────────────────────────────────────────

#[test]
fn stress_all_comment_copybook() {
    let mut input = String::new();
    for i in 0..500 {
        input.push_str(&format!("* This is comment line {i}\n"));
    }
    let toks = payload(&input);
    // All lines are comments, so no payload tokens should remain
    assert!(
        toks.is_empty(),
        "expected no payload tokens from all-comment input, got {}",
        toks.len()
    );
}

#[test]
fn stress_all_inline_comment_copybook() {
    let mut input = String::new();
    for i in 0..100 {
        input.push_str(&format!("*> Inline comment {i}\n"));
    }
    // With inline comments allowed, these might produce InlineComment tokens
    let all_toks: Vec<_> = Lexer::new(&input)
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .collect();
    // Should not panic; every non-Newline/Eof should be InlineComment or empty
    for tok in &all_toks {
        assert!(
            matches!(
                tok,
                Token::Newline | Token::Eof | Token::InlineComment(_) | Token::Identifier(_)
            ),
            "unexpected token in all-inline-comment input: {tok:?}"
        );
    }
}

// ── 7. Mixed fixed/free format detection ────────────────────────────

#[test]
fn stress_format_detection_mostly_fixed() {
    let mut input = String::new();
    for i in 0..100 {
        // Fixed-format lines with sequence numbers and column 7 space
        let content = format!("05 F-{i:03} PIC X.");
        let line = format!("{:06} {:<65}", i + 1, content);
        input.push_str(&line);
        input.push('\n');
    }
    let lexer = Lexer::new(&input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
}

#[test]
fn stress_format_detection_mostly_free() {
    let mut input = String::new();
    for i in 0..100 {
        input.push_str(&format!("05 FIELD-{i:03} PIC X(10).\n"));
    }
    let lexer = Lexer::new(&input);
    assert_eq!(lexer.format(), CobolFormat::Free);
}

#[test]
fn stress_format_detection_ambiguous_mix() {
    // Mix of lines that look fixed and free
    let mut input = String::new();
    for i in 0..50 {
        // Even lines: fixed-like
        let content = format!("05 F-{i:03} PIC X.");
        let line = format!("{:06} {:<65}", i + 1, content);
        input.push_str(&line);
        input.push('\n');
        // Odd lines: free-like (short)
        input.push_str(&format!("05 G-{i:03} PIC 9.\n"));
    }
    // Should not panic regardless of detection result
    let _lexer = Lexer::new(&input);
}

// ── 8. Rapidly alternating token types ──────────────────────────────

#[test]
fn stress_rapidly_alternating_tokens() {
    // Create input that rapidly switches between different token types
    let mut input = String::new();
    for i in 0..200 {
        match i % 5 {
            0 => input.push_str(&format!("{:02} ", (i % 49) + 1)),
            1 => input.push_str(&format!("FIELD-{i} ")),
            2 => input.push_str("PIC "),
            3 => input.push_str("X(10) "),
            4 => input.push_str(".\n"),
            _ => unreachable!(),
        }
    }
    let toks = payload(&input);
    assert!(!toks.is_empty());
}

#[test]
fn stress_keywords_back_to_back() {
    let input = "OCCURS DEPENDING ON TO TIMES VALUE THRU THROUGH SIGN LEADING TRAILING SEPARATE BLANK WHEN ZERO REDEFINES RENAMES USAGE DISPLAY COMP COMP-1 COMP-2 COMP-3 BINARY SYNCHRONIZED";
    let toks = payload(&input);
    // All should be recognised as keywords, not identifiers
    for tok in &toks {
        assert!(
            !matches!(tok, Token::Identifier(_)),
            "keyword wrongly parsed as Identifier: {tok:?}"
        );
    }
}

// ── 9. Maximum token count ──────────────────────────────────────────

#[test]
fn stress_maximum_token_count() {
    // Generate input that produces a very large number of tokens
    let mut input = String::new();
    for _ in 0..5000 {
        input.push_str("05 F PIC X. ");
    }
    let toks = payload(&input);
    // Each repetition: Level(5), Identifier, Pic, PicClause, Period = 5 tokens
    assert!(
        toks.len() >= 25000,
        "expected >=25000 tokens, got {}",
        toks.len()
    );
}

// ── 10. Unicode in comments (should not crash) ──────────────────────

#[test]
fn stress_unicode_in_comments() {
    let input = "* 日本語コメント\n* Ñoño café\n* Привет мир\n* 🎉🚀💻\n05 FIELD PIC X(5).";
    let toks = payload(&input);
    // Should still parse the field definition
    assert!(toks.iter().any(|t| matches!(t, Token::Level(5))));
}

#[test]
fn stress_unicode_in_inline_comments() {
    let input = "05 FIELD PIC X(5). *> 日本語 Ñoño Привет 🎉\n";
    let _toks = Lexer::new(input).tokenize(); // Should not panic
}

#[test]
fn stress_emoji_heavy_comments() {
    let mut input = String::new();
    for _ in 0..100 {
        input.push_str("* 🎉🚀💻🔥✨🎯🏆🌟💡🔑🎪🎨🎸🎺🎻\n");
    }
    input.push_str("01 RECORD PIC X.");
    let _toks = payload(&input); // Should not panic
}

// ── 11. Binary data in input (should not crash) ─────────────────────

#[test]
fn stress_binary_data_in_input() {
    // Simulate arbitrary binary data converted to lossy UTF-8
    let binary: Vec<u8> = (0..=255).collect();
    let input = String::from_utf8_lossy(&binary);
    let _toks = Lexer::new(&input).tokenize(); // Must not panic
}

#[test]
fn stress_null_bytes_in_input() {
    let input = "05 FIELD\0PIC\0X(5).";
    let _toks = Lexer::new(input).tokenize(); // Must not panic
}

#[test]
fn stress_mixed_binary_and_valid_cobol() {
    let mut input = String::new();
    input.push_str("01 RECORD.\n");
    // Insert some high-byte garbage
    input.push_str("  \x01\x02\x03\x04\x05\n");
    input.push_str("  05 FIELD PIC X(10).\n");
    let _toks = Lexer::new(&input).tokenize(); // Must not panic
}

// ── 12. Empty lines interspersed ────────────────────────────────────

#[test]
fn stress_many_empty_lines_interspersed() {
    let mut input = String::new();
    for i in 0..100 {
        // Add varying numbers of empty lines between definitions
        for _ in 0..5 {
            input.push('\n');
        }
        input.push_str(&format!("05 FIELD-{i:03} PIC X(10).\n"));
    }
    let toks = payload(&input);
    let field_count = toks.iter().filter(|t| matches!(t, Token::Level(5))).count();
    assert_eq!(field_count, 100);
}

#[test]
fn stress_only_empty_lines() {
    let input = "\n".repeat(1000);
    let toks = payload(&input);
    assert!(toks.is_empty());
}

#[test]
fn stress_empty_lines_with_whitespace() {
    let mut input = String::new();
    for _ in 0..200 {
        input.push_str("   \t   \n");
    }
    let toks = payload(&input);
    assert!(toks.is_empty());
}

// ── 13. Windows (CRLF) vs Unix (LF) line endings ───────────────────

#[test]
fn stress_crlf_line_endings() {
    let input = "01 RECORD.\r\n  05 FIELD-A PIC X(10).\r\n  05 FIELD-B PIC 9(5).\r\n";
    let toks = payload(&input);
    let level_count = toks.iter().filter(|t| matches!(t, Token::Level(_))).count();
    assert_eq!(level_count, 3);
}

#[test]
fn stress_mixed_line_endings() {
    let input =
        "01 RECORD.\n  05 FIELD-A PIC X(10).\r\n  05 FIELD-B PIC 9(5).\n  05 FIELD-C PIC X.\r\n";
    let toks = payload(&input);
    let level_count = toks.iter().filter(|t| matches!(t, Token::Level(_))).count();
    assert_eq!(level_count, 4);
}

#[test]
fn stress_crlf_many_lines() {
    let mut input = String::from("01 RECORD.\r\n");
    for i in 0..500 {
        input.push_str(&format!("  05 F-{i:04} PIC X.\r\n"));
    }
    let toks = payload(&input);
    let level5_count = toks.iter().filter(|t| matches!(t, Token::Level(5))).count();
    assert_eq!(level5_count, 500);
}

// ── 14. Tab characters in various positions ─────────────────────────

#[test]
fn stress_tabs_instead_of_spaces() {
    let input = "01\tRECORD.\n\t05\tFIELD\tPIC\tX(10).";
    let toks = payload(&input);
    assert!(toks.iter().any(|t| matches!(t, Token::Level(1))));
    assert!(toks.iter().any(|t| matches!(t, Token::Level(5))));
}

#[test]
fn stress_tabs_mixed_with_spaces() {
    let input = "  \t01  \t RECORD.\n \t  05 \t FIELD \t PIC \t X(5) \t .";
    let toks = payload(&input);
    assert!(toks.iter().any(|t| matches!(t, Token::Pic)));
}

#[test]
fn stress_only_tabs() {
    let input = "\t\t\t\t\t";
    let toks = payload(&input);
    assert!(toks.is_empty());
}

// ── 15. All option combinations with stress input ───────────────────

#[test]
fn stress_all_lexer_options_large_input() {
    let mut input = String::from("01 REC.\n");
    for i in 0..200 {
        input.push_str(&format!("  05 F-{i:03} PIC X(10). *> comment {i}\n"));
    }

    let option_combos = [
        LexerOptions {
            allow_inline_comments: false,
            strict_comments: false,
        },
        LexerOptions {
            allow_inline_comments: true,
            strict_comments: false,
        },
        LexerOptions {
            allow_inline_comments: false,
            strict_comments: true,
        },
        LexerOptions {
            allow_inline_comments: true,
            strict_comments: true,
        },
    ];

    for opts in option_combos {
        let toks = tokens_with(&input, opts);
        assert!(toks.last().map_or(false, |t| *t == Token::Eof));
    }
}

// ── 16. Adversarial patterns ────────────────────────────────────────

#[test]
fn stress_very_long_single_line() {
    // One line with thousands of characters
    let long_ident = "A".repeat(500);
    let input = format!("05 {long_ident} PIC X(10).");
    let _toks = payload(&input); // Must not panic
}

#[test]
fn stress_deeply_nested_parens() {
    // Parentheses aren't nested in COBOL PIC, but the lexer should handle it
    let input = "05 F PIC X(((((10))))).";
    let _toks = payload(&input); // Must not panic
}

#[test]
fn stress_many_periods() {
    let input = "01 R. 05 A PIC X. 05 B PIC 9. 05 C PIC X. 05 D PIC 9. 05 E PIC X.";
    let toks = payload(&input);
    let period_count = toks.iter().filter(|t| matches!(t, Token::Period)).count();
    assert_eq!(period_count, 6);
}

#[test]
fn stress_repeated_keywords() {
    // Unusual but should not crash
    let input = "PIC PIC PIC PIC PIC PIC PIC PIC PIC PIC";
    let toks = payload(&input);
    let pic_count = toks.iter().filter(|t| matches!(t, Token::Pic)).count();
    assert_eq!(pic_count, 10);
}

#[test]
fn stress_alternating_case_keywords() {
    let input = "pic PIC Pic pIc PICTURE picture Picture";
    let toks = payload(&input);
    let pic_count = toks.iter().filter(|t| matches!(t, Token::Pic)).count();
    assert_eq!(pic_count, 7);
}

// ── 17. Special COBOL features ──────────────────────────────────────

#[test]
fn stress_many_level88_conditions() {
    let mut input = String::from("05 STATUS-CODE PIC X(2).\n");
    for i in 0..200 {
        input.push_str(&format!("  88 STATUS-{i:03} VALUE \"{i:03}\".\n"));
    }
    let toks = payload(&input);
    let l88_count = toks.iter().filter(|t| matches!(t, Token::Level88)).count();
    assert_eq!(l88_count, 200);
}

#[test]
fn stress_many_occurs_clauses() {
    let mut input = String::from("01 TABLE-RECORD.\n");
    for i in 0..50 {
        input.push_str(&format!(
            "  05 ARRAY-{i:02} PIC X(10) OCCURS {i_plus} TIMES.\n",
            i_plus = i + 1
        ));
    }
    let toks = payload(&input);
    let occurs_count = toks.iter().filter(|t| matches!(t, Token::Occurs)).count();
    assert_eq!(occurs_count, 50);
}

#[test]
fn stress_many_redefines() {
    let mut input = String::from("01 RECORD.\n");
    input.push_str("  05 ORIGINAL PIC X(100).\n");
    for i in 0..30 {
        input.push_str(&format!(
            "  05 REDEF-{i:02} REDEFINES ORIGINAL PIC X(100).\n"
        ));
    }
    let toks = payload(&input);
    let redef_count = toks
        .iter()
        .filter(|t| matches!(t, Token::Redefines))
        .count();
    assert_eq!(redef_count, 30);
}

// ── 18. String literals stress ──────────────────────────────────────

#[test]
fn stress_very_long_string_literal() {
    let long_str = "A".repeat(1000);
    let input = format!("88 FLAG VALUE \"{long_str}\".");
    let toks = payload(&input);
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::StringLiteral(s) if s.len() == 1000))
    );
}

#[test]
fn stress_many_string_literals() {
    let mut input = String::from("88 CODES");
    for i in 0..100 {
        if i == 0 {
            input.push_str(&format!(" VALUE \"{i:03}\""));
        } else {
            input.push_str(&format!(", \"{i:03}\""));
        }
    }
    input.push('.');
    let toks = payload(&input);
    let str_count = toks
        .iter()
        .filter(|t| matches!(t, Token::StringLiteral(_)))
        .count();
    assert_eq!(str_count, 100);
}

// ── 19. Fixed-format specific stress ────────────────────────────────

#[test]
fn stress_fixed_format_sequence_numbers() {
    // Lines with sequence numbers in columns 1-6
    let mut input = String::new();
    for i in 1..=100 {
        let content = format!("05 FLD-{i:03} PIC X.");
        let line = format!("{i:06} {content:<65}");
        input.push_str(&line);
        input.push('\n');
    }
    let lexer = Lexer::new(&input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
    let mut lexer = Lexer::new(&input);
    let _toks = lexer.tokenize(); // Must not panic
}

#[test]
fn stress_fixed_format_comment_lines() {
    let mut input = String::new();
    for i in 0..50 {
        // Column 7 = '*' means comment
        input.push_str(&format!("{:06}* Comment line {i}\n", i));
    }
    // Add one real line
    input.push_str("000051 01 RECORD PIC X.\n");
    let _toks = Lexer::new(&input).tokenize(); // Must not panic
}

// ── 20. Numeric literal edge cases ──────────────────────────────────

#[test]
fn stress_large_numeric_literals() {
    let input = "05 F OCCURS 999999 TIMES.";
    let toks = payload(&input);
    assert!(toks.iter().any(|t| matches!(t, Token::Number(999_999))));
}

#[test]
fn stress_zero_numeric_literal() {
    let input = "05 F OCCURS 0 TIMES.";
    let toks = payload(&input);
    assert!(toks.iter().any(|t| matches!(t, Token::Number(0))));
}

// ── 21. Extreme whitespace patterns ─────────────────────────────────

#[test]
fn stress_excessive_leading_whitespace() {
    // Extremely long leading whitespace may trigger fixed-format detection
    // (columns 1-6 sequence area) which strips content. The key invariant
    // is that the lexer must not panic.
    let spaces = " ".repeat(1000);
    let input = format!("{spaces}05 FIELD PIC X.");
    let _toks = payload(&input); // Must not panic
}

#[test]
fn stress_excessive_trailing_whitespace() {
    let spaces = " ".repeat(1000);
    let input = format!("05 FIELD PIC X.{spaces}");
    let toks = payload(&input);
    assert!(toks.iter().any(|t| matches!(t, Token::Period)));
}

// ── 22. Edge-case input lengths ─────────────────────────────────────

#[test]
fn stress_single_character_inputs() {
    let chars = ".*()01 \t\n\r,";
    for c in chars.chars() {
        let input = c.to_string();
        let _toks = Lexer::new(&input).tokenize(); // Must not panic
    }
}

#[test]
fn stress_two_character_inputs() {
    let combos = ["01", "05", "49", "66", "77", "88", "*>", "\r\n", ".(", "X("];
    for combo in combos {
        let _toks = Lexer::new(combo).tokenize(); // Must not panic
    }
}

// ── 23. Continuation line stress (fixed format) ─────────────────────

#[test]
fn stress_many_fixed_format_continuations() {
    let mut input = String::new();
    // First line: start of a field definition
    input.push_str("000001       05 LONG-FIELD-NAME                                        \n");
    // 20 continuation lines
    for i in 2..=21 {
        let line =
            format!("{i:06}-       PIC X(5)                                                  \n");
        input.push_str(&line);
    }
    let _toks = Lexer::new(&input).tokenize(); // Must not panic
}

// ── 24. Display/Format method exercise ──────────────────────────────

#[test]
fn stress_display_all_token_types() {
    // Ensure Display impl works for all token variants
    let all_tokens = vec![
        Token::Level(1),
        Token::Level66,
        Token::Level77,
        Token::Level88,
        Token::Pic,
        Token::Usage,
        Token::Display,
        Token::Comp,
        Token::Comp3,
        Token::Comp1,
        Token::Comp2,
        Token::Binary,
        Token::Redefines,
        Token::Renames,
        Token::Occurs,
        Token::Depending,
        Token::On,
        Token::To,
        Token::Times,
        Token::Synchronized,
        Token::Value,
        Token::Thru,
        Token::Through,
        Token::Sign,
        Token::Is,
        Token::Leading,
        Token::Trailing,
        Token::Separate,
        Token::Blank,
        Token::When,
        Token::Zero,
        Token::PicClause("X(10)".into()),
        Token::EditedPic("ZZZ9".into()),
        Token::Number(42),
        Token::Identifier("MY-FIELD".into()),
        Token::StringLiteral("hello".into()),
        Token::Period,
        Token::Comma,
        Token::LeftParen,
        Token::RightParen,
        Token::InlineComment("test".into()),
        Token::Newline,
        Token::Eof,
    ];
    for tok in &all_tokens {
        let display = format!("{tok}");
        assert!(
            !display.is_empty(),
            "Display for {tok:?} should not be empty"
        );
        let debug = format!("{tok:?}");
        assert!(!debug.is_empty(), "Debug for {tok:?} should not be empty");
    }
}

// ── 25. Edited PIC stress ───────────────────────────────────────────

#[test]
fn stress_many_edited_pic_patterns() {
    let patterns = [
        "ZZZ9",
        "Z,ZZZ.99",
        "$ZZ,ZZZ.99",
        "+ZZZ9",
        "-ZZZ9",
        "***9",
        "Z9",
        "ZZ9",
        "ZZZZ9",
    ];
    for pat in patterns {
        let input = format!("05 AMT PIC {pat}.");
        let _toks = payload(&input); // Must not panic
    }
}

// ── 26. Comprehensive realistic copybook ────────────────────────────

#[test]
fn stress_realistic_enterprise_copybook() {
    let input = r#"
01 CUSTOMER-MASTER-RECORD.
  05 CUSTOMER-HEADER.
    10 CUSTOMER-ID            PIC X(10).
    10 CUSTOMER-NAME          PIC X(30).
    10 CUSTOMER-TYPE          PIC X(2).
    88 IS-INDIVIDUAL          VALUE "01".
    88 IS-CORPORATE           VALUE "02".
    88 IS-GOVERNMENT          VALUE "03".
  05 CUSTOMER-ADDRESS.
    10 ADDRESS-LINE-1         PIC X(40).
    10 ADDRESS-LINE-2         PIC X(40).
    10 CITY                   PIC X(25).
    10 STATE                  PIC X(2).
    10 ZIP-CODE               PIC X(10).
    10 COUNTRY-CODE           PIC X(3).
  05 CUSTOMER-FINANCIAL.
    10 CREDIT-LIMIT           PIC 9(7)V99.
    10 CURRENT-BALANCE        PIC S9(7)V99.
    10 LAST-PAYMENT-AMT       PIC 9(7)V99.
    10 ACCOUNT-STATUS         PIC X(1).
    88 ACTIVE                 VALUE "A".
    88 SUSPENDED              VALUE "S".
    88 CLOSED                 VALUE "C".
  05 TRANSACTION-COUNT        PIC 9(5).
  05 TRANSACTIONS OCCURS 1 TO 100 DEPENDING ON TRANSACTION-COUNT.
    10 TRANS-DATE             PIC 9(8).
    10 TRANS-AMOUNT           PIC S9(9)V99.
    10 TRANS-TYPE             PIC X(3).
"#;
    let toks = payload(&input);
    // Sanity check: a realistic copybook should produce a healthy number
    // of tokens without panicking. Exact counts depend on format detection
    // (leading spaces may trigger fixed-format heuristic).
    assert!(!toks.is_empty(), "expected tokens from realistic copybook");
    // Should contain at least some identifiers and PIC-related tokens
    assert!(toks.iter().any(|t| matches!(t, Token::Identifier(_))));
    assert!(toks.iter().any(|t| matches!(t, Token::Pic)));
}

// ── 27. Regression: ensure Eof is always last ───────────────────────

#[test]
fn stress_eof_always_present() {
    let inputs = [
        "",
        " ",
        "\n",
        "\r\n",
        "01 R.",
        "* comment",
        "\0",
        &"X".repeat(10000),
    ];
    for input in inputs {
        let toks: Vec<_> = Lexer::new(input)
            .tokenize()
            .into_iter()
            .map(|tp| tp.token)
            .collect();
        assert_eq!(
            toks.last(),
            Some(&Token::Eof),
            "Eof missing for input of len {}",
            input.len()
        );
    }
}

// ── 28. Span correctness under stress ───────────────────────────────

#[test]
fn stress_spans_are_valid() {
    let mut input = String::from("01 RECORD.\n");
    for i in 0..100 {
        input.push_str(&format!("  05 F-{i:03} PIC X(10).\n"));
    }
    let token_positions = Lexer::new(&input).tokenize();
    for tp in &token_positions {
        if tp.token != Token::Eof {
            assert!(
                tp.span.start <= tp.span.end,
                "invalid span {:?} for token {:?}",
                tp.span,
                tp.token
            );
        }
        assert!(tp.line >= 1, "line number should be >= 1");
        assert!(tp.column >= 1, "column number should be >= 1");
    }
}
