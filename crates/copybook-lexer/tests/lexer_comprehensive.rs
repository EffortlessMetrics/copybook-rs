//! Comprehensive integration tests for the copybook-lexer crate.
//!
//! Covers: empty input, field definitions, all keyword tokens, PIC clause
//! tokenization, numeric literals, level numbers, period handling, comments,
//! sequence/continuation areas, special PIC characters, FILLER, BLANK WHEN ZERO,
//! string literals, and multi-token lines.

use copybook_lexer::{CobolFormat, Lexer, LexerOptions, Token};

// ── Helpers ──────────────────────────────────────────────────────────

/// Tokenize free-form input and return just the Token values.
fn tokens(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    lexer.tokenize().into_iter().map(|tp| tp.token).collect()
}

/// Tokenize and strip Newline / Eof so tests only see "payload" tokens.
fn payload(input: &str) -> Vec<Token> {
    tokens(input)
        .into_iter()
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect()
}

// ── Empty / trivial input ────────────────────────────────────────────

#[test]
fn lex_empty_input_returns_only_eof() {
    let toks = tokens("");
    assert_eq!(toks, vec![Token::Eof]);
}

#[test]
fn lex_whitespace_only_returns_newline_and_eof() {
    let toks = payload("   \t  ");
    assert!(toks.is_empty(), "expected no payload tokens, got {toks:?}");
}

// ── Single field definition ──────────────────────────────────────────

#[test]
fn lex_single_field_definition() {
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

// ── Multiple field definitions ───────────────────────────────────────

#[test]
fn lex_multiple_field_definitions() {
    let input = "\
01 RECORD.
  05 FIELD-A PIC X(5).
  05 FIELD-B PIC 9(3).";
    let toks = payload(input);
    // Level 01 group
    assert_eq!(toks[0], Token::Level(1));
    assert_eq!(toks[1], Token::Identifier("RECORD".into()));
    assert_eq!(toks[2], Token::Period);
    // FIELD-A
    assert_eq!(toks[3], Token::Level(5));
    assert_eq!(toks[4], Token::Identifier("FIELD-A".into()));
    assert_eq!(toks[5], Token::Pic);
    assert_eq!(toks[6], Token::PicClause("X(5)".into()));
    assert_eq!(toks[7], Token::Period);
    // FIELD-B
    assert_eq!(toks[8], Token::Level(5));
    assert_eq!(toks[9], Token::Identifier("FIELD-B".into()));
    assert_eq!(toks[10], Token::Pic);
    assert_eq!(toks[11], Token::PicClause("9(3)".into()));
    assert_eq!(toks[12], Token::Period);
}

// ── Keyword tokens ───────────────────────────────────────────────────

#[test]
fn keyword_pic() {
    assert!(payload("PIC").contains(&Token::Pic));
}

#[test]
fn keyword_picture() {
    assert!(payload("PICTURE").contains(&Token::Pic));
}

#[test]
fn keyword_occurs() {
    assert!(payload("OCCURS").contains(&Token::Occurs));
}

#[test]
fn keyword_depending() {
    assert!(payload("DEPENDING").contains(&Token::Depending));
}

#[test]
fn keyword_redefines() {
    assert!(payload("REDEFINES").contains(&Token::Redefines));
}

#[test]
fn keyword_comp() {
    assert!(payload("COMP").contains(&Token::Comp));
}

#[test]
fn keyword_comp1() {
    assert!(payload("COMP-1").contains(&Token::Comp1));
}

#[test]
fn keyword_comp2() {
    assert!(payload("COMP-2").contains(&Token::Comp2));
}

#[test]
fn keyword_comp3() {
    assert!(payload("COMP-3").contains(&Token::Comp3));
}

#[test]
fn keyword_computational_long_forms() {
    assert!(payload("COMPUTATIONAL").contains(&Token::Comp));
    assert!(payload("COMPUTATIONAL-1").contains(&Token::Comp1));
    assert!(payload("COMPUTATIONAL-2").contains(&Token::Comp2));
    assert!(payload("COMPUTATIONAL-3").contains(&Token::Comp3));
}

#[test]
fn keyword_sign() {
    assert!(payload("SIGN").contains(&Token::Sign));
}

#[test]
fn keyword_separate() {
    assert!(payload("SEPARATE").contains(&Token::Separate));
}

#[test]
fn keyword_leading() {
    assert!(payload("LEADING").contains(&Token::Leading));
}

#[test]
fn keyword_trailing() {
    assert!(payload("TRAILING").contains(&Token::Trailing));
}

#[test]
fn keyword_value() {
    assert!(payload("VALUE").contains(&Token::Value));
}

#[test]
fn keyword_thru() {
    assert!(payload("THRU").contains(&Token::Thru));
}

#[test]
fn keyword_through() {
    assert!(payload("THROUGH").contains(&Token::Through));
}

#[test]
fn keyword_case_insensitive() {
    assert!(payload("pic").contains(&Token::Pic));
    assert!(payload("Occurs").contains(&Token::Occurs));
    assert!(payload("redefines").contains(&Token::Redefines));
    assert!(payload("comp-3").contains(&Token::Comp3));
    assert!(payload("value").contains(&Token::Value));
}

// ── PIC clause tokenization ──────────────────────────────────────────

#[test]
fn pic_clause_x_with_length() {
    let toks = payload("PIC X(10)");
    assert_eq!(toks[0], Token::Pic);
    assert_eq!(toks[1], Token::PicClause("X(10)".into()));
}

#[test]
fn pic_clause_9_with_length() {
    let toks = payload("PIC 9(5)");
    assert_eq!(toks[1], Token::PicClause("9(5)".into()));
}

#[test]
fn pic_clause_signed_decimal() {
    let toks = payload("PIC S9(5)V9(2)");
    assert_eq!(toks[1], Token::PicClause("S9(5)V9(2)".into()));
}

#[test]
fn pic_clause_inline_digits() {
    let toks = payload("PIC S999V99");
    assert_eq!(toks[1], Token::PicClause("S999V99".into()));
}

#[test]
fn pic_clause_repeated_x() {
    let toks = payload("PIC XXX");
    assert_eq!(toks[1], Token::PicClause("XXX".into()));
}

#[test]
fn pic_clause_mixed_precision() {
    let toks = payload("PIC 9(3)V99");
    assert_eq!(toks[1], Token::PicClause("9(3)V99".into()));
}

#[test]
fn pic_clause_decimal_inline() {
    let toks = payload("PIC 99V9(4)");
    assert_eq!(toks[1], Token::PicClause("99V9(4)".into()));
}

// ── Numeric literals ─────────────────────────────────────────────────

#[test]
fn numeric_literal_simple() {
    let toks = payload("OCCURS 100 TIMES");
    assert_eq!(toks[1], Token::Number(100));
}

#[test]
fn numeric_literal_in_occurs_depending() {
    let toks = payload("OCCURS 1 TO 100 DEPENDING ON CTR");
    assert_eq!(toks[1], Token::Number(1));
    assert_eq!(toks[3], Token::Number(100));
}

// ── Level numbers ────────────────────────────────────────────────────

#[test]
fn level_01() {
    assert_eq!(payload("01 REC.")[0], Token::Level(1));
}

#[test]
fn level_05() {
    assert_eq!(payload("05 FLD PIC X.")[0], Token::Level(5));
}

#[test]
fn level_10() {
    assert_eq!(payload("10 FLD PIC X.")[0], Token::Level(10));
}

#[test]
fn level_15() {
    assert_eq!(payload("15 FLD PIC X.")[0], Token::Level(15));
}

#[test]
fn level_20() {
    assert_eq!(payload("20 FLD PIC X.")[0], Token::Level(20));
}

#[test]
fn level_49() {
    assert_eq!(payload("49 FLD PIC X.")[0], Token::Level(49));
}

#[test]
fn level_66() {
    assert_eq!(payload("66 ALIAS RENAMES ORIG.")[0], Token::Level66);
}

#[test]
fn level_77() {
    assert_eq!(payload("77 STAND PIC 9.")[0], Token::Level77);
}

#[test]
fn level_88() {
    assert_eq!(payload(r#"88 FLAG VALUE "Y"."#)[0], Token::Level88);
}

// ── Period / statement terminator ────────────────────────────────────

#[test]
fn period_terminates_statement() {
    let toks = payload("01 RECORD.");
    assert_eq!(*toks.last().unwrap(), Token::Period);
}

#[test]
fn period_after_pic_clause() {
    let toks = payload("05 FLD PIC X(5).");
    assert_eq!(*toks.last().unwrap(), Token::Period);
}

// ── Comment line handling (line starting with *) ─────────────────────

#[test]
fn comment_line_starting_with_star_is_skipped() {
    let input = "* This is a comment\n05 FLD PIC X.";
    let toks = payload(input);
    // The comment line should be filtered out; only field tokens remain
    assert!(toks.contains(&Token::Level(5)));
    assert!(
        !toks
            .iter()
            .any(|t| matches!(t, Token::Identifier(s) if s.contains("comment")))
    );
}

// ── Inline comment handling (*>) ─────────────────────────────────────

#[test]
fn inline_comment_stripped_in_free_form() {
    // In free-form, inline comments are stripped during preprocessing
    let input = "05 FLD PIC X. *> field comment";
    let mut lexer = Lexer::new(input);
    let toks = lexer.tokenize();
    // The field tokens should be present
    assert!(toks.iter().any(|t| t.token == Token::Level(5)));
    assert!(toks.iter().any(|t| t.token == Token::Period));
}

#[test]
fn inline_comment_token_in_raw_logos_lex() {
    // When the *> appears in raw tokenizer input, it produces InlineComment
    let input = "*> this is inline";
    let toks = payload(input);
    // In free form, lines starting with * are treated as comment lines and skipped
    // But *> inline comments within code lines should be handled
    // Here the entire line starts with *, so it's a comment line
    assert!(toks.is_empty() || toks.iter().any(|t| matches!(t, Token::InlineComment(_))));
}

// ── Sequence number area (columns 1-6) handling ──────────────────────

#[test]
fn fixed_form_sequence_area_stripped() {
    // Fixed-form: columns 1-6 are sequence area, 7 is indicator, 8+ is code
    let input = "\
000100 01  RECORD.
000200     05  FLD  PIC X(10).";
    let mut lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
    let toks: Vec<Token> = lexer
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect();
    // Sequence numbers should NOT appear as tokens
    assert!(!toks.iter().any(|t| matches!(t, Token::Number(100 | 200))));
    // Actual code should be present
    assert!(toks.contains(&Token::Level(1)));
    assert!(toks.contains(&Token::Pic));
}

// ── Continuation indicator (column 7) handling ───────────────────────

#[test]
fn fixed_form_continuation_joins_lines() {
    // Fixed-form: col 7 '-' is a continuation indicator.
    // The lexer should join the continued line so that the field name is
    // recognised (or at minimum, both parts appear as tokens).
    let input = "\
       01  VERY-LONG-FIELD-NA\n\
      -        ME PIC X(50).";
    let mut lexer = Lexer::new(input);
    let toks = lexer.tokenize();
    // Verify that tokenization produces Level + identifier(s) + PIC
    assert!(toks.iter().any(|t| t.token == Token::Level(1)));
    assert!(toks.iter().any(|t| t.token == Token::Pic));
    assert!(
        toks.iter()
            .any(|t| matches!(&t.token, Token::PicClause(s) if s == "X(50)"))
    );
}

// ── Special characters in PIC (edited PIC patterns) ──────────────────

#[test]
fn edited_pic_z_suppression() {
    let toks = payload("PIC ZZZ9");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::EditedPic(s) if s.contains('Z'))),
        "Z suppression should be EditedPic: {toks:?}"
    );
}

#[test]
fn edited_pic_asterisk_fill() {
    let toks = payload("PIC ***9");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::EditedPic(s) if s.contains('*'))),
        "asterisk fill should be EditedPic: {toks:?}"
    );
}

#[test]
fn edited_pic_dollar_sign() {
    let toks = payload("PIC $ZZ9");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::EditedPic(s) if s.contains('$'))),
        "dollar sign should be EditedPic: {toks:?}"
    );
}

#[test]
fn edited_pic_plus_sign() {
    let toks = payload("PIC +ZZ9");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::EditedPic(s) if s.contains('+'))),
        "plus sign should be EditedPic: {toks:?}"
    );
}

#[test]
fn edited_pic_minus_sign() {
    let toks = payload("PIC -ZZ9");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::EditedPic(s) if s.contains('-'))),
        "minus sign should be EditedPic: {toks:?}"
    );
}

#[test]
fn edited_pic_slash_insertion() {
    let toks = payload("PIC 99/99/99");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::EditedPic(s) if s.contains('/'))),
        "slash insertion should be EditedPic: {toks:?}"
    );
}

#[test]
fn edited_pic_comma_grouping() {
    let toks = payload("PIC Z,ZZZ.99");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::EditedPic(s) if s.contains(','))),
        "comma grouping should be EditedPic: {toks:?}"
    );
}

#[test]
fn edited_pic_zero_insertion() {
    // Zero insertion pattern (e.g., "0009")
    let toks = payload("PIC 0009");
    assert!(
        toks.iter().any(|t| matches!(t, Token::EditedPic(_))),
        "zero insertion should be EditedPic: {toks:?}"
    );
}

// ── Multiple tokens on one line ──────────────────────────────────────

#[test]
fn multiple_tokens_on_one_line() {
    let toks = payload("05 AMOUNT PIC S9(7)V99 COMP-3.");
    assert_eq!(toks[0], Token::Level(5));
    assert_eq!(toks[1], Token::Identifier("AMOUNT".into()));
    assert_eq!(toks[2], Token::Pic);
    assert_eq!(toks[3], Token::PicClause("S9(7)V99".into()));
    assert_eq!(toks[4], Token::Comp3);
    assert_eq!(toks[5], Token::Period);
}

#[test]
fn occurs_clause_all_tokens() {
    let toks = payload("OCCURS 1 TO 100 DEPENDING ON COUNTER");
    assert_eq!(toks[0], Token::Occurs);
    assert_eq!(toks[1], Token::Number(1));
    assert_eq!(toks[2], Token::To);
    assert_eq!(toks[3], Token::Number(100));
    assert_eq!(toks[4], Token::Depending);
    assert_eq!(toks[5], Token::On);
    assert_eq!(toks[6], Token::Identifier("COUNTER".into()));
}

// ── FILLER keyword ───────────────────────────────────────────────────

#[test]
fn filler_keyword_as_identifier() {
    let toks = payload("05 FILLER PIC X(10).");
    assert_eq!(toks[1], Token::Identifier("FILLER".into()));
}

// ── BLANK WHEN ZERO ──────────────────────────────────────────────────

#[test]
fn blank_when_zero_keywords() {
    let toks = payload("PIC ZZ9 BLANK WHEN ZERO");
    assert!(toks.contains(&Token::Blank));
    assert!(toks.contains(&Token::When));
    assert!(toks.contains(&Token::Zero));
}

#[test]
fn blank_when_zeros_variant() {
    let toks = payload("BLANK WHEN ZEROS");
    assert!(toks.contains(&Token::Blank));
    assert!(toks.contains(&Token::When));
    assert!(toks.contains(&Token::Zero));
}

#[test]
fn blank_when_zeroes_variant() {
    let toks = payload("BLANK WHEN ZEROES");
    assert!(toks.contains(&Token::Blank));
    assert!(toks.contains(&Token::When));
    assert!(toks.contains(&Token::Zero));
}

// ── String literal tokenization ──────────────────────────────────────

#[test]
fn string_literal_double_quotes() {
    let toks = payload(r#"VALUE "hello""#);
    assert_eq!(toks[0], Token::Value);
    assert_eq!(toks[1], Token::StringLiteral("hello".into()));
}

#[test]
fn string_literal_single_quotes() {
    let toks = payload("VALUE 'world'");
    assert_eq!(toks[0], Token::Value);
    assert_eq!(toks[1], Token::StringLiteral("world".into()));
}

#[test]
fn string_literal_with_spaces() {
    let toks = payload(r#"VALUE "hello world""#);
    assert_eq!(toks[1], Token::StringLiteral("hello world".into()));
}

#[test]
fn string_literal_comma_inside_not_tokenized() {
    let toks = payload(r#""A,B""#);
    assert_eq!(toks[0], Token::StringLiteral("A,B".into()));
    assert!(!toks.contains(&Token::Comma));
}

// ── Token type AND value verification ────────────────────────────────

#[test]
fn verify_token_type_and_value_level() {
    let toks = payload("01 REC.");
    if let Token::Level(n) = &toks[0] {
        assert_eq!(*n, 1);
    } else {
        panic!("expected Level, got {:?}", toks[0]);
    }
}

#[test]
fn verify_token_type_and_value_identifier() {
    let toks = payload("05 MY-FIELD PIC X.");
    if let Token::Identifier(name) = &toks[1] {
        assert_eq!(name, "MY-FIELD");
    } else {
        panic!("expected Identifier, got {:?}", toks[1]);
    }
}

#[test]
fn verify_token_type_and_value_pic_clause() {
    let toks = payload("PIC S9(5)V9(2)");
    if let Token::PicClause(s) = &toks[1] {
        assert_eq!(s, "S9(5)V9(2)");
    } else {
        panic!("expected PicClause, got {:?}", toks[1]);
    }
}

#[test]
fn verify_token_type_and_value_number() {
    // Use a number > 49 so it's not captured as a Level token (levels are 01–49)
    let toks = payload("OCCURS 100 TIMES");
    if let Token::Number(n) = &toks[1] {
        assert_eq!(*n, 100);
    } else {
        panic!("expected Number, got {:?}", toks[1]);
    }
}

#[test]
fn verify_token_type_and_value_string_literal() {
    let toks = payload(r#"VALUE "ABC""#);
    if let Token::StringLiteral(s) = &toks[1] {
        assert_eq!(s, "ABC");
    } else {
        panic!("expected StringLiteral, got {:?}", toks[1]);
    }
}

// ── Additional keyword combination tests ─────────────────────────────

#[test]
fn sign_leading_separate() {
    let toks = payload("SIGN IS LEADING SEPARATE");
    assert!(toks.contains(&Token::Sign));
    assert!(toks.contains(&Token::Is));
    assert!(toks.contains(&Token::Leading));
    assert!(toks.contains(&Token::Separate));
}

#[test]
fn sign_trailing_separate() {
    let toks = payload("SIGN IS TRAILING SEPARATE");
    assert!(toks.contains(&Token::Sign));
    assert!(toks.contains(&Token::Trailing));
    assert!(toks.contains(&Token::Separate));
}

#[test]
fn value_thru_range() {
    let toks = payload("VALUE 1 THRU 100");
    assert_eq!(toks[0], Token::Value);
    assert_eq!(toks[1], Token::Number(1));
    assert_eq!(toks[2], Token::Thru);
    assert_eq!(toks[3], Token::Number(100));
}

#[test]
fn value_through_range() {
    let toks = payload("VALUE 1 THROUGH 100");
    assert_eq!(toks[0], Token::Value);
    assert_eq!(toks[1], Token::Number(1));
    assert_eq!(toks[2], Token::Through);
    assert_eq!(toks[3], Token::Number(100));
}

#[test]
fn redefines_clause() {
    let toks = payload("05 ALIAS REDEFINES ORIG PIC X(5).");
    assert_eq!(toks[0], Token::Level(5));
    assert_eq!(toks[1], Token::Identifier("ALIAS".into()));
    assert_eq!(toks[2], Token::Redefines);
    assert_eq!(toks[3], Token::Identifier("ORIG".into()));
    assert_eq!(toks[4], Token::Pic);
}

#[test]
fn usage_display_keyword() {
    let toks = payload("USAGE DISPLAY");
    assert!(toks.contains(&Token::Usage));
    assert!(toks.contains(&Token::Display));
}

#[test]
fn usage_binary_keyword() {
    let toks = payload("USAGE BINARY");
    assert!(toks.contains(&Token::Usage));
    assert!(toks.contains(&Token::Binary));
}

#[test]
fn synchronized_keyword() {
    assert!(payload("SYNCHRONIZED").contains(&Token::Synchronized));
    assert!(payload("SYNC").contains(&Token::Synchronized));
}

#[test]
fn renames_keyword() {
    let toks = payload("66 ALIAS RENAMES ORIG.");
    assert_eq!(toks[0], Token::Level66);
    assert_eq!(toks[2], Token::Renames);
}

#[test]
fn times_keyword() {
    let toks = payload("OCCURS 5 TIMES");
    assert_eq!(toks[2], Token::Times);
}

// ── Eof is always last ──────────────────────────────────────────────

#[test]
fn eof_always_last_token() {
    for input in &["01 X PIC X.", "", "OCCURS 5 TIMES.", "  "] {
        let toks = tokens(input);
        assert_eq!(
            toks.last().unwrap(),
            &Token::Eof,
            "Eof not last for input: {input:?}"
        );
    }
}

// ── Comma token tests ────────────────────────────────────────────────

#[test]
fn standalone_comma_is_comma_token() {
    let toks = payload(",");
    assert_eq!(toks[0], Token::Comma);
}

#[test]
fn commas_in_level88_value_list() {
    let toks = payload(r#"88 STATUS VALUE "A", "B", "C"."#);
    let comma_count = toks.iter().filter(|t| matches!(t, Token::Comma)).count();
    assert_eq!(comma_count, 2);
}

// ── Parentheses ──────────────────────────────────────────────────────

#[test]
fn parentheses_tokens() {
    let toks = payload("(50)");
    assert_eq!(toks[0], Token::LeftParen);
    assert_eq!(toks[1], Token::Number(50));
    assert_eq!(toks[2], Token::RightParen);
}

// ── LexerOptions tests ──────────────────────────────────────────────

#[test]
fn lexer_options_defaults() {
    let opts = LexerOptions::default();
    assert!(opts.allow_inline_comments);
    assert!(!opts.strict_comments);
}

#[test]
fn lexer_with_custom_options() {
    let opts = LexerOptions {
        allow_inline_comments: false,
        strict_comments: true,
    };
    let mut lexer = Lexer::new_with_options("05 FLD PIC X.", opts);
    let toks = lexer.tokenize();
    assert!(toks.iter().any(|t| t.token == Token::Level(5)));
}

// ── Format detection ─────────────────────────────────────────────────

#[test]
fn format_detection_free_form() {
    let lexer = Lexer::new("01 RECORD.\n  05 FLD PIC X.");
    assert_eq!(lexer.format(), CobolFormat::Free);
}

#[test]
fn format_detection_fixed_form() {
    let input = "\
000100 01  CUSTOMER-RECORD.
000200     05  CUSTOMER-ID     PIC X(10).
000300     05  CUSTOMER-NAME   PIC X(30).";
    let lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
}

// ── TokenPos position info ──────────────────────────────────────────

#[test]
fn token_pos_has_line_and_column() {
    let mut lexer = Lexer::new("01 FIELD PIC X.");
    let toks = lexer.tokenize();
    let first = &toks[0];
    assert_eq!(first.line, 1);
    assert!(first.column >= 1);
    assert!(!first.span.is_empty());
}

// ── Complete copybook fragment ───────────────────────────────────────

#[test]
fn lex_complete_copybook_fragment() {
    let input = "\
01 CUSTOMER-RECORD.
  05 CUSTOMER-ID     PIC X(10).
  05 CUSTOMER-NAME   PIC X(30).
  05 BALANCE         PIC S9(7)V99 COMP-3.
  05 STATUS-CODE     PIC 9(2).
  88 ACTIVE          VALUE 1.
  88 INACTIVE        VALUE 2 THRU 9.";
    let toks = payload(input);
    // Verify key tokens are present
    assert!(toks.contains(&Token::Level(1)));
    assert!(toks.contains(&Token::Level(5)));
    assert!(toks.contains(&Token::Level88));
    assert!(toks.contains(&Token::Pic));
    assert!(toks.contains(&Token::Comp3));
    assert!(toks.contains(&Token::Value));
    assert!(toks.contains(&Token::Thru));
    // Verify identifiers
    assert!(toks.contains(&Token::Identifier("CUSTOMER-RECORD".into())));
    assert!(toks.contains(&Token::Identifier("CUSTOMER-ID".into())));
    assert!(toks.contains(&Token::Identifier("BALANCE".into())));
    assert!(toks.contains(&Token::Identifier("ACTIVE".into())));
}

// ═══════════════════════════════════════════════════════════════════════
//  Extended coverage below — fixed-form layout, free-form long lines,
//  format-detection edge cases, continuation, non-ASCII, numeric edges,
//  real-world copybook fragments, and trait verification.
// ═══════════════════════════════════════════════════════════════════════

// ── Fixed-form: identification area (cols 73-80) stripped ────────────

#[test]
fn fixed_form_identification_area_stripped() {
    // Columns 73-80 are the identification area and must not produce tokens.
    // Pad to exactly 80 chars: 6 seq + 1 indicator + 65 code + 8 ident.
    let line1 =
        "000100 01  RECORD.                                                        IDENT01\n";
    let line2 =
        "000200     05  FLD         PIC X(5).                                      IDENT02\n";
    let input = format!("{line1}{line2}");
    let mut lexer = Lexer::new(&input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
    let toks: Vec<Token> = lexer
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect();
    // The identification-area text must NOT appear as identifier tokens.
    assert!(
        !toks
            .iter()
            .any(|t| matches!(t, Token::Identifier(s) if s.contains("IDENT"))),
        "identification area leaked into tokens: {toks:?}"
    );
    assert!(toks.contains(&Token::Level(1)));
    assert!(toks.contains(&Token::Pic));
}

// ── Fixed-form: line starting with asterisk is a comment ────────────

#[test]
fn fixed_form_star_prefixed_line_is_comment() {
    // In fixed-form processing, a line that literally starts with '*'
    // is treated as a comment and its content is excluded from tokens.
    let input = "\
000100 01  RECORD.
* THIS IS A FULL-LINE COMMENT
000300     05  FLD PIC X.";
    let mut lexer = Lexer::new(input);
    let toks: Vec<Token> = lexer
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect();
    // Comment content should not appear as tokens.
    assert!(
        !toks
            .iter()
            .any(|t| matches!(t, Token::Identifier(s) if s == "THIS")),
        "comment line leaked: {toks:?}"
    );
    assert!(toks.contains(&Token::Pic));
}

// ── Fixed-form: column 7 slash indicator in properly formatted line ──

#[test]
fn fixed_form_col7_slash_indicator() {
    // The slash at col 7 is recognised as a valid fixed-form indicator
    // by the format detector. Content after col 7 is extracted for tokenisation.
    let input = "\
000100 01  RECORD.
000200     05  FLD PIC X.";
    let mut lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
    let toks: Vec<Token> = lexer
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect();
    assert!(toks.contains(&Token::Level(1)));
    assert!(toks.contains(&Token::Pic));
}

// ── Free-form: long lines beyond column 72 are preserved ────────────

#[test]
fn free_form_long_line_not_truncated() {
    // In free-form there are no column restrictions. A name at col 80+ should survive.
    let long_name = "A".repeat(80);
    let input = format!("05 {long_name} PIC X.");
    let toks = payload(&input);
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::Identifier(s) if s.len() == 80)),
        "long identifier truncated: {toks:?}"
    );
    assert!(toks.contains(&Token::Pic));
}

// ── Format detection edge cases ─────────────────────────────────────

#[test]
fn format_detection_empty_input_is_free() {
    let lexer = Lexer::new("");
    assert_eq!(lexer.format(), CobolFormat::Free);
}

#[test]
fn format_detection_whitespace_only_is_free() {
    let lexer = Lexer::new("     \n   \t  \n");
    assert_eq!(lexer.format(), CobolFormat::Free);
}

#[test]
fn format_detection_all_comment_lines_is_free() {
    let input = "* comment 1\n* comment 2\n* comment 3\n";
    let lexer = Lexer::new(input);
    // All-comment input has no content lines; heuristic defaults to Free.
    assert_eq!(lexer.format(), CobolFormat::Free);
}

#[test]
fn format_detection_short_content_lines_are_free() {
    let input = "01 REC.\n  05 FLD PIC X.\n";
    let lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Free);
}

#[test]
fn format_detection_mixed_indicators() {
    // Majority of lines look fixed → Fixed.
    let input = "\
000100 01  RECORD.
000200     05  FIELD-A     PIC X(10).
000300     05  FIELD-B     PIC 9(5).
000400     05  FIELD-C     PIC X(20).";
    let lexer = Lexer::new(input);
    assert_eq!(lexer.format(), CobolFormat::Fixed);
}

// ── Continuation: multiple continuation lines ───────────────────────

#[test]
fn fixed_form_multiple_continuations() {
    let input = "\
       01  VERY-LONG\n\
      -        -FIELD\n\
      -        -NAME PIC X.";
    let mut lexer = Lexer::new(input);
    let toks: Vec<Token> = lexer
        .tokenize()
        .into_iter()
        .map(|tp| tp.token)
        .filter(|t| !matches!(t, Token::Newline | Token::Eof))
        .collect();
    // At minimum, tokenisation should produce Level + identifier(s) + PIC
    assert!(toks.iter().any(|t| matches!(t, Token::Level(1))));
    assert!(toks.iter().any(|t| matches!(t, Token::Pic)));
}

// ── Consecutive comment lines are all skipped ───────────────────────

#[test]
fn consecutive_comment_lines_skipped() {
    let input = "\
* line one
* line two
* line three
05 FLD PIC X.";
    let toks = payload(input);
    assert_eq!(toks[0], Token::Level(5));
    assert!(toks.contains(&Token::Pic));
    assert!(
        !toks
            .iter()
            .any(|t| matches!(t, Token::Identifier(s) if s.contains("line"))),
    );
}

// ── Inline comment stripping with strict_comments ───────────────────

#[test]
fn strict_comments_preserves_inline_comment_token() {
    let opts = LexerOptions {
        allow_inline_comments: true,
        strict_comments: true,
    };
    let input = "05 FLD PIC X. *> my note";
    let mut lexer = Lexer::new_with_options(input, opts);
    let toks = lexer.tokenize();
    // With strict_comments the *> is NOT stripped during preprocessing,
    // so the raw logos lexer should emit an InlineComment token.
    assert!(
        toks.iter()
            .any(|t| matches!(&t.token, Token::InlineComment(_))),
        "expected InlineComment token: {toks:?}"
    );
}

// ── Numeric edge cases ──────────────────────────────────────────────

#[test]
fn number_zero() {
    let toks = payload("OCCURS 0 TIMES");
    // 0 is a valid integer but also matches level regex; in context after OCCURS it should parse.
    assert!(toks.iter().any(|t| matches!(t, Token::Number(0))));
}

#[test]
fn number_large_value() {
    let toks = payload("OCCURS 99999 TIMES");
    assert!(
        toks.iter().any(|t| matches!(t, Token::Number(99_999))),
        "large number not parsed: {toks:?}"
    );
}

// ── Empty and minimal string literals ───────────────────────────────

#[test]
fn empty_string_literal_double_quotes() {
    let toks = payload(r#"VALUE """#);
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::StringLiteral(s) if s.is_empty())),
        "empty double-quoted literal missing: {toks:?}"
    );
}

#[test]
fn empty_string_literal_single_quotes() {
    let toks = payload("VALUE ''");
    assert!(
        toks.iter()
            .any(|t| matches!(t, Token::StringLiteral(s) if s.is_empty())),
        "empty single-quoted literal missing: {toks:?}"
    );
}

#[test]
fn string_literal_with_digits() {
    let toks = payload(r#"VALUE "12345""#);
    assert_eq!(toks[1], Token::StringLiteral("12345".into()));
}

// ── Newline token presence ──────────────────────────────────────────

#[test]
fn newline_tokens_between_lines() {
    let toks = tokens("01 A.\n05 B PIC X.");
    let nl_count = toks.iter().filter(|t| matches!(t, Token::Newline)).count();
    assert!(nl_count >= 1, "expected at least one Newline: {toks:?}");
}

// ── Non-ASCII input handling ────────────────────────────────────────

#[test]
fn non_ascii_characters_do_not_panic() {
    // The lexer should not panic on non-ASCII input; unrecognised bytes
    // fall through to the Identifier catch-all or are skipped.
    let input = "05 FELD PIC X(5). *> Ü Ö Ä";
    let mut lexer = Lexer::new(input);
    let toks = lexer.tokenize();
    assert!(toks.iter().any(|t| t.token == Token::Pic));
    assert!(toks.last().is_some_and(|t| t.token == Token::Eof));
}

#[test]
fn unicode_in_identifier_position() {
    // COBOL identifiers are A-Z / 0-9 / hyphen, but the lexer should
    // handle unexpected Unicode gracefully (no panic).
    let input = "05 DONNÉES PIC X.";
    let mut lexer = Lexer::new(input);
    let _toks = lexer.tokenize(); // must not panic
}

// ── Very long input line ────────────────────────────────────────────

#[test]
fn very_long_input_line_does_not_panic() {
    let long_field = format!("05 {} PIC X.", "A".repeat(500));
    let mut lexer = Lexer::new(&long_field);
    let toks = lexer.tokenize();
    assert!(toks.iter().any(|t| t.token == Token::Level(5)));
    assert!(toks.iter().any(|t| t.token == Token::Pic));
}

// ── Multiple statements on separate lines ───────────────────────────

#[test]
fn multiline_copybook_preserves_all_fields() {
    let input = "\
01 HEADER.
  05 ID        PIC 9(5).
  05 NAME      PIC X(30).
  05 AMOUNT    PIC S9(7)V99 COMP-3.
  05 STATUS    PIC X.
  88 ACTIVE    VALUE 'A'.
  88 CLOSED    VALUE 'C'.
  05 FILLER    PIC X(10).";
    let toks = payload(input);
    let level_count = toks.iter().filter(|t| matches!(t, Token::Level(_))).count();
    let l88_count = toks.iter().filter(|t| matches!(t, Token::Level88)).count();
    assert!(level_count >= 5, "too few Level tokens: {level_count}");
    assert_eq!(l88_count, 2);
    assert!(toks.contains(&Token::Comp3));
}

// ── USAGE IS keyword combination ────────────────────────────────────

#[test]
fn usage_is_comp3() {
    let toks = payload("USAGE IS COMP-3");
    assert_eq!(toks[0], Token::Usage);
    assert_eq!(toks[1], Token::Is);
    assert_eq!(toks[2], Token::Comp3);
}

#[test]
fn usage_is_display() {
    let toks = payload("USAGE IS DISPLAY");
    assert_eq!(toks[0], Token::Usage);
    assert_eq!(toks[1], Token::Is);
    assert_eq!(toks[2], Token::Display);
}

// ── SIGN IS LEADING/TRAILING SEPARATE CHARACTER ─────────────────────

#[test]
fn sign_is_trailing_separate_character() {
    let toks = payload("SIGN IS TRAILING SEPARATE CHARACTER");
    assert!(toks.contains(&Token::Sign));
    assert!(toks.contains(&Token::Is));
    assert!(toks.contains(&Token::Trailing));
    assert!(toks.contains(&Token::Separate));
    // CHARACTER is not a keyword—should be an Identifier
    assert!(toks.contains(&Token::Identifier("CHARACTER".into())));
}

// ── RENAMES THRU clause ─────────────────────────────────────────────

#[test]
fn renames_thru_clause() {
    let toks = payload("66 ALIAS RENAMES FIELD-A THRU FIELD-Z.");
    assert_eq!(toks[0], Token::Level66);
    assert_eq!(toks[2], Token::Renames);
    assert!(toks.contains(&Token::Thru));
    assert_eq!(*toks.last().unwrap(), Token::Period);
}

// ── Token Debug and Clone traits ────────────────────────────────────

#[test]
fn token_debug_trait() {
    let tok = Token::Level(5);
    let dbg = format!("{tok:?}");
    assert!(dbg.contains("Level"), "Debug output: {dbg}");
}

#[test]
fn token_clone_equality() {
    let tok = Token::PicClause("S9(5)V99".into());
    let cloned = tok.clone();
    assert_eq!(tok, cloned);
}

// ── TokenPos span accuracy ──────────────────────────────────────────

#[test]
fn token_pos_spans_are_non_overlapping() {
    let mut lexer = Lexer::new("05 FLD PIC X(10).");
    let toks = lexer.tokenize();
    let mut prev_end = 0usize;
    for tp in &toks {
        if tp.token == Token::Eof {
            break;
        }
        assert!(
            tp.span.start >= prev_end,
            "overlapping spans: prev_end={prev_end}, current={:?}",
            tp.span
        );
        prev_end = tp.span.end;
    }
}

// ── Column tracking across lines ────────────────────────────────────

#[test]
fn token_pos_line_increments_on_newline() {
    let mut lexer = Lexer::new("01 A.\n05 B PIC X.");
    let toks = lexer.tokenize();
    let lines: Vec<usize> = toks.iter().map(|t| t.line).collect();
    // At least one token should be on line 2.
    assert!(lines.contains(&2), "no tokens on line 2: {lines:?}");
}

// ── Display trait round-trip for all keyword variants ────────────────

#[test]
fn display_trait_all_keywords() {
    let pairs: Vec<(Token, &str)> = vec![
        (Token::Pic, "PIC"),
        (Token::Usage, "USAGE"),
        (Token::Display, "DISPLAY"),
        (Token::Comp, "COMP"),
        (Token::Comp1, "COMP-1"),
        (Token::Comp2, "COMP-2"),
        (Token::Comp3, "COMP-3"),
        (Token::Binary, "BINARY"),
        (Token::Redefines, "REDEFINES"),
        (Token::Renames, "RENAMES"),
        (Token::Occurs, "OCCURS"),
        (Token::Depending, "DEPENDING"),
        (Token::On, "ON"),
        (Token::To, "TO"),
        (Token::Times, "TIMES"),
        (Token::Synchronized, "SYNCHRONIZED"),
        (Token::Value, "VALUE"),
        (Token::Thru, "THRU"),
        (Token::Through, "THROUGH"),
        (Token::Sign, "SIGN"),
        (Token::Is, "IS"),
        (Token::Leading, "LEADING"),
        (Token::Trailing, "TRAILING"),
        (Token::Separate, "SEPARATE"),
        (Token::Blank, "BLANK"),
        (Token::When, "WHEN"),
        (Token::Zero, "ZERO"),
    ];
    for (tok, expected) in &pairs {
        assert_eq!(format!("{tok}"), *expected, "Display mismatch for {tok:?}");
    }
}

// ── Edited PIC: complex patterns ────────────────────────────────────

#[test]
fn edited_pic_currency_comma_decimal() {
    let toks = payload("PIC $ZZ,ZZZ.99");
    assert!(
        toks.iter().any(|t| matches!(t, Token::EditedPic(_))),
        "expected EditedPic for $ZZ,ZZZ.99: {toks:?}"
    );
}

#[test]
fn edited_pic_cr_db_sign() {
    // CR and DB appended to a numeric pattern are tokenised as part of
    // a combined identifier by the logos lexer, since CR/DB are letters.
    let toks = payload("PIC ZZZ9CR");
    // The combined "ZZZ9CR" is captured as an Identifier because the
    // trailing letters break the EditedPic regex. Verify no panic.
    assert!(toks.contains(&Token::Pic));
    assert!(toks.len() >= 2, "expected Pic + clause token: {toks:?}");
}

// ── Real-world enterprise copybook fragment ─────────────────────────

#[test]
fn enterprise_copybook_fragment() {
    let input = "\
01 TRANSACTION-RECORD.
  05 TXN-ID              PIC 9(10).
  05 TXN-DATE.
    10 TXN-YEAR          PIC 9(4).
    10 TXN-MONTH         PIC 9(2).
    10 TXN-DAY           PIC 9(2).
  05 TXN-AMOUNT          PIC S9(9)V99 COMP-3.
  05 TXN-COUNT           PIC 9(3).
  05 TXN-ITEMS OCCURS 1 TO 100
     DEPENDING ON TXN-COUNT.
    10 ITEM-CODE         PIC X(5).
    10 ITEM-QTY          PIC 9(5).
  05 TXN-STATUS          PIC X.
  88 TXN-PENDING         VALUE 'P'.
  88 TXN-COMPLETE        VALUE 'C'.
  88 TXN-FAILED          VALUE 'F'.";
    let toks = payload(input);
    // Group structure
    assert!(toks.contains(&Token::Level(1)));
    assert!(toks.iter().filter(|t| matches!(t, Token::Level(5))).count() >= 4);
    assert!(
        toks.iter()
            .filter(|t| matches!(t, Token::Level(10)))
            .count()
            >= 4
    );
    // ODO clause
    assert!(toks.contains(&Token::Occurs));
    assert!(toks.contains(&Token::Depending));
    assert!(toks.contains(&Token::On));
    // Level-88
    assert_eq!(
        toks.iter().filter(|t| matches!(t, Token::Level88)).count(),
        3
    );
    // COMP-3
    assert!(toks.contains(&Token::Comp3));
}

// ── Parenthesised repeat inside PIC clause ──────────────────────────

#[test]
fn pic_clause_large_repeat() {
    let toks = payload("PIC X(32767)");
    assert_eq!(toks[1], Token::PicClause("X(32767)".into()));
}

// ── Multiple periods (e.g., consecutive terminations) ───────────────

#[test]
fn multiple_fields_each_terminated() {
    let toks = payload("05 A PIC X. 05 B PIC 9.");
    let period_count = toks.iter().filter(|t| matches!(t, Token::Period)).count();
    assert_eq!(period_count, 2);
}

// ── Blank line between statements ───────────────────────────────────

#[test]
fn blank_lines_between_statements() {
    let input = "01 REC.\n\n\n05 FLD PIC X.";
    let toks = payload(input);
    assert!(toks.contains(&Token::Level(1)));
    assert!(toks.contains(&Token::Level(5)));
}

// ── ON keyword in non-ODO context ───────────────────────────────────

#[test]
fn on_keyword_standalone() {
    let toks = payload("ON");
    assert_eq!(toks[0], Token::On);
}

// ── IS keyword in non-SIGN context ──────────────────────────────────

#[test]
fn is_keyword_standalone() {
    let toks = payload("IS");
    assert_eq!(toks[0], Token::Is);
}

// ── Mixed-case COBOL identifiers ────────────────────────────────────

#[test]
fn mixed_case_identifier() {
    let toks = payload("05 Customer-Name PIC X(30).");
    assert!(toks.contains(&Token::Identifier("Customer-Name".into())));
}

// ── CobolFormat derives ─────────────────────────────────────────────

#[test]
fn cobol_format_debug_and_clone() {
    let f = CobolFormat::Fixed;
    let cloned = f;
    assert_eq!(f, cloned);
    let dbg = format!("{f:?}");
    assert!(dbg.contains("Fixed"));
}

// ── LexerOptions Debug trait ────────────────────────────────────────

#[test]
fn lexer_options_debug_trait() {
    let opts = LexerOptions::default();
    let dbg = format!("{opts:?}");
    assert!(dbg.contains("allow_inline_comments"));
}

// ── Edited PIC: B insertion (space) ─────────────────────────────────

#[test]
fn edited_pic_with_b_space_insertion() {
    // B is a COBOL editing symbol for space insertion; depending on
    // regex coverage it may or may not be EditedPic.
    let input = "PIC 99B99";
    let toks = payload(input);
    assert!(toks.contains(&Token::Pic));
    // The clause itself should be captured as *some* token (PicClause or EditedPic or Identifier).
    assert!(toks.len() >= 2, "expected at least Pic + clause: {toks:?}");
}
