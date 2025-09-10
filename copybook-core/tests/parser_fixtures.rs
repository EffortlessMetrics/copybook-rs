//! Parser fixture tests for fixed/free form, continuation, comments, and edited PIC errors
//!
//! This test suite validates the parser's handling of various COBOL copybook formats
//! and syntax variations according to the normative grammar rules.

use copybook_core::{ErrorCode, parse_copybook};

#[test]
fn test_fixed_form_detection() {
    // Fixed-form: ≥70% lines with cols 7-72 content
    let fixed_form = r#"      * This is a comment
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID PIC X(10).
          05 BALANCE PIC S9(7)V99 COMP-3.
"#;

    let schema = parse_copybook(fixed_form).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "CUSTOMER-RECORD");
    assert_eq!(schema.fields[0].children.len(), 2);
}

#[test]
fn test_free_form_detection() {
    // Free-form: inline *> comments, no column restrictions
    let free_form = r#"01 CUSTOMER-RECORD. *> Root record
05 CUSTOMER-ID PIC X(10). *> Customer identifier
05 BALANCE PIC S9(7)V99 COMP-3. *> Account balance
"#;

    let schema = parse_copybook(free_form).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "CUSTOMER-RECORD");
    assert_eq!(schema.fields[0].children.len(), 2);
}

#[test]
fn test_column_7_continuation() {
    // NORMATIVE: Only column-7 '-' is continuation
    let with_continuation = r#"       01 VERY-LONG-FIELD-NAME-THAT-NEEDS-
      -    CONTINUATION PIC X(20).
"#;

    let schema = parse_copybook(with_continuation).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(
        schema.fields[0].name,
        "VERY-LONG-FIELD-NAME-THAT-NEEDS-CONTINUATION"
    );
}

#[test]
fn test_continuation_whitespace_handling() {
    // NORMATIVE: Strip trailing/leading spaces, preserve interior whitespace
    let with_spaces = r#"       01 FIELD-WITH-SPACES   
      -      AND-MORE-SPACES PIC X(10).
"#;

    let schema = parse_copybook(with_spaces).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "FIELD-WITH-SPACES AND-MORE-SPACES");
}

#[test]
fn test_literal_dash_not_continuation() {
    // Dash not in column 7 should be treated as literal
    let literal_dash = r#"       01 FIELD-WITH-DASH PIC X(10).
"#;

    let schema = parse_copybook(literal_dash).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "FIELD-WITH-DASH");
}

#[test]
fn test_fixed_form_comments() {
    // NORMATIVE: '*' at col 1 is comment in fixed-form
    let with_comments = r#"* This is a comment
       01 RECORD-NAME.
* Another comment
          05 FIELD-NAME PIC X(10).
"#;

    let schema = parse_copybook(with_comments).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "RECORD-NAME");
    assert_eq!(schema.fields[0].children.len(), 1);
}

#[test]
fn test_inline_comment_handling() {
    // NORMATIVE: '*>' inline comments in free-form
    let with_inline = r#"01 RECORD-NAME. *> This is an inline comment
   05 FIELD-NAME PIC X(10). *> Another inline comment
"#;

    let schema = parse_copybook(with_inline).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "RECORD-NAME");
    assert_eq!(schema.fields[0].children.len(), 1);
}

#[test]
fn test_edited_pic_error_detection() {
    // NORMATIVE: Edited PICs should fail with CBKP051_UNSUPPORTED_EDITED_PIC
    let edited_pics = vec![
        "01 FIELD1 PIC ZZ9.99.",     // Z for zero suppression
        "01 FIELD2 PIC 999/99/99.",  // / for insertion
        "01 FIELD3 PIC 999,999.99.", // , for insertion
        "01 FIELD5 PIC +999.99.",    // + for sign
        "01 FIELD6 PIC -999.99.",    // - for sign
    ];

    for edited_pic in edited_pics {
        let result = parse_copybook(edited_pic);
        assert!(result.is_err(), "Should fail for: {}", edited_pic);

        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC);
    }
}

#[test]
fn test_sign_clause_as_edited_pic() {
    // NORMATIVE: SIGN LEADING/TRAILING [SEPARATE] treated as edited PIC
    let sign_clauses = vec![
        "01 FIELD1 PIC S999 SIGN LEADING.",
        "01 FIELD2 PIC S999 SIGN TRAILING.",
        "01 FIELD3 PIC S999 SIGN LEADING SEPARATE.",
        "01 FIELD4 PIC S999 SIGN TRAILING SEPARATE.",
    ];

    for sign_clause in sign_clauses {
        let result = parse_copybook(sign_clause);
        assert!(result.is_err(), "Should fail for: {}", sign_clause);

        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC);
    }
}

#[test]
fn test_valid_pic_clauses() {
    // These should parse successfully
    let valid_pics = vec![
        ("01 FIELD1 PIC X(10).", "Alphanum"),
        ("01 FIELD2 PIC 9(5).", "Zoned unsigned"),
        ("01 FIELD3 PIC S9(5).", "Zoned signed"),
        ("01 FIELD4 PIC 9(3)V99.", "Zoned with scale"),
        ("01 FIELD5 PIC S9(3)V99.", "Zoned signed with scale"),
        ("01 FIELD6 PIC 9(5) COMP.", "Binary"),
        ("01 FIELD7 PIC S9(5) COMP.", "Binary signed"),
        ("01 FIELD8 PIC 9(5) COMP-3.", "Packed"),
        ("01 FIELD9 PIC S9(5) COMP-3.", "Packed signed"),
    ];

    for (copybook, description) in valid_pics {
        let result = parse_copybook(copybook);
        assert!(
            result.is_ok(),
            "Should succeed for {}: {}",
            description,
            copybook
        );
    }
}

#[test]
fn test_sequence_area_ignored() {
    // NORMATIVE: Cols 1-6 and 73-80 ignored in fixed-form
    let with_sequence = r#"123456 01 RECORD-NAME.                                          12345678
123456    05 FIELD-NAME PIC X(10).                                   12345678
"#;

    let schema = parse_copybook(with_sequence).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "RECORD-NAME");
    assert_eq!(schema.fields[0].children.len(), 1);
    assert_eq!(schema.fields[0].children[0].name, "FIELD-NAME");
}

#[test]
fn test_page_break_handling() {
    // Column 7 '/' should be treated as page break (ignored)
    let with_page_break = r#"       01 RECORD-NAME.
      /
          05 FIELD-NAME PIC X(10).
"#;

    let schema = parse_copybook(with_page_break).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "RECORD-NAME");
    assert_eq!(schema.fields[0].children.len(), 1);
}

#[test]
fn test_mixed_comment_styles_error() {
    // Should handle mixed comment styles gracefully
    let mixed_comments = r#"* Fixed-form comment
01 RECORD-NAME. *> Free-form comment
   05 FIELD-NAME PIC X(10).
"#;

    // This should parse successfully - both comment styles are valid
    let schema = parse_copybook(mixed_comments).unwrap();
    assert_eq!(schema.fields.len(), 1);
}

#[test]
fn test_error_context_in_parse_errors() {
    // Test that parse errors include proper line numbers and context
    let invalid_syntax = r#"01 RECORD-NAME.
   99 INVALID-LEVEL PIC X(10).
   05 FIELD-NAME PIC X(10).
"#;

    let result = parse_copybook(invalid_syntax);
    assert!(result.is_err());

    let error = result.unwrap_err();
    let context = &error.context;
    assert!(context.is_some());

    let ctx = context.as_ref().unwrap();
    assert!(ctx.line_number.is_some());
    assert_eq!(ctx.line_number.unwrap(), 2); // Error on line 2
}

#[test]
fn test_continuation_across_multiple_lines() {
    // Test continuation across multiple lines
    let multi_continuation = r#"       01 VERY-LONG-FIELD-NAME-THAT-SPANS-
      -    MULTIPLE-LINES-AND-CONTINUES-
      -    EVEN-MORE PIC X(50).
"#;

    let schema = parse_copybook(multi_continuation).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(
        schema.fields[0].name,
        "VERY-LONG-FIELD-NAME-THAT-SPANS-MULTIPLE-LINES-AND-CONTINUES-EVEN-MORE"
    );
}

#[test]
fn test_empty_lines_and_whitespace() {
    // Test handling of empty lines and whitespace-only lines
    let with_empty_lines = r#"

       01 RECORD-NAME.

          05 FIELD-NAME PIC X(10).

"#;

    let schema = parse_copybook(with_empty_lines).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "RECORD-NAME");
    assert_eq!(schema.fields[0].children.len(), 1);
}
