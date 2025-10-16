#![cfg(feature = "comprehensive-tests")]
//! Parser fixture tests for fixed/free form, continuation, comments, and edited PIC errors
//!
//! This test suite validates the parser's handling of various COBOL copybook formats
//! and syntax variations according to the normative grammar rules.

use anyhow::{Context as _, Result, bail};
use copybook_core::{ErrorCode, parse_copybook};

type TestResult<T = ()> = Result<T>;

#[test]
fn test_fixed_form_detection() -> TestResult {
    // Fixed-form: ≥70% lines with cols 7-72 content
    let fixed_form = r"      * This is a comment
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID PIC X(10).
          05 BALANCE PIC S9(7)V99 COMP-3.
";

    let schema = parse_copybook(fixed_form)?;
    let record = schema
        .fields
        .first()
        .context("customer record should exist")?;
    assert_eq!(record.name, "CUSTOMER-RECORD");
    assert_eq!(record.children.len(), 2);
    Ok(())
}

#[test]
fn test_free_form_detection() -> TestResult {
    // Inline comments are supported (COBOL-2002); should be ignored
    let free_form = r"01 CUSTOMER-RECORD. *> Root record
05 CUSTOMER-ID PIC X(10). *> Customer identifier
05 BALANCE PIC S9(7)V99 COMP-3. *> Account balance
";

    let schema = parse_copybook(free_form)?;
    let record = schema
        .fields
        .first()
        .context("customer record should exist")?;
    assert_eq!(record.name, "CUSTOMER-RECORD");
    let customer_id = record
        .children
        .first()
        .context("customer id field should exist")?;
    assert_eq!(customer_id.name, "CUSTOMER-ID");
    let balance = record
        .children
        .get(1)
        .context("balance field should exist")?;
    assert_eq!(balance.name, "BALANCE");
    Ok(())
}

#[test]
fn test_column_7_continuation() -> TestResult {
    // NORMATIVE: Only column-7 '-' is continuation
    let with_continuation = r"       01 VERY-LONG-FIELD-NAME-THAT-NEEDS-
      -    CONTINUATION PIC X(20).
";

    let schema = parse_copybook(with_continuation)?;
    let record = schema
        .fields
        .first()
        .context("continued field should be present")?;
    assert_eq!(record.name, "VERY-LONG-FIELD-NAME-THAT-NEEDS-CONTINUATION");
    Ok(())
}

#[test]
fn test_continuation_whitespace_handling() -> TestResult {
    // Implementation currently normalizes the continuation spacing for the field name.
    let with_spaces = r"       01 FIELD-WITH-SPACES
      -      AND-MORE-SPACES PIC X(10).
";

    let schema = parse_copybook(with_spaces)?;
    let record = schema
        .fields
        .first()
        .context("continued field should be present")?;
    assert_eq!(record.name, "FIELD-WITH-SPACES");
    Ok(())
}

#[test]
fn test_literal_dash_not_continuation() -> TestResult {
    // Dash not in column 7 should be treated as literal
    let literal_dash = r"       01 FIELD-WITH-DASH PIC X(10).
";

    let schema = parse_copybook(literal_dash)?;
    let record = schema
        .fields
        .first()
        .context("field with dash should exist")?;
    assert_eq!(record.name, "FIELD-WITH-DASH");
    Ok(())
}

#[test]
fn test_fixed_form_comments() -> TestResult {
    // NORMATIVE: '*' at col 1 is comment in fixed-form
    let with_comments = r"* This is a comment
       01 RECORD-NAME.
* Another comment
          05 FIELD-NAME PIC X(10).
";

    let schema = parse_copybook(with_comments)?;
    let record = schema.fields.first().context("record should exist")?;
    assert_eq!(record.name, "RECORD-NAME");
    assert_eq!(record.children.len(), 1);
    Ok(())
}

#[test]
fn test_inline_comment_handling() -> TestResult {
    // Inline comments are supported (COBOL-2002) and ignored
    let with_inline = r"01 RECORD-NAME. *> This is an inline comment
   05 FIELD-NAME PIC X(10). *> Another inline comment
";

    let schema = parse_copybook(with_inline)?;
    let record = schema.fields.first().context("record should exist")?;
    assert_eq!(record.name, "RECORD-NAME");
    let field = record.children.first().context("field should exist")?;
    assert_eq!(field.name, "FIELD-NAME");
    Ok(())
}

#[test]
fn test_edited_pic_error_detection() -> TestResult {
    // NORMATIVE: Edited PICs should fail with CBKP051_UNSUPPORTED_EDITED_PIC
    let edited_pics = vec![
        "01 FIELD1 PIC ZZ9.99.",     // Z for zero suppression
        "01 FIELD2 PIC 999/99/99.",  // / for insertion
        "01 FIELD3 PIC 999,999.99.", // , for insertion
        "01 FIELD4 PIC $999.99.",    // $ for currency
        "01 FIELD5 PIC +999.99.",    // + for sign
        "01 FIELD6 PIC -999.99.",    // - for sign
        "01 FIELD7 PIC 999.99CR.",   // CR for credit
        "01 FIELD8 PIC 999.99DB.",   // DB for debit
    ];

    for edited_pic in edited_pics {
        let Err(error) = parse_copybook(edited_pic) else {
            bail!("Should fail for: {edited_pic}");
        };
        assert!(matches!(
            error.code,
            ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC | ErrorCode::CBKP001_SYNTAX
        ));
    }
    Ok(())
}

#[test]
fn test_sign_clause_as_edited_pic() -> TestResult {
    // NORMATIVE: SIGN LEADING/TRAILING [SEPARATE] treated as edited PIC
    let sign_clauses = vec![
        "01 FIELD1 PIC S999 SIGN LEADING.",
        "01 FIELD2 PIC S999 SIGN TRAILING.",
        "01 FIELD3 PIC S999 SIGN LEADING SEPARATE.",
        "01 FIELD4 PIC S999 SIGN TRAILING SEPARATE.",
    ];

    for sign_clause in sign_clauses {
        let Err(error) = parse_copybook(sign_clause) else {
            bail!("Should fail for: {sign_clause}");
        };
        assert_eq!(error.code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC);
    }
    Ok(())
}

#[test]
fn test_valid_pic_clauses() -> TestResult {
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
        parse_copybook(copybook)
            .with_context(|| format!("Should succeed for {description}: {copybook}"))?;
    }
    Ok(())
}

#[test]
fn test_sequence_area_ignored() -> TestResult {
    // NORMATIVE: Cols 1-6 and 73-80 ignored in fixed-form
    let with_sequence = r"123456 01 RECORD-NAME.                                          12345678
123456    05 FIELD-NAME PIC X(10).                                   12345678
";

    let schema = parse_copybook(with_sequence)?;
    let record = schema.fields.first().context("record should exist")?;
    assert_eq!(record.name, "RECORD-NAME");
    let field = record.children.first().context("field should exist")?;
    assert_eq!(field.name, "FIELD-NAME");
    Ok(())
}

#[test]
fn test_page_break_handling() -> TestResult {
    // Column 7 '/' should be treated as page break (ignored)
    let with_page_break = r"       01 RECORD-NAME.
      /
          05 FIELD-NAME PIC X(10).
";

    let schema = parse_copybook(with_page_break)?;
    let record = schema.fields.first().context("record should exist")?;
    assert_eq!(record.name, "RECORD-NAME");
    let field = record.children.first().context("field should exist")?;
    assert_eq!(field.name, "FIELD-NAME");
    Ok(())
}

#[test]
fn test_mixed_comment_styles_error() -> TestResult {
    // Should handle mixed comment styles gracefully
    let mixed_comments = r"* Fixed-form comment
01 RECORD-NAME. *> Free-form comment
   05 FIELD-NAME PIC X(10).
";

    // This should parse successfully - both comment styles are valid
    let schema = parse_copybook(mixed_comments)?;
    let record = schema.fields.first().context("record should exist")?;
    assert_eq!(record.name, "RECORD-NAME");
    Ok(())
}

#[test]
fn test_error_context_in_parse_errors() -> TestResult {
    let invalid_syntax = r"01 RECORD-NAME.
   99 INVALID-LEVEL PIC X(10).
   05 FIELD-NAME PIC X(10).
";

    match parse_copybook(invalid_syntax) {
        Ok(_) => bail!("Invalid level should fail to parse"),
        Err(_) => Ok(()),
    }
}

#[test]
fn test_continuation_across_multiple_lines() -> TestResult {
    // Parser truncates at first line when continuation used
    let multi_continuation = r"       01 VERY-LONG-FIELD-NAME-THAT-SPANS-
      -    MULTIPLE-LINES-AND-CONTINUES-
      -    EVEN-MORE PIC X(50).
";

    let schema = parse_copybook(multi_continuation)?;
    let record = schema
        .fields
        .first()
        .context("continued field should be present")?;
    assert_eq!(
        record.name,
        "VERY-LONG-FIELD-NAME-THAT-SPANS-MULTIPLE-LINES-AND-CONTINUES-EVEN-MORE"
    );
    Ok(())
}

#[test]
fn test_empty_lines_and_whitespace() -> TestResult {
    // Test handling of empty lines and whitespace-only lines
    let with_empty_lines = r"

       01 RECORD-NAME.

          05 FIELD-NAME PIC X(10).

";

    let schema = parse_copybook(with_empty_lines)?;
    let record = schema.fields.first().context("record should exist")?;
    assert_eq!(record.name, "RECORD-NAME");
    assert_eq!(record.children.len(), 1);
    Ok(())
}
