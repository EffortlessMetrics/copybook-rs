//! Comprehensive parser tests covering all normative grammar rules and edge cases
//!
//! This test suite validates the parser's handling of various COBOL copybook formats
//! according to the normative grammar rules specified in the design document.

use copybook_core::{
    ErrorCode, FieldKind, Occurs, ParseOptions, parse_copybook, parse_copybook_with_options,
};

#[test]
fn test_fixed_form_vs_free_form_detection() {
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
fn test_column_7_continuation_normative() {
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

    // Test whitespace handling: strip trailing/leading spaces, preserve interior
    let with_spaces = r#"       01 FIELD-WITH-SPACES   
      -      AND-MORE-SPACES PIC X(10).
"#;

    let schema = parse_copybook(with_spaces).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "FIELD-WITH-SPACES AND-MORE-SPACES");

    // Dash not in column 7 should be treated as literal
    let literal_dash = r#"       01 FIELD-WITH-DASH PIC X(10).
"#;

    let schema = parse_copybook(literal_dash).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "FIELD-WITH-DASH");
}

#[test]
fn test_comment_handling_normative() {
    // NORMATIVE: '*' at col 1 is comment in fixed-form
    let fixed_comments = r#"* This is a comment
       01 RECORD-NAME.
* Another comment
          05 FIELD-NAME PIC X(10).
"#;

    let schema = parse_copybook(fixed_comments).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "RECORD-NAME");
    assert_eq!(schema.fields[0].children.len(), 1);

    // NORMATIVE: '*>' inline comments in free-form
    let inline_comments = r#"01 RECORD-NAME. *> This is an inline comment
   05 FIELD-NAME PIC X(10). *> Another inline comment
"#;

    let schema = parse_copybook(inline_comments).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "RECORD-NAME");
    assert_eq!(schema.fields[0].children.len(), 1);
}

#[test]
fn test_edited_pic_error_normative() {
    // NORMATIVE: Edited PICs should fail with CBKP051_UNSUPPORTED_EDITED_PIC
    let edited_pics = vec![
        ("01 FIELD1 PIC ZZ9.99.", "Z for zero suppression"),
        ("01 FIELD2 PIC 999/99/99.", "/ for insertion"),
        ("01 FIELD3 PIC 999,999.99.", ", for insertion"),
        ("01 FIELD4 PIC $999.99.", "$ for currency"),
        ("01 FIELD5 PIC +999.99.", "+ for sign"),
        ("01 FIELD6 PIC -999.99.", "- for sign"),
        ("01 FIELD7 PIC 999.99CR.", "CR for credit"),
        ("01 FIELD8 PIC 999.99DB.", "DB for debit"),
        ("01 FIELD9 PIC ***9.99.", "* for asterisk fill"),
        ("01 FIELD10 PIC BBB9.99.", "B for blank insertion"),
        ("01 FIELD11 PIC 999.99-.", "Trailing sign"),
        ("01 FIELD12 PIC +999.99+.", "Leading and trailing sign"),
    ];

    for (edited_pic, description) in edited_pics {
        let result = parse_copybook(edited_pic);
        assert!(
            result.is_err(),
            "Should fail for {}: {}",
            description,
            edited_pic
        );

        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC);
        assert!(error.message.contains("edited PIC"));
    }
}

#[test]
fn test_sign_clause_as_edited_pic_normative() {
    // NORMATIVE: SIGN LEADING/TRAILING [SEPARATE] treated as edited PIC
    let sign_clauses = vec![
        ("01 FIELD1 PIC S999 SIGN LEADING.", "SIGN LEADING"),
        ("01 FIELD2 PIC S999 SIGN TRAILING.", "SIGN TRAILING"),
        (
            "01 FIELD3 PIC S999 SIGN LEADING SEPARATE.",
            "SIGN LEADING SEPARATE",
        ),
        (
            "01 FIELD4 PIC S999 SIGN TRAILING SEPARATE.",
            "SIGN TRAILING SEPARATE",
        ),
    ];

    for (sign_clause, description) in sign_clauses {
        let result = parse_copybook(sign_clause);
        assert!(
            result.is_err(),
            "Should fail for {}: {}",
            description,
            sign_clause
        );

        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC);
    }
}

#[test]
fn test_valid_pic_clauses_comprehensive() {
    // These should parse successfully
    let valid_pics = vec![
        (
            "01 FIELD1 PIC X(10).",
            "Alphanum",
            FieldKind::Alphanum { len: 10 },
        ),
        (
            "01 FIELD2 PIC 9(5).",
            "Zoned unsigned",
            FieldKind::ZonedDecimal {
                digits: 5,
                scale: 0,
                signed: false,
            },
        ),
        (
            "01 FIELD3 PIC S9(5).",
            "Zoned signed",
            FieldKind::ZonedDecimal {
                digits: 5,
                scale: 0,
                signed: true,
            },
        ),
        (
            "01 FIELD4 PIC 9(3)V99.",
            "Zoned with scale",
            FieldKind::ZonedDecimal {
                digits: 5,
                scale: 2,
                signed: false,
            },
        ),
        (
            "01 FIELD5 PIC S9(3)V99.",
            "Zoned signed with scale",
            FieldKind::ZonedDecimal {
                digits: 5,
                scale: 2,
                signed: true,
            },
        ),
        (
            "01 FIELD6 PIC 9(5) COMP.",
            "Binary",
            FieldKind::BinaryInt {
                bits: 32,
                signed: false,
            },
        ),
        (
            "01 FIELD7 PIC S9(5) COMP.",
            "Binary signed",
            FieldKind::BinaryInt {
                bits: 32,
                signed: true,
            },
        ),
        (
            "01 FIELD8 PIC 9(5) COMP-3.",
            "Packed",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 0,
                signed: false,
            },
        ),
        (
            "01 FIELD9 PIC S9(5) COMP-3.",
            "Packed signed",
            FieldKind::PackedDecimal {
                digits: 5,
                scale: 0,
                signed: true,
            },
        ),
    ];

    for (copybook, description, expected_kind) in valid_pics {
        let result = parse_copybook(copybook);
        assert!(
            result.is_ok(),
            "Should succeed for {}: {}",
            description,
            copybook
        );

        let schema = result.unwrap();
        assert_eq!(schema.fields.len(), 1);

        // Compare field kinds (simplified comparison)
        match (&schema.fields[0].kind, &expected_kind) {
            (FieldKind::Alphanum { len: a }, FieldKind::Alphanum { len: b }) => assert_eq!(a, b),
            (
                FieldKind::ZonedDecimal {
                    digits: d1,
                    scale: s1,
                    signed: sg1,
                },
                FieldKind::ZonedDecimal {
                    digits: d2,
                    scale: s2,
                    signed: sg2,
                },
            ) => {
                assert_eq!(d1, d2);
                assert_eq!(s1, s2);
                assert_eq!(sg1, sg2);
            }
            (
                FieldKind::BinaryInt {
                    bits: b1,
                    signed: s1,
                },
                FieldKind::BinaryInt {
                    bits: b2,
                    signed: s2,
                },
            ) => {
                assert_eq!(b1, b2);
                assert_eq!(s1, s2);
            }
            (
                FieldKind::PackedDecimal {
                    digits: d1,
                    scale: s1,
                    signed: sg1,
                },
                FieldKind::PackedDecimal {
                    digits: d2,
                    scale: s2,
                    signed: sg2,
                },
            ) => {
                assert_eq!(d1, d2);
                assert_eq!(s1, s2);
                assert_eq!(sg1, sg2);
            }
            _ => panic!(
                "Field kind mismatch for {}: expected {:?}, got {:?}",
                description, expected_kind, schema.fields[0].kind
            ),
        }
    }
}

#[test]
fn test_sequence_area_ignored_normative() {
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
fn test_duplicate_name_disambiguation_normative() {
    // NORMATIVE: Sibling fields with identical names get __dup2, __dup3 suffixes
    let with_duplicates = r#"
01 RECORD-NAME.
   05 FIELD-NAME PIC X(5).
   05 FIELD-NAME PIC X(5).
   05 FIELD-NAME PIC X(5).
   05 OTHER-FIELD PIC X(5).
   05 OTHER-FIELD PIC X(5).
"#;

    let schema = parse_copybook(with_duplicates).unwrap();
    assert_eq!(schema.fields.len(), 1);

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 5);

    // Check duplicate name handling
    assert_eq!(root.children[0].name, "FIELD-NAME");
    assert_eq!(root.children[1].name, "FIELD-NAME__dup2");
    assert_eq!(root.children[2].name, "FIELD-NAME__dup3");
    assert_eq!(root.children[3].name, "OTHER-FIELD");
    assert_eq!(root.children[4].name, "OTHER-FIELD__dup2");
}

#[test]
fn test_filler_field_naming_normative() {
    // NORMATIVE: FILLER fields get _filler_<offset> names when emitted
    let with_filler = r#"
01 RECORD-NAME.
   05 FIELD1 PIC X(5).
   05 FILLER PIC X(3).
   05 FIELD2 PIC X(5).
   05 FILLER PIC X(2).
"#;

    let schema = parse_copybook_with_options(
        with_filler,
        &ParseOptions {
            emit_filler: true,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(schema.fields.len(), 1);

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 4);

    // Check FILLER naming (offset-based)
    assert_eq!(root.children[0].name, "FIELD1");
    assert_eq!(root.children[1].name, "_filler_00000005"); // Offset 5
    assert_eq!(root.children[1].path, "RECORD-NAME._filler_00000005");
    assert_eq!(root.children[2].name, "FIELD2");
    assert_eq!(root.children[3].name, "_filler_00000013"); // Offset 13
    assert_eq!(root.children[3].path, "RECORD-NAME._filler_00000013");
}

#[test]
fn test_multiple_filler_fields_distinct() {
    // Test that multiple FILLER fields get unique names based on their offsets
    let with_multiple_fillers = r#"
01 RECORD-NAME.
   05 FIELD1 PIC X(2).
   05 FILLER PIC X(1).
   05 FIELD2 PIC X(3).
   05 FILLER PIC X(2).
   05 FIELD3 PIC X(1).
   05 FILLER PIC X(4).
"#;

    let schema = parse_copybook_with_options(
        with_multiple_fillers,
        &ParseOptions {
            emit_filler: true,
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(schema.fields.len(), 1);

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 6);

    // Check all FILLER fields have distinct names based on their offsets
    assert_eq!(root.children[0].name, "FIELD1");
    assert_eq!(root.children[1].name, "_filler_00000002"); // Offset 2
    assert_eq!(root.children[1].path, "RECORD-NAME._filler_00000002");
    assert_eq!(root.children[2].name, "FIELD2");
    assert_eq!(root.children[3].name, "_filler_00000006"); // Offset 6
    assert_eq!(root.children[3].path, "RECORD-NAME._filler_00000006");
    assert_eq!(root.children[4].name, "FIELD3");
    assert_eq!(root.children[5].name, "_filler_00000009"); // Offset 9
    assert_eq!(root.children[5].path, "RECORD-NAME._filler_00000009");
}

#[test]
fn test_occurs_fixed_arrays() {
    let copybook = r#"
01 RECORD-WITH-ARRAYS.
   05 SIMPLE-ARRAY OCCURS 5 TIMES PIC X(3).
   05 NESTED-GROUP OCCURS 3 TIMES.
      10 SUB-FIELD PIC 9(2).
      10 SUB-ARRAY OCCURS 2 TIMES PIC X(1).
"#;

    let schema = parse_copybook(copybook).unwrap();
    assert_eq!(schema.fields.len(), 1);

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 2);

    // Check simple array
    let simple_array = &root.children[0];
    assert_eq!(simple_array.name, "SIMPLE-ARRAY");
    assert!(matches!(
        simple_array.occurs,
        Some(Occurs::Fixed { count: 5 })
    ));

    // Check nested group array
    let nested_group = &root.children[1];
    assert_eq!(nested_group.name, "NESTED-GROUP");
    assert!(matches!(
        nested_group.occurs,
        Some(Occurs::Fixed { count: 3 })
    ));
    assert_eq!(nested_group.children.len(), 2);

    // Check nested array within group
    let sub_array = &nested_group.children[1];
    assert_eq!(sub_array.name, "SUB-ARRAY");
    assert!(matches!(sub_array.occurs, Some(Occurs::Fixed { count: 2 })));
}

#[test]
fn test_odo_validation_normative() {
    // NORMATIVE: ODO only at tail, not nested under another ODO
    let valid_odo = r#"
01 RECORD-WITH-ODO.
   05 COUNTER PIC 9(3).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(5).
"#;

    let schema = parse_copybook(valid_odo).unwrap();
    assert_eq!(schema.fields.len(), 1);

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 2);

    let odo_array = &root.children[1];
    assert_eq!(odo_array.name, "VARIABLE-ARRAY");
    assert!(matches!(
        odo_array.occurs,
        Some(Occurs::ODO {
            min: 1,
            max: 10,
            ..
        })
    ));

    // Invalid: ODO not at tail
    let invalid_odo_not_tail = r#"
01 RECORD-WITH-BAD-ODO.
   05 COUNTER PIC 9(3).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(5).
   05 TRAILING-FIELD PIC X(3).
"#;

    let result = parse_copybook(invalid_odo_not_tail);
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL);

    // Invalid: Nested ODO
    let invalid_nested_odo = r#"
01 RECORD-WITH-NESTED-ODO.
   05 OUTER-COUNTER PIC 9(3).
   05 OUTER-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON OUTER-COUNTER.
      10 INNER-COUNTER PIC 9(2).
      10 INNER-ARRAY OCCURS 1 TO 3 TIMES DEPENDING ON INNER-COUNTER PIC X(2).
"#;

    let result = parse_copybook(invalid_nested_odo);
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

#[test]
fn test_redefines_validation() {
    let valid_redefines = r#"
01 RECORD-WITH-REDEFINES.
   05 ORIGINAL-FIELD PIC X(10).
   05 REDEFINING-FIELD REDEFINES ORIGINAL-FIELD PIC 9(10).
   05 ANOTHER-REDEFINE REDEFINES ORIGINAL-FIELD.
      10 PART1 PIC X(5).
      10 PART2 PIC X(5).
"#;

    let schema = parse_copybook(valid_redefines).unwrap();
    assert_eq!(schema.fields.len(), 1);

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 3);

    // Check redefines relationships
    assert_eq!(root.children[0].name, "ORIGINAL-FIELD");
    assert!(root.children[0].redefines_of.is_none());

    assert_eq!(root.children[1].name, "REDEFINING-FIELD");
    assert_eq!(
        root.children[1].redefines_of,
        Some("ORIGINAL-FIELD".to_string())
    );

    assert_eq!(root.children[2].name, "ANOTHER-REDEFINE");
    assert_eq!(
        root.children[2].redefines_of,
        Some("ORIGINAL-FIELD".to_string())
    );

    // Invalid: Redefines non-existent field
    let invalid_redefines = r#"
01 RECORD-WITH-BAD-REDEFINES.
   05 FIELD1 PIC X(5).
   05 FIELD2 REDEFINES NON-EXISTENT PIC X(5).
"#;

    let result = parse_copybook(invalid_redefines);
    assert!(result.is_err());
    // Should get a semantic error about missing redefines target
}

#[test]
fn test_synchronized_alignment() {
    let copybook = r#"
01 RECORD-WITH-SYNC.
   05 CHAR-FIELD PIC X(1).
   05 BINARY-FIELD PIC 9(5) USAGE COMP SYNCHRONIZED.
   05 ANOTHER-CHAR PIC X(3).
   05 ANOTHER-BINARY PIC 9(9) USAGE COMP SYNCHRONIZED.
"#;

    let schema = parse_copybook(copybook).unwrap();
    assert_eq!(schema.fields.len(), 1);

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 4);

    // Check alignment
    let char_field = &root.children[0];
    assert_eq!(char_field.offset, 0);
    assert_eq!(char_field.len, 1);
    assert!(!char_field.synchronized);

    let binary_field = &root.children[1];
    assert_eq!(binary_field.offset, 4); // Aligned to 4-byte boundary
    assert_eq!(binary_field.len, 4); // 32-bit binary
    assert!(binary_field.synchronized);
    assert_eq!(binary_field.sync_padding, Some(3)); // 3 padding bytes

    let another_char = &root.children[2];
    assert_eq!(another_char.offset, 8); // After binary field
    assert_eq!(another_char.len, 3);

    let another_binary = &root.children[3];
    assert_eq!(another_binary.offset, 16); // Aligned to 8-byte boundary (64-bit)
    assert_eq!(another_binary.len, 8); // 64-bit binary
    assert!(another_binary.synchronized);
    assert_eq!(another_binary.sync_padding, Some(5)); // 5 padding bytes
}

#[test]
fn test_error_context_with_line_numbers() {
    // Test that parse errors include proper line numbers and context
    let invalid_syntax = r#"01 RECORD-NAME.
   05 INVALID-LEVEL-99 PIC X(10).
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

#[test]
fn test_binary_width_mapping_normative() {
    // Test NORMATIVE binary width mapping: ≤4→2B, 5-9→4B, 10-18→8B
    let test_cases = vec![
        ("01 BIN1 PIC 9(1) COMP.", 2, 16),  // ≤4 digits → 2 bytes, 16 bits
        ("01 BIN2 PIC 9(4) COMP.", 2, 16),  // ≤4 digits → 2 bytes, 16 bits
        ("01 BIN3 PIC 9(5) COMP.", 4, 32),  // 5-9 digits → 4 bytes, 32 bits
        ("01 BIN4 PIC 9(9) COMP.", 4, 32),  // 5-9 digits → 4 bytes, 32 bits
        ("01 BIN5 PIC 9(10) COMP.", 8, 64), // 10-18 digits → 8 bytes, 64 bits
        ("01 BIN6 PIC 9(18) COMP.", 8, 64), // 10-18 digits → 8 bytes, 64 bits
    ];

    for (copybook, expected_len, expected_bits) in test_cases {
        let schema = parse_copybook(copybook).unwrap();
        let field = &schema.fields[0];

        assert_eq!(field.len, expected_len, "Length mismatch for: {}", copybook);

        if let FieldKind::BinaryInt { bits, .. } = &field.kind {
            assert_eq!(*bits, expected_bits, "Bit width mismatch for: {}", copybook);
        } else {
            panic!("Expected BinaryInt for: {}", copybook);
        }
    }
}

#[test]
fn test_explicit_binary_width_normative() {
    // Test NORMATIVE explicit USAGE BINARY(n) for n ∈ {1,2,4,8}
    let copybook = r#"
01 EXPLICIT-BINARY.
   05 BIN1 PIC 9(3) USAGE BINARY(1).
   05 BIN2 PIC 9(5) USAGE BINARY(2).
   05 BIN4 PIC 9(9) USAGE BINARY(4).
   05 BIN8 PIC 9(18) USAGE BINARY(8).
"#;

    let schema = parse_copybook(copybook).unwrap();

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 4);

    // Verify explicit widths override digit-based calculation
    assert_eq!(root.children[0].len, 1); // BINARY(1)
    assert_eq!(root.children[1].len, 2); // BINARY(2)
    assert_eq!(root.children[2].len, 4); // BINARY(4)
    assert_eq!(root.children[3].len, 8); // BINARY(8)

    // Verify bit widths
    for (i, expected_bits) in [8, 16, 32, 64].iter().enumerate() {
        if let FieldKind::BinaryInt { bits, .. } = &root.children[i].kind {
            assert_eq!(*bits, *expected_bits);
        } else {
            panic!("Expected BinaryInt for child {}", i);
        }
    }
}

#[test]
fn test_blank_when_zero_parsing() {
    let copybook = r#"
01 RECORD-WITH-BWZ.
   05 NORMAL-FIELD PIC 9(5).
   05 BWZ-FIELD PIC 9(5) BLANK WHEN ZERO.
   05 BWZ-SIGNED PIC S9(3)V99 BLANK WHEN ZERO.
"#;

    let schema = parse_copybook(copybook).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 3);

    assert!(!root.children[0].blank_when_zero);
    assert!(root.children[1].blank_when_zero);
    assert!(root.children[2].blank_when_zero);
}

#[test]
fn test_schema_fingerprinting() {
    let copybook = r#"
01 RECORD-FOR-FINGERPRINT.
   05 FIELD1 PIC X(10).
   05 FIELD2 PIC 9(5) COMP.
"#;

    let schema = parse_copybook(copybook).unwrap();

    // Schema should have a non-empty fingerprint
    assert!(!schema.fingerprint.is_empty());

    // Parsing the same copybook should produce the same fingerprint
    let schema2 = parse_copybook(copybook).unwrap();
    assert_eq!(schema.fingerprint, schema2.fingerprint);

    // Different copybook should produce different fingerprint
    let different_copybook = r#"
01 DIFFERENT-RECORD.
   05 FIELD1 PIC X(5).
   05 FIELD2 PIC 9(3) COMP-3.
"#;

    let schema3 = parse_copybook(different_copybook).unwrap();
    assert_ne!(schema.fingerprint, schema3.fingerprint);
}
