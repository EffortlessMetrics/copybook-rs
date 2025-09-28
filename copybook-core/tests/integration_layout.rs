#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Integration tests for layout resolution

use copybook_core::{FieldKind, Occurs, parse_copybook};

#[test]
fn test_comp3_parsing_debug() {
    let input = "01 BALANCE PIC S9(7)V99 COMP-3.";
    println!("Testing input: {input}");

    let mut lexer = copybook_core::lexer::Lexer::new(input);
    let tokens = lexer.tokenize();

    println!("Tokens:");
    for (i, token_pos) in tokens.iter().enumerate() {
        println!("  {}: {:?}", i, token_pos.token);
    }

    let schema = parse_copybook(input).unwrap();
    let field = &schema.fields[0];
    println!(
        "Field: name={}, kind={:?}, len={}",
        field.name, field.kind, field.len
    );

    // Should be PackedDecimal, not ZonedDecimal
    assert!(matches!(field.kind, FieldKind::PackedDecimal { .. }));
}

#[test]
fn test_complex_layout_resolution() {
    let copybook = r"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID PIC X(10).
   05 CUSTOMER-NAME.
      10 FIRST-NAME PIC X(20).
      10 LAST-NAME PIC X(30).
   05 BALANCE PIC S9(7)V99 COMP-3.
   05 ACCOUNT-COUNT PIC 9(3).
   05 ACCOUNTS PIC X(15) OCCURS 5 TIMES DEPENDING ON ACCOUNT-COUNT.
";

    let schema = parse_copybook(copybook).unwrap();

    // Verify root structure
    assert_eq!(schema.fields.len(), 1);
    let root = &schema.fields[0];
    assert_eq!(root.name, "CUSTOMER-RECORD");
    assert_eq!(root.offset, 0);

    // Verify children
    assert_eq!(root.children.len(), 5);

    // Check CUSTOMER-ID
    let customer_id = &root.children[0];
    assert_eq!(customer_id.name, "CUSTOMER-ID");
    assert_eq!(customer_id.offset, 0);
    assert_eq!(customer_id.len, 10);

    // Check CUSTOMER-NAME group
    let customer_name = &root.children[1];
    assert_eq!(customer_name.name, "CUSTOMER-NAME");
    assert_eq!(customer_name.offset, 10);
    assert!(matches!(customer_name.kind, FieldKind::Group));

    // Check nested fields
    assert_eq!(customer_name.children.len(), 2);
    let first_name = &customer_name.children[0];
    assert_eq!(first_name.name, "FIRST-NAME");
    assert_eq!(first_name.offset, 10);
    assert_eq!(first_name.len, 20);

    let last_name = &customer_name.children[1];
    assert_eq!(last_name.name, "LAST-NAME");
    assert_eq!(last_name.offset, 30);
    assert_eq!(last_name.len, 30);

    // Check BALANCE (packed decimal)
    let balance = &root.children[2];
    assert_eq!(balance.name, "BALANCE");
    assert_eq!(balance.offset, 60);
    assert_eq!(balance.len, 5); // S9(7)V99 = 9 digits = ceil((9+1)/2) = 5 bytes
    assert!(matches!(
        balance.kind,
        FieldKind::PackedDecimal {
            digits: 9,
            scale: 2,
            signed: true
        }
    ));

    // Check ACCOUNT-COUNT
    let account_count = &root.children[3];
    assert_eq!(account_count.name, "ACCOUNT-COUNT");
    assert_eq!(account_count.offset, 65);
    assert_eq!(account_count.len, 3);

    // Check ACCOUNTS (ODO array)
    let accounts = &root.children[4];
    assert_eq!(accounts.name, "ACCOUNTS");
    assert_eq!(accounts.offset, 68);
    assert_eq!(accounts.len, 15); // Base field size
    assert!(matches!(
        accounts.occurs,
        Some(Occurs::ODO { min: 5, max: 5, .. })
    ));

    // Should have tail ODO info
    assert!(schema.tail_odo.is_some());
    let tail_odo = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail_odo.counter_path, "ACCOUNT-COUNT");
    assert_eq!(tail_odo.max_count, 5);

    // Should have fixed LRECL since ODO min equals max (effectively fixed-size)
    assert_eq!(schema.lrecl_fixed, Some(143));

    // Verify schema fingerprint is generated
    assert!(!schema.fingerprint.is_empty());
    assert_eq!(schema.fingerprint.len(), 64); // SHA-256 hex string
}

#[test]
fn test_synchronized_binary_alignment() {
    let copybook = r"
01 RECORD-WITH-ALIGNMENT.
   05 CHAR-FIELD PIC X(1).
   05 BINARY-FIELD PIC 9(5) USAGE COMP SYNCHRONIZED.
   05 ANOTHER-CHAR PIC X(3).
   05 ANOTHER-BINARY PIC 9(9) USAGE COMP SYNCHRONIZED.
";

    let schema = parse_copybook(copybook).unwrap();

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 4);

    // CHAR-FIELD at offset 0
    let char_field = &root.children[0];
    assert_eq!(char_field.offset, 0);
    assert_eq!(char_field.len, 1);
    assert!(char_field.sync_padding.is_none());

    // BINARY-FIELD should be aligned to 4-byte boundary (32-bit)
    let binary_field = &root.children[1];
    assert_eq!(binary_field.offset, 4); // Aligned from 1 to 4
    assert_eq!(binary_field.len, 4); // 32-bit = 4 bytes
    assert_eq!(binary_field.sync_padding, Some(3)); // 3 padding bytes
    assert!(binary_field.synchronized);

    // ANOTHER-CHAR immediately after binary field
    let another_char = &root.children[2];
    assert_eq!(another_char.offset, 8);
    assert_eq!(another_char.len, 3);

    // ANOTHER-BINARY should be aligned to 4-byte boundary (32-bit for 9 digits)
    let another_binary = &root.children[3];
    assert_eq!(another_binary.offset, 12); // Aligned from 11 to 12
    assert_eq!(another_binary.len, 4); // 32-bit = 4 bytes
    assert_eq!(another_binary.sync_padding, Some(1)); // 1 padding byte
    assert!(another_binary.synchronized);

    // Should have fixed LRECL
    assert_eq!(schema.lrecl_fixed, Some(16)); // 12 + 4
}

#[test]
fn test_redefines_with_different_sizes() {
    let copybook = r"
01 ORIGINAL-FIELD PIC X(20).
01 SHORT-REDEFINES REDEFINES ORIGINAL-FIELD PIC 9(10).
01 LONG-REDEFINES REDEFINES ORIGINAL-FIELD PIC X(30).
01 NEXT-FIELD PIC X(5).
";

    let schema = parse_copybook(copybook).unwrap();

    assert_eq!(schema.fields.len(), 4);

    // All REDEFINES should have same offset
    assert_eq!(schema.fields[0].offset, 0);
    assert_eq!(schema.fields[1].offset, 0);
    assert_eq!(schema.fields[2].offset, 0);

    // NEXT-FIELD should be after the largest REDEFINES variant
    let next_field = &schema.fields[3];
    assert_eq!(next_field.offset, 30); // After the 30-byte LONG-REDEFINES
    assert_eq!(next_field.len, 5);

    // Total record size should account for largest variant
    assert_eq!(schema.lrecl_fixed, Some(35)); // 30 + 5
}
