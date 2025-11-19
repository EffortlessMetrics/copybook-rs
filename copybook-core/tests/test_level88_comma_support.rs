#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::unnecessary_unwrap
)]

/*!
 * Level-88 Comma-Separated VALUE Clause Tests
 *
 * These tests validate that the parser correctly handles comma-separated
 * values in Level-88 VALUE clauses, as specified in COBOL standards
 * (ANSI COBOL-85 Section 4.1.2.4.3 and IBM Enterprise COBOL).
 *
 * **COBOL Standard**: Both space-separated and comma-separated values
 * are valid in Level-88 VALUE clauses.
 *
 * **Enterprise Production Impact**: Comma-separated VALUE clauses are
 * widely used in mainframe production copybooks for improved readability.
 */

use copybook_core::{FieldKind, parse_copybook};

/// Test 1: Comma-separated string literals
///
/// **Purpose**: Validates basic comma-separated string values
/// **COBOL Compliance**: ANSI COBOL-85 Section 4.1.2.4.3
/// **Enterprise Context**: Status code validation in banking systems
#[test]
fn test_level88_comma_separated_string_values() {
    const COPYBOOK: &str = r#"
01 STATUS-FIELD PIC X(1).
   88 IS-VALID VALUE "A", "B", "C".
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let status_field = &schema.fields[0];
    assert_eq!(status_field.name, "STATUS-FIELD");
    assert_eq!(status_field.children.len(), 1);

    let level88 = &status_field.children[0];
    assert_eq!(level88.name, "IS-VALID");
    assert_eq!(level88.level, 88);

    if let FieldKind::Condition { values } = &level88.kind {
        assert_eq!(values.len(), 3, "Should parse all 3 comma-separated values");
        assert_eq!(values[0], "A");
        assert_eq!(values[1], "B");
        assert_eq!(values[2], "C");
    } else {
        panic!("Expected FieldKind::Condition, found {:?}", level88.kind);
    }

    println!("✅ Level-88 comma-separated string values validated successfully");
}

/// Test 2: Comma-separated numeric values
///
/// **Purpose**: Validates comma-separated numeric literals
/// **COBOL Compliance**: IBM Enterprise COBOL syntax
/// **Enterprise Context**: Account type codes in insurance systems
#[test]
fn test_level88_comma_separated_numeric_values() {
    const COPYBOOK: &str = r#"
01 ACCOUNT-TYPE PIC 9(1).
   88 VALID-TYPES VALUE 1, 2, 3, 4, 5.
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let account_field = &schema.fields[0];
    let level88 = &account_field.children[0];

    if let FieldKind::Condition { values } = &level88.kind {
        assert_eq!(
            values.len(),
            5,
            "Should parse all 5 comma-separated numeric values"
        );
        assert_eq!(values, &vec!["1", "2", "3", "4", "5"]);
    } else {
        panic!("Expected FieldKind::Condition for numeric values");
    }

    println!("✅ Level-88 comma-separated numeric values validated successfully");
}

/// Test 3: Mixed comma and space separators
///
/// **Purpose**: Validates mixed separator styles (dialect-specific)
/// **COBOL Compliance**: Some COBOL dialects allow mixed separators
/// **Enterprise Context**: Legacy mainframe copybooks with varied formatting
#[test]
fn test_level88_mixed_separators() {
    const COPYBOOK: &str = r#"
01 STATUS-FLAG PIC X(2).
   88 ACTIVE-STATUS VALUE "A", "R" "T", "W".
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let status_field = &schema.fields[0];
    let level88 = &status_field.children[0];

    if let FieldKind::Condition { values } = &level88.kind {
        assert_eq!(
            values.len(),
            4,
            "Should parse mixed comma/space separated values"
        );
        assert_eq!(values, &vec!["A", "R", "T", "W"]);
    } else {
        panic!("Expected FieldKind::Condition for mixed separators");
    }

    println!("✅ Level-88 mixed separator values validated successfully");
}

/// Test 4: Comma with THRU ranges
///
/// **Purpose**: Validates comma usage with THRU/THROUGH ranges
/// **COBOL Compliance**: VALUE clause range syntax with commas
/// **Enterprise Context**: Priority code ranges in healthcare systems
#[test]
fn test_level88_comma_with_ranges() {
    const COPYBOOK: &str = r#"
01 PRIORITY-CODE PIC 9(1).
   88 HIGH-PRIORITY VALUE 1, 2, 3 THRU 5.
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let priority_field = &schema.fields[0];
    let level88 = &priority_field.children[0];

    if let FieldKind::Condition { values } = &level88.kind {
        assert_eq!(
            values.len(),
            3,
            "Should parse exactly 3 values: two singles and one range"
        );
        assert_eq!(values, &vec!["1", "2", "3 THRU 5"]);
    } else {
        panic!("Expected FieldKind::Condition for range values");
    }

    println!("✅ Level-88 comma with THRU ranges validated successfully");
}

/// Test 5: Enterprise healthcare scenario
///
/// **Purpose**: Validates realistic enterprise mainframe pattern
/// **COBOL Compliance**: Production-grade copybook structure
/// **Enterprise Context**: Patient status codes from healthcare HL7/HIPAA systems
#[test]
fn test_level88_enterprise_healthcare_comma_values() {
    const COPYBOOK: &str = r#"
01 PATIENT-RECORD.
   05 PATIENT-ID      PIC X(10).
   05 STATUS-CODE     PIC X(1).
      88 STATUS-ACTIVE   VALUE "A", "R", "T".
      88 STATUS-INACTIVE VALUE "I", "D", "C".
      88 STATUS-PENDING  VALUE "P", "W", "H".
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let patient_record = &schema.fields[0];

    let status_field = patient_record
        .children
        .iter()
        .find(|f| f.name == "STATUS-CODE")
        .expect("Should find STATUS-CODE field");

    assert_eq!(
        status_field.children.len(),
        3,
        "Should have 3 Level-88 condition fields"
    );

    for level88 in &status_field.children {
        assert_eq!(level88.level, 88);
        if let FieldKind::Condition { values } = &level88.kind {
            assert_eq!(
                values.len(),
                3,
                "Each condition should have exactly 3 comma-separated values"
            );
        } else {
            panic!("Expected FieldKind::Condition for {}", level88.name);
        }
    }

    // Verify specific condition names
    let condition_names: Vec<&str> = status_field
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert!(condition_names.contains(&"STATUS-ACTIVE"));
    assert!(condition_names.contains(&"STATUS-INACTIVE"));
    assert!(condition_names.contains(&"STATUS-PENDING"));

    println!("✅ Enterprise healthcare Level-88 comma values validated successfully");
}

/// Test 6: Backward compatibility - space-separated still works
///
/// **Purpose**: Ensures existing space-separated syntax remains functional
/// **COBOL Compliance**: Space separator is original COBOL standard
/// **Enterprise Context**: Legacy copybooks must continue to parse correctly
#[test]
fn test_level88_space_separated_backward_compatibility() {
    const COPYBOOK: &str = r#"
01 STATUS-FIELD PIC X(1).
   88 IS-VALID VALUE "A" "B" "C".
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let status_field = &schema.fields[0];
    let level88 = &status_field.children[0];

    if let FieldKind::Condition { values } = &level88.kind {
        assert_eq!(values.len(), 3, "Space-separated values should still parse");
        assert_eq!(values, &vec!["A", "B", "C"]);
    } else {
        panic!("Expected FieldKind::Condition for space-separated values");
    }

    println!("✅ Backward compatibility with space-separated values validated");
}

/// Test 7: Multiple Level-88 with commas in single parent
///
/// **Purpose**: Validates multiple comma-separated Level-88 definitions
/// **COBOL Compliance**: Multiple condition names per field
/// **Enterprise Context**: Financial transaction status codes
#[test]
fn test_multiple_level88_comma_separated_same_parent() {
    const COPYBOOK: &str = r#"
01 TRANSACTION-STATUS PIC X(2).
   88 APPROVED-CODES   VALUE "AP", "AC", "OK".
   88 PENDING-CODES    VALUE "PD", "RV", "HO".
   88 REJECTED-CODES   VALUE "RJ", "DE", "FA".
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let txn_field = &schema.fields[0];
    assert_eq!(txn_field.children.len(), 3);

    for level88 in &txn_field.children {
        assert_eq!(level88.level, 88);
        if let FieldKind::Condition { values } = &level88.kind {
            assert_eq!(values.len(), 3, "{} should have 3 values", level88.name);
        }
    }

    println!("✅ Multiple Level-88 with commas validated successfully");
}

/// Test 8: Trailing comma handling
///
/// **Purpose**: Validates that trailing commas are handled gracefully
/// **COBOL Compliance**: Parser accepts trailing commas defensively
/// **Enterprise Context**: Defensive parsing for malformed copybooks
#[test]
fn test_level88_trailing_comma() {
    // Parser accepts trailing commas gracefully (defensive parsing)
    const COPYBOOK: &str = r#"
01 STATUS-FIELD PIC X(1).
   88 IS-VALID VALUE "A", "B", "C",.
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let level88 = &schema.fields[0].children[0];

    if let FieldKind::Condition { values } = &level88.kind {
        assert_eq!(
            values.len(),
            3,
            "Trailing comma should not create empty value"
        );
        assert_eq!(values, &vec!["A", "B", "C"]);
    } else {
        panic!("Expected FieldKind::Condition for trailing comma test");
    }

    println!("✅ Trailing comma handled gracefully (accepted, ignored)");
}

/// Test 9: Single value with comma (edge case)
///
/// **Purpose**: Validates single-value VALUE clause doesn't require comma
/// **COBOL Compliance**: Single value shouldn't have trailing comma
/// **Enterprise Context**: Simple boolean flags
#[test]
fn test_level88_single_value_no_comma() {
    const COPYBOOK: &str = r#"
01 FLAG-FIELD PIC X(1).
   88 IS-TRUE VALUE "Y".
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let flag_field = &schema.fields[0];
    let level88 = &flag_field.children[0];

    if let FieldKind::Condition { values } = &level88.kind {
        assert_eq!(values.len(), 1, "Single value should parse correctly");
        assert_eq!(values[0], "Y");
    } else {
        panic!("Expected FieldKind::Condition for single value");
    }

    println!("✅ Single value without comma validated successfully");
}

/// Test 10: Complex enterprise banking scenario
///
/// **Purpose**: Validates complex real-world mainframe pattern
/// **COBOL Compliance**: Production-grade copybook with multiple features
/// **Enterprise Context**: Account status tracking in core banking systems
#[test]
fn test_level88_complex_banking_scenario() {
    const COPYBOOK: &str = r#"
01 ACCOUNT-MASTER-RECORD.
   05 ACCOUNT-NUMBER     PIC 9(12).
   05 ACCOUNT-TYPE-CODE  PIC X(2).
      88 CHECKING-ACCT   VALUE "CH", "CK", "C1".
      88 SAVINGS-ACCT    VALUE "SA", "SV", "S1".
      88 LOAN-ACCT       VALUE "LN", "LA", "L1".
      88 INVESTMENT-ACCT VALUE "IN", "IV", "I1", "I2".
   05 STATUS-CODE        PIC 9(1).
      88 ACTIVE-STATUS   VALUE 1, 2, 3.
      88 CLOSED-STATUS   VALUE 7, 8, 9.
   05 BRANCH-CODE        PIC X(4).
"#;

    let schema = parse_copybook(COPYBOOK).unwrap();
    let account_record = &schema.fields[0];

    // Verify account type conditions
    let account_type_field = account_record
        .children
        .iter()
        .find(|f| f.name == "ACCOUNT-TYPE-CODE")
        .expect("Should find ACCOUNT-TYPE-CODE");

    assert_eq!(account_type_field.children.len(), 4);
    for level88 in &account_type_field.children {
        if let FieldKind::Condition { values } = &level88.kind {
            assert!(
                values.len() >= 3,
                "{} should have at least 3 values",
                level88.name
            );
        }
    }

    // Verify status conditions
    let status_field = account_record
        .children
        .iter()
        .find(|f| f.name == "STATUS-CODE")
        .expect("Should find STATUS-CODE");

    assert_eq!(status_field.children.len(), 2);
    for level88 in &status_field.children {
        if let FieldKind::Condition { values } = &level88.kind {
            assert_eq!(values.len(), 3, "{} should have 3 values", level88.name);
        }
    }

    println!("✅ Complex banking scenario validated successfully");
}
