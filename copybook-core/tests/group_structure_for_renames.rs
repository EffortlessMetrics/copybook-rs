//! Parser tests for correct group structure preservation around level-66 RENAMES.
//!
//! Issue #133 Phase R2a: The parser's level-66 handling was incorrectly "popping to level-01",
//! which promoted group children (e.g., ADDRESS) as siblings instead of preserving them
//! as children of their parent group (e.g., CUSTOMER-INFO).
//!
//! This test file locks in the expected tree structure before fixing the parser logic.

use copybook_core::parse_copybook;

#[test]
fn group_structure_without_renames() {
    // First, verify basic group structure works correctly WITHOUT level-66.
    // This establishes the baseline expectation.
    let cb = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
      10 ADDRESS  PIC X(60).
"#;

    let schema = parse_copybook(cb).expect("parse ok");
    let record = &schema.fields[0];

    assert_eq!(record.name, "CUSTOMER-RECORD");
    assert_eq!(record.children.len(), 1, "expected [CUSTOMER-INFO]");

    let customer_info = &record.children[0];
    assert_eq!(customer_info.name, "CUSTOMER-INFO");
    assert_eq!(customer_info.children.len(), 2, "expected [NAME, ADDRESS]");
    assert_eq!(customer_info.children[0].name, "NAME");
    assert_eq!(customer_info.children[1].name, "ADDRESS");
}

#[test]
fn level88_preserves_group_structure() {
    // Level-88 should not disrupt group structure (they're non-storage, like level-66).
    // This tests that the parser correctly handles non-storage levels without promoting children.
    let cb = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
      10 ADDRESS  PIC X(60).
   05 STATUS-CODE PIC X.
      88 ACTIVE   VALUE 'A'.
      88 INACTIVE VALUE 'I'.
"#;

    let schema = parse_copybook(cb).expect("parse ok");
    let record = &schema.fields[0];

    assert_eq!(
        record.children.len(),
        2,
        "expected [CUSTOMER-INFO, STATUS-CODE]"
    );

    let customer_info = &record.children[0];
    assert_eq!(customer_info.name, "CUSTOMER-INFO");
    assert_eq!(customer_info.children.len(), 2, "expected [NAME, ADDRESS]");

    let status = &record.children[1];
    assert_eq!(status.name, "STATUS-CODE");
    assert_eq!(status.children.len(), 2, "expected [ACTIVE, INACTIVE]");
}

#[test]
fn customer_info_group_children_are_preserved() {
    // This copybook has:
    // - CUSTOMER-RECORD (01)
    //   - CUSTOMER-INFO (05) group with two children
    //     - NAME (10)
    //     - ADDRESS (10)
    //   - CUSTOMER-DETAILS (66) RENAMES of the CUSTOMER-INFO group
    //
    // Bug: Parser currently produces:
    //   CUSTOMER-RECORD.children = [CUSTOMER-INFO, ADDRESS, CUSTOMER-DETAILS]
    //   CUSTOMER-INFO.children = [NAME]
    //
    // Expected:
    //   CUSTOMER-RECORD.children = [CUSTOMER-INFO, CUSTOMER-DETAILS]
    //   CUSTOMER-INFO.children = [NAME, ADDRESS]

    let cb = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
      10 ADDRESS  PIC X(60).
   66 CUSTOMER-DETAILS RENAMES CUSTOMER-INFO THRU CUSTOMER-INFO.
"#;

    let schema = parse_copybook(cb).expect("parse ok");
    let record = &schema.fields[0];

    assert_eq!(record.name, "CUSTOMER-RECORD");

    // CUSTOMER-RECORD should have exactly 2 children: CUSTOMER-INFO (storage) and CUSTOMER-DETAILS (alias)
    assert_eq!(
        record.children.len(),
        2,
        "expected [CUSTOMER-INFO, CUSTOMER-DETAILS], got: {:?}",
        record.children.iter().map(|f| &f.name).collect::<Vec<_>>()
    );

    // Find CUSTOMER-INFO group
    let customer_info = record
        .children
        .iter()
        .find(|f| f.name == "CUSTOMER-INFO")
        .expect("CUSTOMER-INFO group should be a child of CUSTOMER-RECORD");

    // CUSTOMER-INFO should have both NAME and ADDRESS as children
    assert_eq!(
        customer_info.children.len(),
        2,
        "CUSTOMER-INFO should have [NAME, ADDRESS] as children, got: {:?}",
        customer_info
            .children
            .iter()
            .map(|f| &f.name)
            .collect::<Vec<_>>()
    );

    assert!(
        customer_info.children.iter().any(|f| f.name == "NAME"),
        "NAME should be a child of CUSTOMER-INFO"
    );
    assert!(
        customer_info.children.iter().any(|f| f.name == "ADDRESS"),
        "ADDRESS should be a child of CUSTOMER-INFO"
    );

    // Find CUSTOMER-DETAILS alias
    let customer_details = record
        .children
        .iter()
        .find(|f| f.name == "CUSTOMER-DETAILS")
        .expect("CUSTOMER-DETAILS should be a child of CUSTOMER-RECORD");

    assert_eq!(
        customer_details.level, 66,
        "CUSTOMER-DETAILS should be level-66"
    );
}

#[test]
#[ignore = "R3 (nested group RENAMES) requires resolver enhancement for recursive lookup - Phase R2b"]
fn nested_group_children_preserved_with_level66() {
    // More complex nested case (R3 scenario):
    // - TRANSACTION-RECORD (01)
    //   - TRANSACTION-HEADER (05) group
    //     - TRANSACTION-DATES (10) nested group
    //       - START-DATE (15)
    //       - END-DATE (15)
    //   - DATE-RANGE (66) RENAMES of the nested TRANSACTION-DATES group
    //
    // Parser now correctly preserves the nested structure, but resolver needs enhancement
    // to support recursive lookup for nested group RENAMES (R3 case).
    //
    // Expected:
    //   TRANSACTION-RECORD.children = [TRANSACTION-HEADER, DATE-RANGE]
    //   TRANSACTION-HEADER.children = [TRANSACTION-DATES]
    //   TRANSACTION-DATES.children = [START-DATE, END-DATE]

    let cb = r#"
01 TRANSACTION-RECORD.
   05 TRANSACTION-HEADER.
      10 TRANSACTION-DATES.
         15 START-DATE  PIC X(10).
         15 END-DATE    PIC X(10).
   66 DATE-RANGE RENAMES TRANSACTION-DATES THRU TRANSACTION-DATES.
"#;

    let schema = parse_copybook(cb).expect("parse ok");
    let record = &schema.fields[0];

    assert_eq!(record.name, "TRANSACTION-RECORD");

    // TRANSACTION-RECORD should have exactly 2 children
    assert_eq!(
        record.children.len(),
        2,
        "expected [TRANSACTION-HEADER, DATE-RANGE], got: {:?}",
        record.children.iter().map(|f| &f.name).collect::<Vec<_>>()
    );

    // Find TRANSACTION-HEADER
    let header = record
        .children
        .iter()
        .find(|f| f.name == "TRANSACTION-HEADER")
        .expect("TRANSACTION-HEADER should be a child");

    // TRANSACTION-HEADER should have TRANSACTION-DATES as child
    assert_eq!(
        header.children.len(),
        1,
        "TRANSACTION-HEADER should have [TRANSACTION-DATES], got: {:?}",
        header.children.iter().map(|f| &f.name).collect::<Vec<_>>()
    );

    let dates = &header.children[0];
    assert_eq!(dates.name, "TRANSACTION-DATES");

    // TRANSACTION-DATES should have both date fields
    assert_eq!(
        dates.children.len(),
        2,
        "TRANSACTION-DATES should have [START-DATE, END-DATE], got: {:?}",
        dates.children.iter().map(|f| &f.name).collect::<Vec<_>>()
    );

    assert!(dates.children.iter().any(|f| f.name == "START-DATE"));
    assert!(dates.children.iter().any(|f| f.name == "END-DATE"));
}
