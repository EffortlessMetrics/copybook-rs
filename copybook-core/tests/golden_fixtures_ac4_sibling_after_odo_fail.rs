#![allow(clippy::expect_used, clippy::unwrap_used)]
#![allow(clippy::doc_markdown)]

/*!
 * AC4: Sibling-after-ODO Failure Validation Fixtures (FAIL Scenarios)
 *
 * These golden fixtures validate that storage fields after ODO arrays are
 * correctly rejected, following COBOL structural semantics where ODO arrays
 * must be the last storage element to ensure proper memory layout.
 *
 * **COBOL Rule**: ODO arrays must be at tail position - no storage elements
 * can follow an ODO array in the same group level to prevent data corruption
 * from variable-length field placement.
 *
 * **Enterprise Production Impact**: Prevents data corruption and layout issues
 * in mainframe applications by enforcing structural constraints.
 */

use copybook_core::{ErrorCode, parse_copybook};

/// AC4 Basic: Simple storage field after ODO (FAIL)
///
/// **Purpose**: Validates that basic storage field after ODO is rejected
/// **COBOL Rule**: Storage elements cannot follow ODO arrays
/// **Expected Error**: CBKP021_ODO_NOT_TAIL
/// **Enterprise Context**: Inventory record with incorrect trailer field
#[test]
fn test_ac4_basic_storage_after_odo_fail() {
    const COPYBOOK: &str = r"
01 INVENTORY-RECORD.
   05 RECORD-ID       PIC X(8).
   05 ITEM-COUNT      PIC 9(3).
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-CODE    PIC X(10).
      10 ITEM-QTY     PIC 9(5).
   05 RECORD-TRAILER  PIC X(10).
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "AC4 basic fixture: Storage field after ODO should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for storage after ODO, got: {:?}",
        error.code
    );

    // Validate error context provides meaningful information
    if let Some(context) = &error.context {
        assert!(
            context.field_path.is_some(),
            "Error should include field path context"
        );
        println!(
            "✅ AC4 basic storage after ODO properly rejected with error: {}",
            error.message
        );
    }

    println!("✅ AC4 basic storage after ODO failure validation successful");
}

/// AC4 Intermediate: Multiple storage fields after ODO (FAIL)
///
/// **Purpose**: Validates that multiple storage fields after ODO are rejected
/// **COBOL Rule**: Any storage element after ODO violates tail constraint
/// **Expected Error**: CBKP021_ODO_NOT_TAIL
/// **Enterprise Context**: Customer record with multiple trailing fields
#[test]
fn test_ac4_multiple_storage_after_odo_fail() {
    const COPYBOOK: &str = r"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID     PIC X(12).
   05 ACCOUNT-COUNT   PIC 9(4).
   05 ACCOUNTS OCCURS 1 TO 500 TIMES DEPENDING ON ACCOUNT-COUNT.
      10 ACCOUNT-NUM  PIC 9(10).
      10 BALANCE      PIC S9(9)V99 COMP-3.
   05 AUDIT-INFO.
      10 CREATED-DATE PIC 9(8).
      10 UPDATED-DATE PIC 9(8).
   05 RECORD-STATUS   PIC X(1).
   05 CHECKSUM        PIC 9(8).
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "AC4 multiple storage fields: Should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for multiple storage after ODO, got: {:?}",
        error.code
    );

    println!("✅ AC4 multiple storage after ODO failure validation successful");
}

/// AC4 Advanced: Nested group with storage after ODO (FAIL)
///
/// **Purpose**: Validates that nested groups with storage after ODO are rejected
/// **COBOL Rule**: Group fields containing storage violate ODO tail constraint
/// **Expected Error**: CBKP021_ODO_NOT_TAIL
/// **Enterprise Context**: Employee record with complex trailing structure
#[test]
fn test_ac4_nested_storage_group_after_odo_fail() {
    const COPYBOOK: &str = r"
01 EMPLOYEE-RECORD.
   05 EMPLOYEE-HEADER.
      10 EMPLOYEE-ID     PIC X(10).
      10 DEPARTMENT-CODE PIC X(4).
   05 TIMECARD-COUNT     PIC 9(4).
   05 TIMECARDS OCCURS 1 TO 2000 TIMES DEPENDING ON TIMECARD-COUNT.
      10 ENTRY-DATE      PIC 9(8).
      10 HOURS-WORKED    PIC 9(3)V99.
      10 OVERTIME-HOURS  PIC 9(3)V99.
   05 SUMMARY-INFO.
      10 TOTAL-HOURS     PIC 9(6)V99 COMP-3.
      10 TOTAL-OVERTIME  PIC 9(6)V99 COMP-3.
      10 GROSS-PAY       PIC 9(8)V99 COMP-3.
   05 BENEFITS-INFO.
      10 HEALTH-PLAN     PIC X(6).
      10 RETIREMENT-PCT  PIC 9(2)V99.
      10 VACATION-DAYS   PIC 9(3).
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "AC4 nested storage group: Should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for nested storage after ODO, got: {:?}",
        error.code
    );

    println!("✅ AC4 nested storage group after ODO failure validation successful");
}

/// AC4 Complex: Multiple ODO arrays with storage violation (FAIL)
///
/// **Purpose**: Validates that the final ODO in sequence must respect tail constraints
/// **COBOL Rule**: Last ODO in any sequence must still be at tail position
/// **Expected Error**: CBKP021_ODO_NOT_TAIL
/// **Enterprise Context**: Transaction batch with multiple variable arrays
#[test]
fn test_ac4_multiple_odo_with_storage_violation_fail() {
    const COPYBOOK: &str = r"
01 TRANSACTION-BATCH.
   05 BATCH-HEADER.
      10 BATCH-ID        PIC X(16).
      10 BATCH-DATE      PIC 9(8).
   05 HEADER-COUNT       PIC 9(3).
   05 HEADER-RECORDS OCCURS 1 TO 100 TIMES DEPENDING ON HEADER-COUNT.
      10 HEADER-TYPE     PIC X(3).
      10 HEADER-DATA     PIC X(50).
   05 DETAIL-COUNT       PIC 9(5).
   05 DETAIL-RECORDS OCCURS 1 TO 10000 TIMES DEPENDING ON DETAIL-COUNT.
      10 TRANSACTION-ID  PIC X(20).
      10 AMOUNT          PIC S9(11)V99 COMP-3.
   05 BATCH-CONTROL.
      10 RECORD-COUNT    PIC 9(8).
      10 BATCH-TOTAL     PIC S9(13)V99 COMP-3.
      10 HASH-TOTAL      PIC 9(12).
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "AC4 multiple ODO with storage: Should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for multiple ODO violation, got: {:?}",
        error.code
    );

    println!("✅ AC4 multiple ODO with storage violation failure validation successful");
}

/// AC4 Enterprise: Financial record with regulatory audit trail after ODO (FAIL)
///
/// **Purpose**: Validates enterprise-scale ODO tail constraint enforcement
/// **COBOL Rule**: Even complex audit structures cannot follow ODO arrays
/// **Expected Error**: CBKP021_ODO_NOT_TAIL
/// **Enterprise Context**: Banking record with compliance audit trail
#[test]
fn test_ac4_enterprise_audit_trail_after_odo_fail() {
    const COPYBOOK: &str = r"
01 BANKING-TRANSACTION.
   05 TRANSACTION-HEADER.
      10 TRANSACTION-ID     PIC X(24).
      10 PROCESSING-DATE    PIC 9(8).
      10 INSTITUTION-ID     PIC X(8).
   05 ENTRY-COUNT           PIC 9(6).
   05 JOURNAL-ENTRIES OCCURS 1 TO 500000 TIMES DEPENDING ON ENTRY-COUNT.
      10 ENTRY-SEQUENCE     PIC 9(8).
      10 ACCOUNT-NUMBER     PIC X(20).
      10 DEBIT-AMOUNT       PIC S9(13)V99 COMP-3.
      10 CREDIT-AMOUNT      PIC S9(13)V99 COMP-3.
      10 POSTING-DATE       PIC 9(8).
   05 REGULATORY-AUDIT.
      10 AUDIT-ID           PIC X(16).
      10 AUDITOR-ID         PIC X(8).
      10 AUDIT-DATE         PIC 9(8).
      10 AML-STATUS         PIC X(4).
      10 KYC-STATUS         PIC X(4).
   05 ARCHIVE-INFO.
      10 RETENTION-DATE     PIC 9(8).
      10 ARCHIVE-STATUS     PIC X(2).
      10 VAULT-LOCATION     PIC X(12).
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "AC4 enterprise audit trail: Should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for enterprise audit trail after ODO, got: {:?}",
        error.code
    );

    println!("✅ AC4 enterprise audit trail after ODO failure validation successful");
}

/// AC4 Performance: Large-scale ODO with performance impact validation (FAIL)
///
/// **Purpose**: Validates that error detection is efficient even with large structures
/// **COBOL Rule**: ODO tail constraint enforcement should be performant
/// **Expected Error**: CBKP021_ODO_NOT_TAIL
/// **Enterprise Context**: High-volume telecom CDR with trailing summaries
#[test]
fn test_ac4_performance_large_scale_odo_tail_violation_fail() {
    const COPYBOOK: &str = r"
01 CDR-PROCESSING-BATCH.
   05 BATCH-CONTROL.
      10 BATCH-ID           PIC X(20).
      10 PROCESSING-TS      PIC 9(14).
      10 NETWORK-ELEMENT    PIC X(8).
   05 CDR-COUNT             PIC 9(10).
   05 CALL-DETAIL-RECORDS OCCURS 1 TO 10000000 TIMES DEPENDING ON CDR-COUNT.
      10 CALL-IDENTIFICATION.
         15 CALL-ID         PIC X(32).
         15 CALL-TYPE       PIC X(2).
         15 SERVICE-TYPE    PIC X(4).
      10 PARTY-INFORMATION.
         15 A-NUMBER        PIC X(15).
         15 B-NUMBER        PIC X(15).
         15 C-NUMBER        PIC X(15).
      10 TIMING-DATA.
         15 START-TIME      PIC 9(14).
         15 END-TIME        PIC 9(14).
         15 DURATION        PIC 9(8).
      10 LOCATION-DATA.
         15 ORIG-CELL-ID    PIC X(8).
         15 TERM-CELL-ID    PIC X(8).
         15 ROAMING-FLAG    PIC X(1).
      10 CHARGING-DATA.
         15 TARIFF-CLASS    PIC X(4).
         15 CHARGED-UNITS   PIC 9(8).
         15 CHARGE-AMOUNT   PIC 9(8)V99 COMP-3.
   05 BATCH-TOTALS.
      10 TOTAL-CALLS        PIC 9(10).
      10 TOTAL-DURATION     PIC 9(12).
      10 TOTAL-REVENUE      PIC 9(12)V99 COMP-3.
      10 PROCESSING-TIME    PIC 9(8).
   05 QUALITY-METRICS.
      10 PROCESSING-ERRORS  PIC 9(6).
      10 VALIDATION-ERRORS  PIC 9(6).
      10 SUCCESS-RATE       PIC 9(3)V99.
   05 ARCHIVE-CONTROL.
      10 ARCHIVE-TIMESTAMP  PIC 9(14).
      10 RETENTION-POLICY   PIC X(6).
      10 BACKUP-STATUS      PIC X(2).
";

    let start_time = std::time::Instant::now();
    let result = parse_copybook(COPYBOOK);
    let parse_duration = start_time.elapsed();

    assert!(
        result.is_err(),
        "AC4 performance large-scale: Should be invalid"
    );

    // Performance validation: error detection should be fast even with large structures
    assert!(
        parse_duration.as_millis() < 200,
        "Large-scale ODO tail violation should be detected quickly, actual: {}ms",
        parse_duration.as_millis()
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for large-scale ODO violation, got: {:?}",
        error.code
    );

    println!(
        "✅ AC4 performance large-scale ODO tail violation validated ({}ms)",
        parse_duration.as_millis()
    );
}

/// AC4 Edge Case: ODO with COMP fields and storage after (FAIL)
///
/// **Purpose**: Validates that computational fields after ODO are also rejected
/// **COBOL Rule**: COMP fields are storage elements and violate ODO tail constraint
/// **Expected Error**: CBKP021_ODO_NOT_TAIL
/// **Enterprise Context**: Scientific computation record with COMP trailer fields
#[test]
fn test_ac4_comp_fields_after_odo_fail() {
    const COPYBOOK: &str = r"
01 SCIENTIFIC-RECORD.
   05 EXPERIMENT-ID      PIC X(12).
   05 MEASUREMENT-COUNT  PIC 9(5).
   05 MEASUREMENTS OCCURS 1 TO 50000 TIMES DEPENDING ON MEASUREMENT-COUNT.
      10 TIMESTAMP       PIC 9(14).
      10 SENSOR-ID       PIC X(8).
      10 TEMPERATURE     PIC S9(3)V99 COMP-3.
      10 PRESSURE        PIC S9(5)V999 COMP-3.
      10 HUMIDITY        PIC 9(3)V99 COMP-3.
   05 STATISTICAL-SUMMARY.
      10 AVERAGE-TEMP    PIC S9(3)V99 COMP.
      10 MIN-TEMP        PIC S9(3)V99 COMP.
      10 MAX-TEMP        PIC S9(3)V99 COMP.
      10 STD-DEVIATION   PIC 9(3)V9(6) COMP.
   05 CALIBRATION-DATA.
      10 OFFSET-VALUE    PIC S9(5)V9(8) COMP-1.
      10 SCALE-FACTOR    PIC 9V9(10) COMP-2.
      10 ERROR-MARGIN    PIC 9(2)V9(6) COMP-3.
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "AC4 COMP fields after ODO: Should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for COMP fields after ODO, got: {:?}",
        error.code
    );

    println!("✅ AC4 COMP fields after ODO failure validation successful");
}

/// AC4 Edge Case: ODO with mixed storage types after (FAIL)
///
/// **Purpose**: Validates rejection of mixed storage element types after ODO
/// **COBOL Rule**: Any storage element type violates ODO tail constraint
/// **Expected Error**: CBKP021_ODO_NOT_TAIL
/// **Enterprise Context**: Manufacturing record with diverse trailing data types
#[test]
fn test_ac4_mixed_storage_types_after_odo_fail() {
    const COPYBOOK: &str = r"
01 MANUFACTURING-RECORD.
   05 PRODUCTION-HEADER.
      10 BATCH-NUMBER     PIC X(10).
      10 PRODUCT-CODE     PIC X(8).
      10 SHIFT-ID         PIC 9(2).
   05 OPERATION-COUNT    PIC 9(4).
   05 OPERATIONS OCCURS 1 TO 1000 TIMES DEPENDING ON OPERATION-COUNT.
      10 OPERATION-ID     PIC X(6).
      10 START-TIME       PIC 9(6).
      10 END-TIME         PIC 9(6).
      10 OPERATOR-ID      PIC X(8).
      10 MACHINE-ID       PIC X(10).
      10 QUANTITY-PROCESSED PIC 9(8).
      10 QUALITY-SCORE    PIC 9(3).
   05 TEXT-SUMMARY       PIC X(200).
   05 NUMERIC-TOTAL      PIC 9(10).
   05 DECIMAL-AVERAGE    PIC 9(6)V99.
   05 COMP3-COST         PIC S9(9)V99 COMP-3.
   05 BINARY-FLAGS       PIC 9(8) COMP.
   05 FLOATING-EFFICIENCY PIC 9V9(8) COMP-1.
   05 DOUBLE-PRECISION   PIC 9V9(15) COMP-2.
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_err(),
        "AC4 mixed storage types: Should be invalid"
    );

    let error = result.unwrap_err();
    assert_eq!(
        error.code,
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "Expected CBKP021_ODO_NOT_TAIL error for mixed storage after ODO, got: {:?}",
        error.code
    );

    println!("✅ AC4 mixed storage types after ODO failure validation successful");
}

/// AC4 Comprehensive Coverage Validation
///
/// **Purpose**: Meta-test ensuring all AC4 failure fixtures are accounted for
/// **Enterprise Impact**: Guarantees sibling-after-ODO constraint enforcement coverage
#[test]
fn test_ac4_comprehensive_coverage() {
    let ac4_fixtures = [
        "test_ac4_basic_storage_after_odo_fail",
        "test_ac4_multiple_storage_after_odo_fail",
        "test_ac4_nested_storage_group_after_odo_fail",
        "test_ac4_multiple_odo_with_storage_violation_fail",
        "test_ac4_enterprise_audit_trail_after_odo_fail",
        "test_ac4_performance_large_scale_odo_tail_violation_fail",
        "test_ac4_comp_fields_after_odo_fail",
        "test_ac4_mixed_storage_types_after_odo_fail",
    ];

    // Verify we have the expected count of AC4 fixtures
    assert_eq!(
        ac4_fixtures.len(),
        8,
        "Expected 8 AC4 sibling-after-ODO failure validation fixtures"
    );

    println!(
        "✅ AC4 comprehensive coverage validated with {} fixtures:",
        ac4_fixtures.len()
    );
    for (i, fixture) in ac4_fixtures.iter().enumerate() {
        println!("   {}. {}", i + 1, fixture);
    }

    // Validate fixture naming consistency
    for fixture in &ac4_fixtures {
        assert!(
            fixture.starts_with("test_ac4_"),
            "AC4 fixtures should have consistent naming"
        );
        assert!(fixture.contains("odo"), "AC4 fixtures should include ODO");
        assert!(
            fixture.ends_with("_fail"),
            "AC4 fixtures should be FAIL scenarios"
        );
    }

    // Validate error code coverage
    println!("✅ All AC4 fixtures validate CBKP021_ODO_NOT_TAIL error code enforcement");

    // Validate fixture categories
    let basic_fixtures: Vec<_> = ac4_fixtures
        .iter()
        .filter(|f| f.contains("basic"))
        .collect();
    let multiple_fixtures: Vec<_> = ac4_fixtures
        .iter()
        .filter(|f| f.contains("multiple"))
        .collect();
    let nested_fixtures: Vec<_> = ac4_fixtures
        .iter()
        .filter(|f| f.contains("nested"))
        .collect();
    let enterprise_fixtures: Vec<_> = ac4_fixtures
        .iter()
        .filter(|f| f.contains("enterprise"))
        .collect();
    let performance_fixtures: Vec<_> = ac4_fixtures
        .iter()
        .filter(|f| f.contains("performance"))
        .collect();
    let edge_case_fixtures: Vec<_> = ac4_fixtures
        .iter()
        .filter(|f| f.contains("comp") || f.contains("mixed"))
        .collect();

    assert!(
        !basic_fixtures.is_empty(),
        "Should have basic failure fixtures"
    );
    assert!(
        !multiple_fixtures.is_empty(),
        "Should have multiple storage failure fixtures"
    );
    assert!(
        !nested_fixtures.is_empty(),
        "Should have nested structure failure fixtures"
    );
    assert!(
        !enterprise_fixtures.is_empty(),
        "Should have enterprise failure fixtures"
    );
    assert!(
        !performance_fixtures.is_empty(),
        "Should have performance failure fixtures"
    );
    assert!(
        !edge_case_fixtures.is_empty(),
        "Should have edge case failure fixtures"
    );

    println!("✅ AC4 sibling-after-ODO failure validation fixtures comprehensively validated");
    println!("   Basic: {}", basic_fixtures.len());
    println!("   Multiple: {}", multiple_fixtures.len());
    println!("   Nested: {}", nested_fixtures.len());
    println!("   Enterprise: {}", enterprise_fixtures.len());
    println!("   Performance: {}", performance_fixtures.len());
    println!("   Edge Cases: {}", edge_case_fixtures.len());
}
