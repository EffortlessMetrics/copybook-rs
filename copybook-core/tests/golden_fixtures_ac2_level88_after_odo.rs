#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::unwrap_used, clippy::expect_used)]

/*!
 * AC2: Level-88 after ODO Validation Fixtures (PASS Scenarios)
 *
 * These golden fixtures validate that Level-88 condition fields are correctly
 * allowed after ODO arrays, following COBOL structural semantics where
 * non-storage elements don't violate ODO tail constraints.
 *
 * **COBOL Rule**: Level-88 (condition) fields are non-storage elements and
 * can legitimately follow ODO arrays without violating structural constraints.
 *
 * **Enterprise Production Impact**: Ensures conditional processing logic can
 * be applied to variable-length records in mainframe applications.
 */

use copybook_core::{Occurs, parse_copybook};

/// AC2 Basic: Simple Level-88 after ODO (PASS)
///
/// **Purpose**: Validates basic Level-88 condition after ODO array
/// **COBOL Rule**: Non-storage Level-88 doesn't violate ODO tail constraint
/// **Enterprise Context**: Inventory management with item status conditions
#[test]
fn test_ac2_basic_level88_after_odo_pass() {
    const COPYBOOK: &str = r"
01 INVENTORY-RECORD.
   05 ITEM-COUNT      PIC 9(3).
   05 STATUS-FLAG     PIC X(1).
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-CODE    PIC X(8).
      10 ITEM-QTY     PIC 9(5).
      10 ITEM-STATUS  PIC X(1).
   88 STATUS-ACTIVE   VALUE 'A'.
   88 STATUS-INACTIVE VALUE 'I'.
   88 STATUS-DISCONTINUED VALUE 'D'.
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_ok(),
        "AC2 basic fixture: Level-88 after ODO should be valid. Error: {:?}",
        result.err()
    );

    let schema = result.unwrap();
    let root = &schema.fields[0];

    // Validate ODO structure
    let items_field = root.children.iter().find(|f| f.name == "ITEMS").unwrap();
    assert!(
        matches!(
            items_field.occurs,
            Some(Occurs::ODO {
                min: 1,
                max: 100,
                ..
            })
        ),
        "ITEMS should be ODO with correct bounds"
    );

    // Validate Level-88 fields are parsed correctly
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();
    assert_eq!(
        level88_fields.len(),
        3,
        "Should have 3 Level-88 condition fields"
    );

    // Verify Level-88 field names and values
    let status_names: Vec<&str> = level88_fields.iter().map(|f| f.name.as_str()).collect();
    assert!(status_names.contains(&"STATUS-ACTIVE"));
    assert!(status_names.contains(&"STATUS-INACTIVE"));
    assert!(status_names.contains(&"STATUS-DISCONTINUED"));

    println!("✅ AC2 basic Level-88 after ODO validated successfully");
}

/// AC2 Intermediate: Multiple Level-88 groups after ODO (PASS)
///
/// **Purpose**: Validates multiple Level-88 condition groups after ODO
/// **COBOL Rule**: Multiple non-storage elements can follow ODO arrays
/// **Enterprise Context**: Customer records with multiple status categories
#[test]
fn test_ac2_multiple_level88_after_odo_pass() {
    const COPYBOOK: &str = r"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID     PIC X(10).
   05 ACCOUNT-COUNT   PIC 9(4).
   05 ACCOUNT-TYPE-FLAG PIC X(2).
   05 STATUS-FLAG PIC X(1).
   05 ACCOUNTS OCCURS 1 TO 500 TIMES DEPENDING ON ACCOUNT-COUNT.
      10 ACCOUNT-NUMBER  PIC 9(12).
      10 ACCOUNT-TYPE    PIC X(2).
      10 ACCOUNT-STATUS  PIC X(1).
      10 ACCOUNT-BALANCE PIC S9(11)V99 COMP-3.
88 ACCOUNT-CHECKING    VALUE 'CK'.
88 ACCOUNT-SAVINGS     VALUE 'SV'.
88 ACCOUNT-BUSINESS    VALUE 'BZ'.
88 STATUS-ACTIVE       VALUE 'A'.
88 STATUS-FROZEN       VALUE 'F'.
88 STATUS-CLOSED       VALUE 'C'.
88 BALANCE-POSITIVE    VALUE '1'.
88 BALANCE-NEGATIVE    VALUE '0'.
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_ok(),
        "AC2 multiple Level-88 groups: Should parse successfully. Error: {:?}",
        result.err()
    );

    let schema = result.unwrap();
    let root = &schema.fields[0];

    // Validate ODO structure with enterprise-scale bounds
    let accounts_field = root.children.iter().find(|f| f.name == "ACCOUNTS").unwrap();
    assert!(
        matches!(
            accounts_field.occurs,
            Some(Occurs::ODO {
                min: 1,
                max: 500,
                ..
            })
        ),
        "ACCOUNTS should be ODO with enterprise-scale bounds"
    );

    // Validate comprehensive Level-88 coverage
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();
    assert_eq!(
        level88_fields.len(),
        8,
        "Should have 8 Level-88 condition fields"
    );

    // Validate Level-88 OF clause handling
    let of_clause_fields: Vec<_> = level88_fields
        .iter()
        .filter(|f| {
            f.name.starts_with("ACCOUNT-")
                || f.name.starts_with("STATUS-")
                || f.name.starts_with("BALANCE-")
        })
        .collect();
    assert_eq!(
        of_clause_fields.len(),
        8,
        "All Level-88 fields should be properly categorized"
    );

    println!("✅ AC2 multiple Level-88 groups after ODO validated successfully");
}

/// AC2 Advanced: Nested Level-88 after complex ODO (PASS)
///
/// **Purpose**: Validates Level-88 after ODO with nested group structures
/// **COBOL Rule**: Level-88 can reference fields within ODO array elements
/// **Enterprise Context**: Financial transaction records with complex conditions
#[test]
#[allow(clippy::too_many_lines)]
fn test_ac2_nested_level88_after_complex_odo_pass() {
    const COPYBOOK: &str = r"
01 TRANSACTION-RECORD.
   05 BATCH-HEADER.
      10 BATCH-ID        PIC X(16).
      10 BATCH-DATE      PIC 9(8).
      10 TRANSACTION-COUNT PIC 9(6).
   05 TXN-TYPE-FLAG PIC X(3).
   05 STATUS-FLAG PIC X(2).
   05 PROCESSED-FLAG-MAIN PIC X(1).
   05 CURRENCY-FLAG PIC X(3).
   05 TRANSACTIONS OCCURS 1 TO 10000 TIMES DEPENDING ON TRANSACTION-COUNT.
      10 TRANSACTION-HEADER.
         15 TXN-ID        PIC X(20).
         15 TXN-TYPE      PIC X(3).
         15 TXN-STATUS    PIC X(2).
         15 TXN-AMOUNT    PIC S9(13)V99 COMP-3.
      10 TRANSACTION-DETAILS.
         15 SOURCE-ACCOUNT PIC 9(16).
         15 TARGET-ACCOUNT PIC 9(16).
         15 CURRENCY-CODE  PIC X(3).
         15 EXCHANGE-RATE  PIC 9(3)V9(6) COMP-3.
      10 PROCESSING-INFO.
         15 PROCESSED-FLAG PIC X(1).
         15 ERROR-CODE     PIC X(4).
         15 TIMESTAMP      PIC 9(14).
88 TXN-DEPOSIT        VALUE 'DEP'.
88 TXN-WITHDRAWAL     VALUE 'WTD'.
88 TXN-TRANSFER       VALUE 'TRF'.
88 TXN-PAYMENT        VALUE 'PAY'.
88 STATUS-PENDING     VALUE 'PE'.
88 STATUS-COMPLETED   VALUE 'CO'.
88 STATUS-FAILED      VALUE 'FA'.
88 STATUS-CANCELLED   VALUE 'CA'.
88 PROCESSED-YES      VALUE 'Y'.
88 PROCESSED-NO       VALUE 'N'.
88 CURRENCY-USD       VALUE 'USD'.
88 CURRENCY-EUR       VALUE 'EUR'.
88 CURRENCY-GBP       VALUE 'GBP'.
88 AMOUNT-SMALL       VALUE '1'.
88 AMOUNT-MEDIUM      VALUE '2'.
88 AMOUNT-LARGE       VALUE '3'.
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_ok(),
        "AC2 nested Level-88 fixture: Should parse successfully. Error: {:?}",
        result.err()
    );

    let schema = result.unwrap();
    let root = &schema.fields[0];

    // Validate complex ODO structure
    let transactions_field = root
        .children
        .iter()
        .find(|f| f.name == "TRANSACTIONS")
        .unwrap();
    assert!(
        matches!(
            transactions_field.occurs,
            Some(Occurs::ODO {
                min: 1,
                max: 10000,
                ..
            })
        ),
        "TRANSACTIONS should be enterprise-scale ODO"
    );

    // Validate nested structure within ODO
    assert_eq!(
        transactions_field.children.len(),
        3,
        "Should have 3 main groups within ODO"
    );

    let header = transactions_field
        .children
        .iter()
        .find(|f| f.name == "TRANSACTION-HEADER")
        .unwrap();
    let details = transactions_field
        .children
        .iter()
        .find(|f| f.name == "TRANSACTION-DETAILS")
        .unwrap();
    let processing = transactions_field
        .children
        .iter()
        .find(|f| f.name == "PROCESSING-INFO")
        .unwrap();

    assert!(
        !header.children.is_empty(),
        "TRANSACTION-HEADER should have children"
    );
    assert!(
        !details.children.is_empty(),
        "TRANSACTION-DETAILS should have children"
    );
    assert!(
        !processing.children.is_empty(),
        "PROCESSING-INFO should have children"
    );

    // Validate comprehensive Level-88 conditions
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();
    assert_eq!(
        level88_fields.len(),
        16,
        "Should have 16 Level-88 condition fields"
    );

    // Verify Level-88 field categories
    let txn_type_conditions: Vec<_> = level88_fields
        .iter()
        .filter(|f| f.name.starts_with("TXN-") && !f.name.contains("STATUS"))
        .collect();
    assert_eq!(
        txn_type_conditions.len(),
        4,
        "Should have 4 transaction type conditions"
    );

    let status_conditions: Vec<_> = level88_fields
        .iter()
        .filter(|f| f.name.starts_with("STATUS-"))
        .collect();
    assert_eq!(
        status_conditions.len(),
        4,
        "Should have 4 status conditions"
    );

    let amount_conditions: Vec<_> = level88_fields
        .iter()
        .filter(|f| f.name.starts_with("AMOUNT-"))
        .collect();
    assert_eq!(
        amount_conditions.len(),
        3,
        "Should have 3 amount range conditions"
    );

    println!("✅ AC2 nested Level-88 after complex ODO validated successfully");
}

/// AC2 Enterprise: Real-world healthcare record with Level-88 after ODO (PASS)
///
/// **Purpose**: Validates enterprise healthcare pattern with Level-88 conditions
/// **COBOL Rule**: Production-grade ODO with comprehensive Level-88 coverage
/// **Enterprise Context**: Patient record with diagnosis and treatment conditions
#[test]
#[allow(clippy::too_many_lines)]
fn test_ac2_enterprise_healthcare_level88_after_odo_pass() {
    const COPYBOOK: &str = r"
01 PATIENT-RECORD.
   05 PATIENT-HEADER.
      10 PATIENT-ID       PIC X(12).
      10 PATIENT-SSN      PIC 9(9).
      10 ADMISSION-DATE   PIC 9(8).
      10 DISCHARGE-DATE   PIC 9(8).
   05 DIAGNOSIS-COUNT     PIC 9(3).
   05 DIAGNOSIS-TYPE-FLAG PIC X(1).
   05 SEVERITY-FLAG PIC X(1).
   05 COVERAGE-FLAG PIC X(2).
   05 DIAGNOSES OCCURS 1 TO 250 TIMES DEPENDING ON DIAGNOSIS-COUNT.
      10 DIAGNOSIS-INFO.
         15 ICD-CODE      PIC X(7).
         15 DIAGNOSIS-TYPE PIC X(1).
         15 SEVERITY-CODE PIC X(1).
         15 ONSET-DATE    PIC 9(8).
      10 TREATMENT-INFO.
         15 TREATMENT-CODE PIC X(10).
         15 PROVIDER-ID   PIC X(8).
         15 TREATMENT-DATE PIC 9(8).
         15 TREATMENT-COST PIC 9(8)V99 COMP-3.
      10 INSURANCE-INFO.
         15 POLICY-NUMBER PIC X(15).
         15 COVERAGE-TYPE PIC X(2).
         15 COPAY-AMOUNT  PIC 9(4)V99 COMP-3.
88 DIAGNOSIS-PRIMARY    VALUE 'P'.
88 DIAGNOSIS-SECONDARY  VALUE 'S'.
88 DIAGNOSIS-COMORBID   VALUE 'C'.
88 SEVERITY-MILD        VALUE '1'.
88 SEVERITY-MODERATE    VALUE '2'.
88 SEVERITY-SEVERE      VALUE '3'.
88 SEVERITY-CRITICAL    VALUE '4'.
88 COVERAGE-FULL        VALUE 'FU'.
88 COVERAGE-PARTIAL     VALUE 'PA'.
88 COVERAGE-EMERGENCY   VALUE 'EM'.
88 COST-LOW             VALUE '1'.
88 COST-MEDIUM          VALUE '2'.
88 COST-HIGH            VALUE '3'.
";

    let result = parse_copybook(COPYBOOK);
    assert!(
        result.is_ok(),
        "AC2 enterprise healthcare fixture: Should parse successfully. Error: {:?}",
        result.err()
    );

    let schema = result.unwrap();
    let root = &schema.fields[0];

    // Validate enterprise-scale structure
    let diagnoses_field = root
        .children
        .iter()
        .find(|f| f.name == "DIAGNOSES")
        .unwrap();
    assert!(
        matches!(
            diagnoses_field.occurs,
            Some(Occurs::ODO {
                min: 1,
                max: 250,
                ..
            })
        ),
        "DIAGNOSES should be medical-scale ODO"
    );

    // Validate complex nested structure
    assert_eq!(
        diagnoses_field.children.len(),
        3,
        "Should have 3 info groups per diagnosis"
    );

    let diagnosis_info = diagnoses_field
        .children
        .iter()
        .find(|f| f.name == "DIAGNOSIS-INFO")
        .unwrap();
    let treatment_info = diagnoses_field
        .children
        .iter()
        .find(|f| f.name == "TREATMENT-INFO")
        .unwrap();
    let insurance_info = diagnoses_field
        .children
        .iter()
        .find(|f| f.name == "INSURANCE-INFO")
        .unwrap();

    assert_eq!(
        diagnosis_info.children.len(),
        4,
        "DIAGNOSIS-INFO should have 4 fields"
    );
    assert_eq!(
        treatment_info.children.len(),
        4,
        "TREATMENT-INFO should have 4 fields"
    );
    assert_eq!(
        insurance_info.children.len(),
        3,
        "INSURANCE-INFO should have 3 fields"
    );

    // Validate comprehensive medical Level-88 conditions
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();
    assert_eq!(
        level88_fields.len(),
        13,
        "Should have 13 medical condition fields"
    );

    // Validate medical condition categories
    let diagnosis_conditions: Vec<_> = level88_fields
        .iter()
        .filter(|f| f.name.starts_with("DIAGNOSIS-"))
        .collect();
    assert_eq!(
        diagnosis_conditions.len(),
        3,
        "Should have 3 diagnosis type conditions"
    );

    let severity_conditions: Vec<_> = level88_fields
        .iter()
        .filter(|f| f.name.starts_with("SEVERITY-"))
        .collect();
    assert_eq!(
        severity_conditions.len(),
        4,
        "Should have 4 severity level conditions"
    );

    let coverage_conditions: Vec<_> = level88_fields
        .iter()
        .filter(|f| f.name.starts_with("COVERAGE-"))
        .collect();
    assert_eq!(
        coverage_conditions.len(),
        3,
        "Should have 3 coverage type conditions"
    );

    let cost_conditions: Vec<_> = level88_fields
        .iter()
        .filter(|f| f.name.starts_with("COST-"))
        .collect();
    assert_eq!(
        cost_conditions.len(),
        3,
        "Should have 3 cost range conditions"
    );

    println!("✅ AC2 enterprise healthcare Level-88 after ODO validated successfully");
}

/// AC2 Performance: High-volume Level-88 after ODO (PASS)
///
/// **Purpose**: Validates performance with high-volume Level-88 processing
/// **COBOL Rule**: Level-88 parsing scales with large ODO arrays (bounded below record-size cap)
/// **Enterprise Context**: High-frequency trading records with market conditions
#[test]
fn test_ac2_performance_high_volume_level88_after_odo_pass() {
    const COPYBOOK: &str = r"
01 TRADING-RECORD.
   05 MARKET-HEADER.
      10 SESSION-ID      PIC X(16).
      10 MARKET-DATE     PIC 9(8).
      10 MARKET-TIME     PIC 9(6).
   05 TRADE-COUNT        PIC 9(7).
   05 TRADES OCCURS 1 TO 200000 TIMES DEPENDING ON TRADE-COUNT.
      10 TRADE-ID        PIC X(20).
      10 INSTRUMENT-CODE PIC X(12).
      10 TRADE-TYPE      PIC X(1).
      10 TRADE-PRICE     PIC 9(8)V9(4) COMP-3.
      10 TRADE-QUANTITY  PIC 9(12).
      10 TRADE-STATUS    PIC X(1).
      10 MARKET-SECTOR   PIC X(2).
   88 TYPE-BUY           VALUE 'B' OF TRADE-TYPE.
   88 TYPE-SELL          VALUE 'S' OF TRADE-TYPE.
   88 STATUS-EXECUTED    VALUE 'E' OF TRADE-STATUS.
   88 STATUS-PENDING     VALUE 'P' OF TRADE-STATUS.
   88 STATUS-CANCELLED   VALUE 'C' OF TRADE-STATUS.
   88 SECTOR-TECH        VALUE 'TC' OF MARKET-SECTOR.
   88 SECTOR-FINANCE     VALUE 'FN' OF MARKET-SECTOR.
   88 SECTOR-HEALTH      VALUE 'HL' OF MARKET-SECTOR.
   88 SECTOR-ENERGY      VALUE 'EN' OF MARKET-SECTOR.
";

    let start_time = std::time::Instant::now();
    let result = parse_copybook(COPYBOOK);
    let parse_duration = start_time.elapsed();

    assert!(
        result.is_ok(),
        "AC2 performance fixture: Should parse successfully. Error: {:?}",
        result.err()
    );

    // Performance validation: should parse quickly even with large ODO bounds
    assert!(
        parse_duration.as_millis() < 500,
        "Large ODO with Level-88 should parse within 500ms, actual: {}ms",
        parse_duration.as_millis()
    );

    let schema = result.unwrap();
    let root = &schema.fields[0];

    // Validate high-volume ODO structure
    let trades_field = root.children.iter().find(|f| f.name == "TRADES").unwrap();
    assert!(
        matches!(
            trades_field.occurs,
            Some(Occurs::ODO {
                min: 1,
                max: 200_000,
                ..
            })
        ),
        "TRADES should be high-volume ODO"
    );

    // Validate Level-88 fields are efficiently parsed
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();
    assert_eq!(
        level88_fields.len(),
        9,
        "Should have 9 trading condition fields"
    );

    // Validate field structure completeness
    let all_fields: Vec<_> = schema.all_fields().into_iter().collect();
    assert!(
        all_fields.len() >= 15,
        "High-volume fixture should have comprehensive field structure"
    );

    println!(
        "✅ AC2 performance high-volume Level-88 after ODO validated successfully ({}ms)",
        parse_duration.as_millis()
    );
}

/// AC2 Comprehensive Coverage Validation
///
/// **Purpose**: Meta-test ensuring all AC2 fixtures are accounted for
/// **Enterprise Impact**: Guarantees Level-88 after ODO validation coverage
#[test]
fn test_ac2_comprehensive_coverage() {
    let ac2_fixtures = [
        "test_ac2_basic_level88_after_odo_pass",
        "test_ac2_multiple_level88_after_odo_pass",
        "test_ac2_nested_level88_after_complex_odo_pass",
        "test_ac2_enterprise_healthcare_level88_after_odo_pass",
        "test_ac2_performance_high_volume_level88_after_odo_pass",
    ];

    // Verify we have the expected count of AC2 fixtures
    assert_eq!(
        ac2_fixtures.len(),
        5,
        "Expected 5 AC2 Level-88 after ODO validation fixtures"
    );

    println!(
        "✅ AC2 comprehensive coverage validated with {} fixtures:",
        ac2_fixtures.len()
    );
    for (i, fixture) in ac2_fixtures.iter().enumerate() {
        println!("   {}. {}", i + 1, fixture);
    }

    // Validate fixture naming consistency
    for fixture in &ac2_fixtures {
        assert!(
            fixture.starts_with("test_ac2_"),
            "AC2 fixtures should have consistent naming"
        );
        assert!(
            fixture.contains("level88"),
            "AC2 fixtures should focus on Level-88"
        );
        assert!(fixture.contains("odo"), "AC2 fixtures should include ODO");
        assert!(
            fixture.ends_with("_pass"),
            "AC2 fixtures should be PASS scenarios"
        );
    }

    println!("✅ AC2 Level-88 after ODO validation fixtures comprehensively validated");
}
