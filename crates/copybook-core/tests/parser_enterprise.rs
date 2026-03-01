// SPDX-License-Identifier: AGPL-3.0-or-later
//! Enterprise-scale parser tests for copybook-core.
//!
//! Tests realistic COBOL copybook patterns from banking, insurance, retail,
//! and payroll domains with 20–30+ fields, COMP-3, COMP, REDEFINES, OCCURS,
//! ODO, Level-88, SIGN SEPARATE, and complex nesting.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use copybook_core::{
    FieldKind, Occurs, Schema, SignPlacement,
    feature_flags::{Feature, FeatureFlags},
    parse_copybook,
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn find_field<'a>(schema: &'a Schema, name: &str) -> &'a copybook_core::Field {
    schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("field '{name}' not found in schema"))
}

fn count_all_fields(schema: &Schema) -> usize {
    schema.all_fields().len()
}

fn enable_sign_separate() {
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::SignSeparate);
    FeatureFlags::set_global(flags);
}

// ===========================================================================
// Copybook definitions
// ===========================================================================

const BANKING_TXN: &str = r"
       01  BANK-TRANSACTION-RECORD.
           05  TXN-HEADER.
               10  TXN-ID                PIC X(16).
               10  TXN-DATE              PIC 9(8).
               10  TXN-TIME              PIC 9(6).
               10  TXN-TYPE-CODE         PIC X(2).
                   88  TXN-DEBIT         VALUE 'DB'.
                   88  TXN-CREDIT        VALUE 'CR'.
                   88  TXN-TRANSFER      VALUE 'TF'.
                   88  TXN-REVERSAL      VALUE 'RV'.
               10  TXN-STATUS            PIC X(1).
                   88  STATUS-PENDING     VALUE 'P'.
                   88  STATUS-COMPLETE    VALUE 'C'.
                   88  STATUS-FAILED      VALUE 'F'.
           05  ACCOUNT-INFO.
               10  ACCT-NUMBER           PIC X(12).
               10  ROUTING-NUMBER        PIC 9(9).
               10  ACCT-TYPE             PIC X(2).
               10  ACCT-CURRENCY         PIC X(3).
           05  AMOUNT-BLOCK.
               10  TXN-AMOUNT            PIC S9(11)V99 COMP-3.
               10  TXN-FEE               PIC S9(7)V99 COMP-3.
               10  TXN-TAX               PIC S9(7)V99 COMP-3.
               10  RUNNING-BALANCE       PIC S9(13)V99 COMP-3.
           05  COUNTERPARTY.
               10  CP-NAME               PIC X(30).
               10  CP-ACCT               PIC X(12).
               10  CP-ROUTING            PIC 9(9).
           05  TXN-DESCRIPTION           PIC X(40).
           05  AUTH-CODE                  PIC X(6).
           05  BRANCH-ID                 PIC 9(4).
           05  TELLER-ID                 PIC X(8).
           05  TXN-LINE-COUNT            PIC 9(2) COMP.
           05  TXN-LINES OCCURS 5 TIMES.
               10  LINE-SEQ              PIC 9(3).
               10  LINE-AMOUNT           PIC S9(9)V99 COMP-3.
               10  LINE-DESC             PIC X(20).
           05  TXN-DETAIL-BLOCK.
               10  DETAIL-TYPE-A.
                   15  CHECK-NUMBER      PIC 9(10).
                   15  CHECK-DATE        PIC 9(8).
               10  DETAIL-TYPE-B         REDEFINES DETAIL-TYPE-A.
                   15  WIRE-REF          PIC X(16).
                   15  SWIFT-CODE        PIC X(2).
           05  BATCH-REFERENCE           PIC X(10).
";

const INSURANCE_CLAIM: &str = r"
       01  INSURANCE-CLAIM-RECORD.
           05  CLAIM-HEADER.
               10  CLAIM-NUMBER          PIC X(12).
               10  POLICY-NUMBER         PIC X(10).
               10  CLAIM-DATE            PIC 9(8).
               10  RECEIVED-DATE         PIC 9(8).
               10  CLAIM-TYPE            PIC X(2).
                   88  TYPE-MEDICAL      VALUE 'MD'.
                   88  TYPE-DENTAL       VALUE 'DN'.
                   88  TYPE-VISION       VALUE 'VS'.
                   88  TYPE-PHARMACY     VALUE 'RX'.
               10  CLAIM-STATUS          PIC X(1).
                   88  STAT-NEW          VALUE 'N'.
                   88  STAT-REVIEW       VALUE 'R'.
                   88  STAT-APPROVED     VALUE 'A'.
                   88  STAT-DENIED       VALUE 'D'.
                   88  STAT-APPEALED     VALUE 'P'.
           05  CLAIMANT-INFO.
               10  CLAIMANT-ID           PIC X(10).
               10  CLAIMANT-LAST-NAME    PIC X(25).
               10  CLAIMANT-FIRST-NAME   PIC X(20).
               10  CLAIMANT-DOB          PIC 9(8).
               10  CLAIMANT-GENDER       PIC X(1).
               10  MEMBER-GROUP-ID       PIC X(8).
           05  PROVIDER-INFO.
               10  PROVIDER-NPI          PIC 9(10).
               10  PROVIDER-NAME         PIC X(35).
               10  PROVIDER-TAX-ID       PIC 9(9).
               10  PROVIDER-SPEC-CODE    PIC X(3).
               10  FACILITY-TYPE         PIC X(2).
           05  DIAGNOSIS-GROUP.
               10  PRIMARY-DIAG-CODE     PIC X(7).
               10  SECONDARY-DIAG-CODE   PIC X(7).
               10  PROCEDURE-CODE        PIC X(5).
               10  PLACE-OF-SERVICE      PIC X(2).
           05  FINANCIAL-SUMMARY.
               10  BILLED-AMOUNT         PIC S9(9)V99 COMP-3.
               10  ALLOWED-AMOUNT        PIC S9(9)V99 COMP-3.
               10  PAID-AMOUNT           PIC S9(9)V99 COMP-3.
               10  COPAY-AMOUNT          PIC S9(7)V99 COMP-3.
               10  DEDUCTIBLE-AMT        PIC S9(7)V99 COMP-3.
               10  COINSURANCE-AMT       PIC S9(7)V99 COMP-3.
           05  LINE-ITEM-COUNT           PIC 9(2).
           05  CLAIM-LINES OCCURS 1 TO 20 TIMES
                   DEPENDING ON LINE-ITEM-COUNT.
               10  SVC-LINE-NUM          PIC 9(2).
               10  SVC-DATE-FROM         PIC 9(8).
               10  SVC-DATE-TO           PIC 9(8).
               10  SVC-PROCEDURE         PIC X(5).
               10  SVC-MODIFIER          PIC X(2).
               10  SVC-BILLED            PIC S9(7)V99 COMP-3.
               10  SVC-ALLOWED           PIC S9(7)V99 COMP-3.
               10  SVC-PAID              PIC S9(7)V99 COMP-3.
               10  SVC-UNITS             PIC 9(3).
               10  SVC-PLACE-OF-SVC      PIC X(2).
";

const RETAIL_POS: &str = r"
       01  POS-TRANSACTION.
           05  STORE-ID                  PIC 9(5).
           05  REGISTER-NUM              PIC 9(3).
           05  TXN-SEQUENCE              PIC 9(8).
           05  TXN-TIMESTAMP             PIC 9(14).
           05  CASHIER-ID                PIC X(8).
           05  CUSTOMER-CARD             PIC X(16).
           05  ITEM-COUNT                PIC 9(3) COMP.
           05  ITEMS OCCURS 10 TIMES.
               10  SKU                   PIC X(12).
               10  ITEM-DESC             PIC X(25).
               10  UNIT-PRICE            PIC S9(5)V99 COMP-3.
               10  QUANTITY              PIC 9(5) COMP.
               10  DISCOUNT-PCT          PIC 9V99 COMP-3.
               10  LINE-TOTAL            PIC S9(7)V99 COMP-3.
               10  TAX-CODE              PIC X(1).
           05  PAYMENT-INFO.
               10  PAYMENT-METHOD        PIC X(2).
                   88  PAY-CASH          VALUE 'CA'.
                   88  PAY-CREDIT        VALUE 'CC'.
                   88  PAY-DEBIT         VALUE 'DC'.
                   88  PAY-MOBILE        VALUE 'MB'.
               10  CARD-LAST-FOUR        PIC X(4).
               10  AUTH-RESPONSE         PIC X(6).
           05  TOTALS.
               10  SUBTOTAL              PIC S9(7)V99 COMP-3.
               10  TAX-TOTAL             PIC S9(7)V99 COMP-3.
               10  DISCOUNT-TOTAL        PIC S9(7)V99 COMP-3.
               10  GRAND-TOTAL           PIC S9(7)V99 COMP-3.
               10  TENDERED              PIC S9(7)V99 COMP-3.
               10  CHANGE-DUE            PIC S9(7)V99 COMP-3.
           05  LOYALTY-POINTS            PIC 9(7) COMP.
           05  RECEIPT-NUMBER            PIC X(12).
";

const PAYROLL_RECORD: &str = r"
       01  PAYROLL-RECORD.
           05  EMPLOYEE-INFO.
               10  EMPLOYEE-ID           PIC X(10).
               10  SSN                   PIC 9(9).
               10  LAST-NAME             PIC X(25).
               10  FIRST-NAME            PIC X(20).
               10  DEPT-CODE             PIC X(4).
               10  JOB-CODE              PIC X(6).
               10  PAY-GRADE             PIC X(2).
               10  HIRE-DATE             PIC 9(8).
               10  TERM-DATE             PIC 9(8).
               10  EMP-STATUS            PIC X(1).
                   88  ACTIVE            VALUE 'A'.
                   88  ON-LEAVE          VALUE 'L'.
                   88  TERMINATED        VALUE 'T'.
           05  PAY-PERIOD-INFO.
               10  PERIOD-START          PIC 9(8).
               10  PERIOD-END            PIC 9(8).
               10  PAY-FREQUENCY         PIC X(1).
                   88  FREQ-WEEKLY       VALUE 'W'.
                   88  FREQ-BIWEEKLY     VALUE 'B'.
                   88  FREQ-MONTHLY      VALUE 'M'.
               10  CHECK-DATE            PIC 9(8).
               10  CHECK-NUMBER          PIC 9(8).
           05  EARNINGS.
               10  REGULAR-HOURS         PIC 9(3)V99 COMP-3.
               10  OVERTIME-HOURS        PIC 9(3)V99 COMP-3.
               10  REGULAR-RATE          PIC S9(5)V9999
                       SIGN IS LEADING SEPARATE.
               10  OVERTIME-RATE         PIC S9(5)V9999
                       SIGN IS LEADING SEPARATE.
               10  REGULAR-PAY           PIC S9(7)V99 COMP-3.
               10  OVERTIME-PAY          PIC S9(7)V99 COMP-3.
               10  BONUS-PAY             PIC S9(7)V99 COMP-3.
               10  COMMISSION-PAY        PIC S9(7)V99 COMP-3.
               10  GROSS-PAY             PIC S9(9)V99 COMP-3.
           05  DEDUCTIONS.
               10  FED-TAX              PIC S9(7)V99 COMP-3.
               10  STATE-TAX            PIC S9(7)V99 COMP-3.
               10  LOCAL-TAX            PIC S9(5)V99 COMP-3.
               10  FICA-SS              PIC S9(7)V99 COMP-3.
               10  FICA-MED             PIC S9(7)V99 COMP-3.
               10  HEALTH-INS           PIC S9(5)V99 COMP-3.
               10  DENTAL-INS           PIC S9(5)V99 COMP-3.
               10  VISION-INS           PIC S9(3)V99 COMP-3.
               10  RETIREMENT-401K      PIC S9(7)V99 COMP-3.
               10  OTHER-DEDUCT         PIC S9(5)V99 COMP-3.
               10  TOTAL-DEDUCTIONS     PIC S9(9)V99 COMP-3.
           05  NET-PAY                   PIC S9(9)V99 COMP-3.
           05  YTD-SUMMARY.
               10  YTD-GROSS            PIC S9(9)V99 COMP-3.
               10  YTD-FED-TAX          PIC S9(9)V99 COMP-3.
               10  YTD-STATE-TAX        PIC S9(9)V99 COMP-3.
               10  YTD-FICA-SS          PIC S9(9)V99 COMP-3.
               10  YTD-FICA-MED         PIC S9(9)V99 COMP-3.
               10  YTD-NET              PIC S9(9)V99 COMP-3.
";

// ===========================================================================
// Banking – parse & structural tests
// ===========================================================================

#[test]
fn banking_parses_without_error() {
    parse_copybook(BANKING_TXN).expect("banking copybook should parse");
}

#[test]
fn banking_lrecl() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    // Fixed-length: no ODO present
    // Header: 16+8+6+2+1 = 33
    // Account: 12+9+2+3 = 26
    // Amounts: 7+5+5+8 = 25  (COMP-3 sizes: 11V99=7, 7V99=5, 7V99=5, 13V99=8)
    // Counterparty: 30+12+9 = 51
    // Desc+Auth+Branch+Teller: 40+6+4+8 = 58
    // LineCount: 2 (COMP PIC 9(2) → 2 bytes)
    // Lines: 5 × (3 + 6 + 20) = 145  (9V99 COMP-3 = 6 bytes)
    // Detail: max(10+8, 16+2) = 18
    // Batch: 10
    // Total: 33+26+25+51+58+2+145+18+10 = 368
    let lrecl = schema
        .lrecl_fixed
        .expect("banking record should have fixed LRECL");
    assert_eq!(lrecl, 368, "banking LRECL mismatch");
}

#[test]
fn banking_field_count() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    let total = count_all_fields(&schema);
    // Must have 20+ data fields, plus groups, plus Level-88s
    assert!(total >= 30, "expected at least 30 fields, got {total}");
}

#[test]
fn banking_txn_amount_is_comp3() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    let f = find_field(&schema, "TXN-AMOUNT");
    match &f.kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(*digits, 13);
            assert_eq!(*scale, 2);
            assert!(*signed);
        }
        other => panic!("expected PackedDecimal for TXN-AMOUNT, got {other:?}"),
    }
    assert_eq!(f.len, 7, "S9(11)V99 COMP-3 = (13+1)/2 = 7 bytes");
}

#[test]
fn banking_running_balance_comp3() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    let f = find_field(&schema, "RUNNING-BALANCE");
    match &f.kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(*digits, 15);
            assert_eq!(*scale, 2);
            assert!(*signed);
        }
        other => panic!("expected PackedDecimal for RUNNING-BALANCE, got {other:?}"),
    }
    assert_eq!(f.len, 8, "S9(13)V99 COMP-3 = (15+1)/2 = 8 bytes");
}

#[test]
fn banking_level88_txn_type() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    for name in &["TXN-DEBIT", "TXN-CREDIT", "TXN-TRANSFER", "TXN-REVERSAL"] {
        let f = find_field(&schema, name);
        assert_eq!(f.level, 88, "{name} should be level 88");
        assert!(
            matches!(&f.kind, FieldKind::Condition { values } if values.len() == 1),
            "{name} should have exactly one condition value"
        );
    }
}

#[test]
fn banking_level88_status() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    for name in &["STATUS-PENDING", "STATUS-COMPLETE", "STATUS-FAILED"] {
        let f = find_field(&schema, name);
        assert_eq!(f.level, 88);
    }
}

#[test]
fn banking_occurs_lines() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    let lines = find_field(&schema, "TXN-LINES");
    assert!(
        matches!(&lines.occurs, Some(Occurs::Fixed { count: 5 })),
        "TXN-LINES should OCCURS 5 TIMES"
    );
    // Each line: 3 + 6 + 20 = 29 bytes (base element length)
    assert_eq!(lines.len, 29, "TXN-LINES per-element length mismatch");
}

#[test]
fn banking_redefines_detail() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    let type_a = find_field(&schema, "DETAIL-TYPE-A");
    let type_b = find_field(&schema, "DETAIL-TYPE-B");
    assert_eq!(
        type_b.redefines_of.as_deref(),
        Some("DETAIL-TYPE-A"),
        "DETAIL-TYPE-B should REDEFINE DETAIL-TYPE-A"
    );
    assert_eq!(
        type_a.offset, type_b.offset,
        "REDEFINES should share offset"
    );
    assert_eq!(type_a.len, type_b.len, "REDEFINES should share length");
}

#[test]
fn banking_line_count_is_comp() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    let f = find_field(&schema, "TXN-LINE-COUNT");
    assert!(
        matches!(&f.kind, FieldKind::BinaryInt { signed: false, .. }),
        "TXN-LINE-COUNT should be COMP (binary)"
    );
}

#[test]
fn banking_alphanumeric_fields() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    let checks = [
        ("TXN-ID", 16u32),
        ("ACCT-NUMBER", 12),
        ("CP-NAME", 30),
        ("TXN-DESCRIPTION", 40),
        ("AUTH-CODE", 6),
        ("TELLER-ID", 8),
        ("BATCH-REFERENCE", 10),
    ];
    for (name, expected_len) in &checks {
        let f = find_field(&schema, name);
        assert!(
            matches!(&f.kind, FieldKind::Alphanum { len } if *len == *expected_len),
            "{name}: expected Alphanum(len={expected_len}), got {:?}",
            f.kind,
        );
    }
}

// ===========================================================================
// Insurance – parse & structural tests
// ===========================================================================

#[test]
fn insurance_parses_without_error() {
    parse_copybook(INSURANCE_CLAIM).expect("insurance copybook should parse");
}

#[test]
fn insurance_has_odo() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    // CLAIM-LINES is ODO → no fixed LRECL
    assert!(
        schema.lrecl_fixed.is_none(),
        "insurance record should have variable LRECL due to ODO"
    );
    assert!(
        schema.tail_odo.is_some(),
        "insurance record should have tail ODO info"
    );
}

#[test]
fn insurance_odo_parameters() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    let lines = find_field(&schema, "CLAIM-LINES");
    match &lines.occurs {
        Some(Occurs::ODO {
            min,
            max,
            counter_path,
        }) => {
            assert_eq!(*min, 1, "ODO min should be 1");
            assert_eq!(*max, 20, "ODO max should be 20");
            assert!(
                counter_path.contains("LINE-ITEM-COUNT"),
                "ODO counter should reference LINE-ITEM-COUNT, got '{counter_path}'"
            );
        }
        other => panic!("expected ODO for CLAIM-LINES, got {other:?}"),
    }
}

#[test]
fn insurance_field_count() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    let total = count_all_fields(&schema);
    assert!(total >= 35, "expected at least 35 fields, got {total}");
}

#[test]
fn insurance_level88_claim_type() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    for name in &[
        "TYPE-MEDICAL",
        "TYPE-DENTAL",
        "TYPE-VISION",
        "TYPE-PHARMACY",
    ] {
        let f = find_field(&schema, name);
        assert_eq!(f.level, 88, "{name} should be level 88");
    }
}

#[test]
fn insurance_level88_claim_status() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    for name in &[
        "STAT-NEW",
        "STAT-REVIEW",
        "STAT-APPROVED",
        "STAT-DENIED",
        "STAT-APPEALED",
    ] {
        let f = find_field(&schema, name);
        assert_eq!(f.level, 88, "{name} should be level 88");
    }
}

#[test]
fn insurance_financial_comp3() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    let large_amounts = ["BILLED-AMOUNT", "ALLOWED-AMOUNT", "PAID-AMOUNT"];
    for name in &large_amounts {
        let f = find_field(&schema, name);
        match &f.kind {
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                assert_eq!(*digits, 11, "{name} digits");
                assert_eq!(*scale, 2, "{name} scale");
                assert!(*signed, "{name} should be signed");
            }
            other => panic!("{name}: expected PackedDecimal, got {other:?}"),
        }
        assert_eq!(f.len, 6, "{name}: S9(9)V99 COMP-3 = (11+1)/2 = 6 bytes");
    }
}

#[test]
fn insurance_copay_deductible_comp3() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    for name in &["COPAY-AMOUNT", "DEDUCTIBLE-AMT", "COINSURANCE-AMT"] {
        let f = find_field(&schema, name);
        match &f.kind {
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                assert_eq!(*digits, 9, "{name} digits");
                assert_eq!(*scale, 2, "{name} scale");
                assert!(*signed, "{name} should be signed");
            }
            other => panic!("{name}: expected PackedDecimal, got {other:?}"),
        }
        assert_eq!(f.len, 5, "{name}: S9(7)V99 COMP-3 = (9+1)/2 = 5 bytes");
    }
}

#[test]
fn insurance_nested_groups() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    let claimant = find_field(&schema, "CLAIMANT-INFO");
    assert!(
        matches!(&claimant.kind, FieldKind::Group),
        "CLAIMANT-INFO should be a Group"
    );
    assert_eq!(
        claimant.children.len(),
        6,
        "CLAIMANT-INFO should have 6 children"
    );

    let provider = find_field(&schema, "PROVIDER-INFO");
    assert_eq!(
        provider.children.len(),
        5,
        "PROVIDER-INFO should have 5 children"
    );

    let diag = find_field(&schema, "DIAGNOSIS-GROUP");
    assert_eq!(
        diag.children.len(),
        4,
        "DIAGNOSIS-GROUP should have 4 children"
    );
}

#[test]
fn insurance_svc_line_fields() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    let svc_billed = find_field(&schema, "SVC-BILLED");
    assert!(
        matches!(
            &svc_billed.kind,
            FieldKind::PackedDecimal { signed: true, .. }
        ),
        "SVC-BILLED should be signed PackedDecimal"
    );
    let svc_units = find_field(&schema, "SVC-UNITS");
    assert!(
        matches!(
            &svc_units.kind,
            FieldKind::ZonedDecimal {
                digits: 3,
                scale: 0,
                signed: false,
                ..
            }
        ),
        "SVC-UNITS should be unsigned ZonedDecimal(3)"
    );
}

// ===========================================================================
// Retail POS – parse & structural tests
// ===========================================================================

#[test]
fn retail_parses_without_error() {
    parse_copybook(RETAIL_POS).expect("retail copybook should parse");
}

#[test]
fn retail_lrecl() {
    let schema = parse_copybook(RETAIL_POS).unwrap();
    // StoreID(5)+Register(3)+Seq(8)+Timestamp(14)+Cashier(8)+Card(16) = 54
    // ItemCount: 2 (COMP PIC 9(3))
    // Items: 10 × (12+25+4+2+2+5+1) = 510
    //   UNIT-PRICE: S9(5)V99 COMP-3 = (7+1)/2 = 4
    //   QUANTITY: PIC 9(5) COMP = 2 (halfword? 4? — depends on dialect, check)
    //   DISCOUNT-PCT: PIC 9V99 COMP-3 = (3+1)/2 = 2
    //   LINE-TOTAL: S9(7)V99 COMP-3 = (9+1)/2 = 5
    // Payment: 2+4+6 = 12
    // Totals: 6 × 5 = 30  (each S9(7)V99 COMP-3 = 5)
    // Loyalty: 4 (COMP PIC 9(7))
    // Receipt: 12
    let lrecl = schema.lrecl_fixed.expect("retail should have fixed LRECL");
    // Verify it is positive and reasonable
    assert!(lrecl > 400, "retail LRECL should be > 400, got {lrecl}");
    assert!(lrecl < 1000, "retail LRECL should be < 1000, got {lrecl}");
}

#[test]
fn retail_field_count() {
    let schema = parse_copybook(RETAIL_POS).unwrap();
    let total = count_all_fields(&schema);
    assert!(total >= 20, "expected at least 20 fields, got {total}");
}

#[test]
fn retail_item_occurs() {
    let schema = parse_copybook(RETAIL_POS).unwrap();
    let items = find_field(&schema, "ITEMS");
    assert!(
        matches!(&items.occurs, Some(Occurs::Fixed { count: 10 })),
        "ITEMS should OCCURS 10 TIMES"
    );
}

#[test]
fn retail_comp3_currency_fields() {
    let schema = parse_copybook(RETAIL_POS).unwrap();
    for name in &[
        "SUBTOTAL",
        "TAX-TOTAL",
        "DISCOUNT-TOTAL",
        "GRAND-TOTAL",
        "TENDERED",
        "CHANGE-DUE",
    ] {
        let f = find_field(&schema, name);
        match &f.kind {
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => {
                assert_eq!(*digits, 9, "{name} digits");
                assert_eq!(*scale, 2, "{name} scale");
                assert!(*signed, "{name} should be signed");
            }
            other => panic!("{name}: expected PackedDecimal, got {other:?}"),
        }
        assert_eq!(f.len, 5, "{name}: S9(7)V99 COMP-3 = 5 bytes");
    }
}

#[test]
fn retail_comp_integer_fields() {
    let schema = parse_copybook(RETAIL_POS).unwrap();
    let item_count = find_field(&schema, "ITEM-COUNT");
    assert!(
        matches!(&item_count.kind, FieldKind::BinaryInt { signed: false, .. }),
        "ITEM-COUNT should be BinaryInt (COMP)"
    );

    let loyalty = find_field(&schema, "LOYALTY-POINTS");
    assert!(
        matches!(&loyalty.kind, FieldKind::BinaryInt { signed: false, .. }),
        "LOYALTY-POINTS should be BinaryInt (COMP)"
    );
}

#[test]
fn retail_level88_payment() {
    let schema = parse_copybook(RETAIL_POS).unwrap();
    for name in &["PAY-CASH", "PAY-CREDIT", "PAY-DEBIT", "PAY-MOBILE"] {
        let f = find_field(&schema, name);
        assert_eq!(f.level, 88, "{name} should be level 88");
    }
}

#[test]
fn retail_mixed_field_types() {
    let schema = parse_copybook(RETAIL_POS).unwrap();
    // Alphanumeric
    let cashier = find_field(&schema, "CASHIER-ID");
    assert!(matches!(&cashier.kind, FieldKind::Alphanum { len: 8 }));

    // Zoned decimal
    let store = find_field(&schema, "STORE-ID");
    assert!(matches!(
        &store.kind,
        FieldKind::ZonedDecimal {
            digits: 5,
            signed: false,
            ..
        }
    ));

    // Groups
    let totals = find_field(&schema, "TOTALS");
    assert!(matches!(&totals.kind, FieldKind::Group));
}

// ===========================================================================
// Payroll – parse & structural tests
// ===========================================================================

#[test]
fn payroll_parses_without_error() {
    enable_sign_separate();
    parse_copybook(PAYROLL_RECORD).expect("payroll copybook should parse");
}

#[test]
fn payroll_lrecl() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let lrecl = schema.lrecl_fixed.expect("payroll should have fixed LRECL");
    // Should be large – verify reasonable bounds
    assert!(lrecl > 150, "payroll LRECL should be > 150, got {lrecl}");
    assert!(lrecl < 500, "payroll LRECL should be < 500, got {lrecl}");
}

#[test]
fn payroll_field_count() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let total = count_all_fields(&schema);
    assert!(total >= 40, "expected at least 40 fields, got {total}");
}

#[test]
fn payroll_sign_separate_regular_rate() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let f = find_field(&schema, "REGULAR-RATE");
    match &f.kind {
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            sign_separate,
        } => {
            assert_eq!(*digits, 9, "REGULAR-RATE total digits");
            assert_eq!(*scale, 4, "REGULAR-RATE scale");
            assert!(*signed, "REGULAR-RATE should be signed");
            let sep = sign_separate
                .as_ref()
                .expect("REGULAR-RATE should have SIGN SEPARATE");
            assert_eq!(
                sep.placement,
                SignPlacement::Leading,
                "REGULAR-RATE sign should be LEADING"
            );
        }
        other => panic!("expected ZonedDecimal for REGULAR-RATE, got {other:?}"),
    }
    // S9(5)V9999 SIGN LEADING SEPARATE = 9 digits + 1 sign byte = 10
    assert_eq!(f.len, 10, "REGULAR-RATE with SIGN SEPARATE = 10 bytes");
}

#[test]
fn payroll_sign_separate_overtime_rate() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let f = find_field(&schema, "OVERTIME-RATE");
    match &f.kind {
        FieldKind::ZonedDecimal {
            signed: true,
            sign_separate: Some(info),
            ..
        } => {
            assert_eq!(info.placement, SignPlacement::Leading);
        }
        other => {
            panic!("expected ZonedDecimal with SIGN SEPARATE for OVERTIME-RATE, got {other:?}")
        }
    }
    assert_eq!(f.len, 10, "OVERTIME-RATE with SIGN SEPARATE = 10 bytes");
}

#[test]
fn payroll_level88_emp_status() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    for name in &["ACTIVE", "ON-LEAVE", "TERMINATED"] {
        let f = find_field(&schema, name);
        assert_eq!(f.level, 88, "{name} should be level 88");
    }
}

#[test]
fn payroll_level88_pay_frequency() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    for name in &["FREQ-WEEKLY", "FREQ-BIWEEKLY", "FREQ-MONTHLY"] {
        let f = find_field(&schema, name);
        assert_eq!(f.level, 88, "{name} should be level 88");
    }
}

#[test]
fn payroll_comp3_earnings() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let gross = find_field(&schema, "GROSS-PAY");
    match &gross.kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(*digits, 11);
            assert_eq!(*scale, 2);
            assert!(*signed);
        }
        other => panic!("expected PackedDecimal for GROSS-PAY, got {other:?}"),
    }
    assert_eq!(gross.len, 6, "S9(9)V99 COMP-3 = (11+1)/2 = 6 bytes");
}

#[test]
fn payroll_comp3_deductions() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    for name in &[
        "FED-TAX",
        "STATE-TAX",
        "FICA-SS",
        "FICA-MED",
        "RETIREMENT-401K",
    ] {
        let f = find_field(&schema, name);
        assert!(
            matches!(&f.kind, FieldKind::PackedDecimal { signed: true, .. }),
            "{name} should be signed PackedDecimal"
        );
    }
}

#[test]
fn payroll_total_deductions_comp3() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let f = find_field(&schema, "TOTAL-DEDUCTIONS");
    match &f.kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(*digits, 11, "TOTAL-DEDUCTIONS digits");
            assert_eq!(*scale, 2, "TOTAL-DEDUCTIONS scale");
            assert!(*signed, "TOTAL-DEDUCTIONS should be signed");
        }
        other => panic!("expected PackedDecimal for TOTAL-DEDUCTIONS, got {other:?}"),
    }
}

#[test]
fn payroll_net_pay_comp3() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let f = find_field(&schema, "NET-PAY");
    assert!(
        matches!(
            &f.kind,
            FieldKind::PackedDecimal {
                digits: 11,
                scale: 2,
                signed: true
            }
        ),
        "NET-PAY should be S9(9)V99 COMP-3"
    );
    assert_eq!(f.len, 6);
}

#[test]
fn payroll_nested_groups() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let emp = find_field(&schema, "EMPLOYEE-INFO");
    assert!(matches!(&emp.kind, FieldKind::Group));
    // 10 data fields + 3 Level-88s = 13 children
    assert!(
        emp.children.len() >= 10,
        "EMPLOYEE-INFO should have at least 10 children, got {}",
        emp.children.len()
    );

    let earnings = find_field(&schema, "EARNINGS");
    assert!(matches!(&earnings.kind, FieldKind::Group));
    assert!(earnings.children.len() >= 9);

    let deductions = find_field(&schema, "DEDUCTIONS");
    assert!(matches!(&deductions.kind, FieldKind::Group));
    assert!(deductions.children.len() >= 11);
}

#[test]
fn payroll_ytd_fields() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    for name in &[
        "YTD-GROSS",
        "YTD-FED-TAX",
        "YTD-STATE-TAX",
        "YTD-FICA-SS",
        "YTD-FICA-MED",
        "YTD-NET",
    ] {
        let f = find_field(&schema, name);
        match &f.kind {
            FieldKind::PackedDecimal {
                digits: 11,
                scale: 2,
                signed: true,
            } => {}
            other => panic!("{name}: expected S9(9)V99 COMP-3, got {other:?}"),
        }
        assert_eq!(f.len, 6, "{name}: S9(9)V99 COMP-3 = 6 bytes");
    }
}

// ===========================================================================
// Cross-cutting / roundtrip tests
// ===========================================================================

#[test]
fn roundtrip_banking_json_parse() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    let json = serde_json::to_string(&schema).expect("schema should serialize to JSON");
    let deserialized: Schema =
        serde_json::from_str(&json).expect("JSON should deserialize back to Schema");

    assert_eq!(schema.lrecl_fixed, deserialized.lrecl_fixed);
    assert_eq!(schema.fields.len(), deserialized.fields.len());
    assert_eq!(
        count_all_fields(&schema),
        count_all_fields(&deserialized),
        "roundtrip field count mismatch"
    );
}

#[test]
fn roundtrip_insurance_json_parse() {
    let schema = parse_copybook(INSURANCE_CLAIM).unwrap();
    let json = serde_json::to_string(&schema).expect("schema should serialize to JSON");
    let deserialized: Schema =
        serde_json::from_str(&json).expect("JSON should deserialize back to Schema");

    assert_eq!(schema.lrecl_fixed, deserialized.lrecl_fixed);
    assert_eq!(schema.fields.len(), deserialized.fields.len());

    // Verify ODO survives roundtrip
    let orig_lines = find_field(&schema, "CLAIM-LINES");
    let rt_lines = find_field(&deserialized, "CLAIM-LINES");
    assert!(matches!(&orig_lines.occurs, Some(Occurs::ODO { .. })));
    assert!(matches!(&rt_lines.occurs, Some(Occurs::ODO { .. })));
}

#[test]
fn roundtrip_retail_json_parse() {
    let schema = parse_copybook(RETAIL_POS).unwrap();
    let json = serde_json::to_string(&schema).expect("schema should serialize to JSON");
    let deserialized: Schema =
        serde_json::from_str(&json).expect("JSON should deserialize back to Schema");

    assert_eq!(schema.lrecl_fixed, deserialized.lrecl_fixed);
    assert_eq!(count_all_fields(&schema), count_all_fields(&deserialized),);
}

#[test]
fn roundtrip_payroll_json_parse() {
    enable_sign_separate();
    let schema = parse_copybook(PAYROLL_RECORD).unwrap();
    let json = serde_json::to_string(&schema).expect("schema should serialize to JSON");
    let deserialized: Schema =
        serde_json::from_str(&json).expect("JSON should deserialize back to Schema");

    assert_eq!(schema.lrecl_fixed, deserialized.lrecl_fixed);
    assert_eq!(schema.fields.len(), deserialized.fields.len());

    // Verify SIGN SEPARATE survives roundtrip
    let orig_rate = find_field(&schema, "REGULAR-RATE");
    let rt_rate = find_field(&deserialized, "REGULAR-RATE");
    assert!(matches!(
        &orig_rate.kind,
        FieldKind::ZonedDecimal {
            sign_separate: Some(_),
            ..
        }
    ));
    assert!(matches!(
        &rt_rate.kind,
        FieldKind::ZonedDecimal {
            sign_separate: Some(_),
            ..
        }
    ));
}

// ===========================================================================
// Fingerprint consistency tests
// ===========================================================================

#[test]
fn fingerprint_banking_nonempty() {
    let schema = parse_copybook(BANKING_TXN).unwrap();
    assert!(
        !schema.fingerprint.is_empty(),
        "banking schema should have a fingerprint"
    );
}

#[test]
fn fingerprint_deterministic() {
    let s1 = parse_copybook(BANKING_TXN).unwrap();
    let s2 = parse_copybook(BANKING_TXN).unwrap();
    assert_eq!(
        s1.fingerprint, s2.fingerprint,
        "parsing the same copybook twice should produce identical fingerprints"
    );
}

#[test]
fn fingerprint_differs_across_schemas() {
    let s_bank = parse_copybook(BANKING_TXN).unwrap();
    let s_ins = parse_copybook(INSURANCE_CLAIM).unwrap();
    assert_ne!(
        s_bank.fingerprint, s_ins.fingerprint,
        "different schemas should produce different fingerprints"
    );
}
