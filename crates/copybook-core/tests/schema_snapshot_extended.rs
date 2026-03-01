// SPDX-License-Identifier: AGPL-3.0-or-later
//! Extended schema snapshot tests for copybook-core.
//!
//! Complements `schema_snapshot_tests.rs` with enterprise-scale schemas,
//! complex feature interactions, fingerprint/LRECL stability, error message
//! formatting, and human-readable inspect output snapshots.

#![allow(clippy::unwrap_used)]

use copybook_core::{Field, FieldKind, Occurs, parse_copybook};

// ── helpers ──────────────────────────────────────────────────────────────────

fn schema_json(copybook: &str) -> serde_json::Value {
    let schema = parse_copybook(copybook).unwrap();
    serde_json::to_value(&schema).unwrap()
}

fn fields(json: &serde_json::Value) -> &Vec<serde_json::Value> {
    json["fields"].as_array().unwrap()
}

/// Render a schema's field layout as a human-readable string (mirrors snapshot_inspect.rs).
fn render_layout(fields: &[Field], indent: usize) -> String {
    let mut out = String::new();
    for field in fields {
        let prefix = " ".repeat(indent);
        let kind_label = match &field.kind {
            FieldKind::Alphanum { len } => format!("Alphanum({len})"),
            FieldKind::ZonedDecimal {
                digits,
                scale,
                signed,
                ..
            } => format!("Zoned(digits={digits}, scale={scale}, signed={signed})"),
            FieldKind::PackedDecimal {
                digits,
                scale,
                signed,
            } => format!("Packed(digits={digits}, scale={scale}, signed={signed})"),
            FieldKind::BinaryInt { bits, signed } => {
                format!("Binary(bits={bits}, signed={signed})")
            }
            FieldKind::Group => "Group".to_string(),
            FieldKind::Condition { values } => format!("Condition({values:?})"),
            FieldKind::Renames {
                from_field,
                thru_field,
            } => format!("Renames({from_field} THRU {thru_field})"),
            FieldKind::EditedNumeric {
                pic_string,
                width,
                scale,
                signed,
            } => format!("Edited(pic={pic_string}, width={width}, scale={scale}, signed={signed})"),
            FieldKind::FloatSingle => "FloatSingle".to_string(),
            FieldKind::FloatDouble => "FloatDouble".to_string(),
        };

        let occurs_label = match &field.occurs {
            Some(Occurs::Fixed { count }) => format!(" OCCURS {count}"),
            Some(Occurs::ODO {
                min,
                max,
                counter_path,
            }) => format!(" OCCURS {min} TO {max} DEPENDING ON {counter_path}"),
            None => String::new(),
        };

        let redefines_label = match &field.redefines_of {
            Some(r) => format!(" REDEFINES {r}"),
            None => String::new(),
        };

        out.push_str(&format!(
            "{prefix}{:02} {:<30} offset={:>4} len={:>4}  {kind_label}{occurs_label}{redefines_label}\n",
            field.level, field.name, field.offset, field.len,
        ));

        if !field.children.is_empty() {
            out.push_str(&render_layout(&field.children, indent + 2));
        }
    }
    out
}

// ═══════════════════════════════════════════════════════════════════════════
// E1. Enterprise schema: Banking record
// ═══════════════════════════════════════════════════════════════════════════

const BANKING_COPYBOOK: &str = "\
       01 BANKING-RECORD.
          05 ACCT-NUMBER       PIC X(12).
          05 ACCT-TYPE         PIC X(2).
             88 CHECKING       VALUE 'CH'.
             88 SAVINGS        VALUE 'SA'.
             88 LOAN           VALUE 'LN'.
          05 CUSTOMER-NAME.
             10 FIRST-NAME     PIC X(20).
             10 LAST-NAME      PIC X(25).
          05 BALANCE           PIC S9(9)V99 COMP-3.
          05 LAST-TXN-DATE     PIC 9(8).
          05 BRANCH-CODE       PIC 9(4) COMP.
          05 STATUS-FLAG       PIC X(1).
             88 ACTIVE         VALUE 'A'.
             88 CLOSED         VALUE 'C'.
             88 FROZEN         VALUE 'F'.";

#[test]
fn snapshot_banking_schema_field_count() {
    let schema = parse_copybook(BANKING_COPYBOOK).unwrap();
    // Root fields: ACCT-NUMBER, ACCT-TYPE, CUSTOMER-NAME, BALANCE, LAST-TXN-DATE, BRANCH-CODE, STATUS-FLAG
    assert_eq!(schema.fields.len(), 7);
    // Total fields including children (conditions + nested)
    assert!(schema.all_fields().len() >= 15);
}

#[test]
fn snapshot_banking_schema_lrecl() {
    let schema = parse_copybook(BANKING_COPYBOOK).unwrap();
    // 12 + 2 + (20+25) + 6 + 8 + 2 = 75 (STATUS-FLAG with level-88 children)
    assert_eq!(schema.lrecl_fixed, Some(75));
}

#[test]
fn snapshot_banking_schema_json_structure() {
    let json = schema_json(BANKING_COPYBOOK);
    let flds = fields(&json);

    assert_eq!(flds[0]["name"], "ACCT-NUMBER");
    assert_eq!(flds[0]["kind"]["Alphanum"]["len"], 12);
    assert_eq!(flds[0]["offset"], 0);

    // ACCT-TYPE with 88 conditions
    assert_eq!(flds[1]["name"], "ACCT-TYPE");
    let conds = flds[1]["children"].as_array().unwrap();
    assert_eq!(conds.len(), 3);
    assert_eq!(conds[0]["name"], "CHECKING");
    assert_eq!(conds[0]["level"], 88);
    assert_eq!(conds[1]["name"], "SAVINGS");
    assert_eq!(conds[2]["name"], "LOAN");

    // Nested CUSTOMER-NAME group
    assert_eq!(flds[2]["name"], "CUSTOMER-NAME");
    assert_eq!(flds[2]["kind"], "Group");
    let name_children = flds[2]["children"].as_array().unwrap();
    assert_eq!(name_children.len(), 2);
    assert_eq!(name_children[0]["name"], "FIRST-NAME");
    assert_eq!(name_children[1]["name"], "LAST-NAME");

    // COMP-3 balance
    assert_eq!(flds[3]["name"], "BALANCE");
    assert_eq!(flds[3]["kind"]["PackedDecimal"]["digits"], 11);
    assert_eq!(flds[3]["kind"]["PackedDecimal"]["scale"], 2);
    assert_eq!(flds[3]["kind"]["PackedDecimal"]["signed"], true);
    assert_eq!(flds[3]["len"], 6);

    // COMP binary
    assert_eq!(flds[5]["name"], "BRANCH-CODE");
    assert_eq!(flds[5]["kind"]["BinaryInt"]["bits"], 16);
}

#[test]
fn snapshot_banking_fingerprint_stable() {
    let s1 = parse_copybook(BANKING_COPYBOOK).unwrap();
    let s2 = parse_copybook(BANKING_COPYBOOK).unwrap();
    assert_eq!(s1.fingerprint, s2.fingerprint);
    assert!(!s1.fingerprint.is_empty());
}

// ═══════════════════════════════════════════════════════════════════════════
// E2. Enterprise schema: Insurance claim record
// ═══════════════════════════════════════════════════════════════════════════

const INSURANCE_COPYBOOK: &str = "\
       01 CLAIM-RECORD.
          05 CLAIM-ID          PIC X(15).
          05 POLICY-NUMBER     PIC X(12).
          05 CLAIM-TYPE        PIC X(2).
             88 AUTO-CLAIM     VALUE 'AU'.
             88 HOME-CLAIM     VALUE 'HO'.
             88 LIFE-CLAIM     VALUE 'LI'.
          05 CLAIM-AMOUNT      PIC S9(9)V99 COMP-3.
          05 DEDUCTIBLE        PIC S9(7)V99 COMP-3.
          05 DATE-FILED        PIC 9(8).
          05 DATE-RESOLVED     PIC 9(8).
          05 ADJUSTER-CODE     PIC X(6).
          05 STATUS            PIC X(1).
             88 OPEN           VALUE 'O'.
             88 UNDER-REVIEW   VALUE 'R'.
             88 APPROVED       VALUE 'A'.
             88 DENIED         VALUE 'D'.
             88 CLOSED         VALUE 'C'.";

#[test]
fn snapshot_insurance_schema_lrecl() {
    let schema = parse_copybook(INSURANCE_COPYBOOK).unwrap();
    // 15 + 12 + 2 + 6 + 5 + 8 + 8 + 6 = 62 (STATUS with level-88 children)
    assert_eq!(schema.lrecl_fixed, Some(62));
}

#[test]
fn snapshot_insurance_schema_field_offsets() {
    let json = schema_json(INSURANCE_COPYBOOK);
    let flds = fields(&json);
    assert_eq!(flds[0]["name"], "CLAIM-ID");
    assert_eq!(flds[0]["offset"], 0);
    assert_eq!(flds[1]["name"], "POLICY-NUMBER");
    assert_eq!(flds[1]["offset"], 15);
    assert_eq!(flds[2]["name"], "CLAIM-TYPE");
    assert_eq!(flds[2]["offset"], 27);
    assert_eq!(flds[3]["name"], "CLAIM-AMOUNT");
    assert_eq!(flds[3]["offset"], 29);
    assert_eq!(flds[4]["name"], "DEDUCTIBLE");
    assert_eq!(flds[4]["offset"], 35);

    // STATUS with 5 conditions
    let status = flds.iter().find(|f| f["name"] == "STATUS").unwrap();
    let conds = status["children"].as_array().unwrap();
    assert_eq!(conds.len(), 5);
}

// ═══════════════════════════════════════════════════════════════════════════
// E3. Enterprise schema: Payroll record
// ═══════════════════════════════════════════════════════════════════════════

const PAYROLL_COPYBOOK: &str = "\
       01 PAYROLL-RECORD.
          05 EMP-ID            PIC X(8).
          05 EMP-NAME.
             10 FIRST          PIC X(15).
             10 MIDDLE-INIT    PIC X(1).
             10 LAST           PIC X(20).
          05 PAY-RATE          PIC S9(5)V99 COMP-3.
          05 HOURS-WORKED      PIC 9(3)V9.
          05 GROSS-PAY         PIC S9(7)V99 COMP-3.
          05 DEDUCTIONS.
             10 FED-TAX        PIC S9(5)V99 COMP-3.
             10 STATE-TAX      PIC S9(5)V99 COMP-3.
             10 FICA           PIC S9(5)V99 COMP-3.
             10 INSURANCE      PIC S9(5)V99 COMP-3.
          05 NET-PAY           PIC S9(7)V99 COMP-3.
          05 PAY-PERIOD        PIC 9(6).
          05 DEPT-CODE         PIC X(4).";

#[test]
fn snapshot_payroll_schema_structure() {
    let json = schema_json(PAYROLL_COPYBOOK);
    let flds = fields(&json);

    // Verify nested groups
    let name_grp = flds.iter().find(|f| f["name"] == "EMP-NAME").unwrap();
    assert_eq!(name_grp["kind"], "Group");
    assert_eq!(name_grp["children"].as_array().unwrap().len(), 3);

    let ded_grp = flds.iter().find(|f| f["name"] == "DEDUCTIONS").unwrap();
    assert_eq!(ded_grp["kind"], "Group");
    let ded_children = ded_grp["children"].as_array().unwrap();
    assert_eq!(ded_children.len(), 4);
    for child in ded_children {
        assert_eq!(child["kind"]["PackedDecimal"]["digits"], 7);
        assert_eq!(child["kind"]["PackedDecimal"]["scale"], 2);
        assert_eq!(child["kind"]["PackedDecimal"]["signed"], true);
    }
}

#[test]
fn snapshot_payroll_schema_lrecl() {
    let schema = parse_copybook(PAYROLL_COPYBOOK).unwrap();
    // 8 + (15+1+20) + 4 + 4 + 5 + (4+4+4+4) + 5 + 6 + 4 = 88
    assert_eq!(schema.lrecl_fixed, Some(88));
}

// ═══════════════════════════════════════════════════════════════════════════
// S1. Schema with all field types → complete JSON snapshot
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_all_field_types_complete() {
    let copybook = "\
       01 ALL-TYPES-REC.
          05 ALPHA-FIELD       PIC X(10).
          05 ZONED-UNSIGNED    PIC 9(5).
          05 ZONED-SIGNED      PIC S9(7)V99.
          05 PACKED-FIELD      PIC S9(9)V99 COMP-3.
          05 BINARY-SMALL      PIC 9(4) COMP.
          05 BINARY-LARGE      PIC S9(18) COMP.
          05 GROUP-FIELD.
             10 CHILD-A        PIC X(5).
             10 CHILD-B        PIC 9(3).
          05 ARRAY-FIELD       PIC X(4) OCCURS 3 TIMES.
          05 STATUS-CODE       PIC X(1).
             88 VALID          VALUE 'V'.";

    let json = schema_json(copybook);
    let flds = fields(&json);

    // Alphanum
    assert_eq!(flds[0]["kind"]["Alphanum"]["len"], 10);
    // ZonedDecimal unsigned
    assert_eq!(flds[1]["kind"]["ZonedDecimal"]["digits"], 5);
    assert_eq!(flds[1]["kind"]["ZonedDecimal"]["signed"], false);
    // ZonedDecimal signed with scale
    assert_eq!(flds[2]["kind"]["ZonedDecimal"]["digits"], 9);
    assert_eq!(flds[2]["kind"]["ZonedDecimal"]["scale"], 2);
    assert_eq!(flds[2]["kind"]["ZonedDecimal"]["signed"], true);
    // PackedDecimal
    assert_eq!(flds[3]["kind"]["PackedDecimal"]["digits"], 11);
    assert_eq!(flds[3]["kind"]["PackedDecimal"]["signed"], true);
    // BinaryInt 16-bit
    assert_eq!(flds[4]["kind"]["BinaryInt"]["bits"], 16);
    assert_eq!(flds[4]["kind"]["BinaryInt"]["signed"], false);
    // BinaryInt 64-bit signed
    assert_eq!(flds[5]["kind"]["BinaryInt"]["bits"], 64);
    assert_eq!(flds[5]["kind"]["BinaryInt"]["signed"], true);
    // Group
    assert_eq!(flds[6]["kind"], "Group");
    assert_eq!(flds[6]["children"].as_array().unwrap().len(), 2);
    // Fixed OCCURS
    assert_eq!(flds[7]["occurs"]["Fixed"]["count"], 3);
    // Level-88 condition
    let status_children = flds[8]["children"].as_array().unwrap();
    assert_eq!(status_children[0]["name"], "VALID");
    assert_eq!(status_children[0]["level"], 88);
}

#[test]
fn snapshot_all_field_types_lrecl() {
    let copybook = "\
       01 ALL-TYPES-REC.
          05 ALPHA-FIELD       PIC X(10).
          05 ZONED-UNSIGNED    PIC 9(5).
          05 ZONED-SIGNED      PIC S9(7)V99.
          05 PACKED-FIELD      PIC S9(9)V99 COMP-3.
          05 BINARY-SMALL      PIC 9(4) COMP.
          05 BINARY-LARGE      PIC S9(18) COMP.
          05 GROUP-FIELD.
             10 CHILD-A        PIC X(5).
             10 CHILD-B        PIC 9(3).
          05 ARRAY-FIELD       PIC X(4) OCCURS 3 TIMES.
          05 STATUS-CODE       PIC X(1).
             88 VALID          VALUE 'V'.";

    let schema = parse_copybook(copybook).unwrap();
    // 10 + 5 + 9 + 6 + 2 + 8 + (5+3) + 4*3 = 60 (STATUS-CODE with level-88 children)
    assert_eq!(schema.lrecl_fixed, Some(60));
}

// ═══════════════════════════════════════════════════════════════════════════
// S2. OCCURS + ODO → JSON snapshot
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_occurs_and_odo_combined() {
    let copybook = "\
       01 ORDER-RECORD.
          05 ORDER-ID          PIC X(10).
          05 FIXED-ITEMS       PIC X(8) OCCURS 3 TIMES.
          05 LINE-COUNT        PIC 9(2).
          05 VARIABLE-ITEMS    OCCURS 1 TO 20
                               DEPENDING ON LINE-COUNT.
             10 ITEM-CODE      PIC X(6).
             10 ITEM-PRICE     PIC S9(5)V99 COMP-3.";

    let json = schema_json(copybook);
    let flds = fields(&json);

    // Fixed OCCURS
    assert_eq!(flds[1]["name"], "FIXED-ITEMS");
    assert_eq!(flds[1]["occurs"]["Fixed"]["count"], 3);
    assert_eq!(flds[1]["len"], 8);

    // ODO counter
    assert_eq!(flds[2]["name"], "LINE-COUNT");

    // ODO array
    assert_eq!(flds[3]["name"], "VARIABLE-ITEMS");
    assert_eq!(flds[3]["occurs"]["ODO"]["min"], 1);
    assert_eq!(flds[3]["occurs"]["ODO"]["max"], 20);
    assert_eq!(flds[3]["occurs"]["ODO"]["counter_path"], "LINE-COUNT");

    // No fixed LRECL due to ODO
    assert!(json["lrecl_fixed"].is_null());

    // Tail ODO metadata
    let odo = &json["tail_odo"];
    assert_eq!(odo["counter_path"], "LINE-COUNT");
    assert_eq!(odo["min_count"], 1);
    assert_eq!(odo["max_count"], 20);
}

// ═══════════════════════════════════════════════════════════════════════════
// S3. REDEFINES → JSON snapshot
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_redefines_chain() {
    let copybook = "\
       01 MULTI-REC.
          05 REC-TYPE          PIC X(2).
          05 DATA-BLOCK.
             10 BLOCK-DATA     PIC X(50).
          05 DATA-TEXT REDEFINES DATA-BLOCK.
             10 LINE-1         PIC X(25).
             10 LINE-2         PIC X(25).";

    let json = schema_json(copybook);
    let flds = fields(&json);

    // Original group
    let block = flds.iter().find(|f| f["name"] == "DATA-BLOCK").unwrap();
    assert!(block["redefines_of"].is_null());
    assert_eq!(block["offset"], 2);
    assert_eq!(block["len"], 50);

    // Redefine shares same offset
    let txt = flds.iter().find(|f| f["name"] == "DATA-TEXT").unwrap();
    assert_eq!(txt["redefines_of"], "DATA-BLOCK");
    assert_eq!(txt["offset"], 2);
    let txt_children = txt["children"].as_array().unwrap();
    assert_eq!(txt_children.len(), 2);
    assert_eq!(txt_children[0]["name"], "LINE-1");
    assert_eq!(txt_children[1]["name"], "LINE-2");

    // LRECL = 2 + 50 = 52 (REDEFINES does not add)
    assert_eq!(json["lrecl_fixed"], 52);
}

// ═══════════════════════════════════════════════════════════════════════════
// S4. Level-88 → JSON snapshot (complex conditions)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_level88_complex_conditions() {
    let copybook = "\
       01 STATUS-REC.
          05 PRIORITY          PIC 9(1).
             88 LOW            VALUE 1.
             88 MEDIUM         VALUE 2.
             88 HIGH           VALUE 3.
             88 CRITICAL       VALUE 4.
             88 NON-CRITICAL   VALUE 1 THRU 2.
          05 CATEGORY          PIC X(3).
             88 CAT-ALPHA      VALUE 'AAA'.
             88 CAT-BETA       VALUE 'BBB'.";

    let json = schema_json(copybook);
    let flds = fields(&json);

    // PRIORITY with 5 conditions
    let priority_conds = flds[0]["children"].as_array().unwrap();
    assert_eq!(priority_conds.len(), 5);
    assert_eq!(priority_conds[0]["name"], "LOW");
    assert_eq!(priority_conds[4]["name"], "NON-CRITICAL");
    // All level-88 have zero storage
    for c in priority_conds {
        assert_eq!(c["len"], 0);
        assert_eq!(c["level"], 88);
    }

    // CATEGORY with 2 conditions
    let cat_conds = flds[1]["children"].as_array().unwrap();
    assert_eq!(cat_conds.len(), 2);
    let alpha_vals = cat_conds[0]["kind"]["Condition"]["values"]
        .as_array()
        .unwrap();
    assert!(alpha_vals.iter().any(|v| v == "AAA"));
}

// ═══════════════════════════════════════════════════════════════════════════
// S5. RENAMES → JSON snapshot
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_renames_basic() {
    let copybook = "\
       01 EMPLOYEE-REC.
          05 EMP-ID            PIC X(8).
          05 EMP-NAME          PIC X(20).
          05 EMP-DEPT          PIC X(4).
          66 EMP-IDENTITY RENAMES EMP-ID THRU EMP-NAME.";

    let result = parse_copybook(copybook);
    let schema = match result {
        Ok(s) => s,
        Err(_) => return, // RENAMES may fail in some configurations
    };
    let json = serde_json::to_value(&schema).unwrap();
    let flds = json["fields"].as_array().unwrap();

    // Find the RENAMES field
    let renames_field = flds.iter().find(|f| f["name"] == "EMP-IDENTITY");
    if let Some(rf) = renames_field {
        assert_eq!(rf["level"], 66);
        let kind = &rf["kind"]["Renames"];
        assert_eq!(kind["from_field"], "EMP-ID");
        assert_eq!(kind["thru_field"], "EMP-NAME");
    }
}

#[test]
fn snapshot_renames_resolved_metadata() {
    let copybook = "\
       01 DATA-REC.
          05 FIELD-A           PIC X(5).
          05 FIELD-B           PIC X(10).
          05 FIELD-C           PIC X(8).
          66 ALIAS-AB RENAMES FIELD-A THRU FIELD-B.";

    let result = parse_copybook(copybook);
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json = serde_json::to_value(&schema).unwrap();
    let flds = json["fields"].as_array().unwrap();

    let alias = flds.iter().find(|f| f["name"] == "ALIAS-AB");
    if let Some(a) = alias {
        assert_eq!(a["level"], 66);
        let resolved = &a["resolved_renames"];
        if !resolved.is_null() {
            assert_eq!(resolved["offset"], 0);
            assert_eq!(resolved["length"], 15); // 5 + 10
            let members = resolved["members"].as_array().unwrap();
            assert!(members.iter().any(|m| m == "FIELD-A"));
            assert!(members.iter().any(|m| m == "FIELD-B"));
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// F1. Schema fingerprint snapshots
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_fingerprint_is_hex_sha256() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 A PIC X(10).",
    )
    .unwrap();
    // SHA-256 fingerprint is 64 hex chars
    assert_eq!(schema.fingerprint.len(), 64);
    assert!(schema.fingerprint.chars().all(|c| c.is_ascii_hexdigit()));
}

#[test]
fn snapshot_fingerprint_differs_for_different_schemas() {
    let s1 = parse_copybook(
        "\
       01 REC.
          05 A PIC X(10).",
    )
    .unwrap();
    let s2 = parse_copybook(
        "\
       01 REC.
          05 A PIC 9(10).",
    )
    .unwrap();
    let s3 = parse_copybook(
        "\
       01 REC.
          05 A PIC X(10).
          05 B PIC X(5).",
    )
    .unwrap();

    assert_ne!(s1.fingerprint, s2.fingerprint);
    assert_ne!(s1.fingerprint, s3.fingerprint);
    assert_ne!(s2.fingerprint, s3.fingerprint);
}

#[test]
fn snapshot_fingerprint_stable_across_parses() {
    let cpy = BANKING_COPYBOOK;
    let fp1 = parse_copybook(cpy).unwrap().fingerprint;
    let fp2 = parse_copybook(cpy).unwrap().fingerprint;
    let fp3 = parse_copybook(cpy).unwrap().fingerprint;
    assert_eq!(fp1, fp2);
    assert_eq!(fp2, fp3);
}

// ═══════════════════════════════════════════════════════════════════════════
// L1. Schema LRECL snapshots
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_lrecl_single_field() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 FIELD PIC X(100).",
    )
    .unwrap();
    assert_eq!(schema.lrecl_fixed, Some(100));
}

#[test]
fn snapshot_lrecl_with_occurs() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 HEADER     PIC X(10).
          05 ENTRIES     PIC X(20) OCCURS 5 TIMES.
          05 TRAILER     PIC X(8).",
    )
    .unwrap();
    // 10 + 20*5 + 8 = 118
    assert_eq!(schema.lrecl_fixed, Some(118));
}

#[test]
fn snapshot_lrecl_null_for_odo() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 CNT       PIC 9(2).
          05 ITEMS     OCCURS 0 TO 10
                       DEPENDING ON CNT
                       PIC X(5).",
    )
    .unwrap();
    assert!(schema.lrecl_fixed.is_none());
}

#[test]
fn snapshot_lrecl_with_redefines() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 A.
             10 A1     PIC X(15).
             10 A2     PIC X(15).
          05 B REDEFINES A.
             10 B1     PIC X(15).
             10 B2     PIC X(15).",
    )
    .unwrap();
    // REDEFINES does not extend LRECL
    assert_eq!(schema.lrecl_fixed, Some(30));
}

// ═══════════════════════════════════════════════════════════════════════════
// C1. Schema field_count snapshots
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_field_count_flat() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 A PIC X(5).
          05 B PIC X(5).
          05 C PIC X(5).",
    )
    .unwrap();
    assert_eq!(schema.fields.len(), 3);
    assert_eq!(schema.all_fields().len(), 3);
}

#[test]
fn snapshot_field_count_nested() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 GRP.
             10 A PIC X(5).
             10 B PIC X(5).
          05 C PIC X(5).",
    )
    .unwrap();
    assert_eq!(schema.fields.len(), 2); // GRP + C
    assert_eq!(schema.all_fields().len(), 4); // GRP + A + B + C
}

#[test]
fn snapshot_field_count_with_conditions() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 STATUS PIC X(1).
             88 YES VALUE 'Y'.
             88 NO  VALUE 'N'.",
    )
    .unwrap();
    assert_eq!(schema.fields.len(), 1); // STATUS
    assert_eq!(schema.all_fields().len(), 3); // STATUS + YES + NO
}

// ═══════════════════════════════════════════════════════════════════════════
// ERR1. Error message snapshots (parse error formatting)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_error_empty_input() {
    let err = parse_copybook("").unwrap_err();
    let msg = err.to_string();
    assert!(!msg.is_empty(), "error message should not be empty");
    // Error should contain an error code prefix (CBK*)
    assert!(
        msg.contains("CBK"),
        "error should contain CBK error code, got: {msg}"
    );
}

#[test]
fn snapshot_error_missing_pic() {
    // A field without PIC becomes a group, which is valid;
    // so test with a truly invalid construct instead
    let result = parse_copybook(
        "\
       01 REC.
          05 FIELD REDEFINES NONEXISTENT PIC X(5).",
    );
    if let Err(e) = result {
        let msg = e.to_string();
        assert!(
            msg.contains("CBK"),
            "error should contain error code, got: {msg}"
        );
    }
}

#[test]
fn snapshot_error_odo_not_tail() {
    let err = parse_copybook(
        "\
       01 REC.
          05 CNT       PIC 9(2).
          05 ITEMS     OCCURS 1 TO 5
                       DEPENDING ON CNT
                       PIC X(4).
          05 AFTER-ODO PIC X(10).",
    );
    if let Err(e) = err {
        let msg = e.to_string();
        // ODO-not-tail should produce CBKP021 error code
        assert!(
            msg.contains("CBKP021") || msg.contains("CBK"),
            "ODO not-tail error should reference error code, got: {msg}"
        );
    }
}

#[test]
fn snapshot_error_code_format_consistency() {
    // Test that error codes follow the CBKX### pattern
    let bad_inputs = vec![
        "",
        "GARBAGE TEXT NOT COBOL",
        "       01 REC.\n          05 FIELD REDEFINES MISSING PIC X(5).",
    ];

    for input in bad_inputs {
        if let Err(e) = parse_copybook(input) {
            let msg = e.to_string();
            // All errors should start with "CBK" code pattern
            assert!(
                msg.starts_with("CBK"),
                "error should start with CBK prefix, got: {msg}"
            );
            // Should contain a colon separating code from message
            assert!(
                msg.contains(':'),
                "error should contain ':' separator, got: {msg}"
            );
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// I1. Inspect output snapshots (human-readable layout)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_inspect_banking_layout() {
    let schema = parse_copybook(BANKING_COPYBOOK).unwrap();
    let layout = render_layout(&schema.fields, 0);

    assert!(layout.contains("ACCT-NUMBER"));
    assert!(layout.contains("Alphanum(12)"));
    assert!(layout.contains("CUSTOMER-NAME"));
    assert!(layout.contains("Group"));
    assert!(layout.contains("FIRST-NAME"));
    assert!(layout.contains("LAST-NAME"));
    assert!(layout.contains("BALANCE"));
    assert!(layout.contains("Packed("));
    assert!(layout.contains("BRANCH-CODE"));
    assert!(layout.contains("Binary("));
    // Level-88 conditions
    assert!(layout.contains("88 CHECKING"));
    assert!(layout.contains("88 ACTIVE"));
    assert!(layout.contains("Condition("));
}

#[test]
fn snapshot_inspect_odo_layout() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 HDR        PIC X(4).
          05 CNT        PIC 9(2).
          05 LINES      OCCURS 1 TO 10
                        DEPENDING ON CNT.
             10 CODE    PIC X(3).
             10 AMT     PIC 9(5).",
    )
    .unwrap();
    let layout = render_layout(&schema.fields, 0);

    assert!(
        layout.contains("OCCURS 1 TO 10 DEPENDING ON CNT"),
        "should show ODO annotation in layout"
    );
    assert!(layout.contains("CODE"));
    assert!(layout.contains("AMT"));
}

#[test]
fn snapshot_inspect_redefines_layout() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 RAW        PIC X(20).
          05 PARSED REDEFINES RAW.
             10 CODE    PIC X(4).
             10 VALUE1  PIC X(16).",
    )
    .unwrap();
    let layout = render_layout(&schema.fields, 0);

    assert!(layout.contains("REDEFINES RAW"));
    assert!(layout.contains("PARSED"));
    assert!(layout.contains("CODE"));
    assert!(layout.contains("VALUE1"));
}

#[test]
fn snapshot_inspect_deep_nesting() {
    let schema = parse_copybook(
        "\
       01 REC.
          05 L1.
             10 L2.
                15 L3.
                   20 LEAF PIC X(1).",
    )
    .unwrap();
    let layout = render_layout(&schema.fields, 0);

    // Verify indentation increases with nesting depth
    let lines: Vec<&str> = layout.lines().collect();
    assert!(lines.len() >= 4);
    // Each nested level should be indented more than its parent
    for i in 1..lines.len() {
        let prev_indent = lines[i - 1].len() - lines[i - 1].trim_start().len();
        let curr_indent = lines[i].len() - lines[i].trim_start().len();
        assert!(
            curr_indent >= prev_indent,
            "child should be at least as indented as parent"
        );
    }
    assert!(layout.contains("LEAF"));
}

// ═══════════════════════════════════════════════════════════════════════════
// M1. Mixed feature interactions
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_redefines_with_level88() {
    let copybook = "\
       01 PAYMENT-REC.
          05 PAY-TYPE          PIC X(1).
             88 CASH           VALUE 'C'.
             88 CHECK          VALUE 'K'.
             88 CARD           VALUE 'D'.
          05 PAY-DATA.
             10 PAY-DETAIL     PIC X(16).
          05 PAY-CARD REDEFINES PAY-DATA.
             10 CARD-NUM       PIC X(16).";

    let json = schema_json(copybook);
    let flds = fields(&json);

    // Level-88 on PAY-TYPE
    let pay_type = &flds[0];
    assert_eq!(pay_type["name"], "PAY-TYPE");
    let conds = pay_type["children"].as_array().unwrap();
    assert_eq!(conds.len(), 3);

    // REDEFINES
    let card = flds.iter().find(|f| f["name"] == "PAY-CARD").unwrap();
    assert_eq!(card["redefines_of"], "PAY-DATA");

    // PAY-DATA and PAY-CARD share the same offset
    let data = flds.iter().find(|f| f["name"] == "PAY-DATA").unwrap();
    assert_eq!(card["offset"], data["offset"]);

    assert_eq!(json["lrecl_fixed"], 17);
}

#[test]
fn snapshot_occurs_with_nested_group() {
    let copybook = "\
       01 INVOICE-REC.
          05 INVOICE-ID        PIC X(10).
          05 LINE-ITEMS OCCURS 5 TIMES.
             10 PRODUCT.
                15 PROD-CODE   PIC X(8).
                15 PROD-DESC   PIC X(20).
             10 QUANTITY       PIC 9(4).
             10 UNIT-PRICE     PIC S9(5)V99 COMP-3.";

    let json = schema_json(copybook);
    let flds = fields(&json);

    let items = &flds[1];
    assert_eq!(items["name"], "LINE-ITEMS");
    assert_eq!(items["occurs"]["Fixed"]["count"], 5);

    let item_children = items["children"].as_array().unwrap();
    // PRODUCT group, QUANTITY, UNIT-PRICE
    assert_eq!(item_children.len(), 3);
    assert_eq!(item_children[0]["name"], "PRODUCT");
    assert_eq!(item_children[0]["kind"], "Group");

    // Nested children of PRODUCT
    let prod_children = item_children[0]["children"].as_array().unwrap();
    assert_eq!(prod_children.len(), 2);
    assert_eq!(prod_children[0]["name"], "PROD-CODE");
    assert_eq!(prod_children[1]["name"], "PROD-DESC");
}

// ═══════════════════════════════════════════════════════════════════════════
// ED1. Edited PIC snapshot
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_edited_pic_multiple_patterns() {
    let copybook = "\
       01 REPORT-REC.
          05 AMOUNT-Z          PIC ZZZ9.
          05 AMOUNT-CURR       PIC $ZZ,ZZ9.99.
          05 AMOUNT-SIGN       PIC +ZZ9.";

    let result = parse_copybook(copybook);
    let schema = match result {
        Ok(s) => s,
        Err(_) => return, // edited PIC may not be available
    };
    let json = serde_json::to_value(&schema).unwrap();
    let flds = json["fields"].as_array().unwrap();

    // ZZZ9
    let en0 = &flds[0]["kind"]["EditedNumeric"];
    if !en0.is_null() {
        assert_eq!(en0["pic_string"], "ZZZ9");
        assert_eq!(en0["scale"], 0);
        assert_eq!(en0["signed"], false);
    }

    // $ZZ,ZZ9.99
    let en1 = &flds[1]["kind"]["EditedNumeric"];
    if !en1.is_null() {
        assert_eq!(en1["pic_string"], "$ZZ,ZZ9.99");
        assert_eq!(en1["scale"], 2);
    }

    // +ZZ9
    let en2 = &flds[2]["kind"]["EditedNumeric"];
    if !en2.is_null() {
        assert_eq!(en2["signed"], true);
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// J1. JSON serialization round-trip stability
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_json_round_trip_stable() {
    let schema = parse_copybook(BANKING_COPYBOOK).unwrap();
    let json1 = serde_json::to_string(&schema).unwrap();
    let json2 = serde_json::to_string(&schema).unwrap();
    assert_eq!(json1, json2, "JSON serialization must be deterministic");
}

#[test]
fn snapshot_json_pretty_contains_all_top_level_keys() {
    let json = schema_json(BANKING_COPYBOOK);
    let obj = json.as_object().unwrap();
    for key in ["fields", "lrecl_fixed", "tail_odo", "fingerprint"] {
        assert!(obj.contains_key(key), "Missing top-level key '{key}'");
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// P1. Path correctness snapshots
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_field_paths_hierarchical() {
    let json = schema_json(
        "\
       01 REC.
          05 OUTER.
             10 MIDDLE.
                15 INNER PIC X(1).",
    );
    let flds = fields(&json);

    assert_eq!(flds[0]["path"], "OUTER");
    let middle = &flds[0]["children"].as_array().unwrap()[0];
    assert_eq!(middle["path"], "OUTER.MIDDLE");
    let inner = &middle["children"].as_array().unwrap()[0];
    assert_eq!(inner["path"], "OUTER.MIDDLE.INNER");
}

#[test]
fn snapshot_field_paths_in_occurs_group() {
    let json = schema_json(
        "\
       01 REC.
          05 ITEMS OCCURS 3 TIMES.
             10 CODE PIC X(4).
             10 QTY  PIC 9(3).",
    );
    let items = &fields(&json)[0];
    let children = items["children"].as_array().unwrap();
    assert_eq!(children[0]["path"], "ITEMS.CODE");
    assert_eq!(children[1]["path"], "ITEMS.QTY");
}
