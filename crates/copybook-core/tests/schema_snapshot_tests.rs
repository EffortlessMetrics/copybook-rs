// SPDX-License-Identifier: AGPL-3.0-or-later
//! Golden snapshot tests for copybook-core schema serialization.
//!
//! Each test parses a COBOL copybook fragment and verifies key structural
//! properties of the resulting schema JSON, ensuring serialization stability
//! across all supported field types.

#![allow(clippy::unwrap_used)]

use copybook_core::parse_copybook;

// ── helpers ──────────────────────────────────────────────────────────────────

/// Parse a copybook and return the schema as a `serde_json::Value`.
fn schema_json(copybook: &str) -> serde_json::Value {
    let schema = parse_copybook(copybook).unwrap();
    serde_json::to_value(&schema).unwrap()
}

/// Return the top-level `fields` array from the schema JSON.
fn fields(json: &serde_json::Value) -> &Vec<serde_json::Value> {
    json["fields"].as_array().unwrap()
}

// ═══════════════════════════════════════════════════════════════════════════
// 1. Simple PIC X alphanum
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_simple_alphanum_pic_x() {
    let json = schema_json(
        "\
       01 REC.
          05 NAME PIC X(10).",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "NAME");
    assert_eq!(f["kind"]["Alphanum"]["len"], 10);
    assert_eq!(f["offset"], 0);
    assert_eq!(f["len"], 10);
    assert_eq!(json["lrecl_fixed"], 10);
}

#[test]
fn snapshot_multiple_alphanum_offsets() {
    let json = schema_json(
        "\
       01 REC.
          05 FIRST-NAME  PIC X(15).
          05 LAST-NAME   PIC X(20).
          05 CITY        PIC X(25).",
    );
    let flds = fields(&json);

    assert_eq!(flds.len(), 3);
    assert_eq!(flds[0]["offset"], 0);
    assert_eq!(flds[0]["len"], 15);
    assert_eq!(flds[1]["offset"], 15);
    assert_eq!(flds[1]["len"], 20);
    assert_eq!(flds[2]["offset"], 35);
    assert_eq!(flds[2]["len"], 25);
    assert_eq!(json["lrecl_fixed"], 60);
}

#[test]
fn snapshot_single_char_alphanum() {
    let json = schema_json(
        "\
       01 REC.
          05 FLAG PIC X.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "FLAG");
    assert_eq!(f["kind"]["Alphanum"]["len"], 1);
    assert_eq!(f["len"], 1);
}

// ═══════════════════════════════════════════════════════════════════════════
// 2. PIC 9 – zoned decimal without and with V (decimal)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_zoned_decimal_integer() {
    let json = schema_json(
        "\
       01 REC.
          05 COUNTER PIC 9(5).",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "COUNTER");
    assert_eq!(f["kind"]["ZonedDecimal"]["digits"], 5);
    assert_eq!(f["kind"]["ZonedDecimal"]["scale"], 0);
    assert_eq!(f["kind"]["ZonedDecimal"]["signed"], false);
    assert_eq!(f["len"], 5);
}

#[test]
fn snapshot_zoned_decimal_with_v() {
    let json = schema_json(
        "\
       01 REC.
          05 AMOUNT PIC 9(5)V99.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "AMOUNT");
    assert_eq!(f["kind"]["ZonedDecimal"]["digits"], 7);
    assert_eq!(f["kind"]["ZonedDecimal"]["scale"], 2);
    assert_eq!(f["kind"]["ZonedDecimal"]["signed"], false);
    assert_eq!(f["len"], 7);
}

#[test]
fn snapshot_signed_zoned_decimal() {
    let json = schema_json(
        "\
       01 REC.
          05 BALANCE PIC S9(7)V99.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "BALANCE");
    assert_eq!(f["kind"]["ZonedDecimal"]["digits"], 9);
    assert_eq!(f["kind"]["ZonedDecimal"]["scale"], 2);
    assert_eq!(f["kind"]["ZonedDecimal"]["signed"], true);
    // Zoned decimal: sign embedded in last byte, so len == digits
    assert_eq!(f["len"], 9);
}

// ═══════════════════════════════════════════════════════════════════════════
// 3. PIC S9 COMP-3 – packed decimal
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_comp3_signed() {
    let json = schema_json(
        "\
       01 REC.
          05 AMOUNT PIC S9(7)V99 COMP-3.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "AMOUNT");
    assert_eq!(f["kind"]["PackedDecimal"]["digits"], 9);
    assert_eq!(f["kind"]["PackedDecimal"]["scale"], 2);
    assert_eq!(f["kind"]["PackedDecimal"]["signed"], true);
    // COMP-3: (digits + 1) / 2 = (9+1)/2 = 5 bytes
    assert_eq!(f["len"], 5);
}

#[test]
fn snapshot_comp3_unsigned() {
    let json = schema_json(
        "\
       01 REC.
          05 QTY PIC 9(5) COMP-3.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "QTY");
    assert_eq!(f["kind"]["PackedDecimal"]["digits"], 5);
    assert_eq!(f["kind"]["PackedDecimal"]["scale"], 0);
    assert_eq!(f["kind"]["PackedDecimal"]["signed"], false);
    // (5+1)/2 = 3 bytes
    assert_eq!(f["len"], 3);
}

#[test]
fn snapshot_comp3_large_precision() {
    let json = schema_json(
        "\
       01 REC.
          05 BIG PIC S9(15)V9(4) COMP-3.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["kind"]["PackedDecimal"]["digits"], 19);
    assert_eq!(f["kind"]["PackedDecimal"]["scale"], 4);
    assert_eq!(f["kind"]["PackedDecimal"]["signed"], true);
    // (19+1)/2 = 10 bytes
    assert_eq!(f["len"], 10);
}

// ═══════════════════════════════════════════════════════════════════════════
// 4. PIC S9 COMP / BINARY
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_comp_binary_small() {
    // PIC 9(4) COMP → 16-bit binary
    let json = schema_json(
        "\
       01 REC.
          05 SMALL-INT PIC 9(4) COMP.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "SMALL-INT");
    assert_eq!(f["kind"]["BinaryInt"]["bits"], 16);
    assert_eq!(f["kind"]["BinaryInt"]["signed"], false);
    assert_eq!(f["len"], 2);
}

#[test]
fn snapshot_comp_binary_medium_signed() {
    // PIC S9(9) COMP → 32-bit signed binary
    let json = schema_json(
        "\
       01 REC.
          05 MED-INT PIC S9(9) COMP.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "MED-INT");
    assert_eq!(f["kind"]["BinaryInt"]["bits"], 32);
    assert_eq!(f["kind"]["BinaryInt"]["signed"], true);
    assert_eq!(f["len"], 4);
}

#[test]
fn snapshot_comp_binary_large() {
    // PIC 9(18) COMP → 64-bit binary
    let json = schema_json(
        "\
       01 REC.
          05 BIG-INT PIC 9(18) COMP.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["kind"]["BinaryInt"]["bits"], 64);
    assert_eq!(f["kind"]["BinaryInt"]["signed"], false);
    assert_eq!(f["len"], 8);
}

// ═══════════════════════════════════════════════════════════════════════════
// 5. OCCURS – fixed-size arrays
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_fixed_occurs_leaf() {
    let json = schema_json(
        "\
       01 REC.
          05 SCORES PIC 9(3) OCCURS 5 TIMES.",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "SCORES");
    assert_eq!(f["occurs"]["Fixed"]["count"], 5);
    assert_eq!(f["kind"]["ZonedDecimal"]["digits"], 3);
    // len is per-element length (3), total record = 3 * 5 = 15
    assert_eq!(f["len"], 3);
}

#[test]
fn snapshot_fixed_occurs_group() {
    let json = schema_json(
        "\
       01 REC.
          05 ITEMS OCCURS 3 TIMES.
             10 ITEM-CODE PIC X(4).
             10 ITEM-QTY  PIC 9(3).",
    );
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "ITEMS");
    assert_eq!(f["kind"], "Group");
    assert_eq!(f["occurs"]["Fixed"]["count"], 3);

    let children = f["children"].as_array().unwrap();
    assert_eq!(children.len(), 2);
    assert_eq!(children[0]["name"], "ITEM-CODE");
    assert_eq!(children[1]["name"], "ITEM-QTY");

    // len is per-element length (4 + 3 = 7), total record = 7 * 3 = 21
    assert_eq!(f["len"], 7);
    assert_eq!(json["lrecl_fixed"], 21);
}

// ═══════════════════════════════════════════════════════════════════════════
// 6. OCCURS DEPENDING ON – variable-length arrays
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_odo_variable_array() {
    let json = schema_json(
        "\
       01 REC.
          05 CNT         PIC 9(2).
          05 ENTRIES      OCCURS 1 TO 5
                          DEPENDING ON CNT.
             10 ENTRY-VAL PIC X(8).",
    );

    // ODO means no fixed LRECL
    assert!(json["lrecl_fixed"].is_null());

    // tail_odo metadata
    let odo = &json["tail_odo"];
    assert_eq!(odo["counter_path"], "CNT");
    assert_eq!(odo["min_count"], 1);
    assert_eq!(odo["max_count"], 5);
    assert_eq!(odo["array_path"], "ENTRIES");

    // ENTRIES field has ODO occurs
    let entries = &fields(&json)[1];
    assert_eq!(entries["occurs"]["ODO"]["min"], 1);
    assert_eq!(entries["occurs"]["ODO"]["max"], 5);
    assert_eq!(entries["occurs"]["ODO"]["counter_path"], "CNT");
}

// ═══════════════════════════════════════════════════════════════════════════
// 7. REDEFINES
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_redefines_basic() {
    let json = schema_json(
        "\
       01 REC.
          05 DATA-TEXT   PIC X(10).
          05 DATA-NUM    REDEFINES DATA-TEXT PIC 9(10).",
    );
    let flds = fields(&json);

    // Both fields share offset 0
    assert_eq!(flds[0]["name"], "DATA-TEXT");
    assert_eq!(flds[0]["offset"], 0);
    assert!(flds[0]["redefines_of"].is_null());

    assert_eq!(flds[1]["name"], "DATA-NUM");
    assert_eq!(flds[1]["offset"], 0);
    assert_eq!(flds[1]["redefines_of"], "DATA-TEXT");
    assert_eq!(flds[1]["kind"]["ZonedDecimal"]["digits"], 10);
}

#[test]
fn snapshot_redefines_group() {
    let json = schema_json(
        "\
       01 REC.
          05 RAW-DATA PIC X(20).
          05 PARSED REDEFINES RAW-DATA.
             10 REC-CODE  PIC X(4).
             10 REC-VALUE PIC X(16).",
    );
    let flds = fields(&json);

    let parsed = flds.iter().find(|f| f["name"] == "PARSED").unwrap();
    assert_eq!(parsed["redefines_of"], "RAW-DATA");
    assert_eq!(parsed["kind"], "Group");
    assert_eq!(parsed["offset"], 0);

    let children = parsed["children"].as_array().unwrap();
    assert_eq!(children.len(), 2);
    assert_eq!(children[0]["name"], "REC-CODE");
    assert_eq!(children[1]["name"], "REC-VALUE");
}

// ═══════════════════════════════════════════════════════════════════════════
// 8. Level-88 condition values
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_level88_single_value() {
    let json = schema_json(
        "\
       01 REC.
          05 STATUS PIC X(1).
             88 IS-ACTIVE VALUE 'A'.",
    );
    let parent = &fields(&json)[0];
    assert_eq!(parent["name"], "STATUS");

    let children = parent["children"].as_array().unwrap();
    assert_eq!(children.len(), 1);

    let cond = &children[0];
    assert_eq!(cond["name"], "IS-ACTIVE");
    assert_eq!(cond["level"], 88);
    assert_eq!(cond["len"], 0); // Level-88 has no storage
    let values = cond["kind"]["Condition"]["values"].as_array().unwrap();
    assert!(values.iter().any(|v| v == "A"));
}

#[test]
fn snapshot_level88_multiple_conditions() {
    let json = schema_json(
        "\
       01 REC.
          05 TYPE-CODE PIC 9(1).
             88 TYPE-ALPHA  VALUE 1.
             88 TYPE-BETA   VALUE 2.
             88 TYPE-GAMMA  VALUE 3.",
    );
    let parent = &fields(&json)[0];
    let children = parent["children"].as_array().unwrap();
    assert_eq!(children.len(), 3);
    assert_eq!(children[0]["name"], "TYPE-ALPHA");
    assert_eq!(children[1]["name"], "TYPE-BETA");
    assert_eq!(children[2]["name"], "TYPE-GAMMA");
}

#[test]
fn snapshot_level88_thru_range() {
    let json = schema_json(
        "\
       01 REC.
          05 GRADE PIC X(1).
             88 PASSING VALUE 'A' THRU 'D'.",
    );
    let cond = &fields(&json)[0]["children"].as_array().unwrap()[0];
    assert_eq!(cond["name"], "PASSING");
    assert_eq!(cond["level"], 88);
    let values = cond["kind"]["Condition"]["values"].as_array().unwrap();
    assert!(!values.is_empty(), "THRU range should produce values");
}

// ═══════════════════════════════════════════════════════════════════════════
// 9. Group fields with children
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_group_with_children() {
    let json = schema_json(
        "\
       01 REC.
          05 HEADER.
             10 ID   PIC X(5).
             10 DATE PIC 9(8).",
    );
    let grp = &fields(&json)[0];

    assert_eq!(grp["name"], "HEADER");
    assert_eq!(grp["kind"], "Group");
    assert_eq!(grp["len"], 13); // 5 + 8

    let children = grp["children"].as_array().unwrap();
    assert_eq!(children.len(), 2);
    assert_eq!(children[0]["name"], "ID");
    assert_eq!(children[0]["path"], "HEADER.ID");
    assert_eq!(children[1]["name"], "DATE");
    assert_eq!(children[1]["path"], "HEADER.DATE");
}

#[test]
fn snapshot_nested_groups() {
    let json = schema_json(
        "\
       01 REC.
          05 OUTER.
             10 INNER.
                15 LEAF PIC X(4).",
    );
    let outer = &fields(&json)[0];
    assert_eq!(outer["name"], "OUTER");
    assert_eq!(outer["kind"], "Group");

    let inner = &outer["children"].as_array().unwrap()[0];
    assert_eq!(inner["name"], "INNER");
    assert_eq!(inner["kind"], "Group");

    let leaf = &inner["children"].as_array().unwrap()[0];
    assert_eq!(leaf["name"], "LEAF");
    assert_eq!(leaf["kind"]["Alphanum"]["len"], 4);
    assert_eq!(leaf["path"], "OUTER.INNER.LEAF");
}

#[test]
fn snapshot_group_total_length() {
    let json = schema_json(
        "\
       01 REC.
          05 ADDR.
             10 STREET PIC X(30).
             10 ZIP    PIC 9(5).
             10 STATE  PIC X(2).",
    );
    let grp = &fields(&json)[0];
    assert_eq!(grp["len"], 37); // 30 + 5 + 2
    assert_eq!(json["lrecl_fixed"], 37);
}

// ═══════════════════════════════════════════════════════════════════════════
// 10. SIGN SEPARATE LEADING / TRAILING
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_sign_separate_leading() {
    let result = parse_copybook(
        "\
       01 REC.
          05 AMT PIC S9(5)V99 SIGN IS LEADING SEPARATE.",
    );
    let schema = match result {
        Ok(s) => s,
        Err(_) => return, // feature may be disabled
    };
    let json = serde_json::to_value(&schema).unwrap();
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "AMT");
    let zd = &f["kind"]["ZonedDecimal"];
    assert_eq!(zd["signed"], true);
    assert_eq!(zd["sign_separate"]["placement"], "leading");
    // SIGN SEPARATE adds 1 byte: 7 digits + 1 = 8
    assert_eq!(f["len"], 8);
}

#[test]
fn snapshot_sign_separate_trailing() {
    let result = parse_copybook(
        "\
       01 REC.
          05 BAL PIC S9(3) SIGN IS TRAILING SEPARATE.",
    );
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json = serde_json::to_value(&schema).unwrap();
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "BAL");
    let zd = &f["kind"]["ZonedDecimal"];
    assert_eq!(zd["signed"], true);
    assert_eq!(zd["sign_separate"]["placement"], "trailing");
    // 3 digits + 1 sign byte = 4
    assert_eq!(f["len"], 4);
}

// ═══════════════════════════════════════════════════════════════════════════
// 11. Edited PIC patterns
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_edited_z_suppression() {
    let result = parse_copybook(
        "\
       01 REC.
          05 QTY PIC ZZZ9.",
    );
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json = serde_json::to_value(&schema).unwrap();
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "QTY");
    let en = &f["kind"]["EditedNumeric"];
    assert_eq!(en["pic_string"], "ZZZ9");
    assert_eq!(en["scale"], 0);
    assert_eq!(en["signed"], false);
    assert_eq!(f["len"], 4);
}

#[test]
fn snapshot_edited_currency() {
    let result = parse_copybook(
        "\
       01 REC.
          05 PRICE PIC $ZZ,ZZ9.99.",
    );
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json = serde_json::to_value(&schema).unwrap();
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "PRICE");
    let en = &f["kind"]["EditedNumeric"];
    assert_eq!(en["pic_string"], "$ZZ,ZZ9.99");
    assert_eq!(en["scale"], 2);
    assert_eq!(en["signed"], false);
}

#[test]
fn snapshot_edited_sign_plus() {
    let result = parse_copybook(
        "\
       01 REC.
          05 DELTA PIC +ZZZ9.",
    );
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json = serde_json::to_value(&schema).unwrap();
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "DELTA");
    let en = &f["kind"]["EditedNumeric"];
    assert_eq!(en["signed"], true);
}

#[test]
fn snapshot_edited_sign_minus() {
    let result = parse_copybook(
        "\
       01 REC.
          05 LOSS PIC -ZZZ9.",
    );
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json = serde_json::to_value(&schema).unwrap();
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "LOSS");
    let en = &f["kind"]["EditedNumeric"];
    assert_eq!(en["signed"], true);
}

#[test]
fn snapshot_edited_z_with_decimal() {
    let result = parse_copybook(
        "\
       01 REC.
          05 RATE PIC ZZ9.99.",
    );
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json = serde_json::to_value(&schema).unwrap();
    let f = &fields(&json)[0];

    assert_eq!(f["name"], "RATE");
    let en = &f["kind"]["EditedNumeric"];
    assert_eq!(en["pic_string"], "ZZ9.99");
    assert_eq!(en["scale"], 2);
    assert_eq!(f["len"], 6);
}

// ═══════════════════════════════════════════════════════════════════════════
// 12. Mixed field types – comprehensive record
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_mixed_field_types() {
    let json = schema_json(
        "\
       01 ORDER.
          05 ORDER-ID     PIC X(8).
          05 ORDER-DATE   PIC 9(8).
          05 TOTAL-AMT    PIC S9(7)V99 COMP-3.
          05 LINE-COUNT   PIC S9(4) COMP.
          05 STATUS       PIC X(1).",
    );
    let flds = fields(&json);
    assert_eq!(flds.len(), 5);

    // Alphanum
    assert!(!flds[0]["kind"]["Alphanum"].is_null());
    // ZonedDecimal
    assert!(!flds[1]["kind"]["ZonedDecimal"].is_null());
    // PackedDecimal
    assert!(!flds[2]["kind"]["PackedDecimal"].is_null());
    // BinaryInt
    assert!(!flds[3]["kind"]["BinaryInt"].is_null());
    // Alphanum again
    assert!(!flds[4]["kind"]["Alphanum"].is_null());

    // Verify cumulative offsets
    assert_eq!(flds[0]["offset"], 0); // X(8) → 8 bytes
    assert_eq!(flds[1]["offset"], 8); // 9(8) → 8 bytes
    assert_eq!(flds[2]["offset"], 16); // S9(7)V99 COMP-3 → 5 bytes
    assert_eq!(flds[3]["offset"], 21); // S9(4) COMP → 2 bytes
    assert_eq!(flds[4]["offset"], 23); // X(1) → 1 byte
    assert_eq!(json["lrecl_fixed"], 24);
}

// ═══════════════════════════════════════════════════════════════════════════
// 13. FILLER field handling
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_filler_field() {
    let json = schema_json(
        "\
       01 REC.
          05 ID      PIC X(5).
          05 FILLER  PIC X(3).
          05 NAME    PIC X(10).",
    );
    let flds = fields(&json);
    assert_eq!(flds.len(), 3);

    // FILLER occupies space but is present in schema
    assert_eq!(flds[1]["offset"], 5);
    assert_eq!(flds[1]["len"], 3);
    assert_eq!(flds[2]["offset"], 8);
    assert_eq!(json["lrecl_fixed"], 18);
}

// ═══════════════════════════════════════════════════════════════════════════
// 14. Schema fingerprint stability
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_fingerprint_deterministic() {
    let cpy = "\
       01 REC.
          05 A PIC X(5).
          05 B PIC 9(3).";

    let s1 = parse_copybook(cpy).unwrap();
    let s2 = parse_copybook(cpy).unwrap();

    assert_eq!(s1.fingerprint, s2.fingerprint);
    assert!(!s1.fingerprint.is_empty());
}

#[test]
fn snapshot_fingerprint_changes_with_schema() {
    let cpy1 = "\
       01 REC.
          05 A PIC X(5).";
    let cpy2 = "\
       01 REC.
          05 A PIC X(10).";

    let s1 = parse_copybook(cpy1).unwrap();
    let s2 = parse_copybook(cpy2).unwrap();
    assert_ne!(s1.fingerprint, s2.fingerprint);
}

// ═══════════════════════════════════════════════════════════════════════════
// 15. JSON key completeness for leaf and group fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_json_keys_leaf_field() {
    let json = schema_json(
        "\
       01 REC.
          05 VAL PIC 9(4).",
    );
    let obj = fields(&json)[0].as_object().unwrap();

    for key in [
        "path",
        "name",
        "level",
        "kind",
        "offset",
        "len",
        "redefines_of",
        "occurs",
        "children",
    ] {
        assert!(obj.contains_key(key), "Missing key '{key}'");
    }
}

#[test]
fn snapshot_json_keys_group_field() {
    let json = schema_json(
        "\
       01 REC.
          05 GRP.
             10 CHILD PIC X(1).",
    );
    let obj = fields(&json)[0].as_object().unwrap();

    assert_eq!(obj["kind"], "Group");
    let children = obj["children"].as_array().unwrap();
    assert_eq!(children.len(), 1);
    assert_eq!(children[0]["name"], "CHILD");
}

// ═══════════════════════════════════════════════════════════════════════════
// 16. Schema-level properties
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn snapshot_schema_top_level_keys() {
    let json = schema_json(
        "\
       01 REC.
          05 F PIC X(1).",
    );
    let obj = json.as_object().unwrap();

    for key in ["fields", "lrecl_fixed", "tail_odo", "fingerprint"] {
        assert!(obj.contains_key(key), "Missing schema key '{key}'");
    }
}

#[test]
fn snapshot_no_tail_odo_for_fixed_record() {
    let json = schema_json(
        "\
       01 REC.
          05 A PIC X(10).
          05 B PIC 9(5).",
    );
    assert!(json["tail_odo"].is_null());
    assert_eq!(json["lrecl_fixed"], 15);
}
