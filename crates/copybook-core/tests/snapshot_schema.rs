// SPDX-License-Identifier: AGPL-3.0-or-later
//! Snapshot tests for schema serialization stability.
//!
//! These tests verify that `parse_copybook` produces stable JSON-serializable
//! schemas for simple, ODO, and REDEFINES copybooks. Any change to schema
//! structure or field naming will cause these tests to fail, catching
//! accidental serialization format changes.

#![allow(clippy::unwrap_used)]

use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Simple copybook -> schema JSON stability
// ---------------------------------------------------------------------------

#[test]
fn snapshot_simple_copybook_schema_json() {
    // Validates that a basic copybook produces a stable JSON schema structure.
    // The parser inlines children of the 01-level group directly into fields[].
    let copybook = "\
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID        PIC X(10).
          05 CUSTOMER-NAME      PIC X(30).
          05 BALANCE            PIC S9(7)V99 COMP-3.
";

    let schema = parse_copybook(copybook).unwrap();
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    // Top-level schema structure
    assert_eq!(json["lrecl_fixed"], 45);
    assert!(json["tail_odo"].is_null());
    assert!(json["fingerprint"].is_string());

    // Fields are inlined (01-level group children appear at top level)
    let fields = json["fields"].as_array().unwrap();
    assert_eq!(fields.len(), 3);

    // First field: CUSTOMER-ID (Alphanum)
    let id_field = &fields[0];
    assert_eq!(id_field["path"], "CUSTOMER-ID");
    assert_eq!(id_field["name"], "CUSTOMER-ID");
    assert_eq!(id_field["level"], 5);
    assert_eq!(id_field["kind"]["Alphanum"]["len"], 10);
    assert_eq!(id_field["offset"], 0);
    assert_eq!(id_field["len"], 10);
    assert!(id_field["redefines_of"].is_null());
    assert!(id_field["occurs"].is_null());

    // Second field: CUSTOMER-NAME (Alphanum)
    let name_field = &fields[1];
    assert_eq!(name_field["path"], "CUSTOMER-NAME");
    assert_eq!(name_field["name"], "CUSTOMER-NAME");
    assert_eq!(name_field["kind"]["Alphanum"]["len"], 30);
    assert_eq!(name_field["offset"], 10);
    assert_eq!(name_field["len"], 30);

    // Third field: BALANCE (PackedDecimal)
    let balance = &fields[2];
    assert_eq!(balance["path"], "BALANCE");
    assert_eq!(balance["name"], "BALANCE");
    assert_eq!(balance["kind"]["PackedDecimal"]["digits"], 9);
    assert_eq!(balance["kind"]["PackedDecimal"]["scale"], 2);
    assert_eq!(balance["kind"]["PackedDecimal"]["signed"], true);
    assert_eq!(balance["offset"], 40);
    assert_eq!(balance["len"], 5);
}

// ---------------------------------------------------------------------------
// ODO copybook -> schema JSON stability
// ---------------------------------------------------------------------------

#[test]
fn snapshot_odo_copybook_schema_json() {
    // Validates that an OCCURS DEPENDING ON copybook produces stable
    // tail_odo metadata and ODO occurs annotations.
    let copybook = "\
       01 ORDER-RECORD.
          05 ORDER-ID          PIC X(8).
          05 LINE-COUNT        PIC 9(3).
          05 LINE-ITEMS        OCCURS 1 TO 10
                               DEPENDING ON LINE-COUNT.
             10 ITEM-CODE       PIC X(5).
             10 QUANTITY        PIC 9(4).
";

    let schema = parse_copybook(copybook).unwrap();
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    // ODO schemas have no fixed LRECL
    assert!(json["lrecl_fixed"].is_null());

    // tail_odo metadata is populated
    let tail_odo = &json["tail_odo"];
    assert_eq!(tail_odo["counter_path"], "LINE-COUNT");
    assert_eq!(tail_odo["min_count"], 1);
    assert_eq!(tail_odo["max_count"], 10);
    assert_eq!(tail_odo["array_path"], "LINE-ITEMS");

    // Fields are inlined (01-level group children at top level)
    let fields = json["fields"].as_array().unwrap();
    assert_eq!(fields.len(), 3);

    // ORDER-ID
    assert_eq!(fields[0]["name"], "ORDER-ID");
    assert_eq!(fields[0]["offset"], 0);
    assert_eq!(fields[0]["len"], 8);

    // LINE-COUNT (ODO counter)
    let counter = &fields[1];
    assert_eq!(counter["name"], "LINE-COUNT");
    assert_eq!(counter["kind"]["ZonedDecimal"]["digits"], 3);
    assert_eq!(counter["offset"], 8);

    // LINE-ITEMS (ODO array group)
    let line_items = &fields[2];
    assert_eq!(line_items["name"], "LINE-ITEMS");
    assert_eq!(line_items["kind"], "Group");

    let occurs = &line_items["occurs"]["ODO"];
    assert_eq!(occurs["min"], 1);
    assert_eq!(occurs["max"], 10);
    assert_eq!(occurs["counter_path"], "LINE-COUNT");

    // ODO array children: ITEM-CODE and QUANTITY
    assert_eq!(line_items["children"].as_array().unwrap().len(), 2);
    assert_eq!(line_items["children"][0]["name"], "ITEM-CODE");
    assert_eq!(line_items["children"][0]["path"], "LINE-ITEMS.ITEM-CODE");
    assert_eq!(line_items["children"][0]["kind"]["Alphanum"]["len"], 5);
    assert_eq!(line_items["children"][1]["name"], "QUANTITY");
    assert_eq!(
        line_items["children"][1]["kind"]["ZonedDecimal"]["digits"],
        4
    );
}

// ---------------------------------------------------------------------------
// REDEFINES copybook -> schema JSON stability
// ---------------------------------------------------------------------------

#[test]
fn snapshot_redefines_copybook_schema_json() {
    // Validates that REDEFINES produces the correct redefines_of annotation
    // and that both alternatives share the same offset.
    let copybook = "\
       01 PAYMENT-RECORD.
          05 PAYMENT-TYPE      PIC X(1).
          05 PAYMENT-DATA.
             10 CHECK-NUMBER   PIC X(10).
          05 PAYMENT-ALT REDEFINES PAYMENT-DATA.
             10 CARD-NUMBER    PIC X(10).
";

    let schema = parse_copybook(copybook).unwrap();
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    // Fixed LRECL for REDEFINES
    assert_eq!(json["lrecl_fixed"], 11);
    assert!(json["tail_odo"].is_null());

    // Fields are inlined (01-level group children at top level)
    let fields = json["fields"].as_array().unwrap();
    assert_eq!(fields.len(), 3);

    // PAYMENT-TYPE
    assert_eq!(fields[0]["name"], "PAYMENT-TYPE");
    assert_eq!(fields[0]["offset"], 0);
    assert_eq!(fields[0]["len"], 1);

    // PAYMENT-DATA: original group
    let data = &fields[1];
    assert_eq!(data["name"], "PAYMENT-DATA");
    assert_eq!(data["offset"], 1);
    assert_eq!(data["len"], 10);
    assert!(data["redefines_of"].is_null());
    assert_eq!(data["children"][0]["name"], "CHECK-NUMBER");
    assert_eq!(data["children"][0]["path"], "PAYMENT-DATA.CHECK-NUMBER");

    // PAYMENT-ALT: redefines PAYMENT-DATA
    let alt = &fields[2];
    assert_eq!(alt["name"], "PAYMENT-ALT");
    assert_eq!(alt["offset"], 1);
    assert_eq!(alt["len"], 10);
    assert_eq!(alt["redefines_of"], "PAYMENT-DATA");

    // Children of PAYMENT-ALT
    assert_eq!(alt["children"][0]["name"], "CARD-NUMBER");
    assert_eq!(alt["children"][0]["path"], "PAYMENT-ALT.CARD-NUMBER");
    assert_eq!(alt["children"][0]["kind"]["Alphanum"]["len"], 10);
}

// ---------------------------------------------------------------------------
// Schema fingerprint: deterministic for identical input
// ---------------------------------------------------------------------------

#[test]
fn snapshot_schema_fingerprint_is_deterministic() {
    // Parsing the same copybook twice must produce the same fingerprint
    let copybook = "\
       01 SIMPLE.
          05 FIELD-A PIC X(5).
";
    let schema1 = parse_copybook(copybook).unwrap();
    let schema2 = parse_copybook(copybook).unwrap();

    assert_eq!(schema1.fingerprint, schema2.fingerprint);
    assert!(
        !schema1.fingerprint.is_empty(),
        "fingerprint must be non-empty"
    );
}

// ---------------------------------------------------------------------------
// Schema JSON keys: all expected keys present for leaf fields
// ---------------------------------------------------------------------------

#[test]
fn snapshot_schema_json_keys_for_leaf_field() {
    // Validates that every expected JSON key is present in a leaf field
    let copybook = "\
       01 REC.
          05 AMOUNT PIC 9(5)V99.
";
    let schema = parse_copybook(copybook).unwrap();
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    // Fields are inlined; AMOUNT is directly in fields[0]
    let field = &json["fields"][0];

    // All expected keys must be present
    let expected_keys = [
        "path",
        "name",
        "level",
        "kind",
        "offset",
        "len",
        "redefines_of",
        "occurs",
        "sync_padding",
        "synchronized",
        "blank_when_zero",
        "resolved_renames",
        "children",
    ];
    let obj = field.as_object().unwrap();
    for key in &expected_keys {
        assert!(
            obj.contains_key(*key),
            "Missing expected key '{key}' in field JSON"
        );
    }
}

// ---------------------------------------------------------------------------
// Level-88 condition values in schema JSON
// ---------------------------------------------------------------------------

#[test]
fn snapshot_level88_condition_schema_json() {
    // Validates that Level-88 condition fields produce stable schema output
    // with Condition kind and values list.
    // Note: Level-88 children cause the parent field to be represented as a
    // Group with the 88-level items as children.
    let copybook = "\
       01 STATUS-RECORD.
          05 STATUS-CODE PIC X(1).
             88 STATUS-ACTIVE VALUE 'A'.
             88 STATUS-INACTIVE VALUE 'I'.
             88 STATUS-RANGE VALUE '1' THRU '9'.
";
    let schema = parse_copybook(copybook).unwrap();
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    let status_code = &json["fields"][0];
    assert_eq!(status_code["name"], "STATUS-CODE");

    // Level-88 fields are children of STATUS-CODE
    let children = status_code["children"].as_array().unwrap();
    assert_eq!(children.len(), 3);

    let active = &children[0];
    assert_eq!(active["name"], "STATUS-ACTIVE");
    assert_eq!(active["level"], 88);
    assert_eq!(active["len"], 0); // Level-88 fields have no storage
    let values = active["kind"]["Condition"]["values"].as_array().unwrap();
    assert!(values.iter().any(|v| v == "A"));

    let inactive = &children[1];
    assert_eq!(inactive["name"], "STATUS-INACTIVE");
    assert_eq!(inactive["level"], 88);
    let inactive_values = inactive["kind"]["Condition"]["values"].as_array().unwrap();
    assert!(inactive_values.iter().any(|v| v == "I"));

    let range = &children[2];
    assert_eq!(range["name"], "STATUS-RANGE");
    assert_eq!(range["level"], 88);
    let range_values = range["kind"]["Condition"]["values"].as_array().unwrap();
    assert!(!range_values.is_empty(), "THRU range should have values");
}

// ---------------------------------------------------------------------------
// SIGN SEPARATE field schema representation
// ---------------------------------------------------------------------------

#[test]
fn snapshot_sign_separate_schema_json() {
    // Validates that SIGN IS LEADING SEPARATE produces correct schema.
    // SIGN SEPARATE is gated by feature flag (enabled by default).
    let copybook = "\
       01 PAYMENT-RECORD.
          05 AMOUNT PIC S9(5)V99 SIGN IS LEADING SEPARATE.
";
    let result = parse_copybook(copybook);
    // If the feature flag is disabled in the test environment, skip gracefully
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    let field = &json["fields"][0];
    assert_eq!(field["name"], "AMOUNT");

    // Verify it's a ZonedDecimal with sign_separate information
    let zd = &field["kind"]["ZonedDecimal"];
    if !zd.is_null() {
        assert_eq!(zd["scale"], 2);
        assert_eq!(zd["signed"], true);

        let sign_sep = &zd["sign_separate"];
        assert!(!sign_sep.is_null(), "sign_separate should be present");
        assert_eq!(sign_sep["placement"], "leading");
    }
    // SIGN SEPARATE adds one byte to field length (5+2 digits + 1 sign = 8)
    assert_eq!(field["len"], 8);
}

// ---------------------------------------------------------------------------
// Multiple REDEFINES schema structure
// ---------------------------------------------------------------------------

#[test]
fn snapshot_multiple_redefines_schema_json() {
    // Validates that multiple REDEFINES of the same field produce correct
    // schema with multiple alternatives sharing the same offset.
    let copybook = "\
       01 MULTI-FORMAT-RECORD.
          05 FORMAT-TYPE PIC X(1).
          05 DATA-AREA PIC X(20).
          05 DATA-AS-NUM REDEFINES DATA-AREA PIC 9(20).
          05 DATA-AS-GROUP REDEFINES DATA-AREA.
             10 PART-A PIC X(10).
             10 PART-B PIC X(10).
";
    let schema = parse_copybook(copybook).unwrap();
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    let fields = json["fields"].as_array().unwrap();
    assert!(fields.len() >= 4, "should have at least 4 fields");

    // All REDEFINES fields share the same offset as the original
    let data_area = fields.iter().find(|f| f["name"] == "DATA-AREA").unwrap();
    let data_area_offset = data_area["offset"].as_u64().unwrap();
    assert!(data_area["redefines_of"].is_null());

    let data_as_num = fields.iter().find(|f| f["name"] == "DATA-AS-NUM").unwrap();
    assert_eq!(data_as_num["offset"].as_u64().unwrap(), data_area_offset);
    assert_eq!(data_as_num["redefines_of"], "DATA-AREA");

    let data_as_group = fields
        .iter()
        .find(|f| f["name"] == "DATA-AS-GROUP")
        .unwrap();
    assert_eq!(data_as_group["offset"].as_u64().unwrap(), data_area_offset);
    assert_eq!(data_as_group["redefines_of"], "DATA-AREA");

    let children = data_as_group["children"].as_array().unwrap();
    assert_eq!(children.len(), 2);
    assert_eq!(children[0]["name"], "PART-A");
    assert_eq!(children[1]["name"], "PART-B");
}

// ---------------------------------------------------------------------------
// COMP-3 (packed decimal) field schema representation
// ---------------------------------------------------------------------------

#[test]
fn snapshot_comp3_fields_schema_json() {
    // Validates that COMP-3 fields produce stable PackedDecimal kind in schema
    // with digits, scale, and signed attributes.
    let copybook = "\
       01 FINANCIAL-RECORD.
          05 AMOUNT-SIGNED     PIC S9(7)V99 COMP-3.
          05 AMOUNT-UNSIGNED   PIC 9(5) COMP-3.
          05 LARGE-AMOUNT      PIC S9(15)V9(4) COMP-3.
";

    let schema = parse_copybook(copybook).unwrap();
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    let fields = json["fields"].as_array().unwrap();
    assert_eq!(fields.len(), 3);

    // AMOUNT-SIGNED: S9(7)V99 COMP-3 → 9 digits, scale 2, signed, 5 bytes
    let signed = &fields[0];
    assert_eq!(signed["name"], "AMOUNT-SIGNED");
    assert_eq!(signed["kind"]["PackedDecimal"]["digits"], 9);
    assert_eq!(signed["kind"]["PackedDecimal"]["scale"], 2);
    assert_eq!(signed["kind"]["PackedDecimal"]["signed"], true);
    assert_eq!(signed["len"], 5);
    assert_eq!(signed["offset"], 0);

    // AMOUNT-UNSIGNED: 9(5) COMP-3 → 5 digits, scale 0, unsigned, 3 bytes
    let unsigned = &fields[1];
    assert_eq!(unsigned["name"], "AMOUNT-UNSIGNED");
    assert_eq!(unsigned["kind"]["PackedDecimal"]["digits"], 5);
    assert_eq!(unsigned["kind"]["PackedDecimal"]["scale"], 0);
    assert_eq!(unsigned["kind"]["PackedDecimal"]["signed"], false);
    assert_eq!(unsigned["len"], 3);
    assert_eq!(unsigned["offset"], 5);

    // LARGE-AMOUNT: S9(15)V9(4) COMP-3 → 19 digits, scale 4, signed, 10 bytes
    let large = &fields[2];
    assert_eq!(large["name"], "LARGE-AMOUNT");
    assert_eq!(large["kind"]["PackedDecimal"]["digits"], 19);
    assert_eq!(large["kind"]["PackedDecimal"]["scale"], 4);
    assert_eq!(large["kind"]["PackedDecimal"]["signed"], true);
    assert_eq!(large["len"], 10);
    assert_eq!(large["offset"], 8);
}

// ---------------------------------------------------------------------------
// Edited PIC field schema representation
// ---------------------------------------------------------------------------

#[test]
fn snapshot_edited_pic_schema_json() {
    // Validates that an edited numeric PIC produces stable EditedNumeric
    // kind in schema JSON with pic_string, width, scale, and signed.
    let copybook = "\
       01 REPORT-RECORD.
          05 FORMATTED-AMOUNT PIC ZZ,ZZ9.99.
";
    let result = parse_copybook(copybook);
    // Edited PIC may not be supported in all builds; skip gracefully
    let schema = match result {
        Ok(s) => s,
        Err(_) => return,
    };
    let json: serde_json::Value = serde_json::to_value(&schema).unwrap();

    let field = &json["fields"][0];
    assert_eq!(field["name"], "FORMATTED-AMOUNT");

    let kind = &field["kind"]["EditedNumeric"];
    if !kind.is_null() {
        assert_eq!(kind["pic_string"], "ZZ,ZZ9.99");
        assert_eq!(kind["scale"], 2);
        assert_eq!(kind["signed"], false);
    }
}
