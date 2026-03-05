@schema-validation
Feature: Schema Validation Edge Cases
  As a developer parsing COBOL copybooks
  I want the parser to validate all schema constraints correctly
  So that invalid copybooks are rejected with clear errors

  Scenario: Parse copybook with single alphanumeric field
    Given a copybook with content:
      """
      01 SIMPLE-ALPHA.
          05 TEXT-FIELD PIC X(25).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the schema should contain 1 top-level field(s)
    And the field "TEXT-FIELD" should have type "alphanumeric"

  Scenario: Parse copybook with single numeric field
    Given a copybook with content:
      """
      01 SIMPLE-NUM.
          05 NUM-FIELD PIC 9(8).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "NUM-FIELD" should have type "zoned"

  Scenario: Parse copybook with COMP-3 field
    Given a copybook with content:
      """
      01 COMP3-SCHEMA.
          05 PKD-AMT PIC S9(7)V99 COMP-3.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "PKD-AMT" should have type "packed"

  Scenario: Parse copybook with COMP field
    Given a copybook with content:
      """
      01 COMP-SCHEMA.
          05 BIN-INT PIC S9(9) COMP.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "BIN-INT" should have type "binary"

  Scenario: Parse copybook with edited PIC
    Given a copybook with content:
      """
      01 EDIT-SCHEMA.
          05 EDIT-AMT PIC ZZZ9.99.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "EDIT-AMT" should have type "edited"

  Scenario: Parse copybook with group and leaf fields
    Given a copybook with content:
      """
      01 GROUP-LEAF.
          05 GRP.
              10 LEAF-A PIC X(5).
              10 LEAF-B PIC 9(3).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "GRP" should have type "group"
    And the field "LEAF-A" should have type "alphanumeric"
    And the field "LEAF-B" should have type "zoned"

  Scenario: Parse copybook and verify leaf field count
    Given a copybook with content:
      """
      01 LEAF-COUNT.
          05 FA PIC X(5).
          05 FB PIC 9(3).
          05 FC PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the schema should contain 3 leaf fields

  Scenario: Parse copybook with OCCURS and verify
    Given a copybook with content:
      """
      01 OCCURS-VERIFY.
          05 ITEMS OCCURS 10 TIMES.
              10 ITEM-NAME PIC X(8).
      """
    When the copybook is parsed
    Then parsing should succeed
    And field ITEMS should have OCCURS count 10

  Scenario: Parse copybook with ODO and verify min/max
    Given a copybook with content:
      """
      01 ODO-VERIFY.
          05 ODO-COUNT PIC 9(3).
          05 ODO-ARRAY OCCURS 1 TO 50 TIMES DEPENDING ON ODO-COUNT.
              10 ODO-ELEM PIC X(5).
      """
    When the copybook is parsed
    Then parsing should succeed
    And field ODO-ARRAY should have ODO with counter "ODO-COUNT"
    And field ODO-ARRAY should have ODO with min 1 and max 50

  Scenario: Parse REDEFINES and verify target
    Given a copybook with content:
      """
      01 REDEF-VERIFY.
          05 ORIG PIC X(20).
          05 ALT REDEFINES ORIG.
              10 P1 PIC X(10).
              10 P2 PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "ALT" should redefine "ORIG"

  Scenario: Parse Level-88 and verify conditions
    Given a copybook with content:
      """
      01 L88-VERIFY.
          05 CODE PIC X(1).
              88 IS-YES VALUE 'Y'.
              88 IS-NO VALUE 'N'.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "CODE" should have Level-88 "IS-YES"
    And the field "CODE" should have Level-88 "IS-NO"

  Scenario: Parse RENAMES and verify level-66 count
    Given a copybook with content:
      """
      01 RENAMES-VERIFY.
          05 FIELD-X PIC X(10).
          05 FIELD-Y PIC X(10).
          66 ALIAS-XY RENAMES FIELD-X THRU FIELD-Y.
      """
    When the copybook is parsed
    Then parsing should succeed
    And there should be 1 level-66 fields

  Scenario: Schema fingerprint is generated
    Given a copybook with content:
      """
      01 FP-RECORD.
          05 FP-DATA PIC X(20).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the schema should have fingerprint

  Scenario: Parse copybook with various levels
    Given a copybook with content:
      """
      01 VARIOUS-LEVELS.
          05 LVL5 PIC X(5).
          10 LVL10 PIC X(5).
          15 LVL15 PIC X(5).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "LVL5" should have level 5

  Scenario: Parse copybook with FILLER fields
    Given a copybook with content:
      """
      01 FILLER-RECORD.
          05 REAL-FIELD PIC X(10).
          05 FILLER PIC X(5).
          05 ANOTHER-FIELD PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "REAL-FIELD" should be present
    And the field "ANOTHER-FIELD" should be present
