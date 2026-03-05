@redefines
Feature: Advanced REDEFINES Scenarios

  As a developer working with COBOL REDEFINES
  I want complex REDEFINES patterns to be handled correctly
  So that overlapping field interpretations work in all scenarios

  # --- REDEFINES with different type combinations ---

  Scenario: REDEFINES numeric as alphanumeric
    Given a copybook with content:
      """
      01 REC.
          05 NUM-DATA PIC 9(8).
          05 ALPHA-VIEW REDEFINES NUM-DATA PIC X(8).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "NUM-DATA" should have type "zoned"
    And the field "ALPHA-VIEW" should have type "alphanumeric"
    And the field "ALPHA-VIEW" should redefine "NUM-DATA"

  Scenario: REDEFINES with signed zoned decimal
    Given a copybook with content:
      """
      01 REC.
          05 RAW-BYTES PIC X(5).
          05 SIGNED-VIEW REDEFINES RAW-BYTES PIC S9(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RAW-BYTES" should have type "alphanumeric"
    And the field "SIGNED-VIEW" should have type "zoned"
    And the field "SIGNED-VIEW" should redefine "RAW-BYTES"

  Scenario: REDEFINES group to elementary
    Given a copybook with content:
      """
      01 REC.
          05 DATE-GROUP.
              10 DT-YEAR PIC 9(4).
              10 DT-MONTH PIC 9(2).
              10 DT-DAY PIC 9(2).
          05 DATE-STRING REDEFINES DATE-GROUP PIC X(8).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DATE-GROUP" should have type "group"
    And the field "DATE-STRING" should have type "alphanumeric"
    And the field "DATE-STRING" should redefine "DATE-GROUP"
    And the field "DATE-STRING" should have length 8

  Scenario: Three REDEFINES of same field
    Given a copybook with content:
      """
      01 REC.
          05 RAW-AREA PIC X(10).
          05 AS-NUMBER REDEFINES RAW-AREA PIC 9(10).
          05 AS-TWO-PARTS REDEFINES RAW-AREA.
              10 PART-A PIC X(5).
              10 PART-B PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AS-NUMBER" should redefine "RAW-AREA"
    And the field "AS-TWO-PARTS" should redefine "RAW-AREA"
    And the field "RAW-AREA" should have offset 0
    And the field "AS-NUMBER" should have offset 0
    And the field "AS-TWO-PARTS" should have offset 0

  Scenario: REDEFINES preserves level numbers
    Given a copybook with content:
      """
      01 REC.
          05 ORIGINAL PIC X(6).
          05 REDEF-FLD REDEFINES ORIGINAL PIC 9(6).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ORIGINAL" should have level 5
    And the field "REDEF-FLD" should have level 5

  Scenario: REDEFINES with COMP binary field
    Given a copybook with content:
      """
      01 REC.
          05 TEXT-AREA PIC X(4).
          05 BIN-VIEW REDEFINES TEXT-AREA PIC S9(9) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TEXT-AREA" should have type "alphanumeric"
    And the field "BIN-VIEW" should have type "binary"
    And the field "BIN-VIEW" should redefine "TEXT-AREA"
    And the field "BIN-VIEW" should have length 4

  # --- REDEFINES offset and length correctness ---

  Scenario: REDEFINES after prefix field has correct offset
    Given a copybook with content:
      """
      01 REC.
          05 HDR PIC X(4).
          05 BODY PIC X(10).
          05 BODY-ALT REDEFINES BODY PIC 9(10).
          05 TRAILER PIC X(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "HDR" should have offset 0
    And the field "BODY" should have offset 4
    And the field "BODY-ALT" should have offset 4
    And the field "TRAILER" should have offset 14

  Scenario: REDEFINES does not change total record length
    Given a copybook with content:
      """
      01 REC.
          05 FIELD-X PIC X(20).
          05 FIELD-X-ALT REDEFINES FIELD-X PIC X(20).
          05 FIELD-Y PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FIELD-Y" should have offset 20
    And the field "FIELD-Y" should have length 5

  # --- Decode with REDEFINES ---

  Scenario: Decode REDEFINES group with date components
    Given a copybook with content:
      """
      01 REC.
          05 DATE-RAW PIC X(8).
          05 DATE-SPLIT REDEFINES DATE-RAW.
              10 D-YEAR PIC 9(4).
              10 D-MONTH PIC 9(2).
              10 D-DAY PIC 9(2).
      """
    And ASCII codepage
    And binary data: "20241225"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: REDEFINES with groups containing packed decimal
    Given a copybook with content:
      """
      01 REC.
          05 RAW-ZONE PIC X(7).
          05 DETAIL REDEFINES RAW-ZONE.
              10 ID-CODE PIC 9(3).
              10 AMOUNT PIC S9(5) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DETAIL" should redefine "RAW-ZONE"
    And the field "ID-CODE" should have type "zoned"
    And the field "AMOUNT" should have type "packed"
    And the field "DETAIL" should have type "group"
