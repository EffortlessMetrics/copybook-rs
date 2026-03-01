@redefines
Feature: REDEFINES Handling

  As a developer working with COBOL copybooks
  I want REDEFINES clauses to be parsed and handled correctly
  So that overlapping field interpretations are available for decoding

  # --- Basic REDEFINES parsing ---

  Scenario: Simple REDEFINES elementary to elementary
    Given a copybook with content:
      """
      01 SIMPLE-REDEF.
          05 FIELD-ALPHA PIC X(10).
          05 FIELD-NUM REDEFINES FIELD-ALPHA PIC 9(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FIELD-NUM" should redefine "FIELD-ALPHA"

  Scenario: Group REDEFINES
    Given a copybook with content:
      """
      01 GROUP-REDEF.
          05 DATE-STRING PIC X(8).
          05 DATE-PARTS REDEFINES DATE-STRING.
              10 YEAR-PART PIC 9(4).
              10 MONTH-PART PIC 9(2).
              10 DAY-PART PIC 9(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DATE-PARTS" should redefine "DATE-STRING"
    And the field "DATE-PARTS" should have type "group"
    And the field "YEAR-PART" should have type "zoned"
    And the field "MONTH-PART" should have type "zoned"
    And the field "DAY-PART" should have type "zoned"

  Scenario: REDEFINES alphanumeric as numeric
    Given a copybook with content:
      """
      01 TYPE-REDEF.
          05 RAW-DATA PIC X(5).
          05 NUM-VIEW REDEFINES RAW-DATA PIC 9(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RAW-DATA" should have type "alphanumeric"
    And the field "NUM-VIEW" should have type "zoned"
    And the field "NUM-VIEW" should redefine "RAW-DATA"

  Scenario: REDEFINES alphanumeric as packed decimal
    Given a copybook with content:
      """
      01 PACK-REDEF.
          05 RAW-BYTES PIC X(3).
          05 PACKED-VIEW REDEFINES RAW-BYTES PIC S9(3) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RAW-BYTES" should have type "alphanumeric"
    And the field "PACKED-VIEW" should have type "packed"
    And the field "PACKED-VIEW" should redefine "RAW-BYTES"

  Scenario: Multiple REDEFINES of same field
    Given a copybook with content:
      """
      01 MULTI-REDEF.
          05 ORIGINAL PIC X(8).
          05 VIEW-AS-NUM REDEFINES ORIGINAL PIC 9(8).
          05 VIEW-AS-DATE REDEFINES ORIGINAL.
              10 V-YEAR PIC 9(4).
              10 V-MONTH PIC 9(2).
              10 V-DAY PIC 9(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "VIEW-AS-NUM" should redefine "ORIGINAL"
    And the field "VIEW-AS-DATE" should redefine "ORIGINAL"

  Scenario: REDEFINES preserves original field offset
    Given a copybook with content:
      """
      01 OFFSET-REDEF.
          05 PREFIX PIC X(5).
          05 TARGET-FIELD PIC X(10).
          05 ALT-FIELD REDEFINES TARGET-FIELD PIC 9(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TARGET-FIELD" should have offset 5
    And the field "ALT-FIELD" should have offset 5

  Scenario: REDEFINES field has same length as original
    Given a copybook with content:
      """
      01 LEN-REDEF.
          05 BASE-FIELD PIC X(20).
          05 REDEF-FIELD REDEFINES BASE-FIELD PIC X(20).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BASE-FIELD" should have length 20
    And the field "REDEF-FIELD" should have length 20

  Scenario: Decode with REDEFINES shows both views
    Given a copybook with content:
      """
      01 DECODE-REDEF.
          05 ALPHA-VIEW PIC X(5).
          05 NUM-VIEW REDEFINES ALPHA-VIEW PIC 9(5).
      """
    And ASCII codepage
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Nested group REDEFINES
    Given a copybook with content:
      """
      01 NESTED-REDEF.
          05 ADDRESS-BLOCK.
              10 ADDR-LINE-1 PIC X(30).
              10 ADDR-LINE-2 PIC X(30).
          05 ADDRESS-COMPACT REDEFINES ADDRESS-BLOCK.
              10 FULL-ADDRESS PIC X(60).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ADDRESS-COMPACT" should redefine "ADDRESS-BLOCK"
    And the field "ADDRESS-BLOCK" should have type "group"
    And the field "ADDRESS-COMPACT" should have type "group"

  Scenario: REDEFINES does not advance record offset
    Given a copybook with content:
      """
      01 NO-ADVANCE.
          05 FIRST-FIELD PIC X(10).
          05 REDEF-FIRST REDEFINES FIRST-FIELD PIC X(10).
          05 SECOND-FIELD PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FIRST-FIELD" should have offset 0
    And the field "REDEF-FIRST" should have offset 0
    And the field "SECOND-FIELD" should have offset 10

  Scenario: REDEFINES with group containing packed decimal
    Given a copybook with content:
      """
      01 MIX-REDEF.
          05 RAW-AREA PIC X(8).
          05 STRUCTURED REDEFINES RAW-AREA.
              10 ID-NUM PIC 9(4).
              10 AMOUNT PIC S9(5) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "STRUCTURED" should redefine "RAW-AREA"
    And the field "ID-NUM" should have type "zoned"
    And the field "AMOUNT" should have type "packed"

  Scenario: REDEFINES with binary integer
    Given a copybook with content:
      """
      01 BIN-REDEF.
          05 TEXT-FIELD PIC X(4).
          05 BIN-FIELD REDEFINES TEXT-FIELD PIC S9(9) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BIN-FIELD" should redefine "TEXT-FIELD"
    And the field "BIN-FIELD" should have type "binary"

  Scenario: REDEFINES round-trip decode
    Given a copybook with content:
      """
      01 RT-REDEF.
          05 NAME-FIELD PIC X(10).
          05 CODE-FIELD REDEFINES NAME-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: REDEFINES with different PIC lengths within same storage
    Given a copybook with content:
      """
      01 DIFF-LEN-REDEF.
          05 LONG-FIELD PIC X(20).
          05 SHORT-VIEW REDEFINES LONG-FIELD PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "LONG-FIELD" should have length 20
    And the field "SHORT-VIEW" should have length 10
    And the field "SHORT-VIEW" should redefine "LONG-FIELD"

  Scenario: REDEFINES preserves field ordering
    Given a copybook with content:
      """
      01 ORDER-REDEF.
          05 FIELD-A PIC X(5).
          05 FIELD-A-ALT REDEFINES FIELD-A PIC 9(5).
          05 FIELD-B PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FIELD-A" should be present
    And the field "FIELD-A-ALT" should be present
    And the field "FIELD-B" should be present

  # --- Additional REDEFINES scenarios ---

  Scenario: REDEFINES elementary to group with two children
    Given a copybook with content:
      """
      01 SPLIT-REDEF.
          05 FULL-NAME PIC X(25).
          05 NAME-PARTS REDEFINES FULL-NAME.
              10 FIRST-N PIC X(15).
              10 LAST-N PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FULL-NAME" should have type "alphanumeric"
    And the field "NAME-PARTS" should have type "group"
    And the field "NAME-PARTS" should redefine "FULL-NAME"
    And the field "FIRST-N" should have type "alphanumeric"
    And the field "LAST-N" should have type "alphanumeric"

  Scenario: REDEFINES between two groups
    Given a copybook with content:
      """
      01 REC.
          05 FORMAT-A.
              10 FA-CODE PIC X(3).
              10 FA-DATA PIC X(7).
          05 FORMAT-B REDEFINES FORMAT-A.
              10 FB-ID PIC 9(4).
              10 FB-NAME PIC X(6).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FORMAT-A" should have type "group"
    And the field "FORMAT-B" should have type "group"
    And the field "FORMAT-B" should redefine "FORMAT-A"

  Scenario: REDEFINES with numeric field and decimal
    Given a copybook with content:
      """
      01 REC.
          05 RAW-AMOUNT PIC X(7).
          05 DECIMAL-VIEW REDEFINES RAW-AMOUNT PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RAW-AMOUNT" should have type "alphanumeric"
    And the field "DECIMAL-VIEW" should have type "zoned"
    And the field "DECIMAL-VIEW" should redefine "RAW-AMOUNT"

  Scenario: Decode with REDEFINES group showing date parts
    Given a copybook with content:
      """
      01 REC.
          05 DATE-STR PIC X(8).
          05 DATE-COMP REDEFINES DATE-STR.
              10 YR PIC 9(4).
              10 MO PIC 9(2).
              10 DY PIC 9(2).
      """
    And ASCII codepage
    And binary data: "20250115"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: REDEFINES decode with alphanumeric data
    Given a copybook with content:
      """
      01 REC.
          05 DATA-A PIC X(6).
          05 DATA-B REDEFINES DATA-A PIC X(6).
      """
    And ASCII codepage
    And binary data: "FOOBAR"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "DATA-A"
