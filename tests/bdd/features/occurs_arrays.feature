@occurs-arrays
Feature: OCCURS Arrays and ODO

  As a developer working with COBOL copybooks
  I want OCCURS and OCCURS DEPENDING ON to be handled correctly
  So that array data is properly parsed, decoded, and encoded

  # --- Fixed OCCURS parsing ---

  Scenario: Simple OCCURS fixed array
    Given a copybook with content:
      """
      01 FIXED-ARRAY-RECORD.
          05 ITEMS OCCURS 5 TIMES.
              10 ITEM-NAME PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have OCCURS count 5

  Scenario: OCCURS with multiple children
    Given a copybook with content:
      """
      01 MULTI-CHILD-RECORD.
          05 ENTRIES OCCURS 3 TIMES.
              10 ENTRY-ID PIC 9(4).
              10 ENTRY-NAME PIC X(20).
              10 ENTRY-AMT PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ENTRIES should have OCCURS count 3
    And the field "ENTRY-ID" should have type "zoned"
    And the field "ENTRY-NAME" should have type "alphanumeric"
    And the field "ENTRY-AMT" should have type "zoned"

  Scenario: OCCURS DEPENDING ON (ODO) basic
    Given a copybook with ODO (OCCURS DEPENDING ON)
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field DYNAMIC-ARRAY should have ODO with counter "COUNT-FIELD"

  Scenario: ODO with explicit min and max
    Given a copybook with content:
      """
      01 ODO-RECORD.
          05 NUM-ITEMS PIC 9(2).
          05 ITEM-LIST OCCURS 1 TO 50 TIMES
              DEPENDING ON NUM-ITEMS.
              10 ITEM-VALUE PIC X(8).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEM-LIST should have ODO with min 1 and max 50
    And field ITEM-LIST should have ODO with counter "NUM-ITEMS"

  Scenario: OCCURS fixed array decode to JSON array
    Given a copybook with content:
      """
      01 DEC-ARRAY.
          05 COLORS OCCURS 3 TIMES.
              10 COLOR-NAME PIC X(5).
      """
    And ASCII codepage
    And binary data: "RED  BLUEEGREN "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: OCCURS fixed array encode from JSON array
    Given a copybook with content:
      """
      01 ENC-ARRAY.
          05 NUMS OCCURS 3 TIMES.
              10 NUM-VAL PIC 9(3).
      """
    And ASCII codepage
    And JSON data:
      """
      {"NUMS":[{"NUM-VAL":"001"},{"NUM-VAL":"002"},{"NUM-VAL":"003"}]}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 9 bytes

  Scenario: OCCURS array encode and decode
    Given a copybook with content:
      """
      01 RT-ARRAY.
          05 TAGS OCCURS 2 TIMES.
              10 TAG-CODE PIC X(4).
      """
    And ASCII codepage
    And binary data: "ABCDWXYZ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: ODO counter validation - counter within range
    Given a copybook with content:
      """
      01 ODO-VALID.
          05 CNT PIC 9(2).
          05 ELEMS OCCURS 1 TO 10 TIMES
              DEPENDING ON CNT.
              10 ELEM-DATA PIC X(5).
      """
    And ASCII codepage
    And binary data with COUNT=03 and 3 elements
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Nested group inside OCCURS
    Given a copybook with content:
      """
      01 NESTED-OCCURS.
          05 TRANSACTIONS OCCURS 2 TIMES.
              10 TXN-HEADER.
                  15 TXN-ID PIC 9(6).
                  15 TXN-TYPE PIC X(1).
              10 TXN-AMOUNT PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field TRANSACTIONS should have OCCURS count 2
    And the field "TXN-HEADER" should have type "group"
    And the field "TXN-ID" should have type "zoned"
    And the field "TXN-TYPE" should have type "alphanumeric"
    And the field "TXN-AMOUNT" should have type "zoned"

  Scenario: OCCURS at group level
    Given a copybook with content:
      """
      01 GROUP-OCCURS.
          05 LINE-ITEMS OCCURS 4 TIMES.
              10 LINE-DESC PIC X(20).
              10 LINE-QTY PIC 9(3).
              10 LINE-PRICE PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field LINE-ITEMS should have OCCURS count 4
    And the field "LINE-DESC" should have type "alphanumeric"

  Scenario: ODO with zero-tolerant dialect
    Given a copybook with content:
      """
      01 ODO-ZERO.
          05 ITEM-COUNT PIC 9(2).
          05 ITEMS OCCURS 1 TO 10 TIMES
              DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(5).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 0 and max 10

  Scenario: OCCURS preserves child field offsets
    Given a copybook with content:
      """
      01 OFFSET-OCCURS.
          05 PREFIX PIC X(2).
          05 ROWS OCCURS 2 TIMES.
              10 COL-A PIC X(3).
              10 COL-B PIC 9(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PREFIX" should have offset 0
    And the field "PREFIX" should have length 2

  Scenario: OCCURS with single element
    Given a copybook with content:
      """
      01 SINGLE-OCCURS.
          05 SOLO OCCURS 1 TIMES.
              10 SOLO-DATA PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field SOLO should have OCCURS count 1

  Scenario: OCCURS decode produces correct element count
    Given a copybook with content:
      """
      01 COUNT-CHECK.
          05 ITEMS OCCURS 3 TIMES.
              10 ITEM-VAL PIC X(4).
      """
    And ASCII codepage
    And binary data: "AAAABBBBCCCC"
    When the binary data is decoded
    Then decoding should succeed
    And there should be 3 ARRAY elements

  Scenario: ODO counter field is numeric
    Given a copybook with content:
      """
      01 ODO-NUM-CTR.
          05 MY-COUNT PIC 9(3).
          05 MY-ARRAY OCCURS 0 TO 20 TIMES
              DEPENDING ON MY-COUNT.
              10 MY-ELEMENT PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "MY-COUNT" should have type "zoned"
    And field MY-ARRAY should have ODO with counter "MY-COUNT"
