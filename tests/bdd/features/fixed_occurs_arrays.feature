@occurs-arrays
Feature: Fixed OCCURS Array Processing

  As a developer working with COBOL fixed-length arrays
  I want OCCURS arrays to be correctly parsed, decoded, and encoded
  So that repeating data structures are handled faithfully

  # --- Parsing OCCURS at various sizes ---

  Scenario: Parse OCCURS with 10 elements
    Given a copybook with content:
      """
      01 REC.
          05 ITEMS OCCURS 10 TIMES.
              10 ITEM-CODE PIC X(3).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have OCCURS count 10
    And the field "ITEM-CODE" should have type "alphanumeric"

  Scenario: Parse OCCURS with 2 elements
    Given a copybook with content:
      """
      01 REC.
          05 PAIR OCCURS 2 TIMES.
              10 PAIR-VAL PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field PAIR should have OCCURS count 2

  Scenario: Parse OCCURS with large count
    Given a copybook with content:
      """
      01 REC.
          05 ROWS OCCURS 50 TIMES.
              10 ROW-DATA PIC X(4).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ROWS should have OCCURS count 50

  Scenario: Parse OCCURS with numeric child field
    Given a copybook with content:
      """
      01 REC.
          05 SCORES OCCURS 5 TIMES.
              10 SCORE-VAL PIC 9(3).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field SCORES should have OCCURS count 5
    And the field "SCORE-VAL" should have type "zoned"
    And the field "SCORE-VAL" should have length 3

  Scenario: Parse OCCURS with mixed children
    Given a copybook with content:
      """
      01 REC.
          05 PRODUCTS OCCURS 3 TIMES.
              10 PROD-NAME PIC X(10).
              10 PROD-QTY PIC 9(4).
              10 PROD-PRICE PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field PRODUCTS should have OCCURS count 3
    And the field "PROD-NAME" should have type "alphanumeric"
    And the field "PROD-QTY" should have type "zoned"
    And the field "PROD-PRICE" should have type "zoned"

  # --- Decode OCCURS arrays ---

  Scenario: Decode OCCURS array with 2 text elements
    Given a copybook with content:
      """
      01 REC.
          05 NAMES OCCURS 2 TIMES.
              10 PERSON PIC X(6).
      """
    And ASCII codepage
    And binary data: "ALICE BOB   "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And there should be 2 ARRAY elements

  Scenario: Decode OCCURS array with 4 elements
    Given a copybook with content:
      """
      01 REC.
          05 DIRS OCCURS 4 TIMES.
              10 DIR-NAME PIC X(5).
      """
    And ASCII codepage
    And binary data: "NORTHSOUTHEAST WEST "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And there should be 4 ARRAY elements

  # --- Encode OCCURS arrays ---

  Scenario: Encode OCCURS array with numeric children
    Given a copybook with content:
      """
      01 REC.
          05 VALS OCCURS 2 TIMES.
              10 NUM-VAL PIC 9(4).
      """
    And ASCII codepage
    And JSON data:
      """
      {"VALS":[{"NUM-VAL":"0010"},{"NUM-VAL":"0020"}]}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 8 bytes

  # --- Round-trip ---

  Scenario: OCCURS array decode with text elements
    Given a copybook with content:
      """
      01 REC.
          05 TAGS OCCURS 3 TIMES.
              10 TAG PIC X(4).
      """
    And ASCII codepage
    And binary data: "AAAABBBBCCCC"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And there should be 3 ARRAY elements

  # --- OCCURS with group ---

  Scenario: Parse OCCURS with nested group children
    Given a copybook with content:
      """
      01 REC.
          05 ORDERS OCCURS 2 TIMES.
              10 ORDER-HDR.
                  15 ORDER-ID PIC 9(6).
                  15 ORDER-TYPE PIC X(1).
              10 ORDER-AMT PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ORDERS should have OCCURS count 2
    And the field "ORDER-HDR" should have type "group"
    And the field "ORDER-ID" should have type "zoned"
    And the field "ORDER-TYPE" should have type "alphanumeric"
    And the field "ORDER-AMT" should have type "zoned"
