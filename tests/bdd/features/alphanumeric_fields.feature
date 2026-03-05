@copybook-parsing
Feature: Alphanumeric Field Processing

  As a developer working with COBOL alphanumeric data
  I want to correctly parse, decode, encode, and round-trip PIC X fields
  So that text data converts faithfully between binary and JSON

  Background:
    Given ASCII codepage

  # --- Parsing various PIC X sizes ---

  Scenario: Parse single-character alphanumeric field
    Given a copybook with content:
      """
      01 REC.
          05 FLAG PIC X(1).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FLAG" should have type "alphanumeric"
    And the field "FLAG" should have length 1

  Scenario: Parse medium alphanumeric field
    Given a copybook with content:
      """
      01 REC.
          05 DESCRIPTION PIC X(50).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DESCRIPTION" should have type "alphanumeric"
    And the field "DESCRIPTION" should have length 50

  Scenario: Parse large alphanumeric field
    Given a copybook with content:
      """
      01 REC.
          05 COMMENTS PIC X(200).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "COMMENTS" should have type "alphanumeric"
    And the field "COMMENTS" should have length 200

  Scenario: Parse multiple alphanumeric fields with correct offsets
    Given a copybook with content:
      """
      01 REC.
          05 FIRST-NAME PIC X(15).
          05 LAST-NAME PIC X(20).
          05 MIDDLE-INIT PIC X(1).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FIRST-NAME" should have offset 0
    And the field "FIRST-NAME" should have length 15
    And the field "LAST-NAME" should have offset 15
    And the field "LAST-NAME" should have length 20
    And the field "MIDDLE-INIT" should have offset 35
    And the field "MIDDLE-INIT" should have length 1

  # --- Decode alphanumeric ---

  Scenario: Decode simple text value
    Given a copybook with content:
      """
      01 REC.
          05 CITY PIC X(10).
      """
    And binary data: "NEW YORK  "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field CITY should be "NEW YORK"

  Scenario: Decode full-length text value without padding
    Given a copybook with content:
      """
      01 REC.
          05 CODE PIC X(5).
      """
    And binary data: "ABCDE"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field CODE should be "ABCDE"

  Scenario: Decode all-spaces field as blank
    Given a copybook with content:
      """
      01 REC.
          05 EMPTY-FIELD PIC X(8).
      """
    And binary data: "        "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field EMPTY-FIELD should be blank

  Scenario: Decode single character field
    Given a copybook with content:
      """
      01 REC.
          05 YN-FLAG PIC X(1).
      """
    And binary data: "Y"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field YN-FLAG should be "Y"

  # --- Encode alphanumeric ---

  Scenario: Encode simple alphanumeric value
    Given a copybook with content:
      """
      01 REC.
          05 NAME PIC X(10).
      """
    And JSON data:
      """
      {"NAME":"SMITH"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes

  Scenario: Encode full-length alphanumeric value
    Given a copybook with content:
      """
      01 REC.
          05 CODE PIC X(3).
      """
    And JSON data:
      """
      {"CODE":"ABC"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 3 bytes

  # --- Round-trip ---

  Scenario: Alphanumeric round-trip preserves value
    Given a copybook with content:
      """
      01 REC.
          05 LABEL PIC X(8).
      """
    And binary data: "TESTDATA"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Alphanumeric round-trip with trailing spaces
    Given a copybook with content:
      """
      01 REC.
          05 NAME PIC X(10).
      """
    And binary data: "HELLO     "
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Mixed records ---

  Scenario: Parse record with mixed alphanumeric and numeric fields
    Given a copybook with content:
      """
      01 CUSTOMER.
          05 CUST-NAME PIC X(20).
          05 CUST-ID PIC 9(8).
          05 CUST-ADDR PIC X(30).
          05 CUST-BAL PIC 9(7)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CUST-NAME" should have type "alphanumeric"
    And the field "CUST-ID" should have type "zoned"
    And the field "CUST-ADDR" should have type "alphanumeric"
    And the field "CUST-BAL" should have type "zoned"

  Scenario: Decode record with alphanumeric in nested group
    Given a copybook with content:
      """
      01 PERSON.
          05 NAME-GROUP.
              10 FIRST PIC X(10).
              10 LAST PIC X(15).
          05 AGE PIC 9(3).
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "FIRST"
    And the decoded output should contain "LAST"
    And the decoded output should contain "AGE"
