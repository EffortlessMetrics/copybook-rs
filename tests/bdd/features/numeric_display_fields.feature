@decode_numeric
Feature: Numeric Display Field Processing

  As a developer working with COBOL numeric DISPLAY data
  I want to correctly parse, decode, encode, and round-trip PIC 9 fields
  So that numeric data converts faithfully between binary and JSON

  Background:
    Given ASCII codepage

  # --- Parsing various PIC 9 sizes ---

  Scenario: Parse single-digit numeric field
    Given a copybook with content:
      """
      01 REC.
          05 DIGIT PIC 9(1).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DIGIT" should have type "zoned"
    And the field "DIGIT" should have length 1

  Scenario: Parse 10-digit numeric field
    Given a copybook with content:
      """
      01 REC.
          05 LARGE-NUM PIC 9(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "LARGE-NUM" should have type "zoned"
    And the field "LARGE-NUM" should have length 10

  Scenario: Parse 18-digit numeric field
    Given a copybook with content:
      """
      01 REC.
          05 MAX-NUM PIC 9(18).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "MAX-NUM" should have type "zoned"
    And the field "MAX-NUM" should have length 18

  Scenario: Parse numeric with implied decimal V99
    Given a copybook with content:
      """
      01 REC.
          05 PRICE PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PRICE" should have type "zoned"
    And the field "PRICE" should have length 7

  Scenario: Parse numeric with four decimal places
    Given a copybook with content:
      """
      01 REC.
          05 RATE PIC 9(3)V9(4).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RATE" should have type "zoned"
    And the field "RATE" should have length 7

  Scenario: Parse multiple numeric fields with offsets
    Given a copybook with content:
      """
      01 REC.
          05 QTY PIC 9(3).
          05 PRICE PIC 9(5)V99.
          05 TOTAL PIC 9(7)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "QTY" should have offset 0
    And the field "QTY" should have length 3
    And the field "PRICE" should have offset 3
    And the field "PRICE" should have length 7
    And the field "TOTAL" should have offset 10
    And the field "TOTAL" should have length 9

  # --- Decode numeric display ---

  Scenario: Decode value with leading zeros
    Given a copybook with content:
      """
      01 REC.
          05 SEQ-NUM PIC 9(5).
      """
    And binary data: "00001"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field SEQ-NUM should be 1

  Scenario: Decode max value all nines
    Given a copybook with content:
      """
      01 REC.
          05 MAX-VAL PIC 9(5).
      """
    And binary data: "99999"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field MAX-VAL should be 99999

  Scenario: Decode zero value
    Given a copybook with content:
      """
      01 REC.
          05 ZERO-VAL PIC 9(5).
      """
    And binary data: "00000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field ZERO-VAL should be 0

  Scenario: Decode single digit value
    Given a copybook with content:
      """
      01 REC.
          05 SINGLE PIC 9(1).
      """
    And binary data: "7"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field SINGLE should be 7

  # --- Encode numeric display ---

  Scenario: Encode numeric value to display format
    Given a copybook with content:
      """
      01 REC.
          05 QTY PIC 9(5).
      """
    And JSON data:
      """
      {"QTY": 42}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Encode zero to display format
    Given a copybook with content:
      """
      01 REC.
          05 AMOUNT PIC 9(7).
      """
    And JSON data:
      """
      {"AMOUNT": 0}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 7 bytes

  # --- Round-trip ---

  Scenario: Numeric display round-trip
    Given a copybook with content:
      """
      01 REC.
          05 COUNT PIC 9(5).
      """
    And binary data: "01234"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Numeric display with implied decimal round-trip
    Given a copybook with content:
      """
      01 REC.
          05 PRICE PIC 9(5)V99.
      """
    And binary data: "0012345"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless
