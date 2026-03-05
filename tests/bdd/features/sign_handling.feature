@sign-handling
Feature: Sign Handling for Zoned Decimal Fields

  As a developer working with signed COBOL numeric data
  I want to correctly handle overpunch, sign separate, and unsigned fields
  So that sign information is preserved through decode, encode, and round-trip

  Background:
    Given ASCII codepage

  # --- Overpunch positive sign decode ---

  Scenario: Overpunch positive sign decode with trailing {
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234{"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "BAL"

  Scenario: Overpunch positive sign decode with trailing A
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234A"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Overpunch positive zero
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "0000{"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field BAL should be 0

  # --- Overpunch negative sign decode ---

  Scenario: Overpunch negative sign decode with trailing }
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234}"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Overpunch negative sign decode with trailing J
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234J"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Overpunch negative large value
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(9).
      """
    And binary data: "12345678}"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Sign separate leading ---

  Scenario: Sign separate leading positive
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And binary data: "+12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMT"

  Scenario: Sign separate leading negative
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And binary data: "-12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Sign separate leading zero
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And binary data: "+00000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Sign separate trailing ---

  Scenario: Sign separate trailing positive
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And binary data: "12345+"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMT"

  Scenario: Sign separate trailing negative
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And binary data: "12345-"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Sign separate trailing zero
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And binary data: "00000+"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Sign round-trip fidelity ---

  Scenario: Overpunch sign round-trip
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234{"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Sign separate leading round-trip
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And binary data: "-12345"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Sign separate trailing round-trip
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And binary data: "12345-"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Unsigned numeric ---

  Scenario: Unsigned numeric decode
    Given a copybook with content:
      """
      01 REC.
          05 QTY PIC 9(5).
      """
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field QTY should be 12345

  Scenario: Unsigned numeric round-trip
    Given a copybook with content:
      """
      01 REC.
          05 QTY PIC 9(5).
      """
    And binary data: "12345"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Sign handling with decimal places ---

  Scenario: Overpunch sign with implied decimal
    Given a copybook with content:
      """
      01 REC.
          05 RATE PIC S9(3)V99.
      """
    And binary data: "1234{"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "RATE"

  Scenario: Sign separate leading with implied decimal
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 RATE PIC S9(3)V99 SIGN IS SEPARATE LEADING.
      """
    And binary data: "+12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Sign separate trailing with implied decimal
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 RATE PIC S9(3)V99 SIGN IS SEPARATE TRAILING.
      """
    And binary data: "12345+"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Encode with sign ---

  Scenario: Encode positive signed zoned decimal
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And JSON data:
      """
      {"BAL": 12340}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Encode negative signed zoned decimal
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And JSON data:
      """
      {"BAL": -12340}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  # --- Schema validation ---

  Scenario: Parse signed field byte length includes sign separate
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should be 6 bytes long

  Scenario: Parse signed field without sign separate
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should be 5 bytes long

  # --- Mixed sign types in one record ---

  Scenario: Decode record with mixed sign handling
    Given a copybook with content:
      """
      01 REC.
          05 OVERPUNCH-FLD PIC S9(5).
          05 UNSIGNED-FLD  PIC 9(5).
          05 PACKED-FLD    PIC S9(5) COMP-3.
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "OVERPUNCH-FLD"
    And the decoded output should contain "UNSIGNED-FLD"
    And the decoded output should contain "PACKED-FLD"

  # --- Additional sign handling scenarios ---

  Scenario: Overpunch positive sign decode with trailing B
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234B"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "BAL"

  Scenario: Overpunch positive sign decode with trailing C
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "BAL"

  Scenario: Overpunch negative sign decode with trailing K
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234K"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Overpunch negative sign decode with trailing R
    Given a copybook with content:
      """
      01 REC.
          05 BAL PIC S9(5).
      """
    And binary data: "1234R"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Unsigned numeric with implied decimal decode
    Given a copybook with content:
      """
      01 REC.
          05 RATE PIC 9(3)V99.
      """
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "RATE"

  Scenario: Parse signed field byte length with sign separate trailing
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should be 6 bytes long

  Scenario: Encode sign separate leading positive value
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And JSON data:
      """
      {"AMT": 500}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes

  Scenario: Encode sign separate trailing negative value
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And JSON data:
      """
      {"AMT": -300}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes
