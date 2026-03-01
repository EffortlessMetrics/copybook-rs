@comp3-packed
Feature: COMP-3 Packed Decimal Decode, Encode, and Round-Trip

  As a developer working with mainframe COBOL data
  I want to correctly decode, encode, and round-trip COMP-3 packed decimal fields
  So that packed numeric data converts faithfully between binary and JSON

  Background:
    Given ASCII codepage

  # --- Decode positive values ---

  Scenario: Decode positive COMP-3 value 12345
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And binary data: "\x01\x23\x4C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field AMT should be 1234

  Scenario: Decode positive COMP-3 value 99999
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And binary data: "\x09\x99\x9C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMT"

  Scenario: Decode positive COMP-3 single digit
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(1) COMP-3.
      """
    And binary data: "\x7C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field AMT should be 7

  # --- Decode negative values ---

  Scenario: Decode negative COMP-3 value
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And binary data: "\x01\x23\x4D"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMT"

  Scenario: Decode negative COMP-3 large value
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(7) COMP-3.
      """
    And binary data: "\x09\x87\x65\x4D"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Decode zero ---

  Scenario: Decode COMP-3 zero with positive sign
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And binary data: "\x00\x00\x0C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field AMT should be 0

  Scenario: Decode COMP-3 zero with negative sign
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And binary data: "\x00\x00\x0D"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Decode with implied decimal (scale) ---

  Scenario: Decode COMP-3 with 2 decimal places
    Given a copybook with content:
      """
      01 REC.
          05 PRICE PIC S9(5)V99 COMP-3.
      """
    And binary data: "\x01\x23\x45\x6C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "PRICE"

  Scenario: Decode COMP-3 with 4 decimal places
    Given a copybook with content:
      """
      01 REC.
          05 RATE PIC S9(3)V9(4) COMP-3.
      """
    And binary data: "\x01\x23\x45\x6C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "RATE"

  # --- Encode JSON to COMP-3 ---

  Scenario: Encode positive number to COMP-3
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And JSON data:
      """
      {"AMT": 1234}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 3 bytes

  Scenario: Encode negative number to COMP-3
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And JSON data:
      """
      {"AMT": -5678}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 3 bytes

  Scenario: Encode zero to COMP-3
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And JSON data:
      """
      {"AMT": 0}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 3 bytes

  # --- Round-trip ---

  Scenario: COMP-3 round-trip positive value
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And binary data: "\x01\x23\x4C"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: COMP-3 round-trip negative value
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) COMP-3.
      """
    And binary data: "\x05\x67\x8D"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: COMP-3 round-trip with decimal scale
    Given a copybook with content:
      """
      01 REC.
          05 PRICE PIC S9(5)V99 COMP-3.
      """
    And binary data: "\x01\x23\x45\x6C"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Maximum digits (PIC S9(18)) ---

  Scenario: Decode COMP-3 with 18 digits
    Given a copybook with content:
      """
      01 REC.
          05 BIG-NUM PIC S9(18) COMP-3.
      """
    And binary data: "\x00\x00\x00\x00\x00\x00\x00\x12\x34\x5C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "BIG-NUM"

  # --- Unsigned COMP-3 (sign nibble F) ---

  Scenario: Decode unsigned COMP-3 with F sign nibble
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(3) COMP-3.
      """
    And binary data: "\x12\x3F"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Schema type validation ---

  Scenario: Parse COMP-3 field as packed decimal type
    Given a copybook with content:
      """
      01 REC.
          05 PKD PIC S9(7) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PKD" should have type "packed"
    And the field "PKD" should be 4 bytes long

  Scenario: Parse COMP-3 with scale has correct byte length
    Given a copybook with content:
      """
      01 REC.
          05 PKD PIC S9(5)V99 COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PKD" should have type "packed"
    And the field "PKD" should be 4 bytes long

  # --- Multi-field COMP-3 record ---

  Scenario: Decode record with multiple COMP-3 fields
    Given a copybook with content:
      """
      01 REC.
          05 AMT-A PIC S9(5) COMP-3.
          05 AMT-B PIC S9(7)V99 COMP-3.
          05 AMT-C PIC S9(3) COMP-3.
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMT-A"
    And the decoded output should contain "AMT-B"
    And the decoded output should contain "AMT-C"
