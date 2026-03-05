@decode_numeric
Feature: Decode Numeric COBOL Types

  As a developer working with COBOL data
  I want to decode all numeric COBOL field types correctly
  So that I can convert mainframe binary data to JSON with full numeric fidelity

  # --- PIC 9(n) display numeric ---

  Scenario: Decode unsigned display numeric PIC 9(5)
    Given a copybook with content:
      """
      01 DISPLAY-RECORD.
          05 AMOUNT PIC 9(5).
      """
    And ASCII codepage
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMOUNT"

  Scenario: Decode unsigned display numeric with leading zeros
    Given a copybook with content:
      """
      01 DISPLAY-RECORD.
          05 AMOUNT PIC 9(5).
      """
    And ASCII codepage
    And binary data: "00042"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode unsigned display numeric all zeros
    Given a copybook with content:
      """
      01 DISPLAY-RECORD.
          05 AMOUNT PIC 9(5).
      """
    And ASCII codepage
    And binary data: "00000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- PIC S9(n) signed display (overpunch) ---

  Scenario: Decode signed display numeric positive overpunch
    Given a copybook with content:
      """
      01 SIGNED-RECORD.
          05 BALANCE PIC S9(5).
      """
    And ASCII codepage
    And binary data: "1234{"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "BALANCE"

  Scenario: Decode signed display numeric negative overpunch
    Given a copybook with content:
      """
      01 SIGNED-RECORD.
          05 BALANCE PIC S9(5).
      """
    And ASCII codepage
    And binary data: "1234}"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- PIC S9(n) COMP-3 packed decimal ---

  Scenario: Decode COMP-3 packed decimal positive value
    Given a copybook with content:
      """
      01 PACKED-RECORD.
          05 AMOUNT PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x4C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMOUNT"

  Scenario: Decode COMP-3 packed decimal negative value
    Given a copybook with content:
      """
      01 PACKED-RECORD.
          05 AMOUNT PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x4D"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-3 packed decimal zero
    Given a copybook with content:
      """
      01 PACKED-RECORD.
          05 AMOUNT PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x00\x00\x0C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-3 with decimal scale
    Given a copybook with content:
      """
      01 PACKED-RECORD.
          05 PRICE PIC S9(5)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x45\x6C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "PRICE"

  # --- PIC S9(n) COMP binary integer ---

  Scenario: Decode COMP binary halfword (2 bytes)
    Given a copybook with content:
      """
      01 BINARY-RECORD.
          05 COUNT-FIELD PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x2A"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "COUNT-FIELD"

  Scenario: Decode COMP binary fullword (4 bytes)
    Given a copybook with content:
      """
      01 BINARY-RECORD.
          05 TOTAL PIC S9(9) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x01\xE2\x40"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "TOTAL"

  Scenario: Decode COMP binary negative value
    Given a copybook with content:
      """
      01 BINARY-RECORD.
          05 OFFSET-VAL PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\xFF\xD6"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP binary zero
    Given a copybook with content:
      """
      01 BINARY-RECORD.
          05 ZERO-FIELD PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- PIC S9(n)V9(n) implied decimal point ---

  Scenario: Decode zoned decimal with implied decimal point
    Given a copybook with content:
      """
      01 DECIMAL-RECORD.
          05 RATE PIC S9(3)V99.
      """
    And ASCII codepage
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "RATE"

  Scenario: Decode packed decimal with implied decimal point
    Given a copybook with content:
      """
      01 DECIMAL-RECORD.
          05 RATE PIC S9(3)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x12\x34\x5C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- PIC S9(n) SIGN SEPARATE LEADING/TRAILING ---

  Scenario: Decode SIGN SEPARATE LEADING positive
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 SIGN-RECORD.
          05 AMOUNT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "+12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMOUNT"

  Scenario: Decode SIGN SEPARATE LEADING negative
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 SIGN-RECORD.
          05 AMOUNT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "-12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode SIGN SEPARATE TRAILING positive
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 SIGN-RECORD.
          05 AMOUNT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "12345+"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode SIGN SEPARATE TRAILING negative
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 SIGN-RECORD.
          05 AMOUNT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "12345-"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Mixed numeric types in one record ---

  Scenario: Decode record with multiple numeric types
    Given a copybook with content:
      """
      01 MIXED-RECORD.
          05 DISPLAY-NUM   PIC 9(5).
          05 SIGNED-ZONED  PIC S9(5).
          05 PACKED-AMT    PIC S9(5) COMP-3.
          05 BINARY-CNT    PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "DISPLAY-NUM"
    And the decoded output should contain "SIGNED-ZONED"
    And the decoded output should contain "PACKED-AMT"
    And the decoded output should contain "BINARY-CNT"

  # --- Schema type validation ---

  Scenario: Parse PIC 9(n) as zoned decimal type
    Given a copybook with content:
      """
      01 TYPE-RECORD.
          05 DISPLAY-FIELD PIC 9(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DISPLAY-FIELD" should have type "zoned"

  Scenario: Parse PIC S9(n) COMP-3 as packed decimal type
    Given a copybook with content:
      """
      01 TYPE-RECORD.
          05 PACKED-FIELD PIC S9(7) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PACKED-FIELD" should have type "packed"

  Scenario: Parse PIC S9(n) COMP as binary type
    Given a copybook with content:
      """
      01 TYPE-RECORD.
          05 BINARY-FIELD PIC S9(9) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BINARY-FIELD" should have type "binary"
