Feature: Decode Edge Cases

  As a developer working with COBOL data
  I want robust decoding of edge-case binary records
  So that I can handle real-world mainframe data reliably

  # --- All-spaces fields ---

  Scenario: Decode alphanumeric field with all spaces
    Given a copybook with content:
      """
      01 SPACE-RECORD.
          05 NAME-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "          "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "NAME-FIELD"

  Scenario: Decode multiple fields where some are all spaces
    Given a copybook with content:
      """
      01 MIXED-RECORD.
          05 FILLED-FIELD PIC X(5).
          05 EMPTY-FIELD PIC X(5).
          05 NUMERIC-FIELD PIC 9(5).
      """
    And ASCII codepage
    And binary data: "HELLO     00042"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field FILLED-FIELD should be "HELLO"

  # --- Truncated records ---

  Scenario: Decode record that is too short for schema
    Given a copybook with content:
      """
      01 LONG-RECORD.
          05 FIELD-A PIC X(10).
          05 FIELD-B PIC X(10).
      """
    And ASCII codepage
    And binary data: "SHORT"
    When the binary data is decoded
    Then an error should occur

  Scenario: Decode empty binary data
    Given a copybook with content:
      """
      01 SIMPLE-REC.
          05 DATA-FIELD PIC X(5).
      """
    And ASCII codepage
    And binary data: ""
    When the binary data is decoded
    Then an error should occur

  # --- COMP-3 packed decimal edge cases ---

  Scenario: Decode COMP-3 field with zero value
    Given a copybook with content:
      """
      01 COMP3-RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x00\x00\x00\x00\x0C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-3 field with positive value
    Given a copybook with content:
      """
      01 COMP3-RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x45\x67\x8C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-3 field with negative value
    Given a copybook with content:
      """
      01 COMP3-RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x45\x67\x8D"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-3 field with invalid nibble
    Given a copybook with content:
      """
      01 COMP3-RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\xFF\xFF\xFF\xFF\xFF"
    When the binary data is decoded
    Then an error should occur

  # --- Different codepages ---

  Scenario: Decode with CP037 EBCDIC codepage
    Given a copybook with content:
      """
      01 EBCDIC-RECORD.
          05 NAME-FIELD PIC X(5).
      """
    And codepage "CP037"
    And binary data: "\xC1\xC2\xC3\xC4\xC5"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "NAME-FIELD"

  Scenario: Decode with CP500 EBCDIC codepage
    Given a copybook with content:
      """
      01 EBCDIC-RECORD.
          05 NAME-FIELD PIC X(5).
      """
    And codepage "CP500"
    And binary data: "\xC1\xC2\xC3\xC4\xC5"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode with CP1047 EBCDIC codepage
    Given a copybook with content:
      """
      01 EBCDIC-RECORD.
          05 NAME-FIELD PIC X(5).
      """
    And codepage "CP1047"
    And binary data: "\xC1\xC2\xC3\xC4\xC5"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Zoned decimal edge cases ---

  Scenario: Decode zoned decimal field with all zeros
    Given a copybook with content:
      """
      01 ZONED-RECORD.
          05 AMOUNT PIC 9(5).
      """
    And ASCII codepage
    And binary data: "00000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode zoned decimal field with max value
    Given a copybook with content:
      """
      01 ZONED-RECORD.
          05 AMOUNT PIC 9(5).
      """
    And ASCII codepage
    And binary data: "99999"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Binary integer edge cases ---

  Scenario: Decode COMP binary integer with zero
    Given a copybook with content:
      """
      01 BINARY-RECORD.
          05 COUNT-FIELD PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
