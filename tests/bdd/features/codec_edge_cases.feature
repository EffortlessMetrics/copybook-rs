@codec-edge-cases
Feature: Codec Edge Cases

  As a developer working with COBOL data
  I want comprehensive coverage of codec edge cases
  So that I can handle malformed, boundary, and unusual data reliably

  Background:
    Given ASCII codepage

  # --- Empty and zero-length records ---

  Scenario: Decoding empty record (zero-length binary)
    Given a copybook with content:
      """
      01 SIMPLE-REC.
         05 DATA-FIELD PIC X(5).
      """
    And binary data: ""
    When the binary data is decoded
    Then an error should occur

  # --- Truncated records ---

  Scenario: Decoding record shorter than schema expects
    Given a copybook with content:
      """
      01 WIDE-REC.
         05 FIELD-A PIC X(10).
         05 FIELD-B PIC X(10).
         05 FIELD-C PIC X(10).
      """
    And binary data: "SHORT"
    When the binary data is decoded
    Then an error should occur

  # --- Extra trailing data ---

  Scenario: Decoding record longer than schema with extra trailing data
    Given a copybook with content:
      """
      01 SMALL-REC.
         05 FIELD-A PIC X(5).
      """
    And binary data: "HELLOEXTRA_TRAILING_BYTES"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field FIELD-A should be "HELLO"

  # --- COMP-3 packed decimal invalid nibbles ---

  Scenario: COMP-3 packed decimal with invalid nibbles
    Given a copybook with content:
      """
      01 COMP3-REC.
         05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And binary data: "\xFF\xFF\xFF\xFF\xFF"
    When the binary data is decoded
    Then an error should occur

  # --- COMP-3 sign nibble variations ---

  Scenario: COMP-3 with positive sign nibble C
    Given a copybook with content:
      """
      01 COMP3-REC.
         05 AMOUNT PIC S9(3) COMP-3.
      """
    And binary data: "\x12\x3C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP-3 with negative sign nibble D
    Given a copybook with content:
      """
      01 COMP3-REC.
         05 AMOUNT PIC S9(3) COMP-3.
      """
    And binary data: "\x12\x3D"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP-3 with unsigned sign nibble F
    Given a copybook with content:
      """
      01 COMP3-REC.
         05 AMOUNT PIC S9(3) COMP-3.
      """
    And binary data: "\x12\x3F"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- DISPLAY numeric with leading/trailing spaces ---

  Scenario: DISPLAY numeric with leading spaces
    Given a copybook with content:
      """
      01 DISPLAY-REC.
         05 NUM-FIELD PIC 9(5).
      """
    And binary data: "  123"
    When the binary data is decoded
    Then an error should occur

  Scenario: DISPLAY numeric with trailing spaces
    Given a copybook with content:
      """
      01 DISPLAY-REC.
         05 NUM-FIELD PIC 9(5).
      """
    And binary data: "123  "
    When the binary data is decoded
    Then an error should occur

  # --- PIC X with special characters ---

  Scenario: PIC X field with all-spaces content
    Given a copybook with content:
      """
      01 ALPHA-REC.
         05 TEXT-FIELD PIC X(10).
      """
    And binary data: "          "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: PIC X field with binary zeros
    Given a copybook with content:
      """
      01 ALPHA-REC.
         05 TEXT-FIELD PIC X(4).
      """
    And binary data: "\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- PIC 9 with non-numeric data ---

  Scenario: PIC 9 field containing alphabetic characters
    Given a copybook with content:
      """
      01 NUM-REC.
         05 NUM-FIELD PIC 9(5).
      """
    And binary data: "ABCDE"
    When the binary data is decoded
    Then an error should occur

  Scenario: PIC 9 field containing mixed alpha-numeric
    Given a copybook with content:
      """
      01 NUM-REC.
         05 NUM-FIELD PIC 9(5).
      """
    And binary data: "12A45"
    When the binary data is decoded
    Then an error should occur

  # --- Signed fields with overpunch ---

  Scenario: Signed zoned decimal with positive overpunch
    Given a copybook with content:
      """
      01 SIGNED-REC.
         05 SIGNED-AMT PIC S9(5).
      """
    And binary data: "1234{"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Signed zoned decimal with negative overpunch
    Given a copybook with content:
      """
      01 SIGNED-REC.
         05 SIGNED-AMT PIC S9(5).
      """
    And binary data: "1234}"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- COMP (binary) fields at boundary values ---

  Scenario: COMP binary field with zero value
    Given a copybook with content:
      """
      01 BIN-REC.
         05 BIN-FIELD PIC S9(4) COMP.
      """
    And binary data: "\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP binary field with max positive halfword
    Given a copybook with content:
      """
      01 BIN-REC.
         05 BIN-FIELD PIC S9(4) COMP.
      """
    And binary data: "\x7F\xFF"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP binary field with min negative halfword
    Given a copybook with content:
      """
      01 BIN-REC.
         05 BIN-FIELD PIC S9(4) COMP.
      """
    And binary data: "\x80\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP binary fullword with zero
    Given a copybook with content:
      """
      01 BIN-REC.
         05 BIN-FIELD PIC S9(9) COMP.
      """
    And binary data: "\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP binary fullword with max positive value
    Given a copybook with content:
      """
      01 BIN-REC.
         05 BIN-FIELD PIC S9(9) COMP.
      """
    And binary data: "\x7F\xFF\xFF\xFF"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP binary fullword with min negative value
    Given a copybook with content:
      """
      01 BIN-REC.
         05 BIN-FIELD PIC S9(9) COMP.
      """
    And binary data: "\x80\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
