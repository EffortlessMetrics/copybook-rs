@comp-binary
Feature: COMP Binary, COMP-1 Float, and COMP-2 Double

  As a developer working with mainframe COBOL binary data types
  I want to correctly decode, encode, and round-trip COMP, COMP-1, and COMP-2 fields
  So that binary integer and floating-point data converts faithfully between binary and JSON

  Background:
    Given ASCII codepage

  # ---------------------------------------------------------------------------
  # COMP halfword (2 bytes) decode
  # ---------------------------------------------------------------------------

  Scenario: COMP halfword positive decode
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And binary data: "\x00\x64"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field CNT should be 100

  # ---------------------------------------------------------------------------
  # COMP fullword (4 bytes) negative decode
  # ---------------------------------------------------------------------------

  Scenario: COMP fullword negative decode
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    And binary data: "\xFF\xFF\xFC\x18"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field TOTAL should be -1000

  # ---------------------------------------------------------------------------
  # COMP doubleword (8 bytes) zero
  # ---------------------------------------------------------------------------

  Scenario: COMP doubleword zero
    Given a copybook with content:
      """
      01 REC.
          05 BIG PIC S9(18) COMP.
      """
    And binary data: "\x00\x00\x00\x00\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field BIG should be 0

  # ---------------------------------------------------------------------------
  # COMP-1 single-precision float decode
  # ---------------------------------------------------------------------------

  Scenario: COMP-1 float decode positive
    Given a copybook with content:
      """
      01 REC.
          05 RATE COMP-1.
      """
    And binary data: "\x42\x28\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "RATE"

  Scenario: COMP-1 float decode zero
    Given a copybook with content:
      """
      01 REC.
          05 RATE COMP-1.
      """
    And binary data: "\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # ---------------------------------------------------------------------------
  # COMP-2 double-precision float decode
  # ---------------------------------------------------------------------------

  Scenario: COMP-2 double decode positive
    Given a copybook with content:
      """
      01 REC.
          05 PRECISE COMP-2.
      """
    And binary data: "\x40\x45\x00\x00\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "PRECISE"

  Scenario: COMP-2 double decode zero
    Given a copybook with content:
      """
      01 REC.
          05 PRECISE COMP-2.
      """
    And binary data: "\x00\x00\x00\x00\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # ---------------------------------------------------------------------------
  # COMP encode roundtrip
  # ---------------------------------------------------------------------------

  Scenario: COMP halfword encode roundtrip
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And binary data: "\x00\x64"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: COMP fullword encode roundtrip
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    And binary data: "\x00\x00\x03\xE8"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # ---------------------------------------------------------------------------
  # Schema size verification
  # ---------------------------------------------------------------------------

  Scenario: Schema COMP halfword is 2 bytes
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CNT" should have type "binary"
    And the field "CNT" should be 2 bytes long

  Scenario: Schema COMP fullword is 4 bytes
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TOTAL" should have type "binary"
    And the field "TOTAL" should be 4 bytes long

  Scenario: Schema COMP doubleword is 8 bytes
    Given a copybook with content:
      """
      01 REC.
          05 BIG PIC S9(18) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BIG" should have type "binary"
    And the field "BIG" should be 8 bytes long

  Scenario: Schema COMP-1 float is 4 bytes
    Given a copybook with content:
      """
      01 REC.
          05 RATE COMP-1.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RATE" should have type "float_single"
    And the field "RATE" should be 4 bytes long

  Scenario: Schema COMP-2 double is 8 bytes
    Given a copybook with content:
      """
      01 REC.
          05 PRECISE COMP-2.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PRECISE" should have type "float_double"
    And the field "PRECISE" should be 8 bytes long
