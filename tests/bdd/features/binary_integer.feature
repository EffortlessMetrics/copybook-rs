@binary-integer
Feature: COMP Binary Integer Decode, Encode, and Round-Trip

  As a developer working with mainframe COBOL data
  I want to correctly decode, encode, and round-trip COMP binary integer fields
  So that binary integer data converts faithfully between binary and JSON

  Background:
    Given ASCII codepage

  # --- Halfword (2 bytes, PIC S9(4) COMP) ---

  Scenario: Decode COMP halfword positive value
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And binary data: "\x00\x2A"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field CNT should be 42

  Scenario: Decode COMP halfword negative value
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And binary data: "\xFF\xD6"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field CNT should be -42

  Scenario: Decode COMP halfword zero
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And binary data: "\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field CNT should be 0

  Scenario: Decode COMP halfword max positive (32767)
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And binary data: "\x7F\xFF"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP halfword min negative (-32768)
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And binary data: "\x80\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Fullword (4 bytes, PIC S9(9) COMP) ---

  Scenario: Decode COMP fullword positive value
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    And binary data: "\x00\x01\xE2\x40"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field TOTAL should be 123456

  Scenario: Decode COMP fullword negative value
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    And binary data: "\xFF\xFE\x1D\xC0"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field TOTAL should be -123456

  Scenario: Decode COMP fullword zero
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    And binary data: "\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field TOTAL should be 0

  Scenario: Decode COMP fullword max positive
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    And binary data: "\x7F\xFF\xFF\xFF"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Doubleword (8 bytes, PIC S9(18) COMP) ---

  Scenario: Decode COMP doubleword positive value
    Given a copybook with content:
      """
      01 REC.
          05 BIG-VAL PIC S9(18) COMP.
      """
    And binary data: "\x00\x00\x00\x00\x00\x01\xE2\x40"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field BIG-VAL should be 123456

  Scenario: Decode COMP doubleword negative value
    Given a copybook with content:
      """
      01 REC.
          05 BIG-VAL PIC S9(18) COMP.
      """
    And binary data: "\xFF\xFF\xFF\xFF\xFF\xFE\x1D\xC0"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field BIG-VAL should be -123456

  Scenario: Decode COMP doubleword zero
    Given a copybook with content:
      """
      01 REC.
          05 BIG-VAL PIC S9(18) COMP.
      """
    And binary data: "\x00\x00\x00\x00\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field BIG-VAL should be 0

  # --- Encode and round-trip ---

  Scenario: Encode positive halfword to COMP
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And JSON data:
      """
      {"CNT": 42}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 2 bytes

  Scenario: Encode negative fullword to COMP
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    And JSON data:
      """
      {"TOTAL": -123456}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: COMP halfword round-trip
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    And binary data: "\x00\x2A"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: COMP fullword round-trip
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    And binary data: "\x00\x01\xE2\x40"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: COMP doubleword round-trip
    Given a copybook with content:
      """
      01 REC.
          05 BIG-VAL PIC S9(18) COMP.
      """
    And binary data: "\x00\x00\x00\x00\x00\x01\xE2\x40"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Schema type and size validation ---

  Scenario: Parse COMP halfword has 2-byte length
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC S9(4) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CNT" should have type "binary"
    And the field "CNT" should be 2 bytes long

  Scenario: Parse COMP fullword has 4-byte length
    Given a copybook with content:
      """
      01 REC.
          05 TOTAL PIC S9(9) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TOTAL" should have type "binary"
    And the field "TOTAL" should be 4 bytes long

  Scenario: Parse COMP doubleword has 8-byte length
    Given a copybook with content:
      """
      01 REC.
          05 BIG-VAL PIC S9(18) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BIG-VAL" should have type "binary"
    And the field "BIG-VAL" should be 8 bytes long

  # --- Multi-field binary record ---

  Scenario: Decode record with multiple COMP fields
    Given a copybook with content:
      """
      01 REC.
          05 HALF-FLD PIC S9(4) COMP.
          05 FULL-FLD PIC S9(9) COMP.
          05 DBL-FLD  PIC S9(18) COMP.
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "HALF-FLD"
    And the decoded output should contain "FULL-FLD"
    And the decoded output should contain "DBL-FLD"
