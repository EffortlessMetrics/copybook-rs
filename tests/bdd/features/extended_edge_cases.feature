@codec-edge-cases @copybook-parsing
Feature: Extended Edge Cases

  As a developer working with COBOL data
  I want comprehensive coverage of parsing and codec edge cases
  So that boundary conditions and unusual layouts are handled correctly

  Background:
    Given ASCII codepage

  # --- Schema structure validation ---

  Scenario: Parse copybook with single PIC X field has correct top-level count
    Given a copybook with content:
      """
      01 SINGLE-REC.
          05 ONLY-FIELD PIC X(8).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level fields
    And the field "ONLY-FIELD" should have type "alphanumeric"
    And the field "ONLY-FIELD" should have offset 0
    And the field "ONLY-FIELD" should have length 8

  Scenario: Parse copybook with many fields at same level
    Given a copybook with content:
      """
      01 WIDE-REC.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC 9(3).
          05 FIELD-C PIC X(10).
          05 FIELD-D PIC 9(8).
          05 FIELD-E PIC X(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FIELD-A" should have offset 0
    And the field "FIELD-B" should have offset 5
    And the field "FIELD-C" should have offset 8
    And the field "FIELD-D" should have offset 18
    And the field "FIELD-E" should have offset 26

  Scenario: Parse copybook with FILLER field
    Given a copybook with content:
      """
      01 FILLER-REC.
          05 REAL-FIELD PIC X(5).
          05 FILLER PIC X(10).
          05 ANOTHER-FIELD PIC 9(3).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "REAL-FIELD" should have offset 0
    And the field "ANOTHER-FIELD" should have offset 15

  # --- Decode edge cases for alphanumeric ---

  Scenario: Decode alphanumeric with trailing spaces trimmed
    Given a copybook with content:
      """
      01 TRIM-REC.
          05 NAME PIC X(20).
      """
    And binary data: "HELLO               "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "NAME"

  Scenario: Decode single-character PIC X field
    Given a copybook with content:
      """
      01 CHAR-REC.
          05 STATUS PIC X(1).
      """
    And binary data: "A"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field STATUS should be "A"

  Scenario: Decode adjacent fields correctly
    Given a copybook with content:
      """
      01 ADJ-REC.
          05 LEFT-FIELD PIC X(3).
          05 RIGHT-FIELD PIC X(3).
      """
    And binary data: "ABCDEF"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field LEFT-FIELD should be "ABC"
    And decoded field RIGHT-FIELD should be "DEF"

  # --- Decode numeric edge cases ---

  Scenario: Decode zoned decimal with implied decimal
    Given a copybook with content:
      """
      01 DEC-REC.
          05 PRICE PIC 9(5)V99.
      """
    And binary data: "1234567"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode unsigned zoned decimal all zeros
    Given a copybook with content:
      """
      01 ZERO-REC.
          05 AMOUNT PIC 9(7).
      """
    And binary data: "0000000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode unsigned zoned decimal all nines
    Given a copybook with content:
      """
      01 MAX-REC.
          05 AMOUNT PIC 9(7).
      """
    And binary data: "9999999"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Group structure decode ---

  Scenario: Decode nested group with multiple children
    Given a copybook with content:
      """
      01 NEST-REC.
          05 HEADER.
              10 REC-TYPE PIC X(2).
              10 REC-SEQ PIC 9(4).
          05 BODY.
              10 DATA-A PIC X(10).
              10 DATA-B PIC 9(5).
      """
    And binary data: "AB0001HELLOWORLD00042"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field REC-TYPE should be "AB"
    And the decoded output should be valid JSON

  Scenario: Decode three-level nested group
    Given a copybook with content:
      """
      01 DEEP-REC.
          05 OUTER.
              10 MIDDLE.
                  15 INNER-VAL PIC X(5).
              10 SIBLING-VAL PIC 9(3).
      """
    And binary data: "HELLO123"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field INNER-VAL should be "HELLO"

  # --- COMP-3 boundary values ---

  Scenario: Decode COMP-3 single-byte zero
    Given a copybook with content:
      """
      01 PKD-REC.
          05 TINY-AMT PIC S9(1) COMP-3.
      """
    And binary data: "\x0C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-3 single-byte positive nine
    Given a copybook with content:
      """
      01 PKD-REC.
          05 TINY-AMT PIC S9(1) COMP-3.
      """
    And binary data: "\x9C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-3 single-byte negative
    Given a copybook with content:
      """
      01 PKD-REC.
          05 TINY-AMT PIC S9(1) COMP-3.
      """
    And binary data: "\x5D"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- COMP binary integer sizes ---

  Scenario: Decode COMP halfword (2 bytes) value 1
    Given a copybook with content:
      """
      01 BIN-REC.
          05 SHORT-VAL PIC S9(4) COMP.
      """
    And binary data: "\x00\x01"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP fullword (4 bytes) value 256
    Given a copybook with content:
      """
      01 BIN-REC.
          05 INT-VAL PIC S9(9) COMP.
      """
    And binary data: "\x00\x00\x01\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Round-trip edge cases ---

  Scenario: Round-trip single-character field
    Given a copybook with content:
      """
      01 RT-REC.
          05 FLAG PIC X(1).
      """
    And binary data: "Y"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip zoned decimal with implied decimal
    Given a copybook with content:
      """
      01 RT-REC.
          05 PRICE PIC 9(3)V99.
      """
    And binary data: "12345"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip mixed field types
    Given a copybook with content:
      """
      01 RT-REC.
          05 NAME PIC X(10).
          05 CODE PIC 9(4).
          05 FLAG PIC X(1).
      """
    And binary data: "ACME CORP 0042X"
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Projection edge cases ---

  Scenario: Project all fields returns complete schema
    Given a copybook with content:
      """
      01 PROJ-REC.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC 9(3).
      """
    And field selection: "FIELD-A,FIELD-B"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "FIELD-A" should be included in projection
    And the field "FIELD-B" should be included in projection

  Scenario: Projection error for misspelled field name
    Given a copybook with content:
      """
      01 PROJ-REC.
          05 CUSTOMER-ID PIC 9(6).
      """
    And field selection: "CUSTMER-ID"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should fail
    And the error code should be "CBKS703_PROJECTION_FIELD_NOT_FOUND"

  # --- Parsing mode edge cases ---

  Scenario: Parse copybook with only groups no leaf fields
    Given a copybook with content:
      """
      01 GROUP-ONLY.
          05 INNER-GROUP.
              10 LEAF PIC X(1).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "GROUP-ONLY" should have type "group"
    And the field "INNER-GROUP" should have type "group"
    And the field "LEAF" should have type "alphanumeric"

  Scenario: Parse copybook with multiple top-level records
    Given a copybook with content:
      """
      01 FIRST-RECORD.
          05 FIELD-1 PIC X(5).
      01 SECOND-RECORD.
          05 FIELD-2 PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed

  Scenario: Decode field with numeric value one
    Given a copybook with content:
      """
      01 ONE-REC.
          05 NUM-VAL PIC 9(5).
      """
    And binary data: "00001"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
