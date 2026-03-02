@memory-safety
Feature: Memory Safety and Boundary Conditions
  As a developer processing mainframe data
  I want the system to handle boundary conditions safely
  So that there are no crashes, panics, or undefined behavior

  # --- Large record handling ---

  Scenario: Parse copybook with large record length
    Given a copybook with content:
      """
      01 LARGE-RECORD.
          05 FIELD-A PIC X(500).
          05 FIELD-B PIC X(500).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "FIELD-A" should have length 500
    And the field "FIELD-B" should have length 500

  Scenario: Decode large alphanumeric field
    Given a copybook with content:
      """
      01 LARGE-ALPHA-REC.
          05 BIG-FIELD PIC X(200).
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Round-trip large record
    Given a copybook with content:
      """
      01 LARGE-RT-REC.
          05 FIELD-1 PIC X(100).
          05 FIELD-2 PIC 9(50).
          05 FIELD-3 PIC X(100).
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Empty and minimal fields ---

  Scenario: Parse copybook with single byte field
    Given a copybook with content:
      """
      01 TINY-RECORD.
          05 TINY PIC X.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "TINY" should have length 1

  Scenario: Decode single byte alphanumeric field
    Given a copybook with content:
      """
      01 SINGLE-BYTE.
          05 BYTE-FIELD PIC X.
      """
    And ASCII codepage
    And binary data: "A"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field BYTE-FIELD should be "A"

  Scenario: Parse copybook with many fields
    Given a copybook with content:
      """
      01 MANY-FIELDS-REC.
          05 F01 PIC X(5).
          05 F02 PIC 9(3).
          05 F03 PIC X(5).
          05 F04 PIC 9(3).
          05 F05 PIC X(5).
          05 F06 PIC 9(3).
          05 F07 PIC X(5).
          05 F08 PIC 9(3).
          05 F09 PIC X(5).
          05 F10 PIC 9(3).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "F01" should be present
    And the field "F10" should be present

  # --- Boundary conditions ---

  Scenario: Decode record at exact LRECL boundary
    Given a copybook with content:
      """
      01 EXACT-REC.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And binary data: "ABCDE12345"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field FIELD-A should be "ABCDE"
    And decoded field FIELD-B should be "12345"

  Scenario: Verify correct field offsets in parsed schema
    Given a copybook with content:
      """
      01 OFFSET-RECORD.
          05 FIRST-FIELD PIC X(10).
          05 SECOND-FIELD PIC X(5).
          05 THIRD-FIELD PIC X(8).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "FIRST-FIELD" should have offset 0
    And the field "SECOND-FIELD" should have offset 10
    And the field "THIRD-FIELD" should have offset 15

  Scenario: Decode with padding preserved for short data
    Given a copybook with content:
      """
      01 PAD-RECORD.
          05 PAD-FIELD PIC X(20).
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Parse copybook with adjacent numeric fields
    Given a copybook with content:
      """
      01 ADJ-NUMERIC.
          05 NUM-A PIC 9(3).
          05 NUM-B PIC 9(3).
          05 NUM-C PIC 9(4).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "NUM-A" should have offset 0
    And the field "NUM-B" should have offset 3
    And the field "NUM-C" should have offset 6

  Scenario: Round-trip record with all numeric fields
    Given a copybook with content:
      """
      01 ALL-NUMERIC.
          05 N1 PIC 9(4).
          05 N2 PIC 9(6).
          05 N3 PIC 9(2).
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Decode binary integer at minimum positive value
    Given a copybook with content:
      """
      01 MIN-COMP.
          05 MIN-VAL PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x01"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Verify schema fingerprint is generated
    Given a copybook with content:
      """
      01 FINGERPRINT-REC.
          05 FP-FIELD PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the schema should have fingerprint
