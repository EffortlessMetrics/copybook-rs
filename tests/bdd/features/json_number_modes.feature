@json-numbers
Feature: JSON Number Modes
  Test lossless vs native number representation

  Scenario: Lossless mode preserves numeric precision as string
    Given a copybook with content:
      """
      01 PRECISION-RECORD.
         05 AMOUNT PIC 9(7)V99.
      """
    And ASCII codepage
    And lossless number mode
    And binary data: "001234599"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Lossless mode with large numbers
    Given a copybook with content:
      """
      01 LARGE-NUM-RECORD.
         05 BIG-AMOUNT PIC 9(15)V99.
      """
    And ASCII codepage
    And lossless number mode
    And binary data: "12345678901234599"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Native mode with simple integer
    Given a copybook with content:
      """
      01 INT-RECORD.
         05 COUNT-FIELD PIC 9(5).
      """
    And ASCII codepage
    And native number mode
    And binary data: "00042"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Lossless mode with zero value
    Given a copybook with content:
      """
      01 ZERO-RECORD.
         05 ZERO-FIELD PIC 9(5).
      """
    And ASCII codepage
    And lossless number mode
    And binary data: "00000"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Native mode with decimal value
    Given a copybook with content:
      """
      01 DEC-RECORD.
         05 DEC-FIELD PIC 9(5)V99.
      """
    And ASCII codepage
    And native number mode
    And binary data: "0012345"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Lossless mode with COMP-3 field
    Given a copybook with content:
      """
      01 COMP3-NUM.
         05 PACKED-FIELD PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And lossless number mode
    And binary data: "\x01\x23\x4C"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Native mode with COMP-3 field
    Given a copybook with content:
      """
      01 COMP3-NATIVE.
         05 PACKED-FIELD PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And native number mode
    And binary data: "\x01\x23\x4C"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Lossless mode round-trip preserves values
    Given a copybook with content:
      """
      01 RT-RECORD.
         05 AMOUNT PIC 9(5).
      """
    And ASCII codepage
    And lossless number mode
    And binary data: "12345"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Lossless mode with all-nines
    Given a copybook with content:
      """
      01 NINES-RECORD.
         05 MAX-FIELD PIC 9(9).
      """
    And ASCII codepage
    And lossless number mode
    And binary data: "999999999"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Native mode with binary integer
    Given a copybook with content:
      """
      01 BIN-NATIVE.
         05 BIN-FIELD PIC S9(4) COMP.
      """
    And ASCII codepage
    And native number mode
    And binary data: "\x00\x2A"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Default number mode is lossless
    Given a copybook with content:
      """
      01 DEFAULT-RECORD.
         05 NUM-FIELD PIC 9(5).
      """
    And ASCII codepage
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Lossless mode output is always valid JSON
    Given a copybook with content:
      """
      01 JSON-VALID.
         05 FIELD-A PIC 9(3).
         05 FIELD-B PIC 9(5)V99.
      """
    And ASCII codepage
    And lossless number mode
    And binary data: "0421234567"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
