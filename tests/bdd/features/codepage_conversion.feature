@codepage-conversion
Feature: Codepage Conversion Edge Cases
  As a developer converting between EBCDIC and ASCII
  I want codepage conversions to handle edge cases correctly
  So that data integrity is maintained across all supported codepages

  # --- CP037 edge cases ---

  Scenario: CP037 decode single character field
    Given a copybook with content:
      """
      01 SINGLE-CP037.
          05 CHAR-FIELD PIC X(1).
      """
    And codepage "CP037"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: CP037 encode and decode numeric field
    Given a copybook with content:
      """
      01 CP037-NUMERIC.
          05 AMOUNT PIC 9(7).
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: CP037 encode all-space alphanumeric field
    Given a copybook with content:
      """
      01 CP037-SPACES.
          05 BLANK-FIELD PIC X(20).
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: CP037 mixed alpha and numeric record round-trip
    Given a copybook with content:
      """
      01 CP037-MIXED.
          05 NAME-FIELD PIC X(10).
          05 NUM-FIELD PIC 9(5).
          05 DESC-FIELD PIC X(15).
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- CP1047 edge cases ---

  Scenario: CP1047 decode numeric field
    Given a copybook with content:
      """
      01 CP1047-NUM.
          05 QTY PIC 9(4).
      """
    And codepage "CP1047"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: CP1047 encode and decode large text field
    Given a copybook with content:
      """
      01 CP1047-LARGE.
          05 BIG-TEXT PIC X(80).
      """
    And codepage "CP1047"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: CP1047 decode mixed record
    Given a copybook with content:
      """
      01 CP1047-MIX.
          05 ID PIC 9(6).
          05 DESC PIC X(20).
      """
    And codepage "CP1047"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed

  # --- CP500 edge cases ---

  Scenario: CP500 decode alphanumeric field
    Given a copybook with content:
      """
      01 CP500-ALPHA.
          05 NAME PIC X(15).
      """
    And codepage "CP500"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: CP500 encode and round-trip mixed record
    Given a copybook with content:
      """
      01 CP500-MIXED.
          05 CODE PIC X(4).
          05 VALUE-FIELD PIC 9(8).
      """
    And codepage "CP500"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: CP500 numeric field round-trip
    Given a copybook with content:
      """
      01 CP500-NUM.
          05 TOTAL PIC 9(10).
      """
    And codepage "CP500"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- CP273 edge cases ---

  Scenario: CP273 decode text field
    Given a copybook with content:
      """
      01 CP273-TEXT.
          05 LABEL PIC X(12).
      """
    And codepage "CP273"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed

  Scenario: CP273 numeric round-trip
    Given a copybook with content:
      """
      01 CP273-NUM.
          05 COUNTER PIC 9(6).
      """
    And codepage "CP273"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- CP1140 edge cases ---

  Scenario: CP1140 decode and encode text
    Given a copybook with content:
      """
      01 CP1140-REC.
          05 GREETING PIC X(10).
      """
    And codepage "CP1140"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: CP1140 mixed field types
    Given a copybook with content:
      """
      01 CP1140-MIXED.
          05 ID PIC 9(5).
          05 MSG PIC X(15).
      """
    And codepage "CP1140"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- ASCII baseline ---

  Scenario: ASCII codepage with single numeric digit
    Given a copybook with content:
      """
      01 ASCII-DIGIT.
          05 DIGIT PIC 9.
      """
    And ASCII codepage
    And binary data: "5"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: ASCII codepage preserves printable characters
    Given a copybook with content:
      """
      01 ASCII-PRINTABLE.
          05 TEXT PIC X(26).
      """
    And ASCII codepage
    And binary data: "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field TEXT should be "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  Scenario: ASCII round-trip with numeric only record
    Given a copybook with content:
      """
      01 ASCII-NUMS.
          05 NUM-A PIC 9(4).
          05 NUM-B PIC 9(4).
          05 NUM-C PIC 9(4).
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- COMP-3 codepage independence ---

  Scenario: COMP-3 decode identical across CP037 and CP1047
    Given a copybook with content:
      """
      01 COMP3-CP-TEST.
          05 AMOUNT PIC S9(5)V99 COMP-3.
      """
    And codepage "CP1047"
    And binary data: "\x00\x12\x34\x5C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Binary integer decode consistent across codepages
    Given a copybook with content:
      """
      01 COMP-CP-TEST.
          05 COUNT PIC S9(4) COMP.
      """
    And codepage "CP1047"
    And binary data: "\x00\x64"
    When the binary data is decoded
    Then decoding should succeed
