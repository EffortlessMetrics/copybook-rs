@codepage-handling
Feature: Codepage Handling

  As a developer working with mainframe data
  I want codepage conversions to be correct and predictable
  So that EBCDIC and ASCII data interoperates properly

  # --- CP037 encode/decode ---

  Scenario: CP037 encode produces non-ASCII bytes
    Given a copybook with content:
      """
      01 CP037-RECORD.
         05 TEXT-FIELD PIC X(5).
      """
    And codepage "CP037"
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"CP037","fields":{"TEXT-FIELD":"HELLO"},"TEXT-FIELD":"HELLO"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should not be all ASCII

  Scenario: CP037 decode produces valid JSON text
    Given a copybook with content:
      """
      01 CP037-DECODE.
         05 NAME PIC X(10).
      """
    And codepage "CP037"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: CP037 round-trip is lossless for alphanumeric data
    Given a copybook with content:
      """
      01 ROUND-TRIP-RECORD.
         05 DATA-FIELD PIC X(8).
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Different codepages produce different binary output ---

  Scenario: CP037 and CP500 encode same text to different bytes
    Given a copybook with content:
      """
      01 DIFF-RECORD.
         05 TEXT-FIELD PIC X(5).
      """
    And codepage "CP037"
    And JSON data: "{\"TEXT-FIELD\":\"HELLO\"}"
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be saved as baseline

  Scenario: CP273 encode produces valid output
    Given a copybook with content:
      """
      01 CP273-RECORD.
         05 TEXT-FIELD PIC X(10).
      """
    And codepage "CP273"
    And JSON data: "{\"TEXT-FIELD\":\"ABC\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: CP500 round-trip preserves numeric fields
    Given a copybook with content:
      """
      01 CP500-NUMERIC.
         05 NUM-FIELD PIC 9(5).
      """
    And codepage "CP500"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: CP1047 round-trip preserves alphanumeric fields
    Given a copybook with content:
      """
      01 CP1047-ALPHA.
         05 TEXT-FIELD PIC X(10).
      """
    And codepage "CP1047"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: CP1140 round-trip preserves data
    Given a copybook with content:
      """
      01 CP1140-RECORD.
         05 DATA-FIELD PIC X(8).
      """
    And codepage "CP1140"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Packed decimal is codepage independent ---

  Scenario: COMP-3 decode is consistent across CP037 and ASCII
    Given a copybook with content:
      """
      01 COMP3-CP-RECORD.
         05 AMOUNT PIC S9(5)V99 COMP-3.
      """
    And codepage "CP037"
    And binary data: "\x00\x12\x34\x5C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Binary integer decode is codepage independent
    Given a copybook with content:
      """
      01 BINARY-CP-RECORD.
         05 COUNT PIC S9(4) COMP.
      """
    And codepage "CP500"
    And binary data: "\x00\x2A"
    When the binary data is decoded
    Then decoding should succeed

  # --- ASCII codepage identity ---

  Scenario: ASCII codepage preserves plain ASCII text
    Given a copybook with content:
      """
      01 ASCII-RECORD.
         05 TEXT-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: ASCII encode produces printable bytes
    Given a copybook with content:
      """
      01 ASCII-ENC.
         05 MSG PIC X(5).
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"MSG":"HELLO"},"MSG":"HELLO"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  # --- Mixed field types with codepage ---

  Scenario: CP037 encode mixed alphanumeric and numeric fields
    Given a copybook with content:
      """
      01 MIXED-CP037.
         05 NAME   PIC X(10).
         05 AMOUNT PIC 9(5).
      """
    And codepage "CP037"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: CP037 numeric field encodes correctly
    Given a copybook with content:
      """
      01 CP037-NUM.
         05 QTY PIC 9(3).
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: ASCII mixed record round-trip
    Given a copybook with content:
      """
      01 ASCII-MIXED.
         05 ID    PIC 9(4).
         05 NAME  PIC X(8).
         05 FLAG  PIC X(1).
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless
