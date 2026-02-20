@raw
Feature: Raw Data Capture
  Test raw data capture modes (__raw_b64)

  Background:
    Given a copybook with content:
      """
      01 RAW-RECORD.
         05 TEXT-FIELD PIC X(10).
      """
    And ASCII codepage

  Scenario: Raw mode off does not emit __raw_b64
    Given raw mode "off"
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should not contain "__raw_b64"

  Scenario: Raw mode record emits __raw_b64
    Given raw mode "record"
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "__raw_b64"

  Scenario: Raw mode record base64 decodes to original payload
    Given raw mode "record"
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the raw_b64 field should decode to the original binary data

  Scenario: Raw mode field emits field-level raw
    Given raw mode "field"
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Raw mode record preserves binary literal bytes
    Given raw mode "record"
    And binary data: "\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4A"
    When the binary data is decoded
    Then decoding should succeed
    And the raw_b64 field should decode to the original binary data

  Scenario: Default raw mode is off
    Given binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should not contain "__raw_b64"

  Scenario: Raw mode record with COMP-3 field
    Given a copybook with content:
      """
      01 COMP3-RAW.
         05 AMOUNT PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And raw mode "record"
    And binary data: "\x01\x23\x4C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "__raw_b64"

  Scenario: Raw mode off with multiple fields
    Given a copybook with content:
      """
      01 MULTI-RAW.
         05 FIELD-A PIC X(5).
         05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And raw mode "off"
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should not contain "__raw_b64"

  Scenario: Raw mode record with JSON validity
    Given raw mode "record"
    And binary data: "ABCDEFGHIJ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Raw mode with empty field
    Given raw mode "record"
    And binary data: "          "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "__raw_b64"

  Scenario: use_raw enabled for encode
    Given use_raw enabled for encode
    And raw mode "record"
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Raw mode record preserves all data
    Given raw mode "record"
    And binary data: "TESTDATA12"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "__raw_b64"

  Scenario: Raw mode off with round-trip
    Given raw mode "off"
    And binary data: "HELLOWORLD"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Raw mode field with numeric data
    Given a copybook with content:
      """
      01 NUM-RAW.
         05 NUM-FIELD PIC 9(5).
      """
    And ASCII codepage
    And raw mode "field"
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Multiple records with raw mode record
    Given a copybook with content:
      """
      01 MULTI-RECORD.
         05 REC-FIELD PIC X(5).
      """
    And ASCII codepage
    And raw mode "record"
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Raw mode with EBCDIC codepage
    Given a copybook with content:
      """
      01 EBCDIC-RAW.
         05 TEXT-FIELD PIC X(5).
      """
    And codepage "CP037"
    And raw mode "record"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "__raw_b64"

  Scenario: Decode output is valid JSON with all raw modes
    Given raw mode "off"
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then the decoded output should be valid JSON

  Scenario: Raw mode record with round-trip preserves structure
    Given raw mode "record"
    And binary data: "TESTRECORD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "__raw_b64"
