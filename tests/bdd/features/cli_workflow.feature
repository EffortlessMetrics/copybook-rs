@cli @workflow
Feature: CLI Workflow Scenarios

  As a user of the copybook CLI
  I want to run full workflows through parse, inspect, decode, encode, and verify
  So that I can validate end-to-end data processing

  Background:
    Given ASCII codepage

  # --- Parse workflow ---

  Scenario: Parse simple copybook succeeds
    Given a copybook with content:
      """
      01 SIMPLE-REC.
          05 FIELD-A PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed

  Scenario: Parse copybook with numeric fields produces correct schema
    Given a copybook with numeric fields
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)

  Scenario: Parse copybook with OCCURS clause
    Given a copybook with OCCURS clause
    When the copybook is parsed
    Then the schema should be successfully parsed

  # --- Inspect (schema validation) workflow ---

  Scenario: Inspect copybook field types
    Given a copybook with diverse field types:
      """
      01 INSPECT-REC.
          05 TEXT-FLD PIC X(10).
          05 NUM-FLD PIC 9(5)V99.
          05 PKD-FLD PIC S9(7)V99 COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "TEXT-FLD" should have type "alphanumeric"
    And field "NUM-FLD" should have type "numeric"
    And field "PKD-FLD" should have type "packed_decimal"

  Scenario: Inspect copybook field offsets
    Given a copybook with content:
      """
      01 OFFSET-REC.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC X(10).
          05 FIELD-C PIC X(3).
      """
    When the copybook is parsed
    Then the field "FIELD-A" should have offset 0
    And the field "FIELD-B" should have offset 5
    And the field "FIELD-C" should have offset 15

  Scenario: Inspect copybook field lengths
    Given a copybook with content:
      """
      01 LEN-REC.
          05 SHORT-FLD PIC X(3).
          05 LONG-FLD PIC X(50).
          05 NUM-FLD PIC 9(9).
      """
    When the copybook is parsed
    Then the field "SHORT-FLD" should have length 3
    And the field "LONG-FLD" should have length 50
    And the field "NUM-FLD" should have length 9

  # --- Decode workflow ---

  Scenario: Decode simple record produces valid JSON
    Given a copybook with content:
      """
      01 DECODE-REC.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC X(5).
      """
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field FIELD-A should be "HELLO"
    And decoded field FIELD-B should be "WORLD"

  Scenario: Decode followed by encode round-trips cleanly
    Given a copybook with content:
      """
      01 RT-REC.
          05 FIELD-A PIC X(10).
      """
    And binary data: "ROUNDTRIP!"
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Encode workflow ---

  Scenario: Encode JSON data to binary
    Given a copybook with content:
      """
      01 ENCODE-REC.
          05 NAME PIC X(10).
      """
    And JSON data: "{\"NAME\":\"TESTVALUE1\"}"
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes

  Scenario: Encode numeric JSON data
    Given a copybook with content:
      """
      01 ENCODE-NUM.
          05 AMOUNT PIC 9(7).
      """
    And JSON data: "{\"AMOUNT\":\"0001234\"}"
    When the JSON data is encoded
    Then encoding should succeed
    And encoded length should be 7 bytes

  # --- Verify workflow ---

  Scenario: Verify valid data succeeds
    Given a copybook with content:
      """
      01 VERIFY-REC.
          05 ID PIC X(5).
          05 NAME PIC X(10).
      """
    And binary data: "ID001TEST NAME "
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors

  Scenario: Verify with JSON report format
    Given a copybook with content:
      """
      01 JSON-VERIFY.
          05 FIELD-A PIC X(10).
      """
    And binary data: "TESTDATA01"
    When the data is verified with JSON report
    Then verification should succeed
    And the verify report should be valid JSON

  # --- Error output formatting ---

  Scenario: Parse error produces error with context
    Given a copybook with content:
      """
      INVALID COBOL SYNTAX
      """
    When the copybook is parsed
    Then parsing should fail
    And the error message should contain context

  Scenario: Decode with padded short data succeeds
    Given a copybook with content:
      """
      01 SHORT-REC.
          05 FIELD-A PIC X(100).
      """
    And binary data that is too short
    When the binary data is decoded
    Then decoding should succeed
