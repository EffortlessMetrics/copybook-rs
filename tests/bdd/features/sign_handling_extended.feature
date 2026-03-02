@sign-handling @extended
Feature: Sign Handling Extended Scenarios

  As a developer working with signed COBOL numeric data
  I want comprehensive coverage of SIGN SEPARATE, overpunch, and negative zero
  So that sign information is preserved through all operations

  Background:
    Given ASCII codepage

  # --- SIGN SEPARATE LEADING ---

  Scenario: Parse SIGN SEPARATE LEADING field
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE LEADING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field should have sign separate information
    And the sign placement should be LEADING

  Scenario: Decode SIGN SEPARATE LEADING positive
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE LEADING.
      """
    And binary data: "+12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode SIGN SEPARATE LEADING negative
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE LEADING.
      """
    And binary data: "-00100"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Encode SIGN SEPARATE LEADING preserves sign byte
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE LEADING.
      """
    And JSON data: "{\"AMT\":\"+12345\"}"
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded data should have leading sign

  # --- SIGN SEPARATE TRAILING ---

  Scenario: Parse SIGN SEPARATE TRAILING field
    Given a copybook with SIGN SEPARATE TRAILING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE TRAILING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field should have sign separate information
    And the sign placement should be TRAILING

  Scenario: Decode SIGN SEPARATE TRAILING positive
    Given a copybook with SIGN SEPARATE TRAILING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE TRAILING.
      """
    And binary data: "12345+"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode SIGN SEPARATE TRAILING negative
    Given a copybook with SIGN SEPARATE TRAILING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE TRAILING.
      """
    And binary data: "00050-"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Encode SIGN SEPARATE TRAILING preserves sign byte
    Given a copybook with SIGN SEPARATE TRAILING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE TRAILING.
      """
    And JSON data: "{\"AMT\":\"12345\"}"
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded data should have trailing sign

  # --- Overpunch sign encoding ---

  Scenario: Decode overpunch positive sign with brace
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5).
      """
    And binary data: "0100{"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode overpunch negative sign with brace
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5).
      """
    And binary data: "0100}"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Negative zero handling ---

  Scenario: Decode negative zero with SIGN SEPARATE LEADING
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE LEADING.
      """
    And binary data: "-00000"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode negative zero with SIGN SEPARATE TRAILING
    Given a copybook with SIGN SEPARATE TRAILING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE TRAILING.
      """
    And binary data: "00000-"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Roundtrip SIGN SEPARATE LEADING preserves sign
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN SEPARATE LEADING.
      """
    And binary data: "+00500"
    When the data is round-tripped
    Then the round-trip should be lossless
    And the sign placement should be preserved
