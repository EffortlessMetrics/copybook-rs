@comp1-comp2
Feature: COMP-1 Float and COMP-2 Double Decode, Encode, and Schema

  As a developer working with mainframe COBOL floating-point data
  I want to correctly decode, encode, and round-trip COMP-1 and COMP-2 fields
  So that single and double precision floating-point data converts faithfully

  Background:
    Given ASCII codepage

  # --- COMP-1 Schema ---

  Scenario: COMP-1 field is float_single and 4 bytes
    Given a copybook with content:
      """
      01 REC.
          05 FLT COMP-1.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FLT" should have type "float_single"
    And the field "FLT" should be 4 bytes long

  Scenario: COMP-1 field has correct level number
    Given a copybook with content:
      """
      01 REC.
          05 FLT COMP-1.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FLT" should have level 5

  Scenario: COMP-1 field at level 10 inside group
    Given a copybook with content:
      """
      01 REC.
          05 GRP.
              10 FLT COMP-1.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "GRP" should have type "group"
    And the field "FLT" should have type "float_single"
    And the field "FLT" should be 4 bytes long

  # --- COMP-2 Schema ---

  Scenario: COMP-2 field is float_double and 8 bytes
    Given a copybook with content:
      """
      01 REC.
          05 DBL COMP-2.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DBL" should have type "float_double"
    And the field "DBL" should be 8 bytes long

  Scenario: COMP-2 field has correct level number
    Given a copybook with content:
      """
      01 REC.
          05 DBL COMP-2.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DBL" should have level 5

  Scenario: COMP-2 field at level 10 inside group
    Given a copybook with content:
      """
      01 REC.
          05 GRP.
              10 DBL COMP-2.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "GRP" should have type "group"
    And the field "DBL" should have type "float_double"
    And the field "DBL" should be 8 bytes long

  # --- COMP-1 Decode ---

  Scenario: Decode COMP-1 positive float value
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

  Scenario: Decode COMP-1 negative float value
    Given a copybook with content:
      """
      01 REC.
          05 RATE COMP-1.
      """
    And binary data: "\xC2\x28\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "RATE"

  Scenario: Decode COMP-1 zero value
    Given a copybook with content:
      """
      01 REC.
          05 RATE COMP-1.
      """
    And binary data: "\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-1 unit value 1.0
    Given a copybook with content:
      """
      01 REC.
          05 RATE COMP-1.
      """
    And binary data: "\x3F\x80\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- COMP-2 Decode ---

  Scenario: Decode COMP-2 positive double value
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

  Scenario: Decode COMP-2 negative double value
    Given a copybook with content:
      """
      01 REC.
          05 PRECISE COMP-2.
      """
    And binary data: "\xC0\x45\x00\x00\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "PRECISE"

  Scenario: Decode COMP-2 zero value
    Given a copybook with content:
      """
      01 REC.
          05 PRECISE COMP-2.
      """
    And binary data: "\x00\x00\x00\x00\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-2 unit value 1.0
    Given a copybook with content:
      """
      01 REC.
          05 PRECISE COMP-2.
      """
    And binary data: "\x3F\xF0\x00\x00\x00\x00\x00\x00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Mixed records ---

  Scenario: Record with both COMP-1 and COMP-2 fields
    Given a copybook with content:
      """
      01 REC.
          05 SINGLE-FLT COMP-1.
          05 DOUBLE-FLT COMP-2.
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "SINGLE-FLT"
    And the decoded output should contain "DOUBLE-FLT"

  Scenario: Record with COMP-1, COMP-2, and alphanumeric
    Given a copybook with content:
      """
      01 REC.
          05 NAME PIC X(10).
          05 SINGLE-VAL COMP-1.
          05 DOUBLE-VAL COMP-2.
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "NAME"
    And the decoded output should contain "SINGLE-VAL"
    And the decoded output should contain "DOUBLE-VAL"

  Scenario: Record with all four COMP types
    Given a copybook with content:
      """
      01 REC.
          05 BIN-FLD PIC S9(4) COMP.
          05 PACKED-FLD PIC S9(5) COMP-3.
          05 FLOAT-FLD COMP-1.
          05 DOUBLE-FLD COMP-2.
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "BIN-FLD"
    And the decoded output should contain "PACKED-FLD"
    And the decoded output should contain "FLOAT-FLD"
    And the decoded output should contain "DOUBLE-FLD"

  # --- Offsets ---

  Scenario: COMP-1 and COMP-2 offsets are correct in mixed record
    Given a copybook with content:
      """
      01 REC.
          05 PREFIX PIC X(5).
          05 FLT COMP-1.
          05 DBL COMP-2.
          05 SUFFIX PIC X(3).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PREFIX" should have offset 0
    And the field "FLT" should have offset 5
    And the field "DBL" should have offset 9
    And the field "SUFFIX" should have offset 17

  # --- Round-trip ---

  Scenario: COMP-1 round-trip preserves bytes
    Given a copybook with content:
      """
      01 REC.
          05 RATE COMP-1.
      """
    And binary data: "\x42\x28\x00\x00"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: COMP-2 round-trip preserves bytes
    Given a copybook with content:
      """
      01 REC.
          05 PRECISE COMP-2.
      """
    And binary data: "\x40\x45\x00\x00\x00\x00\x00\x00"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless
