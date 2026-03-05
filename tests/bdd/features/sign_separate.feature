@sign-separate
Feature: SIGN SEPARATE Leading and Trailing for DISPLAY and COMP-3

  As a developer working with COBOL signed numeric fields
  I want SIGN SEPARATE to work correctly with various field types
  So that sign byte handling is preserved in decode, encode, and round-trip

  # --- Parse SIGN SEPARATE variations ---

  Scenario: Parse SIGN SEPARATE LEADING on 5-digit field
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should be 6 bytes long
    And the field should have sign separate information
    And the sign placement should be LEADING

  Scenario: Parse SIGN SEPARATE TRAILING on 5-digit field
    Given a copybook with content:
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should be 6 bytes long
    And the field should have sign separate information
    And the sign placement should be TRAILING

  Scenario: Parse SIGN SEPARATE LEADING with implied decimal
    Given a copybook with content:
      """
      01 REC.
          05 RATE PIC S9(3)V99 SIGN IS SEPARATE LEADING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RATE" should be 6 bytes long
    And field "RATE" should have sign separate placement "leading"

  Scenario: Parse SIGN SEPARATE TRAILING with implied decimal
    Given a copybook with content:
      """
      01 REC.
          05 RATE PIC S9(3)V99 SIGN IS SEPARATE TRAILING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RATE" should be 6 bytes long
    And field "RATE" should have sign separate placement "trailing"

  Scenario: Parse SIGN SEPARATE LEADING on 9-digit field
    Given a copybook with content:
      """
      01 REC.
          05 BIG-AMT PIC S9(9) SIGN IS SEPARATE LEADING.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BIG-AMT" should be 10 bytes long
    And field "BIG-AMT" should have sign separate placement "leading"

  # --- Decode SIGN SEPARATE LEADING ---

  Scenario: Decode SIGN SEPARATE LEADING positive value
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "+00500"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMT"

  Scenario: Decode SIGN SEPARATE LEADING negative value
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "-12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode SIGN SEPARATE LEADING zero value
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "+00000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field AMT should be 0

  Scenario: Decode SIGN SEPARATE LEADING with decimal
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 RATE PIC S9(3)V99 SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "+12345"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "RATE"

  # --- Decode SIGN SEPARATE TRAILING ---

  Scenario: Decode SIGN SEPARATE TRAILING positive value
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "00500+"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMT"

  Scenario: Decode SIGN SEPARATE TRAILING negative value
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "12345-"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode SIGN SEPARATE TRAILING zero value
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "00000+"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And decoded field AMT should be 0

  # --- Encode SIGN SEPARATE ---

  Scenario: Encode SIGN SEPARATE LEADING positive value
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And JSON data:
      """
      {"AMT": 500}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes
    And the encoded data should have leading sign

  Scenario: Encode SIGN SEPARATE LEADING negative value
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And JSON data:
      """
      {"AMT": -300}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes
    And the encoded data should have leading sign

  Scenario: Encode SIGN SEPARATE TRAILING positive value
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And JSON data:
      """
      {"AMT": 750}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes
    And the encoded data should have trailing sign

  Scenario: Encode SIGN SEPARATE TRAILING negative value
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And JSON data:
      """
      {"AMT": -999}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes
    And the encoded data should have trailing sign

  # --- Round-trip SIGN SEPARATE ---

  Scenario: Round-trip SIGN SEPARATE LEADING positive
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "+12345"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip SIGN SEPARATE LEADING negative
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "-99999"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip SIGN SEPARATE TRAILING positive
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "54321+"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip SIGN SEPARATE TRAILING negative
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01 REC.
          05 AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "00100-"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Mixed records with SIGN SEPARATE and COMP-3 ---

  Scenario: Decode record with SIGN SEPARATE LEADING and COMP-3
    Given a copybook with content:
      """
      01 REC.
          05 DISPLAY-AMT PIC S9(5) SIGN IS SEPARATE LEADING.
          05 PACKED-AMT PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "DISPLAY-AMT"
    And the decoded output should contain "PACKED-AMT"

  Scenario: Decode record with both LEADING and TRAILING sign separate
    Given a copybook with content:
      """
      01 REC.
          05 LEAD-AMT PIC S9(5) SIGN IS SEPARATE LEADING.
          05 TRAIL-AMT PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "LEAD-AMT"
    And the decoded output should contain "TRAIL-AMT"

  Scenario: Decode record with SIGN SEPARATE, overpunch, and unsigned fields
    Given a copybook with content:
      """
      01 REC.
          05 SEP-FLD PIC S9(5) SIGN IS SEPARATE LEADING.
          05 OVER-FLD PIC S9(5).
          05 UNSIGN-FLD PIC 9(5).
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "SEP-FLD"
    And the decoded output should contain "OVER-FLD"
    And the decoded output should contain "UNSIGN-FLD"
