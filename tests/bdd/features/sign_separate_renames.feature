Feature: SIGN SEPARATE and RENAMES R4-R6 Support

  As a developer working with COBOL data
  I want to parse copybooks with SIGN SEPARATE clause and RENAMES R4-R6 rules
  So that I can correctly handle signed numeric fields with separate signs and field aliasing

  Scenario: Parse copybook with SIGN SEPARATE LEADING
    Given a copybook with SIGN SEPARATE LEADING clause
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field should have sign separate information
    And the sign placement should be LEADING

  Scenario: Parse copybook with SIGN SEPARATE TRAILING
    Given a copybook with SIGN SEPARATE TRAILING clause
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field should have sign separate information
    And the sign placement should be TRAILING

  Scenario: Decode field with SIGN SEPARATE LEADING
    Given a copybook with SIGN SEPARATE LEADING
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "-1234"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded value should be "-1234"

  Scenario: Decode field with SIGN SEPARATE TRAILING
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "1234-"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded value should be "-1234"

  Scenario: Encode field with SIGN SEPARATE LEADING
    Given a copybook with SIGN SEPARATE LEADING
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And JSON data:
      """
      {"SIGNED-FIELD": "-1234"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded data should have leading sign

  Scenario: Encode field with SIGN SEPARATE TRAILING
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And JSON data:
      """
      {"SIGNED-FIELD": "-1234"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded data should have trailing sign

  Scenario: Round-trip field with SIGN SEPARATE LEADING
    Given a copybook with SIGN SEPARATE LEADING
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "-1234"
    When the data is round-tripped
    Then round-trip should be lossless
    And the sign placement should be preserved

  Scenario: Round-trip field with SIGN SEPARATE TRAILING
    Given a copybook with SIGN SEPARATE TRAILING
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data: "1234-"
    When the data is round-tripped
    Then round-trip should be lossless
    And the sign placement should be preserved

  Scenario: Parse RENAMES R4 - Multiple REDEFINES
    Given a copybook with RENAMES R4 (multiple REDEFINES)
      """
      01  RECORD.
          05  BASE-FIELD PIC X(10).
          05  REDEF1 PIC 9(10) REDEFINES BASE-FIELD.
          05  REDEF2 PIC X(10) REDEFINES BASE-FIELD.
          05  REDEF3 PIC S9(5) REDEFINES BASE-FIELD.
      66  ALIAS-FIELD RENAMES REDEF1 THRU REDEF3.
      """
    When the copybook is parsed
    Then parsing should succeed
    And RENAMES field should be resolved
    And the alias should cover all REDEFINES fields

  Scenario: Parse RENAMES R5 - OCCURS with DEPENDING ON
    Given a copybook with RENAMES R5 (ODO)
      """
      01  RECORD.
          05  COUNT-FIELD PIC 9(3).
          05  ARRAY-FIELD PIC X(5) OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
      66  ALIAS-FIELD RENAMES ARRAY-FIELD.
      """
    When the copybook is parsed
    Then parsing should succeed
    And RENAMES field should be resolved
    And the alias should reference the ODO field

  Scenario: Parse RENAMES R6 - Level-88 after RENAMES
    Given a copybook with RENAMES R6 (Level-88 after RENAMES)
      """
      01  RECORD.
          05  BASE-FIELD PIC X(10).
          05  REDEF1 PIC 9(10) REDEFINES BASE-FIELD.
          05  REDEF2 PIC X(10) REDEFINES BASE-FIELD.
          66  ALIAS-FIELD RENAMES REDEF1 THRU REDEF2.
              88  ALIAS-VALID VALUE 'A'.
              88  ALIAS-INVALID VALUE 'X'.
      """
    When the copybook is parsed
    Then parsing should succeed
    And RENAMES field should have Level-88 conditions
    And the conditions should be properly associated

  Scenario: Decode RENAMES R4 field
    Given a copybook with RENAMES R4
      """
      01  RECORD.
          05  BASE-FIELD PIC X(10).
          05  REDEF1 PIC 9(10) REDEFINES BASE-FIELD.
          05  REDEF2 PIC X(10) REDEFINES BASE-FIELD.
      66  ALIAS-FIELD RENAMES REDEF1 THRU REDEF2.
      """
    And ASCII codepage
    And binary data: "1234567890ABCDEFGHIJ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "ALIAS-FIELD"

  Scenario: Encode RENAMES R4 field
    Given a copybook with RENAMES R4
      """
      01  RECORD.
          05  BASE-FIELD PIC X(10).
          05  REDEF1 PIC 9(10) REDEFINES BASE-FIELD.
          05  REDEF2 PIC X(10) REDEFINES BASE-FIELD.
      66  ALIAS-FIELD RENAMES REDEF1 THRU REDEF2.
      """
    And ASCII codepage
    And JSON data:
      """
      {"ALIAS-FIELD": "1234567890ABCDEFGHIJ"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded data should match the original

  Scenario: Round-trip RENAMES R4 field
    Given a copybook with RENAMES R4
      """
      01  RECORD.
          05  BASE-FIELD PIC X(10).
          05  REDEF1 PIC 9(10) REDEFINES BASE-FIELD.
          05  REDEF2 PIC X(10) REDEFINES BASE-FIELD.
      66  ALIAS-FIELD RENAMES REDEF1 THRU REDEF2.
      """
    And ASCII codepage
    And binary data: "1234567890ABCDEFGHIJ"
    When the data is round-tripped
    Then round-trip should be lossless
    And the RENAMES structure should be preserved

  Scenario: Decode RENAMES R5 with ODO
    Given a copybook with RENAMES R5
      """
      01  RECORD.
          05  COUNT-FIELD PIC 9(3).
          05  ARRAY-FIELD PIC X(5) OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
      66  ALIAS-FIELD RENAMES ARRAY-FIELD.
      """
    And ASCII codepage
    And binary data: "003ELEM1ELEM2ELEM3"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "ALIAS-FIELD"
    And the array should be properly decoded

  Scenario: Decode RENAMES R6 with Level-88
    Given a copybook with RENAMES R6
      """
      01  RECORD.
          05  BASE-FIELD PIC X(10).
          05  REDEF1 PIC 9(10) REDEFINES BASE-FIELD.
          05  REDEF2 PIC X(10) REDEFINES BASE-FIELD.
      66  ALIAS-FIELD RENAMES REDEF1 THRU REDEF2.
          88  ALIAS-VALID VALUE 'A'.
          88  ALIAS-INVALID VALUE 'X'.
      """
    And ASCII codepage
    And binary data: "1234567890ABCDEFGHIJ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain Level-88 conditions

  Scenario: Combined SIGN SEPARATE and RENAMES
    Given a copybook with both SIGN SEPARATE and RENAMES
      """
      01  RECORD.
          05  BASE-FIELD PIC S9(5) SIGN IS SEPARATE LEADING.
          05  REDEF1 PIC X(6) REDEFINES BASE-FIELD.
          05  REDEF2 PIC 9(6) REDEFINES BASE-FIELD.
      66  ALIAS-FIELD RENAMES REDEF1 THRU REDEF2.
      """
    And ASCII codepage
    And binary data: "-1234ABCD12"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "ALIAS-FIELD"
    And the sign should be properly handled

  Scenario: Error on invalid SIGN SEPARATE placement
    Given a copybook with invalid SIGN SEPARATE
      """
      01  RECORD.
          05  SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE INVALID.
      """
    When the copybook is parsed
    Then parsing should fail
    And the error should indicate invalid SIGN SEPARATE placement

  Scenario: Error on invalid RENAMES range
    Given a copybook with invalid RENAMES range
      """
      01  RECORD.
          05  FIELD1 PIC X(5).
          05  FIELD2 PIC X(5).
      66  ALIAS-FIELD RENAMES FIELD1 THRU NONEXISTENT.
      """
    When the copybook is parsed
    Then parsing should fail
    And the error should indicate invalid RENAMES range
