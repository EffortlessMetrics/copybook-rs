Feature: Error Handling and Edge Cases

  As a developer working with COBOL copybooks
  I want clear and informative error messages when things go wrong
  So that I can quickly identify and fix issues

  Scenario: Parse copybook with syntax error
    Given an invalid copybook with syntax error
    When the copybook is parsed
    Then an error should occur
    And the error message should contain "syntax"

  Scenario: Parse copybook with invalid OCCURS clause
    Given a copybook with invalid OCCURS clause
    When the copybook is parsed
    Then an error should occur
    And the error message should contain "OCCURS"

  Scenario: Parse copybook with invalid PIC clause
    Given a copybook with invalid PIC clause
    When the copybook is parsed
    Then an error should occur
    And the error message should contain "PIC"

  Scenario: Decode binary data that is too short
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data that is too short
    When the binary data is decoded
    Then an error should occur

  Scenario: Decode binary data with invalid encoding
    Given a simple copybook with a single field
    And EBCDIC codepage
    And binary data with invalid encoding
    When the binary data is decoded
    Then an error should occur

  Scenario: Encode JSON data with missing required fields
    Given a simple copybook with a single field
    And ASCII codepage
    And JSON data with missing required fields
    When the JSON data is encoded
    Then an error should occur

  Scenario: Encode JSON data with invalid field types
    Given a simple copybook with a single field
    And ASCII codepage
    And JSON data with invalid field types
    When the JSON data is encoded
    Then an error should occur

  Scenario: Parse copybook with duplicate field names
    Given a copybook with content:
      """
      01 DUPLICATE-RECORD.
          05 FIELD-NAME PIC X(5).
          05 FIELD-NAME PIC 9(5).
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Parse copybook with invalid level numbers
    Given a copybook with content:
      """
      99 INVALID-LEVEL.
          05 FIELD-1 PIC X(5).
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Parse copybook with missing level numbers
    Given a copybook with content:
      """
      INVALID-RECORD.
          05 FIELD-1 PIC X(5).
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Parse copybook with unterminated string
    Given a copybook with content:
      """
      01 UNTERMINATED-RECORD.
          05 FIELD-1 PIC X(10) VALUE 'ABC.
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Parse copybook with invalid REDEFINES
    Given a copybook with content:
      """
      01 REDEFINES-TEST.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 REDEFINES NON-EXISTENT PIC 9(5).
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Parse copybook with circular REDEFINES
    Given a copybook with content:
      """
      01 CIRCULAR-TEST.
          05 FIELD-1 REDEFINES FIELD-2 PIC X(10).
          05 FIELD-2 REDEFINES FIELD-1 PIC X(10).
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Decode with ODO count exceeding maximum
    Given a copybook with ODO (OCCURS DEPENDING ON)
    And ASCII codepage
    And binary data: "999ABCDEFGH"
    When the binary data is decoded
    Then an error should occur

  Scenario: Decode with ODO count below minimum
    Given a copybook with ODO (OCCURS DEPENDING ON)
    And ASCII codepage
    And binary data: "000ABCDEFGH"
    When the binary data is decoded
    Then an error should occur

  Scenario: Encode JSON with invalid ODO count
    Given a copybook with ODO (OCCURS DEPENDING ON)
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"COUNT-FIELD":200,"DYNAMIC-ARRAY":[{"ELEMENT":"ELEMENT0001"}]},"COUNT-FIELD":200,"DYNAMIC-ARRAY":[{"ELEMENT":"ELEMENT0001"}]}
      """
    When the JSON data is encoded
    Then an error should occur

  Scenario: Parse copybook with Level-88 without parent
    Given a copybook with content:
      """
      88 ORPHAN-CONDITION VALUE 'A'.
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Parse copybook with Level-88 without VALUE clause
    Given a copybook with content:
      """
      01 STATUS-RECORD.
          05 STATUS-CODE PIC X(1).
              88 INVALID-CONDITION.
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Decode empty binary data
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data: ""
    When the binary data is decoded
    Then an error should occur

  Scenario: Encode empty JSON data
    Given a simple copybook with a single field
    And ASCII codepage
    And JSON data:
      """
      """
    When the JSON data is encoded
    Then an error should occur

  Scenario: Decode malformed JSONL data
    Given a simple copybook with a single field
    And ASCII codepage
    And JSON data:
      """
      {invalid json}
      """
    When the JSON data is encoded
    Then an error should occur

  Scenario: Parse copybook with edited PIC
    Given a copybook with content:
      """
      01 EDITED-TEST.
          05 EDITED-FIELD PIC ZZ9.99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "EDITED-FIELD" should have type "edited"

  Scenario: Parse copybook with nested ODO
    Given a copybook with content:
      """
      01 NESTED-ODO.
          05 OUTER-COUNT PIC 9(3).
          05 OUTER-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON OUTER-COUNT.
              10 INNER-COUNT PIC 9(3).
              10 INNER-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON INNER-COUNT.
                  15 ELEMENT PIC X(5).
      """
    When the copybook is parsed
    Then an error should occur
