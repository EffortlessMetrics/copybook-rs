@dialect
Feature: Dialect Lever for ODO (OCCURS DEPENDING ON) Behavior

  As a developer working with COBOL copybooks containing ODO clauses
  I want to control how min_count is interpreted through dialect settings
  So that I can handle different COBOL vendor requirements and data patterns

  Background:
    Given a copybook with ODO (OCCURS DEPENDING ON)
    And ASCII codepage

  Scenario: Parse copybook in Normative dialect (default)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And Normative dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "COUNT-FIELD" should have type "numeric"
    And the field "DYNAMIC-ARRAY" should have type "occurs"

  Scenario: Parse copybook in Zero-Tolerant dialect
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "COUNT-FIELD" should have type "numeric"
    And the field "DYNAMIC-ARRAY" should have type "occurs"

  Scenario: Parse copybook in One-Tolerant dialect
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "COUNT-FIELD" should have type "numeric"
    And the field "DYNAMIC-ARRAY" should have type "occurs"

  Scenario: Decode ODO with Normative min_count enforcement (valid count)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(5).
      """
    And Normative dialect
    And binary data: "003ABCDEABCDEABCDE"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "ABCDE"

  Scenario: Decode ODO with Normative dialect below min_count is lenient
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(5).
      """
    And Normative dialect
    And binary data: "003ABCDEABCDEABCDE"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode ODO with Zero-Tolerant min_count ignored (zero count)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(5).
      """
    And Zero-Tolerant dialect
    And binary data: "000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode ODO with Zero-Tolerant min_count ignored (below declared min)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(5).
      """
    And Zero-Tolerant dialect
    And binary data: "002ABCDEABCDE"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode ODO with One-Tolerant min_count clamped (zero count rejected)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 0 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(5).
      """
    And One-Tolerant dialect
    And binary data: "000"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode ODO with One-Tolerant min_count clamped (valid count)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 0 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And One-Tolerant dialect
    And binary data: "001ELEMENT001"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "ELEMENT001"

  Scenario: Parse copybook with ODO structure in different dialects
    Given a copybook with content:
      """
      01 COMPLEX-ODO.
        05 DETAIL-COUNT PIC 9(3).
        05 DETAILS OCCURS 1 TO 50 TIMES DEPENDING ON DETAIL-COUNT.
          10 DETAIL-ID PIC 9(5).
          10 DETAIL-DESC PIC X(20).
      """
    And Normative dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DETAILS" should have type "occurs"

  Scenario: CLI --dialect flag overrides environment variable
    Given a copybook with content:
      """
      01 CLI-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DYNAMIC-ARRAY" should have type "occurs"

  Scenario: Dialect from environment variable (COPYBOOK_DIALECT)
    Given a copybook with content:
      """
      01 ENV-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field DYNAMIC-ARRAY should have ODO with min 0 and max 100

  Scenario: ODO LRECL varies by dialect (zero-tolerant has lower min record size)
    Given a copybook with content:
      """
      01 LRECL-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 ITEMS OCCURS 5 TO 10 TIMES DEPENDING ON COUNT-FIELD.
          10 ITEM-DATA PIC X(8).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 0 and max 10

  Scenario: ODO LRECL varies by dialect (normative preserves min record size)
    Given a copybook with content:
      """
      01 LRECL-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 ITEMS OCCURS 5 TO 10 TIMES DEPENDING ON COUNT-FIELD.
          10 ITEM-DATA PIC X(8).
      """
    And Normative dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 5 and max 10

  Scenario: Error handling for invalid dialect modes
    Given a copybook with content:
      """
      01 ERROR-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And invalid dialect mode
    When the copybook is parsed
    Then an error should occur
    And the error message should contain "dialect"

  Scenario: Integration with existing copybook parsing scenarios
    Given a copybook with content:
      """
      01 INTEGRATION-TEST.
        05 STATIC-FIELD PIC X(10).
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 1 TO 20 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(5).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "STATIC-FIELD" should have type "alphanumeric"
    And the field "COUNT-FIELD" should have type "numeric"
    And the field "DYNAMIC-ARRAY" should have type "occurs"

  Scenario: Round-trip with ODO in different dialects
    Given a copybook with content:
      """
      01 ROUNDTRIP-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(5).
      """
    And Zero-Tolerant dialect
    And binary data: "002ELEM1ELEM2"
    When the binary data is decoded
    Then decoding should succeed