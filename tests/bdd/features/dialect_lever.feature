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
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And Normative dialect
    And binary data: "005ELEMENT001ELEMENT002ELEMENT003ELEMENT004ELEMENT005"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "ELEMENT001"
    And the decoded output should contain "ELEMENT005"

  Scenario: Decode ODO with Normative min_count enforcement (invalid count)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And Normative dialect
    And binary data: "003ELEMENT001ELEMENT002ELEMENT003"
    When the binary data is decoded
    Then an error should occur
    And the error message should contain "min_count"

  Scenario: Decode ODO with Zero-Tolerant min_count ignored (zero count)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And Zero-Tolerant dialect
    And binary data: "000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode ODO with Zero-Tolerant min_count ignored (below min_count)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 5 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And Zero-Tolerant dialect
    And binary data: "003ELEMENT001ELEMENT002ELEMENT003"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "ELEMENT001"

  Scenario: Decode ODO with One-Tolerant min_count clamped (zero min_count)
    Given a copybook with content:
      """
      01 DIALECT-TEST.
        05 COUNT-FIELD PIC 9(3).
        05 DYNAMIC-ARRAY OCCURS 0 TO 100 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(10).
      """
    And One-Tolerant dialect
    And binary data: "000"
    When the binary data is decoded
    Then an error should occur
    And the error message should contain "min_count"

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

  Scenario: Parse copybook with complex ODO structure in different dialects
    Given a copybook with content:
      """
      01 COMPLEX-ODO.
        05 HEADER-COUNT PIC 9(3).
        05 DETAIL-COUNT PIC 9(3).
        05 HEADERS OCCURS 1 TO 10 TIMES DEPENDING ON HEADER-COUNT.
          10 HEADER-KEY PIC X(5).
          10 HEADER-VALUE PIC X(15).
        05 DETAILS OCCURS 5 TO 50 TIMES DEPENDING ON DETAIL-COUNT.
          10 DETAIL-ID PIC 9(5).
          10 DETAIL-DESC PIC X(20).
      """
    And Normative dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "HEADERS" should have type "occurs"
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
        05 DYNAMIC-ARRAY OCCURS 2 TO 20 TIMES DEPENDING ON COUNT-FIELD.
          10 ELEMENT PIC X(5).
        05 TRAILER-FIELD PIC X(8).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "STATIC-FIELD" should have type "alphanumeric"
    And the field "COUNT-FIELD" should have type "numeric"
    And the field "DYNAMIC-ARRAY" should have type "occurs"
    And the field "TRAILER-FIELD" should have type "alphanumeric"

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
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed