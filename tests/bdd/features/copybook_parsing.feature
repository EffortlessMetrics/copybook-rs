@copybook-parsing
Feature: Copybook Parsing

  As a developer working with COBOL copybooks
  I want to parse copybook definitions into a structured schema
  So that I can understand and validate the data structure

  Scenario: Parse a simple copybook with a single field
    Given a simple copybook with a single field
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the field "TEST-RECORD" should have type "group"
    And the schema should have fingerprint

  Scenario: Parse a copybook with numeric fields
    Given a copybook with numeric fields
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the field "PACKED-DECIMAL" should have type "packed"
    And the field "BINARY-INTEGER" should have type "binary"
    And the field "ZONED-DECIMAL" should have type "zoned"

  Scenario: Parse a copybook with OCCURS clause
    Given a copybook with OCCURS clause
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the field "ARRAY-FIELD" should have type "occurs"

  Scenario: Parse a copybook with ODO (OCCURS DEPENDING ON)
    Given a copybook with ODO (OCCURS DEPENDING ON)
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the field "COUNT-FIELD" should have type "numeric"
    And the field "DYNAMIC-ARRAY" should have type "occurs"

  Scenario: Parse a copybook with REDEFINES clause
    Given a copybook with REDEFINES clause
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the field "ORIGINAL-FIELD" should have type "alphanumeric"
    And the field "ALTERNATIVE-FIELD" should have type "group"

  Scenario: Parse a copybook with Level-88 condition values
    Given a copybook with Level-88 condition values
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the field "STATUS-ACTIVE" should have type "condition"
    And the field "STATUS-INACTIVE" should have type "condition"
    And the field "STATUS-PENDING" should have type "condition"

  Scenario: Parse copybook in strict mode
    Given strict parsing mode
    And a copybook with content:
      """
      01 STRICT-RECORD.
          05 FIELD-1 PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed

  Scenario: Parse copybook in tolerant mode
    Given tolerant parsing mode
    And a copybook with content:
      """
      01 TOLERANT-RECORD.
          05 FIELD-1 PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed

  Scenario: Parse copybook with inline comments
    Given a copybook with content:
      """
      01 COMMENTED-RECORD. *> This is an inline comment
          05 FIELD-1 PIC X(10). *> Another inline comment
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)

  Scenario: Verify field offsets are calculated correctly
    Given a copybook with content:
      """
      01 OFFSET-RECORD.
          05 FIELD-1 PIC X(5).
          05 FIELD-2 PIC 9(5).
          05 FIELD-3 PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FIELD-1" should have offset 0
    And the field "FIELD-1" should have length 5
    And the field "FIELD-2" should have offset 5
    And the field "FIELD-2" should have length 5
    And the field "FIELD-3" should have offset 10
    And the field "FIELD-3" should have length 10

  Scenario: Parse copybook with nested group structures
    Given a copybook with content:
      """
      01 NESTED-RECORD.
          05 GROUP-1.
              10 SUB-FIELD-1 PIC X(5).
              10 SUB-FIELD-2 PIC 9(5).
          05 GROUP-2.
              10 SUB-FIELD-3 PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the field "GROUP-1" should have type "group"
    And the field "GROUP-2" should have type "group"

  Scenario: Parse level-88 VALUE list commas without treating them as edited picture tokens
    Given a copybook with an inline VALUE-list for Level-88
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the lexer should produce 2 comma token(s)
    And the lexer should not treat VALUE-list separators as edited-picture tokens
