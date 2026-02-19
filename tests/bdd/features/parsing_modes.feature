@parsing-modes
Feature: Parsing Modes
  Test strict vs tolerant parsing and comment handling

  Scenario: Strict parsing of valid copybook
    Given strict parsing mode
    And a copybook with content:
      """
      01 STRICT-RECORD.
         05 FIELD-A PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Tolerant parsing of valid copybook
    Given tolerant parsing mode
    And a copybook with content:
      """
      01 TOLERANT-RECORD.
         05 FIELD-A PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Default parsing mode succeeds on valid input
    Given a copybook with content:
      """
      01 DEFAULT-RECORD.
         05 FIELD-A PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Strict comments mode
    Given strict_comments mode
    And a copybook with content:
      """
      01 COMMENTS-RECORD.
         05 FIELD-A PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Inline comments disabled
    Given inline comments disabled
    And a copybook with content:
      """
      01 NO-INLINE-RECORD.
         05 FIELD-A PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Strict parsing rejects invalid syntax
    Given strict parsing mode
    And a copybook with content:
      """
      THIS IS NOT VALID COBOL
      """
    When the copybook is parsed
    Then parsing should fail

  Scenario: Tolerant parsing with OCCURS
    Given tolerant parsing mode
    And a copybook with content:
      """
      01 ARRAY-RECORD.
         05 ITEMS OCCURS 5 TIMES.
            10 ITEM-DATA PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Strict parsing with Level-88
    Given strict parsing mode
    And a copybook with content:
      """
      01 L88-RECORD.
         05 STATUS-CODE PIC X(1).
            88 ACTIVE VALUE 'A'.
            88 INACTIVE VALUE 'I'.
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Strict parsing with REDEFINES
    Given strict parsing mode
    And a copybook with content:
      """
      01 REDEF-RECORD.
         05 FIELD-A PIC X(10).
         05 FIELD-B REDEFINES FIELD-A PIC 9(10).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Default mode handles complex copybook
    Given a copybook with content:
      """
      01 COMPLEX-RECORD.
         05 HEADER.
            10 RECORD-TYPE PIC X(2).
            10 RECORD-LEN PIC 9(4).
         05 BODY.
            10 FIELD-A PIC X(20).
            10 FIELD-B PIC S9(7)V99.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the schema should contain 1 top-level field(s)
