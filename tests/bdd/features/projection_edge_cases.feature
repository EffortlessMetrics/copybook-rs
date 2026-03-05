@projection-edge-cases
Feature: Field Projection Edge Cases
  As a developer using field projection
  I want projection to work correctly with various copybook structures
  So that I can select exactly the fields I need

  Scenario: Project single alphanumeric field
    Given a copybook with content:
      """
      01 PROJ-REC.
          05 FIELD-A PIC X(10).
          05 FIELD-B PIC X(10).
          05 FIELD-C PIC X(10).
      """
    And field selection: "FIELD-B"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed
    And the field "FIELD-B" should be included in projection
    And the field "FIELD-A" should not be included in projection
    And the field "FIELD-C" should not be included in projection

  Scenario: Project multiple fields
    Given a copybook with content:
      """
      01 MULTI-PROJ.
          05 ID PIC 9(6).
          05 NAME PIC X(20).
          05 ADDR PIC X(30).
          05 PHONE PIC X(10).
      """
    And field selection: "ID,NAME"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed
    And the field "ID" should be included in projection
    And the field "NAME" should be included in projection
    And the field "ADDR" should not be included in projection

  Scenario: Project all fields
    Given a copybook with content:
      """
      01 ALL-PROJ.
          05 F1 PIC X(5).
          05 F2 PIC X(5).
      """
    And field selection: "F1,F2"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed
    And the field "F1" should be included in projection
    And the field "F2" should be included in projection

  Scenario: Project nonexistent field fails
    Given a copybook with content:
      """
      01 BAD-PROJ.
          05 REAL-FIELD PIC X(10).
      """
    And field selection: "FAKE-FIELD"
    When the copybook is parsed
    And field projection is applied
    Then the projection should fail

  Scenario: Project numeric field
    Given a copybook with content:
      """
      01 NUM-PROJ.
          05 AMOUNT PIC 9(7)V99.
          05 DESC PIC X(20).
      """
    And field selection: "AMOUNT"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed
    And the field "AMOUNT" should be included in projection
    And the field "DESC" should not be included in projection

  Scenario: Project field from nested group
    Given a copybook with content:
      """
      01 NESTED-PROJ.
          05 GRP-A.
              10 INNER-1 PIC X(5).
              10 INNER-2 PIC X(5).
          05 GRP-B.
              10 INNER-3 PIC X(5).
      """
    And field selection: "INNER-1"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed
    And the field "INNER-1" should be included in projection

  Scenario: Project with ODO auto-includes counter
    Given a copybook with content:
      """
      01 ODO-PROJ.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 ITEM-VAL PIC X(5).
      """
    And field selection: "ITEMS"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed
    And the field "ITEMS" should be included in projection
    And the field "CNT" should be included in projection

  Scenario: Project single field decode
    Given a copybook with content:
      """
      01 DEC-PROJ.
          05 KEEP-FIELD PIC X(5).
          05 SKIP-FIELD PIC X(5).
      """
    And field selection: "KEEP-FIELD"
    And ASCII codepage
    And binary data: "HELLOWORLD"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed

  Scenario: Project COMP-3 field
    Given a copybook with content:
      """
      01 COMP3-PROJ.
          05 AMT PIC S9(5)V99 COMP-3.
          05 NAME PIC X(10).
      """
    And field selection: "AMT"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed
    And the field "AMT" should be included in projection
    And the field "NAME" should not be included in projection

  Scenario: Project binary integer field
    Given a copybook with content:
      """
      01 COMP-PROJ.
          05 COUNT-FIELD PIC S9(9) COMP.
          05 LABEL PIC X(15).
      """
    And field selection: "COUNT-FIELD"
    When the copybook is parsed
    And field projection is applied
    Then the projection should succeed
    And the field "COUNT-FIELD" should be included in projection
