@copybook-parsing
Feature: Group Structure Parsing and Decoding

  As a developer working with COBOL copybooks
  I want nested group structures to be correctly parsed and decoded
  So that hierarchical record layouts are properly represented

  # --- Parsing group structures ---

  Scenario: Parse single-level group with two children
    Given a copybook with content:
      """
      01 REC.
          05 ADDRESS.
              10 STREET PIC X(30).
              10 CITY PIC X(20).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ADDRESS" should have type "group"
    And the field "STREET" should have type "alphanumeric"
    And the field "CITY" should have type "alphanumeric"

  Scenario: Parse two groups at same level
    Given a copybook with content:
      """
      01 REC.
          05 NAME-INFO.
              10 FIRST-NAME PIC X(15).
              10 LAST-NAME PIC X(20).
          05 CONTACT-INFO.
              10 PHONE PIC X(10).
              10 EMAIL PIC X(30).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "NAME-INFO" should have type "group"
    And the field "CONTACT-INFO" should have type "group"
    And the field "FIRST-NAME" should have type "alphanumeric"
    And the field "PHONE" should have type "alphanumeric"

  Scenario: Parse deeply nested groups (3 levels)
    Given a copybook with content:
      """
      01 REC.
          05 OUTER.
              10 MIDDLE.
                  15 INNER-FIELD PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "OUTER" should have type "group"
    And the field "MIDDLE" should have type "group"
    And the field "INNER-FIELD" should have type "alphanumeric"

  Scenario: Parse group with mixed field types
    Given a copybook with content:
      """
      01 EMPLOYEE.
          05 EMP-INFO.
              10 EMP-NAME PIC X(25).
              10 EMP-ID PIC 9(6).
              10 EMP-SALARY PIC 9(7)V99.
              10 EMP-DEPT PIC X(4).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "EMP-INFO" should have type "group"
    And the field "EMP-NAME" should have type "alphanumeric"
    And the field "EMP-ID" should have type "zoned"
    And the field "EMP-SALARY" should have type "zoned"
    And the field "EMP-DEPT" should have type "alphanumeric"

  Scenario: Parse group field offsets are calculated correctly
    Given a copybook with content:
      """
      01 REC.
          05 HEADER PIC X(4).
          05 BODY.
              10 ITEM-A PIC X(6).
              10 ITEM-B PIC 9(4).
          05 FOOTER PIC X(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "HEADER" should have offset 0
    And the field "HEADER" should have length 4
    And the field "ITEM-A" should have offset 4
    And the field "ITEM-A" should have length 6
    And the field "ITEM-B" should have offset 10
    And the field "ITEM-B" should have length 4
    And the field "FOOTER" should have offset 14
    And the field "FOOTER" should have length 2

  # --- Decode group structures ---

  Scenario: Decode simple group structure
    Given a copybook with content:
      """
      01 REC.
          05 PERSON.
              10 NAME PIC X(10).
              10 AGE PIC 9(3).
      """
    And ASCII codepage
    And binary data: "JOHN DOE  025"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "NAME"
    And the decoded output should contain "AGE"

  Scenario: Decode record with multiple groups
    Given a copybook with content:
      """
      01 REC.
          05 HDR.
              10 REC-TYPE PIC X(2).
              10 REC-ID PIC 9(4).
          05 DTL.
              10 VALUE-A PIC X(5).
              10 VALUE-B PIC 9(3).
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "REC-TYPE"
    And the decoded output should contain "VALUE-A"

  # --- Round-trip ---

  Scenario: Group structure round-trip
    Given a copybook with content:
      """
      01 REC.
          05 INFO.
              10 CODE PIC X(3).
              10 NUM PIC 9(5).
          05 EXTRA PIC X(4).
      """
    And ASCII codepage
    And binary data: "ABC1234500XY"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Parse group with all numeric children
    Given a copybook with content:
      """
      01 REC.
          05 TOTALS.
              10 SUBTOTAL PIC 9(7)V99.
              10 TAX PIC 9(5)V99.
              10 GRAND-TOTAL PIC 9(8)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TOTALS" should have type "group"
    And the field "SUBTOTAL" should have type "zoned"
    And the field "TAX" should have type "zoned"
    And the field "GRAND-TOTAL" should have type "zoned"

  Scenario: Parse record with group and elementary siblings
    Given a copybook with content:
      """
      01 REC.
          05 RECORD-TYPE PIC X(2).
          05 DETAILS.
              10 DETAIL-A PIC X(10).
              10 DETAIL-B PIC X(10).
          05 RECORD-SEQ PIC 9(6).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RECORD-TYPE" should have type "alphanumeric"
    And the field "DETAILS" should have type "group"
    And the field "RECORD-SEQ" should have type "zoned"
    And the field "RECORD-TYPE" should have offset 0
    And the field "DETAIL-A" should have offset 2
    And the field "RECORD-SEQ" should have offset 22
