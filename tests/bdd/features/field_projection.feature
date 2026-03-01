@field-projection
Feature: Field Projection

  As a developer working with large COBOL data structures
  I want to selectively project only the fields I need from a copybook schema
  So that I can improve performance by processing only relevant data

  Background:
    Given ASCII codepage

  Scenario: Decode with simple field selection
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
         05 CUSTOMER-ID      PIC 9(6).
         05 CUSTOMER-NAME    PIC X(30).
         05 CUSTOMER-ADDRESS PIC X(50).
      """
    And binary data: "000123JOHN DOE                           123 MAIN STREET                        "
    And field selection: "CUSTOMER-ID"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "CUSTOMER-ID" should be included in projection
    And the field "CUSTOMER-NAME" should not be included in projection
    And the field "CUSTOMER-ADDRESS" should not be included in projection

  Scenario: Decode with multiple field selection
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
         05 CUSTOMER-ID      PIC 9(6).
         05 CUSTOMER-NAME    PIC X(30).
         05 CUSTOMER-ADDRESS PIC X(50).
         05 CUSTOMER-PHONE   PIC X(15).
      """
    And binary data: "000123JOHN DOE                           123 MAIN STREET                        555-1234567890  "
    And field selection: "CUSTOMER-ID,CUSTOMER-NAME"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "CUSTOMER-ID" should be included in projection
    And the field "CUSTOMER-NAME" should be included in projection
    And the field "CUSTOMER-ADDRESS" should not be included in projection
    And the field "CUSTOMER-PHONE" should not be included in projection

  Scenario: Decode with group selection
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
         05 CUSTOMER-INFO.
            10 CUSTOMER-ID   PIC 9(6).
            10 CUSTOMER-NAME PIC X(30).
         05 ORDER-INFO.
            10 ORDER-ID      PIC 9(8).
            10 ORDER-DATE    PIC 9(8).
      """
    And binary data: "000123JOHN DOE                           00000001020240117"
    And field selection: "CUSTOMER-INFO"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "CUSTOMER-INFO" should be included in projection
    And the field "CUSTOMER-ID" should be included in projection
    And the field "CUSTOMER-NAME" should be included in projection
    And the field "ORDER-INFO" should not be included in projection
    And the field "ORDER-ID" should not be included in projection

  Scenario: Decode with ODO auto-counter inclusion
    Given a copybook with content:
      """
      01 ORDER-RECORD.
         05 ORDER-COUNT      PIC 9(3).
         05 ORDERS OCCURS 1 TO 100 TIMES DEPENDING ON ORDER-COUNT.
            10 ORDER-ID      PIC 9(8).
            10 ORDER-AMOUNT  PIC 9(7)V99.
      """
    And binary data: "00200000000100000100000000000002000000020000020000000000000"
    And field selection: "ORDERS"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "ORDERS" should be included in projection
    And the field "ORDER-COUNT" should be included in projection
    And the field "ORDER-ID" should be included in projection

  Scenario: Decode with RENAMES alias resolution
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
         05 FIRST-NAME       PIC X(20).
         05 LAST-NAME        PIC X(30).
         05 MIDDLE-INITIAL   PIC X(1).
         05 ADDRESS          PIC X(50).
         66 FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
      """
    And binary data: "JOHN              DOE               M123 MAIN STREET                        "
    And field selection: "FULL-NAME"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "FIRST-NAME" should be included in projection
    And the field "LAST-NAME" should be included in projection
    And the field "MIDDLE-INITIAL" should not be included in projection
    And the field "ADDRESS" should not be included in projection
    And the field "FULL-NAME" should not be included in projection

  Scenario: Decode with RENAMES alias and regular fields
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
         05 FIRST-NAME       PIC X(20).
         05 LAST-NAME        PIC X(30).
         05 CUSTOMER-ID      PIC 9(6).
         66 FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
      """
    And binary data: "JOHN              DOE               000123"
    And field selection: "FULL-NAME,CUSTOMER-ID"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "FIRST-NAME" should be included in projection
    And the field "LAST-NAME" should be included in projection
    And the field "CUSTOMER-ID" should be included in projection

  Scenario: Error when field not found in projection
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
         05 CUSTOMER-ID      PIC 9(6).
         05 CUSTOMER-NAME    PIC X(30).
      """
    And field selection: "NONEXISTENT-FIELD"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should fail
    And the error message should contain "NONEXISTENT-FIELD"
    And the error code should be "CBKS703_PROJECTION_FIELD_NOT_FOUND"

  Scenario: Encode with field projection
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
         05 CUSTOMER-ID      PIC 9(6).
         05 CUSTOMER-NAME    PIC X(30).
         05 CUSTOMER-ADDRESS PIC X(50).
      """
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"CUSTOMER-ID":"000123","CUSTOMER-NAME":"JOHN DOE"},"CUSTOMER-ID":"000123","CUSTOMER-NAME":"JOHN DOE"}
      """
    And field selection: "CUSTOMER-ID,CUSTOMER-NAME"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "CUSTOMER-ID" should be included in projection
    And the field "CUSTOMER-NAME" should be included in projection
    And the field "CUSTOMER-ADDRESS" should not be included in projection

  Scenario: Verify with field projection
    Given a copybook with content:
      """
      01 TRANSACTION-RECORD.
         05 TRANSACTION-ID   PIC 9(10).
         05 TRANSACTION-DATE  PIC 9(8).
         05 TRANSACTION-AMT  PIC 9(9)V99.
         05 TRANSACTION-DESC PIC X(100).
      """
    And field selection: "TRANSACTION-ID,TRANSACTION-AMT"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "TRANSACTION-ID" should be included in projection
    And the field "TRANSACTION-AMT" should be included in projection
    And the field "TRANSACTION-DATE" should not be included in projection
    And the field "TRANSACTION-DESC" should not be included in projection

  Scenario: Empty field selection
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
         05 CUSTOMER-ID      PIC 9(6).
         05 CUSTOMER-NAME    PIC X(30).
      """
    And field selection: ""
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 0 top-level field(s)
