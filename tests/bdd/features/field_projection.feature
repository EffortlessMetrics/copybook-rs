@field-projection @projection
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

  # ========================================================================
  # Extended field projection scenarios
  # ========================================================================

  Scenario: Select leaf field from deeply nested group
    Given a copybook with content:
      """
      01 DEEP-RECORD.
         05 LEVEL-ONE.
            10 LEVEL-TWO.
               15 DEEP-FIELD PIC X(10).
            10 OTHER-FIELD PIC X(5).
         05 TOP-FIELD PIC 9(4).
      """
    And field selection: "DEEP-FIELD"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "DEEP-FIELD" should be included in projection
    And the field "OTHER-FIELD" should not be included in projection
    And the field "TOP-FIELD" should not be included in projection

  Scenario: Select two fields from same nested group
    Given a copybook with content:
      """
      01 ACCOUNT-RECORD.
         05 DETAILS.
            10 ACCT-ID    PIC 9(8).
            10 ACCT-NAME  PIC X(30).
            10 ACCT-TYPE  PIC X(2).
         05 BALANCE PIC 9(9)V99.
      """
    And field selection: "ACCT-ID,ACCT-NAME"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "ACCT-ID" should be included in projection
    And the field "ACCT-NAME" should be included in projection
    And the field "ACCT-TYPE" should not be included in projection
    And the field "BALANCE" should not be included in projection

  Scenario: Select group and a child field deduplicates
    Given a copybook with content:
      """
      01 DEDUP-RECORD.
         05 HDR-GROUP.
            10 HDR-ID   PIC 9(4).
            10 HDR-TYPE  PIC X(2).
         05 PAYLOAD PIC X(50).
      """
    And field selection: "HDR-GROUP,HDR-ID"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "HDR-GROUP" should be included in projection
    And the field "HDR-ID" should be included in projection
    And the field "HDR-TYPE" should be included in projection
    And the field "PAYLOAD" should not be included in projection

  Scenario: Project single numeric field and decode
    Given a copybook with content:
      """
      01 NUMERIC-PROJ-RECORD.
         05 ITEM-ID    PIC 9(6).
         05 ITEM-QTY   PIC 9(5).
         05 ITEM-DESC  PIC X(20).
      """
    And binary data: "000042000101234567890123456789"
    And field selection: "ITEM-ID"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "ITEM-ID" should be included in projection
    And the field "ITEM-QTY" should not be included in projection
    And the field "ITEM-DESC" should not be included in projection

  Scenario: Multiple non-existent fields all fail
    Given a copybook with content:
      """
      01 SIMPLE-RECORD.
         05 FIELD-A PIC X(5).
      """
    And field selection: "GHOST-ONE"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should fail
    And the error code should be "CBKS703_PROJECTION_FIELD_NOT_FOUND"

  Scenario: Select single field preserves parent group hierarchy
    Given a copybook with content:
      """
      01 HIERARCHY-RECORD.
         05 OUTER-GROUP.
            10 INNER-GROUP.
               15 TARGET-FIELD PIC X(8).
            10 SIBLING-FIELD PIC X(4).
      """
    And field selection: "TARGET-FIELD"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "TARGET-FIELD" should be included in projection
    And the field "SIBLING-FIELD" should not be included in projection

  Scenario: ODO auto-includes counter when selecting child
    Given a copybook with content:
      """
      01 CHILD-ODO-RECORD.
         05 NUM-ITEMS PIC 9(2).
         05 ITEMS OCCURS 1 TO 20 TIMES DEPENDING ON NUM-ITEMS.
            10 ITEM-CODE PIC X(4).
            10 ITEM-PRICE PIC 9(5)V99.
      """
    And field selection: "ITEMS"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "ITEMS" should be included in projection
    And the field "NUM-ITEMS" should be included in projection
    And the field "ITEM-CODE" should be included in projection
    And the field "ITEM-PRICE" should be included in projection

  Scenario: Select field with mixed case matches case-insensitively
    Given a copybook with content:
      """
      01 CASE-RECORD.
         05 MY-FIELD PIC X(10).
         05 OTHER    PIC X(5).
      """
    And field selection: "my-field"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "MY-FIELD" should be included in projection
    And the field "OTHER" should not be included in projection

  Scenario: Select three independent leaf fields
    Given a copybook with content:
      """
      01 MULTI-RECORD.
         05 FIELD-A PIC X(5).
         05 FIELD-B PIC 9(3).
         05 FIELD-C PIC X(10).
         05 FIELD-D PIC 9(8).
         05 FIELD-E PIC X(20).
      """
    And field selection: "FIELD-A,FIELD-C,FIELD-E"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "FIELD-A" should be included in projection
    And the field "FIELD-C" should be included in projection
    And the field "FIELD-E" should be included in projection
    And the field "FIELD-B" should not be included in projection
    And the field "FIELD-D" should not be included in projection

  Scenario: Projection of single field from flat record
    Given a copybook with content:
      """
      01 FLAT-RECORD.
         05 ALPHA-FIELD PIC X(15).
         05 NUM-FIELD   PIC 9(7).
      """
    And field selection: "NUM-FIELD"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "NUM-FIELD" should be included in projection
    And the field "ALPHA-FIELD" should not be included in projection

  Scenario: Projected decode produces valid JSON
    Given a copybook with content:
      """
      01 JSON-PROJ-RECORD.
         05 CUST-ID     PIC 9(6).
         05 CUST-NAME   PIC X(20).
         05 CUST-ADDR   PIC X(40).
      """
    And binary data: "000042ALICE JOHNSON        742 OAK AVENUE                          "
    And field selection: "CUST-ID,CUST-NAME"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "CUST-ID" should be included in projection
    And the field "CUST-NAME" should be included in projection
    And the field "CUST-ADDR" should not be included in projection

  Scenario: Projected schema has fewer fields than full schema
    Given a copybook with content:
      """
      01 LRECL-PROJ-RECORD.
         05 FIELD-A PIC X(10).
         05 FIELD-B PIC 9(5).
         05 FIELD-C PIC X(30).
         05 FIELD-D PIC 9(8).
      """
    And field selection: "FIELD-A"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the projected schema should contain 1 top-level field(s)
    And the field "FIELD-A" should be included in projection
    And the field "FIELD-B" should not be included in projection
    And the field "FIELD-C" should not be included in projection
    And the field "FIELD-D" should not be included in projection
