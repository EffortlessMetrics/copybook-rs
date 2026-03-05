@projection-advanced
Feature: Advanced Field Projection

  As a developer using field projection
  I want comprehensive coverage of advanced projection scenarios
  So that edge cases like missing fields, ODO auto-inclusion, and RENAMES work correctly

  Background:
    Given ASCII codepage

  # --- Select non-existent field ---

  Scenario: Select non-existent field returns CBKS703
    Given a copybook with content:
      """
      01 CUSTOMER-REC.
         05 CUSTOMER-ID   PIC 9(6).
         05 CUSTOMER-NAME PIC X(30).
      """
    And field selection: "DOES-NOT-EXIST"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should fail
    And the error code should be "CBKS703_PROJECTION_FIELD_NOT_FOUND"
    And the error message should contain "DOES-NOT-EXIST"

  # --- ODO array auto-includes counter ---

  Scenario: Select ODO array auto-includes counter field
    Given a copybook with content:
      """
      01 ORDER-REC.
         05 ORDER-COUNT     PIC 9(3).
         05 ORDER-ITEMS OCCURS 1 TO 50 TIMES DEPENDING ON ORDER-COUNT.
            10 ITEM-ID      PIC 9(8).
            10 ITEM-QTY     PIC 9(5).
      """
    And field selection: "ORDER-ITEMS"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "ORDER-ITEMS" should be included in projection
    And the field "ORDER-COUNT" should be included in projection
    And the field "ITEM-ID" should be included in projection

  # --- RENAMES alias projection ---

  Scenario: Select RENAMES alias resolves to storage fields
    Given a copybook with content:
      """
      01 PERSON-REC.
         05 FIRST-NAME      PIC X(15).
         05 LAST-NAME       PIC X(20).
         05 AGE             PIC 9(3).
         66 FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
      """
    And field selection: "FULL-NAME"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "FIRST-NAME" should be included in projection
    And the field "LAST-NAME" should be included in projection
    And the field "AGE" should not be included in projection

  # --- Multiple fields from nested groups ---

  Scenario: Select multiple fields from nested groups
    Given a copybook with content:
      """
      01 ACCOUNT-REC.
         05 PERSONAL-INFO.
            10 ACCT-NAME    PIC X(20).
            10 ACCT-ID      PIC 9(8).
         05 FINANCIAL-INFO.
            10 BALANCE      PIC 9(9)V99.
            10 CREDIT-LIMIT PIC 9(9)V99.
      """
    And field selection: "ACCT-ID,BALANCE"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "ACCT-ID" should be included in projection
    And the field "BALANCE" should be included in projection
    And the field "ACCT-NAME" should not be included in projection
    And the field "CREDIT-LIMIT" should not be included in projection

  # --- Select all fields (identity projection) ---

  Scenario: Select all leaf fields produces identity projection
    Given a copybook with content:
      """
      01 SIMPLE-REC.
         05 FIELD-A PIC X(5).
         05 FIELD-B PIC 9(3).
         05 FIELD-C PIC X(10).
      """
    And field selection: "FIELD-A,FIELD-B,FIELD-C"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "FIELD-A" should be included in projection
    And the field "FIELD-B" should be included in projection
    And the field "FIELD-C" should be included in projection

  # --- Select group field includes children ---

  Scenario: Select group field includes all children
    Given a copybook with content:
      """
      01 NESTED-REC.
         05 HEADER-GROUP.
            10 HDR-TYPE  PIC X(2).
            10 HDR-LEN   PIC 9(4).
         05 DETAIL-FIELD PIC X(50).
      """
    And field selection: "HEADER-GROUP"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
    And the field "HEADER-GROUP" should be included in projection
    And the field "HDR-TYPE" should be included in projection
    And the field "HDR-LEN" should be included in projection
    And the field "DETAIL-FIELD" should not be included in projection

  # --- Multiple non-existent fields ---

  Scenario: Select mix of valid and non-existent fields fails
    Given a copybook with content:
      """
      01 MIXED-REC.
         05 REAL-FIELD PIC X(10).
         05 OTHER-FIELD PIC 9(5).
      """
    And field selection: "REAL-FIELD,GHOST-FIELD"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should fail
    And the error code should be "CBKS703_PROJECTION_FIELD_NOT_FOUND"
