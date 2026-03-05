@enterprise @insurance
Feature: Enterprise Insurance Record Layouts

  As a developer processing mainframe insurance data
  I want to decode and encode insurance policy and claim records
  So that complex insurance structures including arrays and REDEFINES are handled

  Background:
    Given ASCII codepage

  # --- Insurance claim records with OCCURS arrays ---

  Scenario: Parse insurance claim record with OCCURS for line items
    Given a copybook with content:
      """
      01 CLAIM-REC.
          05 CLAIM-ID PIC X(12).
          05 CLAIM-DATE PIC X(8).
          05 LINE-ITEMS OCCURS 5 TIMES.
              10 LINE-DESC PIC X(20).
              10 LINE-AMT PIC 9(7)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field LINE-ITEMS should have OCCURS count 5

  Scenario: Decode insurance claim with line items array
    Given a copybook with content:
      """
      01 CLAIM-REC.
          05 CLAIM-ID PIC X(10).
          05 LINE-ITEMS OCCURS 3 TIMES.
              10 LINE-DESC PIC X(10).
              10 LINE-AMT PIC 9(5)V99.
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Roundtrip insurance claim with fixed OCCURS
    Given a copybook with content:
      """
      01 CLAIM-REC.
          05 CLAIM-ID PIC X(8).
          05 ITEMS OCCURS 2 TIMES.
              10 ITEM-CODE PIC X(5).
      """
    And binary data: "CLM00001ITEM1ITEM2"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: Parse insurance claim with large OCCURS array
    Given a copybook with large OCCURS:
      """
      01 LARGE-CLAIM.
          05 CLAIM-ID PIC X(10).
          05 DIAGNOSIS OCCURS 20 TIMES.
              10 DIAG-CODE PIC X(7).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field DIAGNOSIS should have OCCURS count 20

  # --- Policy records with REDEFINES ---

  Scenario: Parse policy record with REDEFINES for policy types
    Given a copybook with content:
      """
      01 POLICY-REC.
          05 POLICY-TYPE PIC X(2).
          05 POLICY-DATA PIC X(30).
          05 POLICY-ALT REDEFINES POLICY-DATA.
              10 ALT-FIELD1 PIC X(15).
              10 ALT-FIELD2 PIC X(15).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "POLICY-ALT" should redefine "POLICY-DATA"

  Scenario: Decode policy record with REDEFINES
    Given a copybook with content:
      """
      01 POLICY-REC.
          05 POLICY-TYPE PIC X(2).
          05 POLICY-DATA PIC X(20).
          05 POLICY-ALT REDEFINES POLICY-DATA.
              10 PART-A PIC X(10).
              10 PART-B PIC X(10).
      """
    And binary data: "LFLife Insurance Data"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Roundtrip policy record preserves REDEFINES
    Given a copybook with content:
      """
      01 POLICY-REC.
          05 POLICY-TYPE PIC X(2).
          05 POLICY-DATA PIC X(20).
          05 POLICY-ALT REDEFINES POLICY-DATA.
              10 PART-A PIC X(10).
              10 PART-B PIC X(10).
      """
    And binary data: "AUAutoInsurancePolicy1"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Large record layouts ---

  Scenario: Parse large insurance record layout
    Given a copybook with content:
      """
      01 LARGE-INS-REC.
          05 POLICY-ID PIC X(15).
          05 HOLDER-NAME PIC X(40).
          05 HOLDER-ADDR PIC X(60).
          05 COVERAGE-AMT PIC 9(9)V99.
          05 PREMIUM PIC 9(7)V99.
          05 EFFECTIVE-DT PIC X(8).
          05 EXPIRY-DT PIC X(8).
          05 AGENT-CODE PIC X(6).
          05 STATUS PIC X(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "HOLDER-NAME" should be 40 bytes long
    And the field "HOLDER-ADDR" should be 60 bytes long

  Scenario: Decode large insurance record
    Given a copybook with content:
      """
      01 LARGE-REC.
          05 POLICY-ID PIC X(10).
          05 HOLDER PIC X(30).
          05 TYPE-CODE PIC X(2).
          05 STATUS PIC X(1).
      """
    And binary data: "POL0001234Jane Doe Insurance Holder   LFA"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Insurance with nested groups ---

  Scenario: Parse insurance record with nested beneficiary group
    Given a copybook with nested groups:
      """
      01 BENEFICIARY-REC.
          05 POLICY-ID PIC X(10).
          05 BENEFICIARY-INFO.
              10 BEN-NAME PIC X(30).
              10 BEN-RELATION PIC X(10).
              10 BEN-PCT PIC 9(3).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BENEFICIARY-INFO" should have type "group"

  Scenario: Verify insurance record data
    Given a copybook with content:
      """
      01 INS-VERIFY.
          05 POL-ID PIC X(10).
          05 POL-STATUS PIC X(1).
          05 POL-TYPE PIC X(2).
      """
    And binary data: "POL0000001ALF"
    When the data is verified
    Then verification should succeed

  # --- Insurance with Level-88 conditions ---

  Scenario: Parse insurance policy type with Level-88
    Given a copybook with Level-88:
      """
      01 POL-TYPE-REC.
          05 POLICY-TYPE PIC X(2).
              88 LIFE-POLICY VALUE 'LF'.
              88 AUTO-POLICY VALUE 'AU'.
              88 HOME-POLICY VALUE 'HM'.
              88 HEALTH-POLICY VALUE 'HE'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "POLICY-TYPE" should have Level-88 "LIFE-POLICY"
    And the field "POLICY-TYPE" should have Level-88 "AUTO-POLICY"

  Scenario: Parse insurance claim status with Level-88
    Given a copybook with Level-88:
      """
      01 CLAIM-STATUS-REC.
          05 CLAIM-STATUS PIC X(1).
              88 SUBMITTED VALUE 'S'.
              88 APPROVED VALUE 'A'.
              88 DENIED VALUE 'D'.
              88 PENDING-REVIEW VALUE 'P'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CLAIM-STATUS" should have Level-88 "SUBMITTED"
    And the field "CLAIM-STATUS" should have Level-88 "DENIED"

  Scenario: Decode insurance record with diverse field types
    Given a copybook with diverse field types:
      """
      01 INS-DIVERSE.
          05 POL-ID PIC X(10).
          05 PREMIUM PIC 9(7)V99.
          05 DEDUCTIBLE PIC 9(5)V99.
          05 STATUS PIC X(1).
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And field types should be diverse
