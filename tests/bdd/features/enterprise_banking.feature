@enterprise @banking
Feature: Enterprise Banking Record Layouts

  As a developer processing mainframe banking data
  I want to decode and encode banking record structures
  So that account numbers, balances, and transactions are handled correctly

  Background:
    Given ASCII codepage

  # --- Account number fields ---

  Scenario: Parse banking account record with alphanumeric account number
    Given a copybook with content:
      """
      01 ACCOUNT-REC.
          05 ACCT-NUMBER PIC X(12).
          05 ACCT-NAME PIC X(30).
          05 ACCT-TYPE PIC X(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ACCT-NUMBER" should have type "alphanumeric"
    And the field "ACCT-NUMBER" should be 12 bytes long

  Scenario: Decode banking account number
    Given a copybook with content:
      """
      01 ACCOUNT-REC.
          05 ACCT-NUMBER PIC X(12).
          05 ACCT-NAME PIC X(30).
      """
    And binary data: "123456789012John Smith                    "
    When the binary data is decoded
    Then decoding should succeed
    And decoded field ACCT-NUMBER should be "123456789012"
    And the decoded output should be valid JSON

  Scenario: Roundtrip banking account record
    Given a copybook with content:
      """
      01 ACCOUNT-REC.
          05 ACCT-NUMBER PIC X(12).
          05 ACCT-NAME PIC X(20).
      """
    And binary data: "123456789012Jane Doe            "
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  # --- COMP-3 monetary amounts ---

  Scenario: Parse COMP-3 balance field with implied decimal
    Given a copybook with COMP-3 field:
      """
      01 BALANCE-REC.
          05 BALANCE PIC S9(9)V99 COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "BALANCE" should have type "packed_decimal"

  Scenario: Parse COMP-3 interest rate with 4 decimal places
    Given a copybook with COMP-3 field:
      """
      01 INTEREST-REC.
          05 INTEREST-RATE PIC S9(3)V9(4) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "INTEREST-RATE" should have type "packed_decimal"

  Scenario: Decode COMP-3 monetary amount
    Given a copybook with COMP-3 field:
      """
      01 BALANCE-REC.
          05 BALANCE PIC S9(7)V99 COMP-3.
      """
    And binary data for value 12345.67
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode COMP-3 zero balance
    Given a copybook with COMP-3 field:
      """
      01 BALANCE-REC.
          05 BALANCE PIC S9(7)V99 COMP-3.
      """
    And binary data for value 0.0
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Roundtrip COMP-3 monetary amount
    Given a copybook with COMP-3 field:
      """
      01 BALANCE-REC.
          05 BALANCE PIC S9(7)V99 COMP-3.
      """
    And binary data for value 99999.99
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Multi-segment records ---

  Scenario: Parse multi-segment banking record with header and detail
    Given a copybook with content:
      """
      01 BANK-RECORD.
          05 RECORD-TYPE PIC X(1).
          05 ACCT-NUMBER PIC X(10).
          05 ACCT-NAME PIC X(20).
          05 BALANCE PIC 9(9)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RECORD-TYPE" should be 1 bytes long
    And the field "BALANCE" should have type "numeric"

  Scenario: Decode banking header record
    Given a copybook with content:
      """
      01 HEADER-REC.
          05 REC-TYPE PIC X(1).
          05 BRANCH-CODE PIC X(5).
          05 BATCH-DATE PIC X(8).
          05 BATCH-NUMBER PIC 9(6).
      """
    And binary data: "H1234520231231001234"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field REC-TYPE should be "H"
    And decoded field BRANCH-CODE should be "12345"

  Scenario: Decode banking detail record
    Given a copybook with content:
      """
      01 DETAIL-REC.
          05 REC-TYPE PIC X(1).
          05 ACCT-NUM PIC X(10).
          05 TXN-AMOUNT PIC 9(7)V99.
          05 TXN-DESC PIC X(20).
      """
    And binary data: "D1234567890001234567DEPOSIT PAYMENT     "
    When the binary data is decoded
    Then decoding should succeed
    And decoded field REC-TYPE should be "D"
    And the decoded output should be valid JSON

  Scenario: Decode banking trailer record
    Given a copybook with content:
      """
      01 TRAILER-REC.
          05 REC-TYPE PIC X(1).
          05 RECORD-COUNT PIC 9(7).
          05 TOTAL-AMOUNT PIC 9(11)V99.
      """
    And binary data: "T00001230000012345678"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field REC-TYPE should be "T"

  # --- Banking with nested groups ---

  Scenario: Parse banking record with nested address group
    Given a copybook with nested groups:
      """
      01 CUSTOMER-REC.
          05 CUST-ID PIC X(10).
          05 CUST-ADDRESS.
              10 STREET PIC X(30).
              10 CITY PIC X(20).
              10 STATE PIC X(2).
              10 ZIP PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CUST-ADDRESS" should have type "group"
    And the field "STREET" should be 30 bytes long

  Scenario: Decode banking customer with nested address
    Given a copybook with nested groups:
      """
      01 CUSTOMER-REC.
          05 CUST-ID PIC X(8).
          05 CUST-ADDRESS.
              10 STREET PIC X(20).
              10 CITY PIC X(15).
              10 STATE PIC X(2).
              10 ZIP PIC X(5).
      """
    And binary data: "CUST0001123 Main Street     New York City  NY10001"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Banking with diverse field types ---

  Scenario: Parse banking record with mixed numeric types
    Given a copybook with diverse field types:
      """
      01 MIXED-BANK-REC.
          05 ACCT-ID PIC X(10).
          05 BALANCE PIC S9(9)V99 COMP-3.
          05 TXN-COUNT PIC S9(5) COMP.
          05 RATE PIC 9(3)V9(4).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field types should be diverse

  Scenario: Decode banking record with all fields populated
    Given a copybook with diverse field types:
      """
      01 BANK-DETAIL.
          05 ACCT-ID PIC X(8).
          05 ACCT-NAME PIC X(20).
          05 STATUS PIC X(1).
      """
    And binary data: "12345678John Smith          A"
    When the binary data is decoded
    Then decoding should succeed
    And all fields should be decoded

  Scenario: Verify banking record data integrity
    Given a copybook with content:
      """
      01 VERIFY-REC.
          05 ACCT-ID PIC X(10).
          05 ACCT-STATUS PIC X(1).
      """
    And binary data: "1234567890A"
    When the data is verified
    Then verification should succeed

  Scenario: Roundtrip banking multi-field record
    Given a copybook with content:
      """
      01 BANK-RT.
          05 BRANCH PIC X(5).
          05 ACCT PIC X(10).
          05 NAME PIC X(20).
      """
    And binary data: "001231234567890Customer Name Here  "
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Parse banking record with Level-88 account status
    Given a copybook with Level-88:
      """
      01 STATUS-REC.
          05 ACCT-STATUS PIC X(1).
              88 ACTIVE VALUE 'A'.
              88 CLOSED VALUE 'C'.
              88 FROZEN VALUE 'F'.
              88 DORMANT VALUE 'D'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ACCT-STATUS" should have Level-88 "ACTIVE"
    And the field "ACCT-STATUS" should have Level-88 "CLOSED"
    And the field "ACCT-STATUS" should have Level-88 "FROZEN"
