@enterprise @retail
Feature: Enterprise Retail Record Layouts

  As a developer processing mainframe retail and POS data
  I want to decode and encode retail transaction records
  So that SKU, inventory, and POS data are handled correctly

  Background:
    Given ASCII codepage

  # --- POS transaction records ---

  Scenario: Parse POS transaction record
    Given a copybook with content:
      """
      01 POS-TXN.
          05 TXN-ID PIC X(12).
          05 STORE-ID PIC X(5).
          05 REGISTER PIC X(3).
          05 TXN-DATE PIC X(8).
          05 TXN-TIME PIC X(6).
          05 TOTAL-AMT PIC 9(7)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TXN-ID" should be 12 bytes long
    And the field "TOTAL-AMT" should have type "numeric"

  Scenario: Decode POS transaction record
    Given a copybook with content:
      """
      01 POS-TXN.
          05 TXN-ID PIC X(10).
          05 STORE-ID PIC X(4).
          05 TOTAL PIC 9(5)V99.
      """
    And binary data: "TXN000123400010012345"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field TXN-ID should be "TXN0001234"
    And the decoded output should be valid JSON

  Scenario: Roundtrip POS transaction record
    Given a copybook with content:
      """
      01 POS-TXN.
          05 TXN-ID PIC X(10).
          05 STORE PIC X(5).
          05 AMOUNT PIC 9(5)V99.
      """
    And binary data: "TXN0001234STR010099999"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Decode POS record with zero amount
    Given a copybook with content:
      """
      01 POS-TXN.
          05 TXN-ID PIC X(10).
          05 AMOUNT PIC 9(5)V99.
      """
    And binary data: "VOID0000000000000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- SKU and inventory records ---

  Scenario: Parse SKU inventory record
    Given a copybook with content:
      """
      01 SKU-REC.
          05 SKU-CODE PIC X(13).
          05 SKU-DESC PIC X(40).
          05 QTY-ON-HAND PIC 9(7).
          05 UNIT-PRICE PIC 9(5)V99.
          05 DEPT-CODE PIC X(4).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "SKU-CODE" should be 13 bytes long
    And the field "SKU-DESC" should be 40 bytes long

  Scenario: Decode SKU record with alphanumeric fields
    Given a copybook with content:
      """
      01 SKU-REC.
          05 SKU-CODE PIC X(10).
          05 SKU-DESC PIC X(20).
          05 QTY PIC 9(5).
      """
    And binary data: "SKU1234567Widget Blue Model A 00150"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field SKU-CODE should be "SKU1234567"

  Scenario: Roundtrip SKU record
    Given a copybook with content:
      """
      01 SKU-REC.
          05 SKU-CODE PIC X(10).
          05 QTY PIC 9(5).
      """
    And binary data: "SKU123456700100"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Decode inventory record with nested location group
    Given a copybook with nested groups:
      """
      01 INV-REC.
          05 SKU PIC X(10).
          05 LOCATION.
              10 WAREHOUSE PIC X(4).
              10 AISLE PIC X(3).
              10 BIN PIC X(3).
          05 QTY PIC 9(5).
      """
    And binary data: "SKU0001234WH01A01B0100150"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Parse inventory with OCCURS for multi-location stock
    Given a copybook with content:
      """
      01 MULTI-LOC-INV.
          05 SKU PIC X(10).
          05 LOCATIONS OCCURS 3 TIMES.
              10 WH-CODE PIC X(4).
              10 WH-QTY PIC 9(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field LOCATIONS should have OCCURS count 3

  # --- Mixed numeric types ---

  Scenario: Parse retail record with mixed numeric types
    Given a copybook with diverse field types:
      """
      01 MIXED-RETAIL.
          05 ITEM-ID PIC X(10).
          05 PRICE PIC 9(5)V99.
          05 QTY PIC S9(5) COMP.
          05 WEIGHT PIC S9(3)V999 COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field types should be diverse

  Scenario: Decode retail record with mixed numeric types
    Given a copybook with diverse field types:
      """
      01 MIXED-RETAIL.
          05 ITEM-ID PIC X(8).
          05 PRICE PIC 9(5)V99.
          05 DISC-PCT PIC 9(2)V99.
      """
    And binary data: "ITEM000100199990500"
    When the binary data is decoded
    Then decoding should succeed
    And all fields should be decoded

  Scenario: Verify retail transaction data
    Given a copybook with content:
      """
      01 RETAIL-VERIFY.
          05 TXN-ID PIC X(10).
          05 AMOUNT PIC 9(5)V99.
      """
    And binary data: "TXN00012340012345"
    When the data is verified
    Then verification should succeed

  Scenario: Parse retail record with Level-88 status codes
    Given a copybook with Level-88:
      """
      01 ORDER-STATUS-REC.
          05 ORDER-STATUS PIC X(2).
              88 NEW-ORDER VALUE 'NO'.
              88 PROCESSING VALUE 'PR'.
              88 SHIPPED VALUE 'SH'.
              88 DELIVERED VALUE 'DL'.
              88 RETURNED VALUE 'RT'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ORDER-STATUS" should have Level-88 "NEW-ORDER"
    And the field "ORDER-STATUS" should have Level-88 "SHIPPED"

  Scenario: Decode retail record with various field levels
    Given a copybook with various levels:
      """
      01 RETAIL-DETAIL.
          05 HEADER-INFO.
              10 STORE PIC X(5).
              10 DATE PIC X(8).
          05 ITEM-INFO.
              10 SKU PIC X(10).
              10 DESC PIC X(15).
              10 PRICE PIC 9(5)V99.
      """
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
