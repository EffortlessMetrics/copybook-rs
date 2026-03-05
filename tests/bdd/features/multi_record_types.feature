@multi-record
Feature: Multi-Record Type Handling
  As a developer processing mainframe data files
  I want to handle multiple record types and layouts within a single copybook
  So that I can process complex multi-record data files correctly

  # --- Multiple record layouts ---

  Scenario: Parse copybook with two distinct record types
    Given a copybook with content:
      """
      01 HEADER-RECORD.
          05 RECORD-TYPE PIC X(2).
          05 HEADER-NAME PIC X(20).
      01 DETAIL-RECORD.
          05 DETAIL-TYPE PIC X(2).
          05 DETAIL-AMOUNT PIC 9(7)V99.
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Parse copybook with header, detail, and trailer
    Given a copybook with content:
      """
      01 HDR-REC.
          05 H-TYPE PIC X(1).
          05 H-DATE PIC 9(8).
      01 DTL-REC.
          05 D-TYPE PIC X(1).
          05 D-ACCT PIC X(10).
          05 D-AMT PIC S9(7)V99.
      01 TRL-REC.
          05 T-TYPE PIC X(1).
          05 T-COUNT PIC 9(6).
          05 T-TOTAL PIC S9(9)V99.
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Decode header record type
    Given a copybook with content:
      """
      01 SIMPLE-REC.
          05 REC-TYPE PIC X(1).
          05 REC-DATA PIC X(9).
      """
    And ASCII codepage
    And binary data: "HHEADER   "
    When the binary data is decoded
    Then decoding should succeed
    And decoded field REC-TYPE should be "H"

  Scenario: Decode detail record type
    Given a copybook with content:
      """
      01 DETAIL-REC.
          05 REC-TYPE PIC X(1).
          05 REC-NAME PIC X(9).
      """
    And ASCII codepage
    And binary data: "DDETAIL   "
    When the binary data is decoded
    Then decoding should succeed
    And decoded field REC-TYPE should be "D"

  Scenario: Decode trailer record type
    Given a copybook with content:
      """
      01 TRAILER-REC.
          05 REC-TYPE PIC X(1).
          05 REC-COUNT PIC 9(9).
      """
    And ASCII codepage
    And binary data: "T000000042"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field REC-TYPE should be "T"

  Scenario: Parse copybook with REDEFINES for record variants
    Given a copybook with content:
      """
      01 MULTI-REC.
          05 COMMON-PART.
              10 REC-TYPE PIC X(2).
          05 VARIANT-A.
              10 FIELD-A1 PIC X(10).
              10 FIELD-A2 PIC 9(8).
          05 VARIANT-B REDEFINES VARIANT-A.
              10 FIELD-B1 PIC X(18).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "VARIANT-B" should redefine "VARIANT-A"

  Scenario: Parse record with multiple group structures
    Given a copybook with content:
      """
      01 COMPLEX-RECORD.
          05 CUSTOMER-GROUP.
              10 CUST-ID PIC 9(6).
              10 CUST-NAME PIC X(20).
          05 ORDER-GROUP.
              10 ORDER-ID PIC 9(8).
              10 ORDER-AMT PIC 9(7)V99.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "CUSTOMER-GROUP" should have type "group"
    And the field "ORDER-GROUP" should have type "group"

  Scenario: Decode multi-group record with ASCII
    Given a copybook with content:
      """
      01 GROUPED-REC.
          05 GRP-A.
              10 FA PIC X(5).
          05 GRP-B.
              10 FB PIC X(5).
      """
    And ASCII codepage
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "HELLO"
    And the decoded output should contain "WORLD"

  Scenario: Encode multi-group record
    Given a copybook with content:
      """
      01 ENC-GROUPED.
          05 GRP-X.
              10 FX PIC X(4).
          05 GRP-Y.
              10 FY PIC X(4).
      """
    And ASCII codepage
    And JSON data:
      """
      {"GRP-X":{"FX":"AAAA"},"GRP-Y":{"FY":"BBBB"}}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 8 bytes

  Scenario: Round-trip multi-group record
    Given a copybook with content:
      """
      01 RT-GROUPED.
          05 PART-A.
              10 FIELD-1 PIC X(5).
          05 PART-B.
              10 FIELD-2 PIC X(5).
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Parse record with mixed numeric and text groups
    Given a copybook with content:
      """
      01 MIXED-GROUPS.
          05 TEXT-GROUP.
              10 NAME PIC X(15).
              10 ADDR PIC X(25).
          05 NUM-GROUP.
              10 BALANCE PIC S9(7)V99.
              10 RATE PIC 9V9999.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "NAME" should have type "alphanumeric"
    And the field "BALANCE" should have type "zoned"

  Scenario: Parse record with OCCURS inside group
    Given a copybook with content:
      """
      01 GROUP-OCCURS-REC.
          05 HEADER-INFO.
              10 HDR-ID PIC 9(4).
          05 LINE-ITEMS OCCURS 5 TIMES.
              10 ITEM-CODE PIC X(6).
              10 ITEM-AMT PIC 9(5)V99.
      """
    When the copybook is parsed
    Then parsing should succeed
    And field LINE-ITEMS should have OCCURS count 5

  Scenario: Decode record with alphanumeric field containing digits
    Given a copybook with content:
      """
      01 ALPHA-DIGIT-REC.
          05 ALPHA-NUM PIC X(10).
      """
    And ASCII codepage
    And binary data: "ABC1234567"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field ALPHA-NUM should be "ABC1234567"

  Scenario: Verify multi-group record structure
    Given a copybook with content:
      """
      01 VERIFY-REC.
          05 V-GRP-A.
              10 VA PIC X(5).
          05 V-GRP-B.
              10 VB PIC 9(5).
      """
    And ASCII codepage
    And binary data: "HELLO12345"
    When the data is verified
    Then verification should succeed

  Scenario: Parse record with FILLER between groups
    Given a copybook with content:
      """
      01 FILLER-BETWEEN-GROUPS.
          05 GRP-FIRST.
              10 F1 PIC X(5).
          05 FILLER PIC X(2).
          05 GRP-SECOND.
              10 F2 PIC X(5).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "F1" should have length 5
    And the field "F2" should have length 5

  Scenario: Decode record with all spaces in text field
    Given a copybook with content:
      """
      01 SPACE-TEXT-REC.
          05 SPACE-FIELD PIC X(10).
          05 OTHER-FIELD PIC X(5).
      """
    And ASCII codepage
    And binary data: "          HELLO"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field SPACE-FIELD should be ""

  Scenario: Parse copybook with three levels of nesting
    Given a copybook with content:
      """
      01 THREE-LEVEL-REC.
          05 OUTER.
              10 MIDDLE.
                  15 INNER-FIELD PIC X(8).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "INNER-FIELD" should have type "alphanumeric"
    And the field "INNER-FIELD" should have length 8
