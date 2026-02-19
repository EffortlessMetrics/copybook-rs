Feature: Golden Fixtures Validation

  As a developer working with copybook-rs
  I want to validate golden fixtures for structural consistency
  So that I can ensure consistent behavior across releases

  Scenario: Validate golden fixture schema structure
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
          05 CUSTOMER-ID PIC 9(6).
          05 CUSTOMER-NAME PIC X(30).
          05 ACCOUNT-BALANCE PIC S9(7)V99 COMP-3.
          05 LAST-ACTIVITY-DATE PIC 9(8).
          05 STATUS-CODE PIC X(1).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And the field "CUSTOMER-ID" should have type "zoned"
    And the field "CUSTOMER-NAME" should have type "alphanumeric"
    And the field "ACCOUNT-BALANCE" should have type "packed"
    And the schema should have fingerprint

  Scenario: Compare current output against golden fixture
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
          05 CUSTOMER-ID PIC 9(6).
          05 CUSTOMER-NAME PIC X(30).
          05 ACCOUNT-BALANCE PIC S9(7)V99 COMP-3.
          05 LAST-ACTIVITY-DATE PIC 9(8).
          05 STATUS-CODE PIC X(1).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CUSTOMER-ID" should have offset 0
    And the field "CUSTOMER-ID" should have length 6
    And the field "CUSTOMER-NAME" should have offset 6
    And the field "CUSTOMER-NAME" should have length 30
    And the field "ACCOUNT-BALANCE" should have offset 36
    And the field "ACCOUNT-BALANCE" should have length 5
    And the field "LAST-ACTIVITY-DATE" should have offset 41
    And the field "LAST-ACTIVITY-DATE" should have length 8
    And the field "STATUS-CODE" should have offset 49
    And the field "STATUS-CODE" should have length 1

  Scenario: Update golden fixtures when behavior changes
    Given a copybook with content:
      """
      01 TEST-RECORD.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC 9(5).
          05 FIELD-3 PIC X(15).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And there should be 3 leaf fields
    And the schema should have fingerprint
    And the field "FIELD-1" should have offset 0
    And the field "FIELD-1" should have length 10
    And the field "FIELD-2" should have offset 10
    And the field "FIELD-2" should have length 5
    And the field "FIELD-3" should have offset 15
    And the field "FIELD-3" should have length 15

  Scenario: Golden fixture validation for simple copybook type
    Given a simple copybook with a single field
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And there should be 1 leaf fields
    And the schema should have fingerprint
    And the field "TEST-FIELD" should have type "alphanumeric"
    And the field "TEST-FIELD" should have offset 0
    And the field "TEST-FIELD" should have length 10

  Scenario: Golden fixture validation for complex copybook type
    Given a copybook with content:
      """
      01 COMPLEX-RECORD.
          05 HEADER-GROUP.
              10 RECORD-TYPE PIC X(1).
              10 RECORD-VERSION PIC 9(2).
          05 DATA-GROUP.
              10 CUSTOMER-ID PIC 9(6).
              10 TRANSACTION-AMOUNT PIC S9(9)V99 COMP-3.
              10 TRANSACTION-DATE PIC 9(8).
          05 ARRAY-DATA OCCURS 5 TIMES.
              10 ARRAY-ELEMENT PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And there should be 6 leaf fields
    And the schema should have fingerprint
    And the field "RECORD-TYPE" should have type "alphanumeric"
    And the field "RECORD-VERSION" should have type "zoned"
    And the field "CUSTOMER-ID" should have type "zoned"
    And the field "TRANSACTION-AMOUNT" should have type "packed"
    And the field "TRANSACTION-DATE" should have type "zoned"
    And the field "ARRAY-DATA" should have type "occurs"

  Scenario: Golden fixture validation for ASCII encoding format
    Given a simple copybook with a single field
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"TEST-FIELD":"TESTDATA123"},"TEST-FIELD":"TESTDATA123"}
      """
    When JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes
    And the round-trip should be lossless
    And decoding should succeed

  Scenario: Golden fixture validation for EBCDIC encoding format
    Given a simple copybook with a single field
    And EBCDIC codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"EBCDIC037","fields":{"TEST-FIELD":"TESTDATA123"},"TEST-FIELD":"TESTDATA123"}
      """
    When JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes
    And the round-trip should be lossless
    And decoding should succeed

  Scenario: Golden fixture validation for error scenarios
    Given a copybook with invalid SIGN SEPARATE
    When the copybook is parsed
    Then parsing should fail
    And parsing should fail with error code "CBKP001"

  Scenario: Golden fixture validation with dialect variations - Normative
    Given Normative dialect
    And a copybook with content:
      """
      01 DIALECT-TEST.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And there should be 2 leaf fields
    And the schema should have fingerprint

  Scenario: Golden fixture validation with dialect variations - Zero-Tolerant
    Given Zero-Tolerant dialect
    And a copybook with content:
      """
      01 DIALECT-TEST.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC 9(5)V99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And there should be 2 leaf fields
    And the schema should have fingerprint

  Scenario: Golden fixture validation with field projection
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
          05 CUSTOMER-ID PIC 9(6).
          05 CUSTOMER-NAME PIC X(30).
          05 ACCOUNT-BALANCE PIC S9(7)V99 COMP-3.
          05 LAST-ACTIVITY-DATE PIC 9(8).
          05 STATUS-CODE PIC X(1).
      """
    And field selection is ["CUSTOMER-ID", "CUSTOMER-NAME"]
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And there should be 2 leaf fields
    And the schema should have fingerprint

  Scenario: Golden fixture validation with RDW processing
    Given RDW record format
    And a copybook with content:
      """
      01 RDW-RECORD.
          05 RECORD-HEADER PIC X(4).
          05 RECORD-DATA PIC X(50).
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"RECORD-HEADER":"HDR1","RECORD-DATA":"SAMPLE DATA FOR RDW PROCESSING TEST"},"RECORD-HEADER":"HDR1","RECORD-DATA":"SAMPLE DATA FOR RDW PROCESSING TEST"}
      """
    When JSON data is encoded
    Then encoding should succeed
    And the round-trip should be lossless
    And decoding should succeed

  Scenario: Golden fixture validation with edited PIC encoding
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 EDITED-NUMBER PIC $$,$$$,$$9.99.
          05 EDITED-DATE PIC 99/99/9999.
          05 EDITED-TIME PIC 99:99:99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should contain 1 top-level field(s)
    And there should be 3 leaf fields
    And the field "EDITED-NUMBER" should have type "edited"
    And the field "EDITED-DATE" should have type "edited"
    And the field "EDITED-TIME" should have type "edited"
    And the schema should have fingerprint

  Scenario: Golden fixture validation for COMP-3 round-trip
    Given a copybook with content:
      """
      01 COMP3-TEST.
          05 RECORD-ID PIC 9(4).
          05 POSITIVE-AMOUNT PIC S9(5)V99 COMP-3.
          05 NEGATIVE-AMOUNT PIC S9(5)V99 COMP-3.
          05 DECIMAL-AMOUNT PIC S9(3)V99 COMP-3.
          05 UNSIGNED-AMOUNT PIC 9(3) COMP-3.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"RECORD-ID":"0001","POSITIVE-AMOUNT":"12345","NEGATIVE-AMOUNT":"-67890","DECIMAL-AMOUNT":"123.45","UNSIGNED-AMOUNT":"999"},"RECORD-ID":"0001","POSITIVE-AMOUNT":"12345","NEGATIVE-AMOUNT":"-67890","DECIMAL-AMOUNT":"123.45","UNSIGNED-AMOUNT":"999"}
      """
    When JSON data is encoded
    Then encoding should succeed
    And the round-trip should be lossless
    And decoding should succeed
    And decoded RECORD-ID should be "0001"
    And decoded POSITIVE-AMOUNT should be "12345"
    And decoded NEGATIVE-AMOUNT should be "-67890"
    And decoded DECIMAL-AMOUNT should be "123.45"
    And decoded UNSIGNED-AMOUNT should be "999"