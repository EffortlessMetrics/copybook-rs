@verification-extended
Feature: Verification and Data Validation Extended
  As a developer validating mainframe data
  I want the verify command to correctly validate data against schemas
  So that data quality issues are caught before processing

  Scenario: Verify valid alphanumeric data
    Given a copybook with content:
      """
      01 VALID-ALPHA.
          05 NAME PIC X(10).
      """
    And ASCII codepage
    And binary data: "JOHN SMITH"
    When the data is verified
    Then verification should succeed

  Scenario: Verify valid numeric data
    Given a copybook with content:
      """
      01 VALID-NUM.
          05 AMOUNT PIC 9(7).
      """
    And ASCII codepage
    And binary data: "0001234"
    When the data is verified
    Then verification should succeed

  Scenario: Verify COMP-3 data
    Given a copybook with content:
      """
      01 VALID-COMP3.
          05 PKD PIC S9(5)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x45\x6C"
    When the data is verified
    Then verification should succeed

  Scenario: Verify binary integer data
    Given a copybook with content:
      """
      01 VALID-COMP.
          05 BIN PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x64"
    When the data is verified
    Then verification should succeed

  Scenario: Verify mixed field record
    Given a copybook with content:
      """
      01 VALID-MIX.
          05 ID PIC 9(4).
          05 NAME PIC X(10).
      """
    And ASCII codepage
    And binary data: "0042JOHN SMITH"
    When the data is verified
    Then verification should succeed

  Scenario: Verify with JSON report format
    Given a copybook with content:
      """
      01 JSON-VERIFY.
          05 DATA PIC X(8).
      """
    And ASCII codepage
    And binary data: "TESTDATA"
    When the data is verified with JSON report
    Then the verify report should be valid JSON

  Scenario: Verify all-spaces record
    Given a copybook with content:
      """
      01 SPACE-VERIFY.
          05 TEXT PIC X(10).
      """
    And ASCII codepage
    And binary data: "          "
    When the data is verified
    Then verification should succeed

  Scenario: Verify all-zeros numeric record
    Given a copybook with content:
      """
      01 ZERO-VERIFY.
          05 NUM PIC 9(8).
      """
    And ASCII codepage
    And binary data with zero value
    When the data is verified
    Then verification should succeed

  Scenario: Verify record with group structure
    Given a copybook with content:
      """
      01 GRP-VERIFY.
          05 GRP.
              10 FA PIC X(5).
              10 FB PIC 9(3).
      """
    And ASCII codepage
    And binary data: "HELLO123"
    When the data is verified
    Then verification should succeed

  Scenario: Verify record with EBCDIC codepage
    Given a copybook with content:
      """
      01 EBCDIC-VERIFY.
          05 DATA PIC X(5).
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is verified
    Then verification should succeed

  Scenario: Verify empty data succeeds
    Given a copybook with content:
      """
      01 EMPTY-VERIFY.
          05 FIELD PIC X(5).
      """
    And ASCII codepage
    And binary data: ""
    When the data is verified
    Then verification should succeed

  Scenario: Verify report has zero errors for valid data
    Given a copybook with content:
      """
      01 ZERO-ERR.
          05 TEXT PIC X(10).
      """
    And ASCII codepage
    And binary data: "VALID DATA"
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors

  Scenario: Verify large alphanumeric field
    Given a copybook with content:
      """
      01 LARGE-VERIFY.
          05 BIG PIC X(100).
      """
    And ASCII codepage
    And binary data for all fields
    When the data is verified
    Then verification should succeed

  Scenario: Verify multi-field record with correct offsets
    Given a copybook with content:
      """
      01 OFFSET-VERIFY.
          05 A PIC X(3).
          05 B PIC 9(4).
          05 C PIC X(3).
      """
    And ASCII codepage
    And binary data: "ABC1234XYZ"
    When the data is verified
    Then verification should succeed

  Scenario: Verify with CP1047 codepage
    Given a copybook with content:
      """
      01 CP1047-VERIFY.
          05 MSG PIC X(8).
      """
    And codepage "CP1047"
    And binary data for all fields
    When the data is verified
    Then verification should succeed
