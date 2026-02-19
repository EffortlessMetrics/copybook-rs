@verify
Feature: Verify Command
  Test data verification against schema

  Scenario: Verify valid fixed-length data
    Given a copybook with content:
      """
      01 VERIFY-RECORD.
         05 NAME-FIELD PIC X(10).
         05 AGE-FIELD PIC 9(3).
      """
    And ASCII codepage
    And binary data: "JOHN DOE  025"
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors

  Scenario: Verify truncated data fails
    Given a copybook with content:
      """
      01 VERIFY-RECORD.
         05 NAME-FIELD PIC X(10).
         05 AGE-FIELD PIC 9(3).
      """
    And ASCII codepage
    And binary data: "SHORT"
    When the data is verified
    Then verification should fail

  Scenario: Verify valid numeric data
    Given a copybook with content:
      """
      01 NUM-RECORD.
         05 AMOUNT PIC 9(7)V99.
      """
    And ASCII codepage
    And binary data: "001234599"
    When the data is verified
    Then verification should succeed

  Scenario: Verify with JSON report format
    Given a copybook with content:
      """
      01 REPORT-RECORD.
         05 DATA-FIELD PIC X(5).
      """
    And ASCII codepage
    And binary data: "HELLO"
    When the data is verified with JSON report
    Then verification should succeed
    And the verify report should be valid JSON

  Scenario: Verify with field projection
    Given a copybook with content:
      """
      01 PROJ-RECORD.
         05 FIELD-A PIC X(5).
         05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And field selection: "FIELD-A"
    And binary data: "HELLOWORLD"
    When the data is verified
    Then verification should succeed

  Scenario: Verify all-spaces data
    Given a copybook with content:
      """
      01 SPACES-RECORD.
         05 TEXT-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "          "
    When the data is verified
    Then verification should succeed

  Scenario: Verify COMP-3 data
    Given a copybook with content:
      """
      01 COMP3-VERIFY.
         05 PACKED-FIELD PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x4C"
    When the data is verified
    Then verification should succeed

  Scenario: Verify with strict mode
    Given a copybook with content:
      """
      01 STRICT-RECORD.
         05 DATA-FIELD PIC X(10).
      """
    And ASCII codepage
    And strict mode
    And binary data: "ABCDEFGHIJ"
    When the data is verified
    Then verification should succeed

  Scenario: Verify with lenient mode
    Given a copybook with content:
      """
      01 LENIENT-RECORD.
         05 DATA-FIELD PIC X(10).
      """
    And ASCII codepage
    And lenient mode
    And binary data: "ABCDEFGHIJ"
    When the data is verified
    Then verification should succeed

  Scenario: Verify empty record
    Given a copybook with content:
      """
      01 EMPTY-RECORD.
         05 DATA-FIELD PIC X(5).
      """
    And ASCII codepage
    And binary data: ""
    When the data is verified
    Then verification should succeed

  Scenario: Verify group-level copybook
    Given a copybook with content:
      """
      01 GROUP-RECORD.
         05 GROUP-A.
            10 FIELD-A1 PIC X(5).
            10 FIELD-A2 PIC 9(3).
         05 GROUP-B.
            10 FIELD-B1 PIC X(5).
      """
    And ASCII codepage
    And binary data: "HELLO123WORLD"
    When the data is verified
    Then verification should succeed

  Scenario: Verify with Normative dialect
    Given a copybook with content:
      """
      01 DIALECT-RECORD.
         05 COUNT-FIELD PIC 9(3).
         05 ITEMS OCCURS 1 TO 5 DEPENDING ON COUNT-FIELD.
            10 ITEM-DATA PIC X(5).
      """
    And Normative dialect
    And ASCII codepage
    And binary data: "002ITEM1ITEM2"
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Verify JSON report contains status
    Given a copybook with content:
      """
      01 STATUS-RECORD.
         05 STATUS PIC X(5).
      """
    And ASCII codepage
    And binary data: "HELLO"
    When the data is verified with JSON report
    Then the verify report should be valid JSON

  Scenario: Verify binary integer data
    Given a copybook with content:
      """
      01 BIN-RECORD.
         05 BIN-FIELD PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x2A"
    When the data is verified
    Then verification should succeed
