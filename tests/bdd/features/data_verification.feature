@data-verification
Feature: Data Verification
  Verify binary data against a copybook schema to detect structural and
  content errors before full decode/encode processing.

  Background:
    Given ASCII codepage

  # --- Fixed-format happy path ---

  Scenario: Verify valid fixed-format alphanumeric record
    Given a copybook with content:
      """
      01 CUSTOMER-REC.
         05 CUST-NAME PIC X(10).
         05 CUST-ID   PIC 9(5).
      """
    And binary data: "JOHN DOE  00042"
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors

  Scenario: Verify valid fixed-format numeric-only record
    Given a copybook with content:
      """
      01 NUM-REC.
         05 AMOUNT PIC 9(7)V99.
      """
    And binary data: "001234599"
    When the data is verified
    Then verification should succeed

  # --- Truncated and oversized records ---

  Scenario: Verify truncated record fails
    Given a copybook with content:
      """
      01 WIDE-REC.
         05 FIELD-A PIC X(10).
         05 FIELD-B PIC X(10).
      """
    And binary data: "SHORT"
    When the data is verified
    Then verification should fail

  Scenario: Verify data longer than LRECL succeeds for single record
    Given a copybook with content:
      """
      01 SMALL-REC.
         05 TINY PIC X(3).
      """
    And binary data: "ABCDEF"
    When the data is verified
    Then verification should succeed

  # --- RDW format ---

  Scenario: Verify valid RDW data
    Given a copybook with content:
      """
      01 RDW-REC.
         05 PAYLOAD PIC X(6).
      """
    And RDW record format
    And binary data: "\x00\x06\x00\x00HELLO!"
    When the data is verified
    Then verification should succeed

  Scenario: Verify invalid RDW header (truncated body)
    Given a copybook with content:
      """
      01 RDW-REC.
         05 PAYLOAD PIC X(6).
      """
    And RDW record format
    And binary data: "\x00\x0A\x00\x00AB"
    When the data is verified
    Then verification should fail

  # --- COMP-3 packed decimal ---

  Scenario: Verify COMP-3 field with valid packed data
    Given a copybook with content:
      """
      01 PKD-REC.
         05 AMT PIC S9(5) COMP-3.
      """
    And binary data: "\x01\x23\x4C"
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors

  Scenario: Verify COMP-3 with invalid nibbles fails in strict mode
    Given a copybook with content:
      """
      01 PKD-REC.
         05 AMT PIC S9(5) COMP-3.
      """
    And strict mode
    And binary data: "\xFF\xFF\xFF"
    When the data is verified
    Then verification should fail

  # --- Numeric field validation ---

  Scenario: Verify numeric field with non-numeric characters
    Given a copybook with content:
      """
      01 NUM-REC.
         05 QTY PIC 9(5).
      """
    And strict mode
    And binary data: "ABCDE"
    When the data is verified
    Then verification should fail

  Scenario: Verify numeric field with valid digits
    Given a copybook with content:
      """
      01 NUM-REC.
         05 QTY PIC 9(5).
      """
    And binary data: "00042"
    When the data is verified
    Then verification should succeed

  # --- Empty and multi-record ---

  Scenario: Verify empty file with zero records
    Given a copybook with content:
      """
      01 ANY-REC.
         05 DATA-FIELD PIC X(10).
      """
    And binary data: ""
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors

  Scenario: Verify multi-record file
    Given a copybook with content:
      """
      01 ROW-REC.
         05 ROW-DATA PIC X(5).
      """
    And binary data: "AAAAABBBBBCCCCC"
    When the data is verified
    Then verification should succeed

  # --- Metadata emission ---

  Scenario: Verify with emit_meta enabled produces metadata
    Given a copybook with content:
      """
      01 META-REC.
         05 ITEM PIC X(8).
      """
    And emit_meta is enabled
    And binary data: "TESTDATA"
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors

  Scenario: Verify with emit_meta disabled
    Given a copybook with content:
      """
      01 META-REC.
         05 ITEM PIC X(8).
      """
    And emit_meta is disabled
    And binary data: "TESTDATA"
    When the data is verified
    Then verification should succeed

  # --- JSON report output ---

  Scenario: Verify JSON report format is valid
    Given a copybook with content:
      """
      01 JSON-REC.
         05 FIELD PIC X(5).
      """
    And binary data: "HELLO"
    When the data is verified with JSON report
    Then verification should succeed
    And the verify report should be valid JSON

  # --- Binary integer verification ---

  Scenario: Verify COMP binary integer data
    Given a copybook with content:
      """
      01 BIN-REC.
         05 COUNT-FIELD PIC S9(4) COMP.
      """
    And binary data: "\x00\x2A"
    When the data is verified
    Then verification should succeed

  # --- Group-level verification ---

  Scenario: Verify nested group structure
    Given a copybook with content:
      """
      01 GRP-REC.
         05 HEADER.
            10 H-NAME PIC X(8).
            10 H-CODE PIC 9(3).
         05 DETAIL.
            10 D-AMT  PIC 9(5).
      """
    And binary data: "CUSTOMER12300042"
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors

  # --- All-spaces record ---

  Scenario: Verify all-spaces alphanumeric record
    Given a copybook with content:
      """
      01 BLANK-REC.
         05 BLANK-FIELD PIC X(10).
      """
    And binary data: "          "
    When the data is verified
    Then verification should succeed

  # --- Mixed field types ---

  Scenario: Verify record with mixed field types
    Given a copybook with content:
      """
      01 MIXED-REC.
         05 NAME    PIC X(8).
         05 BALANCE PIC S9(5) COMP-3.
         05 FLAGS   PIC X(2).
      """
    And binary data for all fields
    When the data is verified
    Then verification should succeed
    And the verify report should contain 0 errors
