Feature: Error Taxonomy Coverage

  As a developer integrating with copybook-rs
  I want each error family to produce correct error codes
  So that I can programmatically handle errors by family

  # --- CBKP: Parse errors ---

  Scenario: CBKP parse error on invalid syntax
    Given a copybook with content:
      """
      THIS IS NOT VALID COBOL
      """
    When the copybook is parsed
    Then an error should occur
    And the error code should start with "CBKP"
    And the error message should contain context

  Scenario: CBKP parse error on invalid PIC clause
    Given a copybook with content:
      """
      01 BAD-PIC.
          05 FIELD-1 PIC INVALID.
      """
    When the copybook is parsed
    Then an error should occur
    And the error code should start with "CBKP"

  Scenario: CBKP parse error on nested ODO
    Given a copybook with content:
      """
      01 NESTED-ODO.
          05 OUTER-COUNT PIC 9(3).
          05 OUTER-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON OUTER-COUNT.
              10 INNER-COUNT PIC 9(3).
              10 INNER-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON INNER-COUNT.
                  15 ELEMENT PIC X(5).
      """
    When the copybook is parsed
    Then an error should occur
    And the error code should start with "CBKP"

  # --- CBKS: Schema validation errors ---

  Scenario: CBKS schema error on projection with nonexistent field
    Given a copybook with content:
      """
      01 PROJ-RECORD.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC X(5).
      """
    And field selection: "NONEXISTENT"
    When the copybook is parsed
    And field projection is applied
    Then an error should occur
    And the error code should start with "CBKS"

  # --- CBKD: Data decoding errors ---

  Scenario: CBKD data error on truncated record
    Given a copybook with content:
      """
      01 LONG-RECORD.
          05 FIELD-A PIC X(20).
      """
    And ASCII codepage
    And binary data: "SHORT"
    When the binary data is decoded
    Then an error should occur
    And the error code should start with "CBKD"

  Scenario: CBKD data error on invalid COMP-3 nibble
    Given a copybook with content:
      """
      01 COMP3-BAD.
          05 BAD-PACKED PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And binary data: "\xFF\xFF\xFF"
    When the binary data is decoded
    Then an error should occur
    And the error code should start with "CBKD"

  # --- CBKE: Encoding errors ---

  Scenario: CBKE encode error on numeric overflow
    Given a copybook with content:
      """
      01 OVERFLOW-RECORD.
          05 TINY-NUM PIC 9(2).
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"TINY-NUM":"99999"},"TINY-NUM":"99999"}
      """
    When the JSON data is encoded
    Then an error should occur
    And the error code should start with "CBKE"

  Scenario: CBKE encode error on string length violation
    Given a copybook with content:
      """
      01 STRING-RECORD.
          05 SHORT-STR PIC X(3).
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"SHORT-STR":"TOOLONGSTRING"},"SHORT-STR":"TOOLONGSTRING"}
      """
    When the JSON data is encoded
    Then an error should occur
    And the error code should start with "CBKE"

  # --- Error message quality ---

  Scenario: Parse errors include line context
    Given a copybook with content:
      """
      01 GOOD-RECORD.
          05 FIELD-1 PIC X(5).
      99 INVALID-LEVEL PIC X(5).
      """
    When the copybook is parsed
    Then an error should occur
    And the error message should contain context

  Scenario: Error code display format is stable
    Given a copybook with content:
      """
      THIS IS NOT VALID COBOL AT ALL
      """
    When the copybook is parsed
    Then an error should occur
    And the error display should contain "CBK"
