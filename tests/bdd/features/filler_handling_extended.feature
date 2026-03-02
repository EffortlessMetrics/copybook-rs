@filler @extended
Feature: FILLER Handling Extended Scenarios

  As a developer working with COBOL copybooks containing FILLER fields
  I want to verify FILLER naming, positioning, and emit_filler flag behavior
  So that FILLER fields are properly handled in all contexts

  # --- FILLER at various positions ---

  Scenario: FILLER at beginning of record
    Given a copybook with content:
      """
      01 FILLER-START.
          05 FILLER PIC X(5).
          05 DATA-FIELD PIC X(10).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "PADDDACTUALDATA"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should be in output

  Scenario: FILLER at end of record
    Given a copybook with content:
      """
      01 FILLER-END.
          05 DATA-FIELD PIC X(10).
          05 FILLER PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "ACTUALDATAPADDD"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should be in output

  Scenario: FILLER between two data fields
    Given a copybook with content:
      """
      01 FILLER-MID.
          05 FIELD-A PIC X(5).
          05 FILLER PIC X(3).
          05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And emit_filler is false
    And binary data: "HELLOPADWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should not be in output
    And decoded field FIELD-A should be "HELLO"
    And decoded field FIELD-B should be "WORLD"

  Scenario: Multiple consecutive FILLERs
    Given a copybook with content:
      """
      01 MULTI-FILLER.
          05 FIELD-A PIC X(3).
          05 FILLER PIC X(2).
          05 FILLER PIC X(2).
          05 FIELD-B PIC X(3).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "ABCXXXXDEF"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should be in output
    And the decoded output should be valid JSON

  # --- FILLER offset naming ---

  Scenario: FILLER offset naming produces valid JSON keys
    Given a copybook with content:
      """
      01 OFFSET-REC.
          05 FIELD-A PIC X(10).
          05 FILLER PIC X(5).
          05 FIELD-B PIC X(10).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "AAAAAAAAAA12345BBBBBBBBBB"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: FILLER names are unique across record
    Given a copybook with content:
      """
      01 UNIQUE-FILLER.
          05 FILLER PIC X(3).
          05 FIELD-A PIC X(4).
          05 FILLER PIC X(3).
          05 FIELD-B PIC X(4).
          05 FILLER PIC X(3).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "XXXDATAYYYINFOZZZ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- emit_filler flag behavior ---

  Scenario: emit_filler false excludes all FILLER fields
    Given a copybook with FILLER:
      """
      01 EXCL-FILLER.
          05 FIELD-A PIC X(5).
          05 FILLER PIC X(10).
          05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And emit_filler is false
    And binary data: "HELLOFILLERPADDWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should not be in output

  Scenario: emit_filler true includes all FILLER fields
    Given a copybook with FILLER:
      """
      01 INCL-FILLER.
          05 FIELD-A PIC X(5).
          05 FILLER PIC X(10).
          05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "HELLOFILLERPADDWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should be in output

  Scenario: FILLER in nested group with emit_filler false
    Given a copybook with content:
      """
      01 NESTED-FILLER.
          05 GROUP-A.
              10 FIELD-A PIC X(5).
              10 FILLER PIC X(3).
              10 FIELD-B PIC X(5).
          05 FIELD-C PIC X(5).
      """
    And ASCII codepage
    And emit_filler is false
    And binary data: "HELLOPADWORLDOUTER"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should not be in output

  Scenario: FILLER in nested group with emit_filler true
    Given a copybook with content:
      """
      01 NESTED-FILLER.
          05 GROUP-A.
              10 FIELD-A PIC X(5).
              10 FILLER PIC X(3).
              10 FIELD-B PIC X(5).
          05 FIELD-C PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "HELLOPADWORLDOUTER"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should be in output

  Scenario: Large FILLER field preserved in round-trip
    Given a copybook with content:
      """
      01 LARGE-FILLER.
          05 DATA-A PIC X(5).
          05 FILLER PIC X(50).
          05 DATA-B PIC X(5).
      """
    And ASCII codepage
    And emit_filler is false
    And binary data for all fields
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
