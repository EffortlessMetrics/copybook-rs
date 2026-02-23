@raw
Feature: Fixed Record Framing

  As a developer handling fixed-length mainframe files
  I want fixed framing to be delegated to a dedicated microcrate
  So that codec behavior stays stable while the framing concern remains isolated

  Scenario: Encoded fixed output round-trips through the fixed microcrate
    Given a copybook with content:
      """
      01 FIXED-RECORD PIC X(8).
      """
    And fixed record format
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"FIXED-RECORD":"ABCDEFGH"},"FIXED-RECORD":"ABCDEFGH"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 8 bytes
    And the encoded output should round-trip through the fixed microcrate with LRECL 8

  Scenario: Raw binary input is readable through the fixed microcrate
    Given a simple copybook with a single field
    And fixed record format
    And ASCII codepage
    And binary data: "HELLO     "
    Then the binary input should be readable by the fixed microcrate with LRECL 10
