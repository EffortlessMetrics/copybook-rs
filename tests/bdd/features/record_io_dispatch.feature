@raw
Feature: Record I/O Dispatch Microcrate

  As a maintainer of codec compatibility APIs
  I want fixed-vs-RDW legacy dispatch isolated in a dedicated microcrate
  So that framing concerns stay single-purpose and independently testable

  Scenario: Fixed encode output round-trips through record I/O dispatch
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
    And the encoded output should round-trip through the record I/O microcrate in fixed mode with LRECL 8

  Scenario: RDW encode output round-trips through record I/O dispatch
    Given a copybook with content:
      """
      01 RDW-RECORD PIC X(5).
      """
    And RDW record format
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"RDW-RECORD":"HELLO"},"RDW-RECORD":"HELLO"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should round-trip through the record I/O microcrate in RDW mode
