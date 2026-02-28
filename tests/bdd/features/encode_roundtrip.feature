Feature: Encode Round-Trip and Edge Cases

  As a developer working with COBOL data
  I want encode-then-decode to produce identical JSON
  So that I can trust data fidelity through the pipeline

  # --- Lossless round-trip ---

  Scenario: Round-trip simple alphanumeric field
    Given a copybook with content:
      """
      01 SIMPLE-RECORD.
          05 NAME-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "JOHN DOE  "
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip zoned decimal field
    Given a copybook with content:
      """
      01 NUMERIC-RECORD.
          05 AMOUNT PIC 9(5).
      """
    And ASCII codepage
    And binary data: "12345"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip nested group structure
    Given a copybook with content:
      """
      01 NESTED-RECORD.
          05 GROUP-A.
              10 FIELD-1 PIC X(5).
              10 FIELD-2 PIC 9(3).
          05 FIELD-3 PIC X(8).
      """
    And ASCII codepage
    And binary data: "ABCDE12300000000"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip OCCURS array
    Given a copybook with OCCURS clause
    And ASCII codepage
    And binary data: "ELEMENT0001ELEMENT0002ELEMENT0003ELEMENT0004ELEMENT0005"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  # --- Numeric overflow handling ---

  Scenario: Encode numeric value that overflows field capacity
    Given a copybook with content:
      """
      01 OVERFLOW-RECORD.
          05 SMALL-NUM PIC 9(3).
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"SMALL-NUM":"999999"},"SMALL-NUM":"999999"}
      """
    When the JSON data is encoded
    Then an error should occur

  Scenario: Encode string that exceeds field length
    Given a copybook with content:
      """
      01 LENGTH-RECORD.
          05 SHORT-FIELD PIC X(3).
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"SHORT-FIELD":"TOOLONGVALUE"},"SHORT-FIELD":"TOOLONGVALUE"}
      """
    When the JSON data is encoded
    Then an error should occur

  # --- Encoding with field projection ---

  Scenario: Decode with field projection selects subset of fields
    Given a copybook with content:
      """
      01 PROJ-RECORD.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC X(5).
          05 FIELD-C PIC X(5).
      """
    And ASCII codepage
    And field selection: "FIELD-A,FIELD-C"
    When field projection is applied
    Then the projection should succeed
    And the field "FIELD-A" should be included in projection
    And the field "FIELD-C" should be included in projection
    And the field "FIELD-B" should not be included in projection

  Scenario: Projection with nonexistent field produces error
    Given a copybook with content:
      """
      01 PROJ-RECORD.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And field selection: "NONEXISTENT-FIELD"
    When field projection is applied
    Then the projection should fail
    And an error should occur

  # --- Encode with EBCDIC round-trip ---

  Scenario: Round-trip with EBCDIC codepage
    Given a simple copybook with a single field
    And EBCDIC codepage
    And binary data: "\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xD1"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  # --- Encode with zero-filled numeric ---

  Scenario: Encode zero-value numeric field
    Given a copybook with content:
      """
      01 ZERO-RECORD.
          05 AMOUNT PIC 9(5).
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"00000"},"AMOUNT":"00000"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes
