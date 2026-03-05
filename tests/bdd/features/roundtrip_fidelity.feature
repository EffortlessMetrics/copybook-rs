@roundtrip
Feature: Round-Trip Fidelity
  Encode-then-decode (or decode-then-encode) must produce byte-identical
  output to prove lossless data conversion through the pipeline.

  # --- Simple field types ---

  Scenario: Roundtrip simple PIC X field
    Given a copybook with content:
      """
      01 ALPHA-REC.
         05 TEXT-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "HELLOWORLD"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip numeric PIC 9 field
    Given a copybook with content:
      """
      01 DIGIT-REC.
         05 NUM-FIELD PIC 9(7).
      """
    And ASCII codepage
    And binary data: "0012345"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip implied decimal field
    Given a copybook with content:
      """
      01 DEC-REC.
         05 AMOUNT PIC 9(5)V99.
      """
    And ASCII codepage
    And binary data: "0012345"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip COMP-3 packed decimal field
    Given a copybook with content:
      """
      01 PKD-REC.
         05 AMT PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x4C"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip COMP binary integer field
    Given a copybook with content:
      """
      01 BIN-REC.
         05 COUNT-VAL PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x2A"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  # --- Multi-field record ---

  Scenario: Roundtrip multi-field record
    Given a copybook with content:
      """
      01 MULTI-REC.
         05 NAME   PIC X(8).
         05 QTY    PIC 9(5).
         05 REMARK PIC X(7).
      """
    And ASCII codepage
    And binary data: "CUSTOMER0012300NOTES"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  # --- Whitespace and leading-zero preservation ---

  Scenario: Roundtrip preserves trailing spaces
    Given a copybook with content:
      """
      01 SPACE-REC.
         05 PADDED PIC X(10).
      """
    And ASCII codepage
    And binary data: "AB        "
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip preserves leading zeros
    Given a copybook with content:
      """
      01 ZERO-REC.
         05 ACCT-NO PIC 9(8).
      """
    And ASCII codepage
    And binary data: "00000001"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  # --- Multiple codepages ---

  Scenario: Roundtrip with CP037 codepage
    Given a copybook with content:
      """
      01 CP037-REC.
         05 DATA-FIELD PIC X(8).
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip with CP500 codepage
    Given a copybook with content:
      """
      01 CP500-REC.
         05 DATA-FIELD PIC X(8).
      """
    And codepage "CP500"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip with CP1047 codepage
    Given a copybook with content:
      """
      01 CP1047-REC.
         05 DATA-FIELD PIC X(8).
      """
    And codepage "CP1047"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Determinism ---

  Scenario: Roundtrip determinism across two runs
    Given a copybook with content:
      """
      01 DET-REC.
         05 ID   PIC 9(4).
         05 NAME PIC X(6).
      """
    And ASCII codepage
    And binary data: "0001ABCDEF"
    When round-trip determinism is checked
    Then determinism should pass
    And the round 1 hash should equal to round 2 hash
    And there should be no byte differences

  # --- Nested group round-trip ---

  Scenario: Roundtrip nested group structure
    Given a copybook with content:
      """
      01 NESTED-REC.
         05 HEADER.
            10 H-ID   PIC 9(4).
            10 H-NAME PIC X(6).
         05 DETAIL.
            10 D-AMT  PIC 9(5).
      """
    And ASCII codepage
    And binary data: "0001ABCDEF00123"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  # --- Zero-value numeric round-trip ---

  Scenario: Roundtrip zero-value numeric field
    Given a copybook with content:
      """
      01 ZERO-REC.
         05 BALANCE PIC 9(7).
      """
    And ASCII codepage
    And binary data: "0000000"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed
