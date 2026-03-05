@record-framing
Feature: Record Framing

  As a developer processing mainframe data files
  I want fixed-length and RDW record framing to be handled correctly
  So that records are properly delimited and errors are caught

  # --- Fixed-length record decode ---

  Scenario: Fixed-length record decodes correctly
    Given a copybook with content:
      """
      01 FIXED-REC.
         05 NAME    PIC X(10).
         05 AGE     PIC 9(3).
      """
    And fixed record format
    And ASCII codepage
    And binary data: "JOHN      025"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "JOHN"

  Scenario: Fixed-length record with exact LRECL
    Given a copybook with content:
      """
      01 EXACT-REC.
         05 FIELD-A PIC X(5).
         05 FIELD-B PIC X(5).
      """
    And fixed record format
    And ASCII codepage
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Fixed-length decode pads short data to LRECL
    Given a copybook with content:
      """
      01 PAD-FIXED-REC.
         05 TEXT-FIELD PIC X(10).
      """
    And fixed record format
    And ASCII codepage
    And binary data: "HI"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Fixed-length numeric record decodes zeros
    Given a copybook with content:
      """
      01 NUM-FIXED-REC.
         05 AMOUNT PIC 9(8).
      """
    And fixed record format
    And ASCII codepage
    And binary data: "00000000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Fixed-length encode produces exact LRECL
    Given a copybook with content:
      """
      01 ENCODE-FIXED.
         05 MSG PIC X(8).
      """
    And fixed record format
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"MSG":"TESTDATA"},"MSG":"TESTDATA"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 8 bytes

  Scenario: Fixed-length round-trip is lossless
    Given a copybook with content:
      """
      01 RT-FIXED.
         05 TEXT-FIELD PIC X(10).
         05 NUM-FIELD PIC 9(5).
      """
    And fixed record format
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- RDW record framing ---

  Scenario: RDW encode wraps payload with 4-byte header
    Given a copybook with content:
      """
      01 RDW-REC.
         05 PAYLOAD PIC X(6).
      """
    And RDW record format
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"PAYLOAD":"ABCDEF"},"PAYLOAD":"ABCDEF"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should start with RDW header

  Scenario: RDW encoded output is readable by RDW reader
    Given a copybook with content:
      """
      01 RDW-READABLE.
         05 DATA PIC X(5).
      """
    And RDW record format
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"DATA":"HELLO"},"DATA":"HELLO"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be readable by the RDW reader microcrate

  Scenario: RDW round-trip through record I/O microcrate
    Given a copybook with content:
      """
      01 RDW-RT.
         05 VAL-DATA PIC X(4).
      """
    And RDW record format
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"VAL-DATA":"TEST"},"VAL-DATA":"TEST"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should round-trip through the record I/O microcrate in RDW mode

  # --- Fixed framing via microcrate ---

  Scenario: Fixed encode round-trips through fixed microcrate
    Given a copybook with content:
      """
      01 FIXED-MC.
         05 ITEM PIC X(6).
      """
    And fixed record format
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"ITEM":"ABCDEF"},"ITEM":"ABCDEF"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes
    And the encoded output should round-trip through the fixed microcrate with LRECL 6

  Scenario: Fixed binary input is readable by fixed microcrate
    Given a copybook with content:
      """
      01 READABLE-FIXED.
         05 BYTE-FIELD PIC X(8).
      """
    And fixed record format
    And ASCII codepage
    And binary data: "12345678"
    Then the binary input should be readable by the fixed microcrate with LRECL 8

  Scenario: Fixed decode of packed decimal in fixed record
    Given a copybook with content:
      """
      01 COMP3-FIXED.
         05 AMOUNT PIC S9(5)V99 COMP-3.
      """
    And fixed record format
    And ASCII codepage
    And binary data: "\x00\x12\x34\x5C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Additional fixed framing scenarios ---

  Scenario: Fixed-length multi-field decode succeeds
    Given a copybook with content:
      """
      01 MULTI-FIXED.
         05 FIELD-A PIC X(3).
         05 FIELD-B PIC X(3).
         05 FIELD-C PIC X(4).
      """
    And fixed record format
    And ASCII codepage
    And binary data: "ABCDEF1234"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Fixed-length binary integer in fixed record
    Given a copybook with content:
      """
      01 BIN-FIXED.
         05 COUNT PIC S9(4) COMP.
      """
    And fixed record format
    And ASCII codepage
    And binary data: "\x00\x0A"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Fixed encode of space-padded alphanumeric field
    Given a copybook with content:
      """
      01 PADDED-FIXED.
         05 SHORT-TEXT PIC X(10).
      """
    And fixed record format
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"SHORT-TEXT":"HI"},"SHORT-TEXT":"HI"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes
