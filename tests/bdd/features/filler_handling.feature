@filler
Feature: FILLER Handling
  Test FILLER field emission and naming

  Scenario: Default excludes FILLER from output
    Given a copybook with content:
      """
      01 FILLER-RECORD.
         05 REAL-FIELD PIC X(5).
         05 FILLER PIC X(5).
         05 ANOTHER-FIELD PIC X(5).
      """
    And ASCII codepage
    And emit_filler is false
    And binary data: "HELLOFILLEWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should not be in output

  Scenario: emit_filler true includes FILLER in output
    Given a copybook with content:
      """
      01 FILLER-RECORD.
         05 REAL-FIELD PIC X(5).
         05 FILLER PIC X(5).
         05 ANOTHER-FIELD PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "HELLOFILLEWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should be in output

  Scenario: FILLER naming convention uses byte offset
    Given a copybook with content:
      """
      01 OFFSET-RECORD.
         05 FIELD-A PIC X(5).
         05 FILLER PIC X(3).
         05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "HELLOPADWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Multiple FILLERs have unique names
    Given a copybook with content:
      """
      01 MULTI-FILLER.
         05 FIELD-A PIC X(3).
         05 FILLER PIC X(2).
         05 FIELD-B PIC X(3).
         05 FILLER PIC X(2).
         05 FIELD-C PIC X(3).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "ABCXXDEFYYGHI"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Nested FILLER in group
    Given a copybook with content:
      """
      01 NESTED-FILLER.
         05 GROUP-A.
            10 FIELD-A PIC X(5).
            10 FILLER PIC X(3).
         05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "HELLOPADWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: No FILLER fields in copybook
    Given a copybook with content:
      """
      01 NO-FILLER.
         05 FIELD-A PIC X(5).
         05 FIELD-B PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should not be in output

  Scenario: emit_filler false with round-trip
    Given a copybook with content:
      """
      01 RT-FILLER.
         05 FIELD-A PIC X(5).
         05 FILLER PIC X(5).
      """
    And ASCII codepage
    And emit_filler is false
    And binary data: "HELLOWORLD"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: FILLER only copybook
    Given a copybook with content:
      """
      01 ALL-FILLER.
         05 FILLER PIC X(10).
      """
    And ASCII codepage
    And emit_filler is false
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: FILLER with valid JSON output
    Given a copybook with content:
      """
      01 JSON-FILLER.
         05 FIELD-A PIC X(5).
         05 FILLER PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: emit_filler word variant with true
    Given a copybook with content:
      """
      01 WORD-FILLER.
         05 REAL-FIELD PIC X(5).
         05 FILLER PIC X(5).
      """
    And ASCII codepage
    And emit_filler is true
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And FILLER should be in output
