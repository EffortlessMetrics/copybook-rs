@dialect-extended
Feature: Dialect and ODO Behavior Extended
  As a developer working with different COBOL dialects
  I want ODO behavior to be correct under all dialect modes
  So that data from IBM, Micro Focus, and standard COBOL systems is handled properly

  Scenario: Normative dialect parses simple ODO
    Given a copybook with content:
      """
      01 NORM-ODO.
          05 CNT PIC 9(3).
          05 ARR OCCURS 1 TO 20 TIMES DEPENDING ON CNT.
              10 ELEM PIC X(5).
      """
    And Normative dialect
    When the copybook is parsed
    Then parsing should succeed
    And field ARR should have ODO with min 1 and max 20

  Scenario: Zero-Tolerant dialect allows zero min
    Given a copybook with content:
      """
      01 ZERO-ODO.
          05 CNT PIC 9(3).
          05 ARR OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 ELEM PIC X(5).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then parsing should succeed
    And field ARR should have ODO with min 0 and max 10

  Scenario: One-Tolerant dialect clamps min to 1
    Given a copybook with content:
      """
      01 ONE-ODO.
          05 CNT PIC 9(3).
          05 ARR OCCURS 0 TO 10 TIMES DEPENDING ON CNT.
              10 ELEM PIC X(5).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then parsing should succeed
    And field ARR should have ODO with min 1 and max 10

  Scenario: Normative dialect preserves declared min
    Given a copybook with content:
      """
      01 PRESERVE-MIN.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 5 TO 25 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(8).
      """
    And Normative dialect
    When the copybook is parsed
    Then parsing should succeed
    And field ITEMS should have ODO with min 5 and max 25

  Scenario: Normative dialect with fixed OCCURS
    Given a copybook with content:
      """
      01 FIXED-OCC.
          05 ROWS OCCURS 10 TIMES.
              10 COL PIC X(5).
      """
    And Normative dialect
    When the copybook is parsed
    Then parsing should succeed
    And field ROWS should have OCCURS count 10

  Scenario: Zero-Tolerant dialect with fixed OCCURS
    Given a copybook with content:
      """
      01 ZT-FIXED.
          05 ROWS OCCURS 5 TIMES.
              10 COL PIC X(3).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then parsing should succeed
    And field ROWS should have OCCURS count 5

  Scenario: One-Tolerant dialect with fixed OCCURS
    Given a copybook with content:
      """
      01 OT-FIXED.
          05 ROWS OCCURS 8 TIMES.
              10 COL PIC X(4).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then parsing should succeed
    And field ROWS should have OCCURS count 8

  Scenario: Normative dialect decode ODO record
    Given a copybook with content:
      """
      01 NORM-DEC-ODO.
          05 CNT PIC 9(3).
          05 ARR OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
              10 VAL PIC X(4).
      """
    And Normative dialect
    And ASCII codepage
    And binary data: "002ABCDWXYZ"
    When the copybook is parsed
    And the binary data is decoded
    Then decoding should succeed

  Scenario: Zero-Tolerant dialect with large ODO max
    Given a copybook with content:
      """
      01 ZT-LARGE-ODO.
          05 CNT PIC 9(3).
          05 ARR OCCURS 1 TO 100 TIMES DEPENDING ON CNT.
              10 ELEM PIC X(3).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then parsing should succeed
    And field ARR should have ODO with min 0 and max 100

  Scenario: Normative dialect parse-only with ODO data
    Given a copybook with content:
      """
      01 NORM-RT-ODO.
          05 CNT PIC 9(3).
          05 ARR OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
              10 VAL PIC X(4).
      """
    And Normative dialect
    And ASCII codepage
    And binary data: "003AAAABBBBCCCC"
    When the copybook is parsed
    And the binary data is decoded
    Then decoding should succeed

  Scenario: Parse ODO with counter field verification
    Given a copybook with content:
      """
      01 CTR-VERIFY.
          05 MY-CTR PIC 9(2).
          05 MY-ARR OCCURS 1 TO 10 TIMES DEPENDING ON MY-CTR.
              10 MY-ELM PIC X(6).
      """
    When the copybook is parsed
    Then parsing should succeed
    And field MY-ARR should have ODO with counter "MY-CTR"

  Scenario: Parse ODO rejects non-tail position
    Given a copybook with content:
      """
      01 NON-TAIL.
          05 CNT PIC 9(3).
          05 ARR OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
              10 ELM PIC X(4).
          05 AFTER PIC X(10).
      """
    When the copybook is parsed
    Then an error should occur
