@dialect
Feature: Dialect Lever Behavior

  As a developer working with copybooks from different COBOL vendors
  I want the dialect lever to control ODO min_count interpretation
  So that I can correctly handle data from IBM, Micro Focus, or standard COBOL compilers

  # --- Normative dialect ---

  Scenario: Normative dialect preserves declared min_count
    Given a copybook with content:
      """
      01 NORM-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 3 TO 10 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(5).
      """
    And Normative dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 3 and max 10

  Scenario: Normative dialect rejects count below declared minimum
    Given a copybook with content:
      """
      01 NORM-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 3 TO 10 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(5).
      """
    And Normative dialect
    And ASCII codepage
    And binary data: "001AAAAA"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Normative dialect accepts count at declared minimum
    Given a copybook with content:
      """
      01 NORM-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 3 TO 10 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(5).
      """
    And Normative dialect
    And ASCII codepage
    And binary data: "003AAAAABBBBBCCCCC"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Normative dialect accepts count at declared maximum
    Given a copybook with content:
      """
      01 NORM-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 2 TO 3 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(4).
      """
    And Normative dialect
    And ASCII codepage
    And binary data: "003AAAABBBBCCCC"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Zero-tolerant dialect (IBM Enterprise) ---

  Scenario: Zero-tolerant dialect ignores min_count and allows zero
    Given a copybook with content:
      """
      01 ZERO-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(4).
      """
    And Zero-Tolerant dialect
    And ASCII codepage
    And binary data: "000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Zero-tolerant dialect allows count below declared min
    Given a copybook with content:
      """
      01 ZERO-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(4).
      """
    And Zero-Tolerant dialect
    And ASCII codepage
    And binary data: "002AAAABBBB"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Zero-tolerant dialect still enforces max_count
    Given a copybook with content:
      """
      01 ZERO-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 3 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(4).
      """
    And Zero-Tolerant dialect
    And ASCII codepage
    And binary data: "005AAAABBBBCCCCDDDDEEEE"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Zero-tolerant dialect schema shows min_count as 0
    Given a copybook with content:
      """
      01 ZERO-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(4).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 0 and max 20

  # --- One-tolerant dialect (Micro Focus) ---

  Scenario: One-tolerant dialect clamps min_count to at least 1
    Given a copybook with content:
      """
      01 ONE-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 0 TO 20 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(4).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 1 and max 20

  Scenario: One-tolerant dialect rejects zero count
    Given a copybook with content:
      """
      01 ONE-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 0 TO 20 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(4).
      """
    And One-Tolerant dialect
    And ASCII codepage
    And binary data: "000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: One-tolerant dialect accepts count of 1
    Given a copybook with content:
      """
      01 ONE-RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 0 TO 20 TIMES DEPENDING ON ITEM-COUNT.
              10 ITEM-DATA PIC X(4).
      """
    And One-Tolerant dialect
    And ASCII codepage
    And binary data: "001AAAA"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Dialect does not affect non-ODO fields ---

  Scenario: Dialect does not affect fixed OCCURS parsing
    Given a copybook with content:
      """
      01 FIXED-RECORD.
          05 ITEMS OCCURS 5 TIMES.
              10 DATA PIC X(3).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have OCCURS count 5

  Scenario: Dialect does not affect simple alphanumeric field
    Given a copybook with content:
      """
      01 SIMPLE-RECORD.
          05 NAME-FIELD PIC X(10).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "NAME-FIELD" should have type "alphanumeric"
