Feature: Dialect Modes for ODO Behavior

  As a developer working with copybooks from different COBOL vendors
  I want dialect modes to control ODO min_count interpretation
  So that I can correctly process data from IBM, Micro Focus, or standard COBOL

  # --- Normative dialect enforcement ---

  Scenario: Normative dialect enforces declared min_count
    Given a copybook with content:
      """
      01 NORM-RECORD.
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON COUNT-FIELD.
              10 ITEM-DATA PIC X(4).
      """
    And Normative dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 5 and max 20

  Scenario: Normative dialect rejects count below declared min
    Given a copybook with content:
      """
      01 NORM-RECORD.
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON COUNT-FIELD.
              10 ITEM-DATA PIC X(4).
      """
    And Normative dialect
    And ASCII codepage
    And binary data: "002AAAABBBB"
    When the binary data is decoded
    Then an error should occur

  Scenario: Normative dialect accepts count at declared min
    Given a copybook with content:
      """
      01 NORM-RECORD.
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON COUNT-FIELD.
              10 ITEM-DATA PIC X(4).
      """
    And Normative dialect
    And ASCII codepage
    And binary data: "005AAAABBBBCCCCDDDDEEEE"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Zero-tolerant dialect ---

  Scenario: Zero-tolerant dialect allows zero count
    Given a copybook with content:
      """
      01 ZERO-RECORD.
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON COUNT-FIELD.
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
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON COUNT-FIELD.
              10 ITEM-DATA PIC X(4).
      """
    And Zero-Tolerant dialect
    And ASCII codepage
    And binary data: "002AAAABBBB"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Zero-tolerant dialect still respects max count
    Given a copybook with content:
      """
      01 ZERO-RECORD.
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 1 TO 3 TIMES DEPENDING ON COUNT-FIELD.
              10 ITEM-DATA PIC X(4).
      """
    And Zero-Tolerant dialect
    And ASCII codepage
    And binary data: "005AAAABBBBCCCCDDDDEEEE"
    When the binary data is decoded
    Then an error should occur

  # --- One-tolerant dialect ---

  Scenario: One-tolerant dialect clamps min_count to 1
    Given a copybook with content:
      """
      01 ONE-RECORD.
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 0 TO 20 TIMES DEPENDING ON COUNT-FIELD.
              10 ITEM-DATA PIC X(4).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 1 and max 20

  Scenario: One-tolerant dialect rejects zero count when min declared as 0
    Given a copybook with content:
      """
      01 ONE-RECORD.
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 0 TO 20 TIMES DEPENDING ON COUNT-FIELD.
              10 ITEM-DATA PIC X(4).
      """
    And One-Tolerant dialect
    And ASCII codepage
    And binary data: "000"
    When the binary data is decoded
    Then an error should occur

  Scenario: One-tolerant dialect accepts count of 1
    Given a copybook with content:
      """
      01 ONE-RECORD.
          05 COUNT-FIELD PIC 9(3).
          05 ITEMS OCCURS 0 TO 20 TIMES DEPENDING ON COUNT-FIELD.
              10 ITEM-DATA PIC X(4).
      """
    And One-Tolerant dialect
    And ASCII codepage
    And binary data: "001AAAA"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Dialect does not affect non-ODO parsing ---

  Scenario: Dialect setting does not affect simple OCCURS
    Given a copybook with content:
      """
      01 FIXED-RECORD.
          05 ITEMS OCCURS 3 TIMES.
              10 DATA PIC X(5).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have OCCURS count 3

  Scenario: Dialect setting does not affect alphanumeric fields
    Given a copybook with content:
      """
      01 ALPHA-RECORD.
          05 NAME-FIELD PIC X(10).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "NAME-FIELD" should have type "alphanumeric"
