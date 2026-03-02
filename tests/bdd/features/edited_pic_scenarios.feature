@edited-pic @scenarios
Feature: Edited PIC Pattern Scenarios

  As a developer working with COBOL edited PICTURE clauses
  I want comprehensive coverage of all edited PIC patterns
  So that Z-suppression, currency, check-protect, sign editing, and BLANK WHEN ZERO work correctly

  Background:
    Given ASCII codepage

  # --- Z-suppression patterns ---

  Scenario: Parse PIC ZZZ9 zero suppression
    Given a copybook with edited PIC:
      """
      01 REC.
          05 AMT PIC ZZZ9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "AMT" should have type "edited_numeric"

  Scenario: Decode PIC ZZZ9 with value 42
    Given a copybook with edited PIC:
      """
      01 REC.
          05 AMT PIC ZZZ9.
      """
    And binary data: "  42"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode PIC ZZZ9 with value 0
    Given a copybook with edited PIC:
      """
      01 REC.
          05 AMT PIC ZZZ9.
      """
    And binary data: "   0"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode PIC ZZZZ9 with max value
    Given a copybook with edited PIC:
      """
      01 REC.
          05 AMT PIC ZZZZ9.
      """
    And binary data: "99999"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode PIC ZZ9.99 with decimal
    Given a copybook with edited PIC:
      """
      01 REC.
          05 AMT PIC ZZ9.99.
      """
    And binary data: " 12.34"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Currency patterns ---

  Scenario: Parse PIC $ZZ,ZZZ.99 currency pattern
    Given a copybook with edited PIC:
      """
      01 REC.
          05 PRICE PIC $ZZ,ZZZ.99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "PRICE" should have type "edited_numeric"

  Scenario: Decode PIC $ZZZ.99 small currency
    Given a copybook with edited PIC:
      """
      01 REC.
          05 PRICE PIC $ZZZ.99.
      """
    And binary data: "$  1.50"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Sign editing patterns ---

  Scenario: Parse PIC +ZZZ9 positive sign editing
    Given a copybook with edited PIC:
      """
      01 REC.
          05 BAL PIC +ZZZ9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "BAL" should have type "edited_numeric"

  Scenario: Parse PIC -ZZZ9 negative sign editing
    Given a copybook with edited PIC:
      """
      01 REC.
          05 BAL PIC -ZZZ9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "BAL" should have type "edited_numeric"

  Scenario: Decode PIC +ZZZ9 positive value
    Given a copybook with edited PIC:
      """
      01 REC.
          05 BAL PIC +ZZZ9.
      """
    And binary data: "+ 123"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode PIC -ZZZ9 negative value
    Given a copybook with edited PIC:
      """
      01 REC.
          05 BAL PIC -ZZZ9.
      """
    And binary data: "-  50"
    When the binary data is decoded
    Then decoding should succeed

  # --- Check protect (asterisk) ---

  Scenario: Parse PIC ***9 check protect
    Given a copybook with edited PIC:
      """
      01 REC.
          05 CHECK-AMT PIC ***9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "CHECK-AMT" should have type "edited_numeric"

  Scenario: Decode PIC ***9 check protect with value
    Given a copybook with edited PIC:
      """
      01 REC.
          05 CHECK-AMT PIC ***9.
      """
    And binary data: "**42"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode PIC **,**9.99 check protect with commas
    Given a copybook with edited PIC:
      """
      01 REC.
          05 CHECK-AMT PIC **,**9.99.
      """
    And binary data: "** **1.23"
    When the binary data is decoded
    Then decoding should succeed

  # --- BLANK WHEN ZERO ---

  Scenario: Parse field with BLANK WHEN ZERO
    Given a copybook with BLANK WHEN ZERO:
      """
      01 REC.
          05 AMT PIC ZZZ9 BLANK WHEN ZERO.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field "AMT" should have blank_when_zero true

  Scenario: Decode BLANK WHEN ZERO with zero value
    Given a copybook with BLANK WHEN ZERO:
      """
      01 REC.
          05 AMT PIC ZZZ9 BLANK WHEN ZERO.
      """
    And binary data: "    "
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode BLANK WHEN ZERO with non-zero value
    Given a copybook with BLANK WHEN ZERO:
      """
      01 REC.
          05 AMT PIC ZZZ9 BLANK WHEN ZERO.
      """
    And binary data: "  42"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Multiple edited PIC fields ---

  Scenario: Parse record with multiple edited PIC patterns
    Given a copybook with multiple edited PICs:
      """
      01 MULTI-EDIT-REC.
          05 PRICE PIC $ZZZ.99.
          05 QTY PIC ZZZ9.
          05 DISCOUNT PIC ZZ9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed

  Scenario: Decode record with multiple edited PIC fields
    Given a copybook with multiple edited PICs:
      """
      01 MULTI-EDIT-REC.
          05 PRICE PIC $ZZZ.99.
          05 QTY PIC ZZZ9.
      """
    And binary data: "$ 99.99 100"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
