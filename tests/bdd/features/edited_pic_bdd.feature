@edited-pic
Feature: Edited PIC BDD Coverage

  As a developer working with COBOL edited PICTURE clauses
  I want comprehensive BDD coverage of edited PIC decode, encode, and round-trip
  So that zero-suppression, currency, check-protect, and sign-editing all work correctly

  Background:
    Given ASCII codepage

  # --- Zero-suppressed fields ---

  Scenario: Decode zero-suppressed field PIC ZZZ9
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ZZZ9.
      """
    And binary data: " 123"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode zero-suppressed field with all leading spaces
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ZZZ9.
      """
    And binary data: "   0"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode zero-suppressed field with decimal PIC ZZZ9.99
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ZZZ9.99.
      """
    And binary data: "  12.34"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Currency-edited fields ---

  Scenario: Decode currency-edited field PIC $ZZ,ZZZ.99
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC $ZZ,ZZZ.99.
      """
    And binary data: "$ 1,234.56"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode currency-edited field with small value
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC $ZZ,ZZZ.99.
      """
    And binary data: "$     5.00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Check-protect fields ---

  Scenario: Decode check-protect field PIC ***9.99
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ***9.99.
      """
    And binary data: "**12.34"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode check-protect field with zero value
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ***9.99.
      """
    And binary data: "***0.00"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Sign-edited fields ---

  Scenario: Decode leading-plus sign-edited field positive value
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC +ZZZ9.
      """
    And binary data: "+ 123"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode leading-minus sign-edited field negative value
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC -ZZZ9.
      """
    And binary data: "-  42"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode leading-minus sign-edited field positive value
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC -ZZZ9.
      """
    And binary data: "  123"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- BLANK WHEN ZERO ---

  Scenario: Decode BLANK WHEN ZERO field with zero value
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ZZZ9 BLANK WHEN ZERO.
      """
    And binary data: "    "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode BLANK WHEN ZERO field with non-zero value
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ZZZ9 BLANK WHEN ZERO.
      """
    And binary data: "  42"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Encode round-trip for edited PIC ---

  Scenario: Round-trip edited PIC with zero suppression
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ZZZ9.
      """
    And binary data: " 123"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip edited PIC with currency and comma
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC $999.
      """
    And binary data: "$123"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip edited PIC with check-protect
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC ***9.
      """
    And binary data: "*123"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip edited PIC with leading sign
    Given a copybook with edited PIC:
      """
      01 EDITED-REC.
         05 AMOUNT PIC +999.
      """
    And binary data: "+123"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed
