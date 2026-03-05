@edited-pic
Feature: Edited PIC Decode, Encode, Round-Trip, and Schema

  As a developer working with COBOL edited PICTURE clauses
  I want comprehensive BDD coverage for edited PIC patterns
  So that Z-suppression, currency, check-protect, sign-editing, and BLANK WHEN ZERO all work correctly

  Background:
    Given ASCII codepage

  # ---------------------------------------------------------------------------
  # Decode: Z-suppressed fields
  # ---------------------------------------------------------------------------

  Scenario: Decode Z-suppressed field PIC ZZZ9 with leading spaces
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9.
      """
    And binary data: " 456"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode Z-suppressed field PIC ZZZ9 all-zero
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9.
      """
    And binary data: "   0"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # ---------------------------------------------------------------------------
  # Decode: currency field
  # ---------------------------------------------------------------------------

  Scenario: Decode currency field PIC $ZZ,ZZZ.99
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $ZZ,ZZZ.99.
      """
    And binary data: "$ 2,500.75"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode currency field PIC $ZZ,ZZZ.99 small value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $ZZ,ZZZ.99.
      """
    And binary data: "$     0.01"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # ---------------------------------------------------------------------------
  # Decode: sign-edited fields
  # ---------------------------------------------------------------------------

  Scenario: Decode leading-plus sign-edited field +ZZZ9 positive
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC +ZZZ9.
      """
    And binary data: "+ 500"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode leading-minus sign-edited field -ZZZ9 negative
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC -ZZZ9.
      """
    And binary data: "-  75"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode leading-minus sign-edited field -ZZZ9 positive (space)
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC -ZZZ9.
      """
    And binary data: "  100"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # ---------------------------------------------------------------------------
  # Decode: asterisk check-protect
  # ---------------------------------------------------------------------------

  Scenario: Decode check-protect field PIC ***9
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ***9.
      """
    And binary data: "**99"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode check-protect PIC ***9.99 with decimal
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ***9.99.
      """
    And binary data: "**50.25"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # ---------------------------------------------------------------------------
  # BLANK WHEN ZERO
  # ---------------------------------------------------------------------------

  Scenario: BLANK WHEN ZERO all-blank for zero value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9 BLANK WHEN ZERO.
      """
    And binary data: "    "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: BLANK WHEN ZERO non-zero value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9 BLANK WHEN ZERO.
      """
    And binary data: "  77"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # ---------------------------------------------------------------------------
  # Encode: Z-suppressed
  # ---------------------------------------------------------------------------

  Scenario: Encode to Z-suppressed PIC ZZZ9
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9.
      """
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMT":"42"},"AMT":"42"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  # ---------------------------------------------------------------------------
  # Encode: currency format
  # ---------------------------------------------------------------------------

  Scenario: Encode to currency format PIC $999
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $999.
      """
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMT":"250"},"AMT":"250"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  # ---------------------------------------------------------------------------
  # Roundtrip: Z-suppression
  # ---------------------------------------------------------------------------

  Scenario: Roundtrip Z-suppression PIC ZZZ9
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9.
      """
    And binary data: " 789"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  # ---------------------------------------------------------------------------
  # Roundtrip: currency
  # ---------------------------------------------------------------------------

  Scenario: Roundtrip currency PIC $999
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $999.
      """
    And binary data: "$456"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  # ---------------------------------------------------------------------------
  # Schema: EditedNumeric recognition
  # ---------------------------------------------------------------------------

  Scenario: Schema recognises EditedNumeric kind for PIC ZZZ9
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "edited"

  Scenario: Schema recognises EditedNumeric kind for PIC $ZZ9.99
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $ZZ9.99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "edited"
    And the field "AMT" should be 7 bytes long

  Scenario: Schema recognises EditedNumeric kind for PIC +ZZZ9
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC +ZZZ9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "edited"
    And the field "AMT" should be 5 bytes long
