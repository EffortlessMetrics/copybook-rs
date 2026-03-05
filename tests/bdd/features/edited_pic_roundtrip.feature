@edited-pic-roundtrip
Feature: Edited PIC Round-Trip and Extended Pattern Coverage

  As a developer working with COBOL edited PICTURE clauses
  I want complete round-trip coverage for edited PIC patterns
  So that Z-suppression, currency, sign, and check-protect patterns encode and decode correctly

  Background:
    Given ASCII codepage

  # --- Z-suppression round-trip ---

  Scenario: Round-trip PIC ZZZ9 with non-zero value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9.
      """
    And binary data: " 123"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip PIC ZZZ9 with small value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9.
      """
    And binary data: "   5"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip PIC ZZZ9 zero shows single zero
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZ9.
      """
    And binary data: "   0"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip PIC ZZZZ9 five-digit value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZZ9.
      """
    And binary data: " 9876"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Currency round-trip ---

  Scenario: Round-trip PIC $999 with full value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $999.
      """
    And binary data: "$456"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip PIC $9999 with large value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $9999.
      """
    And binary data: "$1234"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Sign-edited round-trip ---

  Scenario: Round-trip PIC +9999 positive
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC +9999.
      """
    And binary data: "+5000"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip PIC -9999 negative
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC -9999.
      """
    And binary data: "-1234"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip PIC -9999 positive shows space
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC -9999.
      """
    And binary data: " 1234"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Check-protect round-trip ---

  Scenario: Round-trip PIC ***9 with value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ***9.
      """
    And binary data: "**42"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip PIC ****9 five-digit check protect
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ****9.
      """
    And binary data: "***99"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- Schema type verification for various edited patterns ---

  Scenario: Schema recognizes PIC ZZZZ9 as edited
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZZ9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "edited"
    And the field "AMT" should be 5 bytes long

  Scenario: Schema recognizes PIC $9999 as edited
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $9999.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "edited"
    And the field "AMT" should be 5 bytes long

  Scenario: Schema recognizes PIC -ZZZ9 as edited
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC -ZZZ9.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "edited"
    And the field "AMT" should be 5 bytes long

  Scenario: Schema recognizes PIC +ZZZ9.99 as edited
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC +ZZZ9.99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "edited"
    And the field "AMT" should be 8 bytes long

  Scenario: Schema recognizes PIC ***9.99 as edited
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ***9.99.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "edited"
    And the field "AMT" should be 7 bytes long

  # --- Decode various edited patterns ---

  Scenario: Decode PIC ZZZZ9.99 with value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC ZZZZ9.99.
      """
    And binary data: "  123.45"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "AMT"

  Scenario: Decode PIC $9999 with full value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC $9999.
      """
    And binary data: "$5678"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode PIC -ZZZ9 negative value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC -ZZZ9.
      """
    And binary data: "-  42"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode PIC +9999 positive value
    Given a copybook with edited PIC:
      """
      01 REC.
         05 AMT PIC +9999.
      """
    And binary data: "+1234"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
