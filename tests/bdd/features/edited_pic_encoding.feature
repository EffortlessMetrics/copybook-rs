Feature: Edited PIC E3 Encoding

  As a developer working with COBOL edited PICTURE clauses
  I want to encode numeric values using edited PIC E3 patterns
  So that I can format data with proper zero suppression, signs, and symbols

  # ============================================================================
  # E3.1: Basic Z-editing and Leading Signs
  # ============================================================================

  Scenario: Encode with basic Z-editing (zero suppression)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZZ9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with Z-editing and leading zeros suppressed
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZZ9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"5"},"AMOUNT":"5"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with Z-editing and zero value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZZ9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"0"},"AMOUNT":"0"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with leading plus sign (E3.1)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC +999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with leading plus sign and negative value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC +999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"-123"},"AMOUNT":"-123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with leading minus sign (E3.1)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC -999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with leading minus sign and negative value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC -999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"-123"},"AMOUNT":"-123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  # ============================================================================
  # E3.2: Trailing Signs
  # ============================================================================

  Scenario: Encode with trailing plus sign (E3.2)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999+.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with trailing plus sign and negative value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999+.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"-123"},"AMOUNT":"-123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with trailing minus sign (E3.2)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999-.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with trailing minus sign and negative value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999-.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"-123"},"AMOUNT":"-123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  # ============================================================================
  # E3.3: Credit/Debit Indicators
  # ============================================================================

  Scenario: Encode with CR credit indicator (E3.3)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999CR.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Encode with CR credit indicator and negative value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999CR.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"-123"},"AMOUNT":"-123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Encode with DB debit indicator (E3.3)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999DB.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Encode with DB debit indicator and negative value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999DB.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"-123"},"AMOUNT":"-123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  # ============================================================================
  # E3.4: Comma Separators
  # ============================================================================

  Scenario: Encode with comma separator (E3.4)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999,999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123456"},"AMOUNT":"123456"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 7 bytes

  Scenario: Encode with comma separator and zero suppression
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZZ,ZZ9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"1234"},"AMOUNT":"1234"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 7 bytes

  Scenario: Encode with comma separator and decimal
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZ,ZZ9.99.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"1234.56"},"AMOUNT":"1234.56"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes

  # ============================================================================
  # E3.5: Asterisk Check Protection
  # ============================================================================

  Scenario: Encode with asterisk check protection (E3.5)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ***9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with asterisk check protection and small value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ***9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"5"},"AMOUNT":"5"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with asterisk check protection and zero value
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ***9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"0"},"AMOUNT":"0"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with asterisk check protection and decimal
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC **9.99.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"12.34"},"AMOUNT":"12.34"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes

  # ============================================================================
  # E3.6: Currency Symbols
  # ============================================================================

  Scenario: Encode with currency symbol (E3.6)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC $999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with currency symbol and zero suppression
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC $ZZZ9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"5"},"AMOUNT":"5"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Encode with currency symbol and decimal
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC $ZZ9.99.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"12.34"},"AMOUNT":"12.34"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 7 bytes

  Scenario: Encode with currency symbol and comma separator
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC $Z,ZZ9.99.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"1234.56"},"AMOUNT":"1234.56"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes

  # ============================================================================
  # E3.7: Space Insertion
  # ============================================================================

  Scenario: Encode with space insertion (E3.7)
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 PHONE PIC 999B999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"PHONE":"123456"},"PHONE":"123456"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 7 bytes

  Scenario: Encode with multiple space insertions
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 PHONE PIC 9B9B9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"PHONE":"123"},"PHONE":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Encode with space insertion and zero suppression
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZZB999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123456"},"AMOUNT":"123456"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 7 bytes

  Scenario: Encode with space insertion and decimal
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999B999.99.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123456.78"},"AMOUNT":"123456.78"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes

  # ============================================================================
  # Complex Pattern Combinations
  # ============================================================================

  Scenario: Encode with decimal and zero suppression
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZ9.99.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"12.34"},"AMOUNT":"12.34"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes

  Scenario: Encode with decimal and zero insert
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 00.00.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"12.34"},"AMOUNT":"12.34"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Encode with zero insert pattern
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 0009.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with zero insert and decimal
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 0009.99.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123.45"},"AMOUNT":"123.45"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 7 bytes

  Scenario: Encode with currency, asterisk, and decimal
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC $***9.99.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"12.34"},"AMOUNT":"12.34"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 8 bytes

  Scenario: Encode with leading plus and zero suppression
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC +ZZ9.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 4 bytes

  Scenario: Encode with trailing plus and zero suppression
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZZ9+.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"123"},"AMOUNT":"123"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  # ============================================================================
  # Error Handling
  # ============================================================================

  Scenario: Encode with value overflow should fail
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"1234"},"AMOUNT":"1234"}
      """
    When the JSON data is encoded
    Then encoding should fail

  Scenario: Encode with invalid character in value should fail
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"12a"},"AMOUNT":"12a"}
      """
    When the JSON data is encoded
    Then encoding should fail

  Scenario: Encode with empty value should fail
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":""},"AMOUNT":""}
      """
    When the JSON data is encoded
    Then encoding should fail

  Scenario: Encode with multiple decimal points in value should fail
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999.
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"AMOUNT":"12.34.56"},"AMOUNT":"12.34.56"}
      """
    When the JSON data is encoded
    Then encoding should fail

  # ============================================================================
  # Round-trip Tests
  # ============================================================================

  Scenario: Round-trip edited PIC with Z-editing
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZZ9.
      """
    And ASCII codepage
    And binary data: " 123"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip edited PIC with leading plus
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC +999.
      """
    And ASCII codepage
    And binary data: "+123"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip edited PIC with CR indicator
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999CR.
      """
    And ASCII codepage
    And binary data: "123  "
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip edited PIC with comma separator
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC 999,999.
      """
    And ASCII codepage
    And binary data: "123,456"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Round-trip edited PIC with currency symbol
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC $999.
      """
    And ASCII codepage
    And binary data: "$123"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed
