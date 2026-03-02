@encode-decode-extended
Feature: Encode Decode Edge Cases Extended
  As a developer encoding and decoding mainframe data
  I want edge cases in encode/decode to work correctly
  So that data conversions are reliable

  Scenario: Encode alphanumeric field with exact length
    Given a copybook with content:
      """
      01 EXACT-LEN.
          05 EXACT-FIELD PIC X(5).
      """
    And ASCII codepage
    And JSON data:
      """
      {"EXACT-FIELD":"ABCDE"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 5 bytes

  Scenario: Decode alphanumeric field padded with spaces
    Given a copybook with content:
      """
      01 PAD-DEC.
          05 PAD-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "AB        "
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Encode numeric field with leading zeros
    Given a copybook with content:
      """
      01 LEAD-ZERO.
          05 NUM-FIELD PIC 9(6).
      """
    And ASCII codepage
    And JSON data:
      """
      {"NUM-FIELD":"000042"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 6 bytes

  Scenario: Decode numeric field produces string in lossless mode
    Given a copybook with content:
      """
      01 LOSSLESS-DEC.
          05 AMOUNT PIC 9(5)V99.
      """
    And ASCII codepage
    And lossless number mode
    And binary data: "1234567"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode and verify output contains field name
    Given a copybook with content:
      """
      01 FIELD-NAME-REC.
          05 MY-FIELD PIC X(8).
      """
    And ASCII codepage
    And binary data: "TESTDATA"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "MY-FIELD"

  Scenario: Encode with numeric coercion
    Given a copybook with content:
      """
      01 COERCE-REC.
          05 NUM PIC 9(4).
      """
    And ASCII codepage
    And JSON data:
      """
      {"NUM":42}
      """
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Round-trip alphanumeric field
    Given a copybook with content:
      """
      01 RT-ALPHA.
          05 TEXT PIC X(8).
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip numeric field
    Given a copybook with content:
      """
      01 RT-NUM.
          05 NUM PIC 9(6).
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip COMP-3 field
    Given a copybook with content:
      """
      01 RT-COMP3.
          05 AMT PIC S9(5)V99 COMP-3.
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip COMP binary field
    Given a copybook with content:
      """
      01 RT-COMP.
          05 BIN PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Decode two-field record
    Given a copybook with content:
      """
      01 TWO-FIELD.
          05 FIRST PIC X(3).
          05 SECOND PIC X(3).
      """
    And ASCII codepage
    And binary data: "ABCDEF"
    When the binary data is decoded
    Then decoding should succeed
    And decoded field FIRST should be "ABC"
    And decoded field SECOND should be "DEF"

  Scenario: Encode two-field record
    Given a copybook with content:
      """
      01 TWO-ENC.
          05 FA PIC X(4).
          05 FB PIC X(4).
      """
    And ASCII codepage
    And JSON data:
      """
      {"FA":"AAAA","FB":"BBBB"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 8 bytes

  Scenario: Decode COMP-3 and verify JSON output
    Given a copybook with content:
      """
      01 COMP3-JSON.
          05 VAL PIC S9(3) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x12\x3C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode binary integer and verify JSON output
    Given a copybook with content:
      """
      01 COMP-JSON.
          05 CNT PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x05"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Encode short string for alphanumeric field pads to length
    Given a copybook with content:
      """
      01 SHORT-ENC.
          05 SHORT-FIELD PIC X(10).
      """
    And ASCII codepage
    And JSON data:
      """
      {"SHORT-FIELD":"AB"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes

  Scenario: Decode record with mixed field types
    Given a copybook with content:
      """
      01 MIXED-DEC.
          05 ID PIC 9(4).
          05 NAME PIC X(10).
          05 AMT PIC 9(5)V99.
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And all fields should be decoded

  Scenario: Verify data with mixed fields
    Given a copybook with content:
      """
      01 VERIFY-MIX.
          05 CODE PIC X(3).
          05 QTY PIC 9(4).
      """
    And ASCII codepage
    And binary data: "ABC1234"
    When the data is verified
    Then verification should succeed

  Scenario: Decode with native number mode for integer
    Given a copybook with content:
      """
      01 NATIVE-INT.
          05 COUNT-F PIC 9(4).
      """
    And ASCII codepage
    And native number mode
    And binary data: "0042"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Round-trip mixed alphanumeric and packed decimal
    Given a copybook with content:
      """
      01 MIX-RT.
          05 LABEL PIC X(8).
          05 VALUE-F PIC S9(5)V99 COMP-3.
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip EBCDIC alphanumeric field
    Given a copybook with content:
      """
      01 EBCDIC-RT.
          05 TEXT PIC X(10).
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless
