@codepage
Feature: Codepage Variants
  Test encoding/decoding with all supported EBCDIC codepages

  Background:
    Given a copybook with content:
      """
      01 CODEPAGE-RECORD.
         05 TEXT-FIELD PIC X(10).
         05 NUM-FIELD PIC 9(5).
      """

  Scenario: Decode with CP037 codepage
    Given codepage "CP037"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode with CP273 codepage
    Given codepage "CP273"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode with CP500 codepage
    Given codepage "CP500"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode with CP1047 codepage
    Given codepage "CP1047"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode with CP1140 codepage
    Given codepage "CP1140"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Encode with CP037 codepage
    Given codepage "CP037"
    And JSON data: "{\"TEXT-FIELD\":\"HELLO\",\"NUM-FIELD\":\"12345\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Encode with CP273 codepage
    Given codepage "CP273"
    And JSON data: "{\"TEXT-FIELD\":\"HELLO\",\"NUM-FIELD\":\"12345\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Encode with CP500 codepage
    Given codepage "CP500"
    And JSON data: "{\"TEXT-FIELD\":\"HELLO\",\"NUM-FIELD\":\"12345\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Encode with CP1047 codepage
    Given codepage "CP1047"
    And JSON data: "{\"TEXT-FIELD\":\"HELLO\",\"NUM-FIELD\":\"12345\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Encode with CP1140 codepage
    Given codepage "CP1140"
    And JSON data: "{\"TEXT-FIELD\":\"HELLO\",\"NUM-FIELD\":\"12345\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Round-trip with CP037
    Given codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip with CP500
    Given codepage "CP500"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip with CP1047
    Given codepage "CP1047"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Round-trip with CP1140
    Given codepage "CP1140"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Decode COMP-3 with CP037 codepage
    Given a copybook with content:
      """
      01 COMP3-RECORD.
         05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And codepage "CP037"
    And binary data: "\x00\x00\x12\x34\x5C"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: ASCII codepage decode
    Given ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: ASCII codepage encode
    Given ASCII codepage
    And JSON data: "{\"TEXT-FIELD\":\"HELLO\",\"NUM-FIELD\":\"12345\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: ASCII round-trip is lossless
    Given ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless
