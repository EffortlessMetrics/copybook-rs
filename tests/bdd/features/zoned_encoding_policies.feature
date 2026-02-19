@zoned-encoding
Feature: Zoned Encoding Policies
  Test zoned decimal encoding format options

  Background:
    Given a copybook with content:
      """
      01 ZONED-RECORD.
         05 ZONED-FIELD PIC S9(5).
      """

  Scenario: Auto zoned encoding with ASCII
    Given ASCII codepage
    And zoned encoding "auto"
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: ASCII zoned encoding
    Given ASCII codepage
    And zoned encoding "ascii"
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: EBCDIC zoned encoding
    Given codepage "CP037"
    And zoned encoding "ebcdic"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Zoned encoding override for encode
    Given ASCII codepage
    And zoned encoding override "ascii"
    And binary data: "12345"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Preserve zoned encoding enabled
    Given ASCII codepage
    And preserve zoned encoding enabled
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Preferred zoned encoding ASCII
    Given ASCII codepage
    And preferred zoned encoding "ascii"
    And JSON data: "{\"ZONED-FIELD\":\"12345\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Preferred zoned encoding EBCDIC
    Given codepage "CP037"
    And preferred zoned encoding "ebcdic"
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Auto encoding with round-trip
    Given ASCII codepage
    And zoned encoding "auto"
    And binary data: "12345"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Override none uses default
    Given ASCII codepage
    And zoned encoding override "none"
    And binary data: "12345"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Zoned encoding with decimal field
    Given a copybook with content:
      """
      01 DEC-ZONED.
         05 DEC-FIELD PIC S9(5)V99.
      """
    And ASCII codepage
    And zoned encoding "auto"
    And binary data: "1234567"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Zoned encoding with unsigned field
    Given a copybook with content:
      """
      01 UNSIGNED-ZONED.
         05 UNSIGNED-FIELD PIC 9(5).
      """
    And ASCII codepage
    And zoned encoding "ascii"
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Zoned encoding output is valid JSON
    Given ASCII codepage
    And zoned encoding "auto"
    And binary data: "00042"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
