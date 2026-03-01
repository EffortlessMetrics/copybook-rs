@error-strategies
Feature: Error Handling Strategies
  Test strict/lenient modes and max_errors

  Background:
    Given a copybook with content:
      """
      01 ERROR-RECORD.
         05 DATA-FIELD PIC X(10).
      """

  Scenario: Strict mode with short PIC X data succeeds after padding
    Given ASCII codepage
    And strict mode
    And binary data: "AB"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Lenient mode handles short data
    Given ASCII codepage
    And lenient mode
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Max errors limits error accumulation
    Given ASCII codepage
    And max errors 5
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Max errors unlimited allows all errors
    Given ASCII codepage
    And max errors unlimited
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Strict mode with valid data succeeds
    Given ASCII codepage
    And strict mode
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Lenient mode with valid data succeeds
    Given ASCII codepage
    And lenient mode
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Max errors with encoding
    Given ASCII codepage
    And max errors 3
    And JSON data: "{\"DATA-FIELD\":\"TESTDATA01\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Strict mode encoding with valid JSON
    Given ASCII codepage
    And strict mode
    And JSON data: "{\"DATA-FIELD\":\"TESTDATA01\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Lenient mode encoding
    Given ASCII codepage
    And lenient mode
    And JSON data: "{\"DATA-FIELD\":\"TESTDATA01\"}"
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Max errors with round-trip
    Given ASCII codepage
    And max errors 10
    And binary data: "HELLOWORLD"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Strict mode with numeric field
    Given a copybook with content:
      """
      01 STRICT-NUM.
         05 NUM-FIELD PIC 9(5).
      """
    And ASCII codepage
    And strict mode
    And binary data: "12345"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Max errors set to 1
    Given ASCII codepage
    And max errors 1
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
