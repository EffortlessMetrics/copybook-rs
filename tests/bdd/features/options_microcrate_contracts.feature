@options
Feature: Options Microcrate Contracts
  Validate configuration option behavior through the codec facade.

  Scenario: Decode options builder applies contract fields
    Given default decode options
    When decode record format is set to "rdw"
    And decode json number mode is set to "native"
    And decode raw mode is set to "record+rdw"
    Then decode options format should be "rdw"
    And decode options json number mode should be "native"
    And decode options raw mode should be "record+rdw"

  Scenario: Encode options builder applies override and float format
    Given default encode options
    When encode zoned override is set to "ascii"
    And encode float format is set to "ibm-hex"
    Then encode options zoned override should be "ascii"
    And encode options float format should be "ibm-hex"

  Scenario: Zoned detection recognizes nibble signatures
    When zoned encoding is detected from byte "0x35"
    Then detected zoned encoding should be "ascii"
    When zoned encoding is detected from byte "0xF7"
    Then detected zoned encoding should be "ebcdic"
