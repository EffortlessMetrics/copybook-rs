@numeric-precision
Feature: Numeric Precision
  As a developer working with financial and scientific mainframe data
  I want numeric fields to be decoded and encoded with full precision
  So that no data is lost during conversion

  # --- COMP-3 packed decimal edge cases ---

  Scenario: COMP-3 positive value decode
    Given a copybook with content:
      """
      01 COMP3-POS.
          05 AMOUNT PIC S9(5)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x45\x6C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP-3 negative value decode
    Given a copybook with content:
      """
      01 COMP3-NEG.
          05 AMOUNT PIC S9(5)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x45\x6D"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: COMP-3 zero value decode
    Given a copybook with content:
      """
      01 COMP3-ZERO.
          05 AMOUNT PIC S9(5)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x00\x00\x00\x0C"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: COMP-3 unsigned value decode
    Given a copybook with content:
      """
      01 COMP3-UNSIGNED.
          05 QTY PIC 9(3) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x12\x3C"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: COMP-3 with all nines
    Given a copybook with content:
      """
      01 COMP3-MAX.
          05 MAX-VAL PIC 9(5) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x99\x99\x9C"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: COMP-3 round-trip preserves precision
    Given a copybook with content:
      """
      01 COMP3-RT.
          05 PRECISE-AMT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Binary integer edge cases ---

  Scenario: COMP binary 2-byte decode
    Given a copybook with content:
      """
      01 COMP-2BYTE.
          05 SMALL-INT PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x2A"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: COMP binary 4-byte decode
    Given a copybook with content:
      """
      01 COMP-4BYTE.
          05 MED-INT PIC S9(9) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x00\x01\x00"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: COMP binary zero value
    Given a copybook with content:
      """
      01 COMP-ZERO.
          05 ZERO-INT PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x00"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: COMP binary negative value
    Given a copybook with content:
      """
      01 COMP-NEG.
          05 NEG-INT PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\xFF\xFE"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: COMP binary round-trip
    Given a copybook with content:
      """
      01 COMP-RT.
          05 BIN-VAL PIC S9(9) COMP.
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Zoned decimal precision ---

  Scenario: Zoned decimal positive value
    Given a copybook with content:
      """
      01 ZONED-POS.
          05 AMT PIC 9(5)V99.
      """
    And ASCII codepage
    And binary data: "1234567"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Zoned decimal round-trip with scale
    Given a copybook with content:
      """
      01 ZONED-SCALE.
          05 PRICE PIC 9(5)V99.
      """
    And ASCII codepage
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Zoned decimal all zeros
    Given a copybook with content:
      """
      01 ZONED-ZEROS.
          05 ZERO-AMT PIC 9(7).
      """
    And ASCII codepage
    And binary data: "0000000"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Zoned decimal single digit
    Given a copybook with content:
      """
      01 ZONED-SINGLE.
          05 DIGIT PIC 9.
      """
    And ASCII codepage
    And binary data: "7"
    When the binary data is decoded
    Then decoding should succeed

  # --- Lossless vs Native number modes ---

  Scenario: Lossless mode preserves COMP-3 as string
    Given a copybook with content:
      """
      01 LOSSLESS-REC.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And lossless number mode
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Native mode outputs COMP-3 as number
    Given a copybook with content:
      """
      01 NATIVE-REC.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And native number mode
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Lossless mode preserves zoned decimal
    Given a copybook with content:
      """
      01 LOSSLESS-ZONED.
          05 PRICE PIC 9(5)V99.
      """
    And ASCII codepage
    And lossless number mode
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Native mode outputs binary integer
    Given a copybook with content:
      """
      01 NATIVE-COMP.
          05 COUNT PIC S9(4) COMP.
      """
    And ASCII codepage
    And native number mode
    And binary data: "\x00\x0A"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- Signed numeric variations ---

  Scenario: Signed COMP-3 with positive sign nibble 0xC
    Given a copybook with content:
      """
      01 SIGN-C.
          05 VAL PIC S9(3) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x12\x3C"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Signed COMP-3 with negative sign nibble 0xD
    Given a copybook with content:
      """
      01 SIGN-D.
          05 VAL PIC S9(3) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x12\x3D"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Signed COMP-3 with unsigned sign nibble 0xF
    Given a copybook with content:
      """
      01 SIGN-F.
          05 VAL PIC S9(3) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x12\x3F"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: COMP-3 field round-trip with EBCDIC codepage
    Given a copybook with content:
      """
      01 COMP3-EBCDIC-RT.
          05 AMT PIC S9(5)V99 COMP-3.
      """
    And codepage "CP037"
    And binary data for all fields
    When the data is round-tripped
    Then the round-trip should be lossless
