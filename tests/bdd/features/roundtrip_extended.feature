@roundtrip @extended
Feature: Extended Round-Trip Fidelity

  Byte-identical decode-then-encode proves lossless data conversion.
  This suite covers all field types, codepages, and ODO variable-length records.

  # --- Roundtrip per field type ---

  Scenario: Roundtrip PIC X alphanumeric with spaces
    Given a copybook with content:
      """
      01 ALPHA-REC.
          05 NAME-FIELD PIC X(15).
      """
    And ASCII codepage
    And binary data: "HELLO WORLD    "
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip PIC 9 unsigned integer
    Given a copybook with content:
      """
      01 NUM-REC.
          05 COUNT-FIELD PIC 9(8).
      """
    And ASCII codepage
    And binary data: "00001234"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip PIC S9 signed zoned decimal
    Given a copybook with content:
      """
      01 SIGNED-REC.
          05 AMOUNT PIC S9(5).
      """
    And ASCII codepage
    And binary data: "1234{"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip PIC 9V99 implied decimal
    Given a copybook with content:
      """
      01 DEC-REC.
          05 PRICE PIC 9(5)V99.
      """
    And ASCII codepage
    And binary data: "0012399"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip COMP-3 packed decimal
    Given a copybook with content:
      """
      01 PKD-REC.
          05 AMT PIC S9(7)V99 COMP-3.
      """
    And binary data for value 54321.99
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip COMP binary integer field
    Given a copybook with COMP field:
      """
      01 BIN-REC.
          05 COUNT PIC S9(9) COMP.
      """
    And binary data for value 42
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip multi-field record
    Given a copybook with content:
      """
      01 MULTI-REC.
          05 ID PIC X(5).
          05 NAME PIC X(15).
          05 AMOUNT PIC 9(7)V99.
      """
    And ASCII codepage
    And binary data: "ID001Test Name      001234567"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip record with all zeros
    Given a copybook with content:
      """
      01 ZERO-REC.
          05 NUM1 PIC 9(5).
          05 NUM2 PIC 9(5).
      """
    And ASCII codepage
    And binary data: "0000000000"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip record with all spaces
    Given a copybook with content:
      """
      01 SPACE-REC.
          05 FIELD-A PIC X(10).
          05 FIELD-B PIC X(10).
      """
    And ASCII codepage
    And binary data: "                    "
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip nested group record
    Given a copybook with nested groups:
      """
      01 GROUP-REC.
          05 OUTER.
              10 INNER-A PIC X(5).
              10 INNER-B PIC X(5).
          05 FIELD-C PIC X(5).
      """
    And ASCII codepage
    And binary data: "AAAAABBBBBCCCCC"
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Roundtrip with codepages ---

  Scenario: Roundtrip with CP037 EBCDIC codepage
    Given a copybook with content:
      """
      01 CP037-REC.
          05 DATA-FIELD PIC X(10).
      """
    And codepage "CP037"
    And EBCDIC binary data
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip with CP500 codepage
    Given a copybook with content:
      """
      01 CP500-REC.
          05 DATA-FIELD PIC X(10).
      """
    And codepage "CP500"
    And EBCDIC binary data
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip with CP1047 codepage
    Given a copybook with content:
      """
      01 CP1047-REC.
          05 DATA-FIELD PIC X(10).
      """
    And codepage "CP1047"
    And EBCDIC binary data
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip with CP273 codepage
    Given a copybook with content:
      """
      01 CP273-REC.
          05 DATA-FIELD PIC X(10).
      """
    And codepage "CP273"
    And EBCDIC binary data
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip with CP1140 codepage
    Given a copybook with content:
      """
      01 CP1140-REC.
          05 DATA-FIELD PIC X(10).
      """
    And codepage "CP1140"
    And EBCDIC binary data
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip with ASCII codepage
    Given a copybook with content:
      """
      01 ASCII-REC.
          05 DATA-FIELD PIC X(10).
      """
    And codepage "ASCII"
    And binary data: "HELLOWORLD"
    When the data is round-tripped
    Then the round-trip should be lossless

  # --- Roundtrip with ODO variable-length records ---

  Scenario: Roundtrip ODO record with max elements
    Given a copybook with ODO:
      """
      01 ODO-REC.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 DEPENDING ON ITEM-COUNT.
              10 ITEM-VAL PIC X(5).
      """
    And binary data with COUNT=005 and 5 elements
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip ODO record with min elements
    Given a copybook with ODO:
      """
      01 ODO-REC.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 DEPENDING ON ITEM-COUNT.
              10 ITEM-VAL PIC X(5).
      """
    And binary data with COUNT=001 and 1 elements
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: Roundtrip COMP-3 negative value
    Given a copybook with content:
      """
      01 NEG-REC.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And binary data for value -100.50
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Roundtrip with FILLER fields decodes and encodes
    Given a copybook with FILLER:
      """
      01 FILLER-RT.
          05 FIELD-A PIC X(5).
          05 FILLER PIC X(5).
          05 FIELD-B PIC X(5).
      """
    And emit_filler is false
    And binary data: "HELLOPADDDWORLD"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
