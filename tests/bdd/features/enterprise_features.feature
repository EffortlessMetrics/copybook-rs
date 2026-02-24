Feature: Enterprise COBOL Features

  As a developer working with enterprise COBOL data
  I want to process complex copybook structures
  So that I can handle real-world mainframe data formats

  Scenario: Parse copybook with COMP-3 packed decimal
    Given a copybook with COMP-3 field:
      """
      01 PACKED-RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "AMOUNT" should have type "packed_decimal"

  Scenario: Parse copybook with COMP binary integer
    Given a copybook with COMP field:
      """
      01 BINARY-RECORD.
          05 COUNT PIC S9(9) COMP.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "COUNT" should have type "binary_int"

  Scenario: Parse copybook with COMP-4 binary integer
    Given a copybook with COMP-4 field:
      """
      01 COMP4-RECORD.
          05 VALUE PIC S9(9) COMP-4.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "VALUE" should have type "binary_int"

  Scenario: Parse copybook with COMP-5 binary integer
    Given a copybook with COMP-5 field:
      """
      01 COMP5-RECORD.
          05 VALUE PIC S9(9) COMP-5.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "VALUE" should have type "binary_int"

  Scenario: Parse copybook with SIGN SEPARATE LEADING
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 SIGN-LEADING-RECORD.
          05 AMOUNT PIC S9(5) SIGN SEPARATE LEADING.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "AMOUNT" should have sign separate placement "leading"

  Scenario: Parse copybook with SIGN SEPARATE TRAILING
    Given a copybook with SIGN SEPARATE TRAILING:
      """
      01 SIGN-TRAILING-RECORD.
          05 AMOUNT PIC S9(5) SIGN SEPARATE TRAILING.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "AMOUNT" should have sign separate placement "trailing"

  Scenario: Parse copybook with BLANK WHEN ZERO
    Given a copybook with BLANK WHEN ZERO:
      """
      01 BLANK-WHEN-ZERO-RECORD.
          05 AMOUNT PIC S9(5) BLANK WHEN ZERO.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "AMOUNT" should have blank_when_zero true

  Scenario: Parse copybook with SYNCHRONIZED
    Given a copybook with SYNCHRONIZED:
      """
      01 SYNCHRONIZED-RECORD.
          05 FIELD1 PIC X(5) SYNCHRONIZED.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "FIELD1" should have synchronized true

  Scenario: Parse copybook with RENAMES
    Given a copybook with RENAMES:
      """
      01 RENAMES-RECORD.
          05 FIELD1 PIC X(5).
          05 FIELD2 PIC X(5).
          05 FIELD3 PIC X(5).
          66 ALIAS RENAMES FIELD1 THRU FIELD3.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "ALIAS" should have level 66

  Scenario: Parse copybook with multiple RENAMES
    Given a copybook with multiple RENAMES:
      """
      01 MULTIPLE-RENAMES.
          05 FIELD1 PIC X(5).
          05 FIELD2 PIC X(5).
          05 FIELD3 PIC X(5).
          05 FIELD4 PIC X(5).
          66 ALIAS1 RENAMES FIELD1 THRU FIELD2.
          66 ALIAS2 RENAMES FIELD3 THRU FIELD4.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And there should be 2 level-66 fields

  Scenario: Parse copybook with OCCURS DEPENDING ON (ODO)
    Given a copybook with ODO:
      """
      01 ODO-RECORD.
          05 COUNT PIC 9(3).
          05 ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNT.
              10 ELEMENT PIC X(5).
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "ARRAY" should have ODO with counter "COUNT"

  Scenario: Parse copybook with nested ODO
    Given a copybook with nested ODO:
      """
      01 NESTED-ODO.
          05 OUTER-COUNT PIC 9(3).
          05 OUTER-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON OUTER-COUNT.
              10 INNER-COUNT PIC 9(3).
              10 INNER-ARRAY OCCURS 1 TO 3 TIMES DEPENDING ON INNER-COUNT.
                  15 ELEMENT PIC X(5).
      """
    When copybook is parsed
    Then schema should be successfully parsed

  Scenario: Parse copybook with Level-88 condition
    Given a copybook with Level-88:
      """
      01 LEVEL88-RECORD.
          05 STATUS-CODE PIC X(1).
              88 ACTIVE VALUE 'A'.
              88 INACTIVE VALUE 'I'.
              88 PENDING VALUE 'P'.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "ACTIVE" should have level 88
    And field "INACTIVE" should have level 88
    And field "PENDING" should have level 88

  Scenario: Parse copybook with Level-88 VALUE THROUGH
    Given a copybook with Level-88 VALUE THROUGH:
      """
      01 LEVEL88-THROUGH.
          05 PRIORITY PIC 9(1).
              88 HIGH VALUE 8 THROUGH 9.
              88 MEDIUM VALUE 5 THROUGH 7.
              88 LOW VALUE 0 THROUGH 4.
      """
    When copybook is parsed
    Then schema should be successfully parsed

  Scenario: Parse copybook with edited numeric PIC
    Given a copybook with edited PIC:
      """
      01 EDITED-RECORD.
          05 AMOUNT PIC ZZ,ZZ9.99CR.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "AMOUNT" should have type "edited_numeric"

  Scenario: Parse copybook with multiple edited PIC formats
    Given a copybook with multiple edited PICs:
      """
      01 MULTIPLE-EDITED.
          05 FIELD1 PIC ZZZ9.
          05 FIELD2 PIC $,$$$,$$9.99.
          05 FIELD3 PIC +++9.
      """
    When copybook is parsed
    Then schema should be successfully parsed

  Scenario: Decode COMP-3 packed decimal data
    Given a copybook with COMP-3 field:
      """
      01 PACKED-RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And binary data for value 12345.67
    When binary data is decoded
    Then decoded value should be "12345.67"

  Scenario: Reject corrupted COMP-3 packed decimal data
    Given a copybook with COMP-3 field:
      """
      01 PACKED-RECORD.
          05 AMOUNT PIC S9(4) COMP-3.
      """
    And strict mode
    And binary data: "\xA2\x34\x5C"
    When the binary data is decoded
    Then decoding should fail
    And error code should be "CBKD401_COMP3_INVALID_NIBBLE"

  Scenario: Encode COMP-3 packed decimal data
    Given a copybook with COMP-3 field:
      """
      01 PACKED-RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
      """
    And ASCII codepage
    And JSON data:
      """
      {"AMOUNT":"12345.67"}
      """
    When JSON data is encoded
    Then encoding should succeed
    And encoded length should be 4 bytes

  Scenario: Decode binary integer data
    Given a copybook with COMP field:
      """
      01 BINARY-RECORD.
          05 COUNT PIC S9(9) COMP.
      """
    And ASCII codepage
    And binary data for value 123456789
    When binary data is decoded
    Then decoded value should be "123456789"

  Scenario: Encode binary integer data
    Given a copybook with COMP field:
      """
      01 BINARY-RECORD.
          05 COUNT PIC S9(9) COMP.
      """
    And ASCII codepage
    And JSON data:
      """
      {"COUNT":"123456789"}
      """
    When JSON data is encoded
    Then encoding should succeed
    And encoded length should be 4 bytes

  Scenario: Decode SIGN SEPARATE data
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 SIGN-LEADING-RECORD.
          05 AMOUNT PIC S9(5) SIGN SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data for value -12345
    When binary data is decoded
    Then decoded value should be "-12345"

  Scenario: Encode SIGN SEPARATE data
    Given a copybook with SIGN SEPARATE LEADING:
      """
      01 SIGN-LEADING-RECORD.
          05 AMOUNT PIC S9(5) SIGN SEPARATE LEADING.
      """
    And ASCII codepage
    And JSON data:
      """
      {"AMOUNT":"-12345"}
      """
    When JSON data is encoded
    Then encoding should succeed

  Scenario: Decode ODO array data
    Given a copybook with ODO:
      """
      01 ODO-RECORD.
          05 COUNT PIC 9(3).
          05 ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNT.
              10 ELEMENT PIC X(5).
      """
    And ASCII codepage
    And binary data with COUNT=3 and 3 elements
    When binary data is decoded
    Then decoded COUNT should be "003"
    And there should be 3 ARRAY elements

  Scenario: Encode ODO array data
    Given a copybook with ODO:
      """
      01 ODO-RECORD.
          05 COUNT PIC 9(3).
          05 ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNT.
              10 ELEMENT PIC X(5).
      """
    And ASCII codepage
    And JSON data:
      """
      {"COUNT":"003","ARRAY":[{"ELEMENT":"AAAAA"},{"ELEMENT":"BBBBB"},{"ELEMENT":"CCCCC"}]}
      """
    When JSON data is encoded
    Then encoding should succeed

  Scenario: Parse complex nested structure
    Given a copybook with nested groups:
      """
      01 COMPLEX-NESTED.
          05 GROUP1.
              10 FIELD1 PIC X(10).
              10 GROUP2.
                  15 FIELD2 PIC 9(5).
                  15 FIELD3 PIC X(15).
          05 GROUP3.
              10 FIELD4 PIC S9(7)V99 COMP-3.
              10 FIELD5 PIC S9(5) SIGN SEPARATE TRAILING.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And there should be 5 leaf fields

  Scenario: Decode complex nested structure
    Given a copybook with nested groups:
      """
      01 COMPLEX-NESTED.
          05 GROUP1.
              10 FIELD1 PIC X(10).
              10 GROUP2.
                  15 FIELD2 PIC 9(5).
                  15 FIELD3 PIC X(15).
          05 GROUP3.
              10 FIELD4 PIC S9(7)V99 COMP-3.
              10 FIELD5 PIC S9(5) SIGN SEPARATE TRAILING.
      """
    And ASCII codepage
    And binary data for all fields
    When binary data is decoded
    Then all fields should be decoded
    And FIELD1 should be "ABCDEFGHIJ"
    And FIELD2 should be "12345"
    And FIELD3 should be "LMNOPQRSTUVWXYZ"
    And FIELD4 should be "12345.67"
    And FIELD5 should be "-12345"

  Scenario: Encode complex nested structure
    Given a copybook with nested groups:
      """
      01 COMPLEX-NESTED.
          05 GROUP1.
              10 FIELD1 PIC X(10).
              10 GROUP2.
                  15 FIELD2 PIC 9(5).
                  15 FIELD3 PIC X(15).
          05 GROUP3.
              10 FIELD4 PIC S9(7)V99 COMP-3.
              10 FIELD5 PIC S9(5) SIGN SEPARATE TRAILING.
      """
    And ASCII codepage
    And JSON data:
      """
      {"FIELD1":"ABCDEFGHIJ","FIELD2":"12345","FIELD3":"LMNOPQRSTUVWXYZ","FIELD4":"12345.67","FIELD5":"-12345"}
      """
    When JSON data is encoded
    Then encoding should succeed

  Scenario: Handle BLANK WHEN ZERO on decode
    Given a copybook with BLANK WHEN ZERO:
      """
      01 BLANK-WHEN-ZERO-RECORD.
          05 AMOUNT PIC S9(5) BLANK WHEN ZERO.
      """
    And ASCII codepage
    And binary data with zero value
    When binary data is decoded
    Then decoded AMOUNT should be blank

  Scenario: Handle BLANK WHEN ZERO on encode
    Given a copybook with BLANK WHEN ZERO:
      """
      01 BLANK-WHEN-ZERO-RECORD.
          05 AMOUNT PIC S9(5) BLANK WHEN ZERO.
      """
    And ASCII codepage
    And JSON data:
      """
      {"AMOUNT":"0"}
      """
    When JSON data is encoded
    Then encoding should succeed
    And encoded data should be blank

  Scenario: Parse copybook with multiple record types
    Given a copybook with diverse field types:
      """
      01 DIVERSE-RECORD.
          05 TEXT-FIELD PIC X(20).
          05 ZONED-DEC PIC S9(9).
          05 PACKED-DEC PIC S9(7)V99 COMP-3.
          05 BINARY-INT PIC S9(5) COMP.
          05 SIGN-SEP PIC S9(5) SIGN SEPARATE LEADING.
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And there should be 5 fields
    And field types should be diverse

  Scenario: Decode EBCDIC data with ASCII output
    Given a copybook with text field:
      """
      01 EBCDIC-RECORD.
          05 FIELD1 PIC X(10).
      """
    And EBCDIC codepage
    And EBCDIC binary data
    When binary data is decoded
    Then decoded value should be converted to ASCII

  Scenario: Encode ASCII data with EBCDIC output
    Given a copybook with text field:
      """
      01 EBCDIC-RECORD.
          05 FIELD1 PIC X(10).
      """
    And EBCDIC codepage
    And ASCII JSON data
    When JSON data is encoded
    Then encoded data should be in EBCDIC

  Scenario: Parse copybook with large OCCURS
    Given a copybook with large OCCURS:
      """
      01 LARGE-OCCURS.
          05 ARRAY OCCURS 100 TIMES.
              10 ELEMENT PIC X(5).
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "ARRAY" should have OCCURS count 100

  Scenario: Decode large OCCURS array
    Given a copybook with large OCCURS:
      """
      01 LARGE-OCCURS.
          05 ARRAY OCCURS 100 TIMES.
              10 ELEMENT PIC X(5).
      """
    And ASCII codepage
    And binary data with 100 elements
    When binary data is decoded
    Then there should be 100 ARRAY elements

  Scenario: Encode large OCCURS array
    Given a copybook with large OCCURS:
      """
      01 LARGE-OCCURS.
          05 ARRAY OCCURS 100 TIMES.
              10 ELEMENT PIC X(5).
      """
    And ASCII codepage
    And JSON data with 100 elements
    When JSON data is encoded
    Then encoding should succeed

  Scenario: Parse copybook with mixed level numbers
    Given a copybook with various levels:
      """
      01 MIXED-LEVELS.
          05 LEVEL05-FIELD PIC X(10).
          10 LEVEL10-FIELD PIC 9(5).
          15 LEVEL15-FIELD PIC X(15).
          20 LEVEL20-FIELD PIC 9(3).
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And fields should have correct levels

  Scenario: Parse copybook with FILLER fields
    Given a copybook with FILLER:
      """
      01 FILLER-RECORD.
          05 FIELD1 PIC X(10).
          05 FILLER PIC X(5).
          05 FIELD2 PIC X(10).
      """
    When copybook is parsed
    Then schema should be successfully parsed
    And field "FILLER" should be present

  Scenario: Decode with FILLER fields excluded
    Given a copybook with FILLER:
      """
      01 FILLER-RECORD.
          05 FIELD1 PIC X(10).
          05 FILLER PIC X(5).
          05 FIELD2 PIC X(10).
      """
    And ASCII codepage
    And binary data
    And emit_filler is false
    When binary data is decoded
    Then FILLER should not be in output
    And FIELD1 should be present
    And FIELD2 should be present

  Scenario: Decode with FILLER fields included
    Given a copybook with FILLER:
      """
      01 FILLER-RECORD.
          05 FIELD1 PIC X(10).
          05 FILLER PIC X(5).
          05 FIELD2 PIC X(10).
      """
    And ASCII codepage
    And binary data
    And emit_filler is true
    When binary data is decoded
    Then FILLER should be in output
    And FIELD1 should be present
    And FIELD2 should be present
