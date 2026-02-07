Feature: Encode and Decode Operations

  As a developer working with COBOL data
  I want to encode and decode binary records using copybook schemas
  So that I can convert between COBOL binary format and JSON

  Scenario: Decode ASCII binary data to JSON
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "TEST-FIELD"

  Scenario: Encode JSON data to ASCII binary
    Given a simple copybook with a single field
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"TEST-FIELD":"ABCDEFGHIJ"},"TEST-FIELD":"ABCDEFGHIJ"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes

  Scenario: Round-trip ASCII data losslessly
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Decode numeric fields from binary
    Given a copybook with numeric fields
    And ASCII codepage
    And binary data: "00000000000000000000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "PACKED-DECIMAL"

  Scenario: Encode numeric fields to binary
    Given a copybook with numeric fields
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"PACKED-DECIMAL":"000000000","BINARY-INTEGER":"000000000","ZONED-DECIMAL":"000000"},"PACKED-DECIMAL":"000000000","BINARY-INTEGER":"000000000","ZONED-DECIMAL":"000000"}
      """
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Decode array fields from binary
    Given a copybook with OCCURS clause
    And ASCII codepage
    And binary data: "ELEMENT0001ELEMENT0002ELEMENT0003ELEMENT0004ELEMENT0005"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "ELEMENT"

  Scenario: Encode array fields to binary
    Given a copybook with OCCURS clause
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"ARRAY-FIELD":[{"ELEMENT":"ELEMENT0001"},{"ELEMENT":"ELEMENT0002"},{"ELEMENT":"ELEMENT0003"},{"ELEMENT":"ELEMENT0004"},{"ELEMENT":"ELEMENT0005"}]},"ARRAY-FIELD":[{"ELEMENT":"ELEMENT0001"},{"ELEMENT":"ELEMENT0002"},{"ELEMENT":"ELEMENT0003"},{"ELEMENT":"ELEMENT0004"},{"ELEMENT":"ELEMENT0005"}]}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 50 bytes

  Scenario: Decode with EBCDIC codepage
    Given a simple copybook with a single field
    And EBCDIC codepage
    And binary data: "\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xD1"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Encode with EBCDIC codepage
    Given a simple copybook with a single field
    And EBCDIC codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"EBCDIC037","fields":{"TEST-FIELD":"ABCDEFGHIJ"},"TEST-FIELD":"ABCDEFGHIJ"}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 10 bytes

  Scenario: Round-trip with EBCDIC codepage
    Given a simple copybook with a single field
    And EBCDIC codepage
    And binary data: "\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xD1"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed

  Scenario: Decode with metadata enabled
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "record_index"
    And the decoded output should contain "codepage"

  Scenario: Encode with coercion enabled
    Given a copybook with numeric fields
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"PACKED-DECIMAL":123.45,"BINARY-INTEGER":67890,"ZONED-DECIMAL":12.34},"PACKED-DECIMAL":123.45,"BINARY-INTEGER":67890,"ZONED-DECIMAL":12.34}
      """
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Decode nested group structures
    Given a copybook with content:
      """
      01 NESTED-RECORD.
          05 GROUP-1.
              10 SUB-FIELD-1 PIC X(5).
              10 SUB-FIELD-2 PIC 9(5).
          05 GROUP-2.
              10 SUB-FIELD-3 PIC X(10).
      """
    And ASCII codepage
    And binary data: "ABCDE12345FGHIJKLMNOP"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "SUB-FIELD-1"
    And the decoded output should contain "SUB-FIELD-2"
    And the decoded output should contain "SUB-FIELD-3"

  Scenario: Encode nested group structures
    Given a copybook with content:
      """
      01 NESTED-RECORD.
          05 GROUP-1.
              10 SUB-FIELD-1 PIC X(5).
              10 SUB-FIELD-2 PIC 9(5).
          05 GROUP-2.
              10 SUB-FIELD-3 PIC X(10).
      """
    And ASCII codepage
    And JSON data:
      """
      {"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"GROUP-1":{"SUB-FIELD-1":"ABCDE","SUB-FIELD-2":"12345"},"GROUP-2":{"SUB-FIELD-3":"FGHIJKLMNOP"}},"GROUP-1":{"SUB-FIELD-1":"ABCDE","SUB-FIELD-2":"12345"},"GROUP-2":{"SUB-FIELD-3":"FGHIJKLMNOP"}}
      """
    When the JSON data is encoded
    Then encoding should succeed
    And the encoded output should be 20 bytes

  Scenario: Round-trip nested group structures
    Given a copybook with content:
      """
      01 NESTED-RECORD.
          05 GROUP-1.
              10 SUB-FIELD-1 PIC X(5).
              10 SUB-FIELD-2 PIC 9(5).
          05 GROUP-2.
              10 SUB-FIELD-3 PIC X(10).
      """
    And ASCII codepage
    And binary data: "ABCDE12345FGHIJKLMNOP"
    When the data is round-tripped
    Then the round-trip should be lossless
    And decoding should succeed
    And encoding should succeed
