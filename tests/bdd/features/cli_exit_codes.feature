@exit-codes
Feature: CLI Exit Codes
  Test exit code mapping from error families

  Scenario: Success returns exit code 0
    Given a copybook with content:
      """
      01 EXIT-RECORD.
         05 DATA-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Parse error returns non-zero exit code
    Given a copybook with content:
      """
      INVALID COBOL SYNTAX
      """
    When the copybook is parsed
    When the exit code is computed from the error
    Then the exit code should be 1

  Scenario: Exit code 0 means success
    Then exit code 0 means success

  Scenario: Valid decode has exit code 0
    Given a copybook with content:
      """
      01 VALID-RECORD.
         05 FIELD-A PIC X(5).
      """
    And ASCII codepage
    And binary data: "HELLO"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid encode has exit code 0
    Given a copybook with content:
      """
      01 ENCODE-RECORD.
         05 FIELD-A PIC X(5).
      """
    And ASCII codepage
    And JSON data: "{\"FIELD-A\":\"HELLO\"}"
    When the JSON data is encoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Successful round-trip has exit code 0
    Given a copybook with content:
      """
      01 RT-RECORD.
         05 RT-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "HELLOWORLD"
    When the data is round-tripped
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Successful parse has exit code 0
    Given a copybook with content:
      """
      01 PARSE-RECORD.
         05 PARSE-FIELD PIC X(5).
      """
    When the copybook is parsed
    Then parsing should succeed
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid copybook with numeric fields exits 0
    Given a copybook with content:
      """
      01 NUM-EXIT.
         05 NUM-FIELD PIC 9(5).
      """
    And ASCII codepage
    And binary data: "12345"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid COMP-3 decode exits 0
    Given a copybook with content:
      """
      01 COMP3-EXIT.
         05 PACKED-FIELD PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And binary data: "\x01\x23\x4C"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid binary integer decode exits 0
    Given a copybook with content:
      """
      01 BIN-EXIT.
         05 BIN-FIELD PIC S9(4) COMP.
      """
    And ASCII codepage
    And binary data: "\x00\x2A"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid REDEFINES decode exits 0
    Given a copybook with content:
      """
      01 REDEF-EXIT.
         05 ORIG PIC X(10).
         05 ALT REDEFINES ORIG PIC X(10).
      """
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid signed zoned decimal decode exits 0
    Given a copybook with content:
      """
      01 SIGNED-EXIT.
         05 BAL PIC S9(5).
      """
    And ASCII codepage
    And binary data: "1234{"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid group record decode exits 0
    Given a copybook with content:
      """
      01 GRP-EXIT.
         05 HEADER.
             10 H-CODE PIC X(3).
             10 H-NAME PIC X(7).
      """
    And ASCII codepage
    And binary data: "ABCHELLOOO"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid SIGN SEPARATE LEADING decode exits 0
    Given a copybook with SIGN SEPARATE LEADING
      """
      01 SEP-EXIT.
         05 AMT PIC S9(5) SIGN IS SEPARATE LEADING.
      """
    And ASCII codepage
    And binary data: "+12345"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid multi-field encode exits 0
    Given a copybook with content:
      """
      01 MULTI-EXIT.
         05 FLD-A PIC X(5).
         05 FLD-B PIC 9(3).
      """
    And ASCII codepage
    And JSON data: "{\"FLD-A\":\"HELLO\",\"FLD-B\":\"123\"}"
    When the JSON data is encoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid decimal field decode exits 0
    Given a copybook with content:
      """
      01 DEC-EXIT.
         05 RATE PIC 9(3)V99.
      """
    And ASCII codepage
    And binary data: "12345"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid COMP-1 float decode exits 0
    Given a copybook with content:
      """
      01 FLT-EXIT.
         05 FLT-FIELD COMP-1.
      """
    And ASCII codepage
    And binary data: "\x42\x28\x00\x00"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid COMP-2 double decode exits 0
    Given a copybook with content:
      """
      01 DBL-EXIT.
         05 DBL-FIELD COMP-2.
      """
    And ASCII codepage
    And binary data: "\x40\x45\x00\x00\x00\x00\x00\x00"
    When the binary data is decoded
    When the exit code is computed from the error
    Then the exit code should be 0

  Scenario: Valid round-trip with signed field exits 0
    Given a copybook with content:
      """
      01 RT-SIGN-EXIT.
         05 AMT PIC S9(5).
      """
    And ASCII codepage
    And binary data: "1234{"
    When the data is round-tripped
    When the exit code is computed from the error
    Then the exit code should be 0
