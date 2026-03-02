@error-recovery
Feature: Error Recovery
  As a developer working with COBOL data
  I want the system to handle malformed copybooks, truncated data, and corrupted records gracefully
  So that errors are reported clearly without crashes

  # --- Malformed copybook structures ---

  Scenario: Parse copybook with only comments
    Given a copybook with content:
      """
      * This is a comment line
      * Another comment line
      """
    When the copybook is parsed
    Then an error should occur

  Scenario: Parse copybook with missing PIC clause
    Given a copybook with content:
      """
      01 MISSING-PIC-RECORD.
          05 FIELD-WITHOUT-PIC.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "FIELD-WITHOUT-PIC" should have type "group"

  Scenario: Parse copybook with extremely long field name
    Given a copybook with content:
      """
      01 LONG-NAME-RECORD.
          05 ABCDEFGHIJKLMNOPQRSTUVWXYZ-FIELD PIC X(5).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Parse copybook with numeric field zero digits
    Given a copybook with content:
      """
      01 ZERO-DIGIT-RECORD.
          05 TINY-FIELD PIC 9.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "TINY-FIELD" should have length 1

  Scenario: Parse copybook with maximum PIC length
    Given a copybook with content:
      """
      01 MAX-PIC-RECORD.
          05 BIG-FIELD PIC X(100).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "BIG-FIELD" should have length 100

  Scenario: Decode with binary data shorter than single field gets padded
    Given a copybook with content:
      """
      01 SHORT-RECORD.
          05 LONG-FIELD PIC X(100).
      """
    And ASCII codepage
    And binary data: "AB"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Decode truncated COMP-3 field gets padded
    Given a copybook with content:
      """
      01 TRUNC-COMP3.
          05 AMT PIC S9(9)V99 COMP-3.
      """
    And ASCII codepage
    And binary data: "\x12\x34"
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Encode with null JSON value for alphanumeric field
    Given a copybook with content:
      """
      01 NULL-RECORD.
          05 TEXT-FIELD PIC X(10).
      """
    And ASCII codepage
    And JSON data:
      """
      {"TEXT-FIELD":null}
      """
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Parse copybook with tab characters in content
    Given a copybook with content:
      """
      01 TAB-RECORD.
          05 TAB-FIELD PIC X(10).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Parse copybook with multiple 01 levels
    Given a copybook with content:
      """
      01 FIRST-RECORD.
          05 FIELD-A PIC X(5).
      01 SECOND-RECORD.
          05 FIELD-B PIC 9(5).
      """
    When the copybook is parsed
    Then parsing should succeed

  Scenario: Decode binary data with all space bytes
    Given a copybook with content:
      """
      01 SPACE-RECORD.
          05 TEXT-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "          "
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode binary data with all zero bytes for numeric
    Given a copybook with content:
      """
      01 ZERO-RECORD.
          05 NUM-FIELD PIC 9(5).
      """
    And ASCII codepage
    And binary data with zero value
    When the binary data is decoded
    Then decoding should succeed

  Scenario: Parse copybook with REDEFINES of different length
    Given a copybook with content:
      """
      01 REDEF-LEN-RECORD.
          05 ORIG-FIELD PIC X(20).
          05 ALT-FIELD REDEFINES ORIG-FIELD.
              10 PART-A PIC X(5).
              10 PART-B PIC X(15).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "ALT-FIELD" should redefine "ORIG-FIELD"

  Scenario: Parse copybook with multiple level 88 on same field
    Given a copybook with content:
      """
      01 MULTI-88-RECORD.
          05 STATUS PIC X(2).
              88 IS-ACTIVE VALUE 'AC'.
              88 IS-CLOSED VALUE 'CL'.
              88 IS-PENDING VALUE 'PD'.
              88 IS-SUSPENDED VALUE 'SU'.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "STATUS" should have Level-88 "IS-ACTIVE"
    And the field "STATUS" should have Level-88 "IS-CLOSED"
    And the field "STATUS" should have Level-88 "IS-PENDING"
    And the field "STATUS" should have Level-88 "IS-SUSPENDED"

  Scenario: Parse copybook with OCCURS and child fields
    Given a copybook with content:
      """
      01 OCCURS-CHILD-RECORD.
          05 ITEMS OCCURS 3 TIMES.
              10 ITEM-NAME PIC X(10).
              10 ITEM-QTY PIC 9(5).
      """
    When the copybook is parsed
    Then parsing should succeed
    And field ITEMS should have OCCURS count 3

  Scenario: Encode with boolean JSON value
    Given a copybook with content:
      """
      01 BOOL-RECORD.
          05 FLAG-FIELD PIC X(5).
      """
    And ASCII codepage
    And JSON data:
      """
      {"FLAG-FIELD":true}
      """
    When the JSON data is encoded
    Then encoding should succeed

  Scenario: Parse copybook with deeply nested groups
    Given a copybook with content:
      """
      01 DEEP-RECORD.
          05 LEVEL-5.
              10 LEVEL-10.
                  15 LEVEL-15.
                      20 LEAF-FIELD PIC X(5).
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "LEAF-FIELD" should be present

  Scenario: Parse copybook with single character PIC
    Given a copybook with content:
      """
      01 SINGLE-CHAR-RECORD.
          05 CHAR-FIELD PIC X.
      """
    When the copybook is parsed
    Then parsing should succeed
    And the field "CHAR-FIELD" should have length 1
