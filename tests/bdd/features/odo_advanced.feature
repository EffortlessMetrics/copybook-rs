@odo-advanced
Feature: Advanced ODO with Dialect Variations and Boundary Conditions

  As a developer working with COBOL ODO arrays
  I want boundary conditions and dialect variations handled correctly
  So that variable-length arrays decode, encode, and validate in all scenarios

  # --- ODO parsing with various min/max ranges ---

  Scenario: ODO with min 0 max 10
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 0 TO 10 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with counter "CNT"
    And field ITEMS should have ODO with min 0 and max 10

  Scenario: ODO with min 1 max 100
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 1 and max 100

  Scenario: ODO with min 5 max 50
    Given a copybook with content:
      """
      01 REC.
          05 DETAIL-CNT PIC 9(3).
          05 DETAILS OCCURS 5 TO 50 TIMES DEPENDING ON DETAIL-CNT.
              10 DETAIL-LINE PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field DETAILS should have ODO with min 5 and max 50
    And field DETAILS should have ODO with counter "DETAIL-CNT"

  # --- Dialect-specific ODO min_count behavior ---

  Scenario: Normative dialect preserves declared min_count of 5
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And Normative dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 5 and max 20

  Scenario: Zero-Tolerant dialect forces min_count to 0
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 5 TO 20 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 0 and max 20

  Scenario: One-Tolerant dialect clamps min_count to at least 1
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 0 TO 20 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 1 and max 20

  Scenario: Zero-Tolerant dialect with ODO min already 0 is unchanged
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 0 TO 10 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 0 and max 10

  Scenario: One-Tolerant dialect with ODO min already 1 is unchanged
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And One-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 1 and max 10

  Scenario: Zero-Tolerant dialect with large declared min
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 10 TO 50 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And Zero-Tolerant dialect
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ITEMS should have ODO with min 0 and max 50

  # --- ODO decode boundary conditions ---

  Scenario: Decode ODO with count equals max
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 3 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And ASCII codepage
    And binary data: "003ABCDEABCDEABCDE"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode ODO with count equals min
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And ASCII codepage
    And binary data: "001HELLO"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode ODO with zero count in Zero-Tolerant mode
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And Zero-Tolerant dialect
    And ASCII codepage
    And binary data: "000"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- ODO with group children ---

  Scenario: ODO with group children containing multiple field types
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ENTRIES OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
              10 E-ID PIC 9(5).
              10 E-NAME PIC X(10).
              10 E-AMT PIC S9(7)V99 COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And field ENTRIES should have ODO with counter "CNT"
    And the field "E-ID" should have type "zoned"
    And the field "E-NAME" should have type "alphanumeric"
    And the field "E-AMT" should have type "packed"

  Scenario: Decode ODO with group children
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ENTRIES OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
              10 E-ID PIC 9(3).
              10 E-NAME PIC X(5).
      """
    And ASCII codepage
    And binary data: "002001ABCDE002FGHIJ"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- ODO with different element types ---

  Scenario: ODO with alphanumeric elements
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 LINES OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 LINE-TEXT PIC X(20).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "LINE-TEXT" should have type "alphanumeric"
    And the field "LINE-TEXT" should be 20 bytes long

  Scenario: ODO with numeric elements
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 AMOUNTS OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 AMT PIC 9(7).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMT" should have type "zoned"
    And the field "AMT" should be 7 bytes long

  # --- ODO with prefix fields ---

  Scenario: ODO preceded by multiple fixed fields
    Given a copybook with content:
      """
      01 REC.
          05 HEADER PIC X(10).
          05 REC-TYPE PIC X(2).
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(8).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "HEADER" should have offset 0
    And the field "REC-TYPE" should have offset 10
    And the field "CNT" should have offset 12

  Scenario: Decode ODO with prefix fields
    Given a copybook with content:
      """
      01 REC.
          05 HDR PIC X(3).
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And ASCII codepage
    And binary data: "ABC002HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
    And the decoded output should contain "HDR"

  # --- ODO round-trip ---

  Scenario: ODO round-trip with 2 elements
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And ASCII codepage
    And binary data: "002ABCDEABCDE"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed

  Scenario: ODO round-trip with single element
    Given a copybook with content:
      """
      01 REC.
          05 CNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON CNT.
              10 ITEM PIC X(5).
      """
    And ASCII codepage
    And binary data: "001HELLO"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
