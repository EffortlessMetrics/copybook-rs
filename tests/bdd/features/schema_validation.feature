@copybook-parsing
Feature: Schema Validation and Field Properties

  As a developer working with COBOL copybooks
  I want schema parsing to correctly determine field properties
  So that field types, sizes, and positions are accurate

  # --- Field type detection ---

  Scenario: Parse PIC X as alphanumeric
    Given a copybook with content:
      """
      01 REC.
          05 TEXT-FLD PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TEXT-FLD" should have type "alphanumeric"

  Scenario: Parse PIC 9 as zoned decimal
    Given a copybook with content:
      """
      01 REC.
          05 NUM-FLD PIC 9(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "NUM-FLD" should have type "zoned"

  Scenario: Parse PIC S9 COMP as binary
    Given a copybook with content:
      """
      01 REC.
          05 BIN-FLD PIC S9(4) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BIN-FLD" should have type "binary"

  Scenario: Parse PIC S9 COMP-3 as packed
    Given a copybook with content:
      """
      01 REC.
          05 PKD-FLD PIC S9(5) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PKD-FLD" should have type "packed"

  # --- Byte length calculations ---

  Scenario: Alphanumeric field byte length equals PIC count
    Given a copybook with content:
      """
      01 REC.
          05 FLD-A PIC X(1).
          05 FLD-B PIC X(10).
          05 FLD-C PIC X(100).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FLD-A" should have length 1
    And the field "FLD-B" should have length 10
    And the field "FLD-C" should have length 100

  Scenario: Zoned decimal byte length equals digit count
    Given a copybook with content:
      """
      01 REC.
          05 FLD-A PIC 9(1).
          05 FLD-B PIC 9(5).
          05 FLD-C PIC 9(18).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FLD-A" should have length 1
    And the field "FLD-B" should have length 5
    And the field "FLD-C" should have length 18

  Scenario: Zoned decimal with implied decimal includes scale in length
    Given a copybook with content:
      """
      01 REC.
          05 PRICE PIC 9(5)V99.
          05 RATE PIC 9(3)V9(4).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PRICE" should have length 7
    And the field "RATE" should have length 7

  Scenario: COMP halfword is 2 bytes
    Given a copybook with content:
      """
      01 REC.
          05 HW PIC S9(1) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "HW" should have length 2

  Scenario: COMP fullword is 4 bytes
    Given a copybook with content:
      """
      01 REC.
          05 FW PIC S9(5) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FW" should have length 4

  Scenario: COMP doubleword is 8 bytes
    Given a copybook with content:
      """
      01 REC.
          05 DW PIC S9(10) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DW" should have length 8

  Scenario: COMP-3 byte length uses packed formula
    Given a copybook with content:
      """
      01 REC.
          05 P3 PIC S9(3) COMP-3.
          05 P5 PIC S9(5) COMP-3.
          05 P7 PIC S9(7) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "P3" should have length 2
    And the field "P5" should have length 3
    And the field "P7" should have length 4

  # --- Offset calculations ---

  Scenario: Sequential fields have correct offsets
    Given a copybook with content:
      """
      01 REC.
          05 A PIC X(3).
          05 B PIC 9(5).
          05 C PIC X(10).
          05 D PIC S9(4) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "A" should have offset 0
    And the field "B" should have offset 3
    And the field "C" should have offset 8
    And the field "D" should have offset 18

  Scenario: Schema has fingerprint after parsing
    Given a copybook with content:
      """
      01 REC.
          05 FLD PIC X(10).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the schema should have fingerprint

  # --- Level numbers ---

  Scenario: Fields have correct level numbers
    Given a copybook with content:
      """
      01 TOP.
          05 MID-GROUP.
              10 LEAF-A PIC X(5).
              10 LEAF-B PIC 9(3).
          05 LEAF-C PIC X(8).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TOP" should have level 1
    And the field "MID-GROUP" should have level 5
    And the field "LEAF-A" should have level 10
    And the field "LEAF-B" should have level 10
    And the field "LEAF-C" should have level 5

  Scenario: Signed zoned field length includes sign separate
    Given a copybook with content:
      """
      01 REC.
          05 SIGNED-FLD PIC S9(5) SIGN IS SEPARATE LEADING.
          05 UNSIGNED-FLD PIC 9(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "SIGNED-FLD" should have length 6
    And the field "UNSIGNED-FLD" should have length 5
