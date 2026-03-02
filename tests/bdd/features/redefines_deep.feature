@redefines-deep
Feature: Deep REDEFINES with Multiple Alternates, Nesting, and Group-Level

  As a developer working with complex COBOL REDEFINES patterns
  I want multi-alternate, nested, and group-level REDEFINES to work correctly
  So that all overlapping field interpretations are handled in schema and decoding

  # --- Multiple alternates ---

  Scenario: Four alternates redefining a single field
    Given a copybook with content:
      """
      01 REC.
          05 ORIG PIC X(12).
          05 VIEW-ALPHA REDEFINES ORIG PIC X(12).
          05 VIEW-NUM REDEFINES ORIG PIC 9(12).
          05 VIEW-SPLIT REDEFINES ORIG.
              10 PART-A PIC X(6).
              10 PART-B PIC X(6).
          05 VIEW-DATE REDEFINES ORIG.
              10 YEAR-P PIC 9(4).
              10 MONTH-P PIC 9(4).
              10 DAY-P PIC 9(4).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "VIEW-ALPHA" should redefine "ORIG"
    And the field "VIEW-NUM" should redefine "ORIG"
    And the field "VIEW-SPLIT" should redefine "ORIG"
    And the field "VIEW-DATE" should redefine "ORIG"
    And the field "ORIG" should have offset 0
    And the field "VIEW-ALPHA" should have offset 0
    And the field "VIEW-NUM" should have offset 0
    And the field "VIEW-SPLIT" should have offset 0
    And the field "VIEW-DATE" should have offset 0

  Scenario: Three alternates with mixed types
    Given a copybook with content:
      """
      01 REC.
          05 RAW-DATA PIC X(8).
          05 AS-TEXT REDEFINES RAW-DATA PIC X(8).
          05 AS-NUMBERS REDEFINES RAW-DATA.
              10 NUM-A PIC 9(4).
              10 NUM-B PIC 9(4).
          05 AS-PACKED REDEFINES RAW-DATA.
              10 PKD-A PIC S9(7) COMP-3.
              10 PKD-B PIC S9(7) COMP-3.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AS-TEXT" should redefine "RAW-DATA"
    And the field "AS-NUMBERS" should redefine "RAW-DATA"
    And the field "AS-PACKED" should redefine "RAW-DATA"
    And the field "NUM-A" should have type "zoned"
    And the field "PKD-A" should have type "packed"

  Scenario: Five alternates on a 20-byte field
    Given a copybook with content:
      """
      01 REC.
          05 BASE-AREA PIC X(20).
          05 VIEW-1 REDEFINES BASE-AREA PIC X(20).
          05 VIEW-2 REDEFINES BASE-AREA PIC 9(20).
          05 VIEW-3 REDEFINES BASE-AREA.
              10 V3-A PIC X(10).
              10 V3-B PIC X(10).
          05 VIEW-4 REDEFINES BASE-AREA.
              10 V4-A PIC X(5).
              10 V4-B PIC X(5).
              10 V4-C PIC X(5).
              10 V4-D PIC X(5).
          05 VIEW-5 REDEFINES BASE-AREA.
              10 V5-HDR PIC X(4).
              10 V5-BODY PIC X(16).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "VIEW-1" should redefine "BASE-AREA"
    And the field "VIEW-2" should redefine "BASE-AREA"
    And the field "VIEW-3" should redefine "BASE-AREA"
    And the field "VIEW-4" should redefine "BASE-AREA"
    And the field "VIEW-5" should redefine "BASE-AREA"

  # --- Nested group REDEFINES ---

  Scenario: Nested groups inside REDEFINES alternate
    Given a copybook with content:
      """
      01 REC.
          05 ORIGINAL.
              10 O-TYPE PIC X(2).
              10 O-DATA PIC X(18).
          05 ALTERNATE REDEFINES ORIGINAL.
              10 A-HDR.
                  15 A-CODE PIC X(3).
                  15 A-FLAG PIC X(1).
              10 A-DETAIL.
                  15 A-AMT PIC 9(8).
                  15 A-DESC PIC X(8).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ALTERNATE" should redefine "ORIGINAL"
    And the field "A-HDR" should have type "group"
    And the field "A-DETAIL" should have type "group"
    And the field "A-CODE" should have type "alphanumeric"
    And the field "A-AMT" should have type "zoned"

  Scenario: Deeply nested REDEFINES three levels
    Given a copybook with content:
      """
      01 REC.
          05 OUTER-GRP.
              10 MID-GRP.
                  15 INNER-FLD PIC X(10).
                  15 INNER-ALT REDEFINES INNER-FLD.
                      20 IA-PART-A PIC X(5).
                      20 IA-PART-B PIC X(5).
              10 MID-OTHER PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "OUTER-GRP" should have type "group"
    And the field "MID-GRP" should have type "group"
    And the field "INNER-ALT" should redefine "INNER-FLD"
    And the field "IA-PART-A" should have type "alphanumeric"
    And the field "IA-PART-B" should have type "alphanumeric"

  Scenario: REDEFINES at level-10 inside a group
    Given a copybook with content:
      """
      01 REC.
          05 HEADER.
              10 HDR-DATE PIC X(8).
              10 HDR-DATE-ALT REDEFINES HDR-DATE.
                  15 HDR-YEAR PIC 9(4).
                  15 HDR-MONTH PIC 9(2).
                  15 HDR-DAY PIC 9(2).
          05 BODY PIC X(20).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "HDR-DATE-ALT" should redefine "HDR-DATE"
    And the field "HDR-YEAR" should have type "zoned"
    And the field "BODY" should have offset 8

  # --- Group-level REDEFINES with mixed child types ---

  Scenario: Group REDEFINES with packed decimal and binary children
    Given a copybook with content:
      """
      01 REC.
          05 RAW-BLOCK PIC X(12).
          05 DETAIL-VIEW REDEFINES RAW-BLOCK.
              10 DV-CODE PIC X(2).
              10 DV-AMOUNT PIC S9(9) COMP-3.
              10 DV-QTY PIC S9(4) COMP.
              10 DV-FLAG PIC X(3).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DETAIL-VIEW" should redefine "RAW-BLOCK"
    And the field "DV-CODE" should have type "alphanumeric"
    And the field "DV-AMOUNT" should have type "packed"
    And the field "DV-QTY" should have type "binary"
    And the field "DV-FLAG" should have type "alphanumeric"

  Scenario: Group REDEFINES with all binary integer children
    Given a copybook with content:
      """
      01 REC.
          05 BLOCK PIC X(8).
          05 NUMS REDEFINES BLOCK.
              10 N-A PIC S9(4) COMP.
              10 N-B PIC S9(4) COMP.
              10 N-C PIC S9(4) COMP.
              10 N-D PIC S9(4) COMP.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "NUMS" should redefine "BLOCK"
    And the field "N-A" should have type "binary"
    And the field "N-A" should be 2 bytes long
    And the field "N-B" should be 2 bytes long

  # --- REDEFINES offset and record size ---

  Scenario: Multiple REDEFINES do not affect subsequent field offset
    Given a copybook with content:
      """
      01 REC.
          05 PREFIX PIC X(10).
          05 DATA-AREA PIC X(20).
          05 DATA-ALT-1 REDEFINES DATA-AREA PIC X(20).
          05 DATA-ALT-2 REDEFINES DATA-AREA.
              10 DA2-A PIC X(10).
              10 DA2-B PIC X(10).
          05 SUFFIX PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PREFIX" should have offset 0
    And the field "DATA-AREA" should have offset 10
    And the field "DATA-ALT-1" should have offset 10
    And the field "DATA-ALT-2" should have offset 10
    And the field "SUFFIX" should have offset 30

  Scenario: REDEFINES with shorter alternate preserves original length
    Given a copybook with content:
      """
      01 REC.
          05 ORIG PIC X(16).
          05 ALT-SHORT REDEFINES ORIG PIC X(8).
          05 ALT-FULL REDEFINES ORIG PIC X(16).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ORIG" should have length 16
    And the field "ALT-SHORT" should have length 8
    And the field "ALT-FULL" should have length 16

  # --- Decode with deep REDEFINES ---

  Scenario: Decode with four alternates
    Given a copybook with content:
      """
      01 REC.
          05 RAW PIC X(8).
          05 AS-TEXT REDEFINES RAW PIC X(8).
          05 AS-NUMS REDEFINES RAW PIC 9(8).
          05 AS-PARTS REDEFINES RAW.
              10 PART-1 PIC X(4).
              10 PART-2 PIC X(4).
      """
    And ASCII codepage
    And binary data: "12345678"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode nested REDEFINES group with date parts
    Given a copybook with content:
      """
      01 REC.
          05 HEADER.
              10 H-RAW PIC X(8).
              10 H-DATE REDEFINES H-RAW.
                  15 H-YEAR PIC 9(4).
                  15 H-MON PIC 9(2).
                  15 H-DAY PIC 9(2).
          05 TRAILER PIC X(2).
      """
    And ASCII codepage
    And binary data: "2025013100"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  Scenario: Decode group REDEFINES with packed decimal child
    Given a copybook with content:
      """
      01 REC.
          05 RAW-ZONE PIC X(7).
          05 STRUCTURED REDEFINES RAW-ZONE.
              10 S-CODE PIC X(3).
              10 S-AMT PIC S9(5) COMP-3.
              10 S-FLAG PIC X(1).
      """
    And ASCII codepage
    And binary data for all fields
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON

  # --- REDEFINES with Level-88 conditions ---

  Scenario: REDEFINES original field with Level-88 conditions
    Given a copybook with content:
      """
      01 REC.
          05 TYPE-CODE PIC X(2).
              88 IS-ALPHA VALUE 'AA'.
              88 IS-NUMERIC VALUE 'NN'.
          05 TYPE-ALT REDEFINES TYPE-CODE PIC 9(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TYPE-ALT" should redefine "TYPE-CODE"
    And the field "TYPE-CODE" should have Level-88 "IS-ALPHA"
    And the field "TYPE-CODE" should have Level-88 "IS-NUMERIC"

  Scenario: Level-88 on field inside REDEFINES group
    Given a copybook with content:
      """
      01 REC.
          05 ORIG PIC X(10).
          05 DETAIL REDEFINES ORIG.
              10 D-TYPE PIC X(1).
                  88 D-ACTIVE VALUE 'A'.
                  88 D-CLOSED VALUE 'C'.
              10 D-DATA PIC X(9).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "DETAIL" should redefine "ORIG"
    And the field "D-TYPE" should have Level-88 "D-ACTIVE"
    And the field "D-TYPE" should have Level-88 "D-CLOSED"

  # --- Round-trip with REDEFINES ---

  Scenario: Round-trip decode-encode with simple REDEFINES
    Given a copybook with content:
      """
      01 REC.
          05 NAME-FIELD PIC X(10).
          05 CODE-FIELD REDEFINES NAME-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  Scenario: Round-trip with group REDEFINES
    Given a copybook with content:
      """
      01 REC.
          05 RAW PIC X(8).
          05 SPLIT REDEFINES RAW.
              10 SP-A PIC X(4).
              10 SP-B PIC X(4).
      """
    And ASCII codepage
    And binary data: "TESTTHAT"
    When the data is round-tripped
    Then decoding should succeed
    And encoding should succeed
    And the round-trip should be lossless

  # --- REDEFINES with FILLER ---

  Scenario: REDEFINES group containing FILLER
    Given a copybook with content:
      """
      01 REC.
          05 BLOCK PIC X(10).
          05 BLOCK-ALT REDEFINES BLOCK.
              10 BA-DATA PIC X(6).
              10 FILLER PIC X(4).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BLOCK-ALT" should redefine "BLOCK"
    And the field "BA-DATA" should have type "alphanumeric"
    And the field "BA-DATA" should have length 6

  # --- REDEFINES type and level preservation ---

  Scenario: REDEFINES preserves field level numbers
    Given a copybook with content:
      """
      01 REC.
          05 BASE PIC X(10).
          05 ALT-A REDEFINES BASE PIC X(10).
          05 ALT-B REDEFINES BASE.
              10 AB-P1 PIC X(5).
              10 AB-P2 PIC X(5).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BASE" should have level 5
    And the field "ALT-A" should have level 5
    And the field "ALT-B" should have level 5
    And the field "AB-P1" should have level 10
