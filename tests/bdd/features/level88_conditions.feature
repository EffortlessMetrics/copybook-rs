@level-88
Feature: Level-88 Condition Values

  As a developer working with COBOL copybooks
  I want Level-88 condition names to be parsed and preserved correctly
  So that condition-based logic is available in downstream processing

  # --- Parsing Level-88 on various field types ---

  Scenario: Level-88 on alphanumeric field
    Given a copybook with content:
      """
      01 CUSTOMER-RECORD.
          05 CUST-TYPE PIC X(1).
              88 IS-RETAIL VALUE 'R'.
              88 IS-WHOLESALE VALUE 'W'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CUST-TYPE" should have Level-88 "IS-RETAIL"
    And the field "CUST-TYPE" should have Level-88 "IS-WHOLESALE"

  Scenario: Level-88 on numeric field
    Given a copybook with content:
      """
      01 STATUS-RECORD.
          05 STATUS-CODE PIC 9(2).
              88 STATUS-OK VALUE '00'.
              88 STATUS-ERROR VALUE '98'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "STATUS-CODE" should have Level-88 "STATUS-OK"
    And the field "STATUS-CODE" should have Level-88 "STATUS-ERROR"

  Scenario: Level-88 with single VALUE literal
    Given a copybook with content:
      """
      01 FLAG-RECORD.
          05 FLAG-FIELD PIC X(1).
              88 FLAG-ON VALUE 'Y'.
              88 FLAG-OFF VALUE 'N'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FLAG-FIELD" should have Level-88 "FLAG-ON"
    And the field "FLAG-FIELD" should have Level-88 "FLAG-OFF"

  Scenario: Level-88 with THRU range
    Given a copybook with content:
      """
      01 GRADE-RECORD.
          05 GRADE-CODE PIC X(1).
              88 PASSING-GRADE VALUE 'A' THRU 'C'.
              88 FAILING-GRADE VALUE 'D' THRU 'F'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "GRADE-CODE" should have Level-88 "PASSING-GRADE"
    And the field "GRADE-CODE" should have Level-88 "FAILING-GRADE"
    And the field "PASSING-GRADE" should have type "condition"
    And the field "FAILING-GRADE" should have type "condition"

  Scenario: Level-88 with multiple VALUES
    Given a copybook with content:
      """
      01 CODE-RECORD.
          05 ACTION-CODE PIC X(1).
              88 VALID-ACTION VALUE 'A' 'B' 'C'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ACTION-CODE" should have Level-88 "VALID-ACTION"
    And the field "VALID-ACTION" should have type "condition"

  Scenario: Level-88 with comma-separated VALUES
    Given a copybook with content:
      """
      01 TYPE-RECORD.
          05 TYPE-CODE PIC X(2).
              88 VALID-TYPE VALUE 'AA', 'BB', 'CC'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "TYPE-CODE" should have Level-88 "VALID-TYPE"

  Scenario: Level-88 on COMP-3 field parses conditions
    Given a copybook with content:
      """
      01 AMOUNT-RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
              88 ZERO-AMOUNT VALUE '0'.
              88 MAX-AMOUNT VALUE '9999999'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "AMOUNT" should have Level-88 "ZERO-AMOUNT"
    And the field "AMOUNT" should have Level-88 "MAX-AMOUNT"
    And the field "ZERO-AMOUNT" should have type "condition"
    And the field "MAX-AMOUNT" should have type "condition"

  Scenario: Level-88 condition has zero storage length
    Given a copybook with Level-88 condition values
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "STATUS-ACTIVE" should have type "condition"
    And the field "STATUS-ACTIVE" should have level 88

  Scenario: Level-88 after OCCURS array element
    Given a copybook with content:
      """
      01 ARRAY-RECORD.
          05 STATUS-ARRAY OCCURS 3 TIMES.
              10 ITEM-STATUS PIC X(1).
                  88 ITEM-ACTIVE VALUE 'A'.
                  88 ITEM-DELETED VALUE 'D'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "ITEM-STATUS" should have Level-88 "ITEM-ACTIVE"
    And the field "ITEM-STATUS" should have Level-88 "ITEM-DELETED"

  Scenario: Schema preserves Level-88 conditions as children
    Given a copybook with content:
      """
      01 PRESERVE-RECORD.
          05 COLOR-CODE PIC X(1).
              88 IS-RED VALUE 'R'.
              88 IS-BLUE VALUE 'B'.
              88 IS-GREEN VALUE 'G'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "IS-RED" should have type "condition"
    And the field "IS-BLUE" should have type "condition"
    And the field "IS-GREEN" should have type "condition"
    And the field "IS-RED" should have level 88
    And the field "IS-BLUE" should have level 88
    And the field "IS-GREEN" should have level 88

  Scenario: Multiple fields each with Level-88 conditions
    Given a copybook with content:
      """
      01 MULTI-COND-RECORD.
          05 FIELD-A PIC X(1).
              88 A-YES VALUE 'Y'.
              88 A-NO VALUE 'N'.
          05 FIELD-B PIC X(1).
              88 B-ACTIVE VALUE 'A'.
              88 B-CLOSED VALUE 'C'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "FIELD-A" should have Level-88 "A-YES"
    And the field "FIELD-A" should have Level-88 "A-NO"
    And the field "FIELD-B" should have Level-88 "B-ACTIVE"
    And the field "FIELD-B" should have Level-88 "B-CLOSED"

  Scenario: Level-88 conditions have correct level number
    Given a copybook with content:
      """
      01 LVL-RECORD.
          05 CHECK-FIELD PIC X(1).
              88 CHK-A VALUE 'A'.
              88 CHK-B VALUE 'B'.
              88 CHK-C VALUE 'C'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CHK-A" should have level 88
    And the field "CHK-B" should have level 88
    And the field "CHK-C" should have level 88

  Scenario: Decode record with field that has no Level-88
    Given a copybook with content:
      """
      01 DEC-RECORD.
          05 NAME-FIELD PIC X(10).
      """
    And ASCII codepage
    And binary data: "JOHN DOE  "
    When the binary data is decoded
    Then decoding should succeed
    And decoded field NAME-FIELD should be "JOHN DOE"

  Scenario: Level-88 on group child field
    Given a copybook with content:
      """
      01 GROUP-RECORD.
          05 HEADER.
              10 HDR-TYPE PIC X(1).
                  88 HDR-STANDARD VALUE 'S'.
                  88 HDR-EXTENDED VALUE 'E'.
              10 HDR-VERSION PIC 9(2).
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "HDR-TYPE" should have Level-88 "HDR-STANDARD"
    And the field "HDR-TYPE" should have Level-88 "HDR-EXTENDED"
    And the field "HEADER" should have type "group"

  Scenario: Level-88 with THROUGH keyword synonym
    Given a copybook with content:
      """
      01 RANGE-RECORD.
          05 RANGE-FIELD PIC X(3).
              88 LOW-RANGE VALUE 'AAA' THROUGH 'MMM'.
              88 HIGH-RANGE VALUE 'NNN' THROUGH 'ZZZ'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "RANGE-FIELD" should have Level-88 "LOW-RANGE"
    And the field "RANGE-FIELD" should have Level-88 "HIGH-RANGE"

  Scenario: Level-88 on signed zoned decimal parses conditions
    Given a copybook with content:
      """
      01 SIGNED-RECORD.
          05 BALANCE PIC S9(5)V99.
              88 IS-ZERO VALUE '0'.
              88 IS-POSITIVE VALUE '1' THRU '9999999'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "BALANCE" should have Level-88 "IS-ZERO"
    And the field "BALANCE" should have Level-88 "IS-POSITIVE"
    And the field "IS-ZERO" should have type "condition"
    And the field "IS-POSITIVE" should have type "condition"

  # --- Additional Level-88 scenarios ---

  Scenario: Level-88 on PIC X(3) three-character field
    Given a copybook with content:
      """
      01 CURRENCY-RECORD.
          05 CURR-CODE PIC X(3).
              88 IS-USD VALUE 'USD'.
              88 IS-EUR VALUE 'EUR'.
              88 IS-GBP VALUE 'GBP'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "CURR-CODE" should have Level-88 "IS-USD"
    And the field "CURR-CODE" should have Level-88 "IS-EUR"
    And the field "CURR-CODE" should have Level-88 "IS-GBP"
    And the field "IS-USD" should have type "condition"
    And the field "IS-EUR" should have type "condition"
    And the field "IS-GBP" should have type "condition"

  Scenario: Level-88 on unsigned numeric field
    Given a copybook with content:
      """
      01 UNSIGNED-RECORD.
          05 PRIORITY PIC 9(1).
              88 LOW-PRIORITY VALUE '1'.
              88 MED-PRIORITY VALUE '2'.
              88 HIGH-PRIORITY VALUE '3'.
              88 URGENT VALUE '9'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "PRIORITY" should have Level-88 "LOW-PRIORITY"
    And the field "PRIORITY" should have Level-88 "MED-PRIORITY"
    And the field "PRIORITY" should have Level-88 "HIGH-PRIORITY"
    And the field "PRIORITY" should have Level-88 "URGENT"
    And the field "LOW-PRIORITY" should have level 88
    And the field "MED-PRIORITY" should have level 88
    And the field "HIGH-PRIORITY" should have level 88
    And the field "URGENT" should have level 88

  Scenario: Level-88 deeply nested in group hierarchy
    Given a copybook with content:
      """
      01 DEEP-RECORD.
          05 OUTER-GROUP.
              10 INNER-GROUP.
                  15 DEEP-FLAG PIC X(1).
                      88 DEEP-YES VALUE 'Y'.
                      88 DEEP-NO VALUE 'N'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "OUTER-GROUP" should have type "group"
    And the field "INNER-GROUP" should have type "group"
    And the field "DEEP-FLAG" should have Level-88 "DEEP-YES"
    And the field "DEEP-FLAG" should have Level-88 "DEEP-NO"
    And the field "DEEP-YES" should have type "condition"
    And the field "DEEP-NO" should have type "condition"

  Scenario: Level-88 on PIC X(2) two-character codes
    Given a copybook with content:
      """
      01 CODE-RECORD.
          05 STATE-CODE PIC X(2).
              88 STATE-NY VALUE 'NY'.
              88 STATE-CA VALUE 'CA'.
              88 STATE-TX VALUE 'TX'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "STATE-CODE" should have Level-88 "STATE-NY"
    And the field "STATE-CODE" should have Level-88 "STATE-CA"
    And the field "STATE-CODE" should have Level-88 "STATE-TX"

  Scenario: Three fields each with multiple Level-88 conditions
    Given a copybook with content:
      """
      01 MULTI-RECORD.
          05 STATUS PIC X(1).
              88 STAT-OPEN VALUE 'O'.
              88 STAT-CLOSED VALUE 'C'.
          05 REGION PIC X(1).
              88 REG-NORTH VALUE 'N'.
              88 REG-SOUTH VALUE 'S'.
          05 DEPT PIC X(1).
              88 DEPT-HR VALUE 'H'.
              88 DEPT-IT VALUE 'I'.
      """
    When the copybook is parsed
    Then the schema should be successfully parsed
    And the field "STATUS" should have Level-88 "STAT-OPEN"
    And the field "STATUS" should have Level-88 "STAT-CLOSED"
    And the field "REGION" should have Level-88 "REG-NORTH"
    And the field "REGION" should have Level-88 "REG-SOUTH"
    And the field "DEPT" should have Level-88 "DEPT-HR"
    And the field "DEPT" should have Level-88 "DEPT-IT"
