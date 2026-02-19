Feature: Determinism Validation

  As a developer working with COBOL data
  I want to verify that encode and decode operations produce deterministic results
  So that I can ensure production reliability and data consistency

  Scenario: Decode determinism with identical output
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When decode determinism is checked
    Then the decode should be deterministic
    And the round 1 hash should equal to round 2 hash
    And there should be no byte differences

  Scenario: Encode determinism with identical output
    Given a simple copybook with a single field
    And ASCII codepage
    And JSON data: "{\"TEST-FIELD\":\"ABCDEFGHIJ\"}"
    When encode determinism is checked
    Then the encode should be deterministic
    And the round 1 hash should equal to round 2 hash
    And there should be no byte differences

  Scenario: Round-trip determinism with identical output
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When round-trip determinism is checked
    Then the round-trip should be deterministic
    And the round 1 hash should equal to round 2 hash
    And there should be no byte differences

  Scenario: Decode determinism with multiple fields
    Given a copybook with content:
      """
      01 RECORD.
         05 FIELD-1 PIC X(5).
         05 FIELD-2 PIC X(5).
      """
    And ASCII codepage
    And binary data: "ABCDE12345"
    When decode determinism is checked
    Then the decode should be deterministic
    And the round 1 hash should equal to round 2 hash

  Scenario: Encode determinism with multiple fields
    Given a copybook with content:
      """
      01 RECORD.
         05 FIELD-1 PIC X(5).
         05 FIELD-2 PIC X(5).
      """
    And ASCII codepage
    And JSON data: "{\"FIELD-1\":\"ABCDE\",\"FIELD-2\":\"12345\"}"
    When encode determinism is checked
    Then the encode should be deterministic
    And the round 1 hash should equal to round 2 hash

  Scenario: Determinism result generates JSON output for CI integration
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When decode determinism is checked
    Then the JSON should contain "mode"
    And the JSON should contain "round1_hash"
    And the JSON should contain "round2_hash"
    And the JSON should contain "is_deterministic"

  Scenario: Human-readable determinism output shows verdict
    Given a simple copybook with a single field
    And ASCII codepage
    And binary data: "ABCDEFGHIJ"
    When decode determinism is checked
    Then the human-readable output should show "DETERMINISTIC"
    And the output should contain "Round 1 hash"
    And the output should contain "Round 2 hash"
