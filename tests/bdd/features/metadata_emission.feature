@metadata
Feature: Metadata Emission
  Test emit_meta flag behavior

  Background:
    Given a copybook with content:
      """
      01 META-RECORD.
         05 DATA-FIELD PIC X(10).
      """
    And ASCII codepage

  Scenario: Emit meta enabled includes metadata fields
    Given emit_meta is true
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "schema_fingerprint"

  Scenario: Emit meta disabled excludes metadata fields
    Given emit_meta is false
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should not contain "schema_fingerprint"

  Scenario: Metadata contains record_index
    Given emit_meta is true
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "record_index"

  Scenario: Metadata with multi-field copybook
    Given a copybook with content:
      """
      01 MULTI-META.
         05 FIELD-A PIC X(5).
         05 FIELD-B PIC 9(3).
      """
    And ASCII codepage
    And emit_meta is true
    And binary data: "HELLO042"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "schema_fingerprint"

  Scenario: Emit meta with COMP-3 field
    Given a copybook with content:
      """
      01 COMP3-META.
         05 PACKED-FIELD PIC S9(5) COMP-3.
      """
    And ASCII codepage
    And emit_meta is true
    And binary data: "\x01\x23\x4C"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "schema_fingerprint"

  Scenario: Default emit_meta is enabled
    Given emit_meta is true
    And binary data: "HELLOWORLD"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should contain "schema_fingerprint"

  Scenario: Emit meta does not affect round-trip
    Given emit_meta is true
    And binary data: "HELLOWORLD"
    When the data is round-tripped
    Then the round-trip should be lossless

  Scenario: Emit meta with valid JSON output
    Given emit_meta is true
    And binary data: "TESTDATA10"
    When the binary data is decoded
    Then decoding should succeed
    And the decoded output should be valid JSON
