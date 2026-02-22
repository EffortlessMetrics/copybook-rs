@projection_smoke
Feature: Test Projection Minimal

  Background:
    Given ASCII codepage

  Scenario: Test simple projection
    Given a copybook with content:
      """
      01 TEST-RECORD.
         05 TEST-FIELD PIC X(10).
      """
    And field selection: "TEST-FIELD"
    When the copybook is parsed
    And the schema is projected with selected fields
    Then the projection should succeed
