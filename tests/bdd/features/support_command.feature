@support
Feature: Support Command
  Test the support matrix feature query

  Scenario: Query all features in support matrix
    When the support matrix is queried
    Then the matrix should include "Level88Conditions" feature

  Scenario: Check Level-88 conditions feature
    When feature "level-88" is checked
    Then the feature should have status "Supported"

  Scenario: Check Level-66 RENAMES feature
    When feature "level-66-renames" is checked
    Then the feature should have status "Partial"

  Scenario: Check OCCURS DEPENDING ON feature
    When feature "occurs-depending" is checked
    Then the feature should have status "Partial"

  Scenario: Check Edited PIC feature
    When feature "edited-pic" is checked
    Then the feature should have status "Supported"

  Scenario: Check SIGN SEPARATE feature
    When feature "sign-separate" is checked
    Then the feature should have status "Partial"

  Scenario: Matrix includes multiple features
    When the support matrix is queried
    Then the matrix should include "Level66Renames" feature

  Scenario: Unknown feature returns error
    When feature "nonexistent-feature" is checked
    Then an error should occur
