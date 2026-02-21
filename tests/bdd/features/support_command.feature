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
    Then the feature should have status "Supported"

  Scenario: Matrix includes multiple features
    When the support matrix is queried
    Then the matrix should include "Level66Renames" feature

  Scenario: Unknown feature returns error
    When feature "nonexistent-feature" is checked
    Then an error should occur

  Scenario: Feature-governance mapping for signed separate
    When the governance mapping is checked for feature "sign-separate"
    Then the governance mapping should include feature flag "sign_separate"

  Scenario: Feature-governance mapping for COMP-1/COMP-2
    When the governance mapping is checked for feature "comp-1-comp-2"
    Then the governance mapping should include feature flag "comp_1"
    And the governance mapping should include feature flag "comp_2"

  Scenario: Governance summary is complete
    When the governance grid summary is checked
    Then the governance summary should map 7 support entries
    And the matrix should include "EditedPic" feature

  Scenario: Governance runtime summary reports feature-flag gating state
    When the support matrix runtime availability is checked
    Then the command output should report 7 runtime enabled and 0 runtime disabled support entries

  Scenario: Governance runtime summary reflects disabled feature flags
    When the support matrix runtime availability is checked with sign-separate disabled
    Then the command output should report 6 runtime enabled and 1 runtime disabled support entries
