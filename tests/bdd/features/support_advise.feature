@support
Feature: Support Advise: test
  Advisory verdicts for parsed copybook constructs under evaluated options

  Scenario: Tail ODO advises supported
    When an advise evaluation runs for "tail-ODO" construct
    Then the advisory verdict should be "supported"

  Scenario: Non-tail ODO advises rejected
    When an advise evaluation runs for "non-tail-ODO" construct
    Then the advisory verdict should be "rejected"

  Scenario: Level-88 advises supported
    When an advise evaluation runs for "level-88" construct
    Then the advisory verdict should be "supported"

  Scenario: Unmapped construct advises partial-unknown
    When an advise evaluation runs for "unmapped" construct
    Then the advisory verdict should be "partial-unknown"

  Scenario: Unparsable copybook advises invalid-input
    When an advise evaluation runs for "unparsable" construct
    Then the advisory verdict should be "invalid-input"
