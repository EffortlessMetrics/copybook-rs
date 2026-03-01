@governance
Feature: Governance flag synchronization
  The governance system must keep feature flags, support matrix,
  and runtime behavior in sync.

  Scenario: All stable features are enabled by default
    Given the default feature flags
    Then all stable features should be enabled

  Scenario: Experimental features are disabled by default
    Given the default feature flags
    Then experimental features should be disabled

  Scenario: Enterprise features can be enabled
    Given feature flags with Enterprise category enabled
    Then enterprise features should be accessible

  Scenario: Support matrix matches governance flags
    Given the default feature flags
    Then the support matrix should list all governed features

  Scenario: Feature flags are serializable
    Given feature flags with all categories enabled
    When the flags are serialized to JSON
    Then the JSON should be valid and round-trip

  Scenario: Runtime state evaluation matches flags
    Given feature flags with Experimental category enabled
    When runtime state is evaluated
    Then experimental features should report as enabled
