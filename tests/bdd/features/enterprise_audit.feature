Feature: Enterprise Audit System

  As an enterprise data processing organization
  I want to have comprehensive audit capabilities for regulatory compliance
  So that I can ensure SOX, HIPAA, GDPR, and PCI DSS compliance

  Background:
    Given the audit system is enabled
    And an audit context is initialized

  # Group 1: Compliance Framework Events (4 scenarios)

  Scenario: SOX compliance audit trail
    Given a financial SOX compliance copybook
    When the copybook is processed with audit
    Then the audit event type should be "CopybookParse"
    And the audit event severity should be "Info"
    And the audit context should have compliance profile "SOX"
    And the audit event user should be "bdd_test_user"
    And the audit event timestamp should be non-empty
    And the audit event integrity hash should be non-empty
    And the audit output should be in valid JSON format

  Scenario: HIPAA data processing
    Given a healthcare HIPAA compliance copybook
    When the copybook is processed with audit
    Then the audit event type should be "CopybookParse"
    And the audit context should have compliance profile "HIPAA"
    And the audit context should have security classification "PHI"
    And the audit context metadata should contain "data_classification"

  Scenario: GDPR audit report
    Given a GDPR personal data copybook
    When the copybook is processed with audit
    Then the audit event type should be "CopybookParse"
    And the audit context should have compliance profile "GDPR"
    And the audit context metadata should contain "legal_basis"
    And the audit context metadata should contain "processing_purpose"

  Scenario: PCI DSS compliance
    Given a PCI DSS payment card copybook
    When the copybook is processed with audit
    Then the audit event type should be "CopybookParse"
    And the audit context should have compliance profile "PciDss"
    And the audit context metadata should contain "cardholder_data"

  # Group 2: Operation Audit Events (4 scenarios)

  Scenario: Decode operation audit
    Given a simple audit copybook
    When a decode audit event is created
    Then the audit event type should be "DataTransformation"
    And the audit payload should contain "Decode"
    And the audit payload should contain "records_processed"

  Scenario: Encode operation audit
    Given a simple audit copybook
    When an encode audit event is created
    Then the audit event type should be "DataTransformation"
    And the audit payload should contain "Encode"

  Scenario: Field projection audit
    Given a simple audit copybook
    When a projection audit event is created
    Then the audit event type should be "DataValidation"
    And the audit payload should contain "validation_rules"

  Scenario: Dialect configuration change audit
    Given a simple audit copybook
    When a configuration change audit event is created
    Then the audit event type should be "ConfigurationChange"
    And the audit payload should contain "old_configuration"
    And the audit payload should contain "change_reason"

  # Group 3: Security and Access Events (3 scenarios)

  Scenario: Security classification
    Given security classification "Confidential"
    When a security event is created
    Then the audit event type should be "SecurityEvent"
    And the audit event severity should be "High"
    And the audit context should have security classification "Confidential"

  Scenario: Access event success
    When an access event is created with result "Success"
    Then the audit event type should be "AccessEvent"
    And the audit event severity should be "Info"
    And the audit payload should contain "Success"

  Scenario: Access event denied
    When an access event is created with result "Denied"
    Then the audit event type should be "AccessEvent"
    And the audit event severity should be "Medium"
    And the audit payload should contain "Denied"

  # Group 4: Error and Performance Events (3 scenarios)

  Scenario: Error audit event
    When an error audit event is created
    Then the audit event type should be "ErrorEvent"
    And the audit payload should contain "error_code"
    And the audit payload should contain "error_message"
    And the audit payload should contain "user_impact"

  Scenario: Performance measurement
    When a performance measurement event is created
    Then the audit event type should be "PerformanceMeasurement"
    And the audit payload should contain "metrics"
    And the audit event severity should be "Info"

  Scenario: Performance regression detected
    Given throughput metrics below baseline
    When a performance measurement event is created
    Then the audit event type should be "PerformanceMeasurement"
    And the audit event severity should be "Medium"
    And the audit payload should contain "regression_detected"

  # Group 5: Audit Trail Integrity (2 scenarios)

  Scenario: Audit events JSON serialization
    Given a financial SOX compliance copybook
    When the copybook is processed with audit
    And a security event is created
    Then the audit trail should have 2 events
    And all audit events should have required fields
    And the audit output should be in valid JSON format

  Scenario: Audit chain integrity
    Given a financial SOX compliance copybook
    When the copybook is processed with audit
    And a security event is created
    Then the audit chain should be valid

  # Group 6: Multi-Framework and Advanced (2 scenarios)

  Scenario: Multi-framework compliance
    Given compliance profile "SOX"
    And compliance profile "HIPAA"
    And compliance profile "GDPR"
    When a compliance check event is created
    Then the audit event type should be "ComplianceCheck"
    And the audit context should have compliance profile "SOX"
    And the audit context should have compliance profile "HIPAA"
    And the audit context should have compliance profile "GDPR"

  Scenario: Child context for nested operations
    When a child context is created for "nested_decode"
    Then the child context should have parent operation
    And the child context operation id should differ from parent
