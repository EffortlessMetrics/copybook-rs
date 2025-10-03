# [Task]: Implement or Remove Unused `config` Fields in Compliance Validators

**Issue Description**

In `copybook-core/src/audit/compliance.rs`, the compliance validator structs (`SoxValidator`, `HipaaValidator`, `GdprValidator`, `PciDssValidator`) are stubbed out. Each contains a `config` field that is initialized but **never used**, with the compiler silenced by `#[allow(dead_code)]`. This pattern indicates an incomplete feature.

While the `ComplianceEngine` itself is used in tests, the validators do not respect their configuration, meaning their behavior is static and cannot be controlled. The configuration fields (e.g., `SoxConfig.financial_data_validation`) clearly suggest that the validation logic was intended to be conditional.

**Files and Locations:**

- `copybook-core/src/audit/compliance.rs:244` (`SoxValidator`)
- `copybook-core/src/audit/compliance.rs:372` (`HipaaValidator`)
- `copybook-core/src/audit/compliance.rs:499` (`GdprValidator`)
- `copybook-core/src/audit/compliance.rs:616` (`PciDssValidator`)

**Proposed Fix: Implement Configuration Logic**

The validation logic within each validator should be updated to use its `config` field, making the compliance checks fully configurable as intended. If a particular check is not meant to be configurable, its flag should be removed from the config struct.

Here are concrete implementation examples for each validator:

### 1. `SoxValidator`

The `SoxConfig` has a `financial_data_validation` flag. This should be used to gate the execution of `validate_financial_data_controls`.

```rust
// copybook-core/src/audit/compliance.rs

#[async_trait::async_trait]
impl ComplianceValidator for SoxValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult> {
        let mut violations = Vec::new();
        let mut warnings = Vec::new();

        // Use the config flag to control validation
        if self.config.financial_data_validation {
            violations.extend(self.validate_financial_data_controls(context));
        }

        // ... (rest of the function)

        Ok(ComplianceValidationResult { violations, warnings })
    }
    // ...
}
```

### 2. `HipaaValidator`

The `HipaaConfig` has a `minimum_necessary_enforcement` flag. This can be used to control the warning for missing justification.

```rust
// copybook-core/src/audit/compliance.rs

#[async_trait::async_trait]
impl ComplianceValidator for HipaaValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult> {
        let mut violations = Vec::new();
        let mut warnings = Vec::new();

        if matches!(context.security.classification, SecurityClassification::PHI) {
            if self.config.phi_encryption_required { // Example usage
                violations.extend(self.validate_phi_protection(context));
            }

            // Use the config flag for minimum necessary enforcement
            if self.config.minimum_necessary_enforcement
                && !context
                    .metadata
                    .contains_key("minimum_necessary_justification")
            {
                warnings.push(ComplianceWarning {
                    warning_id: "HIPAA-MIN-001".to_string(),
                    title: "Minimum Necessary Justification Missing".to_string(),
                    // ...
                });
            }
        }

        Ok(ComplianceValidationResult { violations, warnings })
    }
    // ...
}
```

### 3. `GdprValidator`

The `GdprConfig` has a `legal_basis_validation` flag. This should control the check for legal basis documentation.

```rust
// copybook-core/src/audit/compliance.rs

impl GdprValidator {
    // ...
    fn validate_data_protection_principles(
        &self,
        context: &AuditContext,
    ) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // Use the config flag to control validation
        if self.config.legal_basis_validation && !self.has_legal_basis_documentation(context) {
            violations.push(ComplianceViolation {
                violation_id: "GDPR-ART5-001".to_string(),
                regulation: "GDPR Article 5(1)(a)".to_string(),
                // ...
            });
        }

        // ...
        violations
    }
    // ...
}
```

### 4. `PciDssValidator`

This validator is a complete placeholder. The implementation should be built out to use the `cardholder_data_validation` flag from `PciDssConfig`.

```rust
// copybook-core/src/audit/compliance.rs

#[async_trait::async_trait]
impl ComplianceValidator for PciDssValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult> {
        let mut violations = Vec::new();

        if self.config.cardholder_data_validation && context.metadata.contains_key("has_cardholder_data") {
            // Placeholder for actual PCI DSS validation logic
            // For example, check for PAN truncation, encryption, etc.
            // violations.push(...);
        }

        Ok(ComplianceValidationResult {
            violations,
            warnings: Vec::new(),
        })
    }
    // ...
}
```

By implementing these changes, the compliance engine will become a flexible and powerful tool, fulfilling its intended design and removing the technical debt of the stubbed-out implementation.