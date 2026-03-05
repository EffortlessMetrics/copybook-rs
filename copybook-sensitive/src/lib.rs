#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Sensitive-field keyword detection helpers.

const SENSITIVE_KEYWORDS: &[&str] = &[
    "SSN",
    "SOCIAL",
    "PASSWORD",
    "CREDIT",
    "CARD",
    "ACCOUNT",
    "DOB",
    "BIRTH",
    "SALARY",
    "BALANCE",
    "ROUTING",
    "TAX-ID",
    "MEDICAL",
    "PATIENT",
    "DIAGNOSIS",
    "PIN",
    "ENCRYPT",
    "SECRET",
];

/// Returns true when a field name matches a default sensitive keyword.
#[must_use]
pub fn is_sensitive_field_name(name: &str) -> bool {
    let candidate = name.to_ascii_uppercase();
    SENSITIVE_KEYWORDS
        .iter()
        .any(|keyword| candidate.contains(keyword))
}

#[cfg(test)]
mod tests {
    use super::is_sensitive_field_name;

    #[test]
    fn flags_sensitive_keywords_case_insensitively() {
        assert!(is_sensitive_field_name("customer_ssn"));
        assert!(is_sensitive_field_name("password_hash"));
        assert!(is_sensitive_field_name("employeeSalary"));
        assert!(is_sensitive_field_name("routing_number"));
    }

    #[test]
    fn ignores_non_sensitive_names() {
        assert!(!is_sensitive_field_name("order_id"));
        assert!(!is_sensitive_field_name("transaction_date"));
        assert!(!is_sensitive_field_name("postal_code"));
    }
}
