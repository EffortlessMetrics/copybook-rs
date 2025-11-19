//! Audit Context System
//!
//! Provides comprehensive context tracking for all audit operations including
//! user context, environment information, security classification, and
//! compliance requirements.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

use super::{ComplianceProfile, generate_audit_id, generate_lightweight_audit_id};

/// Comprehensive audit context for all operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditContext {
    /// Unique operation identifier for correlation
    pub operation_id: String,

    /// User context information
    pub user: Option<String>,

    /// System environment context
    pub environment: EnvironmentContext,

    /// Processing configuration context
    pub processing_config: ProcessingConfig,

    /// Security context and classification
    pub security: SecurityContext,

    /// Compliance requirements for this operation
    pub compliance_profiles: Vec<ComplianceProfile>,

    /// Custom metadata for operation-specific context
    pub metadata: HashMap<String, String>,

    /// Operation start timestamp
    pub started_at: String,

    /// Parent operation ID for nested operations
    pub parent_operation_id: Option<String>,
}

impl AuditContext {
    /// Create a new audit context with default values
    pub fn new() -> Self {
        Self {
            operation_id: generate_audit_id(),
            user: None,
            environment: EnvironmentContext::current(),
            processing_config: ProcessingConfig::default(),
            security: SecurityContext::default(),
            compliance_profiles: Vec::new(),
            metadata: HashMap::new(),
            started_at: chrono::Utc::now().to_rfc3339(),
            parent_operation_id: None,
        }
    }

    /// Create a lightweight audit context for performance-critical operations
    /// Avoids expensive system calls and allocations for better performance
    pub fn new_lightweight() -> Self {
        Self {
            operation_id: generate_lightweight_audit_id(),
            user: None,
            environment: EnvironmentContext::lightweight(),
            processing_config: ProcessingConfig::default(),
            security: SecurityContext::default(),
            compliance_profiles: Vec::new(),
            metadata: HashMap::new(),
            started_at: "1970-01-01T00:00:00Z".to_string(), // Avoid timestamp generation
            parent_operation_id: None,
        }
    }

    /// Create an audit context from a reusable template to avoid repeated allocations
    pub fn from_template(template: &Self, operation_id: impl Into<String>) -> Self {
        let mut context = template.clone();
        context.operation_id = operation_id.into();
        context.started_at = chrono::Utc::now().to_rfc3339();
        context.parent_operation_id = None;
        context
    }

    /// Set the operation identifier
    #[must_use]
    pub fn with_operation_id(mut self, operation_id: impl Into<String>) -> Self {
        self.operation_id = operation_id.into();
        self
    }

    /// Set the user context
    #[must_use]
    pub fn with_user(mut self, user: impl Into<String>) -> Self {
        self.user = Some(user.into());
        self
    }

    /// Set the environment context
    #[must_use]
    pub fn with_environment(mut self, environment: EnvironmentContext) -> Self {
        self.environment = environment;
        self
    }

    /// Set the processing configuration
    #[must_use]
    pub fn with_processing_config(mut self, config: ProcessingConfig) -> Self {
        self.processing_config = config;
        self
    }

    /// Set the security context
    #[must_use]
    pub fn with_security_context(mut self, security: SecurityContext) -> Self {
        self.security = security;
        self
    }

    /// Add compliance profiles
    #[must_use]
    pub fn with_compliance_profiles(mut self, profiles: &[ComplianceProfile]) -> Self {
        self.compliance_profiles = profiles.to_vec();
        self
    }

    /// Add a single compliance profile
    #[must_use]
    pub fn with_compliance_profile(mut self, profile: ComplianceProfile) -> Self {
        self.compliance_profiles.push(profile);
        self
    }

    /// Set security classification
    #[must_use]
    pub fn with_security_classification(mut self, classification: SecurityClassification) -> Self {
        self.security.classification = classification;
        self
    }

    /// Add metadata key-value pair
    #[must_use]
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Set parent operation ID for nested operations
    #[must_use]
    pub fn with_parent_operation_id(mut self, parent_id: impl Into<String>) -> Self {
        self.parent_operation_id = Some(parent_id.into());
        self
    }

    /// Create a child context for nested operations
    #[must_use]
    pub fn create_child_context(&self, child_operation_id: impl Into<String>) -> Self {
        let mut child = self.clone();
        child.operation_id = child_operation_id.into();
        child.parent_operation_id = Some(self.operation_id.clone());
        child.started_at = chrono::Utc::now().to_rfc3339();
        child
    }

    /// Create a lightweight child context for performance-critical operations
    /// Avoids timestamp generation and expensive cloning
    #[must_use]
    pub fn create_lightweight_child_context(&self, child_operation_id: impl Into<String>) -> Self {
        Self {
            operation_id: child_operation_id.into(),
            user: self.user.clone(),
            environment: EnvironmentContext::lightweight(),
            processing_config: ProcessingConfig::default(),
            security: self.security.clone(),
            compliance_profiles: self.compliance_profiles.clone(),
            metadata: HashMap::new(), // Don't clone metadata for performance
            started_at: "1970-01-01T00:00:00Z".to_string(),
            parent_operation_id: Some(self.operation_id.clone()),
        }
    }

    /// Check if this context requires specific compliance validation
    pub fn requires_compliance(&self, profile: ComplianceProfile) -> bool {
        self.compliance_profiles.contains(&profile)
    }

    /// Get the effective security level for this context
    pub fn effective_security_level(&self) -> SecurityLevel {
        // Determine effective security level based on classification and compliance
        match self.security.classification {
            SecurityClassification::Public => SecurityLevel::Low,
            SecurityClassification::Internal => SecurityLevel::Medium,
            SecurityClassification::Confidential => {
                if self.compliance_profiles.contains(&ComplianceProfile::SOX)
                    || self.compliance_profiles.contains(&ComplianceProfile::HIPAA)
                {
                    SecurityLevel::High
                } else {
                    SecurityLevel::Medium
                }
            }
            SecurityClassification::MaterialTransaction | SecurityClassification::PHI => {
                SecurityLevel::Critical
            }
        }
    }
}

impl Default for AuditContext {
    fn default() -> Self {
        Self::new()
    }
}

/// System environment context information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentContext {
    /// System hostname or identifier
    pub hostname: String,

    /// Process ID
    pub process_id: u32,

    /// System architecture (x86_64, aarch64, etc.)
    pub system_arch: String,

    /// copybook-rs version
    pub version: String,

    /// Command line invocation
    pub command_line: Vec<String>,

    /// Environment variables (filtered for security)
    pub environment_variables: HashMap<String, String>,

    /// Working directory
    pub working_directory: String,

    /// System timestamp when context was created
    pub system_timestamp: u64,
}

impl EnvironmentContext {
    /// Create environment context from current system state
    pub fn current() -> Self {
        Self {
            hostname: Self::get_hostname(),
            process_id: std::process::id(),
            system_arch: std::env::consts::ARCH.to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            command_line: std::env::args().collect(),
            environment_variables: Self::get_filtered_env_vars(),
            working_directory: std::env::current_dir()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string(),
            system_timestamp: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }

    /// Create lightweight environment context for performance-critical operations
    /// Avoids expensive system calls and allocations
    #[inline]
    pub fn lightweight() -> Self {
        Self {
            hostname: "performance-host".to_string(),
            process_id: std::process::id(),
            system_arch: std::env::consts::ARCH.to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            command_line: vec!["copybook-rs".to_string()],
            environment_variables: HashMap::new(),
            working_directory: "/tmp".to_string(),
            system_timestamp: 0, // Avoid system time call
        }
    }

    fn get_hostname() -> String {
        std::env::var("HOSTNAME")
            .or_else(|_| std::env::var("COMPUTERNAME"))
            .unwrap_or_else(|_| "unknown".to_string())
    }

    fn get_filtered_env_vars() -> HashMap<String, String> {
        let allowed_vars = [
            "USER", "USERNAME", "HOME", "PATH", "SHELL", "TERM", "LANG", "LC_ALL", "TZ", "PWD",
        ];

        std::env::vars()
            .filter(|(key, _)| allowed_vars.contains(&key.as_str()))
            .collect()
    }
}

/// Processing configuration context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessingConfig {
    /// Input file or data source information
    pub input_source: Option<String>,

    /// Output destination information
    pub output_destination: Option<String>,

    /// Processing mode (batch, streaming, etc.)
    pub processing_mode: ProcessingMode,

    /// Batch size for batch processing
    pub batch_size: Option<usize>,

    /// Number of processing threads
    pub thread_count: Option<usize>,

    /// Processing timeout settings
    pub timeout_seconds: Option<u64>,

    /// Memory limit settings
    pub memory_limit_mb: Option<usize>,

    /// Retry configuration
    pub retry_config: RetryConfig,
}

impl Default for ProcessingConfig {
    fn default() -> Self {
        Self {
            input_source: None,
            output_destination: None,
            processing_mode: ProcessingMode::Batch,
            batch_size: Some(1000),
            thread_count: Some(1),
            timeout_seconds: Some(3600), // 1 hour default
            memory_limit_mb: Some(512),
            retry_config: RetryConfig::default(),
        }
    }
}

/// Processing mode enumeration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ProcessingMode {
    Batch,
    Streaming,
    Interactive,
    Scheduled,
}

/// Retry configuration for processing operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RetryConfig {
    pub max_retries: u32,
    pub initial_delay_ms: u64,
    pub max_delay_ms: u64,
    pub backoff_multiplier: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_retries: 3,
            initial_delay_ms: 100,
            max_delay_ms: 30000,
            backoff_multiplier: 2.0,
        }
    }
}

/// Security context and classification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityContext {
    /// Data security classification
    pub classification: SecurityClassification,

    /// Access control requirements
    pub access_control: AccessControlRequirements,

    /// Encryption requirements
    pub encryption: EncryptionRequirements,

    /// Network security context
    pub network: NetworkSecurityContext,

    /// Audit trail requirements
    pub audit_requirements: AuditRequirements,
}

impl Default for SecurityContext {
    fn default() -> Self {
        Self {
            classification: SecurityClassification::Internal,
            access_control: AccessControlRequirements::default(),
            encryption: EncryptionRequirements::default(),
            network: NetworkSecurityContext::default(),
            audit_requirements: AuditRequirements::default(),
        }
    }
}

/// Data security classification levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum SecurityClassification {
    Public,
    Internal,
    Confidential,
    MaterialTransaction,
    PHI, // Protected Health Information
}

/// Effective security level derived from classification and compliance
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum SecurityLevel {
    Low,
    Medium,
    High,
    Critical,
}

/// Access control requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::struct_excessive_bools)]
pub struct AccessControlRequirements {
    pub authentication_required: bool,
    pub authorization_required: bool,
    pub multi_factor_authentication: bool,
    pub role_based_access: bool,
    pub segregation_of_duties: bool,
}

impl Default for AccessControlRequirements {
    fn default() -> Self {
        Self {
            authentication_required: true,
            authorization_required: true,
            multi_factor_authentication: false,
            role_based_access: true,
            segregation_of_duties: false,
        }
    }
}

/// Encryption requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EncryptionRequirements {
    pub at_rest: EncryptionStandard,
    pub in_transit: EncryptionStandard,
    pub key_management: KeyManagementRequirements,
}

impl Default for EncryptionRequirements {
    fn default() -> Self {
        Self {
            at_rest: EncryptionStandard::AES256,
            in_transit: EncryptionStandard::TLS12,
            key_management: KeyManagementRequirements::default(),
        }
    }
}

/// Encryption standards
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EncryptionStandard {
    None,
    AES128,
    AES256,
    TLS12,
    TLS13,
}

/// Key management requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeyManagementRequirements {
    pub key_rotation_days: Option<u32>,
    pub hardware_security_module: bool,
    pub key_escrow: bool,
}

impl Default for KeyManagementRequirements {
    fn default() -> Self {
        Self {
            key_rotation_days: Some(90),
            hardware_security_module: false,
            key_escrow: false,
        }
    }
}

/// Network security context
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct NetworkSecurityContext {
    pub source_ip: Option<String>,
    pub allowed_networks: Vec<String>,
    pub vpn_required: bool,
    pub firewall_rules: Vec<String>,
}

/// Audit trail requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::struct_excessive_bools)]
pub struct AuditRequirements {
    pub comprehensive_logging: bool,
    pub integrity_protection: bool,
    pub real_time_monitoring: bool,
    pub retention_days: u32,
    pub tamper_detection: bool,
}

impl Default for AuditRequirements {
    fn default() -> Self {
        Self {
            comprehensive_logging: true,
            integrity_protection: true,
            real_time_monitoring: false,
            retention_days: 2555, // 7 years default
            tamper_detection: true,
        }
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_audit_context_creation() {
        let context = AuditContext::new();

        assert!(context.operation_id.starts_with("audit-"));
        assert!(context.user.is_none());
        assert_eq!(context.compliance_profiles.len(), 0);
    }

    #[test]
    fn test_audit_context_builder_pattern() {
        let context = AuditContext::new()
            .with_operation_id("test-operation")
            .with_user("test_user")
            .with_compliance_profile(ComplianceProfile::SOX)
            .with_security_classification(SecurityClassification::Confidential);

        assert_eq!(context.operation_id, "test-operation");
        assert_eq!(context.user, Some("test_user".to_string()));
        assert!(context.requires_compliance(ComplianceProfile::SOX));
        assert_eq!(
            context.security.classification,
            SecurityClassification::Confidential
        );
    }

    #[test]
    fn test_child_context_creation() {
        let parent = AuditContext::new()
            .with_operation_id("parent-operation")
            .with_user("test_user");

        let child = parent.create_child_context("child-operation");

        assert_eq!(child.operation_id, "child-operation");
        assert_eq!(
            child.parent_operation_id,
            Some("parent-operation".to_string())
        );
        assert_eq!(child.user, Some("test_user".to_string()));
    }

    #[test]
    fn test_effective_security_level() {
        let low_security =
            AuditContext::new().with_security_classification(SecurityClassification::Public);
        assert_eq!(low_security.effective_security_level(), SecurityLevel::Low);

        let high_security = AuditContext::new()
            .with_security_classification(SecurityClassification::Confidential)
            .with_compliance_profile(ComplianceProfile::SOX);
        assert_eq!(
            high_security.effective_security_level(),
            SecurityLevel::High
        );

        let critical_security =
            AuditContext::new().with_security_classification(SecurityClassification::PHI);
        assert_eq!(
            critical_security.effective_security_level(),
            SecurityLevel::Critical
        );
    }

    #[test]
    fn test_environment_context_current() {
        let env_context = EnvironmentContext::current();

        assert!(!env_context.hostname.is_empty());
        assert!(env_context.process_id > 0);
        assert!(!env_context.system_arch.is_empty());
        assert!(!env_context.version.is_empty());
    }
}
