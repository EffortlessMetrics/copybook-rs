//! Golden test utilities with SHA-256 validation

use sha2::{Sha256, Digest};
use serde::{Deserialize, Serialize};

/// A golden test case with SHA-256 validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoldenTest {
    /// Test name
    pub name: String,
    /// Copybook text
    pub copybook: String,
    /// Input data SHA-256 hash
    pub input_hash: String,
    /// Expected output SHA-256 hash
    pub expected_output_hash: String,
    /// Test metadata
    pub metadata: GoldenTestMetadata,
}

/// Metadata for golden tests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoldenTestMetadata {
    /// Test creation timestamp
    pub created_at: String,
    /// Test description
    pub description: String,
    /// Test tags
    pub tags: Vec<String>,
}

impl GoldenTest {
    /// Create a new golden test
    pub fn new(name: &str, copybook: &str, data: &[u8]) -> Self {
        let input_hash = Self::hash_bytes(data);
        
        Self {
            name: name.to_string(),
            copybook: copybook.to_string(),
            input_hash,
            expected_output_hash: String::new(), // Will be set after first run
            metadata: GoldenTestMetadata {
                created_at: chrono::Utc::now().to_rfc3339(),
                description: format!("Golden test: {}", name),
                tags: vec!["synthetic".to_string()],
            },
        }
    }
    
    /// Calculate SHA-256 hash of bytes
    pub fn hash_bytes(data: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(data);
        format!("{:x}", hasher.finalize())
    }
    
    /// Validate output against expected hash
    pub fn validate_output(&self, output: &[u8]) -> bool {
        if self.expected_output_hash.is_empty() {
            return true; // First run, no expected hash yet
        }
        
        let actual_hash = Self::hash_bytes(output);
        actual_hash == self.expected_output_hash
    }
    
    /// Update expected output hash
    pub fn update_expected_hash(&mut self, output: &[u8]) {
        self.expected_output_hash = Self::hash_bytes(output);
    }
}