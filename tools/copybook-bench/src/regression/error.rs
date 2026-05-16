use std::fmt;

#[derive(Debug)]
pub enum RegressionDetectionError {
    BaselineNotFound(String),
    InsufficientData(String),
    StatisticalAnalysisError(String),
    StorageError(String),
    EnvironmentError(String),
}

impl fmt::Display for RegressionDetectionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BaselineNotFound(msg) => write!(f, "Baseline not found: {msg}"),
            Self::InsufficientData(msg) => write!(f, "Insufficient data: {msg}"),
            Self::StatisticalAnalysisError(msg) => write!(f, "Statistical analysis error: {msg}"),
            Self::StorageError(msg) => write!(f, "Storage error: {msg}"),
            Self::EnvironmentError(msg) => write!(f, "Environment error: {msg}"),
        }
    }
}

impl std::error::Error for RegressionDetectionError {}
