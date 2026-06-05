//! Shared audit command helpers for formatting, routing, and enum mapping.

use super::*;

#[derive(Deserialize)]
pub(super) struct ReportSectionFile {
    #[serde(default)]
    pub(super) compliance_validation: Option<Value>,
    #[serde(default)]
    pub(super) lineage_analysis: Option<Value>,
    #[serde(default)]
    pub(super) performance_audit: Option<Value>,
    #[serde(default)]
    pub(super) security_audit: Option<Value>,
}

pub(super) fn generate_random_suffix() -> String {
    chrono::Utc::now()
        .timestamp_nanos_opt()
        .map_or_else(|| "fallback".to_string(), |value| value.to_string())
}

#[inline]
pub(super) fn bytes_to_gbps(bytes_per_second: f64) -> f64 {
    bytes_per_second / (1024.0_f64 * 1024.0_f64 * 1024.0_f64)
}

#[inline]
pub(super) fn bytes_to_mbps(bytes_per_second: f64) -> f64 {
    bytes_per_second / (1024.0_f64 * 1024.0_f64)
}

pub(super) fn combine_exit_code(current: ExitCode, candidate: ExitCode) -> ExitCode {
    if candidate.precedence() >= current.precedence() {
        candidate
    } else {
        current
    }
}

pub(super) fn sidecar_path(output: &Path, suffix: &str) -> PathBuf {
    let stem = output
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("audit");
    let ext = output
        .extension()
        .and_then(|s| s.to_str())
        .unwrap_or("json");
    let file_name = format!("{stem}.{suffix}.{ext}");
    output
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .join(file_name)
}

fn parse_compliance_profile(raw: &str) -> Option<ComplianceProfile> {
    match raw.to_ascii_lowercase().as_str() {
        "sox" => Some(ComplianceProfile::SOX),
        "hipaa" => Some(ComplianceProfile::HIPAA),
        "gdpr" => Some(ComplianceProfile::GDPR),
        "pci" | "pcidss" | "pci_dss" | "pci-dss" => Some(ComplianceProfile::PciDss),
        _ => None,
    }
}

pub(super) fn parse_compliance_profiles(
    profiles: &str,
) -> Result<(Vec<ComplianceProfile>, Vec<String>), io::Error> {
    let mut parsed = Vec::new();
    let mut names = Vec::new();
    let mut seen = HashSet::new();

    for raw in profiles
        .split(',')
        .map(str::trim)
        .filter(|item| !item.is_empty())
    {
        if let Some(profile) = parse_compliance_profile(raw) {
            if seen.insert(profile) {
                names.push(raw.to_string());
                parsed.push(profile);
            }
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("invalid compliance profile: '{raw}'"),
            ));
        }
    }

    Ok((parsed, names))
}

pub(super) fn transformation_label(transformation: &TransformationType) -> &'static str {
    match transformation {
        TransformationType::DirectMapping => "direct_mapping",
        TransformationType::TypeConversion => "type_conversion",
        TransformationType::Aggregation => "aggregation",
        TransformationType::Calculation => "calculation",
        TransformationType::Lookup => "lookup",
        TransformationType::Filter => "filter",
    }
}

pub(super) fn risk_level_rank(level: &RiskLevel) -> u8 {
    match level {
        RiskLevel::Low => 1,
        RiskLevel::Medium => 2,
        RiskLevel::High => 3,
        RiskLevel::Critical => 4,
    }
}

pub(super) fn security_classification(
    classification: Option<DataClassification>,
) -> copybook_core::audit::context::SecurityClassification {
    match classification {
        Some(DataClassification::Public) => {
            copybook_core::audit::context::SecurityClassification::Public
        }
        Some(DataClassification::Internal) => {
            copybook_core::audit::context::SecurityClassification::Internal
        }
        Some(DataClassification::Confidential) => {
            copybook_core::audit::context::SecurityClassification::Confidential
        }
        Some(DataClassification::MaterialTransaction) => {
            copybook_core::audit::context::SecurityClassification::MaterialTransaction
        }
        Some(DataClassification::PHI) => copybook_core::audit::context::SecurityClassification::PHI,
        None => copybook_core::audit::context::SecurityClassification::Internal,
    }
}

pub(super) fn clamp_u64(value: f64) -> u64 {
    if !value.is_finite() || value <= 0.0 {
        0
    } else if value >= u64::MAX as f64 {
        u64::MAX
    } else {
        value.round() as u64
    }
}

pub(super) fn report_section_from_file(path: &Path) -> AuditResult<ReportSectionFile> {
    let content = fs::read_to_string(path)?;
    serde_json::from_str::<ReportSectionFile>(&content).map_err(|err| err.into())
}
