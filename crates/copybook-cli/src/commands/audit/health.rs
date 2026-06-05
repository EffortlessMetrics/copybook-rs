//! Audit-trail health event parsing and normalization.

use super::*;

#[derive(Deserialize)]
struct RawHealthEvent {
    #[serde(default)]
    event_id: Option<String>,
    #[serde(default)]
    timestamp: Option<String>,
    #[serde(default)]
    source: Option<String>,
    #[serde(default)]
    event_type: Option<String>,
    #[serde(default)]
    integrity_hash: Option<String>,
    #[serde(default)]
    previous_hash: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub(super) struct HealthEventRecord {
    event_id: String,
    pub(super) timestamp: String,
    source: String,
    event_type: String,
    pub(super) integrity_hash: String,
    pub(super) previous_hash: Option<String>,
}

pub(super) fn parse_health_events(
    path: &Path,
) -> AuditResult<(Vec<HealthEventRecord>, Vec<String>)> {
    let content = fs::read_to_string(path)?;
    let mut parse_issues = Vec::new();

    if content.trim().is_empty() {
        return Ok((Vec::new(), parse_issues));
    }

    if let Ok(mut events) = serde_json::from_str::<Vec<RawHealthEvent>>(&content) {
        let mut parsed = Vec::with_capacity(events.len());
        for (index, raw_event) in events.drain(..).enumerate() {
            match normalize_health_event(raw_event) {
                Ok(event) => parsed.push(event),
                Err(err) => parse_issues.push(format!(
                    "entry {}: failed to normalize audit health event: {err}",
                    index + 1
                )),
            }
        }
        return Ok((parsed, parse_issues));
    }

    let mut parsed = Vec::new();
    for (line_number, raw_line) in content.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }

        match serde_json::from_str::<RawHealthEvent>(line) {
            Ok(raw_event) => match normalize_health_event(raw_event) {
                Ok(event) => parsed.push(event),
                Err(err) => parse_issues.push(format!(
                    "line {}: failed to normalize audit health event: {err}",
                    line_number + 1
                )),
            },
            Err(err) => parse_issues.push(format!(
                "line {}: invalid health event JSON: {err}",
                line_number + 1
            )),
        }
    }

    Ok((parsed, parse_issues))
}

pub(super) fn parse_audit_events_for_health(
    path: &Path,
) -> AuditResult<(Vec<AuditEvent>, Vec<String>)> {
    let content = fs::read_to_string(path)?;
    let mut parse_issues = Vec::new();

    if content.trim().is_empty() {
        return Ok((Vec::new(), parse_issues));
    }

    if let Ok(events) = serde_json::from_str::<Vec<AuditEvent>>(&content) {
        return Ok((events, parse_issues));
    }

    let mut events = Vec::new();
    for (line_number, raw_line) in content.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }

        match serde_json::from_str::<AuditEvent>(line) {
            Ok(event) => events.push(event),
            Err(err) => parse_issues.push(format!(
                "line {}: not a parseable AuditEvent JSON object: {err}",
                line_number + 1
            )),
        }
    }

    Ok((events, parse_issues))
}

fn normalize_health_event(raw: RawHealthEvent) -> Result<HealthEventRecord, String> {
    let event_id = raw
        .event_id
        .unwrap_or_else(|| format!("health-event-{}", generate_random_suffix()));
    let timestamp = raw
        .timestamp
        .unwrap_or_else(|| chrono::Utc::now().to_rfc3339());
    let source = raw.source.unwrap_or_else(|| "copybook-core".to_string());
    let event_type = raw.event_type.unwrap_or_else(|| "Unknown".to_string());
    let integrity_hash = raw.integrity_hash.unwrap_or_else(|| "".to_string());

    Ok(HealthEventRecord {
        event_id,
        timestamp,
        source,
        event_type,
        integrity_hash,
        previous_hash: raw.previous_hash,
    })
}
