//! Access-log parsing for audit security checks.

use super::*;

#[allow(
    clippy::too_many_arguments,
    clippy::fn_params_excessive_bools,
    clippy::used_underscore_binding
)]
#[derive(Deserialize)]
struct RawAccessEvent {
    #[serde(default)]
    user: Option<String>,
    #[serde(default)]
    user_id: Option<String>,
    #[serde(default)]
    action: Option<String>,
    #[serde(default)]
    access_type: Option<String>,
    #[serde(default)]
    resource_type: Option<String>,
    #[serde(default)]
    resource: Option<String>,
    #[serde(default)]
    resource_id: Option<String>,
    #[serde(default)]
    result: Option<String>,
    #[serde(default)]
    status: Option<String>,
    #[serde(default)]
    source_ip: Option<String>,
    #[serde(default)]
    user_agent: Option<String>,
    #[serde(default)]
    source_ip_address: Option<String>,
}

fn parse_access_result(raw: Option<&str>) -> AccessResult {
    match raw.unwrap_or_default().to_ascii_lowercase().as_str() {
        "deny" | "denied" | "forbidden" | "failure" | "failed" => AccessResult::Denied,
        "error" | "invalid" | "blocked" => AccessResult::Failed,
        _ => AccessResult::Success,
    }
}

pub(super) fn parse_access_events(path: &Path) -> AuditResult<(Vec<AccessEvent>, Vec<String>)> {
    let file = fs::File::open(path)?;
    let reader = BufReader::new(file);
    let mut events = Vec::new();
    let mut parse_issues = Vec::new();

    for (line_number, line_result) in reader.lines().enumerate() {
        let line = match line_result {
            Ok(value) => value,
            Err(err) => {
                parse_issues.push(format!(
                    "line {}: failed to read access event line: {err}",
                    line_number + 1
                ));
                continue;
            }
        };
        if line.trim().is_empty() {
            continue;
        }

        match serde_json::from_str::<RawAccessEvent>(&line) {
            Ok(raw) => {
                let user_id = raw
                    .user
                    .or(raw.user_id)
                    .unwrap_or_else(|| "unknown-user".to_string());
                let access_type = raw
                    .action
                    .or(raw.access_type)
                    .unwrap_or_else(|| "read".to_string());
                let resource_type = raw.resource_type.unwrap_or_else(|| "resource".to_string());
                let resource_id = raw
                    .resource
                    .or(raw.resource_id)
                    .unwrap_or_else(|| "generic".to_string());
                let result = parse_access_result(raw.result.as_deref().or(raw.status.as_deref()));

                events.push(AccessEvent {
                    user_id,
                    resource_type,
                    resource_id,
                    access_type,
                    source_ip: raw.source_ip.or(raw.source_ip_address),
                    user_agent: raw.user_agent,
                    result,
                    timestamp: None,
                });
            }
            Err(err) => {
                parse_issues.push(format!(
                    "line {}: invalid access event JSON: {err}",
                    line_number + 1
                ));
            }
        }
    }

    Ok((events, parse_issues))
}
