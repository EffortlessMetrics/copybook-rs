use super::*;
use std::collections::HashMap;
use std::time::{Duration, SystemTime};

impl AlertSystem {
    pub fn new() -> Self {
        Self {
            alert_policies: vec![AlertPolicy {
                policy_id: "regression_alert".to_string(),
                trigger_conditions: vec![AlertTrigger::RegressionDetected {
                    min_severity: RegressionSeverity::Medium,
                }],
                severity: AlertSeverity::Warning,
                escalation_policy_id: Some("standard_escalation".to_string()),
            }],
            escalation_manager: EscalationManager::new(),
            notification_channels: vec![NotificationChannel::Slack {
                webhook_url: std::env::var("SLACK_WEBHOOK").unwrap_or_default(),
                channel: "#performance".to_string(),
            }],
        }
    }

    pub fn evaluate_alerts(
        &mut self,
        analysis: &RegressionAnalysis,
    ) -> Result<Vec<AlertResult>, Box<dyn std::error::Error>> {
        let mut results = Vec::new();

        // Clone policies to avoid borrow checker issues
        let policies = self.alert_policies.clone();
        for policy in &policies {
            if self.should_trigger_alert(&policy, analysis) {
                let alert_result = self.trigger_alert(&policy, analysis)?;
                results.push(alert_result);
            }
        }

        Ok(results)
    }

    fn should_trigger_alert(&self, policy: &AlertPolicy, analysis: &RegressionAnalysis) -> bool {
        for trigger in &policy.trigger_conditions {
            match trigger {
                AlertTrigger::RegressionDetected { min_severity } => {
                    if let RegressionStatus::MajorRegression { severity }
                    | RegressionStatus::CriticalRegression { severity } = &analysis.status
                    {
                        return matches!(
                            (min_severity, severity),
                            (RegressionSeverity::Low, _)
                                | (
                                    RegressionSeverity::Medium,
                                    RegressionSeverity::Medium
                                        | RegressionSeverity::High
                                        | RegressionSeverity::Critical
                                )
                                | (
                                    RegressionSeverity::High,
                                    RegressionSeverity::High | RegressionSeverity::Critical
                                )
                                | (RegressionSeverity::Critical, RegressionSeverity::Critical)
                        );
                    }
                }
                _ => {} // Other triggers not implemented in this simplified version
            }
        }
        false
    }

    fn trigger_alert(
        &mut self,
        policy: &AlertPolicy,
        _analysis: &RegressionAnalysis,
    ) -> Result<AlertResult, Box<dyn std::error::Error>> {
        let alert_id = format!(
            "alert_{}",
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)?
                .as_secs()
        );

        let notifications_sent = self.send_notifications(&alert_id, policy)?;

        Ok(AlertResult {
            alert_id,
            policy_id: policy.policy_id.clone(),
            severity: policy.severity.clone(),
            message: "Performance regression detected".to_string(),
            notifications_sent,
            escalation_triggered: false,
        })
    }

    fn send_notifications(
        &self,
        _alert_id: &str,
        _policy: &AlertPolicy,
    ) -> Result<Vec<NotificationResult>, Box<dyn std::error::Error>> {
        // Simplified notification sending
        Ok(vec![NotificationResult {
            channel: "slack".to_string(),
            success: true,
            error_message: None,
            delivery_time: Duration::from_millis(150),
        }])
    }
}

impl EscalationManager {
    pub fn new() -> Self {
        Self {
            escalation_policies: HashMap::new(),
            active_escalations: HashMap::new(),
        }
    }
}
