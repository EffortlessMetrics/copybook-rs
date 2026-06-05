use super::*;
impl CiIntegrator {
    pub fn new() -> Self {
        Self {
            performance_gates: vec![
                PerformanceGate {
                    gate_id: "display_throughput".to_string(),
                    metric_type: GateMetricType::DisplayThroughput,
                    threshold: GateThreshold {
                        max_regression_percent: 2.0,
                        confidence_level: 0.95,
                        require_statistical_significance: true,
                    },
                    action: GateAction::Block,
                },
                PerformanceGate {
                    gate_id: "comp3_throughput".to_string(),
                    metric_type: GateMetricType::Comp3Throughput,
                    threshold: GateThreshold {
                        max_regression_percent: 2.0,
                        confidence_level: 0.95,
                        require_statistical_significance: true,
                    },
                    action: GateAction::Block,
                },
            ],
            notification_config: NotificationConfig {
                slack_webhook: std::env::var("SLACK_WEBHOOK").ok(),
                email_recipients: Vec::new(),
                github_integration: None,
            },
            integration_config: CiIntegrationConfig {
                platforms: vec![CiPlatform::GitHubActions],
                artifact_storage: ArtifactStorageConfig {
                    store_raw_data: true,
                    store_analysis_reports: true,
                    retention_days: 30,
                },
                reporting_config: ReportingConfig {
                    generate_html_reports: true,
                    generate_json_reports: true,
                    include_trend_analysis: true,
                },
            },
        }
    }

    pub fn execute_performance_gates(
        &self,
        analysis: &RegressionAnalysis,
    ) -> Result<Vec<GateResult>, Box<dyn std::error::Error>> {
        let mut results = Vec::new();

        for gate in &self.performance_gates {
            let result = self.evaluate_gate(gate, analysis)?;
            results.push(result);
        }

        Ok(results)
    }

    fn evaluate_gate(
        &self,
        gate: &PerformanceGate,
        analysis: &RegressionAnalysis,
    ) -> Result<GateResult, Box<dyn std::error::Error>> {
        let (measured_value, threshold_value) = match gate.metric_type {
            GateMetricType::DisplayThroughput => {
                let change = analysis
                    .metrics_comparison
                    .throughput_changes
                    .iter()
                    .find(|c| c.metric_name.contains("display"))
                    .map_or(0.0, |c| c.change_percent);
                (change, gate.threshold.max_regression_percent)
            }
            GateMetricType::Comp3Throughput => {
                let change = analysis
                    .metrics_comparison
                    .throughput_changes
                    .iter()
                    .find(|c| c.metric_name.contains("comp3"))
                    .map_or(0.0, |c| c.change_percent);
                (change, gate.threshold.max_regression_percent)
            }
            _ => (0.0, gate.threshold.max_regression_percent), // Simplified
        };

        let status = if measured_value <= threshold_value {
            GateStatus::Passed
        } else if measured_value <= threshold_value * 1.5 {
            GateStatus::Warning
        } else {
            GateStatus::Failed
        };

        let message = format!(
            "Gate {}: measured {:.2}%, threshold {:.2}%",
            gate.gate_id, measured_value, threshold_value
        );

        Ok(GateResult {
            gate_id: gate.gate_id.clone(),
            status,
            measured_value,
            threshold_value,
            message,
        })
    }
}
