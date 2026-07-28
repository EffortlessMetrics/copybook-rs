use super::*;
use sha2::{Digest, Sha256};
use std::time::{Instant, SystemTime};

impl PerformanceRegressionDetector {
    /// Create new performance regression detector
    pub fn new() -> Self {
        Self {
            baseline_repository: BaselineRepository::new(),
            statistical_analyzer: StatisticalRegressionAnalyzer::new(),
            ci_integrator: CiIntegrator::new(),
            alert_system: AlertSystem::new(),
        }
    }

    /// Detect performance regression with comprehensive analysis
    pub fn detect_performance_regression(
        &mut self,
        current_metrics: PerformanceMetrics,
    ) -> Result<RegressionAnalysis, Box<dyn std::error::Error>> {
        // Find compatible baseline for comparison
        let environment = self.get_current_environment()?;
        let baseline_metadata = self
            .baseline_repository
            .find_compatible_baseline(&environment)?
            .ok_or("No compatible baseline found")?;

        // Perform statistical analysis
        let comparison = self
            .statistical_analyzer
            .compare_metrics(&baseline_metadata.performance_metrics, &current_metrics)?;

        // Run statistical tests
        let statistical_tests = self
            .statistical_analyzer
            .run_statistical_tests(&baseline_metadata.performance_metrics, &current_metrics)?;

        // Determine regression status
        let status = self.determine_regression_status(&comparison, &statistical_tests)?;

        // Calculate confidence score
        let confidence_score = self.calculate_confidence_score(&statistical_tests);

        // Generate recommendations
        let recommendations = self.generate_recommendations(&status, &comparison)?;

        Ok(RegressionAnalysis {
            status,
            metrics_comparison: comparison,
            statistical_tests,
            confidence_score,
            recommendations,
        })
    }

    /// Establish new performance baseline
    pub fn establish_baseline(
        &mut self,
        metrics: PerformanceMetrics,
        environment: EnvironmentInfo,
    ) -> Result<String, Box<dyn std::error::Error>> {
        // Generate baseline ID
        let baseline_id = self.generate_baseline_id(&environment);

        // Calculate statistical properties
        let statistical_properties = self
            .statistical_analyzer
            .calculate_statistical_properties(&metrics)?;

        // Create baseline metadata
        let baseline_metadata = BaselineMetadata {
            baseline_id: baseline_id.clone(),
            creation_timestamp: SystemTime::now(),
            git_commit_hash: self.get_current_git_commit()?,
            environment_info: environment,
            performance_metrics: metrics,
            statistical_properties,
            validation_status: BaselineValidationStatus::Valid,
        };

        // Store baseline
        self.baseline_repository.store_baseline(baseline_metadata)?;

        Ok(baseline_id)
    }

    /// Execute CI performance check
    pub fn execute_ci_performance_check(
        &mut self,
    ) -> Result<CiCheckResult, Box<dyn std::error::Error>> {
        let start_time = Instant::now();

        // Run performance measurements
        let current_metrics = self.measure_current_performance()?;

        // Detect regressions
        let regression_analysis = self.detect_performance_regression(current_metrics)?;

        // Execute performance gates
        let gate_results = self
            .ci_integrator
            .execute_performance_gates(&regression_analysis)?;

        // Determine overall status
        let overall_status = self.determine_ci_status(&gate_results);

        // Generate recommendations
        let recommendations =
            self.generate_ci_recommendations(&regression_analysis, &gate_results)?;

        // Create analysis summary
        let analysis_summary = AnalysisSummary {
            total_metrics_analyzed: self.count_analyzed_metrics(&regression_analysis),
            regressions_detected: self.count_regressions(&regression_analysis),
            improvements_detected: self.count_improvements(&regression_analysis),
            confidence_score: regression_analysis.confidence_score,
            analysis_duration: start_time.elapsed(),
        };

        Ok(CiCheckResult {
            overall_status,
            gate_results,
            analysis_summary,
            recommendations,
        })
    }

    /// Trigger performance alerts
    pub fn trigger_alerts(
        &mut self,
        analysis: &RegressionAnalysis,
    ) -> Result<Vec<AlertResult>, Box<dyn std::error::Error>> {
        self.alert_system.evaluate_alerts(analysis)
    }

    // Helper methods

    fn get_current_environment(&self) -> Result<EnvironmentInfo, Box<dyn std::error::Error>> {
        Ok(EnvironmentInfo {
            rust_version: std::env::var("RUSTC_VERSION").unwrap_or_else(|_| "unknown".to_string()),
            target_triple: std::env::var("TARGET").unwrap_or_else(|_| "unknown".to_string()),
            cpu_info: CpuInfo {
                model: "Generic CPU".to_string(),
                cores: num_cpus::get(),
                frequency_mhz: 2400, // Default assumption
                cache_size_kb: 8192, // Default assumption
            },
            memory_info: MemoryInfo {
                total_gb: 16.0,    // Default assumption
                available_gb: 8.0, // Default assumption
                memory_type: "DDR4".to_string(),
            },
            build_configuration: BuildConfiguration {
                optimization_level: if cfg!(debug_assertions) {
                    OptimizationLevel::Debug
                } else {
                    OptimizationLevel::Release
                },
                debug_info: cfg!(debug_assertions),
                target_cpu: std::env::var("TARGET_CPU")
                    .unwrap_or_else(|_| "x86-64-v3".to_string()),
                features_enabled: Vec::new(),
            },
        })
    }

    pub(crate) fn generate_baseline_id(&self, environment: &EnvironmentInfo) -> String {
        let mut hasher = Sha256::new();
        hasher.update(&environment.rust_version);
        hasher.update(&environment.target_triple);
        hasher.update(format!(
            "{}",
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or_else(|_| std::time::Duration::from_secs(0))
                .as_secs()
        ));
        format!("baseline_{:x}", hasher.finalize())[..16].to_string()
    }

    fn get_current_git_commit(&self) -> Result<Option<String>, Box<dyn std::error::Error>> {
        // Simplified - real implementation would use git2 or similar
        Ok(std::env::var("GIT_COMMIT").ok())
    }

    pub(crate) fn determine_regression_status(
        &self,
        comparison: &MetricsComparison,
        tests: &StatisticalTestResults,
    ) -> Result<RegressionStatus, Box<dyn std::error::Error>> {
        let max_degradation = comparison
            .throughput_changes
            .iter()
            .chain(comparison.memory_changes.iter())
            .chain(comparison.latency_changes.iter())
            .filter(|change| matches!(change.change_direction, ChangeDirection::Degradation))
            .map(|change| change.change_percent)
            .fold(0.0f64, f64::max);
        let max_improvement = comparison
            .throughput_changes
            .iter()
            .chain(comparison.memory_changes.iter())
            .chain(comparison.latency_changes.iter())
            .filter(|change| matches!(change.change_direction, ChangeDirection::Improvement))
            .map(|change| change.change_percent)
            .fold(0.0f64, f64::max);

        let is_significant = tests.t_test_result.significant;

        if !is_significant {
            Ok(RegressionStatus::NoRegression)
        } else if max_improvement >= 2.0 && max_degradation < 2.0 {
            Ok(RegressionStatus::Improvement {
                magnitude: max_improvement,
            })
        } else if max_degradation < 2.0 {
            Ok(RegressionStatus::NoRegression)
        } else if max_degradation < 5.0 {
            Ok(RegressionStatus::MinorRegression {
                severity: RegressionSeverity::Low,
            })
        } else if max_degradation < 15.0 {
            Ok(RegressionStatus::MajorRegression {
                severity: RegressionSeverity::Medium,
            })
        } else {
            Ok(RegressionStatus::CriticalRegression {
                severity: RegressionSeverity::High,
            })
        }
    }

    fn calculate_confidence_score(&self, tests: &StatisticalTestResults) -> f64 {
        let mut score = 0.0;

        // T-test contribution
        if tests.t_test_result.significant {
            score += 0.4 * (1.0 - tests.t_test_result.p_value);
        }

        // Mann-Whitney U contribution
        if tests.mann_whitney_u_result.significant {
            score += 0.3 * (1.0 - tests.mann_whitney_u_result.p_value);
        }

        // Effect size contribution
        score += 0.2
            * match tests.effect_size.interpretation {
                EffectSizeInterpretation::Negligible => 0.1,
                EffectSizeInterpretation::Small => 0.3,
                EffectSizeInterpretation::Medium => 0.6,
                EffectSizeInterpretation::Large => 0.8,
                EffectSizeInterpretation::VeryLarge => 1.0,
            };

        // Power analysis contribution
        if tests.power_analysis.adequate_power {
            score += 0.1;
        }

        score.min(1.0)
    }

    fn generate_recommendations(
        &self,
        status: &RegressionStatus,
        _comparison: &MetricsComparison,
    ) -> Result<Vec<RecommendationAction>, Box<dyn std::error::Error>> {
        let mut recommendations = Vec::new();

        match status {
            RegressionStatus::NoRegression => {
                recommendations.push(RecommendationAction::AcceptRegression {
                    reason: "Performance metrics within acceptable variance".to_string(),
                });
            }
            RegressionStatus::MinorRegression { .. } => {
                recommendations.push(RecommendationAction::InvestigateRegression {
                    focus_areas: vec![
                        "Algorithm efficiency".to_string(),
                        "Memory allocation patterns".to_string(),
                    ],
                });
            }
            RegressionStatus::MajorRegression { .. } => {
                recommendations.push(RecommendationAction::RejectChanges {
                    reason: "Major performance regression detected".to_string(),
                });
                recommendations.push(RecommendationAction::OptimizePerformance {
                    suggested_areas: vec![
                        "Hot paths optimization".to_string(),
                        "Data structure efficiency".to_string(),
                    ],
                });
            }
            RegressionStatus::CriticalRegression { .. } => {
                recommendations.push(RecommendationAction::RejectChanges {
                    reason: "Critical performance regression - immediate action required"
                        .to_string(),
                });
            }
            RegressionStatus::Improvement { .. } => {
                recommendations.push(RecommendationAction::AcceptRegression {
                    reason: "Performance improvement detected".to_string(),
                });
            }
        }

        Ok(recommendations)
    }

    fn measure_current_performance(
        &self,
    ) -> Result<PerformanceMetrics, Box<dyn std::error::Error>> {
        // Simplified implementation - real version would run actual benchmarks
        Ok(PerformanceMetrics {
            display_throughput: ThroughputMetrics {
                mean: 4200.0,
                median: 4150.0,
                percentile_95: 4800.0,
                percentile_99: 5200.0,
                std_deviation: 250.0,
                min: 3800.0,
                max: 5500.0,
            },
            comp3_throughput: ThroughputMetrics {
                mean: 580.0,
                median: 575.0,
                percentile_95: 620.0,
                percentile_99: 650.0,
                std_deviation: 25.0,
                min: 540.0,
                max: 680.0,
            },
            memory_usage: MemoryUsageMetrics {
                peak_memory_mb: 245.0,
                average_memory_mb: 180.0,
                steady_state_memory_mb: 165.0,
                memory_variance: 12.5,
            },
            latency_metrics: LatencyMetrics {
                p50_ms: 0.24,
                p95_ms: 0.45,
                p99_ms: 0.68,
                p999_ms: 1.2,
                max_latency_ms: 2.1,
            },
        })
    }

    fn determine_ci_status(&self, gate_results: &[GateResult]) -> CiStatus {
        let failed_count = gate_results
            .iter()
            .filter(|r| matches!(r.status, GateStatus::Failed))
            .count();
        let warning_count = gate_results
            .iter()
            .filter(|r| matches!(r.status, GateStatus::Warning))
            .count();

        if failed_count > 0 {
            CiStatus::Failed {
                reason: format!("{} performance gate(s) failed", failed_count),
            }
        } else if warning_count > 0 {
            CiStatus::Warning {
                message: format!("{} performance gate(s) have warnings", warning_count),
            }
        } else {
            CiStatus::Passed
        }
    }

    fn generate_ci_recommendations(
        &self,
        _analysis: &RegressionAnalysis,
        _gates: &[GateResult],
    ) -> Result<Vec<RecommendationAction>, Box<dyn std::error::Error>> {
        // Simplified implementation
        Ok(vec![RecommendationAction::InvestigateRegression {
            focus_areas: vec!["CI environment consistency".to_string()],
        }])
    }

    fn count_analyzed_metrics(&self, analysis: &RegressionAnalysis) -> usize {
        analysis.metrics_comparison.throughput_changes.len()
            + analysis.metrics_comparison.memory_changes.len()
            + analysis.metrics_comparison.latency_changes.len()
    }

    fn count_regressions(&self, analysis: &RegressionAnalysis) -> usize {
        match analysis.status {
            RegressionStatus::NoRegression | RegressionStatus::Improvement { .. } => 0,
            _ => 1,
        }
    }

    fn count_improvements(&self, analysis: &RegressionAnalysis) -> usize {
        match analysis.status {
            RegressionStatus::Improvement { .. } => 1,
            _ => 0,
        }
    }
}

impl Default for PerformanceRegressionDetector {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod num_cpus {
    pub fn get() -> usize {
        4 // Default for testing
    }
}

#[cfg(not(test))]
mod num_cpus {
    pub fn get() -> usize {
        std::thread::available_parallelism().map_or(4, |n| n.get())
    }
}
