<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# SLO Validation Framework and Performance Regression Detection

## Document Information

- **Document Type**: SLO Validation and Regression Detection Specification
- **Component**: Performance Floor Validation and Statistical Regression Analysis
- **Issue**: #52 - Machine-Readable Benchmark Reporting Infrastructure
- **Status**: Implementation Ready
- **Version**: 1.0
- **Date**: 2025-09-28

## Overview

This specification defines a comprehensive SLO (Service Level Objective) validation framework and performance regression detection system for copybook-rs. The system enforces enterprise performance floors while providing sophisticated statistical analysis for detecting performance regressions in mainframe data processing workloads.

## SLO Framework Architecture

### Performance Floor Definitions

The enterprise performance floors represent the minimum acceptable performance levels for production mainframe workloads:

```rust
/// Enterprise performance floor requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceFloors {
    /// DISPLAY-heavy processing minimum (80 MB/s)
    pub display_mbps_floor: f64,

    /// COMP-3-heavy processing minimum (40 MB/s)
    pub comp3_mbps_floor: f64,

    /// Memory usage ceiling (256 MiB steady-state)
    pub memory_mb_ceiling: f64,

    /// Maximum variance tolerance (5%)
    pub variance_tolerance: f64,

    /// Warning threshold factor (25% above floor)
    pub warning_factor: f64,

    /// Critical threshold factor (10% above floor)
    pub critical_factor: f64,
}

impl Default for PerformanceFloors {
    fn default() -> Self {
        Self {
            display_mbps_floor: 80.0,      // 80 MB/s enterprise minimum
            comp3_mbps_floor: 40.0,        // 40 MB/s enterprise minimum
            memory_mb_ceiling: 256.0,      // 256 MiB memory ceiling
            variance_tolerance: 0.05,      // 5% maximum variance
            warning_factor: 1.25,          // Warn at 25% above floor
            critical_factor: 1.10,         // Critical at 10% above floor
        }
    }
}
```

### SLO Validation Engine

```rust
/// SLO validation engine with enterprise compliance assessment
#[derive(Debug)]
pub struct SloValidationEngine {
    floors: PerformanceFloors,
    statistical_analyzer: StatisticalRegressionAnalyzer,
    compliance_assessor: ComplianceAssessor,
    historical_tracker: HistoricalPerformanceTracker,
}

impl SloValidationEngine {
    /// Comprehensive SLO validation with enterprise context
    pub fn validate_comprehensive_slos(
        &self,
        current_metrics: &PerformanceMetrics,
        baseline_metrics: Option<&PerformanceMetrics>,
    ) -> Result<ComprehensiveSloResult, ValidationError> {
        // 1. Basic floor validation
        let floor_validation = self.validate_performance_floors(current_metrics)?;

        // 2. Regression analysis (if baseline available)
        let regression_analysis = if let Some(baseline) = baseline_metrics {
            Some(self.analyze_performance_regression(baseline, current_metrics)?)
        } else {
            None
        };

        // 3. Statistical significance assessment
        let statistical_validation = self.validate_statistical_significance(current_metrics)?;

        // 4. Enterprise compliance assessment
        let compliance_assessment = self.assess_enterprise_compliance(
            &floor_validation,
            regression_analysis.as_ref(),
            &statistical_validation,
        )?;

        // 5. Historical trend analysis
        let trend_analysis = self.analyze_historical_trends(current_metrics)?;

        Ok(ComprehensiveSloResult {
            floor_validation,
            regression_analysis,
            statistical_validation,
            compliance_assessment,
            trend_analysis,
            overall_status: self.determine_overall_status(
                &floor_validation,
                regression_analysis.as_ref(),
                &compliance_assessment,
            ),
        })
    }

    /// Validate performance against enterprise floors
    fn validate_performance_floors(
        &self,
        metrics: &PerformanceMetrics,
    ) -> Result<FloorValidationResult, ValidationError> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let mut validation_details = Vec::new();

        // DISPLAY throughput validation
        let display_validation = self.validate_display_throughput(
            metrics.display_throughput.mean,
            &mut warnings,
            &mut errors,
        );
        validation_details.push(display_validation);

        // COMP-3 throughput validation
        let comp3_validation = self.validate_comp3_throughput(
            metrics.comp3_throughput.mean,
            &mut warnings,
            &mut errors,
        );
        validation_details.push(comp3_validation);

        // Memory usage validation
        let memory_validation = self.validate_memory_usage(
            &metrics.memory_usage,
            &mut warnings,
            &mut errors,
        );
        validation_details.push(memory_validation);

        // Latency validation
        let latency_validation = self.validate_latency_requirements(
            &metrics.latency_metrics,
            &mut warnings,
            &mut errors,
        );
        validation_details.push(latency_validation);

        // Calculate safety margins
        let safety_margins = self.calculate_safety_margins(metrics);

        Ok(FloorValidationResult {
            passed: errors.is_empty(),
            warnings,
            errors,
            safety_margins,
            validation_details,
            compliance_score: self.calculate_compliance_score(&safety_margins, &errors, &warnings),
        })
    }

    /// Validate DISPLAY processing throughput
    fn validate_display_throughput(
        &self,
        display_gibs: f64,
        warnings: &mut Vec<String>,
        errors: &mut Vec<String>,
    ) -> MetricValidationDetail {
        let display_mbps = display_gibs * 1073.74; // Convert GiB/s to MB/s
        let floor = self.floors.display_mbps_floor;
        let warning_threshold = floor * self.floors.warning_factor;
        let critical_threshold = floor * self.floors.critical_factor;

        let status = if display_mbps < floor {
            errors.push(format!(
                "DISPLAY throughput {:.2f} MB/s below enterprise floor of {:.2f} MB/s (deficit: {:.2f} MB/s)",
                display_mbps, floor, floor - display_mbps
            ));
            ValidationStatus::Failed
        } else if display_mbps < critical_threshold {
            errors.push(format!(
                "DISPLAY throughput {:.2f} MB/s critically close to floor of {:.2f} MB/s (margin: {:.2f} MB/s)",
                display_mbps, floor, display_mbps - floor
            ));
            ValidationStatus::Critical
        } else if display_mbps < warning_threshold {
            warnings.push(format!(
                "DISPLAY throughput {:.2f} MB/s approaching floor threshold of {:.2f} MB/s (margin: {:.2f} MB/s)",
                display_mbps, floor, display_mbps - floor
            ));
            ValidationStatus::Warning
        } else {
            ValidationStatus::Passed
        };

        MetricValidationDetail {
            metric_name: "DISPLAY Throughput".to_string(),
            measured_value: display_mbps,
            floor_value: floor,
            safety_factor: display_mbps / floor,
            status,
            message: format!("DISPLAY: {:.2f} MB/s vs {:.2f} MB/s floor", display_mbps, floor),
        }
    }

    /// Validate COMP-3 processing throughput
    fn validate_comp3_throughput(
        &self,
        comp3_mibs: f64,
        warnings: &mut Vec<String>,
        errors: &mut Vec<String>,
    ) -> MetricValidationDetail {
        let floor = self.floors.comp3_mbps_floor;
        let warning_threshold = floor * self.floors.warning_factor;
        let critical_threshold = floor * self.floors.critical_factor;

        let status = if comp3_mibs < floor {
            errors.push(format!(
                "COMP-3 throughput {:.2f} MiB/s below enterprise floor of {:.2f} MB/s (deficit: {:.2f} MB/s)",
                comp3_mibs, floor, floor - comp3_mibs
            ));
            ValidationStatus::Failed
        } else if comp3_mibs < critical_threshold {
            errors.push(format!(
                "COMP-3 throughput {:.2f} MiB/s critically close to floor of {:.2f} MB/s (margin: {:.2f} MB/s)",
                comp3_mibs, floor, comp3_mibs - floor
            ));
            ValidationStatus::Critical
        } else if comp3_mibs < warning_threshold {
            warnings.push(format!(
                "COMP-3 throughput {:.2f} MiB/s approaching floor threshold of {:.2f} MB/s (margin: {:.2f} MB/s)",
                comp3_mibs, floor, comp3_mibs - floor
            ));
            ValidationStatus::Warning
        } else {
            ValidationStatus::Passed
        };

        MetricValidationDetail {
            metric_name: "COMP-3 Throughput".to_string(),
            measured_value: comp3_mibs,
            floor_value: floor,
            safety_factor: comp3_mibs / floor,
            status,
            message: format!("COMP-3: {:.2f} MiB/s vs {:.2f} MB/s floor", comp3_mibs, floor),
        }
    }

    /// Calculate comprehensive safety margins
    fn calculate_safety_margins(&self, metrics: &PerformanceMetrics) -> SafetyMargins {
        let display_mbps = metrics.display_throughput.mean * 1073.74;
        let comp3_mibs = metrics.comp3_throughput.mean;

        SafetyMargins {
            display_safety_factor: display_mbps / self.floors.display_mbps_floor,
            comp3_safety_factor: comp3_mibs / self.floors.comp3_mbps_floor,
            memory_efficiency_score: self.calculate_memory_efficiency_score(&metrics.memory_usage),
            variance_stability_score: self.calculate_variance_stability_score(metrics),
            overall_readiness_score: self.calculate_overall_readiness_score(metrics),
        }
    }

    /// Calculate overall enterprise readiness score
    fn calculate_overall_readiness_score(&self, metrics: &PerformanceMetrics) -> f64 {
        let display_mbps = metrics.display_throughput.mean * 1073.74;
        let comp3_mibs = metrics.comp3_throughput.mean;

        // Weighted scoring (0.0 to 1.0)
        let display_score = (display_mbps / self.floors.display_mbps_floor).min(2.0) / 2.0; // Cap at 2x
        let comp3_score = (comp3_mibs / self.floors.comp3_mbps_floor).min(2.0) / 2.0;
        let memory_score = self.calculate_memory_efficiency_score(&metrics.memory_usage);
        let variance_score = self.calculate_variance_stability_score(metrics);

        // Weighted average (40% throughput, 30% memory, 30% stability)
        0.25 * display_score + 0.25 * comp3_score + 0.25 * memory_score + 0.25 * variance_score
    }
}

/// Comprehensive SLO validation result
#[derive(Debug, Clone)]
pub struct ComprehensiveSloResult {
    pub floor_validation: FloorValidationResult,
    pub regression_analysis: Option<RegressionAnalysisResult>,
    pub statistical_validation: StatisticalValidationResult,
    pub compliance_assessment: ComplianceAssessment,
    pub trend_analysis: TrendAnalysisResult,
    pub overall_status: OverallValidationStatus,
}

/// Floor validation result with detailed diagnostics
#[derive(Debug, Clone)]
pub struct FloorValidationResult {
    pub passed: bool,
    pub warnings: Vec<String>,
    pub errors: Vec<String>,
    pub safety_margins: SafetyMargins,
    pub validation_details: Vec<MetricValidationDetail>,
    pub compliance_score: f64,
}

/// Individual metric validation detail
#[derive(Debug, Clone)]
pub struct MetricValidationDetail {
    pub metric_name: String,
    pub measured_value: f64,
    pub floor_value: f64,
    pub safety_factor: f64,
    pub status: ValidationStatus,
    pub message: String,
}

/// Validation status for individual metrics
#[derive(Debug, Clone, PartialEq)]
pub enum ValidationStatus {
    Passed,     // Well above thresholds
    Warning,    // Approaching thresholds
    Critical,   // Very close to floors
    Failed,     // Below minimum floors
}
```

## Performance Regression Detection

### Statistical Regression Analysis

```rust
/// Advanced statistical regression detection with enterprise context
#[derive(Debug)]
pub struct PerformanceRegressionDetector {
    baseline_repository: BaselineRepository,
    statistical_analyzer: StatisticalRegressionAnalyzer,
    significance_tester: SignificanceTester,
    trend_analyzer: TrendAnalyzer,
}

impl PerformanceRegressionDetector {
    /// Comprehensive regression analysis with multiple statistical tests
    pub fn detect_comprehensive_regression(
        &self,
        baseline_metrics: &PerformanceMetrics,
        current_metrics: &PerformanceMetrics,
        analysis_config: &RegressionAnalysisConfig,
    ) -> Result<RegressionAnalysisResult, RegressionError> {
        // 1. Basic metric comparison
        let metric_comparisons = self.compare_performance_metrics(baseline_metrics, current_metrics)?;

        // 2. Statistical significance testing
        let significance_tests = self.run_comprehensive_significance_tests(
            baseline_metrics,
            current_metrics,
            analysis_config,
        )?;

        // 3. Effect size calculation
        let effect_sizes = self.calculate_comprehensive_effect_sizes(baseline_metrics, current_metrics)?;

        // 4. Power analysis
        let power_analysis = self.perform_power_analysis(baseline_metrics, current_metrics)?;

        // 5. Regression severity assessment
        let severity_assessment = self.assess_regression_severity(
            &metric_comparisons,
            &significance_tests,
            &effect_sizes,
        )?;

        // 6. Confidence assessment
        let confidence_assessment = self.assess_regression_confidence(
            &significance_tests,
            &effect_sizes,
            &power_analysis,
        )?;

        // 7. Enterprise impact analysis
        let enterprise_impact = self.analyze_enterprise_impact(
            &metric_comparisons,
            &severity_assessment,
        )?;

        Ok(RegressionAnalysisResult {
            metric_comparisons,
            significance_tests,
            effect_sizes,
            power_analysis,
            severity_assessment,
            confidence_assessment,
            enterprise_impact,
            recommendations: self.generate_recommendations(
                &severity_assessment,
                &enterprise_impact,
            )?,
        })
    }

    /// Compare performance metrics with statistical rigor
    fn compare_performance_metrics(
        &self,
        baseline: &PerformanceMetrics,
        current: &PerformanceMetrics,
    ) -> Result<Vec<MetricComparison>, RegressionError> {
        let mut comparisons = Vec::new();

        // DISPLAY throughput comparison
        comparisons.push(self.compare_throughput_metric(
            "DISPLAY Throughput",
            &baseline.display_throughput,
            &current.display_throughput,
            ThroughputUnit::GibPerSecond,
        )?);

        // COMP-3 throughput comparison
        comparisons.push(self.compare_throughput_metric(
            "COMP-3 Throughput",
            &baseline.comp3_throughput,
            &current.comp3_throughput,
            ThroughputUnit::MibPerSecond,
        )?);

        // Memory usage comparison
        comparisons.push(self.compare_memory_metric(
            "Memory Usage",
            &baseline.memory_usage,
            &current.memory_usage,
        )?);

        // Latency comparison
        comparisons.push(self.compare_latency_metric(
            "Latency",
            &baseline.latency_metrics,
            &current.latency_metrics,
        )?);

        Ok(comparisons)
    }

    /// Run comprehensive statistical significance tests
    fn run_comprehensive_significance_tests(
        &self,
        baseline: &PerformanceMetrics,
        current: &PerformanceMetrics,
        config: &RegressionAnalysisConfig,
    ) -> Result<SignificanceTestResults, RegressionError> {
        // Student's t-test for normally distributed metrics
        let t_test_results = self.run_t_tests(baseline, current, config)?;

        // Mann-Whitney U test for non-parametric comparison
        let mann_whitney_results = self.run_mann_whitney_tests(baseline, current)?;

        // Kolmogorov-Smirnov test for distribution comparison
        let ks_test_results = self.run_kolmogorov_smirnov_tests(baseline, current)?;

        // Bootstrap confidence intervals
        let bootstrap_results = self.run_bootstrap_analysis(baseline, current, config)?;

        // Bayesian analysis for uncertainty quantification
        let bayesian_results = self.run_bayesian_analysis(baseline, current, config)?;

        Ok(SignificanceTestResults {
            t_test_results,
            mann_whitney_results,
            ks_test_results,
            bootstrap_results,
            bayesian_results,
            overall_significance: self.determine_overall_significance(&t_test_results, &mann_whitney_results),
        })
    }

    /// Calculate comprehensive effect sizes
    fn calculate_comprehensive_effect_sizes(
        &self,
        baseline: &PerformanceMetrics,
        current: &PerformanceMetrics,
    ) -> Result<EffectSizeAnalysis, RegressionError> {
        // Cohen's d for standardized effect size
        let cohens_d = self.calculate_cohens_d(baseline, current)?;

        // Glass's delta for baseline-standardized effect
        let glass_delta = self.calculate_glass_delta(baseline, current)?;

        // Hedge's g for bias-corrected effect size
        let hedges_g = self.calculate_hedges_g(baseline, current)?;

        // Cliff's delta for non-parametric effect size
        let cliffs_delta = self.calculate_cliffs_delta(baseline, current)?;

        // Common language effect size
        let common_language_effect = self.calculate_common_language_effect(baseline, current)?;

        Ok(EffectSizeAnalysis {
            cohens_d,
            glass_delta,
            hedges_g,
            cliffs_delta,
            common_language_effect,
            interpretation: self.interpret_effect_sizes(&cohens_d),
        })
    }

    /// Assess regression severity with enterprise context
    fn assess_regression_severity(
        &self,
        comparisons: &[MetricComparison],
        significance: &SignificanceTestResults,
        effect_sizes: &EffectSizeAnalysis,
    ) -> Result<RegressionSeverityAssessment, RegressionError> {
        let mut severity_factors = Vec::new();

        for comparison in comparisons {
            let factor = SeverityFactor {
                metric_name: comparison.metric_name.clone(),
                change_magnitude: comparison.change_percent.abs(),
                statistical_significance: comparison.statistically_significant,
                effect_size_magnitude: self.get_effect_size_for_metric(&comparison.metric_name, effect_sizes),
                enterprise_impact_weight: self.get_enterprise_impact_weight(&comparison.metric_name),
            };
            severity_factors.push(factor);
        }

        // Calculate overall severity score (0.0 to 1.0)
        let overall_severity = self.calculate_overall_severity_score(&severity_factors);

        // Determine severity level
        let severity_level = match overall_severity {
            x if x >= 0.8 => RegressionSeverityLevel::Critical,
            x if x >= 0.6 => RegressionSeverityLevel::Major,
            x if x >= 0.3 => RegressionSeverityLevel::Minor,
            x if x >= 0.1 => RegressionSeverityLevel::Negligible,
            _ => RegressionSeverityLevel::NoRegression,
        };

        Ok(RegressionSeverityAssessment {
            severity_factors,
            overall_severity,
            severity_level,
            enterprise_risk_assessment: self.assess_enterprise_risk(overall_severity, &severity_factors),
        })
    }
}

/// Regression analysis configuration
#[derive(Debug, Clone)]
pub struct RegressionAnalysisConfig {
    pub significance_level: f64,        // Default: 0.05 (5%)
    pub minimum_sample_size: usize,     // Default: 30
    pub bootstrap_iterations: usize,    // Default: 10,000
    pub effect_size_threshold: f64,     // Default: 0.2 (small effect)
    pub regression_threshold: f64,      // Default: 0.02 (2%)
    pub enterprise_mode: bool,          // Default: true
}

/// Comprehensive regression analysis result
#[derive(Debug, Clone)]
pub struct RegressionAnalysisResult {
    pub metric_comparisons: Vec<MetricComparison>,
    pub significance_tests: SignificanceTestResults,
    pub effect_sizes: EffectSizeAnalysis,
    pub power_analysis: PowerAnalysisResult,
    pub severity_assessment: RegressionSeverityAssessment,
    pub confidence_assessment: ConfidenceAssessment,
    pub enterprise_impact: EnterpriseImpactAnalysis,
    pub recommendations: Vec<RegressionRecommendation>,
}

/// Enterprise impact analysis
#[derive(Debug, Clone)]
pub struct EnterpriseImpactAnalysis {
    pub production_readiness_impact: ProductionReadinessImpact,
    pub performance_slo_impact: PerformanceSloImpact,
    pub resource_utilization_impact: ResourceUtilizationImpact,
    pub compliance_impact: ComplianceImpact,
    pub risk_assessment: EnterpriseRiskAssessment,
}

/// Production readiness impact assessment
#[derive(Debug, Clone)]
pub struct ProductionReadinessImpact {
    pub current_readiness_score: f64,   // 0.0 to 1.0
    pub baseline_readiness_score: f64,  // 0.0 to 1.0
    pub readiness_change: f64,          // Change in readiness
    pub deployment_recommendation: DeploymentRecommendation,
}

/// Deployment recommendation based on regression analysis
#[derive(Debug, Clone, PartialEq)]
pub enum DeploymentRecommendation {
    Approved,                           // Safe for production deployment
    ConditionallyApproved,             // Safe with monitoring
    RequiresInvestigation,             // Investigate before deployment
    NotRecommended,                    // Performance regression too severe
    Blocked,                           // Critical regression, deployment blocked
}
```

## Statistical Analysis Framework

### Confidence Intervals and Hypothesis Testing

```python
# scripts/bench/statistical_analyzer.py
"""Advanced statistical analysis for performance regression detection."""

import numpy as np
import scipy.stats as stats
from scipy.stats import mannwhitneyu, kstest, bootstrap
from typing import Dict, List, Tuple, Optional, Any
import warnings
from dataclasses import dataclass

@dataclass
class StatisticalTestConfig:
    """Configuration for statistical tests"""
    significance_level: float = 0.05    # 5% significance level
    min_sample_size: int = 30           # Minimum samples for t-test
    bootstrap_iterations: int = 10000   # Bootstrap iterations
    confidence_level: float = 0.95      # 95% confidence intervals
    effect_size_threshold: float = 0.2  # Small effect size threshold

class AdvancedStatisticalAnalyzer:
    """Advanced statistical analysis for performance regression detection"""

    def __init__(self, config: StatisticalTestConfig = None):
        self.config = config or StatisticalTestConfig()

    def comprehensive_regression_analysis(
        self,
        baseline_data: Dict[str, List[float]],
        current_data: Dict[str, List[float]]
    ) -> Dict[str, Any]:
        """Perform comprehensive statistical regression analysis"""

        results = {}

        for metric_name in baseline_data.keys():
            if metric_name not in current_data:
                continue

            baseline_values = np.array(baseline_data[metric_name])
            current_values = np.array(current_data[metric_name])

            # Validate sample sizes
            if len(baseline_values) < 2 or len(current_values) < 2:
                continue

            metric_results = self._analyze_metric_regression(
                baseline_values,
                current_values,
                metric_name
            )
            results[metric_name] = metric_results

        return results

    def _analyze_metric_regression(
        self,
        baseline: np.ndarray,
        current: np.ndarray,
        metric_name: str
    ) -> Dict[str, Any]:
        """Analyze regression for a single metric"""

        results = {
            'metric_name': metric_name,
            'baseline_stats': self._calculate_descriptive_stats(baseline),
            'current_stats': self._calculate_descriptive_stats(current),
        }

        # Basic comparison
        results['change_percent'] = self._calculate_change_percent(baseline, current)

        # Normality testing
        results['normality_tests'] = self._test_normality(baseline, current)

        # Parametric tests (if data is normal)
        if results['normality_tests']['both_normal']:
            results['t_test'] = self._perform_t_test(baseline, current)
            results['f_test'] = self._perform_f_test(baseline, current)
        else:
            results['t_test'] = None
            results['f_test'] = None

        # Non-parametric tests (always performed)
        results['mann_whitney'] = self._perform_mann_whitney_test(baseline, current)
        results['kolmogorov_smirnov'] = self._perform_ks_test(baseline, current)

        # Effect size calculations
        results['effect_sizes'] = self._calculate_effect_sizes(baseline, current)

        # Bootstrap confidence intervals
        results['bootstrap'] = self._perform_bootstrap_analysis(baseline, current)

        # Power analysis
        results['power_analysis'] = self._perform_power_analysis(baseline, current)

        # Overall assessment
        results['regression_assessment'] = self._assess_regression_significance(results)

        return results

    def _calculate_descriptive_stats(self, data: np.ndarray) -> Dict[str, float]:
        """Calculate comprehensive descriptive statistics"""
        return {
            'mean': float(np.mean(data)),
            'median': float(np.median(data)),
            'std': float(np.std(data, ddof=1)),
            'var': float(np.var(data, ddof=1)),
            'min': float(np.min(data)),
            'max': float(np.max(data)),
            'q25': float(np.percentile(data, 25)),
            'q75': float(np.percentile(data, 75)),
            'iqr': float(np.percentile(data, 75) - np.percentile(data, 25)),
            'skewness': float(stats.skew(data)),
            'kurtosis': float(stats.kurtosis(data)),
            'n': len(data)
        }

    def _test_normality(self, baseline: np.ndarray, current: np.ndarray) -> Dict[str, Any]:
        """Test normality of distributions"""
        baseline_shapiro = stats.shapiro(baseline) if len(baseline) <= 5000 else None
        current_shapiro = stats.shapiro(current) if len(current) <= 5000 else None

        # Shapiro-Wilk test (for smaller samples)
        baseline_normal = baseline_shapiro[1] > self.config.significance_level if baseline_shapiro else None
        current_normal = current_shapiro[1] > self.config.significance_level if current_shapiro else None

        # Anderson-Darling test (alternative for larger samples)
        baseline_anderson = stats.anderson(baseline)
        current_anderson = stats.anderson(current)

        return {
            'baseline_shapiro': {
                'statistic': baseline_shapiro[0] if baseline_shapiro else None,
                'p_value': baseline_shapiro[1] if baseline_shapiro else None,
                'normal': baseline_normal
            },
            'current_shapiro': {
                'statistic': current_shapiro[0] if current_shapiro else None,
                'p_value': current_shapiro[1] if current_shapiro else None,
                'normal': current_normal
            },
            'baseline_anderson': {
                'statistic': baseline_anderson.statistic,
                'critical_values': baseline_anderson.critical_values.tolist(),
                'significance_levels': baseline_anderson.significance_level
            },
            'current_anderson': {
                'statistic': current_anderson.statistic,
                'critical_values': current_anderson.critical_values.tolist(),
                'significance_levels': current_anderson.significance_level
            },
            'both_normal': baseline_normal and current_normal if baseline_normal is not None and current_normal is not None else False
        }

    def _perform_t_test(self, baseline: np.ndarray, current: np.ndarray) -> Dict[str, Any]:
        """Perform Student's t-test"""
        # Test for equal variances first
        levene_stat, levene_p = stats.levene(baseline, current)
        equal_var = levene_p > self.config.significance_level

        # Perform appropriate t-test
        if equal_var:
            t_stat, p_value = stats.ttest_ind(baseline, current, equal_var=True)
            test_type = "Independent t-test (equal variances)"
        else:
            t_stat, p_value = stats.ttest_ind(baseline, current, equal_var=False)
            test_type = "Welch's t-test (unequal variances)"

        # Calculate degrees of freedom
        if equal_var:
            df = len(baseline) + len(current) - 2
        else:
            # Welch's formula for unequal variances
            s1_sq = np.var(baseline, ddof=1)
            s2_sq = np.var(current, ddof=1)
            n1, n2 = len(baseline), len(current)
            df = (s1_sq/n1 + s2_sq/n2)**2 / ((s1_sq/n1)**2/(n1-1) + (s2_sq/n2)**2/(n2-1))

        # Calculate confidence interval for difference in means
        mean_diff = np.mean(current) - np.mean(baseline)
        pooled_se = np.sqrt(np.var(baseline, ddof=1)/len(baseline) + np.var(current, ddof=1)/len(current))
        t_critical = stats.t.ppf(1 - self.config.significance_level/2, df)
        ci_lower = mean_diff - t_critical * pooled_se
        ci_upper = mean_diff + t_critical * pooled_se

        return {
            'test_type': test_type,
            'statistic': float(t_stat),
            'p_value': float(p_value),
            'degrees_of_freedom': float(df),
            'significant': p_value < self.config.significance_level,
            'mean_difference': float(mean_diff),
            'confidence_interval': {
                'lower': float(ci_lower),
                'upper': float(ci_upper),
                'level': self.config.confidence_level
            },
            'equal_variances': equal_var,
            'levene_test': {
                'statistic': float(levene_stat),
                'p_value': float(levene_p)
            }
        }

    def _perform_mann_whitney_test(self, baseline: np.ndarray, current: np.ndarray) -> Dict[str, Any]:
        """Perform Mann-Whitney U test (non-parametric)"""
        try:
            u_statistic, p_value = mannwhitneyu(baseline, current, alternative='two-sided')

            # Calculate effect size (rank-biserial correlation)
            r = 1 - (2 * u_statistic) / (len(baseline) * len(current))

            return {
                'u_statistic': float(u_statistic),
                'p_value': float(p_value),
                'significant': p_value < self.config.significance_level,
                'effect_size_r': float(r),
                'interpretation': self._interpret_mann_whitney_effect_size(abs(r))
            }
        except Exception as e:
            return {
                'error': str(e),
                'u_statistic': None,
                'p_value': None,
                'significant': False
            }

    def _perform_bootstrap_analysis(self, baseline: np.ndarray, current: np.ndarray) -> Dict[str, Any]:
        """Perform bootstrap analysis for robust confidence intervals"""
        def mean_difference(x, y):
            return np.mean(y) - np.mean(x)

        try:
            # Bootstrap resampling
            rng = np.random.default_rng(42)  # Fixed seed for reproducibility

            # Manual bootstrap implementation for more control
            bootstrap_diffs = []
            n_bootstrap = self.config.bootstrap_iterations

            for _ in range(n_bootstrap):
                baseline_resample = rng.choice(baseline, size=len(baseline), replace=True)
                current_resample = rng.choice(current, size=len(current), replace=True)
                diff = mean_difference(baseline_resample, current_resample)
                bootstrap_diffs.append(diff)

            bootstrap_diffs = np.array(bootstrap_diffs)

            # Calculate confidence intervals
            alpha = 1 - self.config.confidence_level
            ci_lower = np.percentile(bootstrap_diffs, (alpha/2) * 100)
            ci_upper = np.percentile(bootstrap_diffs, (1 - alpha/2) * 100)

            # Test if zero is in confidence interval
            zero_in_ci = ci_lower <= 0 <= ci_upper

            return {
                'mean_difference': float(np.mean(bootstrap_diffs)),
                'std_error': float(np.std(bootstrap_diffs)),
                'confidence_interval': {
                    'lower': float(ci_lower),
                    'upper': float(ci_upper),
                    'level': self.config.confidence_level
                },
                'zero_in_ci': zero_in_ci,
                'significant': not zero_in_ci,
                'n_bootstrap': n_bootstrap
            }
        except Exception as e:
            return {
                'error': str(e),
                'mean_difference': None,
                'confidence_interval': None,
                'significant': False
            }

    def _calculate_effect_sizes(self, baseline: np.ndarray, current: np.ndarray) -> Dict[str, Any]:
        """Calculate multiple effect size measures"""
        baseline_mean = np.mean(baseline)
        current_mean = np.mean(current)
        baseline_std = np.std(baseline, ddof=1)
        current_std = np.std(current, ddof=1)

        # Cohen's d (standardized mean difference)
        pooled_std = np.sqrt(((len(baseline) - 1) * baseline_std**2 +
                             (len(current) - 1) * current_std**2) /
                            (len(baseline) + len(current) - 2))
        cohens_d = (current_mean - baseline_mean) / pooled_std if pooled_std > 0 else 0

        # Glass's delta (baseline standardized)
        glass_delta = (current_mean - baseline_mean) / baseline_std if baseline_std > 0 else 0

        # Hedge's g (bias-corrected Cohen's d)
        df = len(baseline) + len(current) - 2
        correction_factor = 1 - 3 / (4 * df - 1) if df > 3 else 1
        hedges_g = cohens_d * correction_factor

        return {
            'cohens_d': float(cohens_d),
            'glass_delta': float(glass_delta),
            'hedges_g': float(hedges_g),
            'interpretation': self._interpret_effect_size(abs(cohens_d)),
            'practical_significance': abs(cohens_d) >= self.config.effect_size_threshold
        }

    def _interpret_effect_size(self, effect_size: float) -> str:
        """Interpret effect size magnitude (Cohen's conventions)"""
        if effect_size < 0.2:
            return "negligible"
        elif effect_size < 0.5:
            return "small"
        elif effect_size < 0.8:
            return "medium"
        else:
            return "large"

    def _assess_regression_significance(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Assess overall regression significance"""
        # Collect significance indicators
        significance_indicators = []

        if results.get('t_test') and results['t_test'].get('significant'):
            significance_indicators.append(('t_test', results['t_test']['p_value']))

        if results.get('mann_whitney') and results['mann_whitney'].get('significant'):
            significance_indicators.append(('mann_whitney', results['mann_whitney']['p_value']))

        if results.get('bootstrap') and results['bootstrap'].get('significant'):
            significance_indicators.append(('bootstrap', 'confidence_interval'))

        # Effect size consideration
        effect_sizes = results.get('effect_sizes', {})
        practical_significance = effect_sizes.get('practical_significance', False)

        # Overall assessment
        has_statistical_significance = len(significance_indicators) >= 1
        has_practical_significance = practical_significance

        # Determine regression level
        if has_statistical_significance and has_practical_significance:
            regression_level = "significant_regression"
        elif has_statistical_significance:
            regression_level = "statistical_regression"
        elif has_practical_significance:
            regression_level = "practical_regression"
        else:
            regression_level = "no_regression"

        return {
            'statistical_significance': has_statistical_significance,
            'practical_significance': has_practical_significance,
            'regression_level': regression_level,
            'significance_tests_passed': len(significance_indicators),
            'recommendation': self._generate_regression_recommendation(
                has_statistical_significance,
                has_practical_significance,
                results.get('change_percent', 0)
            )
        }

    def _generate_regression_recommendation(
        self,
        statistical_sig: bool,
        practical_sig: bool,
        change_percent: float
    ) -> str:
        """Generate recommendation based on regression analysis"""
        if not statistical_sig and not practical_sig:
            return "accept_changes"
        elif statistical_sig and practical_sig and abs(change_percent) > 10:
            return "reject_changes"
        elif statistical_sig and practical_sig:
            return "investigate_regression"
        elif statistical_sig:
            return "monitor_closely"
        else:
            return "proceed_with_caution"
```

This comprehensive SLO validation framework and performance regression detection system provides enterprise-grade statistical analysis capabilities while maintaining practical usability for copybook-rs performance monitoring.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
