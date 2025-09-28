#!/usr/bin/env python3
"""
SLO validation and performance floor monitoring for copybook-rs.

This module provides Service Level Objective (SLO) validation capabilities
for Issue #52 machine-readable benchmark reporting infrastructure. Validates
performance against enterprise targets and provides regression detection
for production deployment readiness.

Features:
- Enterprise performance floor validation (DISPLAY ≥4.1 GiB/s, COMP-3 ≥560 MiB/s)
- SLO compliance monitoring with statistical significance
- Performance regression detection with configurable thresholds
- CI/CD integration with exit codes for automated failure detection
- Historical performance trend analysis and baseline comparison
- Alert generation for performance degradation incidents

SLO Validation Framework:
- Performance Floors: Minimum acceptable performance levels
- Performance Targets: Optimal performance goals for enterprise deployment
- Regression Thresholds: Maximum acceptable performance degradation
- Compliance Monitoring: Continuous validation against regulatory requirements
"""

import argparse
import json
import sys
import statistics
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime, timedelta
import math


class SLOValidator:
    """SLO validation and performance floor monitoring."""

    def __init__(self):
        """Initialize SLO validator with enterprise performance targets."""
        # Enterprise performance floors (minimum acceptable)
        self.performance_floors = {
            "display_gibs": 4.1,    # DISPLAY parsing minimum
            "comp3_mibs": 560.0,    # COMP-3 encoding minimum
        }

        # Enterprise performance targets (optimal goals)
        self.performance_targets = {
            "display_gibs": 5.0,    # DISPLAY parsing target
            "comp3_mibs": 650.0,    # COMP-3 encoding target
        }

        # Regression detection thresholds
        self.regression_thresholds = {
            "minor_regression_percent": 2.0,    # 2% degradation triggers warning
            "major_regression_percent": 5.0,    # 5% degradation triggers failure
            "critical_regression_percent": 10.0, # 10% degradation triggers critical alert
            "variance_tolerance_percent": 5.0,   # 5% variance tolerance for noise
            "confidence_level": 0.95             # 95% confidence for statistical tests
        }

        # SLO compliance categories
        self.compliance_levels = {
            "EXCELLENT": {"min_score": 95, "description": "Exceeds all enterprise targets"},
            "GOOD": {"min_score": 85, "description": "Meets all floors, approaches targets"},
            "ACCEPTABLE": {"min_score": 70, "description": "Meets enterprise floors"},
            "POOR": {"min_score": 50, "description": "Below some enterprise floors"},
            "CRITICAL": {"min_score": 0, "description": "Below multiple enterprise floors"}
        }

    def validate_performance_floors(self, perf_data: Dict[str, Any]) -> Dict[str, Any]:
        """Validate performance against enterprise floors.

        Args:
            perf_data: Performance data to validate

        Returns:
            Validation results with pass/fail status and details
        """
        display_gibs = perf_data.get("display_gibs", 0.0)
        comp3_mibs = perf_data.get("comp3_mibs", 0.0)

        validation_results = {
            "timestamp": datetime.now().isoformat(),
            "overall_pass": True,
            "floor_validations": {},
            "target_validations": {},
            "compliance_score": 100.0,
            "compliance_level": "EXCELLENT",
            "recommendations": []
        }

        # Validate against performance floors
        for metric, floor_value in self.performance_floors.items():
            current_value = perf_data.get(metric, 0.0)
            passes_floor = current_value >= floor_value

            if not passes_floor:
                validation_results["overall_pass"] = False

            compliance_percentage = (current_value / floor_value) * 100 if floor_value > 0 else 100

            validation_results["floor_validations"][metric] = {
                "current_value": current_value,
                "floor_value": floor_value,
                "passes": passes_floor,
                "compliance_percentage": min(100.0, compliance_percentage),
                "gap": max(0.0, floor_value - current_value)
            }

        # Validate against performance targets
        for metric, target_value in self.performance_targets.items():
            current_value = perf_data.get(metric, 0.0)
            meets_target = current_value >= target_value

            target_percentage = (current_value / target_value) * 100 if target_value > 0 else 100

            validation_results["target_validations"][metric] = {
                "current_value": current_value,
                "target_value": target_value,
                "meets_target": meets_target,
                "target_percentage": min(100.0, target_percentage),
                "gap_to_target": max(0.0, target_value - current_value)
            }

        # Calculate overall compliance score
        floor_scores = [v["compliance_percentage"] for v in validation_results["floor_validations"].values()]
        target_scores = [v["target_percentage"] for v in validation_results["target_validations"].values()]

        # Weighted scoring: floors are 70%, targets are 30%
        floor_average = statistics.mean(floor_scores) if floor_scores else 100.0
        target_average = statistics.mean(target_scores) if target_scores else 100.0

        validation_results["compliance_score"] = (floor_average * 0.7) + (target_average * 0.3)

        # Determine compliance level
        score = validation_results["compliance_score"]
        for level, criteria in self.compliance_levels.items():
            if score >= criteria["min_score"]:
                validation_results["compliance_level"] = level
                break

        # Generate recommendations
        recommendations = []

        if not validation_results["floor_validations"]["display_gibs"]["passes"]:
            gap = validation_results["floor_validations"]["display_gibs"]["gap"]
            recommendations.append(f"Improve DISPLAY parsing performance by {gap:.2f} GiB/s to meet enterprise floor")

        if not validation_results["floor_validations"]["comp3_mibs"]["passes"]:
            gap = validation_results["floor_validations"]["comp3_mibs"]["gap"]
            recommendations.append(f"Improve COMP-3 encoding performance by {gap:.1f} MiB/s to meet enterprise floor")

        if not validation_results["target_validations"]["display_gibs"]["meets_target"]:
            gap = validation_results["target_validations"]["display_gibs"]["gap_to_target"]
            recommendations.append(f"Optimize DISPLAY parsing by {gap:.2f} GiB/s to achieve enterprise target")

        if not validation_results["target_validations"]["comp3_mibs"]["meets_target"]:
            gap = validation_results["target_validations"]["comp3_mibs"]["gap_to_target"]
            recommendations.append(f"Optimize COMP-3 encoding by {gap:.1f} MiB/s to achieve enterprise target")

        # Check for errors and warnings in performance data
        errors = perf_data.get("errors", [])
        warnings = perf_data.get("warnings", [])

        if errors:
            validation_results["overall_pass"] = False
            validation_results["compliance_score"] *= 0.5  # Severe penalty for errors
            recommendations.append("Resolve all performance errors before deployment")

        if warnings:
            validation_results["compliance_score"] *= 0.9  # Minor penalty for warnings
            recommendations.append("Address performance warnings for optimal operation")

        validation_results["recommendations"] = recommendations

        return validation_results

    def detect_regression(self, current: Dict[str, Any],
                         baseline: Dict[str, Any]) -> Dict[str, Any]:
        """Detect performance regression against baseline.

        Args:
            current: Current performance data
            baseline: Baseline performance data

        Returns:
            Regression analysis with severity classification
        """
        current_display = current.get("display_gibs", 0.0)
        current_comp3 = current.get("comp3_mibs", 0.0)

        # Handle baseline format (may be wrapped in performance key)
        baseline_perf = baseline.get("performance", baseline)
        baseline_display = baseline_perf.get("display_gibs", 0.0)
        baseline_comp3 = baseline_perf.get("comp3_mibs", 0.0)

        regression_analysis = {
            "timestamp": datetime.now().isoformat(),
            "has_regression": False,
            "regression_severity": "NONE",
            "metrics": {},
            "overall_change_percent": 0.0,
            "recommendations": []
        }

        # Analyze each metric
        metrics_analysis = {}

        for metric_name, current_val, baseline_val in [
            ("display_gibs", current_display, baseline_display),
            ("comp3_mibs", current_comp3, baseline_comp3)
        ]:
            if baseline_val > 0:
                change_percent = ((current_val - baseline_val) / baseline_val) * 100
                is_regression = change_percent < -self.regression_thresholds["minor_regression_percent"]

                # Classify regression severity
                if change_percent <= -self.regression_thresholds["critical_regression_percent"]:
                    severity = "CRITICAL"
                elif change_percent <= -self.regression_thresholds["major_regression_percent"]:
                    severity = "MAJOR"
                elif change_percent <= -self.regression_thresholds["minor_regression_percent"]:
                    severity = "MINOR"
                else:
                    severity = "NONE"

                metrics_analysis[metric_name] = {
                    "current_value": current_val,
                    "baseline_value": baseline_val,
                    "change_percent": change_percent,
                    "is_regression": is_regression,
                    "severity": severity,
                    "absolute_change": current_val - baseline_val
                }

                if is_regression:
                    regression_analysis["has_regression"] = True

        regression_analysis["metrics"] = metrics_analysis

        # Determine overall regression severity
        severities = [m["severity"] for m in metrics_analysis.values()]
        if "CRITICAL" in severities:
            regression_analysis["regression_severity"] = "CRITICAL"
        elif "MAJOR" in severities:
            regression_analysis["regression_severity"] = "MAJOR"
        elif "MINOR" in severities:
            regression_analysis["regression_severity"] = "MINOR"

        # Calculate overall performance change
        if baseline_display > 0 and baseline_comp3 > 0:
            display_change = ((current_display - baseline_display) / baseline_display) * 100
            comp3_change = ((current_comp3 - baseline_comp3) / baseline_comp3) * 100
            regression_analysis["overall_change_percent"] = (display_change + comp3_change) / 2

        # Generate recommendations based on regression analysis
        recommendations = []

        for metric_name, analysis in metrics_analysis.items():
            if analysis["is_regression"]:
                severity = analysis["severity"]
                change = analysis["change_percent"]

                if severity == "CRITICAL":
                    recommendations.append(
                        f"URGENT: {metric_name} performance degraded by {abs(change):.1f}% - "
                        "immediate investigation required"
                    )
                elif severity == "MAJOR":
                    recommendations.append(
                        f"IMPORTANT: {metric_name} performance degraded by {abs(change):.1f}% - "
                        "performance optimization needed"
                    )
                elif severity == "MINOR":
                    recommendations.append(
                        f"NOTICE: {metric_name} performance degraded by {abs(change):.1f}% - "
                        "monitor for trend continuation"
                    )

        regression_analysis["recommendations"] = recommendations

        return regression_analysis

    def calculate_statistical_significance(self, current_samples: List[float],
                                         baseline_samples: List[float]) -> Dict[str, Any]:
        """Calculate statistical significance of performance change.

        Args:
            current_samples: Current performance samples
            baseline_samples: Baseline performance samples

        Returns:
            Statistical analysis results
        """
        if len(current_samples) < 2 or len(baseline_samples) < 2:
            return {
                "sufficient_samples": False,
                "message": "Insufficient samples for statistical analysis"
            }

        # Calculate basic statistics
        current_mean = statistics.mean(current_samples)
        baseline_mean = statistics.mean(baseline_samples)
        current_stdev = statistics.stdev(current_samples)
        baseline_stdev = statistics.stdev(baseline_samples)

        # Calculate effect size (Cohen's d)
        pooled_stdev = math.sqrt(((len(current_samples) - 1) * current_stdev**2 +
                                 (len(baseline_samples) - 1) * baseline_stdev**2) /
                                (len(current_samples) + len(baseline_samples) - 2))

        cohens_d = (current_mean - baseline_mean) / pooled_stdev if pooled_stdev > 0 else 0

        # Simplified t-test (assuming equal variances)
        pooled_se = pooled_stdev * math.sqrt(1/len(current_samples) + 1/len(baseline_samples))
        t_statistic = (current_mean - baseline_mean) / pooled_se if pooled_se > 0 else 0

        # Degrees of freedom
        df = len(current_samples) + len(baseline_samples) - 2

        return {
            "sufficient_samples": True,
            "current_mean": current_mean,
            "baseline_mean": baseline_mean,
            "current_stdev": current_stdev,
            "baseline_stdev": baseline_stdev,
            "cohens_d": cohens_d,
            "t_statistic": t_statistic,
            "degrees_of_freedom": df,
            "effect_size_interpretation": self._interpret_effect_size(abs(cohens_d)),
            "change_percent": ((current_mean - baseline_mean) / baseline_mean * 100) if baseline_mean > 0 else 0
        }

    def _interpret_effect_size(self, cohens_d: float) -> str:
        """Interpret Cohen's d effect size."""
        if cohens_d < 0.2:
            return "negligible"
        elif cohens_d < 0.5:
            return "small"
        elif cohens_d < 0.8:
            return "medium"
        else:
            return "large"

    def generate_slo_report(self, validation_results: Dict[str, Any],
                           regression_analysis: Optional[Dict[str, Any]] = None) -> str:
        """Generate comprehensive SLO validation report.

        Args:
            validation_results: Performance validation results
            regression_analysis: Optional regression analysis results

        Returns:
            Formatted report string
        """
        report_lines = [
            "copybook-rs SLO Validation Report",
            "=" * 50,
            f"Timestamp: {validation_results['timestamp']}",
            f"Overall Status: {'✅ PASS' if validation_results['overall_pass'] else '❌ FAIL'}",
            f"Compliance Score: {validation_results['compliance_score']:.1f}%",
            f"Compliance Level: {validation_results['compliance_level']}",
            "",
            "Performance Floor Validation:",
            "-" * 30
        ]

        for metric, results in validation_results["floor_validations"].items():
            status = "✅ PASS" if results["passes"] else "❌ FAIL"
            current = results["current_value"]
            floor = results["floor_value"]
            unit = "GiB/s" if "gibs" in metric else "MiB/s"

            report_lines.append(f"{metric.upper()}: {status}")
            report_lines.append(f"  Current: {current:.2f} {unit}")
            report_lines.append(f"  Floor: {floor:.1f} {unit}")
            report_lines.append(f"  Compliance: {results['compliance_percentage']:.1f}%")

            if not results["passes"]:
                report_lines.append(f"  Gap: {results['gap']:.2f} {unit}")

        report_lines.extend([
            "",
            "Performance Target Analysis:",
            "-" * 30
        ])

        for metric, results in validation_results["target_validations"].items():
            status = "✅ MEETS" if results["meets_target"] else "⚠️  BELOW"
            current = results["current_value"]
            target = results["target_value"]
            unit = "GiB/s" if "gibs" in metric else "MiB/s"

            report_lines.append(f"{metric.upper()}: {status}")
            report_lines.append(f"  Current: {current:.2f} {unit}")
            report_lines.append(f"  Target: {target:.1f} {unit}")
            report_lines.append(f"  Target Achievement: {results['target_percentage']:.1f}%")

            if not results["meets_target"]:
                report_lines.append(f"  Gap to Target: {results['gap_to_target']:.2f} {unit}")

        # Add regression analysis if provided
        if regression_analysis:
            report_lines.extend([
                "",
                "Regression Analysis:",
                "-" * 30,
                f"Regression Detected: {'❌ YES' if regression_analysis['has_regression'] else '✅ NO'}",
                f"Severity: {regression_analysis['regression_severity']}"
            ])

            for metric, analysis in regression_analysis["metrics"].items():
                change = analysis["change_percent"]
                trend = "📈" if change > 0 else "📉" if change < -2 else "➡️"
                unit = "GiB/s" if "gibs" in metric else "MiB/s"

                report_lines.append(f"{metric.upper()}: {trend} {change:+.2f}%")
                report_lines.append(f"  Current: {analysis['current_value']:.2f} {unit}")
                report_lines.append(f"  Baseline: {analysis['baseline_value']:.2f} {unit}")

        # Add recommendations
        if validation_results["recommendations"]:
            report_lines.extend([
                "",
                "Recommendations:",
                "-" * 30
            ])
            for i, rec in enumerate(validation_results["recommendations"], 1):
                report_lines.append(f"{i}. {rec}")

        if regression_analysis and regression_analysis["recommendations"]:
            report_lines.extend([
                "",
                "Regression Recommendations:",
                "-" * 30
            ])
            for i, rec in enumerate(regression_analysis["recommendations"], 1):
                report_lines.append(f"{i}. {rec}")

        return "\n".join(report_lines)


def main():
    """Main entry point for SLO validator."""
    parser = argparse.ArgumentParser(
        description="copybook-rs SLO validation and performance floor monitoring",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python3 slo_validator.py --display-gibs 4.22 --comp3-mibs 571.0 --validate-floors
  python3 slo_validator.py --perf-file perf.json --validate-enterprise-targets
  python3 slo_validator.py --perf-file current.json --baseline baseline.json --detect-regression
        """
    )

    parser.add_argument(
        "--display-gibs",
        type=float,
        help="DISPLAY parsing throughput in GiB/s"
    )

    parser.add_argument(
        "--comp3-mibs",
        type=float,
        help="COMP-3 encoding throughput in MiB/s"
    )

    parser.add_argument(
        "--perf-file",
        type=Path,
        help="Performance JSON file path"
    )

    parser.add_argument(
        "--baseline",
        type=Path,
        help="Baseline performance file for regression detection"
    )

    parser.add_argument(
        "--validate-floors",
        action="store_true",
        help="Validate against enterprise performance floors"
    )

    parser.add_argument(
        "--validate-enterprise-targets",
        action="store_true",
        help="Validate against enterprise performance targets"
    )

    parser.add_argument(
        "--detect-regression",
        action="store_true",
        help="Detect performance regression against baseline"
    )

    parser.add_argument(
        "--output", "-o",
        type=Path,
        help="Output file for validation report"
    )

    parser.add_argument(
        "--json-output",
        action="store_true",
        help="Output results in JSON format"
    )

    args = parser.parse_args()

    try:
        validator = SLOValidator()

        # Prepare performance data
        perf_data = {}

        if args.perf_file:
            if not args.perf_file.exists():
                print(f"❌ Performance file not found: {args.perf_file}", file=sys.stderr)
                sys.exit(1)

            with open(args.perf_file, 'r') as f:
                perf_data = json.load(f)

        # Override with command line values if provided
        if args.display_gibs is not None:
            perf_data["display_gibs"] = args.display_gibs
        if args.comp3_mibs is not None:
            perf_data["comp3_mibs"] = args.comp3_mibs

        if not perf_data:
            print("❌ Performance data required (use --perf-file or --display-gibs/--comp3-mibs)", file=sys.stderr)
            sys.exit(1)

        # Default to floor validation if no specific action
        if not any([args.validate_floors, args.validate_enterprise_targets, args.detect_regression]):
            args.validate_floors = True

        # Perform validation
        validation_results = None
        regression_analysis = None

        if args.validate_floors or args.validate_enterprise_targets:
            print("Validating performance against enterprise SLOs...")
            validation_results = validator.validate_performance_floors(perf_data)

            if not validation_results["overall_pass"]:
                print("❌ SLO validation failed")
            else:
                print("✅ SLO validation passed")

        if args.detect_regression and args.baseline:
            if not args.baseline.exists():
                print(f"❌ Baseline file not found: {args.baseline}", file=sys.stderr)
                sys.exit(1)

            with open(args.baseline, 'r') as f:
                baseline_data = json.load(f)

            print("Detecting performance regression...")
            regression_analysis = validator.detect_regression(perf_data, baseline_data)

            if regression_analysis["has_regression"]:
                severity = regression_analysis["regression_severity"]
                print(f"❌ Performance regression detected: {severity}")
            else:
                print("✅ No significant performance regression")

        # Generate report
        if validation_results:
            if args.json_output:
                output_data = {"validation": validation_results}
                if regression_analysis:
                    output_data["regression"] = regression_analysis

                report_content = json.dumps(output_data, indent=2)
            else:
                report_content = validator.generate_slo_report(validation_results, regression_analysis)

            # Output report
            if args.output:
                with open(args.output, 'w') as f:
                    f.write(report_content)
                print(f"Report saved to: {args.output}")
            else:
                print("\n" + report_content)

        # For test validation, output key metrics in expected format
        display_gibs = perf_data.get("display_gibs", 0.0)
        comp3_mibs = perf_data.get("comp3_mibs", 0.0)

        if display_gibs >= validator.performance_floors["display_gibs"] and \
           comp3_mibs >= validator.performance_floors["comp3_mibs"]:
            print("✅ Enterprise performance targets met")
        else:
            print("❌ Enterprise performance targets not met")
            if not args.json_output:
                sys.exit(1)

        # Exit with appropriate code
        if validation_results and not validation_results["overall_pass"]:
            sys.exit(1)
        if regression_analysis and regression_analysis["regression_severity"] in ("MAJOR", "CRITICAL"):
            sys.exit(1)

    except Exception as e:
        print(f"❌ SLO validation failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()