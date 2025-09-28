#!/usr/bin/env python3
"""
Baseline promotion and management for copybook-rs performance monitoring.

This module provides baseline management capabilities for Issue #52 machine-readable
benchmark reporting infrastructure. Handles baseline promotion workflows,
performance tracking, and regression detection for enterprise deployment.

Features:
- Automated baseline promotion from successful CI runs
- Performance history tracking and trend analysis
- Regression threshold validation and alerting
- Enterprise audit trail generation for baseline changes
- Multi-environment baseline management (dev, staging, production)
- Statistical validation of baseline stability

Baseline Management Workflow:
1. Validate current performance against thresholds
2. Compare against existing baseline for regression detection
3. Promote new baseline if performance improvements are stable
4. Generate audit trail for compliance tracking
"""

import argparse
import json
import os
import shutil
import sys
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime, timedelta
import statistics


class BaselineManager:
    """Baseline promotion and management for performance monitoring."""

    def __init__(self, workspace_root: Optional[Path] = None):
        """Initialize baseline manager.

        Args:
            workspace_root: Path to copybook-rs workspace root. If None, auto-detect.
        """
        if workspace_root is None:
            # Auto-detect workspace root
            current = Path.cwd()
            while current != current.parent:
                if (current / "Cargo.toml").exists():
                    cargo_content = (current / "Cargo.toml").read_text()
                    if "copybook-" in cargo_content and "workspace" in cargo_content:
                        workspace_root = current
                        break
                current = current.parent

            if workspace_root is None:
                raise RuntimeError("Could not find copybook-rs workspace root")

        self.workspace_root = workspace_root
        self.baselines_dir = workspace_root / "baselines"
        self.history_dir = self.baselines_dir / "history"

        # Enterprise performance thresholds
        self.thresholds = {
            "display_floor_gibs": 4.1,     # Minimum DISPLAY performance
            "comp3_floor_mibs": 560.0,     # Minimum COMP-3 performance
            "regression_threshold": 0.02,   # 2% regression threshold
            "improvement_threshold": 0.01,  # 1% improvement to promote baseline
            "stability_period_days": 7,     # Days of stable performance for promotion
            "min_samples": 3                # Minimum samples for statistical validation
        }

        # Ensure baseline directories exist
        self.baselines_dir.mkdir(exist_ok=True)
        self.history_dir.mkdir(exist_ok=True)

    def load_current_baseline(self, environment: str = "main") -> Optional[Dict[str, Any]]:
        """Load current baseline for specified environment.

        Args:
            environment: Target environment (main, dev, staging, production)

        Returns:
            Baseline data dictionary or None if no baseline exists
        """
        baseline_file = self.baselines_dir / f"{environment}_baseline.json"

        if not baseline_file.exists():
            return None

        try:
            with open(baseline_file, 'r') as f:
                return json.load(f)
        except (json.JSONDecodeError, IOError) as e:
            print(f"⚠️  Failed to load baseline: {e}", file=sys.stderr)
            return None

    def save_baseline(self, performance_data: Dict[str, Any], environment: str = "main",
                     promotion_reason: str = "manual") -> bool:
        """Save new baseline for specified environment.

        Args:
            performance_data: Performance data to save as baseline
            environment: Target environment
            promotion_reason: Reason for baseline promotion (for audit trail)

        Returns:
            True if baseline saved successfully
        """
        try:
            # Create baseline metadata
            baseline_data = {
                "performance": performance_data.copy(),
                "metadata": {
                    "promoted_at": datetime.now().isoformat(),
                    "environment": environment,
                    "promotion_reason": promotion_reason,
                    "copybook_rs_version": performance_data.get("metadata", {}).get("copybook_rs_version", "unknown"),
                    "format_version": "1.0"
                }
            }

            # Save new baseline
            baseline_file = self.baselines_dir / f"{environment}_baseline.json"
            with open(baseline_file, 'w') as f:
                json.dump(baseline_data, f, indent=2, sort_keys=True)

            # Archive previous baseline to history
            self._archive_baseline_to_history(baseline_data, environment)

            print(f"✅ Baseline promoted for environment: {environment}")
            print(f"   DISPLAY: {performance_data.get('display_gibs', 0):.2f} GiB/s")
            print(f"   COMP-3: {performance_data.get('comp3_mibs', 0):.1f} MiB/s")
            print(f"   Reason: {promotion_reason}")

            return True

        except Exception as e:
            print(f"❌ Failed to save baseline: {e}", file=sys.stderr)
            return False

    def _archive_baseline_to_history(self, baseline_data: Dict[str, Any],
                                   environment: str) -> None:
        """Archive baseline to history for audit trail."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        history_file = self.history_dir / f"{environment}_baseline_{timestamp}.json"

        with open(history_file, 'w') as f:
            json.dump(baseline_data, f, indent=2, sort_keys=True)

    def validate_current_performance(self, performance_data: Dict[str, Any]) -> Tuple[bool, List[str]]:
        """Validate current performance against enterprise thresholds.

        Args:
            performance_data: Performance data to validate

        Returns:
            Tuple of (is_valid, list_of_issues)
        """
        issues = []

        display_gibs = performance_data.get("display_gibs", 0.0)
        comp3_mibs = performance_data.get("comp3_mibs", 0.0)

        # Check enterprise performance floors
        if display_gibs < self.thresholds["display_floor_gibs"]:
            issues.append(
                f"DISPLAY performance {display_gibs:.2f} GiB/s below enterprise floor "
                f"({self.thresholds['display_floor_gibs']} GiB/s)"
            )

        if comp3_mibs < self.thresholds["comp3_floor_mibs"]:
            issues.append(
                f"COMP-3 performance {comp3_mibs:.1f} MiB/s below enterprise floor "
                f"({self.thresholds['comp3_floor_mibs']} MiB/s)"
            )

        # Check for performance data errors
        errors = performance_data.get("errors", [])
        if errors:
            issues.extend([f"Performance error: {error}" for error in errors])

        return len(issues) == 0, issues

    def analyze_regression(self, current: Dict[str, Any],
                          baseline: Optional[Dict[str, Any]]) -> Dict[str, Any]:
        """Analyze performance regression against baseline.

        Args:
            current: Current performance data
            baseline: Baseline performance data

        Returns:
            Regression analysis results
        """
        if baseline is None:
            return {
                "has_baseline": False,
                "regression_detected": False,
                "analysis": "No baseline available for comparison"
            }

        baseline_perf = baseline.get("performance", baseline)
        current_display = current.get("display_gibs", 0.0)
        current_comp3 = current.get("comp3_mibs", 0.0)
        baseline_display = baseline_perf.get("display_gibs", 0.0)
        baseline_comp3 = baseline_perf.get("comp3_mibs", 0.0)

        analysis = {
            "has_baseline": True,
            "baseline_timestamp": baseline.get("metadata", {}).get("promoted_at", "unknown"),
            "display_change_percent": 0.0,
            "comp3_change_percent": 0.0,
            "display_regression": False,
            "comp3_regression": False,
            "overall_regression": False,
            "improvement_detected": False
        }

        # Calculate percentage changes
        if baseline_display > 0:
            analysis["display_change_percent"] = (
                (current_display - baseline_display) / baseline_display * 100
            )
            analysis["display_regression"] = (
                analysis["display_change_percent"] < -self.thresholds["regression_threshold"] * 100
            )

        if baseline_comp3 > 0:
            analysis["comp3_change_percent"] = (
                (current_comp3 - baseline_comp3) / baseline_comp3 * 100
            )
            analysis["comp3_regression"] = (
                analysis["comp3_change_percent"] < -self.thresholds["regression_threshold"] * 100
            )

        analysis["overall_regression"] = (
            analysis["display_regression"] or analysis["comp3_regression"]
        )

        # Check for significant improvements
        improvement_threshold = self.thresholds["improvement_threshold"] * 100
        analysis["improvement_detected"] = (
            analysis["display_change_percent"] > improvement_threshold or
            analysis["comp3_change_percent"] > improvement_threshold
        )

        return analysis

    def should_promote_baseline(self, current: Dict[str, Any],
                              baseline: Optional[Dict[str, Any]]) -> Tuple[bool, str]:
        """Determine if current performance should be promoted to baseline.

        Args:
            current: Current performance data
            baseline: Existing baseline data

        Returns:
            Tuple of (should_promote, reason)
        """
        # Validate current performance meets enterprise requirements
        is_valid, issues = self.validate_current_performance(current)
        if not is_valid:
            return False, f"Performance validation failed: {'; '.join(issues)}"

        # If no baseline exists, promote if valid
        if baseline is None:
            return True, "Initial baseline establishment"

        # Analyze regression
        regression_analysis = self.analyze_regression(current, baseline)

        # Don't promote if regression detected
        if regression_analysis["overall_regression"]:
            return False, (
                f"Performance regression detected: "
                f"DISPLAY {regression_analysis['display_change_percent']:+.2f}%, "
                f"COMP-3 {regression_analysis['comp3_change_percent']:+.2f}%"
            )

        # Promote if significant improvement detected
        if regression_analysis["improvement_detected"]:
            return True, (
                f"Performance improvement detected: "
                f"DISPLAY {regression_analysis['display_change_percent']:+.2f}%, "
                f"COMP-3 {regression_analysis['comp3_change_percent']:+.2f}%"
            )

        # Otherwise, maintain current baseline
        return False, "No significant change - maintaining current baseline"

    def get_performance_history(self, environment: str = "main",
                               days: int = 30) -> List[Dict[str, Any]]:
        """Get performance history for trend analysis.

        Args:
            environment: Target environment
            days: Number of days of history to retrieve

        Returns:
            List of historical performance data
        """
        history_files = []
        cutoff_date = datetime.now() - timedelta(days=days)

        # Find history files within date range
        pattern = f"{environment}_baseline_*.json"
        for file in self.history_dir.glob(pattern):
            try:
                # Extract timestamp from filename
                timestamp_str = file.stem.split("_")[-2] + "_" + file.stem.split("_")[-1]
                file_date = datetime.strptime(timestamp_str, "%Y%m%d_%H%M%S")

                if file_date >= cutoff_date:
                    with open(file, 'r') as f:
                        data = json.load(f)
                        data["file_date"] = file_date.isoformat()
                        history_files.append(data)

            except (ValueError, json.JSONDecodeError, IOError):
                continue

        # Sort by date
        history_files.sort(key=lambda x: x.get("file_date", ""))

        return history_files

    def generate_trend_analysis(self, environment: str = "main") -> Dict[str, Any]:
        """Generate performance trend analysis.

        Args:
            environment: Target environment

        Returns:
            Trend analysis results
        """
        history = self.get_performance_history(environment, days=30)

        if len(history) < 2:
            return {
                "sufficient_data": False,
                "message": "Insufficient historical data for trend analysis"
            }

        # Extract performance metrics
        display_values = []
        comp3_values = []

        for entry in history:
            perf_data = entry.get("performance", {})
            display_values.append(perf_data.get("display_gibs", 0.0))
            comp3_values.append(perf_data.get("comp3_mibs", 0.0))

        # Calculate trend statistics
        analysis = {
            "sufficient_data": True,
            "sample_count": len(history),
            "date_range": {
                "start": history[0].get("file_date", "unknown"),
                "end": history[-1].get("file_date", "unknown")
            },
            "display_stats": {
                "mean": statistics.mean(display_values),
                "median": statistics.median(display_values),
                "stdev": statistics.stdev(display_values) if len(display_values) > 1 else 0.0,
                "min": min(display_values),
                "max": max(display_values)
            },
            "comp3_stats": {
                "mean": statistics.mean(comp3_values),
                "median": statistics.median(comp3_values),
                "stdev": statistics.stdev(comp3_values) if len(comp3_values) > 1 else 0.0,
                "min": min(comp3_values),
                "max": max(comp3_values)
            }
        }

        # Calculate coefficient of variation for stability assessment
        display_cv = (analysis["display_stats"]["stdev"] / analysis["display_stats"]["mean"]) * 100
        comp3_cv = (analysis["comp3_stats"]["stdev"] / analysis["comp3_stats"]["mean"]) * 100

        analysis["stability"] = {
            "display_cv_percent": display_cv,
            "comp3_cv_percent": comp3_cv,
            "is_stable": display_cv < 5.0 and comp3_cv < 5.0  # <5% coefficient of variation
        }

        return analysis


def main():
    """Main entry point for baseline manager."""
    parser = argparse.ArgumentParser(
        description="copybook-rs baseline promotion and management",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python3 baseline_manager.py --validate-current --perf-file perf.json
  python3 baseline_manager.py --promote --perf-file perf.json --environment main
  python3 baseline_manager.py --analyze-trends --environment main
  python3 baseline_manager.py --compare-baseline --perf-file perf.json
        """
    )

    parser.add_argument(
        "--validate-current",
        action="store_true",
        help="Validate current performance against enterprise thresholds"
    )

    parser.add_argument(
        "--promote",
        action="store_true",
        help="Promote performance data to baseline"
    )

    parser.add_argument(
        "--compare-baseline",
        action="store_true",
        help="Compare current performance against baseline"
    )

    parser.add_argument(
        "--analyze-trends",
        action="store_true",
        help="Generate performance trend analysis"
    )

    parser.add_argument(
        "--perf-file",
        type=Path,
        help="Performance JSON file path"
    )

    parser.add_argument(
        "--environment",
        default="main",
        help="Target environment (main, dev, staging, production)"
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Perform analysis without making changes"
    )

    parser.add_argument(
        "--workspace-root",
        type=Path,
        help="Path to copybook-rs workspace root (auto-detected if not specified)"
    )

    args = parser.parse_args()

    try:
        manager = BaselineManager(workspace_root=args.workspace_root)

        # If no specific action, default to validation
        if not any([args.validate_current, args.promote, args.compare_baseline, args.analyze_trends]):
            args.validate_current = True

        # Load performance data if required
        performance_data = None
        if args.perf_file:
            if not args.perf_file.exists():
                print(f"❌ Performance file not found: {args.perf_file}", file=sys.stderr)
                sys.exit(1)

            with open(args.perf_file, 'r') as f:
                performance_data = json.load(f)

        # Validate current performance
        if args.validate_current:
            if not performance_data:
                print("❌ Performance file required for validation", file=sys.stderr)
                sys.exit(1)

            print("Validating current performance against enterprise thresholds...")
            is_valid, issues = manager.validate_current_performance(performance_data)

            if is_valid:
                print("✅ Performance validation passed")
                display_gibs = performance_data.get("display_gibs", 0)
                comp3_mibs = performance_data.get("comp3_mibs", 0)
                print(f"   DISPLAY: {display_gibs:.2f} GiB/s (≥{manager.thresholds['display_floor_gibs']} required)")
                print(f"   COMP-3: {comp3_mibs:.1f} MiB/s (≥{manager.thresholds['comp3_floor_mibs']} required)")
            else:
                print("❌ Performance validation failed:")
                for issue in issues:
                    print(f"   • {issue}")
                sys.exit(1)

        # Compare against baseline
        if args.compare_baseline:
            if not performance_data:
                print("❌ Performance file required for baseline comparison", file=sys.stderr)
                sys.exit(1)

            print(f"Comparing against baseline for environment: {args.environment}")
            baseline = manager.load_current_baseline(args.environment)
            analysis = manager.analyze_regression(performance_data, baseline)

            if not analysis["has_baseline"]:
                print("⚠️  No baseline available for comparison")
            else:
                print(f"Baseline timestamp: {analysis['baseline_timestamp']}")
                print(f"DISPLAY change: {analysis['display_change_percent']:+.2f}%")
                print(f"COMP-3 change: {analysis['comp3_change_percent']:+.2f}%")

                if analysis["overall_regression"]:
                    print("❌ Performance regression detected")
                    sys.exit(1)
                else:
                    print("✅ No significant performance regression")

        # Promote baseline
        if args.promote:
            if not performance_data:
                print("❌ Performance file required for baseline promotion", file=sys.stderr)
                sys.exit(1)

            baseline = manager.load_current_baseline(args.environment)
            should_promote, reason = manager.should_promote_baseline(performance_data, baseline)

            if should_promote:
                if args.dry_run:
                    print(f"DRY RUN: Would promote baseline - {reason}")
                else:
                    success = manager.save_baseline(performance_data, args.environment, reason)
                    if not success:
                        sys.exit(1)
            else:
                print(f"❌ Baseline promotion not recommended: {reason}")
                if not args.dry_run:
                    sys.exit(1)

        # Analyze trends
        if args.analyze_trends:
            print(f"Analyzing performance trends for environment: {args.environment}")
            analysis = manager.generate_trend_analysis(args.environment)

            if not analysis["sufficient_data"]:
                print(f"⚠️  {analysis['message']}")
            else:
                print(f"Analysis based on {analysis['sample_count']} samples")
                print(f"Date range: {analysis['date_range']['start']} to {analysis['date_range']['end']}")
                print()

                display_stats = analysis["display_stats"]
                comp3_stats = analysis["comp3_stats"]
                stability = analysis["stability"]

                print("DISPLAY Statistics:")
                print(f"  Mean: {display_stats['mean']:.2f} GiB/s")
                print(f"  Range: {display_stats['min']:.2f} - {display_stats['max']:.2f} GiB/s")
                print(f"  Std Dev: {display_stats['stdev']:.3f} GiB/s")
                print(f"  CV: {stability['display_cv_percent']:.2f}%")
                print()

                print("COMP-3 Statistics:")
                print(f"  Mean: {comp3_stats['mean']:.1f} MiB/s")
                print(f"  Range: {comp3_stats['min']:.1f} - {comp3_stats['max']:.1f} MiB/s")
                print(f"  Std Dev: {comp3_stats['stdev']:.2f} MiB/s")
                print(f"  CV: {stability['comp3_cv_percent']:.2f}%")
                print()

                if stability["is_stable"]:
                    print("✅ Performance is stable (CV < 5%)")
                else:
                    print("⚠️  Performance shows high variance (CV ≥ 5%)")

    except Exception as e:
        print(f"❌ Baseline management failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()