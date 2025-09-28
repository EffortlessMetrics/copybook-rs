#!/usr/bin/env python3
"""
JSON schema validation and processing for copybook-rs benchmark reports.

This module provides comprehensive JSON processing capabilities for Issue #52
machine-readable benchmark reporting infrastructure. Validates performance
reports against enterprise schema requirements and provides data transformation
utilities for CI/CD integration.

Features:
- JSON schema validation against copybook-rs performance report format
- Data transformation and normalization for enterprise systems
- Performance metrics extraction and validation
- Error detection and reporting with structured taxonomy
- Integration with baseline comparison systems

JSON Schema Format:
{
    "display_gibs": 4.22,
    "comp3_mibs": 571.0,
    "warnings": [],
    "errors": []
}
"""

import argparse
import json
import sys
from pathlib import Path
from typing import Dict, List, Any, Optional, Union
import re


class JSONProcessor:
    """JSON schema validation and processing for performance reports."""

    # Schema definition for copybook-rs performance reports
    PERFORMANCE_SCHEMA = {
        "type": "object",
        "required": ["display_gibs", "comp3_mibs", "warnings", "errors"],
        "properties": {
            "display_gibs": {
                "type": "number",
                "minimum": 0,
                "description": "DISPLAY parsing throughput in GiB/s"
            },
            "comp3_mibs": {
                "type": "number",
                "minimum": 0,
                "description": "COMP-3 encoding throughput in MiB/s"
            },
            "warnings": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Non-fatal warnings from benchmark execution"
            },
            "errors": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Fatal errors from benchmark execution"
            },
            "metadata": {
                "type": "object",
                "properties": {
                    "timestamp": {"type": "number"},
                    "environment": {"type": "object"},
                    "copybook_rs_version": {"type": "string"},
                    "benchmark_format_version": {"type": "string"}
                }
            }
        }
    }

    # Enterprise performance targets for validation
    ENTERPRISE_TARGETS = {
        "display_floor_gibs": 4.1,    # Minimum DISPLAY performance
        "comp3_floor_mibs": 560.0,    # Minimum COMP-3 performance
        "variance_tolerance": 0.05,   # 5% variance tolerance
        "regression_threshold": 0.02   # 2% regression threshold
    }

    def __init__(self):
        """Initialize JSON processor with enterprise validation rules."""
        self.validation_errors: List[str] = []
        self.validation_warnings: List[str] = []

    def validate_schema(self, data: Dict[str, Any]) -> bool:
        """Validate JSON data against copybook-rs performance schema.

        Args:
            data: JSON data to validate

        Returns:
            True if schema is valid, False otherwise

        Side Effects:
            Updates self.validation_errors and self.validation_warnings
        """
        self.validation_errors.clear()
        self.validation_warnings.clear()

        # Check required fields
        required_fields = self.PERFORMANCE_SCHEMA["required"]
        for field in required_fields:
            if field not in data:
                self.validation_errors.append(f"Missing required field: {field}")

        if self.validation_errors:
            return False

        # Validate field types and constraints
        self._validate_field("display_gibs", data.get("display_gibs"), "number", min_value=0)
        self._validate_field("comp3_mibs", data.get("comp3_mibs"), "number", min_value=0)
        self._validate_field("warnings", data.get("warnings"), "array")
        self._validate_field("errors", data.get("errors"), "array")

        # Validate array contents
        if isinstance(data.get("warnings"), list):
            for i, warning in enumerate(data["warnings"]):
                if not isinstance(warning, str):
                    self.validation_errors.append(f"warnings[{i}] must be string, got {type(warning).__name__}")

        if isinstance(data.get("errors"), list):
            for i, error in enumerate(data["errors"]):
                if not isinstance(error, str):
                    self.validation_errors.append(f"errors[{i}] must be string, got {type(error).__name__}")

        # Validate metadata if present
        if "metadata" in data:
            self._validate_metadata(data["metadata"])

        return len(self.validation_errors) == 0

    def _validate_field(self, field_name: str, value: Any, expected_type: str,
                       min_value: Optional[float] = None) -> None:
        """Validate individual field against schema constraints."""
        if value is None:
            return

        if expected_type == "number":
            if not isinstance(value, (int, float)):
                self.validation_errors.append(
                    f"{field_name} must be number, got {type(value).__name__}")
                return

            if min_value is not None and value < min_value:
                self.validation_errors.append(
                    f"{field_name} must be >= {min_value}, got {value}")

        elif expected_type == "array":
            if not isinstance(value, list):
                self.validation_errors.append(
                    f"{field_name} must be array, got {type(value).__name__}")

    def _validate_metadata(self, metadata: Dict[str, Any]) -> None:
        """Validate metadata object structure."""
        if not isinstance(metadata, dict):
            self.validation_errors.append("metadata must be object")
            return

        # Optional metadata validation
        if "timestamp" in metadata and not isinstance(metadata["timestamp"], (int, float)):
            self.validation_warnings.append("metadata.timestamp should be number")

        if "copybook_rs_version" in metadata:
            version = metadata["copybook_rs_version"]
            if not isinstance(version, str):
                self.validation_warnings.append("metadata.copybook_rs_version should be string")
            elif not re.match(r'^\d+\.\d+\.\d+', version):
                self.validation_warnings.append("metadata.copybook_rs_version should follow semver format")

    def validate_enterprise_targets(self, data: Dict[str, Any]) -> bool:
        """Validate performance data against enterprise targets.

        Args:
            data: Performance data to validate

        Returns:
            True if all enterprise targets are met
        """
        enterprise_errors = []

        display_gibs = data.get("display_gibs", 0)
        comp3_mibs = data.get("comp3_mibs", 0)

        if display_gibs < self.ENTERPRISE_TARGETS["display_floor_gibs"]:
            enterprise_errors.append(
                f"DISPLAY performance {display_gibs:.2f} GiB/s below enterprise floor "
                f"({self.ENTERPRISE_TARGETS['display_floor_gibs']} GiB/s)")

        if comp3_mibs < self.ENTERPRISE_TARGETS["comp3_floor_mibs"]:
            enterprise_errors.append(
                f"COMP-3 performance {comp3_mibs:.1f} MiB/s below enterprise floor "
                f"({self.ENTERPRISE_TARGETS['comp3_floor_mibs']} MiB/s)")

        if enterprise_errors:
            self.validation_errors.extend(enterprise_errors)
            return False

        return True

    def extract_metrics(self, data: Dict[str, Any]) -> Dict[str, Union[float, List[str]]]:
        """Extract key performance metrics from JSON data.

        Args:
            data: Performance report JSON data

        Returns:
            Dictionary with extracted metrics in normalized format
        """
        return {
            "display_throughput_gibs": data.get("display_gibs", 0.0),
            "comp3_throughput_mibs": data.get("comp3_mibs", 0.0),
            "warning_count": len(data.get("warnings", [])),
            "error_count": len(data.get("errors", [])),
            "warnings": data.get("warnings", []),
            "errors": data.get("errors", []),
            "has_performance_issues": (
                data.get("display_gibs", 0) < self.ENTERPRISE_TARGETS["display_floor_gibs"] or
                data.get("comp3_mibs", 0) < self.ENTERPRISE_TARGETS["comp3_floor_mibs"]
            )
        }

    def normalize_data(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Normalize performance data for enterprise systems integration.

        Args:
            data: Raw performance data

        Returns:
            Normalized data with enterprise-compatible formatting
        """
        normalized = data.copy()

        # Round performance metrics to appropriate precision
        if "display_gibs" in normalized:
            normalized["display_gibs"] = round(float(normalized["display_gibs"]), 2)

        if "comp3_mibs" in normalized:
            normalized["comp3_mibs"] = round(float(normalized["comp3_mibs"]), 1)

        # Ensure arrays are present
        if "warnings" not in normalized:
            normalized["warnings"] = []
        if "errors" not in normalized:
            normalized["errors"] = []

        # Add enterprise validation flags
        metrics = self.extract_metrics(normalized)
        normalized["enterprise_compliance"] = {
            "meets_display_target": metrics["display_throughput_gibs"] >= self.ENTERPRISE_TARGETS["display_floor_gibs"],
            "meets_comp3_target": metrics["comp3_throughput_mibs"] >= self.ENTERPRISE_TARGETS["comp3_floor_mibs"],
            "has_warnings": metrics["warning_count"] > 0,
            "has_errors": metrics["error_count"] > 0,
            "validation_timestamp": data.get("metadata", {}).get("timestamp")
        }

        return normalized

    def compare_baselines(self, current: Dict[str, Any],
                         baseline: Dict[str, Any]) -> Dict[str, Any]:
        """Compare current performance against baseline for regression detection.

        Args:
            current: Current performance data
            baseline: Baseline performance data

        Returns:
            Comparison results with regression analysis
        """
        comparison = {
            "display_regression": False,
            "comp3_regression": False,
            "display_change_percent": 0.0,
            "comp3_change_percent": 0.0,
            "overall_regression": False
        }

        current_display = current.get("display_gibs", 0)
        baseline_display = baseline.get("display_gibs", 0)
        current_comp3 = current.get("comp3_mibs", 0)
        baseline_comp3 = baseline.get("comp3_mibs", 0)

        if baseline_display > 0:
            comparison["display_change_percent"] = (
                (current_display - baseline_display) / baseline_display * 100
            )
            comparison["display_regression"] = (
                comparison["display_change_percent"] < -self.ENTERPRISE_TARGETS["regression_threshold"] * 100
            )

        if baseline_comp3 > 0:
            comparison["comp3_change_percent"] = (
                (current_comp3 - baseline_comp3) / baseline_comp3 * 100
            )
            comparison["comp3_regression"] = (
                comparison["comp3_change_percent"] < -self.ENTERPRISE_TARGETS["regression_threshold"] * 100
            )

        comparison["overall_regression"] = (
            comparison["display_regression"] or comparison["comp3_regression"]
        )

        return comparison


def main():
    """Main entry point for JSON processor."""
    parser = argparse.ArgumentParser(
        description="copybook-rs JSON schema validation and processing",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python3 json_processor.py --validate perf.json
  python3 json_processor.py --extract-metrics perf.json
  python3 json_processor.py --normalize perf.json --output normalized.json
  python3 json_processor.py --compare-baseline current.json baseline.json
        """
    )

    parser.add_argument(
        "input_file",
        type=Path,
        help="Input JSON file to process"
    )

    parser.add_argument(
        "--validate",
        action="store_true",
        help="Validate JSON against copybook-rs schema"
    )

    parser.add_argument(
        "--extract-metrics",
        action="store_true",
        help="Extract key performance metrics"
    )

    parser.add_argument(
        "--normalize",
        action="store_true",
        help="Normalize data for enterprise systems"
    )

    parser.add_argument(
        "--compare-baseline",
        type=Path,
        help="Compare against baseline file for regression detection"
    )

    parser.add_argument(
        "--output", "-o",
        type=Path,
        help="Output file for processed data"
    )

    parser.add_argument(
        "--enterprise-targets",
        action="store_true",
        help="Validate against enterprise performance targets"
    )

    args = parser.parse_args()

    try:
        processor = JSONProcessor()

        # Load input data
        if not args.input_file.exists():
            print(f"❌ Input file not found: {args.input_file}", file=sys.stderr)
            sys.exit(1)

        with open(args.input_file, 'r') as f:
            data = json.load(f)

        # Perform requested operations
        if args.validate or not any([args.extract_metrics, args.normalize, args.compare_baseline]):
            print("Validating JSON schema...")
            is_valid = processor.validate_schema(data)

            if processor.validation_errors:
                print("❌ Schema validation errors:")
                for error in processor.validation_errors:
                    print(f"  • {error}")

            if processor.validation_warnings:
                print("⚠️  Schema validation warnings:")
                for warning in processor.validation_warnings:
                    print(f"  • {warning}")

            if is_valid:
                print("✅ JSON schema validation passed")
            else:
                sys.exit(1)

        if args.enterprise_targets:
            print("Validating enterprise performance targets...")
            meets_targets = processor.validate_enterprise_targets(data)

            if meets_targets:
                print("✅ Enterprise performance targets met")
            else:
                print("❌ Enterprise performance targets not met")
                for error in processor.validation_errors:
                    if "floor" in error:
                        print(f"  • {error}")
                sys.exit(1)

        if args.extract_metrics:
            print("Extracting performance metrics...")
            metrics = processor.extract_metrics(data)

            output_data = metrics
            if args.output:
                with open(args.output, 'w') as f:
                    json.dump(output_data, f, indent=2)
                print(f"Metrics written to: {args.output}")
            else:
                print(json.dumps(output_data, indent=2))

        if args.normalize:
            print("Normalizing data for enterprise systems...")
            normalized = processor.normalize_data(data)

            if args.output:
                with open(args.output, 'w') as f:
                    json.dump(normalized, f, indent=2)
                print(f"Normalized data written to: {args.output}")
            else:
                print(json.dumps(normalized, indent=2))

        if args.compare_baseline:
            print("Comparing against baseline...")

            if not args.compare_baseline.exists():
                print(f"❌ Baseline file not found: {args.compare_baseline}", file=sys.stderr)
                sys.exit(1)

            with open(args.compare_baseline, 'r') as f:
                baseline = json.load(f)

            comparison = processor.compare_baselines(data, baseline)

            print(f"DISPLAY change: {comparison['display_change_percent']:+.2f}%")
            print(f"COMP-3 change: {comparison['comp3_change_percent']:+.2f}%")

            if comparison["overall_regression"]:
                print("❌ Performance regression detected")
                sys.exit(1)
            else:
                print("✅ No significant performance regression")

            if args.output:
                with open(args.output, 'w') as f:
                    json.dump(comparison, f, indent=2)

    except Exception as e:
        print(f"❌ JSON processing failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()