<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Python Utilities Architecture for Machine-Readable Benchmark Reporting

## Document Information

- **Document Type**: Python Utilities Architecture Specification
- **Component**: scripts/bench/ Python Infrastructure
- **Issue**: #52 - Machine-Readable Benchmark Reporting Infrastructure
- **Status**: Implementation Ready
- **Version**: 1.0
- **Date**: 2025-09-28

## Overview

This document defines the complete architecture for the Python utilities suite that provides machine-readable benchmark reporting, PR automation, baseline management, and enterprise audit capabilities for copybook-rs. The utilities integrate seamlessly with the existing Rust-based copybook-bench infrastructure while providing enterprise-grade automation and compliance features.

## Directory Structure

```
scripts/bench/
├── README.md                          # Usage documentation and examples
├── requirements.txt                   # Python dependencies with version pins
├── setup.py                          # Package installation configuration
├── pytest.ini                        # Test configuration
├── .pylintrc                         # Code quality configuration
├── bench_runner.py                   # Main benchmark execution coordinator
├── json_processor.py                 # JSON report processing and validation
├── pr_automation.py                  # GitHub PR automation engine
├── baseline_manager.py               # Baseline promotion workflows
├── audit_generator.py                # Enterprise audit reporting
├── slo_validator.py                  # Performance floor validation
├── interfaces.py                     # Public API contracts and abstractions
├── exceptions.py                     # Custom exception hierarchy
├── utils.py                          # Common utilities and helpers
├── config/
│   ├── __init__.py
│   ├── thresholds.toml               # Performance thresholds configuration
│   ├── audit_config.yaml             # Enterprise audit configuration
│   ├── pr_template.md                # PR comment template
│   └── schema.json                   # JSON schema definition
├── templates/
│   ├── __init__.py
│   ├── performance_report.html       # HTML performance report template
│   ├── audit_report.html             # Enterprise audit report template
│   ├── trend_analysis.html           # Historical trend analysis template
│   └── compliance_summary.html       # Regulatory compliance template
├── tests/
│   ├── __init__.py
│   ├── test_bench_runner.py
│   ├── test_json_processor.py
│   ├── test_pr_automation.py
│   ├── test_baseline_manager.py
│   ├── test_audit_generator.py
│   ├── test_slo_validator.py
│   ├── fixtures/
│   │   ├── sample_perf.json
│   │   ├── sample_criterion_output.json
│   │   └── test_baselines/
│   └── integration/
│       ├── test_e2e_workflow.py
│       └── test_ci_integration.py
└── cli/
    ├── __init__.py
    ├── main.py                       # Main CLI entry point
    ├── benchmark.py                  # Benchmark commands
    ├── baseline.py                   # Baseline management commands
    └── audit.py                      # Audit and reporting commands
```

## Core Module Architecture

### 1. bench_runner.py - Main Benchmark Coordinator

**Purpose**: Orchestrates benchmark execution, extracts performance metrics, and generates JSON reports.

```python
"""Main benchmark execution and coordination for copybook-rs performance monitoring."""

import json
import logging
import os
import subprocess
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Any, Optional, List, Tuple
import tempfile
import shutil

from dataclasses import dataclass, asdict
from .interfaces import BenchmarkExecutor, BenchmarkConfig
from .exceptions import BenchmarkExecutionError, MetricExtractionError
from .slo_validator import SloValidator
from .json_processor import JsonProcessor
from .utils import setup_logging, get_git_info, get_environment_info

@dataclass
class BenchmarkResults:
    """Structured benchmark execution results"""
    execution_time_seconds: float
    criterion_data: Dict[str, Any]
    raw_output: str
    stderr_output: str
    return_code: int
    environment_snapshot: Dict[str, Any]

class CopybookBenchmarkRunner(BenchmarkExecutor):
    """Main benchmark execution coordinator for copybook-rs"""

    def __init__(self, config: BenchmarkConfig):
        self.config = config
        self.logger = setup_logging(__name__)
        self.slo_validator = SloValidator()
        self.json_processor = JsonProcessor()

    def execute_benchmarks(self) -> Dict[str, Any]:
        """Execute cargo bench and return comprehensive performance report"""
        try:
            self.logger.info(f"Starting benchmark execution with config: {self.config}")

            # Pre-execution validation
            self._validate_execution_environment()

            # Execute benchmarks with monitoring
            benchmark_results = self._execute_cargo_bench()

            # Extract performance metrics
            performance_metrics = self._extract_performance_metrics(benchmark_results)

            # Validate SLO compliance
            slo_validation = self.slo_validator.validate_performance_floors(
                performance_metrics.get('display_gibs', 0.0),
                performance_metrics.get('comp3_mibs', 0.0)
            )

            # Generate comprehensive report
            report = self._generate_comprehensive_report(
                performance_metrics,
                slo_validation,
                benchmark_results
            )

            # Save report to configured output path
            self._save_report(report)

            self.logger.info(f"Benchmark execution completed successfully")
            return report

        except Exception as e:
            self.logger.error(f"Benchmark execution failed: {e}")
            raise BenchmarkExecutionError(f"Failed to execute benchmarks: {e}") from e

    def _validate_execution_environment(self) -> None:
        """Validate environment is suitable for benchmark execution"""
        # Check Rust toolchain
        try:
            rust_version = subprocess.run(
                ['rustc', '--version'],
                capture_output=True,
                text=True,
                check=True
            )
            self.logger.debug(f"Rust version: {rust_version.stdout.strip()}")
        except subprocess.CalledProcessError as e:
            raise BenchmarkExecutionError(f"Rust toolchain not available: {e}")

        # Check cargo bench availability
        try:
            subprocess.run(
                ['cargo', '--version'],
                capture_output=True,
                text=True,
                check=True
            )
        except subprocess.CalledProcessError as e:
            raise BenchmarkExecutionError(f"Cargo not available: {e}")

        # Validate copybook-bench package exists
        cargo_toml_path = Path("copybook-bench/Cargo.toml")
        if not cargo_toml_path.exists():
            raise BenchmarkExecutionError(f"copybook-bench package not found at {cargo_toml_path}")

    def _execute_cargo_bench(self) -> BenchmarkResults:
        """Execute cargo bench with comprehensive monitoring"""
        start_time = time.time()

        # Prepare environment
        env = os.environ.copy()
        if self.config.perf_mode:
            env['PERF'] = '1'
        if self.config.enterprise_validation:
            env['GENERATE_JSON_REPORT'] = '1'

        # Prepare command
        cmd = ['cargo', 'bench', '-p', 'copybook-bench']
        if self.config.baseline_id:
            cmd.extend(['--save-baseline', self.config.baseline_id])

        self.logger.info(f"Executing benchmark command: {' '.join(cmd)}")

        try:
            # Execute with timeout and capture
            result = subprocess.run(
                cmd,
                env=env,
                capture_output=True,
                text=True,
                timeout=self.config.timeout_seconds,
                cwd=Path.cwd(),  # Ensure we're in the right directory
            )

            execution_time = time.time() - start_time

            if result.returncode != 0:
                raise BenchmarkExecutionError(
                    f"Benchmark execution failed with exit code {result.returncode}.\n"
                    f"STDOUT: {result.stdout}\n"
                    f"STDERR: {result.stderr}"
                )

            # Parse criterion output for structured data
            criterion_data = self._parse_criterion_output(result.stdout)

            return BenchmarkResults(
                execution_time_seconds=execution_time,
                criterion_data=criterion_data,
                raw_output=result.stdout,
                stderr_output=result.stderr,
                return_code=result.returncode,
                environment_snapshot=get_environment_info()
            )

        except subprocess.TimeoutExpired:
            raise BenchmarkExecutionError(
                f"Benchmark execution timed out after {self.config.timeout_seconds} seconds"
            )
        except Exception as e:
            raise BenchmarkExecutionError(f"Unexpected error during benchmark execution: {e}") from e

    def _parse_criterion_output(self, stdout: str) -> Dict[str, Any]:
        """Parse criterion output to extract structured benchmark data"""
        try:
            # Look for criterion JSON output or parse text output
            criterion_data = {
                'benchmarks': [],
                'groups': [],
                'summary': {}
            }

            # Parse text output for throughput measurements
            lines = stdout.split('\n')
            current_group = None

            for line in lines:
                line = line.strip()

                # Group detection
                if 'Benchmarking' in line:
                    current_group = line.replace('Benchmarking ', '').strip()
                    criterion_data['groups'].append(current_group)

                # Throughput extraction (looking for format: "time: [x.xx xx.xx xx.xx] thrpt: [x.xx xx.xx xx.xx] elem/s")
                if 'thrpt:' in line and current_group:
                    benchmark_entry = self._extract_throughput_from_line(line, current_group)
                    if benchmark_entry:
                        criterion_data['benchmarks'].append(benchmark_entry)

            return criterion_data

        except Exception as e:
            self.logger.warning(f"Failed to parse criterion output: {e}")
            return {'benchmarks': [], 'groups': [], 'summary': {}}

    def _extract_throughput_from_line(self, line: str, group: str) -> Optional[Dict[str, Any]]:
        """Extract throughput measurement from criterion output line"""
        try:
            # Example line: "time: [4.2841 s 4.3156 s 4.3513 s] thrpt: [4.22 GiB/s 4.25 GiB/s 4.28 GiB/s]"
            if 'thrpt:' not in line:
                return None

            # Extract throughput values
            thrpt_part = line.split('thrpt:')[1].strip()
            # Parse format: [x.xx unit x.xx unit x.xx unit]
            import re
            pattern = r'\[([\d.]+)\s+(\w+/s)\s+([\d.]+)\s+(\w+/s)\s+([\d.]+)\s+(\w+/s)\]'
            match = re.search(pattern, thrpt_part)

            if match:
                low_val, low_unit = float(match.group(1)), match.group(2)
                mid_val, mid_unit = float(match.group(3)), match.group(4)
                high_val, high_unit = float(match.group(5)), match.group(6)

                return {
                    'name': group,
                    'throughput': {
                        'estimate': mid_val,
                        'lower_bound': low_val,
                        'upper_bound': high_val,
                        'unit': mid_unit
                    }
                }

            return None

        except Exception as e:
            self.logger.debug(f"Failed to extract throughput from line: {line}, error: {e}")
            return None

    def _extract_performance_metrics(self, results: BenchmarkResults) -> Dict[str, float]:
        """Extract DISPLAY and COMP-3 performance metrics from benchmark results"""
        try:
            metrics = {
                'display_gibs': 0.0,
                'comp3_mibs': 0.0,
                'parse_time_ms': 0.0,
                'memory_efficiency_score': 0.0
            }

            # Extract from criterion data
            for benchmark in results.criterion_data.get('benchmarks', []):
                name = benchmark.get('name', '').lower()
                throughput = benchmark.get('throughput', {})
                estimate = throughput.get('estimate', 0.0)
                unit = throughput.get('unit', '')

                # DISPLAY-heavy benchmarks
                if 'display' in name and 'gibs' in unit.lower():
                    metrics['display_gibs'] = max(metrics['display_gibs'], estimate)

                # COMP-3-heavy benchmarks
                elif 'comp3' in name and ('mibs' in unit.lower() or 'mib/s' in unit.lower()):
                    metrics['comp3_mibs'] = max(metrics['comp3_mibs'], estimate)

                # Parse time benchmarks
                elif 'parse' in name:
                    # Convert to milliseconds if needed
                    if 'ms' in unit.lower():
                        metrics['parse_time_ms'] = estimate
                    elif 's' in unit.lower():
                        metrics['parse_time_ms'] = estimate * 1000

            # If no direct extraction, try parsing from raw output
            if metrics['display_gibs'] == 0.0 or metrics['comp3_mibs'] == 0.0:
                self._extract_from_raw_output(results.raw_output, metrics)

            return metrics

        except Exception as e:
            raise MetricExtractionError(f"Failed to extract performance metrics: {e}") from e

    def _extract_from_raw_output(self, raw_output: str, metrics: Dict[str, float]) -> None:
        """Fallback extraction from raw text output"""
        import re

        # Look for GiB/s measurements
        gibs_pattern = r'([\d.]+)\s+GiB/s'
        gibs_matches = re.findall(gibs_pattern, raw_output)
        if gibs_matches:
            metrics['display_gibs'] = max(float(match) for match in gibs_matches)

        # Look for MiB/s measurements
        mibs_pattern = r'([\d.]+)\s+MiB/s'
        mibs_matches = re.findall(mibs_pattern, raw_output)
        if mibs_matches:
            metrics['comp3_mibs'] = max(float(match) for match in mibs_matches)

    def _generate_comprehensive_report(
        self,
        metrics: Dict[str, float],
        slo_validation: Any,
        benchmark_results: BenchmarkResults
    ) -> Dict[str, Any]:
        """Generate comprehensive performance report with enterprise metadata"""

        git_info = get_git_info()

        report = {
            "schema_version": "1.0",
            "display_gibs": round(metrics.get('display_gibs', 0.0), 2),
            "comp3_mibs": round(metrics.get('comp3_mibs', 0.0), 1),
            "warnings": slo_validation.warnings,
            "errors": slo_validation.errors,
            "_metadata": {
                "timestamp": datetime.now(timezone.utc).isoformat(),
                "git_commit": git_info.get('commit_hash'),
                "rust_version": self._get_rust_version(),
                "environment": {
                    "platform": benchmark_results.environment_snapshot.get('platform'),
                    "optimization_level": "release" if self.config.perf_mode else "debug",
                    "cpu_cores": benchmark_results.environment_snapshot.get('cpu_cores'),
                    "memory_gb": benchmark_results.environment_snapshot.get('memory_gb')
                },
                "execution_context": {
                    "execution_time_seconds": benchmark_results.execution_time_seconds,
                    "config": asdict(self.config),
                    "safety_margins": {
                        "display_safety_factor": slo_validation.safety_margins.display_safety_factor,
                        "comp3_safety_factor": slo_validation.safety_margins.comp3_safety_factor
                    }
                }
            }
        }

        return report

    def _get_rust_version(self) -> str:
        """Get current Rust version"""
        try:
            result = subprocess.run(
                ['rustc', '--version'],
                capture_output=True,
                text=True,
                check=True
            )
            return result.stdout.strip()
        except subprocess.CalledProcessError:
            return "unknown"

    def _save_report(self, report: Dict[str, Any]) -> None:
        """Save performance report to configured output path with validation"""
        try:
            # Ensure output directory exists
            self.config.output_path.parent.mkdir(parents=True, exist_ok=True)

            # Validate report against schema
            validation_result = self.json_processor.validate_report(report)
            if not validation_result.is_valid:
                self.logger.warning(f"Report validation warnings: {validation_result.warnings}")
                if validation_result.errors:
                    raise ValueError(f"Report validation failed: {validation_result.errors}")

            # Save with atomic write
            temp_path = self.config.output_path.with_suffix('.tmp')
            with open(temp_path, 'w', encoding='utf-8') as f:
                json.dump(report, f, indent=2, ensure_ascii=False)

            # Atomic move
            temp_path.replace(self.config.output_path)

            self.logger.info(f"Performance report saved to {self.config.output_path}")

        except Exception as e:
            raise BenchmarkExecutionError(f"Failed to save performance report: {e}") from e

def main():
    """CLI entry point for benchmark execution"""
    import argparse

    parser = argparse.ArgumentParser(description="Execute copybook-rs benchmarks with JSON reporting")
    parser.add_argument('--output', type=Path, default=Path('scripts/bench/perf.json'),
                       help='Output path for JSON report')
    parser.add_argument('--baseline-id', type=str, help='Baseline ID for comparison')
    parser.add_argument('--timeout', type=int, default=3600, help='Timeout in seconds')
    parser.add_argument('--no-perf', action='store_true', help='Disable PERF=1 mode')
    parser.add_argument('--verbose', '-v', action='store_true', help='Enable verbose logging')

    args = parser.parse_args()

    # Setup logging level
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )

    # Create configuration
    config = BenchmarkConfig(
        perf_mode=not args.no_perf,
        output_path=args.output,
        baseline_id=args.baseline_id,
        timeout_seconds=args.timeout
    )

    # Execute benchmarks
    try:
        runner = CopybookBenchmarkRunner(config)
        report = runner.execute_benchmarks()

        print(f"✅ Benchmarks completed successfully")
        print(f"📊 DISPLAY: {report['display_gibs']:.2f} GiB/s")
        print(f"📊 COMP-3: {report['comp3_mibs']:.1f} MiB/s")

        if report['warnings']:
            print(f"⚠️  Warnings: {len(report['warnings'])}")
        if report['errors']:
            print(f"❌ Errors: {len(report['errors'])}")
            sys.exit(1)

    except Exception as e:
        logging.error(f"Benchmark execution failed: {e}")
        sys.exit(1)

if __name__ == '__main__':
    main()
```

### 2. slo_validator.py - Performance Floor Validation

**Purpose**: Validates performance metrics against enterprise SLO floors and calculates safety margins.

```python
"""SLO validation and performance floor enforcement for copybook-rs benchmarks."""

import logging
from dataclasses import dataclass
from typing import List, Dict, Any
from .interfaces import PerformanceValidator, ValidationResult
from .exceptions import ValidationError

@dataclass
class PerformanceFloors:
    """Enterprise performance floor definitions"""
    display_mbps_floor: float = 80.0      # 80 MB/s minimum for DISPLAY
    comp3_mbps_floor: float = 40.0        # 40 MB/s minimum for COMP-3
    memory_mb_ceiling: float = 256.0      # 256 MiB memory ceiling
    variance_tolerance: float = 0.05      # 5% maximum variance

    # Warning thresholds (approaching floors)
    warning_factor: float = 1.25          # Warn at 25% above floor

@dataclass
class SafetyMargins:
    """Safety margin calculations vs performance floors"""
    display_safety_factor: float
    comp3_safety_factor: float
    memory_efficiency_score: float
    variance_stability_score: float

class SloValidator(PerformanceValidator):
    """Enterprise SLO validation for copybook-rs performance metrics"""

    def __init__(self, floors: PerformanceFloors = None):
        self.floors = floors or PerformanceFloors()
        self.logger = logging.getLogger(__name__)

    def validate_performance_floors(self, display_gibs: float, comp3_mibs: float) -> ValidationResult:
        """Validate performance metrics against enterprise floors"""
        try:
            warnings = []
            errors = []

            # Convert GiB/s to MB/s for DISPLAY (1 GiB/s ≈ 1073.74 MB/s)
            display_mbps = display_gibs * 1073.74

            # Validate DISPLAY floor
            display_validation = self._validate_display_performance(display_mbps)
            warnings.extend(display_validation['warnings'])
            errors.extend(display_validation['errors'])

            # Validate COMP-3 floor
            comp3_validation = self._validate_comp3_performance(comp3_mibs)
            warnings.extend(comp3_validation['warnings'])
            errors.extend(comp3_validation['errors'])

            # Calculate safety margins
            safety_margins = self.calculate_safety_margins({
                'display_gibs': display_gibs,
                'comp3_mibs': comp3_mibs
            })

            # Determine compliance level
            compliance_level = self._determine_compliance_level(errors, warnings, safety_margins)

            return ValidationResult(
                passed=len(errors) == 0,
                warnings=warnings,
                errors=errors,
                safety_margins=safety_margins.__dict__,
                compliance_level=compliance_level
            )

        except Exception as e:
            raise ValidationError(f"SLO validation failed: {e}") from e

    def _validate_display_performance(self, display_mbps: float) -> Dict[str, List[str]]:
        """Validate DISPLAY processing performance"""
        warnings = []
        errors = []

        if display_mbps < self.floors.display_mbps_floor:
            errors.append(
                f"DISPLAY throughput {display_mbps:.2f} MB/s below enterprise floor "
                f"of {self.floors.display_mbps_floor:.2f} MB/s "
                f"(deficit: {self.floors.display_mbps_floor - display_mbps:.2f} MB/s)"
            )
        elif display_mbps < self.floors.display_mbps_floor * self.floors.warning_factor:
            warnings.append(
                f"DISPLAY throughput {display_mbps:.2f} MB/s approaching floor threshold "
                f"of {self.floors.display_mbps_floor:.2f} MB/s "
                f"(margin: {display_mbps - self.floors.display_mbps_floor:.2f} MB/s)"
            )

        return {'warnings': warnings, 'errors': errors}

    def _validate_comp3_performance(self, comp3_mibs: float) -> Dict[str, List[str]]:
        """Validate COMP-3 processing performance"""
        warnings = []
        errors = []

        if comp3_mibs < self.floors.comp3_mbps_floor:
            errors.append(
                f"COMP-3 throughput {comp3_mibs:.2f} MiB/s below enterprise floor "
                f"of {self.floors.comp3_mbps_floor:.2f} MB/s "
                f"(deficit: {self.floors.comp3_mbps_floor - comp3_mibs:.2f} MB/s)"
            )
        elif comp3_mibs < self.floors.comp3_mbps_floor * self.floors.warning_factor:
            warnings.append(
                f"COMP-3 throughput {comp3_mibs:.2f} MiB/s approaching floor threshold "
                f"of {self.floors.comp3_mbps_floor:.2f} MB/s "
                f"(margin: {comp3_mibs - self.floors.comp3_mbps_floor:.2f} MB/s)"
            )

        return {'warnings': warnings, 'errors': errors}

    def calculate_safety_margins(self, metrics: Dict[str, float]) -> SafetyMargins:
        """Calculate safety margins vs minimum performance floors"""
        try:
            display_gibs = metrics.get('display_gibs', 0.0)
            comp3_mibs = metrics.get('comp3_mibs', 0.0)

            # Convert DISPLAY to MB/s for comparison
            display_mbps = display_gibs * 1073.74

            # Calculate safety factors
            display_safety_factor = display_mbps / self.floors.display_mbps_floor
            comp3_safety_factor = comp3_mibs / self.floors.comp3_mbps_floor

            # Memory efficiency score (placeholder - would integrate with actual memory metrics)
            memory_efficiency_score = min(self.floors.memory_mb_ceiling / 200.0, 1.0)  # Assume 200MB usage

            # Variance stability score (placeholder - would integrate with statistical data)
            variance_stability_score = max(0.0, 1.0 - self.floors.variance_tolerance)

            return SafetyMargins(
                display_safety_factor=display_safety_factor,
                comp3_safety_factor=comp3_safety_factor,
                memory_efficiency_score=memory_efficiency_score,
                variance_stability_score=variance_stability_score
            )

        except Exception as e:
            raise ValidationError(f"Failed to calculate safety margins: {e}") from e

    def _determine_compliance_level(
        self,
        errors: List[str],
        warnings: List[str],
        safety_margins: SafetyMargins
    ) -> str:
        """Determine enterprise compliance level"""

        if errors:
            return "NonCompliant"

        # Check safety margins for compliance assessment
        min_safety_factor = 10.0  # Require 10x safety margin for full compliance
        strong_safety_factor = 5.0  # Require 5x for conditional compliance

        min_margin = min(safety_margins.display_safety_factor, safety_margins.comp3_safety_factor)

        if min_margin >= min_safety_factor and not warnings:
            return "FullyCompliant"
        elif min_margin >= strong_safety_factor:
            return "ConditionallyCompliant"
        elif warnings:
            return "RequiresAttention"
        else:
            return "ConditionallyCompliant"

    def validate_slo_compliance(self, metrics: Dict[str, float]) -> ValidationResult:
        """Interface implementation for SLO compliance validation"""
        return self.validate_performance_floors(
            metrics.get('display_gibs', 0.0),
            metrics.get('comp3_mibs', 0.0)
        )
```

### 3. pr_automation.py - GitHub PR Automation Engine

**Purpose**: Handles GitHub PR comment automation and baseline management integration.

```python
"""GitHub PR automation for copybook-rs performance reporting."""

import os
import json
import requests
import logging
from typing import Dict, Any, Optional, List
from pathlib import Path
from dataclasses import dataclass
from .interfaces import GitHubAutomator
from .exceptions import GitHubApiError, AutomationError

@dataclass
class GitHubConfig:
    """GitHub API configuration"""
    token: str
    repo: str  # Format: "owner/repo"
    api_base: str = "https://api.github.com"
    timeout_seconds: int = 30
    retry_attempts: int = 3

class GitHubPrAutomation(GitHubAutomator):
    """GitHub PR automation engine for performance reporting"""

    def __init__(self, config: GitHubConfig):
        self.config = config
        self.logger = logging.getLogger(__name__)
        self.session = self._create_session()

    def _create_session(self) -> requests.Session:
        """Create configured requests session with authentication"""
        session = requests.Session()
        session.headers.update({
            "Authorization": f"token {self.config.token}",
            "Accept": "application/vnd.github.v3+json",
            "User-Agent": "copybook-rs-benchmark-automation/1.0"
        })
        session.timeout = self.config.timeout_seconds
        return session

    def post_performance_comment(self, pr_number: int, perf_data: Dict[str, Any]) -> bool:
        """Post one-liner performance summary to PR with detailed breakdown"""
        try:
            self.logger.info(f"Posting performance comment to PR #{pr_number}")

            # Generate performance comment
            comment_body = self._generate_performance_comment(perf_data)

            # Check for existing performance comments to update
            existing_comment_id = self._find_existing_performance_comment(pr_number)

            if existing_comment_id:
                success = self._update_comment(existing_comment_id, comment_body)
                if success:
                    self.logger.info(f"Updated existing performance comment {existing_comment_id} on PR #{pr_number}")
                else:
                    # Fallback to new comment if update fails
                    success = self._create_comment(pr_number, comment_body)
            else:
                success = self._create_comment(pr_number, comment_body)

            return success

        except Exception as e:
            self.logger.error(f"Failed to post performance comment to PR #{pr_number}: {e}")
            raise AutomationError(f"PR comment automation failed: {e}") from e

    def _find_existing_performance_comment(self, pr_number: int) -> Optional[int]:
        """Find existing performance comment to update instead of creating new one"""
        try:
            url = f"{self.config.api_base}/repos/{self.config.repo}/issues/{pr_number}/comments"

            for attempt in range(self.config.retry_attempts):
                try:
                    response = self.session.get(url)
                    response.raise_for_status()

                    comments = response.json()

                    # Look for existing performance report comment
                    for comment in comments:
                        body = comment.get('body', '')
                        if '🚀 Performance Report' in body and 'copybook-rs-benchmark-automation' in comment.get('user', {}).get('login', ''):
                            return comment['id']

                    return None

                except requests.RequestException as e:
                    self.logger.warning(f"Attempt {attempt + 1} failed to fetch comments: {e}")
                    if attempt == self.config.retry_attempts - 1:
                        raise

            return None

        except Exception as e:
            self.logger.warning(f"Failed to find existing comment: {e}")
            return None

    def _create_comment(self, pr_number: int, comment_body: str) -> bool:
        """Create new PR comment"""
        try:
            url = f"{self.config.api_base}/repos/{self.config.repo}/issues/{pr_number}/comments"

            for attempt in range(self.config.retry_attempts):
                try:
                    response = self.session.post(url, json={"body": comment_body})
                    response.raise_for_status()

                    comment_data = response.json()
                    self.logger.info(f"Created performance comment {comment_data['id']} on PR #{pr_number}")
                    return True

                except requests.RequestException as e:
                    self.logger.warning(f"Attempt {attempt + 1} failed to create comment: {e}")
                    if attempt == self.config.retry_attempts - 1:
                        raise GitHubApiError(f"Failed to create PR comment after {self.config.retry_attempts} attempts: {e}")

            return False

        except Exception as e:
            self.logger.error(f"Failed to create PR comment: {e}")
            return False

    def _update_comment(self, comment_id: int, comment_body: str) -> bool:
        """Update existing PR comment"""
        try:
            url = f"{self.config.api_base}/repos/{self.config.repo}/issues/comments/{comment_id}"

            for attempt in range(self.config.retry_attempts):
                try:
                    response = self.session.patch(url, json={"body": comment_body})
                    response.raise_for_status()
                    return True

                except requests.RequestException as e:
                    self.logger.warning(f"Attempt {attempt + 1} failed to update comment: {e}")
                    if attempt == self.config.retry_attempts - 1:
                        raise GitHubApiError(f"Failed to update PR comment after {self.config.retry_attempts} attempts: {e}")

            return False

        except Exception as e:
            self.logger.error(f"Failed to update PR comment {comment_id}: {e}")
            return False

    def _generate_performance_comment(self, perf_data: Dict[str, Any]) -> str:
        """Generate comprehensive performance summary comment"""
        try:
            display_gibs = perf_data.get('display_gibs', 0.0)
            comp3_mibs = perf_data.get('comp3_mibs', 0.0)
            errors = perf_data.get('errors', [])
            warnings = perf_data.get('warnings', [])
            metadata = perf_data.get('_metadata', {})

            # Determine status with enterprise context
            if errors:
                status = "❌ FAILED"
                status_detail = "Performance below enterprise floors"
            elif warnings:
                status = "⚠️ WARNING"
                status_detail = "Performance approaching thresholds"
            else:
                status = "✅ PASSED"
                status_detail = "All enterprise SLOs met"

            # Calculate safety margins (vs 80 MB/s and 40 MB/s floors)
            display_mbps = display_gibs * 1073.74
            display_margin = display_mbps / 80.0
            comp3_margin = comp3_mibs / 40.0

            # Get execution context
            execution_context = metadata.get('execution_context', {})
            execution_time = execution_context.get('execution_time_seconds', 0)
            git_commit = metadata.get('git_commit', 'unknown')[:8]

            comment = f"""## 🚀 Performance Report - copybook-rs

**{status}** | DISPLAY: **{display_gibs:.2f} GiB/s** ({display_margin:.1f}x safety) | COMP-3: **{comp3_mibs:.0f} MiB/s** ({comp3_margin:.1f}x safety)

<details>
<summary>📊 Performance Metrics</summary>

### Throughput Analysis
- **DISPLAY Processing**: {display_gibs:.2f} GiB/s ({display_mbps:.0f} MB/s)
  - Enterprise Floor: 80 MB/s
  - Safety Margin: **{display_margin:.1f}x** ({display_mbps - 80:.0f} MB/s above floor)

- **COMP-3 Processing**: {comp3_mibs:.0f} MiB/s
  - Enterprise Floor: 40 MB/s
  - Safety Margin: **{comp3_margin:.1f}x** ({comp3_mibs - 40:.0f} MiB/s above floor)

### Enterprise Readiness
- **Status**: {status_detail}
- **Compliance Level**: {self._determine_compliance_badge(display_margin, comp3_margin, errors, warnings)}
- **Production Ready**: {'✅ Yes' if not errors else '❌ No - Performance floors not met'}

### Execution Context
- **Benchmark Duration**: {execution_time:.1f}s
- **Git Commit**: `{git_commit}`
- **Timestamp**: {metadata.get('timestamp', 'unknown')}
"""

            if warnings:
                comment += f"\n### ⚠️ Performance Warnings\n"
                for i, warning in enumerate(warnings[:5], 1):  # Limit to 5 warnings
                    comment += f"{i}. {warning}\n"
                if len(warnings) > 5:
                    comment += f"... and {len(warnings) - 5} more warnings\n"

            if errors:
                comment += f"\n### ❌ Performance Errors  \n"
                for i, error in enumerate(errors[:5], 1):  # Limit to 5 errors
                    comment += f"{i}. {error}\n"
                if len(errors) > 5:
                    comment += f"... and {len(errors) - 5} more errors\n"

            comment += f"""
</details>

---
*Generated by copybook-rs benchmark automation • [View detailed logs](https://github.com/{self.config.repo}/actions)*
"""

            return comment

        except Exception as e:
            self.logger.error(f"Failed to generate performance comment: {e}")
            # Fallback simple comment
            return f"""## 🚀 Performance Report - copybook-rs

❌ **ERROR**: Failed to generate performance report - {e}

DISPLAY: {perf_data.get('display_gibs', 0.0):.2f} GiB/s | COMP-3: {perf_data.get('comp3_mibs', 0.0):.0f} MiB/s

*Generated by copybook-rs benchmark automation*
"""

    def _determine_compliance_badge(
        self,
        display_margin: float,
        comp3_margin: float,
        errors: List[str],
        warnings: List[str]
    ) -> str:
        """Generate compliance level badge"""
        if errors:
            return "🔴 Non-Compliant"

        min_margin = min(display_margin, comp3_margin)

        if min_margin >= 10.0 and not warnings:
            return "🟢 Fully Compliant"
        elif min_margin >= 5.0:
            return "🟡 Conditionally Compliant"
        elif warnings:
            return "🟠 Requires Attention"
        else:
            return "🟡 Conditionally Compliant"

    def update_baseline_on_merge(self, pr_number: int, perf_data: Dict[str, Any]) -> bool:
        """Update performance baseline when PR merges to main"""
        try:
            self.logger.info(f"Processing baseline update for merged PR #{pr_number}")

            # Validate PR is actually merged
            if not self._verify_pr_merged(pr_number):
                self.logger.warning(f"PR #{pr_number} is not merged, skipping baseline update")
                return False

            # Import baseline manager for promotion logic
            from .baseline_manager import BaselineManager

            baseline_manager = BaselineManager()
            success = baseline_manager.promote_baseline_on_merge(pr_number, perf_data)

            if success:
                # Post follow-up comment about baseline promotion
                promotion_comment = f"""## 🎯 Baseline Promoted

Performance baseline has been updated following the merge of PR #{pr_number}.

**New Baseline Metrics:**
- DISPLAY: {perf_data.get('display_gibs', 0.0):.2f} GiB/s
- COMP-3: {perf_data.get('comp3_mibs', 0.0):.0f} MiB/s

This baseline will be used for future performance regression detection.

*Automated baseline promotion by copybook-rs benchmark system*
"""
                self._create_comment(pr_number, promotion_comment)

            return success

        except Exception as e:
            self.logger.error(f"Failed to update baseline for PR #{pr_number}: {e}")
            return False

    def _verify_pr_merged(self, pr_number: int) -> bool:
        """Verify that PR is actually merged"""
        try:
            url = f"{self.config.api_base}/repos/{self.config.repo}/pulls/{pr_number}"
            response = self.session.get(url)
            response.raise_for_status()

            pr_data = response.json()
            return pr_data.get('merged', False)

        except Exception as e:
            self.logger.warning(f"Failed to verify PR merge status: {e}")
            return False

    def test_github_authentication(self) -> bool:
        """Test GitHub API authentication and permissions"""
        try:
            url = f"{self.config.api_base}/repos/{self.config.repo}"
            response = self.session.get(url)
            response.raise_for_status()

            repo_data = response.json()
            self.logger.info(f"Successfully authenticated with GitHub API")
            self.logger.info(f"Repository: {repo_data.get('full_name')}")
            self.logger.info(f"Permissions: {repo_data.get('permissions', {})}")

            return True

        except requests.RequestException as e:
            self.logger.error(f"GitHub API authentication failed: {e}")
            return False
        except Exception as e:
            self.logger.error(f"Unexpected error testing GitHub authentication: {e}")
            return False

def create_github_config_from_env() -> GitHubConfig:
    """Create GitHub configuration from environment variables"""
    token = os.getenv('GITHUB_TOKEN')
    if not token:
        raise AutomationError("GITHUB_TOKEN environment variable is required")

    repo = os.getenv('GITHUB_REPOSITORY')
    if not repo:
        raise AutomationError("GITHUB_REPOSITORY environment variable is required")

    return GitHubConfig(token=token, repo=repo)
```

This comprehensive Python utilities architecture provides enterprise-grade benchmark automation capabilities while maintaining clean abstractions, comprehensive error handling, and seamless integration with the existing copybook-bench infrastructure.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
