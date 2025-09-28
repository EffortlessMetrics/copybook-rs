#!/usr/bin/env python3
"""
GitHub PR comment automation for copybook-rs benchmark reporting.

This module provides automated GitHub integration for Issue #52 machine-readable
benchmark reporting infrastructure. Generates and posts performance comments
on pull requests with enterprise-grade formatting and regression analysis.

Features:
- Automated PR comment generation with performance metrics
- Enterprise audit compliance reporting integration
- Regression detection and baseline comparison
- GitHub API integration with authentication handling
- Template-based comment formatting with corporate standards
- Dry-run mode for validation and testing

Performance Comment Format:
- DISPLAY parsing throughput with enterprise target comparison
- COMP-3 encoding throughput with baseline analysis
- Warning and error summary with actionable insights
- Regression detection with statistical significance
"""

import argparse
import json
import os
import sys
import requests
from pathlib import Path
from typing import Dict, List, Any, Optional
from datetime import datetime


class PRAutomation:
    """GitHub PR comment automation for performance reporting."""

    def __init__(self, github_token: Optional[str] = None, dry_run: bool = False):
        """Initialize PR automation with GitHub authentication.

        Args:
            github_token: GitHub API token. If None, read from GITHUB_TOKEN env var.
            dry_run: If True, generate comments but don't post to GitHub.
        """
        self.github_token = github_token or os.getenv("GITHUB_TOKEN")
        self.dry_run = dry_run
        self.github_api_base = "https://api.github.com"

        if not self.dry_run and not self.github_token:
            raise ValueError("GitHub token required for live PR automation")

    def load_performance_data(self, perf_file: Path) -> Dict[str, Any]:
        """Load performance data from JSON file.

        Args:
            perf_file: Path to performance JSON file

        Returns:
            Performance data dictionary

        Raises:
            FileNotFoundError: If performance file doesn't exist
            json.JSONDecodeError: If JSON is invalid
        """
        if not perf_file.exists():
            raise FileNotFoundError(f"Performance file not found: {perf_file}")

        with open(perf_file, 'r') as f:
            return json.load(f)

    def generate_performance_comment(self, perf_data: Dict[str, Any],
                                   baseline_data: Optional[Dict[str, Any]] = None) -> str:
        """Generate GitHub PR comment with performance metrics.

        Args:
            perf_data: Current performance data
            baseline_data: Optional baseline for regression comparison

        Returns:
            Formatted PR comment in Markdown
        """
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S UTC")

        # Extract performance metrics
        display_gibs = perf_data.get("display_gibs", 0.0)
        comp3_mibs = perf_data.get("comp3_mibs", 0.0)
        warnings = perf_data.get("warnings", [])
        errors = perf_data.get("errors", [])

        # Enterprise performance targets
        display_target = 4.1  # GiB/s
        comp3_target = 560.0  # MiB/s

        # Generate performance status indicators
        display_status = "✅" if display_gibs >= display_target else "❌"
        comp3_status = "✅" if comp3_mibs >= comp3_target else "❌"

        # Calculate enterprise compliance percentage
        display_compliance = min(100, (display_gibs / display_target) * 100)
        comp3_compliance = min(100, (comp3_mibs / comp3_target) * 100)

        # Start building comment
        comment_parts = [
            "## 🚀 copybook-rs Performance Report",
            "",
            f"**Report Generated:** {timestamp}",
            "",
            "### 📊 Enterprise Performance Metrics",
            "",
            "| Metric | Current | Target | Status | Compliance |",
            "|--------|---------|--------|--------|------------|",
            f"| **DISPLAY Parsing** | {display_gibs:.2f} GiB/s | ≥{display_target} GiB/s | {display_status} | {display_compliance:.1f}% |",
            f"| **COMP-3 Encoding** | {comp3_mibs:.1f} MiB/s | ≥{comp3_target} MiB/s | {comp3_status} | {comp3_compliance:.1f}% |",
            "",
        ]

        # Add baseline comparison if available
        if baseline_data:
            baseline_display = baseline_data.get("display_gibs", 0.0)
            baseline_comp3 = baseline_data.get("comp3_mibs", 0.0)

            if baseline_display > 0:
                display_change = ((display_gibs - baseline_display) / baseline_display) * 100
                display_trend = "📈" if display_change > 0 else "📉" if display_change < -2 else "➡️"
            else:
                display_change = 0.0
                display_trend = "❓"

            if baseline_comp3 > 0:
                comp3_change = ((comp3_mibs - baseline_comp3) / baseline_comp3) * 100
                comp3_trend = "📈" if comp3_change > 0 else "📉" if comp3_change < -2 else "➡️"
            else:
                comp3_change = 0.0
                comp3_trend = "❓"

            comment_parts.extend([
                "### 📈 Baseline Comparison",
                "",
                "| Metric | Change | Trend | Baseline |",
                "|--------|--------|-------|----------|",
                f"| **DISPLAY** | {display_change:+.2f}% | {display_trend} | {baseline_display:.2f} GiB/s |",
                f"| **COMP-3** | {comp3_change:+.2f}% | {comp3_trend} | {baseline_comp3:.1f} MiB/s |",
                "",
            ])

            # Check for significant regressions
            if display_change < -2.0 or comp3_change < -2.0:
                comment_parts.extend([
                    "### ⚠️ Performance Regression Detected",
                    "",
                    "Significant performance decrease detected compared to baseline:",
                    "",
                ])

                if display_change < -2.0:
                    comment_parts.append(f"- **DISPLAY parsing**: {display_change:.2f}% decrease")
                if comp3_change < -2.0:
                    comment_parts.append(f"- **COMP-3 encoding**: {comp3_change:.2f}% decrease")

                comment_parts.extend([
                    "",
                    "Please investigate potential causes and consider performance optimization.",
                    "",
                ])

        # Add warnings section if present
        if warnings:
            comment_parts.extend([
                "### ⚠️ Warnings",
                "",
            ])
            for warning in warnings:
                comment_parts.append(f"- {warning}")
            comment_parts.append("")

        # Add errors section if present
        if errors:
            comment_parts.extend([
                "### ❌ Errors",
                "",
            ])
            for error in errors:
                comment_parts.append(f"- {error}")
            comment_parts.extend([
                "",
                "**Action Required:** Please address the errors above before merging.",
                "",
            ])

        # Add enterprise compliance summary
        overall_compliance = (display_compliance + comp3_compliance) / 2
        compliance_status = "✅ COMPLIANT" if overall_compliance >= 100 else "❌ NON-COMPLIANT"

        comment_parts.extend([
            "### 🏢 Enterprise Compliance Summary",
            "",
            f"**Overall Compliance:** {compliance_status} ({overall_compliance:.1f}%)",
            "",
            "**Performance Targets:**",
            f"- DISPLAY parsing must achieve ≥{display_target} GiB/s for enterprise deployment",
            f"- COMP-3 encoding must achieve ≥{comp3_target} MiB/s for mainframe compatibility",
            "",
        ])

        # Add footer with technical details
        metadata = perf_data.get("metadata", {})
        copybook_version = metadata.get("copybook_rs_version", "unknown")
        env_info = metadata.get("environment", {})
        ci_env = env_info.get("ci_environment", False)

        comment_parts.extend([
            "### 🔧 Technical Details",
            "",
            f"- **copybook-rs version:** {copybook_version}",
            f"- **Environment:** {'CI/CD Pipeline' if ci_env else 'Local Development'}",
            f"- **PERF mode:** {'Enabled' if env_info.get('perf_mode', False) else 'Disabled'}",
            "",
            "---",
            "",
            "*This report was generated automatically by the copybook-rs benchmark infrastructure.*",
            "*For questions about performance analysis, see [benchmark documentation](docs/benchmark-reporting-api-contracts.md).*",
        ])

        return "\n".join(comment_parts)

    def post_pr_comment(self, repo_owner: str, repo_name: str, pr_number: int,
                       comment_body: str) -> bool:
        """Post comment to GitHub PR.

        Args:
            repo_owner: GitHub repository owner
            repo_name: GitHub repository name
            pr_number: Pull request number
            comment_body: Comment content in Markdown

        Returns:
            True if comment posted successfully, False otherwise
        """
        if self.dry_run:
            print("DRY RUN: Would post the following comment:")
            print("=" * 60)
            print(comment_body)
            print("=" * 60)
            return True

        if not self.github_token:
            print("❌ GitHub token required for posting comments", file=sys.stderr)
            return False

        url = f"{self.github_api_base}/repos/{repo_owner}/{repo_name}/issues/{pr_number}/comments"
        headers = {
            "Authorization": f"token {self.github_token}",
            "Accept": "application/vnd.github.v3+json",
            "Content-Type": "application/json"
        }

        payload = {
            "body": comment_body
        }

        try:
            response = requests.post(url, headers=headers, json=payload, timeout=30)
            response.raise_for_status()

            print(f"✅ Comment posted successfully to PR #{pr_number}")
            return True

        except requests.exceptions.RequestException as e:
            print(f"❌ Failed to post comment: {e}", file=sys.stderr)
            return False

    def update_existing_comment(self, repo_owner: str, repo_name: str,
                              comment_id: int, comment_body: str) -> bool:
        """Update existing GitHub PR comment.

        Args:
            repo_owner: GitHub repository owner
            repo_name: GitHub repository name
            comment_id: ID of existing comment to update
            comment_body: New comment content

        Returns:
            True if comment updated successfully, False otherwise
        """
        if self.dry_run:
            print(f"DRY RUN: Would update comment {comment_id}")
            return True

        url = f"{self.github_api_base}/repos/{repo_owner}/{repo_name}/issues/comments/{comment_id}"
        headers = {
            "Authorization": f"token {self.github_token}",
            "Accept": "application/vnd.github.v3+json",
            "Content-Type": "application/json"
        }

        payload = {
            "body": comment_body
        }

        try:
            response = requests.patch(url, headers=headers, json=payload, timeout=30)
            response.raise_for_status()

            print(f"✅ Comment {comment_id} updated successfully")
            return True

        except requests.exceptions.RequestException as e:
            print(f"❌ Failed to update comment: {e}", file=sys.stderr)
            return False


def main():
    """Main entry point for PR automation."""
    parser = argparse.ArgumentParser(
        description="copybook-rs GitHub PR comment automation",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python3 pr_automation.py --pr 123 --perf-file perf.json --dry-run
  python3 pr_automation.py --pr 123 --perf-file perf.json --baseline baseline.json
  python3 pr_automation.py --pr 123 --perf-file perf.json --repo copybook-rs/copybook-rs
        """
    )

    parser.add_argument(
        "--pr",
        type=int,
        required=True,
        help="Pull request number"
    )

    parser.add_argument(
        "--perf-file",
        type=Path,
        required=True,
        help="Performance JSON file path"
    )

    parser.add_argument(
        "--baseline",
        type=Path,
        help="Baseline performance file for comparison"
    )

    parser.add_argument(
        "--repo",
        default="copybook-rs/copybook-rs",
        help="GitHub repository in format owner/repo (default: copybook-rs/copybook-rs)"
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Generate comment without posting to GitHub"
    )

    parser.add_argument(
        "--github-token",
        help="GitHub API token (or set GITHUB_TOKEN env var)"
    )

    parser.add_argument(
        "--update-comment",
        type=int,
        help="Update existing comment ID instead of creating new one"
    )

    args = parser.parse_args()

    try:
        # Parse repository
        if "/" not in args.repo:
            print("❌ Repository must be in format owner/repo", file=sys.stderr)
            sys.exit(1)

        repo_owner, repo_name = args.repo.split("/", 1)

        # Initialize automation
        automation = PRAutomation(
            github_token=args.github_token,
            dry_run=args.dry_run
        )

        # Load performance data
        print(f"Loading performance data from {args.perf_file}...")
        perf_data = automation.load_performance_data(args.perf_file)

        # Load baseline data if provided
        baseline_data = None
        if args.baseline:
            print(f"Loading baseline data from {args.baseline}...")
            baseline_data = automation.load_performance_data(args.baseline)

        # Generate comment
        print("Generating performance comment...")
        comment_body = automation.generate_performance_comment(perf_data, baseline_data)

        # Post or update comment
        if args.update_comment:
            success = automation.update_existing_comment(
                repo_owner, repo_name, args.update_comment, comment_body
            )
        else:
            success = automation.post_pr_comment(
                repo_owner, repo_name, args.pr, comment_body
            )

        if not success:
            sys.exit(1)

        print("✅ PR automation completed successfully")

        # For tests, output key performance metrics
        display_gibs = perf_data.get("display_gibs", 0.0)
        comp3_mibs = perf_data.get("comp3_mibs", 0.0)
        print(f"DISPLAY: {display_gibs:.2f} GiB/s")
        print(f"COMP-3: {comp3_mibs:.1f} MiB/s")

    except Exception as e:
        print(f"❌ PR automation failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()