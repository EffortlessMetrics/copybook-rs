# SPDX-License-Identifier: AGPL-3.0-or-later
"""Validate raw cargo-audit JSON and emit the weekly issue decision."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any, TextIO


def parse_audit_report(raw_json: bytes) -> tuple[dict[str, Any], list[Any]]:
    """Parse cargo-audit JSON bytes and return the validated finding list."""
    try:
        document = json.loads(raw_json)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"cargo-audit output is not valid JSON: {error}") from error

    if not isinstance(document, dict):
        raise ValueError("cargo-audit output must be a JSON object")
    vulnerabilities = document.get("vulnerabilities")
    if not isinstance(vulnerabilities, dict):
        raise ValueError("cargo-audit output is missing the vulnerabilities object")
    findings = vulnerabilities.get("list")
    if not isinstance(findings, list):
        raise ValueError("cargo-audit vulnerabilities.list must be an array")

    count = len(findings)
    declared_count = vulnerabilities.get("count")
    if declared_count is not None:
        if not isinstance(declared_count, int) or isinstance(declared_count, bool):
            raise ValueError("cargo-audit vulnerabilities.count must be an integer")
        if declared_count != count:
            raise ValueError(
                "cargo-audit vulnerabilities.count does not match vulnerabilities.list"
            )
    return document, findings


def load_audit_report(path: Path) -> tuple[dict[str, Any], list[Any]]:
    """Load cargo-audit JSON and return the document and validated finding list."""
    try:
        raw_json = path.read_bytes()
    except FileNotFoundError as error:
        raise ValueError(f"cargo-audit output is missing: {path}") from error
    return parse_audit_report(raw_json)


def classify(path: Path) -> tuple[str, int]:
    """Return the issue decision and finding count for cargo-audit JSON."""
    _document, findings = load_audit_report(path)
    count = len(findings)
    return ("create_or_update" if count else "no_action", count)


def write_github_output(output: TextIO, decision: str, count: int) -> None:
    """Write stable step outputs consumed by the workflow."""
    output.write(f"decision={decision}\n")
    output.write(f"finding_count={count}\n")


def evaluate_outcome(
    deny_exit_status: str,
    audit_exit_status: str,
    classification_outcome: str,
    decision: str,
) -> tuple[bool, str]:
    """Return whether the weekly gate may pass and a diagnostic reason."""
    failures = []
    if deny_exit_status != "0":
        failures.append(
            f"cargo-deny failed (exit status: {deny_exit_status or 'missing'})"
        )
    if audit_exit_status != "0":
        failures.append(
            f"cargo-audit failed (exit status: {audit_exit_status or 'missing'})"
        )
    if classification_outcome != "success":
        failures.append("cargo-audit output classification failed")
    elif decision == "create_or_update":
        failures.append("cargo-audit reported vulnerabilities")
    elif decision != "no_action":
        failures.append(f"invalid or missing audit decision: {decision or 'missing'}")
    if failures:
        return False, "; ".join(failures)
    return True, "cargo-deny and cargo-audit completed with no vulnerabilities"


def main() -> int:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)

    classify_parser = subparsers.add_parser("classify")
    classify_parser.add_argument("audit_json", type=Path)
    classify_parser.add_argument("--github-output", type=Path)

    enforce_parser = subparsers.add_parser("enforce")
    enforce_parser.add_argument("--deny-exit-status", required=True)
    enforce_parser.add_argument("--audit-exit-status", required=True)
    enforce_parser.add_argument("--classification-outcome", required=True)
    enforce_parser.add_argument("--decision", required=True)
    args = parser.parse_args()

    if args.command == "enforce":
        passed, reason = evaluate_outcome(
            args.deny_exit_status,
            args.audit_exit_status,
            args.classification_outcome,
            args.decision,
        )
        print(reason)
        return 0 if passed else 1

    try:
        decision, count = classify(args.audit_json)
    except (OSError, ValueError) as error:
        parser.exit(1, f"error: {error}\n")

    if args.github_output is not None:
        with args.github_output.open("a", encoding="utf-8") as output:
            write_github_output(output, decision, count)
    else:
        print(json.dumps({"decision": decision, "finding_count": count}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
