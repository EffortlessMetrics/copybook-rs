# SPDX-License-Identifier: AGPL-3.0-or-later
"""Validate raw cargo-audit JSON and emit the weekly issue decision."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any, NoReturn, TextIO


def _reject_json_constant(value: str) -> NoReturn:
    """Reject Python's NaN/Infinity extensions to the JSON number grammar."""
    raise ValueError(f"cargo-audit output is not valid JSON: {value} is not a JSON number")


def _unique_json_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    """Reject ambiguous objects instead of silently replacing earlier values."""
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"cargo-audit output contains duplicate JSON object key: {key!r}")
        result[key] = value
    return result


def parse_audit_report(raw_json: bytes) -> tuple[dict[str, Any], list[Any]]:
    """Parse cargo-audit JSON bytes and return the validated finding list."""
    try:
        document = json.loads(
            raw_json.decode("utf-8"),
            parse_constant=_reject_json_constant,
            object_pairs_hook=_unique_json_object,
        )
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


def _finding_identity(finding: Any, index: int) -> tuple[str, str, str]:
    """Return the closed identity tuple used by lifecycle fingerprints."""
    if not isinstance(finding, dict):
        raise ValueError(f"finding {index} must be an object")
    advisory = finding.get("advisory")
    package = finding.get("package")
    if not isinstance(advisory, dict) or not isinstance(package, dict):
        raise ValueError(f"finding {index} is missing advisory or package")
    identity = (advisory.get("id"), package.get("name"), package.get("version"))
    if any(not isinstance(value, str) or not value for value in identity):
        raise ValueError(
            f"finding {index} must contain non-empty advisory.id, package.name, and package.version"
        )
    return identity


def findings_fingerprint(findings: list[Any]) -> str:
    """Hash the sorted canonical finding identity tuples."""
    identities = sorted(_finding_identity(finding, index) for index, finding in enumerate(findings))
    canonical = json.dumps(identities, ensure_ascii=False, separators=(",", ":"))
    return f"sha256:{hashlib.sha256(canonical.encode('utf-8')).hexdigest()}"


def classify_lifecycle(path: Path, audit_exit_status: str | None = None) -> dict[str, Any]:
    """Return deterministic lifecycle state, fingerprint, and eligibility."""
    _document, findings = load_audit_report(path)
    count = len(findings)
    status = audit_exit_status
    fingerprint = findings_fingerprint(findings) if count and audit_exit_status is not None else None
    if count:
        # cargo-audit returns 1 for a successfully parsed findings report;
        # every other status is a tool/error or internally inconsistent state.
        eligible = status == "1"
        state = "findings"
    else:
        eligible = status == "0"
        state = "clean"
    return {
        "decision": "create_or_update" if count else "no_action",
        "finding_count": count,
        "findings_fingerprint": fingerprint,
        "state": state,
        "eligible": eligible,
        "audit_exit_status": status,
    }


def classify(path: Path) -> tuple[str, int]:
    """Return the issue decision and finding count for cargo-audit JSON."""
    result = classify_lifecycle(path)
    return (result["decision"], result["finding_count"])


def write_github_output(output: TextIO, result: dict[str, Any]) -> None:
    """Write stable step outputs consumed by the workflow."""
    for key, value in result.items():
        if value is not None:
            output.write(f"{key}={json.dumps(value) if isinstance(value, bool) else value}\n")


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
    classify_parser.add_argument("--audit-exit-status")

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
        result = classify_lifecycle(args.audit_json, args.audit_exit_status)
    except (OSError, ValueError) as error:
        parser.exit(1, f"error: {error}\n")

    if args.github_output is not None:
        with args.github_output.open("a", encoding="utf-8") as output:
            write_github_output(output, result)
    else:
        print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
