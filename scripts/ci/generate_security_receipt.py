# SPDX-License-Identifier: AGPL-3.0-or-later
"""Generate and validate deterministic normalized cargo-audit evidence v2."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import tempfile
from pathlib import Path
from typing import Any
from urllib.parse import urlparse

from classify_security_audit import load_audit_report

SCHEMA_VERSION = "2.0"
SCAN_TYPES = frozenset({"pr-gate", "weekly-scan", "manual"})
STATES = frozenset({"clean", "findings", "tool_error"})
SEVERITIES = ("critical", "high", "medium", "low", "unknown")
SEVERITY_SET = frozenset(SEVERITIES)
SHA256_PATTERN = re.compile(r"^[0-9a-f]{64}$")
COMMIT_PATTERN = re.compile(r"^[0-9a-f]{40}$")
ADVISORY_PATTERN = re.compile(r"^RUSTSEC-\d{4}-\d{4}$")


def _require_object(value: object, label: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise ValueError(f"{label} must be an object")
    return value


def _require_string(value: object, label: str) -> str:
    if not isinstance(value, str) or not value:
        raise ValueError(f"{label} must be a non-empty string")
    return value


def _require_integer(value: object, label: str) -> int:
    if not isinstance(value, int) or isinstance(value, bool) or not 0 <= value <= 255:
        raise ValueError(f"{label} must be an integer between 0 and 255")
    return value


def _require_exact_keys(
    value: dict[str, Any], required: set[str], optional: set[str], label: str
) -> None:
    missing = sorted(required - value.keys())
    unknown = sorted(value.keys() - required - optional)
    if missing:
        raise ValueError(f"{label} is missing required fields: {', '.join(missing)}")
    if unknown:
        raise ValueError(f"{label} contains unknown fields: {', '.join(unknown)}")


def _normalize_finding(raw: object, index: int) -> dict[str, Any]:
    finding = _require_object(raw, f"cargo-audit finding {index}")
    advisory = _require_object(
        finding.get("advisory"), f"cargo-audit finding {index}.advisory"
    )
    package = _require_object(
        finding.get("package"), f"cargo-audit finding {index}.package"
    )
    versions_value = finding.get("versions", {})
    versions = _require_object(versions_value, f"cargo-audit finding {index}.versions")

    advisory_id = _require_string(
        advisory.get("id"), f"cargo-audit finding {index}.advisory.id"
    )
    if ADVISORY_PATTERN.fullmatch(advisory_id) is None:
        raise ValueError(f"cargo-audit finding {index}.advisory.id is invalid")

    severity_value = advisory.get("severity")
    if severity_value is None:
        severity = "unknown"
    elif isinstance(severity_value, str) and severity_value in SEVERITY_SET:
        severity = severity_value
    else:
        raise ValueError(
            f"cargo-audit finding {index}.advisory.severity is unsupported"
        )

    normalized: dict[str, Any] = {
        "advisory_id": advisory_id,
        "package": {
            "name": _require_string(
                package.get("name"), f"cargo-audit finding {index}.package.name"
            ),
            "version": _require_string(
                package.get("version"),
                f"cargo-audit finding {index}.package.version",
            ),
        },
        "severity": severity,
    }

    for key in ("title", "url"):
        if key in advisory:
            normalized[key] = _require_string(
                advisory[key], f"cargo-audit finding {index}.advisory.{key}"
            )

    patched = versions.get("patched", [])
    if not isinstance(patched, list) or not all(
        isinstance(version, str) and version for version in patched
    ):
        raise ValueError(
            f"cargo-audit finding {index}.versions.patched must be an array of non-empty strings"
        )
    normalized["patched_versions"] = patched
    return normalized


def _canonical_digest(value: object) -> str:
    canonical = json.dumps(value, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(canonical).hexdigest()


def _receipt_identity(identity: dict[str, Any], scanner: dict[str, Any]) -> str:
    material = {
        "schema_version": SCHEMA_VERSION,
        "identity": identity,
        "scanner": scanner,
    }
    return f"sha256:{_canonical_digest(material)}"


def generate_receipt(
    audit_path: Path,
    *,
    commit_sha: str,
    scan_type: str,
    workflow_run_id: str,
    cargo_audit_version: str,
    audit_exit_code: int,
) -> dict[str, Any]:
    """Generate a deterministic normalized receipt from explicit scan identity."""
    if COMMIT_PATTERN.fullmatch(commit_sha) is None:
        raise ValueError(
            "commit_sha must be exactly 40 lowercase hexadecimal characters"
        )
    if not isinstance(scan_type, str) or scan_type not in SCAN_TYPES:
        raise ValueError(f"scan_type must be one of: {', '.join(sorted(SCAN_TYPES))}")
    _require_string(workflow_run_id, "workflow_run_id")
    _require_string(cargo_audit_version, "cargo_audit_version")
    _require_integer(audit_exit_code, "audit_exit_code")

    raw_bytes = audit_path.read_bytes()
    _document, raw_findings = load_audit_report(audit_path)
    findings = [
        _normalize_finding(raw_finding, index)
        for index, raw_finding in enumerate(raw_findings)
    ]

    by_severity = {severity: 0 for severity in SEVERITIES}
    for finding in findings:
        by_severity[finding["severity"]] += 1

    if findings:
        state = "findings"
    elif audit_exit_code == 0:
        state = "clean"
    else:
        state = "tool_error"

    identity = {
        "commit_sha": commit_sha,
        "scan_type": scan_type,
        "workflow_run_id": workflow_run_id,
        "raw_audit_sha256": hashlib.sha256(raw_bytes).hexdigest(),
    }
    scanner = {
        "name": "cargo-audit",
        "version": cargo_audit_version,
        "exit_code": audit_exit_code,
    }
    receipt = {
        "schema_version": SCHEMA_VERSION,
        "receipt_id": _receipt_identity(identity, scanner),
        "identity": identity,
        "scanner": scanner,
        "outcome": {
            "state": state,
            "finding_count": len(findings),
            "by_severity": by_severity,
        },
        "findings": findings,
    }
    validate_receipt(receipt)
    return receipt


def validate_receipt(receipt: object) -> None:
    """Fail closed when a normalized v2 receipt violates its closed contract."""
    root = _require_object(receipt, "receipt")
    _require_exact_keys(
        root,
        {"schema_version", "receipt_id", "identity", "scanner", "outcome", "findings"},
        set(),
        "receipt",
    )
    if root["schema_version"] != SCHEMA_VERSION:
        raise ValueError(f"unsupported schema_version: {root['schema_version']!r}")

    identity = _require_object(root["identity"], "receipt.identity")
    _require_exact_keys(
        identity,
        {"commit_sha", "scan_type", "workflow_run_id", "raw_audit_sha256"},
        set(),
        "receipt.identity",
    )
    commit_sha = _require_string(identity["commit_sha"], "receipt.identity.commit_sha")
    if COMMIT_PATTERN.fullmatch(commit_sha) is None:
        raise ValueError("receipt.identity.commit_sha is invalid")
    if (
        not isinstance(identity["scan_type"], str)
        or identity["scan_type"] not in SCAN_TYPES
    ):
        raise ValueError("receipt.identity.scan_type is unsupported")
    _require_string(identity["workflow_run_id"], "receipt.identity.workflow_run_id")
    raw_digest = _require_string(
        identity["raw_audit_sha256"], "receipt.identity.raw_audit_sha256"
    )
    if SHA256_PATTERN.fullmatch(raw_digest) is None:
        raise ValueError("receipt.identity.raw_audit_sha256 is invalid")

    scanner = _require_object(root["scanner"], "receipt.scanner")
    _require_exact_keys(
        scanner, {"name", "version", "exit_code"}, set(), "receipt.scanner"
    )
    if scanner["name"] != "cargo-audit":
        raise ValueError("receipt.scanner.name must be cargo-audit")
    _require_string(scanner["version"], "receipt.scanner.version")
    exit_code = _require_integer(scanner["exit_code"], "receipt.scanner.exit_code")

    receipt_id = _require_string(root["receipt_id"], "receipt.receipt_id")
    if receipt_id != _receipt_identity(identity, scanner):
        raise ValueError("receipt.receipt_id does not match its identity inputs")

    findings_value = root["findings"]
    if not isinstance(findings_value, list):
        raise ValueError("receipt.findings must be an array")
    findings = findings_value
    computed_severity = {severity: 0 for severity in SEVERITIES}
    for index, raw_finding in enumerate(findings):
        finding = _require_object(raw_finding, f"receipt.findings[{index}]")
        _require_exact_keys(
            finding,
            {"advisory_id", "package", "severity", "patched_versions"},
            {"title", "url"},
            f"receipt.findings[{index}]",
        )
        advisory_id = _require_string(
            finding["advisory_id"], f"receipt.findings[{index}].advisory_id"
        )
        if ADVISORY_PATTERN.fullmatch(advisory_id) is None:
            raise ValueError(f"receipt.findings[{index}].advisory_id is invalid")
        package = _require_object(
            finding["package"], f"receipt.findings[{index}].package"
        )
        _require_exact_keys(
            package,
            {"name", "version"},
            set(),
            f"receipt.findings[{index}].package",
        )
        _require_string(package["name"], f"receipt.findings[{index}].package.name")
        _require_string(
            package["version"], f"receipt.findings[{index}].package.version"
        )
        severity = finding["severity"]
        if not isinstance(severity, str) or severity not in SEVERITY_SET:
            raise ValueError(f"receipt.findings[{index}].severity is unsupported")
        computed_severity[severity] += 1
        patched = finding["patched_versions"]
        if not isinstance(patched, list) or not all(
            isinstance(version, str) and version for version in patched
        ):
            raise ValueError(
                f"receipt.findings[{index}].patched_versions must be an array of non-empty strings"
            )
        for key in ("title", "url"):
            if key in finding:
                value = _require_string(
                    finding[key], f"receipt.findings[{index}].{key}"
                )
                if key == "url":
                    parsed = urlparse(value)
                    if parsed.scheme not in {"http", "https"} or not parsed.netloc:
                        raise ValueError(f"receipt.findings[{index}].url is invalid")

    outcome = _require_object(root["outcome"], "receipt.outcome")
    _require_exact_keys(
        outcome,
        {"state", "finding_count", "by_severity"},
        set(),
        "receipt.outcome",
    )
    state = outcome["state"]
    if not isinstance(state, str) or state not in STATES:
        raise ValueError("receipt.outcome.state is unsupported")
    finding_count = outcome["finding_count"]
    if not isinstance(finding_count, int) or isinstance(finding_count, bool):
        raise ValueError("receipt.outcome.finding_count must be an integer")
    if finding_count != len(findings):
        raise ValueError(
            "receipt.outcome.finding_count does not match receipt.findings"
        )
    by_severity = _require_object(outcome["by_severity"], "receipt.outcome.by_severity")
    _require_exact_keys(
        by_severity, set(SEVERITIES), set(), "receipt.outcome.by_severity"
    )
    for severity in SEVERITIES:
        value = by_severity[severity]
        if not isinstance(value, int) or isinstance(value, bool) or value < 0:
            raise ValueError(
                f"receipt.outcome.by_severity.{severity} must be a non-negative integer"
            )
    if by_severity != computed_severity:
        raise ValueError("receipt.outcome.by_severity does not match receipt.findings")

    expected_state = (
        "findings" if findings else ("clean" if exit_code == 0 else "tool_error")
    )
    if state != expected_state:
        raise ValueError(
            "receipt.outcome.state is inconsistent with findings and exit_code"
        )


def _write_atomic(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        dir=path.parent, prefix=f".{path.name}.", suffix=".tmp"
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8", newline="\n") as output:
            output.write(content)
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, path)
    except Exception:
        temporary.unlink(missing_ok=True)
        raise


def _read_receipt(path: Path) -> object:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError as error:
        raise ValueError(f"normalized receipt is missing: {path}") from error
    except json.JSONDecodeError as error:
        raise ValueError(f"normalized receipt is not valid JSON: {error}") from error


def main() -> int:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)

    generate = subparsers.add_parser("generate")
    generate.add_argument("audit_json", type=Path)
    generate.add_argument("--commit-sha", required=True)
    generate.add_argument("--scan-type", required=True)
    generate.add_argument("--workflow-run-id", required=True)
    generate.add_argument("--cargo-audit-version", required=True)
    generate.add_argument("--audit-exit-code", required=True, type=int)
    generate.add_argument("--output", type=Path)

    validate = subparsers.add_parser("validate")
    validate.add_argument("receipt_json", type=Path)
    args = parser.parse_args()

    try:
        if args.command == "validate":
            validate_receipt(_read_receipt(args.receipt_json))
            print("valid normalized security receipt v2")
            return 0

        receipt = generate_receipt(
            args.audit_json,
            commit_sha=args.commit_sha,
            scan_type=args.scan_type,
            workflow_run_id=args.workflow_run_id,
            cargo_audit_version=args.cargo_audit_version,
            audit_exit_code=args.audit_exit_code,
        )
        content = json.dumps(receipt, indent=2, sort_keys=True) + "\n"
        if args.output is None:
            print(content, end="")
        else:
            _write_atomic(args.output, content)
    except (OSError, ValueError) as error:
        parser.exit(1, f"error: {error}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
