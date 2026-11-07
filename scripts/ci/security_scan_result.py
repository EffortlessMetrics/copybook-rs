# SPDX-License-Identifier: AGPL-3.0-or-later
"""Classify independent main-CI scanner evidence and fail closed."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

from classify_security_audit import load_audit_report

# cargo-deny --format json emits one JSON object per line on stderr with a
# "type" discriminator. "diagnostic" carries fields.severity ("error" marks a
# finding); "log" carries fields.level ("ERROR" marks a tool failure);
# per-check "summary" records only repeat counts and carry no verdict, so they
# are informational. Any other record shape is a tool error (fail closed).
DENY_INFORMATIONAL_TYPES = frozenset({"summary"})


def classify_deny(path: Path, status: str) -> str:
    """Read cargo-deny's JSON diagnostic stream, including tool-error logs."""
    if not status:
        return "not_run"
    try:
        records = [json.loads(line) for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]
        errors = False
        for record in records:
            fields = record["fields"]
            kind = record["type"]
            if kind == "log":
                if fields.get("level", "").upper() == "ERROR":
                    return "tool_error"
            elif kind == "diagnostic":
                errors = errors or fields.get("severity") == "error"
            elif kind in DENY_INFORMATIONAL_TYPES:
                continue
            else:
                return "tool_error"
        if status == "0" and not errors:
            return "clean"
        if status == "1" and errors:
            return "findings"
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        pass
    return "tool_error"


def classify_audit(path: Path, status: str) -> str:
    if not status:
        return "not_run"
    try:
        _, findings = load_audit_report(path)
        if status == "0" and not findings:
            return "clean"
        if status == "1" and findings:
            return "findings"
    except (OSError, ValueError):
        pass
    return "tool_error"


def evidence(root: Path, scanner: str, status: str) -> dict:
    files = {}
    for suffix in ("json", "stdout" if scanner == "deny" else "stderr", "version"):
        path = root / f"security.{scanner}.{suffix}"
        files[path.name] = hashlib.sha256(path.read_bytes()).hexdigest()
    version = (root / f"security.{scanner}.version").read_text(encoding="utf-8").strip()
    if not version:
        raise ValueError(f"missing {scanner} version")
    classifier = classify_deny if scanner == "deny" else classify_audit
    return {"state": classifier(root / f"security.{scanner}.json", status),
            "exit_status": status, "version": version, "sha256": files}


def enforce(summary: dict, upload: str) -> bool:
    return (upload == "success" and bool(summary.get("commit"))
            and all(summary.get(scanner, {}).get("state") == "clean"
                    for scanner in ("deny", "audit")))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("classify", "enforce"))
    parser.add_argument("--root", type=Path, default=Path("target/security"))
    parser.add_argument("--commit", default="")
    parser.add_argument("--deny-status", default="")
    parser.add_argument("--audit-status", default="")
    parser.add_argument("--upload", default="")
    args = parser.parse_args()
    summary_path = args.root / "summary.json"
    try:
        if args.mode == "classify":
            summary = {"commit": args.commit}
            for scanner, status in (("deny", args.deny_status), ("audit", args.audit_status)):
                try:
                    summary[scanner] = evidence(args.root, scanner, status)
                except (OSError, ValueError) as error:
                    summary[scanner] = {"state": "tool_error" if status else "not_run",
                                        "exit_status": status, "error": str(error)}
            args.root.mkdir(parents=True, exist_ok=True)
            summary_path.write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
            print(json.dumps(summary))
            return 0
        summary = json.loads(summary_path.read_text(encoding="utf-8"))
        if enforce(summary, args.upload):
            print("Both scanners clean; evidence uploaded")
            return 0
        print(f"Security gate failed: upload={args.upload}, summary={summary}")
    except (OSError, ValueError, TypeError, AttributeError) as error:
        print(f"Security evidence error: {error}")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
