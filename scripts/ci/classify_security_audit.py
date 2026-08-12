# SPDX-License-Identifier: AGPL-3.0-or-later
"""Validate raw cargo-audit JSON and emit the weekly issue decision."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import TextIO


def classify(path: Path) -> tuple[str, int]:
    """Return the issue decision and finding count for cargo-audit JSON."""
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError as error:
        raise ValueError(f"cargo-audit output is missing: {path}") from error
    except json.JSONDecodeError as error:
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
    return ("create_or_update" if count else "no_action", count)


def write_github_output(output: TextIO, decision: str, count: int) -> None:
    """Write stable step outputs consumed by the workflow."""
    output.write(f"decision={decision}\n")
    output.write(f"finding_count={count}\n")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("audit_json", type=Path)
    parser.add_argument("--github-output", type=Path)
    args = parser.parse_args()

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
