#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Satellite lockfile freshness gate.

Runs the exact Standalone MSRV commands for every satellite manifest
inventoried by .github/workflows/msrv-standalone.yml. A satellite lockfile
that would change under `--locked` fails this check locally, before the long
hosted run. The workflow matrix is the inventory authority: adding a manifest
there automatically extends this check. This script never regenerates or
commits lockfiles.
"""

from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "msrv-standalone.yml"
MANIFEST_RE = re.compile(r"^\s*(?:-\s*)?manifest:\s*(\S+)\s*$")


def standalone_manifests(workflow: Path = WORKFLOW) -> list[str]:
    """Manifest paths inventoried by the Standalone MSRV workflow matrix."""
    manifests: list[str] = []
    for line in workflow.read_text().splitlines():
        match = MANIFEST_RE.match(line)
        if match and match.group(1) not in manifests:
            manifests.append(match.group(1))
    return manifests


def lock_is_fresh(manifest: str, root: Path = ROOT) -> tuple[bool, str]:
    """Run the exact workflow command; True plus output when the lock holds."""
    proc = subprocess.run(
        ["cargo", "check", "--locked", "--manifest-path", manifest],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )
    output = (proc.stdout + proc.stderr).strip()
    tail = "\n".join(output.splitlines()[-5:])
    return proc.returncode == 0, tail


def main() -> int:
    try:
        manifests = standalone_manifests()
    except OSError as error:
        print(f"satellite-locks: cannot read workflow inventory: {error}", file=sys.stderr)
        return 1
    if not manifests:
        print(
            "satellite-locks: no manifests inventoried by msrv-standalone.yml",
            file=sys.stderr,
        )
        return 1
    failures = 0
    for manifest in manifests:
        fresh, tail = lock_is_fresh(manifest)
        if fresh:
            print(f"satellite-locks: fresh {manifest}")
            continue
        failures += 1
        print(f"satellite-locks: STALE {manifest}", file=sys.stderr)
        print(tail, file=sys.stderr)
        print(
            f"satellite-locks: refresh with `cargo check --manifest-path {manifest}` "
            "and commit only the required resolution.",
            file=sys.stderr,
        )
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
