#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Fail when two documented workspace targets would write the same rustdoc path.

`cargo doc` emits one `target/doc/<name>/index.html` per documented library or
binary target. When a library and a binary share a name (as the public
`copybook` facade library and the `copybook-cli` binary once did), their
outputs collide and one silently overwrites the other (cargo#6313). The cargo
warning alone does not fail the build, so this guard runs before `cargo doc`
and fails closed on any duplicate documented target name.

Ownership policy: the public facade library owns the `copybook` rustdoc
namespace. Same-named binaries that are not part of the public documentation
contract opt out with `doc = false` on their `[[bin]]` target; CLI
documentation lives in `docs/CLI_REFERENCE.md`, not rustdoc.
"""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

DOCUMENTED_KINDS = ("lib", "bin")


def documented_targets(metadata: dict) -> list[tuple[str, str, str]]:
    """Return (target name, package name, kind) for documented lib/bin targets."""
    entries: list[tuple[str, str, str]] = []
    for package in metadata.get("packages", []):
        for target in package.get("targets", []):
            if not target.get("doc", False):
                continue
            kinds = [k for k in target.get("kind", []) if k in DOCUMENTED_KINDS]
            for kind in kinds:
                entries.append((target["name"], package["name"], kind))
    return entries


def collisions(
    entries: list[tuple[str, str, str]],
) -> dict[str, list[tuple[str, str]]]:
    """Group documented targets by output name; keep names with >1 owner."""
    owners: dict[str, list[tuple[str, str]]] = {}
    for name, package, kind in entries:
        owners.setdefault(name, []).append((package, kind))
    return {
        name: sorted(set(owner_list))
        for name, owner_list in owners.items()
        if len(set(owner_list)) > 1
    }


def workspace_metadata(workspace_root: Path) -> dict:
    proc = subprocess.run(
        [
            "cargo",
            "metadata",
            "--no-deps",
            "--format-version",
            "1",
            "--manifest-path",
            str(workspace_root / "Cargo.toml"),
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        raise RuntimeError(f"cargo metadata failed: {proc.stderr.strip()}")
    return json.loads(proc.stdout)


def main() -> int:
    workspace_root = Path(__file__).resolve().parents[2]
    try:
        metadata = workspace_metadata(workspace_root)
    except (RuntimeError, json.JSONDecodeError) as exc:
        print(f"ERROR: cannot inventory rustdoc targets: {exc}")
        return 1
    entries = documented_targets(metadata)
    print(f"documented lib/bin targets: {len(entries)}")
    dupes = collisions(entries)
    if dupes:
        for name in sorted(dupes):
            owners = ", ".join(f"{pkg} ({kind})" for pkg, kind in dupes[name])
            print(
                f"ERROR: rustdoc output collision at "
                f"target/doc/{name}/index.html owned by {owners}"
            )
        return 1
    print("no duplicate rustdoc output identities")
    return 0


if __name__ == "__main__":
    sys.exit(main())
