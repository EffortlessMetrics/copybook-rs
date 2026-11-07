#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Unit tests for scripts/ci/check_rustdoc_targets.py (no network, no cargo)."""

from __future__ import annotations

import subprocess
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from check_rustdoc_targets import collisions, documented_targets


def pkg(name: str, targets: list[dict]) -> dict:
    return {"name": name, "targets": targets}


def lib(name: str, doc: bool = True) -> dict:
    return {"name": name, "kind": ["lib"], "doc": doc}


def binary(name: str, doc: bool = True) -> dict:
    return {"name": name, "kind": ["bin"], "doc": doc}


class DocumentedTargetsTests(unittest.TestCase):
    def test_lib_bin_collision_detected(self) -> None:
        meta = {
            "packages": [
                pkg("copybook", [lib("copybook")]),
                pkg("copybook-cli", [binary("copybook")]),
            ]
        }
        dupes = collisions(documented_targets(meta))
        self.assertIn("copybook", dupes)
        self.assertEqual(
            dupes["copybook"],
            [("copybook", "lib"), ("copybook-cli", "bin")],
        )

    def test_doc_false_bin_excluded(self) -> None:
        meta = {
            "packages": [
                pkg("copybook", [lib("copybook")]),
                pkg("copybook-cli", [binary("copybook", doc=False)]),
            ]
        }
        self.assertEqual(collisions(documented_targets(meta)), {})

    def test_distinct_names_pass(self) -> None:
        meta = {
            "packages": [
                pkg("copybook", [lib("copybook")]),
                pkg("copybook-cli", [binary("copybook-cli-tool")]),
            ]
        }
        self.assertEqual(collisions(documented_targets(meta)), {})

    def test_undocumented_targets_ignored(self) -> None:
        meta = {
            "packages": [
                pkg("a", [{"name": "x", "kind": ["test"], "doc": False}]),
                pkg("b", [{"name": "x", "kind": ["test"], "doc": False}]),
            ]
        }
        self.assertEqual(documented_targets(meta), [])
        self.assertEqual(collisions(documented_targets(meta)), {})

    def test_two_libs_same_name_flagged(self) -> None:
        meta = {
            "packages": [
                pkg("left", [lib("shared")]),
                pkg("right", [lib("shared")]),
            ]
        }
        dupes = collisions(documented_targets(meta))
        self.assertIn("shared", dupes)


class WorkspaceInventoryTests(unittest.TestCase):
    def test_live_workspace_has_no_collisions(self) -> None:
        workspace_root = Path(__file__).resolve().parents[2]
        proc = subprocess.run(
            [sys.executable, str(workspace_root / "scripts" / "ci" / "check_rustdoc_targets.py")],
            capture_output=True,
            text=True,
            check=False,
            cwd=workspace_root,
        )
        self.assertEqual(proc.returncode, 0, proc.stdout + proc.stderr)
        self.assertIn("no duplicate rustdoc output identities", proc.stdout)


if __name__ == "__main__":
    unittest.main()
