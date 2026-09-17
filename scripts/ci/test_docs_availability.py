#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for bounded docs.rs availability checks (scripts/ci/docs_availability.py).

The network is injected: every test supplies a stub fetch callable, so no
test touches docs.rs. Live behavior was measured once against 0.8.1 to
ground the redirect discriminator (see test names).
"""

from __future__ import annotations

import ast
import json
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

from docs_availability import (  # noqa: E402
    EXIT_FAILED,
    EXIT_INCOMPLETE,
    EXIT_READY,
    check_plan,
    classify_docs,
    main,
)

BASE = "https://docs.rs"


def lib_workspace(tmp: Path, packages: list[str], bin_only: frozenset = frozenset()) -> Path:
    for package in packages:
        src = tmp / "crates" / package / "src"
        src.mkdir(parents=True)
        if package not in bin_only:
            (src / "lib.rs").write_text("// lib\n")
    return tmp


def plan(*packages: str, version: str = "0.8.1") -> list[dict]:
    return [{"package": p, "version": version} for p in packages]


def fetch_of(mapping: dict[str, tuple]) -> callable:
    def fetch(url: str, timeout: float):
        assert timeout > 0
        return mapping[url]
    return fetch


class RedirectDiscriminatorTest(unittest.TestCase):
    """Measured 0.8.1 shapes: library docs redirect into the versioned tree,
    binary-only crates redirect back to the crate page."""

    def test_built_library_docs(self):
        kind, _ = classify_docs("copybook", "0.8.1",
                                "https://docs.rs/copybook/0.8.1/copybook/")
        self.assertEqual(kind, "built")

    def test_binary_only_redirects_to_crate_page(self):
        kind, _ = classify_docs("copybook-cli", "0.8.1",
                                "https://docs.rs/crate/copybook-cli/0.8.1")
        self.assertEqual(kind, "absent")

    def test_sideways_version_redirect_never_passes(self):
        kind, detail = classify_docs("copybook", "0.8.1",
                                     "https://docs.rs/copybook/latest/copybook/")
        self.assertEqual(kind, "absent")
        self.assertIn("0.8.1", detail)

    def test_missing_final_url_is_unknown(self):
        kind, _ = classify_docs("copybook", "0.8.1", None)
        self.assertEqual(kind, "unknown")


class OverallTest(unittest.TestCase):
    def test_all_built_is_ready(self):
        with tempfile.TemporaryDirectory() as tmp:
            workspace = lib_workspace(Path(tmp), ["demo"])
            fetch = fetch_of({
                f"{BASE}/crate/demo/0.8.1": (200, f"{BASE}/crate/demo/0.8.1"),
                f"{BASE}/demo/0.8.1": (200, f"{BASE}/demo/0.8.1/demo/"),
            })
            result = check_plan(plan("demo"), fetch, workspace, 5.0, 0, BASE)
            self.assertEqual(result["overall"], "ready")
            self.assertEqual(result["crates"][0]["status"], "docs-built")

    def test_binary_only_without_docs_is_not_applicable(self):
        with tempfile.TemporaryDirectory() as tmp:
            workspace = lib_workspace(Path(tmp), ["tool"], bin_only=frozenset({"tool"}))
            fetch = fetch_of({
                f"{BASE}/crate/tool/0.8.1": (200, f"{BASE}/crate/tool/0.8.1"),
                f"{BASE}/tool/0.8.1": (200, f"{BASE}/crate/tool/0.8.1"),
            })
            result = check_plan(plan("tool"), fetch, workspace, 5.0, 0, BASE)
            self.assertEqual(result["overall"], "ready")
            self.assertEqual(result["crates"][0]["status"], "not-applicable")

    def test_library_without_docs_is_pending_not_failed(self):
        with tempfile.TemporaryDirectory() as tmp:
            workspace = lib_workspace(Path(tmp), ["demo"])
            fetch = fetch_of({
                f"{BASE}/crate/demo/0.8.1": (200, f"{BASE}/crate/demo/0.8.1"),
                f"{BASE}/demo/0.8.1": (404, None),
            })
            result = check_plan(plan("demo"), fetch, workspace, 5.0, 0, BASE)
            self.assertEqual(result["overall"], "incomplete")
            self.assertEqual(result["crates"][0]["status"], "pending")

    def test_missing_crate_page_blocks(self):
        with tempfile.TemporaryDirectory() as tmp:
            workspace = lib_workspace(Path(tmp), ["demo"])
            fetch = fetch_of({f"{BASE}/crate/demo/0.8.1": (404, None)})
            result = check_plan(plan("demo"), fetch, workspace, 5.0, 2, BASE)
            self.assertEqual(result["overall"], "failed")
            self.assertEqual(result["crates"][0]["status"], "crate-missing")

    def test_timeout_is_resumable_incomplete(self):
        with tempfile.TemporaryDirectory() as tmp:
            workspace = lib_workspace(Path(tmp), ["demo"])
            fetch = fetch_of({
                f"{BASE}/crate/demo/0.8.1": (200, f"{BASE}/crate/demo/0.8.1"),
                f"{BASE}/demo/0.8.1": (None, None),
            })
            result = check_plan(plan("demo"), fetch, workspace, 5.0, 0, BASE)
            self.assertEqual(result["overall"], "incomplete")
            self.assertEqual(result["crates"][0]["status"], "unknown")

    def test_failed_beats_incomplete(self):
        with tempfile.TemporaryDirectory() as tmp:
            workspace = lib_workspace(Path(tmp), ["gone", "slow"])
            fetch = fetch_of({
                f"{BASE}/crate/gone/0.8.1": (404, None),
                f"{BASE}/crate/slow/0.8.1": (200, f"{BASE}/crate/slow/0.8.1"),
                f"{BASE}/slow/0.8.1": (None, None),
            })
            result = check_plan(plan("gone", "slow"), fetch, workspace, 5.0, 0, BASE)
            self.assertEqual(result["overall"], "failed")


class CliTest(unittest.TestCase):
    def test_cli_exit_codes(self):
        with tempfile.TemporaryDirectory() as tmp:
            workspace = lib_workspace(Path(tmp), ["demo"])
            plan_path = Path(tmp) / "plan.json"
            out_path = Path(tmp) / "result.json"
            plan_path.write_text(json.dumps(plan("demo")))
            # Real fetch would hit the network; point at an unroutable base
            # with zero timeout behavior instead: use a docs URL that 404s
            # fast is network-dependent, so only exercise input validation.
            self.assertEqual(
                main(["--plan", str(Path(tmp) / "absent.json"),
                      "--workspace", str(workspace)]), EXIT_FAILED)
            plan_path.write_text("not json")
            self.assertEqual(
                main(["--plan", str(plan_path), "--workspace", str(workspace),
                      "--out", str(out_path)]), EXIT_FAILED)


class ReadOnlyTest(unittest.TestCase):
    def test_helper_mutates_nothing_but_its_out_file(self):
        """The helper only reads plan/workspace/network and writes --out:
        no subprocess, no shell, no publication primitives."""
        source = (ROOT / "scripts" / "ci" / "docs_availability.py").read_text()
        tree = ast.parse(source)
        imports = set()
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                imports.update(a.name.split(".")[0] for a in node.names)
            elif isinstance(node, ast.ImportFrom) and node.module:
                imports.add(node.module.split(".")[0])
        self.assertLessEqual(
            imports, {"__future__", "argparse", "datetime", "json", "sys",
                      "urllib", "pathlib"})
        for forbidden in ("subprocess", "socket", "os.system", "shutil",
                          "cargo ", "gh release", "crates.io"):
            self.assertNotIn(forbidden, source)


if __name__ == "__main__":
    unittest.main()
