#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for the panic-policy baseline gate (check_no_new_test_panic.sh).

Regression context: on push events CI checks out with fetch-depth 1 and the
gate falls back to BASE_SHA=HEAD~1, which does not exist in a depth-1 clone.
The gate must fail loudly in that state (never silently pass), and pass once
the parent commit is available. These tests pin that contract using fixture
git repositories.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "check_no_new_test_panic.sh"

GIT_ENV = {
    "GIT_AUTHOR_NAME": "ci-test",
    "GIT_AUTHOR_EMAIL": "ci-test@example.com",
    "GIT_COMMITTER_NAME": "ci-test",
    "GIT_COMMITTER_EMAIL": "ci-test@example.com",
}


def git(repo: Path, *args: str) -> None:
    env = dict(os.environ)
    env.update(GIT_ENV)
    subprocess.run(
        ["git", *args],
        cwd=repo,
        env=env,
        capture_output=True,
        text=True,
        check=True,
    )


def run_gate(repo: Path, base_sha: str, head_sha: str = "HEAD") -> subprocess.CompletedProcess[str]:
    env = dict(os.environ)
    env["BASE_SHA"] = base_sha
    env["HEAD_SHA"] = head_sha
    return subprocess.run(
        ["bash", str(SCRIPT)],
        cwd=repo,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def init_repo(path: Path, files: dict[str, str]) -> Path:
    git(path, "init", "-q")
    for name, content in files.items():
        target = path / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(content)
    git(path, "add", "-A")
    git(path, "commit", "-qm", "initial")
    return path


@unittest.skipUnless(shutil.which("git"), "git is required")
@unittest.skipUnless(shutil.which("bash"), "bash is required")
class PanicPolicyBaselineTests(unittest.TestCase):
    def test_missing_baseline_fails_loudly(self) -> None:
        """A depth-1 clone (no HEAD~1) must error, never silently pass."""
        with tempfile.TemporaryDirectory() as tmp:
            repo = init_repo(Path(tmp), {"src/lib.rs": "pub fn f() {}\n"})
            proc = run_gate(repo, "HEAD~1")
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn("cannot resolve panic-policy baseline", proc.stderr)

    def test_clean_diff_passes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            repo = init_repo(Path(tmp), {"src/lib.rs": "pub fn f() {}\n"})
            (repo / "src/lib.rs").write_text("pub fn f() -> u32 { 1 }\n")
            git(repo, "add", "-A")
            git(repo, "commit", "-qm", "second")
            proc = run_gate(repo, "HEAD~1")
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def test_new_panic_macro_fails_with_location(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            repo = init_repo(Path(tmp), {"src/lib.rs": "pub fn f() {}\n"})
            (repo / "src/lib.rs").write_text("pub fn f() { panic!(\"boom\"); }\n")
            git(repo, "add", "-A")
            git(repo, "commit", "-qm", "second")
            proc = run_gate(repo, "HEAD~1")
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn("src/lib.rs", proc.stderr)


if __name__ == "__main__":
    unittest.main()
