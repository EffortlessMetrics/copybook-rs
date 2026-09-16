#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for the changie fragment requirement gate (changelog.yml)."""

from __future__ import annotations

import shutil
import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci" / "require-changie-fragment.sh"


class RequireFragmentTests(unittest.TestCase):
    """User-facing changes without a fragment must fail closed."""

    @classmethod
    def setUpClass(cls) -> None:
        cls.bash = shutil.which("bash")
        if cls.bash is None:
            raise unittest.SkipTest("bash is required for fragment gate tests")

    def run_gate(self, userfacing: str, fragment: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [self.bash, str(SCRIPT), userfacing, fragment],
            capture_output=True,
            text=True,
            check=False,
        )

    def test_userfacing_without_fragment_fails(self) -> None:
        proc = self.run_gate("true", "false")
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn(".changes/unreleased/", proc.stderr)

    def test_userfacing_with_fragment_passes(self) -> None:
        proc = self.run_gate("true", "true")
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def test_non_userfacing_without_fragment_passes(self) -> None:
        proc = self.run_gate("false", "false")
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def test_non_userfacing_with_fragment_passes(self) -> None:
        proc = self.run_gate("false", "true")
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def test_invalid_signal_fails_closed(self) -> None:
        for args in (["yes", "false"], ["true", "maybe"], []):
            proc = subprocess.run(
                [self.bash, str(SCRIPT), *args],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertNotEqual(proc.returncode, 0, f"args={args}")


if __name__ == "__main__":
    unittest.main()
