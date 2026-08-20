# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for cargo-audit advisory database cache recovery."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

from prepare_advisory_db import remove_if_unusable


class PrepareAdvisoryDbTests(unittest.TestCase):
    def test_workflow_uses_versioned_cache_and_refresh_gate(self) -> None:
        workflow = (Path(__file__).parents[2] / ".github/workflows/security-scan.yml").read_text(
            encoding="utf-8"
        )
        self.assertIn("shared-key: advisory-db-v2", workflow)
        self.assertIn("cache-directories: ~/.cargo/advisory-db", workflow)
        self.assertIn("python3 scripts/ci/prepare_advisory_db.py", workflow)
        self.assertIn("cargo audit fetch --force", workflow)
        self.assertIn("if: always() && steps.advisory_db.outcome == 'success'", workflow)
        self.assertLess(
            workflow.index("cargo audit fetch --force"),
            workflow.index("name: Run cargo audit"),
        )

    def test_absent_database_is_left_absent(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "advisory-db"
            self.assertFalse(remove_if_unusable(path))
            self.assertFalse(path.exists())

    def test_non_git_directory_is_removed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "advisory-db"
            path.mkdir()
            (path / "partial-pack").write_text("incomplete", encoding="utf-8")

            self.assertTrue(remove_if_unusable(path))
            self.assertFalse(path.exists())

    def test_valid_committed_checkout_is_reused(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "advisory-db"
            path.mkdir()
            subprocess.run(["git", "-C", str(path), "init"], check=True, capture_output=True)
            subprocess.run(
                [
                    "git",
                    "-C",
                    str(path),
                    "-c",
                    "user.name=copybook-test",
                    "-c",
                    "user.email=copybook-test@example.invalid",
                    "commit",
                    "--allow-empty",
                    "-m",
                    "seed",
                ],
                check=True,
                capture_output=True,
            )

            self.assertFalse(remove_if_unusable(path))
            self.assertTrue((path / ".git").exists())


if __name__ == "__main__":
    unittest.main()
