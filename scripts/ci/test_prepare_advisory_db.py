# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for cargo-audit advisory database cache recovery."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

try:
    from prepare_advisory_db import remove_if_unusable
except ModuleNotFoundError:
    from scripts.ci.prepare_advisory_db import remove_if_unusable


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

    def test_symlink_is_removed_without_touching_target(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            target = root / "outside"
            target.mkdir()
            (target / "sentinel").write_text("keep", encoding="utf-8")
            link = root / "advisory-db"
            try:
                os.symlink(target, link, target_is_directory=True)
            except (OSError, NotImplementedError) as error:
                self.skipTest(f"symlink fixtures unavailable: {error}")

            self.assertTrue(remove_if_unusable(link))
            self.assertFalse(link.exists())
            self.assertEqual((target / "sentinel").read_text(encoding="utf-8"), "keep")

    def test_symlink_to_valid_checkout_is_removed_without_touching_target(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            target = root / "outside"
            target.mkdir()
            subprocess.run(["git", "-C", str(target), "init"], check=True, capture_output=True)
            subprocess.run(
                [
                    "git",
                    "-C",
                    str(target),
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
            link = root / "advisory-db"
            try:
                os.symlink(target, link, target_is_directory=True)
            except (OSError, NotImplementedError) as error:
                self.skipTest(f"symlink fixtures unavailable: {error}")

            self.assertTrue(remove_if_unusable(link))
            self.assertFalse(link.exists())
            self.assertTrue((target / ".git").exists())

    def test_cli_has_no_arbitrary_path_override(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            result = subprocess.run(
                [sys.executable, str(Path(__file__).with_name("prepare_advisory_db.py")), "--path", temporary],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertNotEqual(result.returncode, 0)

    def test_cli_only_uses_cargo_home_advisory_db(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            cargo_home = Path(temporary) / "cargo"
            unrelated = Path(temporary) / "unrelated" / "advisory-db"
            unrelated.mkdir(parents=True)
            (unrelated / "sentinel").write_text("keep", encoding="utf-8")
            result = subprocess.run(
                [sys.executable, str(Path(__file__).with_name("prepare_advisory_db.py"))],
                check=False,
                capture_output=True,
                text=True,
                env={**os.environ, "CARGO_HOME": str(cargo_home)},
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue((unrelated / "sentinel").exists())

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
