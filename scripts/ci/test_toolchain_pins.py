#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for the nightly-toolchain-pin gate."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci" / "check_toolchain_pins.py"


def run_checker(workflows: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT), str(workflows)],
        capture_output=True,
        text=True,
        check=False,
    )


class ToolchainPinTests(unittest.TestCase):
    def test_repo_workflows_pass(self) -> None:
        proc = subprocess.run(
            [sys.executable, str(SCRIPT)],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def test_implicit_nightly_step_fails(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "night.yml"
            path.write_text(
                "jobs:\n"
                "  fuzz:\n"
                "    steps:\n"
                "      - name: Install Rust toolchain\n"
                "        uses: dtolnay/rust-toolchain@nightly\n"
            )
            proc = run_checker(Path(tmp))
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn("night.yml", proc.stderr)

    def test_explicit_nightly_step_passes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "night.yml"
            path.write_text(
                "jobs:\n"
                "  fuzz:\n"
                "    steps:\n"
                "      - uses: dtolnay/rust-toolchain@nightly\n"
                "        with:\n"
                "          toolchain: nightly\n"
            )
            proc = run_checker(Path(tmp))
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def test_file_env_opt_out_passes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "night.yml"
            path.write_text(
                "env:\n"
                "  RUSTUP_TOOLCHAIN: nightly\n"
                "jobs:\n"
                "  fuzz:\n"
                "    steps:\n"
                "      - uses: dtolnay/rust-toolchain@nightly\n"
            )
            proc = run_checker(Path(tmp))
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def test_stable_step_without_marker_passes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "quick.yml"
            path.write_text(
                "jobs:\n"
                "  test:\n"
                "    steps:\n"
                "      - uses: dtolnay/rust-toolchain@stable\n"
                "        with:\n"
                "          components: rustfmt, clippy\n"
            )
            proc = run_checker(Path(tmp))
        self.assertEqual(proc.returncode, 0, proc.stderr)


if __name__ == "__main__":
    unittest.main()
