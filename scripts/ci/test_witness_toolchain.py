#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for the toolchain-selection witness (scripts/ci/witness_toolchain.sh).

The repository pins 1.98 in rust-toolchain.toml, which takes precedence over
a rustup default. The witness must fail closed when the effective toolchain
comes from that file and pass when RUSTUP_TOOLCHAIN governs the selection.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci" / "witness_toolchain.sh"


def run_witness(extra_env: dict[str, str] | None = None) -> subprocess.CompletedProcess[str]:
    env = dict(os.environ)
    env.pop("RUSTUP_TOOLCHAIN", None)
    if extra_env:
        env.update(extra_env)
    return subprocess.run(
        ["bash", str(SCRIPT), "test-selector"],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


@unittest.skipUnless(shutil.which("bash"), "bash is required")
@unittest.skipUnless(shutil.which("rustup"), "rustup is required")
class WitnessToolchainTests(unittest.TestCase):
    def test_file_pin_fails_closed(self) -> None:
        proc = run_witness()
        self.assertNotEqual(proc.returncode, 0)
        combined = proc.stdout + proc.stderr
        self.assertIn("rust-toolchain.toml", combined)

    def test_env_selection_passes(self) -> None:
        proc = run_witness({"RUSTUP_TOOLCHAIN": "stable"})
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertIn("environment variable RUSTUP_TOOLCHAIN", proc.stdout)


if __name__ == "__main__":
    unittest.main()
