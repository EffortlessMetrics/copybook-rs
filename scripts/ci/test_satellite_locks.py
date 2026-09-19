#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for the satellite-lockfile freshness gate."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci" / "check_satellite_locks.py"
sys.path.insert(0, str(SCRIPT.parent))
import check_satellite_locks as checker  # noqa: E402


def write_crate(root: Path, name: str, version: str, dep: str | None) -> Path:
    crate = root / name
    src = crate / "src"
    src.mkdir(parents=True)
    manifest = (
        "[package]\n"
        f'name = "{name}"\n'
        f'version = "{version}"\n'
        'edition = "2021"\n'
    )
    if dep is None:
        (src / "lib.rs").write_text("pub fn f() {}\n")
    else:
        manifest += f'\n[dependencies]\n{dep}\n'
        (src / "main.rs").write_text("fn main() {}\n")
    (crate / "Cargo.toml").write_text(manifest)
    return crate / "Cargo.toml"


class SatelliteLockTests(unittest.TestCase):
    def test_derives_workflow_manifests(self) -> None:
        manifests = checker.standalone_manifests()
        self.assertEqual(
            manifests,
            [
                "fuzz/Cargo.toml",
                "examples/kafka_pipeline/Cargo.toml",
                "examples/kafka_streaming/Cargo.toml",
            ],
        )

    def test_derivation_dedupes_and_ignores_other_keys(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            workflow = Path(tmp) / "msrv.yml"
            workflow.write_text(
                "matrix:\n"
                "  - manifest: a/Cargo.toml\n"
                "    kafka_system_deps: true\n"
                "  - manifest: a/Cargo.toml\n"
                "  - manifest: b/Cargo.toml\n"
            )
            self.assertEqual(
                checker.standalone_manifests(workflow),
                ["a/Cargo.toml", "b/Cargo.toml"],
            )

    def test_fresh_repo_passes(self) -> None:
        fresh, _ = checker.lock_is_fresh("fuzz/Cargo.toml")
        self.assertTrue(fresh)

    def test_stale_fixture_lock_fails(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            write_crate(root, "sat-a", "0.1.0", None)
            consumer = write_crate(root, "sat-b", "0.1.0", 'sat-a = { path = "../sat-a" }')
            subprocess.run(
                ["cargo", "generate-lockfile", "--manifest-path", str(consumer)],
                cwd=root,
                capture_output=True,
                check=True,
            )
            fresh, _ = checker.lock_is_fresh("sat-b/Cargo.toml", root)
            self.assertTrue(fresh)
            # A version bump invalidates the committed lock resolution.
            manifest = root / "sat-a" / "Cargo.toml"
            manifest.write_text(manifest.read_text().replace('0.1.0', '0.2.0'))
            fresh, tail = checker.lock_is_fresh("sat-b/Cargo.toml", root)
            self.assertFalse(fresh)
            self.assertIn("lock", tail.lower())

    def test_repo_gate_passes(self) -> None:
        proc = subprocess.run(
            [sys.executable, str(SCRIPT)],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(proc.returncode, 0, proc.stderr)


if __name__ == "__main__":
    unittest.main()
