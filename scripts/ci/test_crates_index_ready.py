#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for exact-version index readiness (scripts/ci/crates_index_ready.sh).

All boundaries are injected: a fixture sparse-index tree served over file://,
a sequencing curl shim, and a counting sleep shim. No network, no waiting.
"""

from __future__ import annotations

import json
import os
import shlex
import shutil
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
HELPER = ROOT / "scripts" / "ci" / "crates_index_ready.sh"


def entry(*versions: str) -> str:
    # Compact separators, matching real sparse-index files.
    return "".join(
        json.dumps({"name": "demo", "vers": v, "cksum": "0" * 64}, separators=(",", ":"))
        + "\n"
        for v in versions
    )


def run_helper(
    func: str,
    args: list[str],
    index_dir: Path,
    curl_shim: Path | None = None,
    sleep_shim: Path | None = None,
) -> subprocess.CompletedProcess[str]:
    env = dict(os.environ)
    env["CRATES_INDEX_BASE_URL"] = f"file://{index_dir}"
    env["CURL_BIN"] = str(curl_shim) if curl_shim else "curl"
    if sleep_shim:
        env["SLEEP_BIN"] = str(sleep_shim)
    script = f'source "{HELPER}"; set +e; {func} ' + " ".join(shlex.quote(a) for a in args)
    return subprocess.run(
        ["bash", "-c", script],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def write_index(index_dir: Path, package: str, body: str) -> None:
    # Sparse-index layout for names of length >= 4: ab/cd/name.
    target = index_dir / package[:2] / package[2:4] / package
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(body)


def make_sleep_counter(tmp: Path) -> tuple[Path, Path]:
    count = tmp / "sleeps.txt"
    count.write_text("")
    shim = tmp / "sleep-shim.sh"
    shim.write_text(f"#!/bin/sh\necho x >> {count}\n")
    shim.chmod(shim.stat().st_mode | stat.S_IEXEC)
    return shim, count


def make_curl_sequence(tmp: Path, bodies: list[str]) -> Path:
    """A curl shim emitting staged response bodies, one per invocation."""
    state = tmp / "curl-state.txt"
    state.write_text("0")
    shim = tmp / "curl-shim.sh"
    shim.write_text(
        "#!/bin/sh\n"
        f"n=$(cat {state})\n"
        f"echo $((n + 1)) > {state}\n"
        "i=0\n"
        + "".join(f'[ "$n" -eq {k} ] && printf %s {b!r} && exit 0\n' for k, b in enumerate(bodies))
        + "exit 22\n"
    )
    shim.chmod(shim.stat().st_mode | stat.S_IEXEC)
    return shim


@unittest.skipUnless(shutil.which("bash"), "bash is required")
@unittest.skipUnless(shutil.which("curl"), "curl is required")
class IndexReadinessTests(unittest.TestCase):
    def test_ready_version_reports_ready(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            idx = Path(tmp) / "index"
            write_index(idx, "demo", entry("0.8.0", "0.8.1"))
            proc = run_helper("probe_package_version", ["demo", "0.8.1"], idx)
        self.assertEqual(proc.returncode, 0, proc.stderr)

    def test_absent_version_is_not_ready(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            idx = Path(tmp) / "index"
            write_index(idx, "demo", entry("0.8.0"))
            proc = run_helper("probe_package_version", ["demo", "0.8.1"], idx)
        self.assertEqual(proc.returncode, 1)

    def test_version_prefix_is_not_readiness(self) -> None:
        # 0.8.1 must not match a 0.8.10 entry.
        with tempfile.TemporaryDirectory() as tmp:
            idx = Path(tmp) / "index"
            write_index(idx, "demo", entry("0.8.10"))
            proc = run_helper("probe_package_version", ["demo", "0.8.1"], idx)
        self.assertEqual(proc.returncode, 1)

    def test_malformed_entry_is_not_ready(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            idx = Path(tmp) / "index"
            write_index(idx, "demo", "not json at all\n")
            proc = run_helper("probe_package_version", ["demo", "0.8.1"], idx)
        self.assertEqual(proc.returncode, 1)

    def test_unreachable_index_is_tool_failure(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            idx = Path(tmp) / "missing"
            proc = run_helper("probe_package_version", ["demo", "0.8.1"], idx)
        self.assertEqual(proc.returncode, 2)

    def test_empty_package_is_tool_failure(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            idx = Path(tmp) / "index"
            proc = run_helper("probe_package_version", ["", "0.8.1"], idx)
        self.assertEqual(proc.returncode, 2)

    def test_wait_succeeds_without_sleep_when_ready(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            t = Path(tmp)
            idx = t / "index"
            write_index(idx, "demo", entry("0.8.1"))
            sleep_shim, count = make_sleep_counter(t)
            proc = run_helper("wait_for_version", ["demo", "0.8.1", "3", "10"], idx,
                              sleep_shim=sleep_shim)
            self.assertEqual(proc.returncode, 0, proc.stderr)
            self.assertEqual(count.read_text(), "")

    def test_wait_retries_until_ready_then_stops(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            t = Path(tmp)
            idx = t / "index"
            curl_shim = make_curl_sequence(t, [entry("0.8.0"), entry("0.8.0"), entry("0.8.1")])
            sleep_shim, count = make_sleep_counter(t)
            proc = run_helper("wait_for_version", ["demo", "0.8.1", "5", "10"], idx,
                              curl_shim=curl_shim, sleep_shim=sleep_shim)
            self.assertEqual(proc.returncode, 0, proc.stderr)
            self.assertEqual(count.read_text(), "x\nx\n")

    def test_wait_bound_expires_with_explicit_state(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            t = Path(tmp)
            idx = t / "index"
            write_index(idx, "demo", entry("0.8.0"))
            sleep_shim, count = make_sleep_counter(t)
            proc = run_helper("wait_for_version", ["demo", "0.8.1", "3", "10"], idx,
                              sleep_shim=sleep_shim)
            self.assertEqual(proc.returncode, 1)
            self.assertEqual(count.read_text(), "x\nx\n")
            self.assertIn("demo@0.8.1", proc.stderr)

    def test_wait_stops_immediately_on_tool_failure(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            t = Path(tmp)
            idx = t / "missing"
            sleep_shim, count = make_sleep_counter(t)
            proc = run_helper("wait_for_version", ["demo", "0.8.1", "5", "10"], idx,
                              sleep_shim=sleep_shim)
            self.assertEqual(proc.returncode, 1)
            self.assertEqual(count.read_text(), "")


if __name__ == "__main__":
    unittest.main()
