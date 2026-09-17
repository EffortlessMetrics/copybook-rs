#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for head-bound PR CI assessment (scripts/ci/pr_head_status.py).

All GitHub API payloads are inline fixtures. No network, no subprocess,
no gh invocation: the helper under test is a pure function over captured
documents plus a thin CLI wrapper.
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

from pr_head_status import POLICY, assess, main  # noqa: E402

HEAD = "a" * 40
OTHER_HEAD = "b" * 40

# One passing sample per expected policy pattern. If POLICY gains an
# expected entry, `test_ready_requires_full_expected_set` fails until a
# sample is added here: the expected set is a deliberate list, not a
# green percentage.
EXPECTED_SAMPLES = [
    "Rustfmt",
    "Clippy",
    "Test Suite (ubuntu-latest, stable)",
    "Build Examples (ubuntu-latest, stable)",
    "Security Checks",
    "Determinism Smoke",
    "Governance + BDD Smoke",
    "RDW iterator tests",
    "Exit code mapping (ubuntu-latest)",
    "Code Coverage",
    "Documentation",
    "Strict Comments Mode",
    "Validate PR Title",
    "API Freeze Check",
    "Determinism smoke (codec + CLI)",
    "validate-receipt",
    "check-governance",
    "Property Tests - Core (ubuntu-latest, stable)",
    "Property Tests - Codec (ubuntu-latest, stable)",
    "Property Tests - Integration (stable)",
    "Property Test Summary",
    "insights",
]


def run(name, status="completed", conclusion="success", head=HEAD,
        started="2026-09-17T05:00:00Z", completed="2026-09-17T05:01:00Z"):
    row = {"name": name, "status": status, "head_sha": head,
           "started_at": started, "html_url": f"https://example.test/{name}"}
    if conclusion is not None:
        row["conclusion"] = conclusion
    if completed is not None:
        row["completed_at"] = completed
    return row


def pr_doc(head=HEAD):
    return {"number": 1, "headRefOid": head,
            "mergeStateStatus": "UNSTABLE", "mergeable": "MERGEABLE"}


class HeadBindingTest(unittest.TestCase):
    def test_head_changed_during_observation_is_unknown(self):
        result = assess(pr_doc(head=OTHER_HEAD), [run("Rustfmt")], [], HEAD)
        self.assertFalse(result["binding_ok"])
        self.assertEqual(result["verdict"], "unknown")

    def test_row_from_another_sha_is_unknown_not_green(self):
        rows = [run("Rustfmt"), run("Clippy", head=OTHER_HEAD)]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertFalse(result["binding_ok"])
        self.assertEqual(result["verdict"], "unknown")
        self.assertTrue(result["binding_violations"])

    def test_empty_evidence_is_unknown(self):
        result = assess(pr_doc(), [], [], HEAD)
        self.assertEqual(result["verdict"], "unknown")


class AttemptSelectionTest(unittest.TestCase):
    def base(self):
        return [run(n) for n in EXPECTED_SAMPLES if n != "Clippy"]

    def test_newer_failure_replaces_older_success(self):
        rows = self.base() + [
            run("Clippy", conclusion="success",
                started="2026-09-17T05:00:00Z", completed="2026-09-17T05:01:00Z"),
            run("Clippy", conclusion="failure",
                started="2026-09-17T06:00:00Z", completed="2026-09-17T06:01:00Z"),
        ]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "blocked")
        self.assertIn("Clippy", result["required_fail"])

    def test_rerun_success_records_superseded_failure(self):
        rows = self.base() + [
            run("Clippy", conclusion="failure",
                started="2026-09-17T05:00:00Z", completed="2026-09-17T05:01:00Z"),
            run("Clippy", conclusion="success",
                started="2026-09-17T06:00:00Z", completed="2026-09-17T06:01:00Z"),
        ]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "ready")
        clippy = next(c for c in result["checks"] if c["name"] == "Clippy")
        self.assertEqual(clippy["attempts"], 2)
        self.assertIn("failure", clippy["superseded"])

    def test_rerun_in_flight_is_pending_not_green(self):
        rows = self.base() + [
            run("Clippy", conclusion="success",
                started="2026-09-17T05:00:00Z", completed="2026-09-17T05:01:00Z"),
            run("Clippy", status="in_progress", conclusion=None,
                started="2026-09-17T06:00:00Z", completed=None),
        ]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "incomplete")
        self.assertIn("Clippy", result["required_pending"])

    def test_duplicate_names_resolve_deterministically(self):
        rows = self.base() + [run("testExpected"), run("testExpected")]
        result = assess(pr_doc(), rows, [], HEAD)
        selected = [c for c in result["checks"] if c["name"] == "testExpected"]
        self.assertEqual(len(selected), 1)
        self.assertEqual(selected[0]["attempts"], 2)


class StateSeparationTest(unittest.TestCase):
    def test_fail_cancel_skip_advisory_stay_distinct(self):
        rows = (
            [run(n) for n in EXPECTED_SAMPLES
             if n not in ("Test Suite (ubuntu-latest, stable)",
                          "Build Examples (ubuntu-latest, stable)",
                          "Code Coverage")]
            + [run("Test Suite (ubuntu-latest, stable)", conclusion="failure")]
            + [run("Build Examples (ubuntu-latest, stable)", conclusion="cancelled")]
            + [run("Code Coverage", conclusion="skipped")]
            + [run("Result Docs Advisory", conclusion="failure")]
            + [run("RIPR test-oracle pilot (advisory)", conclusion="failure")]
        )
        result = assess(pr_doc(), rows, [], HEAD)
        # The failed root job blocks; canceled siblings are reported as
        # canceled, not as failures and not as passes.
        self.assertEqual(result["verdict"], "blocked")
        self.assertIn("Test Suite (ubuntu-latest, stable)", result["required_fail"])
        self.assertIn("Build Examples (ubuntu-latest, stable)", result["required_canceled"])
        self.assertNotIn("Build Examples (ubuntu-latest, stable)", result["required_fail"])
        # Skipped and advisory failures never block.
        self.assertNotIn("Code Coverage",
                         result["required_fail"] + result["required_canceled"]
                         + result["required_pending"])
        self.assertIn("Result Docs Advisory", result["advisory_fail"])
        self.assertIn("RIPR test-oracle pilot (advisory)", result["advisory_fail"])


class PolicyClosureTest(unittest.TestCase):
    def test_ready_requires_full_expected_set(self):
        import re
        expected = [(p, lane) for p, lane, presence, _s in POLICY
                    if lane == "required" and presence == "expected"]
        self.assertTrue(expected)
        for pattern, _lane in expected:
            self.assertTrue(
                any(re.fullmatch(pattern, sample) for sample in EXPECTED_SAMPLES),
                f"expected pattern without a passing sample: {pattern}")

    def test_all_green_expected_set_is_ready(self):
        rows = [run(n) for n in EXPECTED_SAMPLES]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "ready")
        self.assertEqual(result["missing_expected"], [])
        self.assertEqual(result["unmapped"], [])

    def test_unmapped_check_blocks_ready(self):
        rows = [run(n) for n in EXPECTED_SAMPLES] + [run("Shiny New Gate")]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "incomplete")
        self.assertIn("Shiny New Gate", result["unmapped"])

    def test_skipped_unmapped_check_does_not_block(self):
        rows = [run(n) for n in EXPECTED_SAMPLES] + [
            run("Shiny New Gate", conclusion="skipped")]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "ready")

    def test_conditional_perf_absent_stays_ready(self):
        # perf.yml runs on PRs only under its trigger path filter; a head
        # that does not touch those paths has no `perf` row and stays ready.
        rows = [run(n) for n in EXPECTED_SAMPLES]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "ready")
        self.assertEqual(result["unmapped"], [])

    def test_conditional_perf_present_and_green_stays_ready(self):
        rows = [run(n) for n in EXPECTED_SAMPLES] + [run("perf")]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "ready")
        perf = next(c for c in result["checks"] if c["name"] == "perf")
        self.assertEqual((perf["lane"], perf["presence"]), ("required", "conditional"))

    def test_conditional_perf_failure_blocks(self):
        rows = [run(n) for n in EXPECTED_SAMPLES] + [run("perf", conclusion="failure")]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "blocked")
        self.assertIn("perf", result["required_fail"])

    def test_missing_expected_check_is_incomplete(self):
        rows = [run(n) for n in EXPECTED_SAMPLES if n != "Clippy"]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["verdict"], "incomplete")
        self.assertTrue(
            any("Clippy" in pattern for pattern in result["missing_expected"]))

    def test_merge_ref_recorded_never_conflated(self):
        rows = [run(n) for n in EXPECTED_SAMPLES]
        result = assess(pr_doc(), rows, [], HEAD)
        self.assertEqual(result["merge_ref"]["mergeStateStatus"], "UNSTABLE")
        self.assertEqual(result["verdict"], "ready")

    def test_external_status_rows_fold_in(self):
        rows = [run(n) for n in EXPECTED_SAMPLES]
        statuses = [{"context": "codecov/project", "state": "failure",
                     "target_url": "https://example.test/cov"}]
        result = assess(pr_doc(), rows, statuses, HEAD)
        # Advisory external failure is recorded without blocking.
        self.assertEqual(result["verdict"], "ready")
        self.assertIn("status:codecov/project",
                      [c["name"] for c in result["checks"]])


class PaginationTest(unittest.TestCase):
    def test_page_objects_flatten(self):
        from pr_head_status import main as helper_main
        with tempfile.TemporaryDirectory() as tmp:
            pr_path = Path(tmp) / "pr.json"
            runs_path = Path(tmp) / "runs.json"
            pr_path.write_text(json.dumps(pr_doc()))
            pages = [
                {"total_count": 2, "check_runs": [run(n) for n in EXPECTED_SAMPLES[:11]]},
                {"total_count": 2, "check_runs": [run(n) for n in EXPECTED_SAMPLES[11:]]},
            ]
            runs_path.write_text(json.dumps(pages))
            self.assertEqual(
                helper_main(["--pr", str(pr_path), "--check-runs", str(runs_path),
                             "--format", "json"]), 0)


class ReadOnlyTest(unittest.TestCase):
    def test_helper_performs_no_io_or_network(self):
        """The helper must stay a pure stdin/file -> stdout reporter: no
        subprocess, socket, HTTP client, or write-mode file access, so the
        documented procedure cannot merge, rerun, resolve, tag, or publish."""
        source = (ROOT / "scripts" / "ci" / "pr_head_status.py").read_text()
        tree = ast.parse(source)
        imports = set()
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                imports.update(a.name.split(".")[0] for a in node.names)
            elif isinstance(node, ast.ImportFrom) and node.module:
                imports.add(node.module.split(".")[0])
        self.assertLessEqual(
            imports, {"__future__", "argparse", "json", "re", "sys", "dataclasses"})
        for forbidden in ("subprocess", "socket", "urllib", "http", "os.",
                          "os.system", "os.popen", "shutil", ", \"w\"", ", 'w'"):
            self.assertNotIn(forbidden, source)


class CliTest(unittest.TestCase):
    def test_cli_exit_codes(self):
        with tempfile.TemporaryDirectory() as tmp:
            pr_path = Path(tmp) / "pr.json"
            runs_path = Path(tmp) / "runs.json"
            pr_path.write_text(json.dumps(pr_doc()))
            # Incomplete: expected set absent.
            runs_path.write_text(json.dumps([run("Rustfmt")]))
            self.assertEqual(
                main(["--pr", str(pr_path), "--check-runs", str(runs_path)]), 1)
            # Ready.
            runs_path.write_text(
                json.dumps([run(n) for n in EXPECTED_SAMPLES]))
            self.assertEqual(
                main(["--pr", str(pr_path), "--check-runs", str(runs_path),
                      "--format", "json"]), 0)
            # Unknown: malformed input.
            runs_path.write_text("not json")
            self.assertEqual(
                main(["--pr", str(pr_path), "--check-runs", str(runs_path)]), 2)


if __name__ == "__main__":
    unittest.main()
