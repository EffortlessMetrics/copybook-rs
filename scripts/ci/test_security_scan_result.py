# SPDX-License-Identifier: AGPL-3.0-or-later
"""Regression tests for main-CI independent scanner evidence (#1174).

Covers scripts/ci/security_scan_result.py against schema-faithful deny/audit
fixtures and pins the ci.yml security-job orchestration invariants: both
scanners run independently (if: always()), evidence lands under
target/security/, and a final always-running gate fails closed.
"""

from __future__ import annotations

import json
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

try:
    from security_scan_result import classify_audit, classify_deny, enforce, evidence
except ModuleNotFoundError:
    from scripts.ci.security_scan_result import (  # type: ignore[no-redef]
        classify_audit,
        classify_deny,
        enforce,
        evidence,
    )

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = Path(__file__).with_name("security_scan_result.py")
WORKFLOW = ROOT / ".github" / "workflows" / "ci.yml"
AUDIT_FIXTURES = ROOT / "tests" / "fixtures" / "security-scanning" / "raw-audit"

# Compact deny fixtures replay the exact JSON-lines schema cargo-deny
# --format json emits on stderr (verified against cargo-deny 0.19+ output:
# {"type","fields"} records; diagnostics carry fields.severity; tool failures
# carry {"type": "log", "fields": {"level": "ERROR"}}; per-check "summary"
# records repeat counts and carry no verdict).
DENY_WARNING = json.dumps({
    "fields": {
        "code": "license-not-encountered",
        "graphs": [],
        "labels": [],
        "message": "license was not encountered",
        "severity": "warning",
    },
    "type": "diagnostic",
})
DENY_ERROR = json.dumps({
    "fields": {
        "advisory": {"id": "RUSTSEC-2026-0346"},
        "code": "vulnerability",
        "graphs": [],
        "labels": [],
        "message": "example advisory finding",
        "notes": [],
        "severity": "error",
    },
    "type": "diagnostic",
})
DENY_SUMMARY = json.dumps({
    "fields": {"advisories": {"errors": 1, "helps": 0, "notes": 0, "warnings": 0}},
    "type": "summary",
})
DENY_ERROR_LOG = json.dumps({
    "fields": {"level": "ERROR", "message": "boom", "timestamp": "2026-01-01T00:00:00Z"},
    "type": "log",
})
DENY_INFO_LOG = json.dumps({
    "fields": {"level": "INFO", "message": "working", "timestamp": "2026-01-01T00:00:00Z"},
    "type": "log",
})


def write_deny(directory: Path, *lines: str) -> Path:
    path = directory / "deny.json"
    path.write_text("\n".join(lines) + "\n" if lines else "", encoding="utf-8")
    return path


class ClassifyDenyTests(unittest.TestCase):
    def test_clean_pass_with_warnings_and_summary(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = write_deny(Path(temporary), DENY_WARNING, DENY_SUMMARY)
            self.assertEqual(classify_deny(path, "0"), "clean")

    def test_summary_only_pass_is_clean(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = write_deny(Path(temporary), DENY_SUMMARY)
            self.assertEqual(classify_deny(path, "0"), "clean")

    def test_info_logs_are_ignored(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = write_deny(Path(temporary), DENY_INFO_LOG, DENY_SUMMARY)
            self.assertEqual(classify_deny(path, "0"), "clean")

    def test_error_diagnostic_with_summary_is_findings(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = write_deny(Path(temporary), DENY_WARNING, DENY_ERROR, DENY_SUMMARY)
            self.assertEqual(classify_deny(path, "1"), "findings")

    def test_error_log_is_tool_error_even_with_findings(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = write_deny(Path(temporary), DENY_ERROR_LOG, DENY_ERROR)
            self.assertEqual(classify_deny(path, "1"), "tool_error")

    def test_unknown_record_type_is_tool_error(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = write_deny(Path(temporary), json.dumps({"fields": {}, "type": "weird"}))
            self.assertEqual(classify_deny(path, "1"), "tool_error")

    def test_status_content_mismatch_is_tool_error(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.assertEqual(
                classify_deny(write_deny(root, DENY_ERROR, DENY_SUMMARY), "0"),
                "tool_error",
            )
            self.assertEqual(
                classify_deny(write_deny(root, DENY_WARNING, DENY_SUMMARY), "1"),
                "tool_error",
            )

    def test_malformed_and_missing_output_is_tool_error(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.assertEqual(
                classify_deny(write_deny(root, "{not-json"), "1"), "tool_error"
            )
            self.assertEqual(
                classify_deny(write_deny(root, "[1,2]"), "1"), "tool_error"
            )
            self.assertEqual(
                classify_deny(root / "absent.json", "1"), "tool_error"
            )

    def test_empty_status_is_not_run(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.assertEqual(
                classify_deny(write_deny(root, DENY_ERROR), ""), "not_run"
            )
            self.assertEqual(classify_deny(root / "absent.json", ""), "not_run")


class ClassifyAuditTests(unittest.TestCase):
    def test_repo_fixtures_classify(self) -> None:
        self.assertEqual(
            classify_audit(AUDIT_FIXTURES / "clean.json", "0"), "clean"
        )
        self.assertEqual(
            classify_audit(AUDIT_FIXTURES / "findings.json", "1"), "findings"
        )

    def test_status_content_mismatch_is_tool_error(self) -> None:
        self.assertEqual(
            classify_audit(AUDIT_FIXTURES / "findings.json", "0"), "tool_error"
        )
        self.assertEqual(
            classify_audit(AUDIT_FIXTURES / "clean.json", "1"), "tool_error"
        )

    def test_malformed_and_missing_output_is_tool_error(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            malformed = root / "audit.json"
            malformed.write_text("{not-json", encoding="utf-8")
            self.assertEqual(classify_audit(malformed, "1"), "tool_error")
            self.assertEqual(classify_audit(root / "absent.json", "1"), "tool_error")

    def test_empty_status_is_not_run(self) -> None:
        self.assertEqual(
            classify_audit(AUDIT_FIXTURES / "findings.json", ""), "not_run"
        )


class EvidenceEnforceTests(unittest.TestCase):
    def write_evidence(self, root: Path, scanner: str, payload: str) -> None:
        (root / f"security.{scanner}.json").write_text(payload, encoding="utf-8")
        sidecar = "stdout" if scanner == "deny" else "stderr"
        (root / f"security.{scanner}.{sidecar}").write_text("", encoding="utf-8")
        (root / f"security.{scanner}.version").write_text(
            f"cargo-{scanner} test-version", encoding="utf-8"
        )

    def test_evidence_records_hashes_version_state(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.write_evidence(root, "deny", DENY_SUMMARY + "\n")
            result = evidence(root, "deny", "0")
            self.assertEqual(result["state"], "clean")
            self.assertEqual(result["exit_status"], "0")
            self.assertEqual(result["version"], "cargo-deny test-version")
            self.assertEqual(
                sorted(result["sha256"]),
                ["security.deny.json", "security.deny.stdout", "security.deny.version"],
            )

    def test_evidence_rejects_empty_version(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.write_evidence(root, "audit", '{"vulnerabilities": {"list": []}}')
            (root / "security.audit.version").write_text("  \n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "missing audit version"):
                evidence(root, "audit", "0")

    def test_evidence_missing_files_raise(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaises(OSError):
                evidence(Path(temporary), "deny", "1")

    def test_enforce_has_exactly_one_pass_state(self) -> None:
        base = {"commit": "abc", "deny": {"state": "clean"}, "audit": {"state": "clean"}}
        self.assertTrue(enforce(base, "success"))
        for state in ("findings", "tool_error", "not_run"):
            with self.subTest(state=state):
                summary = {"commit": "abc", "deny": {"state": state}, "audit": {"state": "clean"}}
                self.assertFalse(enforce(summary, "success"))
                summary = {"commit": "abc", "deny": {"state": "clean"}, "audit": {"state": state}}
                self.assertFalse(enforce(summary, "success"))
        for upload in ("failure", "skipped", ""):
            with self.subTest(upload=upload):
                self.assertFalse(enforce(base, upload))
        self.assertFalse(enforce({**base, "commit": ""}, "success"))
        self.assertFalse(enforce({}, "success"))


class CliRoundTripTests(unittest.TestCase):
    def make_root(self, parent: Path, deny_payload: str, audit_payload: str) -> Path:
        root = parent / "security"
        root.mkdir()
        (root / "security.deny.json").write_text(deny_payload, encoding="utf-8")
        (root / "security.deny.stdout").write_text("", encoding="utf-8")
        (root / "security.deny.version").write_text("cargo-deny test", encoding="utf-8")
        (root / "security.audit.json").write_text(audit_payload, encoding="utf-8")
        (root / "security.audit.stderr").write_text("", encoding="utf-8")
        (root / "security.audit.version").write_text("cargo-audit test", encoding="utf-8")
        return root

    def run_script(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(SCRIPT), *args],
            check=False,
            capture_output=True,
            text=True,
        )

    def test_findings_classify_then_enforce_fails(self) -> None:
        audit_findings = (AUDIT_FIXTURES / "findings.json").read_text(encoding="utf-8")
        with tempfile.TemporaryDirectory() as temporary:
            root = self.make_root(
                Path(temporary), DENY_ERROR + "\n" + DENY_SUMMARY + "\n", audit_findings
            )
            classified = self.run_script(
                "classify", "--root", str(root), "--commit", "abc",
                "--deny-status", "1", "--audit-status", "1",
            )
            self.assertEqual(classified.returncode, 0, classified.stderr)
            summary = json.loads((root / "summary.json").read_text(encoding="utf-8"))
            self.assertEqual(summary["deny"]["state"], "findings")
            self.assertEqual(summary["audit"]["state"], "findings")
            enforced = self.run_script("enforce", "--root", str(root), "--upload", "success")
            self.assertEqual(enforced.returncode, 1)
            self.assertIn("Security gate failed", enforced.stdout)

    def test_clean_classify_then_enforce_passes(self) -> None:
        audit_clean = (AUDIT_FIXTURES / "clean.json").read_text(encoding="utf-8")
        with tempfile.TemporaryDirectory() as temporary:
            root = self.make_root(
                Path(temporary), DENY_WARNING + "\n" + DENY_SUMMARY + "\n", audit_clean
            )
            classified = self.run_script(
                "classify", "--root", str(root), "--commit", "abc",
                "--deny-status", "0", "--audit-status", "0",
            )
            self.assertEqual(classified.returncode, 0, classified.stderr)
            enforced = self.run_script("enforce", "--root", str(root), "--upload", "success")
            self.assertEqual(enforced.returncode, 0, enforced.stdout)
            self.assertIn("Both scanners clean", enforced.stdout)

    def test_failed_upload_never_passes(self) -> None:
        audit_clean = (AUDIT_FIXTURES / "clean.json").read_text(encoding="utf-8")
        with tempfile.TemporaryDirectory() as temporary:
            root = self.make_root(
                Path(temporary), DENY_SUMMARY + "\n", audit_clean
            )
            self.assertEqual(
                self.run_script(
                    "classify", "--root", str(root), "--commit", "abc",
                    "--deny-status", "0", "--audit-status", "0",
                ).returncode,
                0,
            )
            for upload in ("failure", "skipped", ""):
                with self.subTest(upload=upload):
                    result = self.run_script("enforce", "--root", str(root), "--upload", upload)
                    self.assertEqual(result.returncode, 1)

    def test_missing_evidence_classifies_not_run_and_enforce_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "security"
            classified = self.run_script(
                "classify", "--root", str(root), "--commit", "abc"
            )
            self.assertEqual(classified.returncode, 0, classified.stderr)
            summary = json.loads((root / "summary.json").read_text(encoding="utf-8"))
            self.assertEqual(summary["deny"]["state"], "not_run")
            self.assertEqual(summary["audit"]["state"], "not_run")
            self.assertEqual(
                self.run_script("enforce", "--root", str(root), "--upload", "success").returncode,
                1,
            )

    def test_enforce_without_summary_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            result = self.run_script(
                "enforce", "--root", str(Path(temporary) / "absent"), "--upload", "success"
            )
            self.assertEqual(result.returncode, 1)


def read_security_blocks() -> list[str]:
    """Split the ci.yml security job into dash-led step blocks (no yaml dep)."""
    lines = WORKFLOW.read_text(encoding="utf-8").splitlines()
    start = next(i for i, line in enumerate(lines) if line == "  security:")
    end = next(
        (i for i in range(start + 1, len(lines))
         if re.match(r"^  [A-Za-z0-9_-]+:$", lines[i])),
        len(lines),
    )
    blocks: list[str] = []
    current: list[str] = []
    for line in lines[start:end]:
        if line.startswith("    - ") and current:
            blocks.append("\n".join(current))
            current = []
        if line.startswith("    - ") or current:
            current.append(line)
    if current:
        blocks.append("\n".join(current))
    return blocks


class WorkflowInvariantTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.blocks = read_security_blocks()
        assert cls.blocks, "security job has no steps"

    def find(self, needle: str) -> str:
        for block in self.blocks:
            if needle in block:
                return block
        self.fail(f"security job has no step containing {needle!r}")
        raise AssertionError("unreachable")

    def find_id(self, step_id: str) -> str:
        for block in self.blocks:
            if any(line.strip() == f"id: {step_id}" for line in block.splitlines()):
                return block
        self.fail(f"security job has no step with id {step_id!r}")
        raise AssertionError("unreachable")

    def test_scanners_install_independently(self) -> None:
        deny_install = self.find_id("install_deny")
        self.assertIn("taiki-e/install-action", deny_install)
        self.assertRegex(deny_install, r"cargo-deny@\d+\.\d+\.\d+")
        audit_install = self.find_id("install_audit")
        self.assertIn("if: always()", audit_install)
        self.assertIn("continue-on-error: true", audit_install)
        self.assertRegex(audit_install, r"cargo-audit@\d+\.\d+\.\d+")

    def test_both_scanners_run_despite_findings(self) -> None:
        for step_id, evidence_name in (("deny", "security.deny."), ("audit", "security.audit.")):
            with self.subTest(step=step_id):
                block = self.find_id(step_id)
                self.assertIn("if: always()", block)
                self.assertIn("continue-on-error: true", block)
                self.assertIn("mkdir -p target/security", block)
                self.assertIn(evidence_name, block)
                self.assertIn("exit_status", block)
                self.assertIn("GITHUB_OUTPUT", block)

    def test_classify_consumes_both_exit_statuses(self) -> None:
        block = self.find_id("classify")
        self.assertIn("if: always()", block)
        self.assertIn("security_scan_result.py classify", block)
        self.assertIn("steps.deny.outputs.exit_status", block)
        self.assertIn("steps.audit.outputs.exit_status", block)

    def test_receipt_upload_is_fail_closed(self) -> None:
        block = self.find_id("upload_security")
        self.assertIn("if: always()", block)
        self.assertIn("actions/upload-artifact", block)
        self.assertIn("path: target/security/", block)
        self.assertIn("if-no-files-found: error", block)

    def test_final_gate_checks_everything(self) -> None:
        block = self.find("Enforce security results")
        self.assertIn("if: always()", block)
        self.assertNotIn("continue-on-error", block)
        self.assertIn("security_scan_result.py enforce", block)
        self.assertIn("steps.classify.outcome", block)
        self.assertIn("steps.install_deny.outcome", block)
        self.assertIn("steps.install_audit.outcome", block)
        self.assertIn("steps.upload_security.outcome", block)

    def test_step_ordering_is_install_run_classify_upload_enforce(self) -> None:
        positions = [
            self.blocks.index(self.find_id("install_deny")),
            self.blocks.index(self.find_id("install_audit")),
            self.blocks.index(self.find_id("deny")),
            self.blocks.index(self.find_id("audit")),
            self.blocks.index(self.find_id("classify")),
            self.blocks.index(self.find_id("upload_security")),
            self.blocks.index(self.find("Enforce security results")),
        ]
        self.assertEqual(positions, sorted(positions))

    def test_helper_script_exists(self) -> None:
        self.assertTrue(SCRIPT.is_file())


if __name__ == "__main__":
    unittest.main()
