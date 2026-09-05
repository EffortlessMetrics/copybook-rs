# SPDX-License-Identifier: AGPL-3.0-or-later
"""Regression tests for weekly cargo-audit issue classification."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from itertools import product
from pathlib import Path

try:
    from classify_security_audit import classify, classify_lifecycle, evaluate_outcome
except ModuleNotFoundError:
    from scripts.ci.classify_security_audit import classify, classify_lifecycle, evaluate_outcome


class ClassifySecurityAuditTests(unittest.TestCase):
    def write_document(self, directory: Path, document: object) -> Path:
        path = directory / "audit.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        return path

    def test_zero_findings_requires_no_issue_action(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = self.write_document(
                Path(temporary), {"vulnerabilities": {"count": 0, "list": []}}
            )
            self.assertEqual(classify(path), ("no_action", 0))

    def test_findings_require_create_or_update(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = self.write_document(
                Path(temporary),
                {
                    "vulnerabilities": {
                        "count": 1,
                        "list": [{"advisory": {"id": "RUSTSEC-1"}}],
                    }
                },
            )
            self.assertEqual(classify(path), ("create_or_update", 1))

    def test_fingerprint_is_stable_for_reordered_findings(self) -> None:
        findings = [
            {"advisory": {"id": "RUSTSEC-2"}, "package": {"name": "b", "version": "2.0"}},
            {"advisory": {"id": "RUSTSEC-1"}, "package": {"name": "a", "version": "1.0"}},
        ]
        with tempfile.TemporaryDirectory() as temporary:
            first = self.write_document(Path(temporary), {"vulnerabilities": {"count": 2, "list": findings}})
            original = classify_lifecycle(first, "1")
            second = Path(temporary) / "reordered.json"
            second.write_text(json.dumps({"vulnerabilities": {"count": 2, "list": list(reversed(findings))}}), encoding="utf-8")
            reordered = classify_lifecycle(second, "1")
            self.assertEqual(original["findings_fingerprint"], reordered["findings_fingerprint"])
            changed = json.loads(second.read_text(encoding="utf-8"))
            changed["vulnerabilities"]["list"][0]["package"]["version"] = "2.1"
            second.write_text(json.dumps(changed), encoding="utf-8")
            self.assertNotEqual(original["findings_fingerprint"], classify_lifecycle(second, "1")["findings_fingerprint"])

    def test_lifecycle_eligibility_is_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            clean = self.write_document(root, {"vulnerabilities": {"count": 0, "list": []}})
            self.assertTrue(classify_lifecycle(clean, "0")["eligible"])
            self.assertFalse(classify_lifecycle(clean, "1")["eligible"])
            findings = self.write_document(root, {"vulnerabilities": {"count": 1, "list": [{"advisory": {"id": "RUSTSEC-1"}, "package": {"name": "a", "version": "1"}}]}})
            self.assertTrue(classify_lifecycle(findings, "1")["eligible"])
            self.assertFalse(classify_lifecycle(findings, "2")["eligible"])
            self.assertFalse(classify_lifecycle(findings, "0")["eligible"])
            self.assertFalse(classify_lifecycle(findings, None)["eligible"])

    def test_fingerprint_identity_is_required(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = self.write_document(Path(temporary), {"vulnerabilities": {"count": 1, "list": [{"advisory": {"id": "RUSTSEC-1"}, "package": {"name": "a"}}]}})
            with self.assertRaisesRegex(ValueError, "package.version"):
                classify_lifecycle(path, "1")

    def test_malformed_json_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "audit.json"
            path.write_text("{not-json", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "not valid JSON"):
                classify(path)

    def test_missing_json_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "missing.json"
            with self.assertRaisesRegex(ValueError, "output is missing"):
                classify(path)

    def test_declared_count_mismatch_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = self.write_document(
                Path(temporary), {"vulnerabilities": {"count": 1, "list": []}}
            )
            with self.assertRaisesRegex(ValueError, "count does not match"):
                classify(path)

    def test_non_json_numbers_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "audit.json"
            for constant in ("NaN", "Infinity", "-Infinity"):
                with self.subTest(constant=constant):
                    path.write_text(
                        '{"vulnerabilities":{"count":0,"list":[]},'
                        '"metadata":{"nested":[' + constant + ']}}',
                        encoding="utf-8",
                    )
                    with self.assertRaisesRegex(ValueError, "not valid JSON"):
                        classify_lifecycle(path, "0")

    def test_duplicate_json_keys_fail_closed(self) -> None:
        finding = (
            '{"advisory":{"id":"RUSTSEC-1"},'
            '"package":{"name":"a","version":"1"}}'
        )
        documents = {
            "overwritten report": (
                '{"vulnerabilities":{"count":1,"list":[' + finding + ']},'
                '"vulnerabilities":{"count":0,"list":[]}}'
            ),
            "overwritten findings": (
                '{"vulnerabilities":{"count":0,"list":[' + finding + '],"list":[]}}'
            ),
            "identical counts": '{"vulnerabilities":{"count":0,"count":0,"list":[]}}',
            "escaped key": r'{"vulnerabilities":{"count":0,"\u0063ount":0,"list":[]}}',
            "nested metadata": (
                '{"vulnerabilities":{"count":0,"list":[]},'
                '"metadata":{"value":0,"value":1}}'
            ),
        }
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "audit.json"
            for name, document in documents.items():
                with self.subTest(case=name):
                    path.write_text(document, encoding="utf-8")
                    with self.assertRaisesRegex(ValueError, "duplicate JSON object key"):
                        classify_lifecycle(path, "0")

    def test_repeated_keys_in_separate_objects_are_valid(self) -> None:
        findings = [
            {"advisory": {"id": "RUSTSEC-1"}, "package": {"name": "a", "version": "1"}},
            {"advisory": {"id": "RUSTSEC-2"}, "package": {"name": "b", "version": "2"}},
        ]
        with tempfile.TemporaryDirectory() as temporary:
            path = self.write_document(
                Path(temporary), {"vulnerabilities": {"count": 2, "list": findings}}
            )
            result = classify_lifecycle(path, "1")
            self.assertEqual(result["finding_count"], 2)
            self.assertTrue(result["eligible"])
            self.assertTrue(result["findings_fingerprint"].startswith("sha256:"))

    def test_constant_names_in_strings_are_valid(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = self.write_document(
                Path(temporary),
                {
                    "vulnerabilities": {"count": 0, "list": []},
                    "metadata": {"values": ["NaN", "Infinity", "-Infinity"]},
                },
            )
            self.assertEqual(classify(path), ("no_action", 0))
            self.assertTrue(classify_lifecycle(path, "0")["eligible"])

    def test_cli_rejects_invalid_json_before_writing_outputs(self) -> None:
        documents = (
            '{"vulnerabilities":{"count":0,"list":[]},"metadata":NaN}',
            '{"vulnerabilities":{"count":0,"list":[],"list":[]}}',
        )
        script = Path(__file__).with_name("classify_security_audit.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            path = root / "audit.json"
            output = root / "github-output"
            for document, existing in product(documents, (False, True)):
                with self.subTest(document=document, existing=existing):
                    path.write_text(document, encoding="utf-8")
                    output.unlink(missing_ok=True)
                    if existing:
                        output.write_bytes(b"sentinel=keep\n")
                    result = subprocess.run(
                        [
                            sys.executable, str(script), "classify", str(path),
                            "--audit-exit-status", "0", "--github-output", str(output),
                        ],
                        check=False,
                        capture_output=True,
                        text=True,
                    )
                    self.assertEqual(result.returncode, 1, result.stderr)
                    self.assertEqual(result.stdout, "")
                    self.assertIn("error:", result.stderr)
                    if existing:
                        self.assertEqual(output.read_bytes(), b"sentinel=keep\n")
                    else:
                        self.assertFalse(output.exists())

    def test_invalid_utf8_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "audit.json"
            path.write_bytes(b'{"vulnerabilities":{"count":0,"list":[]},"metadata":"\xff"}')
            with self.assertRaisesRegex(ValueError, "not valid JSON"):
                classify_lifecycle(path, "0")

    def test_gate_outcome_cross_product_has_exactly_one_pass_state(self) -> None:
        deny_statuses = ("0", "1", "2", "")
        audit_statuses = ("0", "1", "2", "")
        classification_outcomes = ("success", "failure")
        decisions = ("no_action", "create_or_update", "")

        for deny, audit, classification, decision in product(
            deny_statuses,
            audit_statuses,
            classification_outcomes,
            decisions,
        ):
            with self.subTest(
                deny=deny,
                audit=audit,
                classification=classification,
                decision=decision,
            ):
                passed, _reason = evaluate_outcome(
                    deny, audit, classification, decision
                )
                expected = (deny, audit, classification, decision) == (
                    "0",
                    "0",
                    "success",
                    "no_action",
                )
                self.assertEqual(passed, expected)

    def test_findings_fail_even_when_both_tools_exit_zero(self) -> None:
        passed, reason = evaluate_outcome("0", "0", "success", "create_or_update")
        self.assertFalse(passed)
        self.assertIn("vulnerabilities", reason)

    def test_combined_failure_preserves_deny_and_findings_diagnostics(self) -> None:
        passed, reason = evaluate_outcome("2", "1", "success", "create_or_update")
        self.assertFalse(passed)
        self.assertIn("cargo-deny failed", reason)
        self.assertIn("cargo-audit failed", reason)
        self.assertIn("vulnerabilities", reason)


if __name__ == "__main__":
    unittest.main()
