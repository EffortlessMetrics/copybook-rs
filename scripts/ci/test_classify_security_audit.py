# SPDX-License-Identifier: AGPL-3.0-or-later
"""Regression tests for weekly cargo-audit issue classification."""

from __future__ import annotations

import json
import tempfile
import unittest
from itertools import product
from pathlib import Path

from classify_security_audit import classify, evaluate_outcome


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
