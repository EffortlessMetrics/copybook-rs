# SPDX-License-Identifier: AGPL-3.0-or-later
"""Regression tests for weekly cargo-audit issue classification."""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

from classify_security_audit import classify


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


if __name__ == "__main__":
    unittest.main()
