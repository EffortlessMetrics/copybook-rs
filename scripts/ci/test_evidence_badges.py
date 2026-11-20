#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

import evidence_badges as subject


def endpoint(label: str, message: str = "12") -> dict:
    return {
        "schemaVersion": 1,
        "label": label,
        "message": message,
        "color": "orange",
    }


class EvidenceBadgeTests(unittest.TestCase):
    def write_all(self, directory: Path, *, message: str = "12") -> None:
        directory.mkdir(parents=True, exist_ok=True)
        for filename, label in subject.ENDPOINTS.items():
            (directory / filename).write_text(
                json.dumps(endpoint(label, message)) + "\n",
                encoding="utf-8",
            )

    def test_validate_directory_accepts_exact_numeric_shields_endpoints(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            self.write_all(directory)
            values = subject.validate_directory(directory)
            self.assertEqual(values["ripr-plus.json"]["message"], "12")
            self.assertEqual(
                values["unsafe-review-plus.json"]["label"], "unsafe-review+"
            )

    def test_non_numeric_message_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            self.write_all(directory)
            (directory / "ripr-plus.json").write_text(
                json.dumps(endpoint("ripr+", "partial")) + "\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(subject.EndpointError, "decimal count"):
                subject.validate_directory(directory)

    def test_duplicate_json_key_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "ripr.json"
            path.write_text(
                '{"schemaVersion":1,"label":"ripr","label":"other",'
                '"message":"1","color":"green"}\n',
                encoding="utf-8",
            )
            with self.assertRaisesRegex(subject.EndpointError, "duplicate JSON key"):
                subject.validate_endpoint(path, "ripr")

    def test_publish_then_check_is_stable_and_detects_drift(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            generated = root / "generated"
            committed = root / "badges"
            self.write_all(generated, message="7")

            subject.publish(generated, committed)
            subject.check(generated, committed)

            (committed / "unsafe-review-plus.json").write_text(
                json.dumps(endpoint("unsafe-review+", "8")) + "\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(subject.EndpointError, "endpoint drift"):
                subject.check(generated, committed)


if __name__ == "__main__":
    unittest.main()
