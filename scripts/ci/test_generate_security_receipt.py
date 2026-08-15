# SPDX-License-Identifier: AGPL-3.0-or-later
"""Regression tests for normalized security evidence v2."""

from __future__ import annotations

import copy
import hashlib
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from classify_security_audit import classify
from generate_security_receipt import generate_receipt, validate_receipt

REPO_ROOT = Path(__file__).resolve().parents[2]
RAW_ROOT = REPO_ROOT / "tests/fixtures/security-scanning/raw-audit"
RECEIPT_ROOT = REPO_ROOT / "tests/fixtures/security-scanning/receipts-v2"
SCHEMA_PATH = REPO_ROOT / "docs/reference/security-receipt-schema-v2.json"
GENERATOR_PATH = Path(__file__).with_name("generate_security_receipt.py")

CASES = (
    (
        "clean",
        "a" * 40,
        "pr-gate",
        "1001",
        0,
    ),
    (
        "findings",
        "b" * 40,
        "weekly-scan",
        "1002",
        1,
    ),
    (
        "tool-error",
        "c" * 40,
        "manual",
        "local-tool-error",
        2,
    ),
)


def generate_case(case: tuple[str, str, str, str, int]) -> dict[str, object]:
    name, commit_sha, scan_type, run_id, exit_code = case
    return generate_receipt(
        RAW_ROOT / f"{name}.json",
        commit_sha=commit_sha,
        scan_type=scan_type,
        workflow_run_id=run_id,
        cargo_audit_version="0.21.2",
        audit_exit_code=exit_code,
    )


class SecurityReceiptV2Tests(unittest.TestCase):
    def test_generated_documents_match_committed_fixtures_and_repeat(self) -> None:
        for case in CASES:
            with self.subTest(case=case[0]):
                generated = generate_case(case)
                expected = json.loads(
                    (RECEIPT_ROOT / f"{case[0]}.json").read_text(encoding="utf-8")
                )
                self.assertEqual(generated, expected)
                self.assertEqual(generate_case(case), generated)
                validate_receipt(generated)

    def test_digest_and_findings_bind_the_same_single_read(self) -> None:
        findings_bytes = (RAW_ROOT / "findings.json").read_bytes()
        clean_bytes = (RAW_ROOT / "clean.json").read_bytes()
        original_read_bytes = Path.read_bytes

        with tempfile.TemporaryDirectory() as temporary:
            audit_path = Path(temporary) / "audit.json"
            audit_path.write_bytes(findings_bytes)

            def read_then_replace(path: Path) -> bytes:
                content = original_read_bytes(path)
                path.write_bytes(clean_bytes)
                return content

            with patch.object(Path, "read_bytes", autospec=True) as read_bytes:
                read_bytes.side_effect = read_then_replace
                receipt = generate_receipt(
                    audit_path,
                    commit_sha="e" * 40,
                    scan_type="manual",
                    workflow_run_id="single-read",
                    cargo_audit_version="0.21.2",
                    audit_exit_code=1,
                )

            self.assertEqual(read_bytes.call_count, 1)
            self.assertEqual(receipt["outcome"]["finding_count"], 2)
            self.assertEqual(
                receipt["identity"]["raw_audit_sha256"],
                hashlib.sha256(findings_bytes).hexdigest(),
            )

    def test_raw_audit_digest_preserves_canonical_lf_byte_identity(self) -> None:
        lf_bytes = (RAW_ROOT / "clean.json").read_bytes()
        self.assertNotIn(b"\r", lf_bytes)
        variants = {
            "crlf": lf_bytes.replace(b"\n", b"\r\n"),
            "lone-cr": lf_bytes.replace(b"\n", b"\r"),
        }
        canonical = generate_case(CASES[0])

        with tempfile.TemporaryDirectory() as temporary:
            for name, content in variants.items():
                with self.subTest(name=name):
                    variant_path = Path(temporary) / f"clean-{name}.json"
                    variant_path.write_bytes(content)
                    variant = generate_receipt(
                        variant_path,
                        commit_sha="a" * 40,
                        scan_type="pr-gate",
                        workflow_run_id="1001",
                        cargo_audit_version="0.21.2",
                        audit_exit_code=0,
                    )
                    self.assertEqual(canonical["outcome"], variant["outcome"])
                    self.assertEqual(
                        variant["identity"]["raw_audit_sha256"],
                        hashlib.sha256(content).hexdigest(),
                    )
                    self.assertNotEqual(
                        canonical["identity"]["raw_audit_sha256"],
                        variant["identity"]["raw_audit_sha256"],
                    )

        self.assertEqual(
            canonical["identity"]["raw_audit_sha256"],
            hashlib.sha256(lf_bytes).hexdigest(),
        )

    def test_classifier_and_generator_reject_noncanonical_json_encodings(self) -> None:
        document = '{"vulnerabilities":{"count":0,"list":[]}}'
        variants = {
            "utf16": document.encode("utf-16"),
            "utf8-bom": b"\xef\xbb\xbf" + document.encode("utf-8"),
        }
        with tempfile.TemporaryDirectory() as temporary:
            audit_path = Path(temporary) / "audit.json"
            for name, content in variants.items():
                with self.subTest(name=name):
                    audit_path.write_bytes(content)
                    with self.assertRaisesRegex(ValueError, "not valid JSON"):
                        classify(audit_path)
                    with self.assertRaisesRegex(ValueError, "not valid JSON"):
                        generate_receipt(
                            audit_path,
                            commit_sha="f" * 40,
                            scan_type="manual",
                            workflow_run_id="encoding-reject",
                            cargo_audit_version="0.21.2",
                            audit_exit_code=0,
                        )

    def test_findings_preserve_explicit_severity_and_do_not_infer_missing(self) -> None:
        receipt = generate_case(CASES[1])
        findings = receipt["findings"]
        self.assertEqual(
            [finding["severity"] for finding in findings], ["high", "unknown"]
        )
        self.assertEqual(
            receipt["outcome"]["by_severity"],
            {"critical": 0, "high": 1, "medium": 0, "low": 0, "unknown": 1},
        )

    def test_raw_audit_malformed_missing_and_count_mismatch_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for path, message in (
                (root / "missing.json", "missing"),
                (RAW_ROOT / "reject-malformed.json", "not valid JSON"),
                (RAW_ROOT / "reject-count-mismatch.json", "count does not match"),
            ):
                with self.subTest(path=path.name):
                    with self.assertRaisesRegex((OSError, ValueError), message):
                        generate_receipt(
                            path,
                            commit_sha="d" * 40,
                            scan_type="manual",
                            workflow_run_id="reject",
                            cargo_audit_version="0.21.2",
                            audit_exit_code=2,
                        )

    def test_malformed_finding_and_unknown_severity_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "audit.json"
            path.write_text(
                json.dumps({"vulnerabilities": {"count": 1, "list": [None]}}),
                encoding="utf-8",
            )
            for invalid_path in (path, RAW_ROOT / "reject-unknown-severity.json"):
                with self.subTest(path=invalid_path.name):
                    with self.assertRaises(ValueError):
                        generate_receipt(
                            invalid_path,
                            commit_sha="d" * 40,
                            scan_type="manual",
                            workflow_run_id="reject",
                            cargo_audit_version="0.21.2",
                            audit_exit_code=1,
                        )

    def test_invalid_explicit_identity_inputs_fail_closed(self) -> None:
        valid = {
            "commit_sha": "d" * 40,
            "scan_type": "manual",
            "workflow_run_id": "local",
            "cargo_audit_version": "0.21.2",
            "audit_exit_code": 0,
        }
        invalid = (
            {"commit_sha": "D" * 40},
            {"scan_type": "scheduled"},
            {"workflow_run_id": ""},
            {"cargo_audit_version": ""},
            {"audit_exit_code": -1},
            {"audit_exit_code": 256},
        )
        for override in invalid:
            with self.subTest(override=override):
                arguments = valid | override
                with self.assertRaises(ValueError):
                    generate_receipt(RAW_ROOT / "clean.json", **arguments)

    def test_validator_rejects_unknown_fields_counts_states_and_identity_drift(
        self,
    ) -> None:
        receipt = generate_case(CASES[1])
        mutations = []
        unknown_root = json.loads(
            (RECEIPT_ROOT / "reject-unknown-field.json").read_text(encoding="utf-8")
        )
        mutations.append((unknown_root, "unknown fields"))
        unknown_nested = copy.deepcopy(receipt)
        unknown_nested["scanner"]["channel"] = "stable"
        mutations.append((unknown_nested, "unknown fields"))
        count_mismatch = copy.deepcopy(receipt)
        count_mismatch["outcome"]["finding_count"] = 1
        mutations.append((count_mismatch, "finding_count does not match"))
        severity_mismatch = copy.deepcopy(receipt)
        severity_mismatch["outcome"]["by_severity"]["unknown"] = 0
        mutations.append((severity_mismatch, "by_severity does not match"))
        invalid_state = copy.deepcopy(receipt)
        invalid_state["outcome"]["state"] = "success"
        mutations.append((invalid_state, "state is unsupported"))
        invalid_version = copy.deepcopy(receipt)
        invalid_version["schema_version"] = "3.0"
        mutations.append((invalid_version, "unsupported schema_version"))
        identity_drift = copy.deepcopy(receipt)
        identity_drift["identity"]["workflow_run_id"] = "changed"
        mutations.append((identity_drift, "receipt_id does not match"))

        for mutation, message in mutations:
            with self.subTest(message=message):
                with self.assertRaisesRegex(ValueError, message):
                    validate_receipt(mutation)

    def test_schema_is_closed_and_matches_generator_enums(self) -> None:
        schema = json.loads(SCHEMA_PATH.read_text(encoding="utf-8"))
        self.assertEqual(schema["properties"]["schema_version"]["const"], "2.0")
        self.assertFalse(schema["additionalProperties"])
        self.assertFalse(schema["properties"]["identity"]["additionalProperties"])
        self.assertFalse(schema["properties"]["scanner"]["additionalProperties"])
        self.assertFalse(schema["properties"]["outcome"]["additionalProperties"])
        self.assertFalse(
            schema["properties"]["outcome"]["properties"]["by_severity"][
                "additionalProperties"
            ]
        )
        item_schema = schema["properties"]["findings"]["items"]
        self.assertFalse(item_schema["additionalProperties"])
        self.assertFalse(item_schema["properties"]["package"]["additionalProperties"])
        self.assertEqual(
            set(schema["properties"]["outcome"]["properties"]["state"]["enum"]),
            {"clean", "findings", "tool_error"},
        )
        self.assertEqual(
            set(item_schema["properties"]["severity"]["enum"]),
            {"critical", "high", "medium", "low", "unknown"},
        )

    def test_cli_generates_and_validates_without_publishing(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "receipt.json"
            generated = subprocess.run(
                [
                    sys.executable,
                    str(GENERATOR_PATH),
                    "generate",
                    str(RAW_ROOT / "clean.json"),
                    "--commit-sha",
                    "a" * 40,
                    "--scan-type",
                    "pr-gate",
                    "--workflow-run-id",
                    "1001",
                    "--cargo-audit-version",
                    "0.21.2",
                    "--audit-exit-code",
                    "0",
                    "--output",
                    str(output),
                ],
                capture_output=True,
                check=False,
                text=True,
            )
            self.assertEqual(generated.returncode, 0, generated.stderr)
            self.assertEqual(
                json.loads(output.read_text(encoding="utf-8")), generate_case(CASES[0])
            )

            validated = subprocess.run(
                [sys.executable, str(GENERATOR_PATH), "validate", str(output)],
                capture_output=True,
                check=False,
                text=True,
            )
            self.assertEqual(validated.returncode, 0, validated.stderr)
            self.assertEqual(
                validated.stdout.strip(), "valid normalized security receipt v2"
            )

    def test_cli_failure_does_not_replace_existing_output(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            malformed = root / "malformed.json"
            malformed.write_text("{not-json", encoding="utf-8")
            output = root / "receipt.json"
            output.write_text("existing evidence\n", encoding="utf-8")
            generated = subprocess.run(
                [
                    sys.executable,
                    str(GENERATOR_PATH),
                    "generate",
                    str(malformed),
                    "--commit-sha",
                    "d" * 40,
                    "--scan-type",
                    "manual",
                    "--workflow-run-id",
                    "reject",
                    "--cargo-audit-version",
                    "0.21.2",
                    "--audit-exit-code",
                    "2",
                    "--output",
                    str(output),
                ],
                capture_output=True,
                check=False,
                text=True,
            )
            self.assertNotEqual(generated.returncode, 0)
            self.assertIn("not valid JSON", generated.stderr)
            self.assertEqual(output.read_text(encoding="utf-8"), "existing evidence\n")

    def test_cli_rejects_output_aliases_without_mutating_raw_evidence(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            raw = root / "audit.json"
            raw.write_bytes((RAW_ROOT / "clean.json").read_bytes())
            original = raw.read_bytes()
            nested = root / "nested"
            nested.mkdir()

            aliases = (
                raw,
                Path("audit.json"),
                nested / ".." / "audit.json",
            )
            for output in aliases:
                with self.subTest(output=output):
                    generated = self.run_cli_generate(raw, output, cwd=root)
                    self.assertNotEqual(generated.returncode, 0)
                    self.assertIn("must not alias", generated.stderr)
                    self.assertEqual(raw.read_bytes(), original)

    def test_cli_rejects_symlink_parent_alias_without_mutating_raw_evidence(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            raw = root / "audit.json"
            raw.write_bytes((RAW_ROOT / "clean.json").read_bytes())
            original = raw.read_bytes()
            alias_parent = root / "alias"
            try:
                alias_parent.symlink_to(root, target_is_directory=True)
            except OSError as error:
                self.skipTest(f"directory symlinks unavailable: {error}")

            generated = self.run_cli_generate(
                raw, alias_parent / "audit.json", cwd=root
            )
            self.assertNotEqual(generated.returncode, 0)
            self.assertIn("must not alias", generated.stderr)
            self.assertEqual(raw.read_bytes(), original)

    def run_cli_generate(
        self, raw: Path, output: Path, *, cwd: Path
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [
                sys.executable,
                str(GENERATOR_PATH),
                "generate",
                str(raw),
                "--commit-sha",
                "d" * 40,
                "--scan-type",
                "manual",
                "--workflow-run-id",
                "alias-test",
                "--cargo-audit-version",
                "0.21.2",
                "--audit-exit-code",
                "0",
                "--output",
                str(output),
            ],
            capture_output=True,
            check=False,
            cwd=cwd,
            text=True,
        )


if __name__ == "__main__":
    unittest.main()
