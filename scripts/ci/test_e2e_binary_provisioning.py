#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for the cross-package E2E binary provisioning step (#1183).

`copybook-e2e` locates the CLI through `CARGO_BIN_EXE_copybook` at runtime.
Newer cargo layouts break assert_cmd's target-directory fallback, so the Test
Suite job must build the binary and export its exact path before any test
step runs. These tests assert that invariant textually in ci.yml.
"""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CI_YML = ROOT / ".github" / "workflows" / "ci.yml"

PROVISION_STEP = "Provision copybook binary for cross-package E2E"
TEST_STEPS = (
    "Run tests (nextest)",
    "Run tests (no features)",
    "Run tests (with features)",
)


class E2EBinaryProvisioningTests(unittest.TestCase):
    def test_provision_step_provisions_exact_binary(self) -> None:
        text = CI_YML.read_text()
        self.assertIn(PROVISION_STEP, text)
        self.assertIn("cargo build -p copybook-cli", text)
        self.assertIn("CARGO_BIN_EXE_copybook=", text)
        self.assertIn("GITHUB_ENV", text)

    def test_provision_step_precedes_test_execution(self) -> None:
        text = CI_YML.read_text()
        provision_at = text.index(PROVISION_STEP)
        for step in TEST_STEPS:
            self.assertIn(step, text)
            self.assertLess(
                provision_at,
                text.index(step),
                f"provisioning must precede '{step}'",
            )


if __name__ == "__main__":
    unittest.main()
