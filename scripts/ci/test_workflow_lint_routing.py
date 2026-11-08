#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Pin the CI Quick filter inputs that select offline workflow lint.

This is a declaration test, not an execution of dorny/paths-filter. The
literal include patterns cover each change type documented for jactionlint.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CI_QUICK = ROOT / ".github" / "workflows" / "ci-quick.yml"


class WorkflowLintRoutingTests(unittest.TestCase):
    def setUp(self) -> None:
        self.workflow = CI_QUICK.read_text()
        match = re.search(
            r"(?m)^            rust:\n((?:              - '[^']+'\n)+)",
            self.workflow,
        )
        self.assertIsNotNone(match, "CI Quick must declare its rust input filter")
        self.patterns = re.findall(r"- '([^']+)'", match.group(1))

    def test_workflow_and_config_changes_select_lint(self) -> None:
        self.assertIn(".github/workflows/**", self.patterns)
        self.assertIn(".github/jactionlint.yaml", self.patterns)

    def test_both_dependabot_filenames_select_lint(self) -> None:
        self.assertIn(".github/dependabot.yml", self.patterns)
        self.assertIn(".github/dependabot.yaml", self.patterns)

    def test_composite_action_changes_select_lint(self) -> None:
        self.assertIn(".github/actions/**", self.patterns)

    def test_filter_output_controls_the_job_that_runs_lint(self) -> None:
        self.assertIn("rust: ${{ steps.filter.outputs.rust }}", self.workflow)
        job = self.workflow.split("\n  test:\n", 1)[1].split("\n  testExpected:", 1)[0]
        self.assertIn("if: needs.changes.outputs.rust == 'true'", job)
        self.assertIn("uses: jdx/jactionlint@", job)


if __name__ == "__main__":
    unittest.main()
