#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Hostile tests for the API baseline tool compatibility preflight."""

from __future__ import annotations

import os
import json
from pathlib import Path
import shutil
import stat
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "api-baseline.sh"


class ApiBaselinePreflightTests(unittest.TestCase):
    """The preflight must fail closed before accepting an unsafe tool state."""

    @classmethod
    def setUpClass(cls) -> None:
        cls.bash = shutil.which("bash")
        if cls.bash is None:
            raise unittest.SkipTest("bash is required for API baseline preflight tests")

    def run_preflight(self, tool: str | None) -> subprocess.CompletedProcess[str]:
        with tempfile.TemporaryDirectory(prefix="api-baseline-preflight-") as temp:
            temp_path = Path(temp)
            stable_packages = [
                "copybook",
                "copybook-charset",
                "copybook-cli",
                "copybook-cli-determinism",
                "copybook-codec",
                "copybook-codepage",
                "copybook-contracts",
                "copybook-core",
                "copybook-error",
                "copybook-error-reporter",
            ]
            workspace_packages = [*stable_packages, "copybook-arrow"]
            metadata = {
                "workspace_members": [f"test:{name}" for name in workspace_packages],
                "packages": [
                    {"id": f"test:{name}", "name": name} for name in workspace_packages
                ],
            }
            self.write_tool(
                temp_path / "cargo",
                """#!/usr/bin/env bash
if [[ \"$1\" == \"metadata\" ]]; then
  printf '%s\\n' '%s'
  exit 0
fi
exit 99
""".replace("printf '%s\\n' '%s'", "printf '%s\\n' '" + json.dumps(metadata) + "'"),
            )
            self.write_tool(
                temp_path / "rustc",
                """#!/usr/bin/env bash
printf '%s\\n' 'rustc 1.98.0 (test)'
""",
            )
            self.write_tool(
                temp_path / "rustdoc",
                """#!/usr/bin/env bash
printf '%s\\n' 'rustdoc 1.98.0 (test)'
""",
            )
            self.write_tool(
                temp_path / "git",
                """#!/usr/bin/env bash
if [[ \"$1\" == \"rev-parse\" ]]; then
  printf '%s\\n' 'test-revision'
  exit 0
fi
exit 99
""",
            )
            if tool is not None:
                self.write_tool(temp_path / "cargo-semver-checks", tool)

            env = os.environ.copy()
            env["PATH"] = f"{temp}{os.pathsep}{env.get('PATH', '')}"
            env["HOME"] = temp
            return subprocess.run(
                [self.bash, "scripts/api-baseline.sh", "preflight"],
                cwd=ROOT,
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )

    @staticmethod
    def write_tool(path: Path, contents: str) -> None:
        path.write_text(contents, encoding="utf-8", newline="\n")
        path.chmod(path.stat().st_mode | stat.S_IXUSR)

    def test_missing_tool_fails_closed(self) -> None:
        result = self.run_preflight(None)
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("cargo-semver-checks is not installed", output)

    def test_old_tool_fails_closed(self) -> None:
        result = self.run_preflight(
            """#!/usr/bin/env bash
if [[ \"$1\" == \"--version\" ]]; then echo 'cargo-semver-checks 0.48.0'; exit 0; fi
if [[ \"$1\" == \"check-release\" && \"$2\" == \"--help\" ]]; then exit 0; fi
exit 99
"""
        )
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("older than required 0.49.0", output)

    def test_malformed_version_fails_closed(self) -> None:
        result = self.run_preflight(
            """#!/usr/bin/env bash
if [[ \"$1\" == \"--version\" ]]; then echo 'cargo-semver-checks unknown'; exit 0; fi
if [[ \"$1\" == \"check-release\" && \"$2\" == \"--help\" ]]; then exit 0; fi
exit 99
"""
        )
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("valid cargo-semver-checks version", output)

    @unittest.skipIf(os.name == "nt", "Git Bash cannot resolve Windows worktree .git files")
    def test_parser_failure_reports_compatibility_context(self) -> None:
        result = self.run_preflight(
            """#!/usr/bin/env bash
if [[ \"$1\" == \"--version\" ]]; then echo 'cargo-semver-checks 0.49.0'; exit 0; fi
if [[ \"$1\" == \"check-release\" && \"$2\" == \"--help\" ]]; then exit 0; fi
echo 'unsupported rustdoc format v61' >&2
exit 101
"""
        )
        output = result.stdout + result.stderr
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("could not parse the active rustdoc JSON format", output)
        self.assertIn("Expected rustdoc JSON format: v60", output)
        self.assertIn("cargo-semver-checks 0.49.0", output)

    @unittest.skipIf(os.name == "nt", "Git Bash cannot resolve Windows worktree .git files")
    def test_compatible_tool_passes_probe(self) -> None:
        result = self.run_preflight(
            """#!/usr/bin/env bash
if [[ \"$1\" == \"--version\" ]]; then echo 'cargo-semver-checks 0.49.0'; exit 0; fi
if [[ \"$1\" == \"check-release\" && \"$2\" == \"--help\" ]]; then exit 0; fi
exit 0
"""
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("parsed rustdoc JSON v60", result.stdout)


if __name__ == "__main__":
    unittest.main()
