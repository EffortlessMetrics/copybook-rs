#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Tests for truthful leak-detection assessment (scripts/ci/leak_result.py).

Includes the five reproduction fixtures from #1000: each must produce a
truthful, distinct result, and a missing or failed execution must never
produce a clean claim. No Valgrind/LSAN execution, no network.
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

from leak_result import (  # noqa: E402
    EXIT_CLEAN,
    EXIT_LEAK,
    EXIT_UNAVAILABLE,
    assess_lsan,
    assess_valgrind,
    main,
    parse_valgrind_xml,
)

LSAN_BUILD_FAILURE_LOG = """\
error: could not compile `copybook-codec` due to 1 previous error
warning: build failed, waiting for other jobs to finish...
"""

LSAN_CLEAN_LOG = """\
test result: ok. 8985 passed; 0 failed; 0 ignored
"""

LSAN_SUMMARY_LOG = """\
==42==ERROR: LeakSanitizer: detected memory leaks

SUMMARY: LeakSanitizer: 1024 byte(s) leaked in 8 allocation(s).
"""

CLEAN_XML = """\
<?xml version="1.0"?>
<valgrindoutput>
<protocolversion>4</protocolversion>
<protocoltool>memcheck</protocoltool>
<preamble><line>Memcheck, a memory error detector</line></preamble>
<pid>123</pid>
<ppid>1</ppid>
<tool>memcheck</tool>
<args><vargv><exe>copybook</exe></vargv></args>
<status><state>RUNNING</state><time>00:00:00:00.000</time></status>
<status><state>FINISHED</state><time>00:00:01:00.000</time></status>
<errorcounts></errorcounts>
</valgrindoutput>
"""

DEFINITE_LEAK_XML = """\
<?xml version="1.0"?>
<valgrindoutput>
<protocolversion>4</protocolversion>
<protocoltool>memcheck</protocoltool>
<error>
<unique>0x1</unique>
<tid>1</tid>
<kind>Leak_Definite</kind>
<xwhat>
<text>1,024 bytes in 8 blocks are definitely lost in loss record 1 of 1</text>
<leakedbytes>1024</leakedbytes>
<leakedblocks>8</leakedblocks>
</xwhat>
</error>
<errorcounts><pair><count>1</count><unique>0x1</unique></pair></errorcounts>
</valgrindoutput>
"""

POSSIBLE_ONLY_XML = """\
<?xml version="1.0"?>
<valgrindoutput>
<protocolversion>4</protocolversion>
<protocoltool>memcheck</protocoltool>
<error>
<unique>0x2</unique>
<tid>1</tid>
<kind>Leak_Possible</kind>
<xwhat>
<text>64 bytes in 1 blocks are possibly lost in loss record 1 of 1</text>
<leakedbytes>64</leakedbytes>
<leakedblocks>1</leakedblocks>
</xwhat>
</error>
</valgrindoutput>
"""


class IssueFixturesTest(unittest.TestCase):
    """The five #1000 reproduction cases produce truthful, distinct results."""

    def test_missing_lsan_log_is_unavailable(self):
        result = assess_lsan(None, 0, "3.x", "nightly")
        self.assertEqual(result["verdict"], "unavailable")

    def test_lsan_build_failure_is_unavailable_not_clean(self):
        result = assess_lsan(LSAN_BUILD_FAILURE_LOG, 101, "3.x", "nightly")
        self.assertEqual(result["verdict"], "unavailable")
        self.assertNotEqual(result["verdict"], "clean")

    def test_missing_valgrind_evidence_is_unavailable(self):
        result = assess_valgrind(None, 0, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "unavailable")

    def test_valgrind_launch_failure_is_unavailable(self):
        # command-not-found: no XML, nonzero producer status.
        result = assess_valgrind(None, 127, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "unavailable")

    def test_reachable_only_valgrind_is_clean(self):
        # Zero lost bytes with reachable allocations and ERROR SUMMARY 0
        # must not be labelled a leak failure.
        kinds, error = parse_valgrind_xml(CLEAN_XML)
        self.assertEqual(error, "")
        self.assertEqual(kinds, {})
        result = assess_valgrind(CLEAN_XML, 0, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "clean")


class PolicyTest(unittest.TestCase):
    def test_lsan_summary_is_leak_found(self):
        result = assess_lsan(LSAN_SUMMARY_LOG, 0, "3.x", "nightly")
        self.assertEqual(result["verdict"], "leak-found")

    def test_valid_no_error_completion_passes(self):
        self.assertEqual(assess_lsan(LSAN_CLEAN_LOG, 0, "3.x", "nightly")["verdict"],
                         "clean")
        self.assertEqual(assess_valgrind(CLEAN_XML, 0, 42,
                                         ["Leak_Definite", "Leak_Indirect"],
                                         "", "3.x")["verdict"], "clean")

    def test_definite_leak_blocks(self):
        result = assess_valgrind(DEFINITE_LEAK_XML, 42, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "leak-found")
        self.assertIn("Leak_Definite", result["reason"])

    def test_possible_only_does_not_block(self):
        result = assess_valgrind(POSSIBLE_ONLY_XML, 0, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "clean")

    def test_error_exit_without_xml_loss_is_unavailable(self):
        result = assess_valgrind(CLEAN_XML, 42, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "unavailable")

    def test_malformed_xml_is_unavailable(self):
        result = assess_valgrind("<valgrindoutput><error>", 0, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "unavailable")

    def test_unknown_kind_is_unavailable(self):
        xml = CLEAN_XML.replace("</valgrindoutput>",
                                "<error><kind>Leak_Mystery</kind></error>"
                                "</valgrindoutput>")
        result = assess_valgrind(xml, 0, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "unavailable")

    def test_producer_timeout_is_unavailable(self):
        result = assess_valgrind(CLEAN_XML, 124, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.x")
        self.assertEqual(result["verdict"], "unavailable")

    def test_sanitizer_error_without_summary_is_unavailable(self):
        result = assess_lsan("==42==ERROR: LeakSanitizer: SEGV\n", 0, "3.x", "nightly")
        self.assertEqual(result["verdict"], "unavailable")

    def test_empty_log_is_unavailable(self):
        result = assess_lsan("  \n", 0, "3.x", "nightly")
        self.assertEqual(result["verdict"], "unavailable")

    def test_policy_recorded_in_result(self):
        result = assess_valgrind(CLEAN_XML, 0, 42,
                                 ["Leak_Definite", "Leak_Indirect"], "", "3.24.0")
        self.assertEqual(result["policy"]["errors_for_leak_kinds"],
                         ["Leak_Definite", "Leak_Indirect"])
        self.assertEqual(result["versions"]["valgrind"], "3.24.0")


class CliTest(unittest.TestCase):
    def test_cli_exit_codes_distinguish_all_three(self):
        with tempfile.TemporaryDirectory() as tmp:
            log = Path(tmp) / "test_output.log"
            rc = Path(tmp) / "rc"
            out = Path(tmp) / "result.json"
            log.write_text(LSAN_CLEAN_LOG)
            rc.write_text("0\n")
            self.assertEqual(main(["lsan", "--log", str(log), "--rc", str(rc),
                                   "--out", str(out)]), EXIT_CLEAN)
            result = json.loads(out.read_text())
            self.assertEqual(result["verdict"], "clean")

            log.write_text(LSAN_SUMMARY_LOG)
            self.assertEqual(main(["lsan", "--log", str(log), "--rc", str(rc),
                                   "--out", str(out)]), EXIT_LEAK)

            rc.write_text("101\n")
            self.assertEqual(main(["lsan", "--log", str(log), "--rc", str(rc),
                                   "--out", str(out)]), EXIT_UNAVAILABLE)

            missing = Path(tmp) / "absent.log"
            self.assertEqual(main(["lsan", "--log", str(missing), "--rc", str(rc),
                                   "--out", str(out)]), EXIT_UNAVAILABLE)


class ReadOnlyTest(unittest.TestCase):
    def test_helper_performs_no_execution(self):
        """The helper parses evidence only: no subprocess, no sockets, no
        execution of the instrumented binaries it assesses."""
        source = (ROOT / "scripts" / "ci" / "leak_result.py").read_text()
        tree = ast.parse(source)
        imports = set()
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                imports.update(a.name.split(".")[0] for a in node.names)
            elif isinstance(node, ast.ImportFrom) and node.module:
                imports.add(node.module.split(".")[0])
        self.assertLessEqual(
            imports, {"__future__", "argparse", "json", "sys", "xml", "pathlib"})
        for forbidden in ("subprocess", "socket", "urllib", "os.system", "os.popen"):
            self.assertNotIn(forbidden, source)


if __name__ == "__main__":
    unittest.main()
