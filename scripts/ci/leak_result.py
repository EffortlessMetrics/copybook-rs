#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Truthful leak-detection assessment (#1000).

Parses LeakSanitizer and Valgrind evidence produced by instrumented runs
and emits a small JSON result with one of three distinct verdicts:

- ``clean``         the instrumented execution completed and its evidence
                    shows no selected-category finding under the recorded
                    error policy;
- ``leak-found``    the evidence shows a finding under the recorded policy;
- ``unavailable``   the execution failed, timed out, or its evidence is
                    missing, truncated, or malformed. Unavailable is never
                    clean and never a leak claim.

Exit codes mirror the verdicts: 0 = clean, 1 = leak-found, 2 = unavailable
(including unusable CLI input). The helper performs no execution itself:
producers (build/test/valgrind) preserve their own exit status in `--rc`
files, and a nonzero producer status always yields ``unavailable`` — an
analyzer step with `if: always()` may collect evidence and summarize a
failure, but it cannot certify a run that never happened.

Valgrind error policy is explicit and recorded in the result: only the
leak kinds passed via `--kinds` (mirroring Valgrind's
``--errors-for-leak-kinds``) can produce ``leak-found``. Possible and
reachable allocations are reported, never blocking. There are no
suppressions unless `--suppressions` names them.
"""

from __future__ import annotations

import argparse
import json
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

EXIT_CLEAN = 0
EXIT_LEAK = 1
EXIT_UNAVAILABLE = 2

# Valgrind memcheck leak-kind element values we recognize.
KNOWN_KINDS = frozenset({
    "Leak_Definite",
    "Leak_Indirect",
    "Leak_Possible",
    "Leak_Reachable",
    "Leak_Suppressed",
})


def _read(path: str) -> tuple[str | None, str]:
    try:
        return Path(path).read_text(encoding="utf-8", errors="replace"), ""
    except OSError as exc:
        return None, f"cannot read {path}: {exc}"


def _read_rc(path: str) -> tuple[int | None, str]:
    text, error = _read(path)
    if text is None:
        return None, error
    try:
        return int(text.strip().split()[0]), ""
    except (ValueError, IndexError):
        return None, f"malformed rc file {path}: {text.strip()!r}"


def assess_lsan(log_text: str | None, producer_rc: int | None,
                valgrind_version: str, rustc_version: str) -> dict:
    """Assess one LeakSanitizer instrumented-test run."""
    base = {"tool": "lsan", "producer_rc": producer_rc,
            "versions": {"valgrind": valgrind_version, "rustc": rustc_version}}
    if log_text is None:
        return {**base, "verdict": "unavailable",
                "reason": "instrumented-test log is missing"}
    if producer_rc is None:
        return {**base, "verdict": "unavailable",
                "reason": "producer exit status is missing"}
    if producer_rc != 0:
        return {**base, "verdict": "unavailable",
                "reason": f"instrumented tests exited {producer_rc}; "
                          "a failing run cannot certify leak absence"}
    if "SUMMARY: LeakSanitizer" in log_text:
        return {**base, "verdict": "leak-found",
                "reason": "log contains a LeakSanitizer summary"}
    if "ERROR: LeakSanitizer" in log_text:
        return {**base, "verdict": "unavailable",
                "reason": "log contains a sanitizer error without a leak summary; "
                          "evidence is truncated or unusable"}
    if not log_text.strip():
        return {**base, "verdict": "unavailable",
                "reason": "instrumented-test log is empty"}
    return {**base, "verdict": "clean",
            "reason": "instrumented tests exited 0 with no LeakSanitizer summary"}


def parse_valgrind_xml(xml_text: str) -> tuple[dict[str, dict[str, int]] | None, str]:
    """Parse Valgrind XML error entries into {kind: {errors, bytes, blocks}}."""
    try:
        root = ET.fromstring(xml_text)
    except ET.ParseError as exc:
        return None, f"malformed valgrind XML: {exc}"
    if root.tag != "valgrindoutput":
        return None, f"unexpected valgrind XML root: {root.tag!r}"
    kinds: dict[str, dict[str, int]] = {}
    for error in root.findall("error"):
        kind_el = error.find("kind")
        kind = kind_el.text if kind_el is not None else None
        if kind not in KNOWN_KINDS:
            return None, f"unrecognized valgrind error kind: {kind!r}"
        xwhat = error.find("xwhat")
        leaked_bytes = 0
        leaked_blocks = 0
        if xwhat is not None:
            for tag, key in (("leakedbytes", "bytes"), ("leakedblocks", "blocks")):
                el = xwhat.find(tag)
                if el is not None and el.text is not None:
                    try:
                        if key == "bytes":
                            leaked_bytes = int(el.text.replace(",", "").strip())
                        else:
                            leaked_blocks = int(el.text.replace(",", "").strip())
                    except ValueError:
                        return None, f"malformed valgrind count in {tag}: {el.text!r}"
        entry = kinds.setdefault(kind, {"errors": 0, "bytes": 0, "blocks": 0})
        entry["errors"] += 1
        entry["bytes"] += leaked_bytes
        entry["blocks"] += leaked_blocks
    return kinds, ""


def assess_valgrind(xml_text: str | None, producer_rc: int | None,
                    error_exitcode: int, kinds: list[str],
                    suppressions: str, valgrind_version: str) -> dict:
    """Assess one Valgrind instrumented run under an explicit kind policy."""
    policy = {"errors_for_leak_kinds": kinds, "error_exitcode": error_exitcode,
              "suppressions": suppressions or "none (default)"}
    base: dict = {"tool": "valgrind", "producer_rc": producer_rc, "policy": policy,
                  "versions": {"valgrind": valgrind_version}}
    if xml_text is None:
        return {**base, "verdict": "unavailable",
                "reason": "valgrind XML evidence is missing"}
    if producer_rc is None:
        return {**base, "verdict": "unavailable",
                "reason": "producer exit status is missing"}
    parsed, error = parse_valgrind_xml(xml_text)
    if parsed is None:
        if producer_rc != 0:
            return {**base, "verdict": "unavailable",
                    "reason": f"valgrind exited {producer_rc} with unusable XML ({error})"}
        return {**base, "verdict": "unavailable",
                "reason": f"valgrind XML is unusable ({error})"}
    base["evidence"] = {"kinds": parsed}
    selected = {kind: parsed.get(kind, {"errors": 0, "bytes": 0, "blocks": 0})
                for kind in kinds}
    selected_bytes = sum(entry["bytes"] for entry in selected.values())
    if selected_bytes > 0:
        detail = ", ".join(f"{k}={v['bytes']}B/{v['blocks']}blk"
                           for k, v in sorted(selected.items()) if v["bytes"] > 0)
        return {**base, "verdict": "leak-found",
                "reason": f"selected leak kinds report lost bytes: {detail}"}
    if producer_rc == error_exitcode:
        return {**base, "verdict": "unavailable",
                "reason": f"valgrind exited {error_exitcode} but parsed XML shows "
                          "no selected-kind loss; evidence is inconsistent"}
    if producer_rc != 0:
        return {**base, "verdict": "unavailable",
                "reason": f"valgrind exited {producer_rc}; a failed run cannot "
                          "certify leak absence"}
    return {**base, "verdict": "clean",
            "reason": "valgrind exited 0 with no selected-kind loss "
                      f"(policy kinds: {', '.join(kinds)})"}


def _emit(result: dict, out: str | None) -> int:
    text = json.dumps(result, indent=2, sort_keys=True)
    if out:
        try:
            Path(out).write_text(text + "\n", encoding="utf-8")
        except OSError as exc:
            print(f"cannot write result: {exc}", file=sys.stderr)
            return EXIT_UNAVAILABLE
    print(text)
    return {"clean": EXIT_CLEAN, "leak-found": EXIT_LEAK,
            "unavailable": EXIT_UNAVAILABLE}[result["verdict"]]


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Truthful leak-detection assessment.")
    sub = parser.add_subparsers(dest="tool", required=True)

    lsan = sub.add_parser("lsan", help="assess a LeakSanitizer test run")
    lsan.add_argument("--log", required=True)
    lsan.add_argument("--rc", required=True)
    lsan.add_argument("--valgrind-version", default="unknown")
    lsan.add_argument("--rustc-version", default="unknown")
    lsan.add_argument("--out", default=None)

    vg = sub.add_parser("valgrind", help="assess a Valgrind run")
    vg.add_argument("--xml", required=True)
    vg.add_argument("--rc", required=True)
    vg.add_argument("--error-exitcode", type=int, default=42)
    vg.add_argument("--kinds", default="Leak_Definite,Leak_Indirect",
                    help="comma-separated memcheck kinds that block (mirrors "
                         "--errors-for-leak-kinds)")
    vg.add_argument("--suppressions", default="")
    vg.add_argument("--valgrind-version", default="unknown")
    vg.add_argument("--out", default=None)

    args = parser.parse_args(argv)
    if args.tool == "lsan":
        log_text, log_error = _read(args.log)
        if log_text is None:
            return _emit({"tool": "lsan", "verdict": "unavailable",
                          "reason": log_error, "producer_rc": None,
                          "versions": {"rustc": args.rustc_version}}, args.out)
        producer_rc, rc_error = _read_rc(args.rc)
        if producer_rc is None:
            return _emit({"tool": "lsan", "verdict": "unavailable",
                          "reason": rc_error, "producer_rc": None,
                          "versions": {"rustc": args.rustc_version}}, args.out)
        return _emit(assess_lsan(log_text, producer_rc,
                                 args.valgrind_version, args.rustc_version), args.out)

    xml_text, xml_error = _read(args.xml)
    if xml_text is None:
        return _emit({"tool": "valgrind", "verdict": "unavailable",
                      "reason": xml_error, "producer_rc": None,
                      "policy": {"errors_for_leak_kinds": args.kinds.split(",")},
                      "versions": {}}, args.out)
    producer_rc, rc_error = _read_rc(args.rc)
    if producer_rc is None:
        return _emit({"tool": "valgrind", "verdict": "unavailable",
                      "reason": rc_error, "producer_rc": None,
                      "policy": {"errors_for_leak_kinds": args.kinds.split(",")},
                      "versions": {}}, args.out)
    return _emit(assess_valgrind(xml_text, producer_rc, args.error_exitcode,
                                 [k.strip() for k in args.kinds.split(",") if k.strip()],
                                 args.suppressions, args.valgrind_version), args.out)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
