#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Head-bound pull-request CI assessment (#994).

Reads captured GitHub API documents and produces ONE verdict bound to a
single PR head SHA. It never contacts the network, never mutates anything
(no merge, rerun, branch/ruleset change, thread resolution, tag, publish),
and exits nonzero unless the current head is fully evidenced.

Live capture (read-only `gh` commands; re-read the head before assessing):

    HEAD_SHA=$(gh pr view <PR> --json headRefOid --jq .headRefOid)
    gh pr view <PR> --json number,headRefOid,mergeStateStatus,mergeable > pr.json
    gh api --paginate "repos/<owner>/<repo>/commits/$HEAD_SHA/check-runs?per_page=100" \
        > pages.json
    jq -s '[.[].check_runs[]]' pages.json > check-runs.json
    gh api "repos/<owner>/<repo>/commits/$HEAD_SHA/status" > status.json  # optional
    # Re-read the head; a changed SHA invalidates the captured evidence:
    HEAD_SHA2=$(gh pr view <PR> --json headRefOid --jq .headRefOid)
    python3 scripts/ci/pr_head_status.py --pr pr.json --check-runs check-runs.json \
        [--statuses status.json] --expected-head "$HEAD_SHA2"

Exit codes: 0 = ready (current-head required evidence all green),
1 = blocked/incomplete (required failure/cancel, required pending, expected
check absent, or unmapped check present), 2 = unknown (no evidence, head
mismatch between capture and observation, or malformed input).

The required/advisory mapping below is derived from repository workflow
evidence (job-level `continue-on-error`, trigger paths, changes-gates), not
from hosted branch protection: the hosted ruleset contents are not visible
to a read-only token, which is recorded as unknown rather than assumed.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass, field


# (pattern, lane, presence, source)
# lane: "required" blocks the ready verdict; "advisory" never does.
# presence: "expected" must be present on every assessed head;
# "conditional" is evaluated only when present (path filters, changes-gates,
# schedule-gated jobs, dispatch-only lanes, external reporters).
# Patterns are tried in order; the first full match wins.
POLICY: list[tuple[str, str, str, str]] = [
    # Unconditional ci.yml gates (no path filter, no job-level if, no CoE).
    (r"Rustfmt", "required", "expected", "ci.yml job fmt"),
    (r"Clippy", "required", "expected", "ci.yml job clippy"),
    (r"Test Suite( \(.*\))?", "required", "expected", "ci.yml job test (matrix)"),
    (r"Build Examples( \(.*\))?", "required", "expected", "ci.yml job examples (matrix)"),
    (r"Security Checks", "required", "expected", "ci.yml job security"),
    (r"Determinism Smoke", "required", "expected",
     "ci.yml job determinism-smoke (no job-level continue-on-error)"),
    (r"Governance \+ BDD Smoke", "required", "expected", "ci.yml job bdd-tests"),
    (r"RDW iterator tests", "required", "expected", "ci.yml job rdw-iterator-tests"),
    (r"Exit code mapping( \(.*\))?", "required", "expected",
     "ci.yml job exit-code-matrix (matrix)"),
    (r"Code Coverage", "required", "expected", "ci.yml job coverage"),
    (r"Documentation", "required", "expected", "ci.yml job docs"),
    (r"Strict Comments Mode", "required", "expected", "ci.yml job strict-comments"),
    # Unconditional single-shot PR gates.
    (r"Validate PR Title", "required", "expected", "commit-lint.yml (unconditional)"),
    (r"API Freeze Check", "required", "expected", "api-freeze.yml (unconditional trigger)"),
    (r"Determinism smoke \(codec \+ CLI\)", "required", "expected",
     "determinism-smoke.yml (unconditional trigger)"),
    (r"validate-receipt", "required", "expected", "perf-validation.yml (unconditional)"),
    (r"check-governance", "required", "expected", "perf-validation.yml (unconditional)"),
    (r"Property Tests - (Core|Codec|Integration)( \(.*\))?", "required", "expected",
     "ci-proptest.yml (unconditional trigger)"),
    (r"Property Test Summary", "required", "expected", "ci-proptest.yml proptest-summary"),
    (r"insights", "required", "expected",
     "pr-insights.yml (same-repo PRs; forks record a skipped run)"),
    # Changes-gated / trigger-path-filtered gates: required when they run.
    (r"truth", "required", "conditional", "docs-truth.yml (docs changes-gate)"),
    (r"Classify changelog requirement", "required", "conditional",
     "changelog.yml (changes-gate)"),
    (r"Validate Changie", "required", "conditional", "changelog.yml (changes-gate)"),
    (r"changes", "required", "conditional", "changes path-filter jobs (no-op when absent)"),
    (r"test( \(.*\))?", "required", "conditional",
     "ci-quick.yml / feature-flags.yml test jobs (changes-gated; matrix suffix)"),
    (r"testExpected", "required", "conditional", "ci-quick.yml / feature-flags.yml"),
    (r"test-features-module", "required", "conditional", "feature-flags.yml (changes-gate)"),
    (r"test-cli-integration", "required", "conditional", "feature-flags.yml (changes-gate)"),
    (r"Publish Plan Check", "required", "conditional",
     "publish-plan-check.yml (trigger path filter)"),
    (r"Performance gate", "required", "conditional", "perf-gate.yml (trigger path filter)"),
    (r"perf", "required", "conditional",
     "perf.yml job perf (PR trigger path filter; no job-level if/CoE; "
     "fails on >5% regression when it runs)"),
    (r"Benchmark \+ Comment", "required", "conditional",
     "pr-bench-comment.yml (conditional poster)"),
    (r"pedantic-diff", "required", "conditional", "pedantic-diff.yml (trigger path filter)"),
    (r"Rust 1\.98 — .*", "required", "conditional", "msrv-standalone.yml (trigger path filter)"),
    (r"Memory Leak Detection \(LSAN\)", "required", "conditional",
     "leak-detection.yml (runs on PR when selected; see #1000)"),
    (r"Cargo Fuzz( \(.*\))?", "required", "conditional",
     "fuzz-integration.yml (heavy; evaluated when present)"),
    (r"Generate Fuzzing Report", "required", "conditional", "fuzz-integration.yml"),
    (r"Check Fuzzing Feature Flag", "required", "conditional", "fuzz-integration.yml"),
    (r"Comprehensive .* tests", "required", "conditional",
     "ci-comprehensive.yml (skips on PRs outside its scope)"),
    (r"Tarpaulin Coverage", "required", "conditional", "ci-coverage.yml"),
    # Advisory by construction or by ownership: recorded, never blocking.
    (r"Result Docs Advisory", "advisory", "conditional",
     "ci.yml job-level continue-on-error"),
    (r"Coverage Diff", "advisory", "conditional", "ci-coverage.yml job-level continue-on-error"),
    (r"RIPR test-oracle pilot \(advisory\)", "advisory", "conditional",
     "ripr.yml job-level continue-on-error; pilot is explicitly advisory"),
    (r"codecov/.*", "advisory", "conditional", "external coverage reporter"),
    (r"status:codecov/.*", "advisory", "conditional",
     "external coverage reporter folded in from the commit-status API"),
    (r"GitGuardian Security Checks", "advisory", "conditional",
     "external secret scanner; informational review metadata"),
    (r"Devin Review", "advisory", "conditional",
     "external review bot; evaluated on merits per AGENTIC_PR_OPERATIONS.md"),
    (r"Extended Proptest Fuzzing", "advisory", "conditional",
     "ci-fuzz.yml is workflow_dispatch-only; never PR evidence"),
    (r"Fuzz Summary", "advisory", "conditional", "ci-fuzz.yml is workflow_dispatch-only"),
    (r"Weekly Security Audit", "advisory", "conditional", "ci-security.yml is schedule-only"),
    (r"Benchmarks \(perf\)", "advisory", "conditional", "perf-bench.yml is schedule-only"),
    (r"Benchmark SLO gate", "advisory", "conditional", "soak.yml is schedule-only"),
    (r"Performance Benchmarks", "advisory", "conditional", "benchmark.yml is dispatch-only"),
    (r"Publish Dry Run", "advisory", "conditional", "publish-dry-run.yml is dispatch-only"),
]

# conclusion -> bucket for completed check runs.
_COMPLETED_BUCKET = {
    "success": "pass",
    "skipped": "skipped",
    "neutral": "skipped",  # no opinion rendered; listed, never blocking
    "failure": "fail",
    "timed_out": "fail",
    "action_required": "fail",
    "cancelled": "canceled",
    "stale": "canceled",  # superseded commit; head-bound queries should not see this
}

# commit-status `state` -> bucket for external status rows.
_STATUS_BUCKET = {
    "success": "pass",
    "failure": "fail",
    "error": "fail",
    "pending": "pending",
}


@dataclass
class SelectedCheck:
    name: str
    bucket: str
    lane: str
    presence: str
    detail: str
    url: str = ""
    attempts: int = 1
    superseded: list[str] = field(default_factory=list)


def _classify_run(run: dict) -> tuple[str, str]:
    """Return (bucket, detail) for one check-runs API row."""
    status = run.get("status")
    conclusion = run.get("conclusion")
    if status != "completed" or conclusion is None:
        return "pending", f"status={status}"
    bucket = _COMPLETED_BUCKET.get(conclusion)
    if bucket is None:
        return "pending", f"unknown conclusion={conclusion}"
    return bucket, f"conclusion={conclusion}"


def _select_latest(runs: list[dict]) -> tuple[dict, list[str]]:
    """Pick the current row for one check name; list superseded conclusions."""
    # Latest timestamp wins so an old success can never mask a newer failure
    # (or a newer rerun still in flight). Running rows sort first on ties so
    # equal timestamps stay conservative (pending, not green).
    ordered = sorted(
        runs,
        key=lambda r: (r.get("completed_at") or r.get("started_at") or "",
                       1 if r.get("status") != "completed" else 0),
    )
    current = ordered[-1]
    superseded = []
    for older in ordered[:-1]:
        state = older.get("conclusion") or older.get("status")
        superseded.append(str(state))
    return current, superseded


def _lane_for(name: str) -> tuple[str, str]:
    for pattern, lane, presence, _source in POLICY:
        if re.fullmatch(pattern, name):
            return lane, presence
    return "unmapped", "conditional"


def assess(pr: dict, check_runs: list[dict], statuses: list[dict],
           expected_head: str) -> dict:
    """Assess captured evidence bound to `expected_head`. Pure function."""
    assessment: dict = {
        "head": expected_head,
        "pr_head": pr.get("headRefOid"),
        "binding_ok": True,
        "binding_violations": [],
        "checks": [],
        "required_fail": [],
        "required_canceled": [],
        "required_pending": [],
        "missing_expected": [],
        "unmapped": [],
        "advisory_fail": [],
        "merge_ref": {
            "mergeStateStatus": pr.get("mergeStateStatus"),
            "mergeable": pr.get("mergeable"),
            "note": "merge-ref state is recorded, never conflated with head evidence",
        },
        "verdict": "unknown",
        "reasons": [],
    }

    if pr.get("headRefOid") != expected_head:
        assessment["binding_ok"] = False
        assessment["reasons"].append(
            f"head changed during observation: captured {expected_head}, "
            f"PR now at {pr.get('headRefOid')}"
        )
        assessment["verdict"] = "unknown"
        return assessment

    if not check_runs and not statuses:
        assessment["reasons"].append("no check runs or statuses captured for this head")
        assessment["verdict"] = "unknown"
        return assessment

    for run in check_runs:
        if run.get("head_sha") != expected_head:
            assessment["binding_ok"] = False
            assessment["binding_violations"].append(
                f"{run.get('name')}: head_sha={run.get('head_sha')}"
            )

    by_name: dict[str, list[dict]] = {}
    for run in check_runs:
        by_name.setdefault(run.get("name", "?"), []).append(run)
    for entry in statuses:
        name = f"status:{entry.get('context', '?')}"
        by_name.setdefault(name, []).append({"_external": True, **entry})

    matched_patterns: set[str] = set()
    for name in sorted(by_name):
        rows = by_name[name]
        current, superseded = _select_latest(rows)
        lane, presence = _lane_for(name)
        if lane != "unmapped":
            for pattern, entry_lane, _p, _s in POLICY:
                if entry_lane == lane and re.fullmatch(pattern, name):
                    matched_patterns.add(pattern)
                    break
        if current.get("_external"):
            state = current.get("state")
            bucket = _STATUS_BUCKET.get(state, "pending")
            detail = f"external state={state}"
            url = current.get("target_url", "")
        else:
            bucket, detail = _classify_run(current)
            url = current.get("html_url", "")
        selected = SelectedCheck(
            name=name, bucket=bucket, lane=lane, presence=presence,
            detail=detail, url=url, attempts=len(rows), superseded=superseded,
        )
        assessment["checks"].append(selected.__dict__)
        if lane == "unmapped":
            if bucket != "skipped":
                assessment["unmapped"].append(name)
        elif lane == "required":
            if bucket == "fail":
                assessment["required_fail"].append(name)
            elif bucket == "canceled":
                assessment["required_canceled"].append(name)
            elif bucket == "pending":
                assessment["required_pending"].append(name)
        elif bucket == "fail":
            assessment["advisory_fail"].append(name)

    for pattern, lane, presence, _source in POLICY:
        if lane == "required" and presence == "expected" and pattern not in matched_patterns:
            assessment["missing_expected"].append(pattern)

    if not assessment["binding_ok"]:
        assessment["verdict"] = "unknown"
        assessment["reasons"].append("evidence mixes SHAs other than the assessed head")
    elif assessment["required_fail"]:
        assessment["verdict"] = "blocked"
        assessment["reasons"].append(
            f"required failures: {', '.join(assessment['required_fail'])}")
    elif assessment["required_canceled"]:
        assessment["verdict"] = "blocked"
        assessment["reasons"].append(
            f"required canceled (not failures, not passes): "
            f"{', '.join(assessment['required_canceled'])}")
    elif assessment["required_pending"]:
        assessment["verdict"] = "incomplete"
        assessment["reasons"].append(
            f"required still pending: {', '.join(assessment['required_pending'])}")
    elif assessment["missing_expected"]:
        assessment["verdict"] = "incomplete"
        assessment["reasons"].append(
            f"expected required checks absent: {', '.join(assessment['missing_expected'])}")
    elif assessment["unmapped"]:
        assessment["verdict"] = "incomplete"
        assessment["reasons"].append(
            f"unmapped checks need policy classification: "
            f"{', '.join(assessment['unmapped'])}")
    else:
        assessment["verdict"] = "ready"
        assessment["reasons"].append("all required head-bound evidence green")
        if assessment["advisory_fail"]:
            assessment["reasons"].append(
                f"advisory failures recorded (non-blocking): "
                f"{', '.join(assessment['advisory_fail'])}")

    return assessment


def render_human(assessment: dict) -> str:
    lines = [
        f"head: {assessment['head']}",
        f"binding_ok: {assessment['binding_ok']}",
        f"verdict: {assessment['verdict']}",
    ]
    for reason in assessment["reasons"]:
        lines.append(f"  - {reason}")
    buckets: dict[str, list[str]] = {}
    for check in assessment["checks"]:
        buckets.setdefault(f"{check['lane']}/{check['bucket']}", []).append(check["name"])
    lines.append("counts:")
    for key in sorted(buckets):
        lines.append(f"  {key}: {len(buckets[key])}")
    for key in ("required_fail", "required_canceled", "required_pending",
                "missing_expected", "unmapped", "advisory_fail"):
        items = assessment[key]
        if items:
            lines.append(f"{key} ({len(items)}):")
            for name in items:
                lines.append(f"  - {name}")
    merge_ref = assessment["merge_ref"]
    lines.append(
        f"merge_ref (recorded only): mergeStateStatus={merge_ref['mergeStateStatus']} "
        f"mergeable={merge_ref['mergeable']}")
    return "\n".join(lines) + "\n"


def _load_json(path: str) -> object:
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Head-bound PR CI assessment (read-only).")
    parser.add_argument("--pr", required=True, help="gh pr view JSON document")
    parser.add_argument("--check-runs", required=True,
                        help="check-runs array (or {\"check_runs\": [...]}) for ONE head SHA")
    parser.add_argument("--statuses", default=None,
                        help="optional commits/SHA/status document")
    parser.add_argument("--expected-head", default=None,
                        help="head SHA to bind to (default: pr.headRefOid)")
    parser.add_argument("--format", choices=("human", "json"), default="human")
    args = parser.parse_args(argv)

    try:
        pr = _load_json(args.pr)
        raw_runs = _load_json(args.check_runs)
        statuses_doc = _load_json(args.statuses) if args.statuses else {}
    except (OSError, ValueError) as exc:
        print(f"observation failed: cannot load input: {exc}", file=sys.stderr)
        return 2
    if not isinstance(pr, dict):
        print("observation failed: pr document must be an object", file=sys.stderr)
        return 2

    check_runs: list[dict] = []
    if isinstance(raw_runs, dict) and isinstance(raw_runs.get("check_runs"), list):
        check_runs = raw_runs["check_runs"]
    elif isinstance(raw_runs, list) and all(
            isinstance(item, dict) and isinstance(item.get("check_runs"), list)
            for item in raw_runs):
        # Paginated capture: one page object per element (`jq -s` over pages).
        for page in raw_runs:
            check_runs.extend(page["check_runs"])
    elif isinstance(raw_runs, list):
        check_runs = raw_runs
    else:
        print("observation failed: check-runs document must be an array",
              file=sys.stderr)
        return 2

    statuses: list[dict] = []
    if isinstance(statuses_doc, dict) and isinstance(statuses_doc.get("statuses"), list):
        statuses = statuses_doc["statuses"]

    expected_head = args.expected_head or pr.get("headRefOid")
    if not expected_head:
        print("observation failed: no head SHA available", file=sys.stderr)
        return 2

    assessment = assess(pr, check_runs, statuses, expected_head)
    if args.format == "json":
        print(json.dumps(assessment, indent=2, sort_keys=True))
    else:
        print(render_human(assessment), end="")

    return {"ready": 0, "blocked": 1, "incomplete": 1, "unknown": 2}[assessment["verdict"]]


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
