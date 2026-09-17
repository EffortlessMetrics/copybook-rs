#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Bounded docs.rs availability checks from the publish plan (#988 slice B).

For every package@version in the publish plan, probes the crate page
(`.../crate/<pkg>/<ver>`, proving registry publication) and the docs URL
(`.../<pkg>/<ver>`, proving built library docs). Outcomes per crate:

- ``docs-built``      docs URL resolves to built documentation for the
                      requested version (a redirect into the versioned
                      tree, not back to the crate page or sideways to a
                      different version);
- ``docs-absent``     published, but no library docs for this version yet
                      (binary-only crates) or still building (libraries).
                      Pending discovery remains pending: never pass, never
                      proven failure;
- ``crate-missing``   the crate page itself 404s after retries, which
                      contradicts a successful publish and blocks;
- ``unknown``         fetch error or timeout. A timeout yields a resumable
                      incomplete state: it never triggers republishing,
                      retagging, or a yank.

Overall: ``ready`` (every crate built or not-applicable), ``incomplete``
(any pending/unknown), ``failed`` (any crate-missing). Exit codes mirror
the overall state: 0 = ready, 1 = failed, 2 = incomplete. The workflow
records incomplete receipts without failing the release; only ``failed``
blocks.

Whether a crate is expected to carry library docs is derived from the
workspace checkout (`crates/<package>/src/lib.rs`), not from HTTP: a
binary-only crate without docs is ``not-applicable``, never pending.
"""

from __future__ import annotations

import argparse
import datetime
import json
import sys
import urllib.error
import urllib.request
from pathlib import Path

EXIT_READY = 0
EXIT_FAILED = 1
EXIT_INCOMPLETE = 2


def http_fetch(url: str, timeout: float):
    """Fetch one URL, following redirects. Returns (status, final_url)."""
    request = urllib.request.Request(url, headers={"User-Agent": "copybook-rs-release-smoke"})
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            return response.status, response.geturl()
    except urllib.error.HTTPError as exc:
        location = exc.headers.get("Location") if exc.headers else None
        return exc.code, location
    except (urllib.error.URLError, TimeoutError, OSError):
        return None, None


def classify_docs(package: str, version: str, final_url: str | None) -> tuple[str, str]:
    """Classify a docs-URL outcome. Never pass on a sideways redirect."""
    if not final_url:
        return "unknown", "docs URL did not resolve to a final location"
    prefix = f"/{package}/{version}/"
    if "/crate/" in (final_url.split("docs.rs", 1)[-1] if "docs.rs" in final_url else final_url):
        return "absent", f"docs URL redirects to the crate page: {final_url}"
    tail = final_url.split("docs.rs", 1)[-1] if "docs.rs" in final_url else final_url
    if prefix in tail and "/crate/" not in tail:
        return "built", f"docs resolve into the versioned tree: {final_url}"
    return "absent", f"docs URL does not resolve to {package}@{version}: {final_url}"


def check_plan(plan: list[dict], fetch, workspace: Path,
               timeout: float, retries: int, base_url: str) -> dict:
    """Assess every plan entry. `fetch(url, timeout)` is injected (stubbed in tests)."""
    crates = []
    for entry in plan:
        package = entry["package"]
        version = entry["version"]
        expected_docs = (workspace / "crates" / package / "src" / "lib.rs").exists()
        crate_url = f"{base_url}/crate/{package}/{version}"
        docs_url = f"{base_url}/{package}/{version}"

        crate_status: int | None = None
        for _ in range(retries + 1):
            crate_status, _ = fetch(crate_url, timeout)
            if crate_status != 404:
                break
        if crate_status == 404:
            crates.append({
                "package": package, "version": version,
                "expected_docs": expected_docs,
                "crate_page": "missing", "docs": "not-checked",
                "status": "crate-missing",
                "reason": f"crate page 404 after {retries + 1} attempts: {crate_url}",
            })
            continue
        if crate_status != 200:
            crates.append({
                "package": package, "version": version,
                "expected_docs": expected_docs,
                "crate_page": "unknown", "docs": "not-checked",
                "status": "unknown",
                "reason": f"crate page fetch failed (status={crate_status}): {crate_url}",
            })
            continue

        docs_status, docs_final = fetch(docs_url, timeout)
        if docs_status is None:
            crates.append({
                "package": package, "version": version,
                "expected_docs": expected_docs,
                "crate_page": "ok", "docs": "unknown",
                "status": "unknown",
                "reason": f"docs fetch failed: {docs_url}",
            })
            continue
        if docs_status == 404 and not docs_final:
            # Published but no docs build yet (or binary-only): pending
            # discovery, never a proven failure.
            kind, detail = "absent", f"docs URL 404 for {package}@{version}"
        else:
            kind, detail = classify_docs(package, version, docs_final)
        if kind == "built":
            status = "docs-built"
        elif not expected_docs:
            status = "not-applicable"
        else:
            status = "pending"
        crates.append({
            "package": package, "version": version,
            "expected_docs": expected_docs,
            "crate_page": "ok", "docs": kind,
            "status": status,
            "reason": detail,
        })

    if any(c["status"] == "crate-missing" for c in crates):
        overall = "failed"
    elif any(c["status"] in ("pending", "unknown") for c in crates):
        overall = "incomplete"
    else:
        overall = "ready"
    return {
        "schema": "copybook-docs-availability/1",
        "issue": "#988 slice B (registry docs)",
        "overall": overall,
        "checked_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "base_url": base_url,
        "crates": crates,
    }


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Bounded docs.rs availability checks.")
    parser.add_argument("--plan", required=True, help="publish plan JSON document")
    parser.add_argument("--workspace", default=None,
                        help="workspace checkout (default: script's repository root)")
    parser.add_argument("--out", default=None, help="write the result JSON here")
    parser.add_argument("--timeout", type=float, default=20.0, help="per-request seconds")
    parser.add_argument("--retries", type=int, default=3, help="extra crate-page attempts")
    parser.add_argument("--base-url", default="https://docs.rs")
    args = parser.parse_args(argv)

    try:
        with open(args.plan, encoding="utf-8") as handle:
            plan = json.load(handle)
    except (OSError, ValueError) as exc:
        print(f"cannot load plan: {exc}", file=sys.stderr)
        return EXIT_FAILED
    if not isinstance(plan, list) or not plan:
        print("plan must be a non-empty array", file=sys.stderr)
        return EXIT_FAILED

    workspace = Path(args.workspace) if args.workspace else Path(__file__).resolve().parents[2]
    result = check_plan(plan, http_fetch, workspace, args.timeout, args.retries, args.base_url)

    text = json.dumps(result, indent=2, sort_keys=True)
    if args.out:
        try:
            Path(args.out).write_text(text + "\n", encoding="utf-8")
        except OSError as exc:
            print(f"cannot write result: {exc}", file=sys.stderr)
            return EXIT_FAILED
    print(text)
    return {"ready": EXIT_READY, "failed": EXIT_FAILED,
            "incomplete": EXIT_INCOMPLETE}[result["overall"]]


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
