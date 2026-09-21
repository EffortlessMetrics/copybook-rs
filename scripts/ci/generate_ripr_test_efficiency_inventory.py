#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Generate the conservative downstream fact ledger required by ripr+.

ripr 0.10.0 requires target/ripr/reports/test-efficiency.json before it can
render ripr+ endpoints. ripr's richer test-efficiency producer is currently a
repo-private xtask, so copybook-rs deliberately does not copy its private
heuristics. Instead, this bridge inventories real Rust test declarations and
marks every test opaque. That is fail-closed: it establishes that the tests
exist without claiming discriminator strength, reached owners, or activation
facts that this repository has not independently computed.

When ripr exposes a portable public test-efficiency producer, replace this
bridge rather than extending it into a second implementation.
"""

from __future__ import annotations

import argparse
import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

SCHEMA_VERSION = "0.1"
ROOTS = ("crates", "tools", "tests")
SKIP_PARTS = {
    ".git",
    ".venv",
    "node_modules",
    "target",
    "vendor",
}

CLASS_KEYS = (
    "strong_discriminator",
    "useful_but_broad",
    "smoke_only",
    "likely_vacuous",
    "possibly_circular",
    "duplicative",
    "opaque",
)

REASON_KEYS = (
    "no_assertion_detected",
    "smoke_oracle_only",
    "relational_oracle",
    "broad_oracle",
    "assertion_may_not_match_detected_owner",
    "opaque_helper_or_fixture_boundary",
    "no_activation_literal_detected",
    "expected_value_computed_from_detected_owner_path",
    "duplicate_activation_and_oracle_shape",
)

TEST_ATTR_RE = re.compile(
    r"^\s*#\[\s*(?:test|tokio::test(?:\([^\]]*\))?|"
    r"async_std::test(?:\([^\]]*\))?|rstest(?:\([^\]]*\))?)\s*\]"
)
FN_RE = re.compile(r"\b(?:async\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?:<[^>]*>\s*)?\(")


@dataclass(frozen=True, order=True)
class TestDecl:
    path: str
    line: int
    name: str


def _normalized(path: Path) -> str:
    return path.as_posix()


def _candidate_files(root: Path) -> Iterable[Path]:
    for base_name in ROOTS:
        base = root / base_name
        if not base.exists():
            continue
        for path in base.rglob("*.rs"):
            try:
                relative = path.relative_to(root)
            except ValueError:
                continue
            if any(part in SKIP_PARTS for part in relative.parts):
                continue
            yield path


def discover_test_declarations(root: Path) -> list[TestDecl]:
    declarations: list[TestDecl] = []
    for path in sorted(_candidate_files(root)):
        relative = _normalized(path.relative_to(root))
        text = path.read_text(encoding="utf-8", errors="strict")
        pending_attr_line: int | None = None

        for line_number, line in enumerate(text.splitlines(), start=1):
            stripped = line.strip()

            if TEST_ATTR_RE.match(line):
                pending_attr_line = line_number
                fn_match = FN_RE.search(line)
                if fn_match:
                    declarations.append(
                        TestDecl(relative, pending_attr_line, fn_match.group(1))
                    )
                    pending_attr_line = None
                continue

            if pending_attr_line is None:
                continue

            if not stripped or stripped.startswith("//") or stripped.startswith("#["):
                continue

            fn_match = FN_RE.search(line)
            if fn_match:
                declarations.append(
                    TestDecl(relative, pending_attr_line, fn_match.group(1))
                )
            pending_attr_line = None

    return sorted(set(declarations))


def build_report(root: Path) -> dict:
    tests = discover_test_declarations(root)
    if not tests:
        raise RuntimeError(
            "no Rust test declarations found under crates/, tools/, or tests/; "
            "refusing to emit an empty ripr+ fact ledger"
        )

    limitation = (
        "copybook-rs downstream inventory bridge: test exists, but reached "
        "owners, activation values, and oracle quality were not independently "
        "computed; entry is intentionally opaque"
    )

    entries = [
        {
            "path": test.path,
            "name": test.name,
            "line": test.line,
            "class": "opaque",
            "oracle_kind": "opaque oracle",
            "oracle_strength": "smoke",
            "reached_owners": [],
            "observed_values": [],
            "reasons": [
                "no_activation_literal_detected",
                "opaque_helper_or_fixture_boundary",
            ],
            "static_limitations": [limitation],
            "duplicate_group_id": None,
        }
        for test in tests
    ]

    class_counts = {key: 0 for key in CLASS_KEYS}
    class_counts["opaque"] = len(entries)
    reason_counts = {key: 0 for key in REASON_KEYS}
    reason_counts["opaque_helper_or_fixture_boundary"] = len(entries)
    reason_counts["no_activation_literal_detected"] = len(entries)

    return {
        "schema_version": SCHEMA_VERSION,
        "status": "warn",
        "advisory": True,
        "producer": {
            "name": "copybook-rs-ripr-test-efficiency-inventory",
            "mode": "conservative_opaque_inventory",
            "upstream_contract": "ripr 0.10.0 test-efficiency schema 0.1",
            "limitation": (
                "This is a downstream fact-source bridge, not ripr's private "
                "test-efficiency analyzer."
            ),
        },
        "metrics": {
            "tests_scanned": len(entries),
            "class_counts": class_counts,
            "reason_counts": reason_counts,
            "duplicate_discriminator_group_count": 0,
        },
        "duplicate_groups": [],
        "tests": entries,
    }


def write_report(root: Path, output: Path) -> None:
    report = build_report(root)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        json.dumps(report, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("target/ripr/reports/test-efficiency.json"),
    )
    args = parser.parse_args()

    root = args.root.resolve()
    output = args.output
    if not output.is_absolute():
        output = root / output

    write_report(root, output)
    print(f"ripr test-efficiency inventory: wrote {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
