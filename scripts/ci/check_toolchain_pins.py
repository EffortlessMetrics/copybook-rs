#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Fail when a nightly-only CI lane inherits the stable toolchain pin.

`rust-toolchain.toml` pins stable 1.98 for fmt parity. Any workflow step that
needs nightly (cargo-fuzz, miri, sanitizers) must declare
`toolchain: nightly` explicitly on its dtolnay/rust-toolchain step instead of
relying on the action default, which now resolves to the pinned file.
"""

from __future__ import annotations

import sys
from pathlib import Path

NIGHTLY_MARKERS = ("nightly", "cargo-fuzz", "cargo fuzz", "miri", "sanitizer", "-z ")


def step_blocks(path: Path) -> list[tuple[int, list[str]]]:
    """Split a workflow file into dash-led step blocks with start lines."""
    blocks: list[tuple[int, list[str]]] = []
    current: list[str] = []
    start = 0
    for lineno, line in enumerate(path.read_text().splitlines(), start=1):
        stripped = line.lstrip()
        indent = len(line) - len(stripped)
        if stripped.startswith("- ") and indent <= 10 and current:
            blocks.append((start, current))
            current = []
            start = lineno
        if stripped.startswith("- "):
            current = [line]
            start = lineno
        elif current:
            current.append(line)
    if current:
        blocks.append((start, current))
    return blocks


def violations_for(workflows: Path) -> list[str]:
    problems: list[str] = []
    for path in sorted(workflows.glob("*.yml")):
        try:
            whole = path.read_text()
        except OSError:
            continue
        # A file-level RUSTUP_TOOLCHAIN opt-out covers every step.
        file_opt_out = "RUSTUP_TOOLCHAIN" in whole
        for start, block in step_blocks(path):
            text = "\n".join(block)
            if "dtolnay/rust-toolchain" not in text:
                continue
            lowered = text.lower()
            nightly_implied = any(m in lowered for m in NIGHTLY_MARKERS)
            explicit = "toolchain:" in text or file_opt_out
            if nightly_implied and not explicit:
                problems.append(
                    f"{path.name}:{start}: nightly-implied dtolnay step "
                    "without explicit `toolchain:` or RUSTUP_TOOLCHAIN"
                )
    return problems


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    if args:
        workflows = Path(args[0])
    else:
        root = Path(__file__).resolve().parents[2]
        workflows = root / ".github" / "workflows"
    problems = violations_for(workflows)
    for problem in problems:
        print(f"error: {problem}", file=sys.stderr)
    return 1 if problems else 0


if __name__ == "__main__":
    raise SystemExit(main())
