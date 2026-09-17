#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Fail when a CI lane's executed toolchain can silently differ from its intent.

`rust-toolchain.toml` pins 1.98 for fmt parity, and that file takes precedence
over the compiler a toolchain-install step selects as the rustup default.
This script checks declarations only:

- a nightly-implied dtolnay step must declare `toolchain:` (or ride a
  file-level RUSTUP_TOOLCHAIN);
- a job that sets job-level `RUSTUP_TOOLCHAIN` must contain a
  `witness_toolchain.sh` step proving the selection at execution time.

Declaration checks alone cannot prove execution: the runtime proof is the
witness step itself, which fails closed unless rustup reports the override
reason `environment variable RUSTUP_TOOLCHAIN`.
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


def job_blocks(path: Path) -> list[tuple[str, int, list[str]]]:
    """Split a workflow file into (job name, start line, lines) blocks."""
    jobs: list[tuple[str, int, list[str]]] = []
    current: list[str] = []
    name = ""
    start = 0
    for lineno, line in enumerate(path.read_text().splitlines(), start=1):
        stripped = line.lstrip()
        indent = len(line) - len(stripped)
        if line.startswith("  ") and not line.startswith("   ") and stripped.endswith(":"):
            if current:
                jobs.append((name, start, current))
            name = stripped[:-1]
            start = lineno
            current = [line]
        elif current:
            current.append(line)
    if current:
        jobs.append((name, start, current))
    return jobs


def job_env_toolchain(block: list[str]) -> str | None:
    """Return the job-level RUSTUP_TOOLCHAIN value, if the job sets one."""
    in_env = False
    for line in block:
        stripped = line.lstrip()
        indent = len(line) - len(stripped)
        if indent == 4 and stripped.rstrip() == "env:":
            in_env = True
            continue
        if indent <= 4 and stripped:
            in_env = False
        if in_env and stripped.startswith("RUSTUP_TOOLCHAIN:"):
            return stripped.split(":", 1)[1].strip()
    return None


def witness_violations_for(workflows: Path) -> list[str]:
    """Fail jobs that select RUSTUP_TOOLCHAIN without a witness step."""
    problems: list[str] = []
    for path in sorted(workflows.glob("*.yml")):
        try:
            jobs = job_blocks(path)
        except OSError:
            continue
        for name, start, block in jobs:
            if name in {"env", "defaults", "concurrency", "permissions"}:
                continue
            text = "\n".join(block)
            selected = job_env_toolchain(block)
            if selected is not None and "witness_toolchain.sh" not in text:
                problems.append(
                    f"{path.name}:{start}: job `{name}` sets RUSTUP_TOOLCHAIN "
                    f"({selected}) without a witness_toolchain.sh step"
                )
    return problems


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    if args:
        workflows = Path(args[0])
    else:
        root = Path(__file__).resolve().parents[2]
        workflows = root / ".github" / "workflows"
    problems = violations_for(workflows) + witness_violations_for(workflows)
    for problem in problems:
        print(f"error: {problem}", file=sys.stderr)
    return 1 if problems else 0


if __name__ == "__main__":
    raise SystemExit(main())
