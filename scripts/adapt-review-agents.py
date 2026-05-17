#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Compatibility launcher for the Rust-native copybook-scripts command."""

import subprocess
import sys
from pathlib import Path

COMMAND = "adapt-review-agents"


def workspace_root() -> Path:
    return Path(
        subprocess.check_output(
            ["git", "rev-parse", "--show-toplevel"],
            text=True,
        ).strip()
    )


def main() -> int:
    root = workspace_root()
    cmd = [
        "cargo",
        "run",
        "--quiet",
        "--manifest-path",
        str(root / "tools" / "copybook-scripts" / "Cargo.toml"),
        "--",
        COMMAND,
        *sys.argv[1:],
    ]
    return subprocess.run(cmd, check=False).returncode


if __name__ == "__main__":
    raise SystemExit(main())
