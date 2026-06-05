#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Compatibility wrapper for the native Rust copybook-scripts implementation."""

import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]

sys.exit(
    subprocess.call(
        [
            "cargo",
            "run",
            "--quiet",
            "--manifest-path",
            "tools/copybook-scripts/Cargo.toml",
            "--",
            "final-cleanup-agents",
        ],
        cwd=REPO_ROOT,
    )
)
