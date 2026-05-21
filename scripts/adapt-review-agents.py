#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
"""Compatibility wrapper for the native Rust copybook-scripts implementation."""

import subprocess
import sys

sys.exit(
    subprocess.call([
        "cargo",
        "run",
        "--quiet",
        "--manifest-path",
        "tools/copybook-scripts/Cargo.toml",
        "--",
        "adapt-review-agents",
    ])
)
